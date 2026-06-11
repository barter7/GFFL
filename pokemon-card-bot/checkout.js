// Personal-use restock monitor + fast-checkout for a single Target item.
//
// Design notes (read before running):
//   * This connects to YOUR real, already-running Chrome over the DevTools
//     Protocol. It does not launch a fresh automated browser and adds no
//     stealth/fingerprint shims. The session it drives is literally your own
//     logged-in browser, which is both the most reliable approach and keeps
//     this honest: there is no bot-detection-evasion layer here.
//   * If Target shows a CAPTCHA, a "verify you're human" wall, or a virtual
//     queue, the script detects it and PAUSES (and pings you, if a Discord
//     webhook is set) so you can clear it. It cannot beat these on its own by
//     design — so "fully unattended" stalls here until a human shows up.
//   * keepAwake holds a system stay-awake lock so the machine won't suspend
//     while watching. You can't run code on a sleeping CPU; this prevents the
//     sleep. Set keepAwake:false to opt out.
//   * placeOrder defaults to false. Leave it false until you've watched it
//     navigate correctly once; flip it to true to run unattended.

import { readFileSync } from "node:fs";
import { spawn } from "node:child_process";
import { platform } from "node:os";
import { chromium } from "playwright";

const cfg = JSON.parse(readFileSync(new URL("./config.json", import.meta.url)));

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const now = () => new Date().toLocaleTimeString();
const log = (...a) => console.log(`[${now()}]`, ...a);

// Hold a system "stay awake" lock so the machine doesn't sleep while we're
// watching. The display may still sleep; the OS just won't suspend. Returns a
// cleanup function. This is the only legitimate way to "run while asleep" —
// you can't execute code on a suspended CPU, so we prevent the suspend.
function keepAwake() {
  const os = platform();
  try {
    if (os === "darwin") {
      // -i prevent idle sleep, -m prevent disk sleep, -s while on AC.
      const p = spawn("caffeinate", ["-ims"], { stdio: "ignore" });
      log("keep-awake: caffeinate active (macOS).");
      return () => p.kill();
    }
    if (os === "linux") {
      // Inhibit sleep for as long as this child sleeps (i.e. forever) — we
      // kill it on exit.
      const p = spawn(
        "systemd-inhibit",
        ["--what=sleep:idle", "--why=pokemon-card-bot", "sleep", "infinity"],
        { stdio: "ignore" }
      );
      log("keep-awake: systemd-inhibit active (Linux).");
      return () => p.kill();
    }
    if (os === "win32") {
      // SetThreadExecutionState keeps the system + display awake until the
      // PowerShell process exits.
      const ps = [
        "$sig='[DllImport(\"kernel32.dll\")]public static extern uint SetThreadExecutionState(uint e);';",
        "$t=Add-Type -MemberDefinition $sig -Name K -Namespace W -PassThru;",
        // ES_CONTINUOUS | ES_SYSTEM_REQUIRED | ES_DISPLAY_REQUIRED
        "$t::SetThreadExecutionState(0x80000003);",
        "while($true){Start-Sleep 60}",
      ].join("");
      const p = spawn("powershell", ["-NoProfile", "-Command", ps], { stdio: "ignore" });
      log("keep-awake: SetThreadExecutionState active (Windows).");
      return () => p.kill();
    }
  } catch (e) {
    log("keep-awake unavailable:", e.message, "- machine may sleep on its own.");
  }
  return () => {};
}

async function notify(msg) {
  log(msg);
  if (!cfg.discordWebhookUrl) return;
  try {
    await fetch(cfg.discordWebhookUrl, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ content: msg }),
    });
  } catch (e) {
    log("notify failed:", e.message);
  }
}

// Returns true if the page is showing something a human needs to resolve
// (bot check / CAPTCHA / queue). We stop and wait rather than try to defeat it.
async function needsHuman(page) {
  const url = page.url();
  if (/queue-it\.net|akamai|\/blocked|\/errors\/|captcha/i.test(url)) return true;
  const markers = [
    'text=/verify you.?re (a )?human/i',
    'text=/are you a robot/i',
    'text=/press (and hold|hold)/i',
    'iframe[src*="captcha"]',
    'iframe[title*="challenge"]',
  ];
  for (const sel of markers) {
    if (await page.locator(sel).first().isVisible().catch(() => false)) return true;
  }
  return false;
}

async function waitForHuman(page) {
  await notify("⚠️ Human check / CAPTCHA / queue detected. Pausing — clear it in the browser, I'll resume automatically.");
  while (await needsHuman(page)) await sleep(2000);
  await notify("✅ Resuming.");
}

// Is the item buyable right now? We treat a visible, enabled shipping/
// add-to-cart button on the PDP as "in stock".
async function inStock(page) {
  const buyButtons = [
    '[data-test="shippingButton"]',
    '[data-test="orderPickupButton"]',
    'button:has-text("Add to cart")',
    'button:has-text("Ship it")',
  ];
  for (const sel of buyButtons) {
    const btn = page.locator(sel).first();
    if (await btn.isVisible().catch(() => false)) {
      const disabled = await btn.isDisabled().catch(() => true);
      if (!disabled) return sel;
    }
  }
  return null;
}

async function clickFirst(page, selectors, label) {
  for (const sel of selectors) {
    const el = page.locator(sel).first();
    if (await el.isVisible().catch(() => false)) {
      await el.click().catch(() => {});
      log(`clicked: ${label} (${sel})`);
      return true;
    }
  }
  log(`could not find: ${label}`);
  return false;
}

async function runCheckout(page, buySelector) {
  await notify(`🟢 In stock! Starting checkout for TCIN ${cfg.tcin}.`);

  // 1. Add to cart from the PDP.
  await page.locator(buySelector).first().click().catch(() => {});
  await sleep(1500);
  if (await needsHuman(page)) await waitForHuman(page);

  // 2. From the add-to-cart confirmation, go to checkout (skip the cart page
  //    when Target offers the direct button).
  await clickFirst(
    page,
    [
      '[data-test="content-wrapper"] >> text=/checkout/i',
      '[data-test="addToCartModalViewCartCheckout"]',
      'a:has-text("checkout")',
      'button:has-text("Checkout")',
    ],
    "go to checkout"
  );
  await page.waitForLoadState("domcontentloaded").catch(() => {});
  await sleep(2000);
  if (await needsHuman(page)) await waitForHuman(page);

  // 3. We should be on /co (checkout). Confirm we're logged in and pre-filled;
  //    if Target dumps us at sign-in, that's a human moment.
  if (/\/signin|\/login/i.test(page.url())) {
    await notify("🔐 Target wants a sign-in. Pausing — log in, I'll resume.");
    while (/\/signin|\/login/i.test(page.url())) await sleep(2000);
  }

  // 4. Place the order — only if explicitly enabled.
  if (!cfg.placeOrder) {
    await notify("🟡 Reached checkout. placeOrder is false — stopping at review. Finish the final click yourself.");
    return;
  }

  await notify(`🛒 Placing order in ${cfg.confirmDelaySeconds}s. Ctrl+C now to abort.`);
  await sleep(cfg.confirmDelaySeconds * 1000);
  if (await needsHuman(page)) await waitForHuman(page);

  const placed = await clickFirst(
    page,
    [
      '[data-test="placeOrderButton"]',
      'button:has-text("Place your order")',
      'button:has-text("Place order")',
    ],
    "place order"
  );

  if (placed) {
    await page.waitForLoadState("networkidle").catch(() => {});
    await notify("🎉 Order placed (pending Target confirmation). Verify in your account.");
  } else {
    await notify("❗ Couldn't find the place-order button. Finish manually in the browser.");
  }
}

async function main() {
  const releaseAwake = cfg.keepAwake === false ? () => {} : keepAwake();
  const cleanup = () => releaseAwake();
  process.on("exit", cleanup);
  process.on("SIGINT", () => { cleanup(); process.exit(0); });

  log(`Connecting to your Chrome at ${cfg.cdpEndpoint} ...`);
  const browser = await chromium.connectOverCDP(cfg.cdpEndpoint);
  const ctx = browser.contexts()[0];
  if (!ctx) throw new Error("No browser context found. Is Chrome running with --remote-debugging-port?");
  const page = ctx.pages()[0] || (await ctx.newPage());

  log(`Opening ${cfg.productUrl}`);
  await page.goto(cfg.productUrl, { waitUntil: "domcontentloaded" }).catch(() => {});

  await notify(`👀 Watching TCIN ${cfg.tcin}. placeOrder=${cfg.placeOrder}. Poll≈${cfg.pollSeconds}s.`);

  // Poll loop: reload the PDP with jitter, check stock, fire on hit.
  // Stops itself after a successful checkout attempt.
  // eslint-disable-next-line no-constant-condition
  while (true) {
    try {
      if (await needsHuman(page)) await waitForHuman(page);

      const sel = await inStock(page);
      if (sel) {
        await runCheckout(page, sel);
        break;
      }
    } catch (e) {
      log("poll error:", e.message);
    }

    const jitter = Math.random() * (cfg.pollJitterSeconds || 0);
    await sleep((cfg.pollSeconds + jitter) * 1000);
    await page.reload({ waitUntil: "domcontentloaded" }).catch(() => {});
  }

  log("Done. Leaving your browser open.");
  // Intentionally do NOT close the browser — it's your real session.
}

main().catch((e) => {
  console.error("Fatal:", e);
  process.exit(1);
});
