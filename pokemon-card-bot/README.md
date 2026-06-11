# Pokémon card restock fast-checkout (personal use)

A small Node + Playwright tool that watches a single Target product page and,
when it restocks, runs add-to-cart → checkout in **your own logged-in Chrome**.

## What it is (and isn't)

- It drives **your real Chrome** over the DevTools Protocol. There is **no**
  stealth plugin, proxy rotation, fingerprint spoofing, or CAPTCHA solver.
  Your genuine logged-in session is the most reliable way through, and it
  keeps this an automation convenience rather than a detection-evasion bot.
- If Target shows a CAPTCHA / "verify you're human" / virtual queue, the
  script **pauses** and waits for you to clear it. You're expected to be
  present and watching.
- Auto-placing the order is **off by default**. Watch it navigate once, then
  turn it on.

> Heads up: automated checkout is against Target's terms, and bot-flagged
> orders can be canceled and refunded after the fact. You've accepted that.
> Use your own account and card, one item, and stay at the keyboard.

## Setup

```bash
cd pokemon-card-bot
npm install
npx playwright install chromium   # only needed once
cp config.example.json config.json
```

Edit `config.json`:

| Key | Meaning |
| --- | --- |
| `productUrl` | The Target PDP to watch |
| `tcin` | The item's TCIN (the number after `A-` in the URL) |
| `cdpEndpoint` | Your Chrome's remote-debugging address (default `http://localhost:9222`) |
| `pollSeconds` / `pollJitterSeconds` | How often to re-check, plus randomness |
| `placeOrder` | `false` = stop at review (you click buy); `true` = full auto |
| `confirmDelaySeconds` | Grace period before the final click when `placeOrder` is true (Ctrl+C to abort) |
| `discordWebhookUrl` | Optional — get phone pings for every step |

## Launch Chrome so the script can attach

Fully **quit Chrome first**, then start it with remote debugging pointed at
your normal profile (so you stay logged in with your saved address/card):

**macOS**
```bash
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
  --remote-debugging-port=9222 \
  --user-data-dir="$HOME/Library/Application Support/Google/Chrome"
```

**Windows (PowerShell)**
```powershell
& "C:\Program Files\Google\Chrome\Application\chrome.exe" `
  --remote-debugging-port=9222 `
  --user-data-dir="$env:LOCALAPPDATA\Google\Chrome\User Data"
```

**Linux**
```bash
google-chrome \
  --remote-debugging-port=9222 \
  --user-data-dir="$HOME/.config/google-chrome"
```

Sign in to Target in that window and confirm your shipping address and a
payment method are saved (so checkout is one step). Then:

```bash
npm run checkout
```

## Recommended first run

1. Set `placeOrder: false`.
2. Run it against the live item and watch it detect stock and walk to the
   review screen, then stop.
3. Once you trust the navigation, set `placeOrder: true` and `confirmDelaySeconds`
   to whatever reaction time you want before the final click.

## Notes / limits

- Stock detection keys off a visible, enabled "Ship it" / "Add to cart" button
  on the PDP. Target's drops often go live at an unannounced moment — the poll
  loop catches the real flip, not a posted time.
- Target updates their DOM occasionally; if a button stops being found, the
  selectors are near the top of `checkout.js` and easy to update.
