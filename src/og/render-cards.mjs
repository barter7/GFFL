#!/usr/bin/env node
// Render the link-preview card for every recap into public/og/recaps/
// as real .png files, plus latest.png for the recaps index.
//
// The site is a static export, and a Next `opengraph-image` route in a
// static export lands in out/ as a file with no extension - which static
// hosting can serve with a generic content type that iMessage and other
// link scrapers reject. A .png under public/ has no such ambiguity.
// Runs before every build (package.json "prebuild") and can be run by
// hand: node src/og/render-cards.mjs
//
// Lives under src/ (not scripts/) on purpose: .vercelignore excludes
// /scripts/ from the deployment upload, and a prebuild pointing there
// failed every build with 'cannot find module'.
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

// Never fail a build over a card: the .png files are committed, so any
// error here means the previous card ships, not no site. (The first
// version used import.meta.dirname, which older Node runtimes on the
// build host do not have; that threw before any handler was installed
// and failed every deployment for an afternoon.)
process.on("uncaughtException", (e) => { console.error(`[og] ${e.message}`); process.exit(0); });
process.on("unhandledRejection", (e) => { console.error(`[og] ${e?.message ?? e}`); process.exit(0); });

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
let ImageResponse;
try {
  ({ ImageResponse } = await import("next/og.js"));
} catch (e) {
  console.error(`[og] next/og unavailable (${e.message}); keeping the committed cards`);
  process.exit(0);
}
const SRC = path.join(ROOT, "src", "data", "recaps");
const OUT = path.join(ROOT, "public", "og", "recaps");
fs.mkdirSync(OUT, { recursive: true });

const recaps = fs.readdirSync(SRC).filter((f) => f.endsWith(".md")).map((f) => {
  const raw = fs.readFileSync(path.join(SRC, f), "utf8");
  const fm = raw.match(/^---\n([\s\S]*?)\n---/);
  const meta = {};
  for (const line of (fm?.[1] ?? "").split("\n")) {
    const m = line.match(/^(\w+):\s*(.*)$/);
    if (m) meta[m[1]] = m[2].replace(/^"|"$/g, "");
  }
  return { slug: f.replace(/\.md$/, ""), title: meta.title ?? f, season: Number(meta.season) || 0, week: Number(meta.week) || 0 };
}).sort((a, b) => b.season - a.season || b.week - a.week);

const card = (banner, title, sub) => {
  const titleSize = title.length > 44 ? 54 : title.length > 30 ? 64 : 76;
  return new ImageResponse(
    {
      type: "div",
      props: {
        style: { width: "100%", height: "100%", display: "flex", flexDirection: "column", justifyContent: "space-between",
          padding: "56px 72px", background: "#013369", color: "#ffffff", fontFamily: "sans-serif" },
        children: [
          { type: "div", props: { style: { display: "flex", alignItems: "center", gap: 22 }, children: [
            { type: "div", props: { style: { display: "flex", alignItems: "center", gap: 14, background: "#D50A0A", color: "#ffffff", fontSize: 34, fontWeight: 800, letterSpacing: 2, padding: "10px 26px", borderRadius: 999 },
              children: [{ type: "div", props: { style: { width: 18, height: 18, borderRadius: 999, background: "#ffffff" } } }, "LIVE"] } },
            { type: "div", props: { style: { display: "flex", fontSize: 40, fontWeight: 800, letterSpacing: 3, color: "#d4af37" }, children: banner } },
          ] } },
          { type: "div", props: { style: { display: "flex", flexDirection: "column", gap: 18 }, children: [
            { type: "div", props: { style: { display: "flex", fontSize: titleSize, fontWeight: 800, lineHeight: 1.1, maxWidth: 1056 }, children: title } },
            { type: "div", props: { style: { display: "flex", fontSize: 34, fontWeight: 600, color: "#dbe4f3" }, children: sub } },
          ] } },
          { type: "div", props: { style: { display: "flex", flexDirection: "column", gap: 14 }, children: [
            { type: "div", props: { style: { display: "flex", height: 4, width: 1056, background: "linear-gradient(90deg, #d4af37 0%, #f5e08a 50%, #d4af37 100%)" } } },
            { type: "div", props: { style: { display: "flex", fontSize: 30, fontWeight: 700, letterSpacing: 4, color: "#d4af37" }, children: "GROUPIES FANTASY FOOTBALL LEAGUE" } },
          ] } },
        ],
      },
    },
    { width: 1200, height: 630 },
  );
};

let n = 0;
for (const r of recaps) {
  try {
    const png = Buffer.from(await card("GFFL RECAP IS LIVE", r.title, `${r.season} · Week ${r.week}`).arrayBuffer());
    fs.writeFileSync(path.join(OUT, `${r.slug}.png`), png);
    n++;
  } catch (e) {
    console.error(`[og] ${r.slug}: ${e.message}`);
  }
}
if (recaps[0]) {
  const r = recaps[0];
  try {
    const png = Buffer.from(await card(`WEEK ${r.week} RECAP IS LIVE`, r.title, `${r.season} · Week ${r.week}`).arrayBuffer());
    fs.writeFileSync(path.join(OUT, "latest.png"), png);
  } catch (e) {
    console.error(`[og] latest: ${e.message}`);
  }
}
console.log(`[og] ${n} recap card(s) -> public/og/recaps/, latest = ${recaps[0]?.slug ?? "none"}`);
