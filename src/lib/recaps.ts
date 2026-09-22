// Weekly recap loader. Recaps are markdown files in
// src/data/recaps/, named <season>-week-<NN>.md, each opening with
// a small front matter block (title / season / week / date).

import { createHash } from "node:crypto";
import { existsSync, readFileSync, readdirSync } from "node:fs";
import path from "node:path";

export interface Recap {
  slug: string;
  title: string;
  season: number;
  week: number;
  date: string;
  /** markdown body, front matter stripped */
  body: string;
}

const DIR = path.join(process.cwd(), "src", "data", "recaps");

function parse(slug: string, raw: string): Recap {
  let title = slug;
  let season = 0;
  let week = 0;
  let date = "";
  let body = raw;
  const fm = raw.match(/^---\n([\s\S]*?)\n---\n?/);
  if (fm) {
    body = raw.slice(fm[0].length);
    for (const line of fm[1].split("\n")) {
      const m = line.match(/^(\w+):\s*(.*)$/);
      if (!m) continue;
      const val = m[2].replace(/^"|"$/g, "");
      if (m[1] === "title") title = val;
      else if (m[1] === "season") season = Number(val);
      else if (m[1] === "week") week = Number(val);
      else if (m[1] === "date") date = val;
    }
  }
  return { slug, title, season, week, date, body: body.trim() };
}

export function listRecaps(): Recap[] {
  let files: string[] = [];
  try {
    files = readdirSync(DIR).filter((f) => f.endsWith(".md"));
  } catch {
    return [];
  }
  return files
    .map((f) =>
      parse(f.replace(/\.md$/, ""), readFileSync(path.join(DIR, f), "utf8")),
    )
    .sort((a, b) => b.season - a.season || b.week - a.week);
}

export function getRecap(slug: string): Recap | null {
  // slugs come from route params; only allow known files
  return listRecaps().find((r) => r.slug === slug) ?? null;
}

/** the share card's URL for a recap (or "latest" for the index), with a
 *  content hash so a regenerated card is a new URL to every cache -
 *  iMessage, Vercel's CDN and browsers all key on the URL */
export function cardUrl(name: string): string {
  const f = path.join(process.cwd(), "public", "og", "recaps", `${name}.png`);
  if (!existsSync(f)) return `/og/recaps/${name}.png`;
  const h = createHash("sha1").update(readFileSync(f)).digest("hex").slice(0, 10);
  return `/og/recaps/${name}.png?v=${h}`;
}
