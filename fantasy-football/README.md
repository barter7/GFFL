# GFFL Fantasy — NFL model site

Standalone Next.js app for the NFL model: 2026 depth charts, offseason
churn, fantasy-stock signals and a ranked draft board.

This is **its own Vercel project**, separate from the GFFL Archives app
at the repo root. Same repository, different site.

## Deploying

Import the repo at [vercel.com/new](https://vercel.com/new) and set:

| setting | value |
|---|---|
| Project name | `fantasy_football` |
| Root Directory | `fantasy-football` |
| Framework preset | Next.js (auto-detected) |

Vercel builds only this subdirectory; the archives project at the repo
root is untouched. The root's `.vercelignore` excludes `/fantasy-football/`
so the two never build each other.

## Local development

```bash
cd fantasy-football
npm install
npm run dev     # http://localhost:3000
npm run build   # static export to out/
```

## Where the data comes from

The site is a **static export** (`output: "export"`) — no server, no API
routes. All data is committed JSON, imported at build time:

```
nflmodel/           (R)  nflverse pbp -> depth charts, signals, ECR
  export_site.R          -> fantasy-football/src/data/nfl-depth.json
src/lib/nfl.ts           typed loader (columnar -> objects)
src/app/page.tsx         the page
```

To refresh after the model changes:

```bash
cd nflmodel && Rscript export_site.R
cd .. && git add fantasy-football/src/data/nfl-depth.json && git commit
```

The push triggers the rebuild. There is no runtime fetch, so the data is
only as fresh as the last commit.

## Conventions worth knowing

- **Every join is on `gsis_id`**, never a player name (nflmodel rule
  R1.13). Names collide; ids don't.
- **`null` must stay `null` in the JSON.** jsonlite writes an R `NULL` as
  `{}` by default and `{}` is truthy in JavaScript, which once published
  1,177 active players as "RESERVE" (nflmodel study S24). `export_site.R`
  passes `null = "null"`; don't remove it.
- **ADP and ECR are different things** and the UI never substitutes one
  for the other. Until a FantasyPros ADP export exists, the ADP sort
  renders disabled rather than silently ordering by ECR (rule R2.11).
- **Position colours** are fixed: RB `#73c3a6`, WR `#46a2cb`,
  QB `#46a2cb`, TE `#cc8d4a`. QB and WR share a hue by spec, so the
  position letters — not the colour — distinguish those two. Text on a
  colour wash uses a darkened variant of the same hue for contrast.
- **Stock chips encode direction with a glyph** (▲ ▼ ◆), not colour
  alone, so they survive for a colour-blind reader.
