# GFFL Archives

Historical archive site for the Groupies Fantasy Football League (est. 2016).

Originally a Shiny app, now a fully static **Next.js** site deployable on
**Vercel** — no server required. All league data is pre-fetched from ESPN and
baked into the build.

## Architecture

```
fetch_data.R          (R)      ESPN/nflreadr -> data/*.rds        [run locally]
scripts/convert_data.py        data/*.rds -> src/data/*.json      [run locally]
scripts/optimize_images.py     www/photos|recaps -> public/*.webp [run locally]
src/                  (Next.js) static site, computes all stats from the JSON
```

- `src/lib/data.ts` — loads the JSON, maps ESPN accounts to real owner names,
  classifies regular season vs playoffs (port of `helpers.R`).
- `src/lib/league.ts` — shared computations (all-time standings, owner vs
  owner, streaks).
- `src/app/<tab>/` — one route per tab of the original Shiny app.
- `app.R` / `helpers.R` — the original Shiny app, kept for reference.

## Local development

```bash
npm install
npm run dev        # http://localhost:3000
npm run build      # static export to out/
```

## Deploying to Vercel

The repo is zero-config for Vercel: import it at vercel.com/new (framework
preset: Next.js) and deploy. Every push builds a fully static site.

## Updating league data

1. `Rscript fetch_data.R` (needs `ESPN_S2` / `ESPN_SWID`, see `.Renviron.example`)
2. `pip install pyreadr rdata pandas pillow`
3. `python3 scripts/convert_data.py`
4. `python3 scripts/optimize_images.py` (only if photos changed)
5. Commit the regenerated `src/data/*.json` / `public/**` and push.
