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

### Automated (daily, in season)

`.github/workflows/update-data.yml` refreshes the data automatically every
day at **10:00 UTC during September–January**: it runs `fetch_data.R`, then
`scripts/convert_data.py`, and commits any changes to `data/` + `src/data/`
on `main` as `github-actions[bot]` ("Daily league data refresh"). It can also
be triggered manually from the Actions tab (workflow_dispatch).

**One-time setup** — add the ESPN cookies as repository secrets:

1. Repo → **Settings → Secrets and variables → Actions → New repository secret**
2. Add `ESPN_S2` and `ESPN_SWID` (grab them from your browser's cookies while
   logged in to ESPN Fantasy; see `.Renviron.example`).

**Fail-closed behavior** — a bad refresh can never wipe the site:

- `fetch_data.R` refuses to save if any *historical* season comes back empty
  (typical symptom of expired cookies) or if nothing was fetched at all. If
  only the newest season is missing (league not yet renewed on ESPN), it
  warns loudly and continues.
- `scripts/convert_data.py` asserts the expected ffscrapr columns, drops
  unplayed games (null result) and — for the current season — any week that
  isn't fully complete (so a daily snapshot never records a half-played
  Thursday game as final). Before writing anything it compares against the
  existing `src/data/*.json`: if a season vanished or a historical season
  shrank, it exits non-zero without writing a single file.
- Any failure fails the workflow run; the previously committed data stays
  live. When cookies expire, update the two secrets and re-run the workflow.

### Manual fallback

1. `Rscript fetch_data.R` (needs `ESPN_S2` / `ESPN_SWID`, see `.Renviron.example`)
2. `pip install pyreadr rdata pandas pillow`
3. `python3 scripts/convert_data.py`
4. `python3 scripts/optimize_images.py` (only if photos changed)
5. Commit the regenerated `src/data/*.json` / `public/**` and push.
