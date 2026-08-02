# RotoWire blurb drop-in

Two ways blurbs get here (R2.9), matching how PropSZN already
handles RotoWire in `default_lineups.R`:

**A. scrape_rotowire.R** — the NFL sibling of PropSZN's RotoWire
scraper: rvest + CSS selectors, player IDs read out of hrefs, one
news-feed request per page rather than 1,200 player pages, 2s
between requests, and the whole fetch in a tryCatch so a 403 or
CAPTCHA warns and keeps the cache instead of failing the run.

    Rscript -e 'source("scrape_rotowire.R"); inspect_rotowire_page()'
    Rscript scrape_rotowire.R

Run `inspect_rotowire_page()` FIRST — the selectors in `RW_SEL`
are unverified (rotowire.com 403s from the dev sandbox, so they
were never exercised against live markup). It prints the real
container classes so you can correct them in one edit.

**B. drop-in** — if you'd rather export from a feed/API you have
access to, put the file here and it is picked up automatically.

What the pipeline does regardless:

- **Deep links** are built for every player from the `rotowire_id`
  in the nflverse roster crosswalk (~66% of the 2026 roster).
  Verified: Kyler Murray `rotowire_id` 13613 ->
  `/football/player/kyler-murray-13613`, matching the live page.
  Players without an id get no link rather than a guessed URL.
- **Blurbs**, if you are entitled to them, go here. Any `.csv` or
  `.json` in this folder with:
  - `rotowire_id` (or `id`) — the join key, per R1.13, and
  - a text column named `blurb` / `news` / `notes` / `analysis` /
    `description`
  - optionally `date` / `updated` (newest row per player wins)

  `load_rotowire_blurbs()` picks them up automatically and they
  appear on the depth-chart rows.

Files here are gitignored — third-party content stays local.
