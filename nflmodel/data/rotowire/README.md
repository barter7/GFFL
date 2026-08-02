# RotoWire blurb drop-in

**This repo does not scrape RotoWire.** Their player-news blurbs are
the product they sell; bulk-pulling them would breach their terms.
RotoWire publishes an official API/feed for subscribers who want the
text inside an application — that is the sanctioned path.

What the pipeline does instead (R2.9):

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
