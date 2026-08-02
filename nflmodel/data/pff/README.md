# PFF grade drop-in

PFF grades are proprietary — they appear in no nflverse dataset (R1.1),
so nothing here is fetched automatically. Drop your own PFF exports in
this folder and `depth_sheet.R::load_pff_grades()` joins them.

**Expected:** any `.csv` with
- an id column named `pff_id` (or `player_id` — it gets renamed), and
- one or more `grades_*` columns
  (e.g. `grades_offense`, `grades_pass`, `grades_run`, `grades_pass_block`,
  `grades_defense`, `grades_run_defense`, `grades_pass_rush_defense`,
  `grades_coverage_defense`).

Include a `season` column for season grades; a file without one is
treated as career grades. Players join to our data through the
`pff_id` column already carried in the nflverse roster crosswalk.

Files here are gitignored (paid data — do not commit).

## How the join works (ID-first, per R1.13)

1. **`pff_id`** — primary key, carried in the nflverse roster
   crosswalk. Coverage: ~70% of the 2025 roster, ~58% of 2026.
2. **Name fallback** — used ONLY for rows the id join missed.
   Required in practice: nflverse currently has `pff_id` for
   **0 of 682** 2026 rookies, so the draft class joins by name
   until the crosswalk catches up. Include a `player` or `name`
   column in your export so this pass can work.

## Getting the files

Export them from your PFF subscription's own download/export
feature — that is what it is for, and it keeps credentials out of
this pipeline entirely. Do not put a password anywhere in this
repo; automated login/scraping would also breach PFF's terms and
risk the account.
