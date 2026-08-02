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
