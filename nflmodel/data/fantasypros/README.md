# data/fantasypros/ — ADP drop-in

This folder is where **average draft position** enters the model.
Nothing here is fetched automatically, and the folder's contents
are gitignored (this README is the only committed file).

## Why it's a drop-in

`fantasypros.com` is unreachable from the dev sandbox — every
request returns `000`, the same as PFF, RotoWire and PFR. ADP is
also not mirrored on GitHub anywhere I could find, so unlike ECR
there is no public path to fetch it. Use FantasyPros' own export
button while logged in; no credentials go anywhere near this repo.

## What is NOT here

`data/context/fp_ecr.csv` holds **ECR** (expert consensus rank),
fetched automatically from the DynastyProcess mirror. It is not a
substitute for ADP and the code never treats it as one:

- **ECR** = where analysts rank a player.
- **ADP** = where drafters actually take him.

They diverge on recency bias, name-brand inertia and post-hype
discounts — which is precisely where the money is. `fp_market_gap()`
computes `adp - rank_1qb` and **refuses to run** when only ECR is
present rather than approximating it.

## Expected file

Any CSV named `adp*.csv` (e.g. `adp_ppr_2026.csv`). Multiple files
are read and stacked, tagged by `source_file`.

The raw FantasyPros export works as-is — its headers are mapped
automatically:

| export header | mapped to | required |
|---|---|---|
| `AVG` or `Average` | `adp` | **yes** |
| `Player` or `Name` | `player` | for name fallback |
| `POS` | `position` | no |
| `Team` | `team` | no |
| `fp_id` | `fp_id` | no, but preferred |

## Joining (R1.13)

- If the export carries `fp_id`, rows join by ID through
  `data/context/fp_xwalk.csv`.
- Otherwise they fall back to a normalized name match.
- Either way `id_source` records which path each row took, so a
  name-matched row is never mistaken for an ID-matched one.

Note that exports usually list `POS` as `WR1`, `RB12` etc. — the
positional rank, not the position. That's carried through as-is;
strip the digits if you need a bare position.

## Reading it

```r
source("fantasypros.R")
adp <- load_fp_adp()      # NULL + a message when nothing is here
gap <- fp_market_gap()    # NULL unless ADP is present
```
