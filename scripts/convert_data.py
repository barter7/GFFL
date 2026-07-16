#!/usr/bin/env python3
"""Convert the cached RDS files in data/ into JSON consumed by the Next.js site.

Run after refreshing data with fetch_data.R:
    pip install pyreadr rdata pandas
    python3 scripts/convert_data.py

Outputs compact columnar JSON ({"columns": [...], "rows": [[...]]}) into src/data/.

Safety properties (the daily CI refresh depends on these):
  * Required columns are asserted per input file -> exits non-zero on
    ffscrapr schema drift instead of silently emitting empty columns.
  * Schedule rows with a null result (future / unplayed games) are dropped.
  * For the newest season, weeks that are still in progress (any matchup with
    a null result or a zero score) are dropped from schedule AND starters, so
    a daily snapshot never records a half-played Thursday game as final.
  * FAIL CLOSED: all payloads are built in memory and compared against the
    existing src/data/*.json before anything is written. If a season vanished
    or a historical season shrank (e.g. because an expired ESPN cookie wiped
    the fetch), the script exits non-zero WITHOUT writing any file.
"""

import json
import math
import os
import re
import sys

import pandas as pd
import pyreadr
import rdata

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT_DIR = os.path.join(ROOT, "src", "data")

# Columns each output file needs. If ffscrapr renames/drops one of these the
# conversion aborts instead of shipping JSON with silently-missing columns.
REQUIRED_COLUMNS = {
    "standings": [
        "season", "franchise_id", "franchise_name", "league_rank",
        "h2h_wins", "h2h_losses", "h2h_ties", "h2h_winpct",
        "points_for", "points_against",
        "allplay_wins", "allplay_losses", "allplay_winpct",
    ],
    "schedule": [
        "season", "week", "franchise_id", "franchise_score",
        "result", "opponent_id", "opponent_score",
    ],
    "drafts": [
        "season", "round", "pick", "overall", "franchise_id", "franchise_name",
        "user_nickname", "player_id", "player_name", "pos", "team",
    ],
    "starters": [
        "season", "week", "franchise_id", "franchise_name", "franchise_score",
        "lineup_slot", "player_score", "projected_score",
        "player_id", "player_name", "pos", "team",
    ],
}


def die(msg: str) -> None:
    print(f"\nERROR: {msg}", file=sys.stderr)
    print("Aborting WITHOUT writing anything: src/data/*.json is untouched.",
          file=sys.stderr)
    sys.exit(1)


def read_rds(name: str) -> pd.DataFrame:
    path = os.path.join(ROOT, "data", f"{name}.rds")
    try:
        return pyreadr.read_r(path)[None]
    except Exception:
        return rdata.read_rds(path)


def assert_columns(name: str, df: pd.DataFrame) -> None:
    missing = [c for c in REQUIRED_COLUMNS[name] if c not in df.columns]
    if missing:
        die(
            f"data/{name}.rds is missing required column(s) {missing} "
            f"(has: {sorted(map(str, df.columns))}). This usually means the "
            f"ffscrapr schema changed - update REQUIRED_COLUMNS / the site "
            f"before refreshing data."
        )
    if len(df) == 0:
        die(f"data/{name}.rds has zero rows.")


def fix_encoding(val):
    """RDS strings come through double-encoded (UTF-8 bytes read as latin-1)."""
    if not isinstance(val, str):
        return val
    try:
        fixed = val.encode("latin1").decode("utf8")
        return fixed
    except (UnicodeEncodeError, UnicodeDecodeError):
        return val


def normalize_name(name) -> str:
    """Normalize a player name for headshot lookup.

    MUST stay in sync with normalizeName() in src/lib/data.ts:
    lowercase -> strip periods -> strip trailing generational suffix
    (jr|sr|ii|iii|iv) -> collapse whitespace.
    """
    s = str(name).lower()
    s = s.replace(".", "")
    s = re.sub(r"\s+(jr|sr|ii|iii|iv)$", "", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def clean(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    df = df[[c for c in columns if c in df.columns]].copy()
    for col in df.columns:
        if df[col].dtype == object or pd.api.types.is_string_dtype(df[col]):
            df[col] = df[col].map(fix_encoding)
    return df


def build_payload(df: pd.DataFrame) -> dict:
    rows = []
    for row in df.itertuples(index=False):
        out = []
        for v in row:
            if v is None or v is pd.NA or (isinstance(v, float) and math.isnan(v)):
                out.append(None)
            elif isinstance(v, float) and v.is_integer() and abs(v) < 2**40:
                out.append(int(v))
            elif hasattr(v, "item"):
                out.append(v.item())
            else:
                out.append(v)
        rows.append(out)
    return {"columns": [str(c) for c in df.columns], "rows": rows}


def season_counts(payload: dict) -> dict[int, int]:
    idx = payload["columns"].index("season")
    counts: dict[int, int] = {}
    for row in payload["rows"]:
        s = int(row[idx])
        counts[s] = counts.get(s, 0) + 1
    return counts


def validate_no_data_loss(payloads: dict[str, dict]) -> None:
    """Compare new payloads against the existing src/data/*.json (fail closed).

    (a) every season present in the old file must be present in the new one;
    (b) for every old season except the newest old season (which may still be
        gaining or losing in-progress weeks), new row count >= old row count.
    Violations abort the run before anything is written, so an expired ESPN
    cookie can never wipe the site's data.
    """
    problems = []
    for name, new_payload in payloads.items():
        path = os.path.join(OUT_DIR, f"{name}.json")
        if not os.path.exists(path):
            continue
        with open(path) as f:
            old_payload = json.load(f)
        if "season" not in old_payload.get("columns", []):
            continue
        old_counts = season_counts(old_payload)
        new_counts = season_counts(new_payload)
        newest_old = max(old_counts)
        for season, old_n in sorted(old_counts.items()):
            new_n = new_counts.get(season)
            if new_n is None:
                problems.append(
                    f"{name}: season {season} ({old_n} rows) vanished from the new data"
                )
            elif season != newest_old and new_n < old_n:
                problems.append(
                    f"{name}: season {season} shrank from {old_n} to {new_n} rows"
                )
    if problems:
        for p in problems:
            print(f"  DATA LOSS: {p}", file=sys.stderr)
        die(
            "new data would lose rows/seasons vs the current src/data/*.json "
            "(expired ESPN_S2/ESPN_SWID cookies? partial ESPN response?)"
        )


def drop_incomplete_weeks(schedule: pd.DataFrame, starters: pd.DataFrame):
    """Snapshot safety for the in-progress (max) season.

    A week counts as complete only if EVERY matchup row that week has a
    non-null result and both scores > 0. Incomplete weeks are dropped from
    both schedule and starters so half-played games are never baked in as
    final. Historical seasons are left untouched.
    """
    max_season = int(schedule["season"].max())
    cur = schedule[schedule["season"] == max_season]
    incomplete = []
    for week, grp in cur.groupby("week"):
        complete = (
            grp["result"].notna().all()
            and (grp["franchise_score"].fillna(0) > 0).all()
            and (grp["opponent_score"].fillna(0) > 0).all()
        )
        if not complete:
            incomplete.append(week)
    if incomplete:
        print(
            f"  Dropping incomplete week(s) {sorted(int(w) for w in incomplete)} "
            f"of season {max_season} (in progress)"
        )
        schedule = schedule[
            ~((schedule["season"] == max_season) & (schedule["week"].isin(incomplete)))
        ]
        if "week" in starters.columns and "season" in starters.columns:
            starters = starters[
                ~((starters["season"] == max_season) & (starters["week"].isin(incomplete)))
            ]
    # Any remaining null-result rows (future weeks of unplayed games in
    # historical seasons) are unplayed games: drop them too.
    n_null = int(schedule["result"].isna().sum())
    if n_null:
        print(f"  Dropping {n_null} schedule row(s) with null result (unplayed games)")
        schedule = schedule[schedule["result"].notna()]
    return schedule, starters


def build_headshots(player_ids: pd.DataFrame, drafts: pd.DataFrame,
                    starters: pd.DataFrame) -> dict:
    """Map normalized player name -> espn_id, restricted to players who
    actually appear in league data (keeps the file small). Keys use the same
    normalization as normalizeName() in src/lib/data.ts."""
    league_names = set()
    for df in (drafts, starters):
        league_names.update(
            normalize_name(fix_encoding(n)) for n in df["player_name"].dropna()
        )
    headshots = {}
    for row in player_ids[["name", "espn_id"]].dropna().itertuples(index=False):
        key = normalize_name(row.name)
        if key in league_names and key not in headshots:
            try:
                headshots[key] = int(float(row.espn_id))
            except (TypeError, ValueError):
                continue
    return headshots


def main():
    os.makedirs(OUT_DIR, exist_ok=True)

    # ---- Read + validate inputs -------------------------------------------
    standings = read_rds("standings")
    schedule = read_rds("schedule")
    drafts = read_rds("drafts")
    starters = read_rds("starters")
    for name, df in (("standings", standings), ("schedule", schedule),
                     ("drafts", drafts), ("starters", starters)):
        assert_columns(name, df)

    # ---- Snapshot safety: unplayed games / in-progress weeks ---------------
    schedule, starters = drop_incomplete_weeks(schedule, starters)

    # ---- Build every payload in memory (nothing written yet) ---------------
    payloads = {
        "standings": build_payload(clean(standings, REQUIRED_COLUMNS["standings"])),
        "schedule": build_payload(clean(schedule, REQUIRED_COLUMNS["schedule"])),
        "drafts": build_payload(clean(drafts, REQUIRED_COLUMNS["drafts"])),
        "starters": build_payload(clean(starters, REQUIRED_COLUMNS["starters"])),
    }

    player_ids = read_rds("player_ids")
    if "name" not in player_ids.columns or "espn_id" not in player_ids.columns:
        die("data/player_ids.rds is missing 'name'/'espn_id' columns.")
    headshots = build_headshots(player_ids, drafts, starters)
    if not headshots:
        die("headshots mapping came out empty - refusing to write.")

    # ---- Fail closed: compare against existing output before writing -------
    validate_no_data_loss(payloads)

    # ---- All validations passed: write everything ---------------------------
    for name, payload in payloads.items():
        path = os.path.join(OUT_DIR, f"{name}.json")
        with open(path, "w") as f:
            json.dump(payload, f, separators=(",", ":"), ensure_ascii=False)
        counts = season_counts(payload)
        print(
            f"  src/data/{name}.json  ({len(payload['rows'])} rows, "
            f"{os.path.getsize(path) // 1024} KB, "
            f"seasons {min(counts)}-{max(counts)})"
        )

    path = os.path.join(OUT_DIR, "headshots.json")
    with open(path, "w") as f:
        json.dump(headshots, f, separators=(",", ":"), ensure_ascii=False)
    print(f"  src/data/headshots.json  ({len(headshots)} players, "
          f"{os.path.getsize(path) // 1024} KB)")


if __name__ == "__main__":
    main()
