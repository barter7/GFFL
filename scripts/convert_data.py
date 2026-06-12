#!/usr/bin/env python3
"""Convert the cached RDS files in data/ into JSON consumed by the Next.js site.

Run after refreshing data with fetch_data.R:
    pip install pyreadr rdata pandas
    python3 scripts/convert_data.py

Outputs compact columnar JSON ({"columns": [...], "rows": [[...]]}) into src/data/.
"""

import json
import math
import os

import pandas as pd
import pyreadr
import rdata

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT_DIR = os.path.join(ROOT, "src", "data")


def read_rds(name: str) -> pd.DataFrame:
    path = os.path.join(ROOT, "data", f"{name}.rds")
    try:
        return pyreadr.read_r(path)[None]
    except Exception:
        return rdata.read_rds(path)


def fix_encoding(val):
    """RDS strings come through double-encoded (UTF-8 bytes read as latin-1)."""
    if not isinstance(val, str):
        return val
    try:
        fixed = val.encode("latin1").decode("utf8")
        return fixed
    except (UnicodeEncodeError, UnicodeDecodeError):
        return val


def clean(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    df = df[[c for c in columns if c in df.columns]].copy()
    for col in df.columns:
        if df[col].dtype == object or pd.api.types.is_string_dtype(df[col]):
            df[col] = df[col].map(fix_encoding)
    return df


def write_json(name: str, df: pd.DataFrame) -> None:
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
    payload = {"columns": [str(c) for c in df.columns], "rows": rows}
    path = os.path.join(OUT_DIR, f"{name}.json")
    with open(path, "w") as f:
        json.dump(payload, f, separators=(",", ":"), ensure_ascii=False)
    print(f"  src/data/{name}.json  ({len(rows)} rows, {os.path.getsize(path) // 1024} KB)")


def main():
    os.makedirs(OUT_DIR, exist_ok=True)

    standings = read_rds("standings")
    write_json("standings", clean(standings, [
        "season", "franchise_id", "franchise_name", "league_rank",
        "h2h_wins", "h2h_losses", "h2h_ties", "h2h_winpct",
        "points_for", "points_against",
        "allplay_wins", "allplay_losses", "allplay_winpct",
    ]))

    schedule = read_rds("schedule")
    write_json("schedule", clean(schedule, [
        "season", "week", "franchise_id", "franchise_score",
        "result", "opponent_id", "opponent_score",
    ]))

    drafts = read_rds("drafts")
    write_json("drafts", clean(drafts, [
        "season", "round", "pick", "overall", "franchise_id", "franchise_name",
        "user_nickname", "player_id", "player_name", "pos", "team",
    ]))

    starters = read_rds("starters")
    write_json("starters", clean(starters, [
        "season", "week", "franchise_id", "franchise_name", "franchise_score",
        "lineup_slot", "player_score", "projected_score",
        "player_id", "player_name", "pos", "team",
    ]))

    # Headshots: map lowercase player name -> espn_id, restricted to players
    # who actually appear in league data (keeps the file small).
    player_ids = read_rds("player_ids")
    league_names = set()
    for df in (drafts, starters):
        league_names.update(
            str(fix_encoding(n)).lower() for n in df["player_name"].dropna()
        )
    headshots = {}
    for row in player_ids[["name", "espn_id"]].dropna().itertuples(index=False):
        key = str(row.name).lower()
        if key in league_names and key not in headshots:
            try:
                headshots[key] = int(float(row.espn_id))
            except (TypeError, ValueError):
                continue
    path = os.path.join(OUT_DIR, "headshots.json")
    with open(path, "w") as f:
        json.dump(headshots, f, separators=(",", ":"), ensure_ascii=False)
    print(f"  src/data/headshots.json  ({len(headshots)} players, {os.path.getsize(path) // 1024} KB)")


if __name__ == "__main__":
    main()
