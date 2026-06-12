// Port of the draft value analysis from app.R (draft_value_data reactive,
// lines ~2095-2190). The R app prefers full nflreadr player stats when
// available; the static site always uses the rostered-points fallback
// (rv$starters_data), summing player_score by player/pos per season.

import { DraftRow, StarterRow } from "@/lib/data";

export interface SeasonPts {
  player_name: string;
  pos: string;
  total_pts: number;
}

const SKILL_POS = new Set(["QB", "RB", "WR", "TE"]);

/** Season fantasy points per player from rostered (starters) data. */
export function seasonPointsFromStarters(
  starters: StarterRow[],
  season: number
): SeasonPts[] {
  const map = new Map<string, SeasonPts>();
  for (const s of starters) {
    if (s.season !== season) continue;
    const key = `${s.player_name}|${s.pos}`;
    let e = map.get(key);
    if (!e) {
      e = { player_name: s.player_name, pos: s.pos, total_pts: 0 };
      map.set(key, e);
    }
    e.total_pts += s.player_score ?? 0;
  }
  return [...map.values()];
}

/** rank(-x, ties.method = "min") — 1 = highest value. */
export function rankMinDesc(values: number[]): number[] {
  const sorted = [...values].sort((a, b) => b - a);
  const firstIdx = new Map<number, number>();
  sorted.forEach((v, i) => {
    if (!firstIdx.has(v)) firstIdx.set(v, i + 1);
  });
  return values.map((v) => firstIdx.get(v)!);
}

export interface DraftValueRow {
  season: number;
  owner: string;
  player_name: string;
  pos: string;
  drafted_pos_rank: number;
  actual_pos_rank: number;
  total_pts: number;
  expected_pts: number | null;
  value_diff: number | null;
  rank_diff: number;
}

export function computeDraftValueData(
  drafts: DraftRow[],
  starters: StarterRow[]
): DraftValueRow[] {
  const skill = drafts.filter((d) => SKILL_POS.has(d.pos));

  // Drafted position rank per season (e.g., 1st RB taken = 1)
  const draftedRank = new Map<DraftRow, number>();
  const bySeasonPos = new Map<string, DraftRow[]>();
  for (const d of skill) {
    const key = `${d.season}|${d.pos}`;
    const arr = bySeasonPos.get(key) ?? [];
    arr.push(d);
    bySeasonPos.set(key, arr);
  }
  for (const arr of bySeasonPos.values()) {
    [...arr]
      .sort((a, b) => a.overall - b.overall)
      .forEach((d, i) => draftedRank.set(d, i + 1));
  }

  const allYears = [...new Set(skill.map((d) => d.season))].sort((a, b) => a - b);
  const results: DraftValueRow[] = [];

  for (const yr of allYears) {
    const sp = seasonPointsFromStarters(starters, yr).filter((p) =>
      SKILL_POS.has(p.pos)
    );
    if (sp.length === 0) continue;

    // Actual position rank per position
    const actualRank = new Map<SeasonPts, number>();
    const spByPos = new Map<string, SeasonPts[]>();
    for (const p of sp) {
      const arr = spByPos.get(p.pos) ?? [];
      arr.push(p);
      spByPos.set(p.pos, arr);
    }
    for (const arr of spByPos.values()) {
      const ranks = rankMinDesc(arr.map((p) => p.total_pts));
      arr.forEach((p, i) => actualRank.set(p, ranks[i]));
    }
    const maxActualRank = Math.max(...[...actualRank.values()]);

    // "Expected points" = points of the actual player at that draft rank
    // e.g., if drafted as 1st RB, expected = actual RB1 points that season
    const expectedPts = new Map<string, number>();
    for (const [pos, arr] of spByPos) {
      [...arr]
        .sort((a, b) => actualRank.get(a)! - actualRank.get(b)!)
        .forEach((p, i) => expectedPts.set(`${pos}|${i + 1}`, p.total_pts));
    }

    const spByKey = new Map(sp.map((p) => [`${p.player_name}|${p.pos}`, p]));
    for (const pick of skill) {
      if (pick.season !== yr) continue;
      const match = spByKey.get(`${pick.player_name}|${pick.pos}`);
      const totalPts = match ? match.total_pts : 0;
      const actual = match ? actualRank.get(match)! : maxActualRank + 1;
      const drafted = draftedRank.get(pick)!;
      const expected = expectedPts.get(`${pick.pos}|${drafted}`) ?? null;
      results.push({
        season: pick.season,
        owner: pick.owner,
        player_name: pick.player_name,
        pos: pick.pos,
        drafted_pos_rank: drafted,
        actual_pos_rank: actual,
        total_pts: totalPts,
        expected_pts: expected,
        value_diff: expected == null ? null : totalPts - expected,
        rank_diff: actual - drafted,
      });
    }
  }

  return results;
}
