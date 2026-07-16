// Shared league computations, ported from helpers.R.

import { ScheduleRow, StandingRow } from "./data";

export interface AlltimeRow {
  team: string;
  seasons: number;
  wins: number;
  losses: number;
  ties: number;
  winPct: number;
  pf: number;
  pa: number;
  pfPerGame: number;
  championships: number;
}

export function computeAlltimeStandings(standings: StandingRow[]): AlltimeRow[] {
  const byOwner = new Map<string, AlltimeRow>();
  for (const s of standings) {
    let row = byOwner.get(s.owner);
    if (!row) {
      row = {
        team: s.owner, seasons: 0, wins: 0, losses: 0, ties: 0,
        winPct: 0, pf: 0, pa: 0, pfPerGame: 0, championships: 0,
      };
      byOwner.set(s.owner, row);
    }
    row.seasons += 1;
    row.wins += s.h2h_wins;
    row.losses += s.h2h_losses;
    row.ties += s.h2h_ties ?? 0;
    row.pf += s.points_for;
    row.pa += s.points_against;
    if (s.league_rank === 1) row.championships += 1;
  }
  for (const row of byOwner.values()) {
    // Ties count as half a win, matching ESPN's own h2h_winpct.
    const games = row.wins + row.losses + row.ties;
    row.winPct = games > 0 ? (row.wins + 0.5 * row.ties) / games : 0;
    row.pfPerGame = games > 0 ? row.pf / games : 0;
  }
  return [...byOwner.values()].sort((a, b) => b.wins - a.wins || b.winPct - a.winPct);
}

export interface H2HRecord {
  owner: string;
  opponent: string;
  wins: number;
  losses: number;
  ties: number;
  games: number;
  winPct: number;
  pf: number;
  pa: number;
  avgMargin: number;
}

export function computeOwnerVsOwner(
  schedule: ScheduleRow[],
  regSeasonOnly = true
): H2HRecord[] {
  const games = schedule.filter(
    (g) => g.result != null && (!regSeasonOnly || g.game_type === "Regular Season")
  );
  const map = new Map<string, H2HRecord>();
  for (const g of games) {
    const key = `${g.team_owner}|${g.opponent_owner}`;
    let rec = map.get(key);
    if (!rec) {
      rec = {
        owner: g.team_owner, opponent: g.opponent_owner,
        wins: 0, losses: 0, ties: 0, games: 0, winPct: 0, pf: 0, pa: 0, avgMargin: 0,
      };
      map.set(key, rec);
    }
    rec.games += 1;
    if (g.result === "W") rec.wins += 1;
    else if (g.result === "L") rec.losses += 1;
    else rec.ties += 1;
    rec.pf += g.franchise_score;
    rec.pa += g.opponent_score;
  }
  for (const rec of map.values()) {
    rec.winPct = rec.games > 0 ? rec.wins / rec.games : 0;
    rec.avgMargin = rec.games > 0 ? (rec.pf - rec.pa) / rec.games : 0;
  }
  return [...map.values()].sort(
    (a, b) => a.owner.localeCompare(b.owner) || b.wins - a.wins
  );
}

export interface StreakResult {
  team: string;
  streak: number;
}

function longestStreakOf(schedule: ScheduleRow[], result: "W" | "L"): StreakResult {
  const byTeam = new Map<string, ScheduleRow[]>();
  for (const g of schedule) {
    const arr = byTeam.get(g.team_owner) ?? [];
    arr.push(g);
    byTeam.set(g.team_owner, arr);
  }
  let best: StreakResult = { team: "", streak: 0 };
  for (const [team, games] of byTeam) {
    games.sort((a, b) => a.season - b.season || a.week - b.week);
    let cur = 0;
    let max = 0;
    for (const g of games) {
      if (g.result === result) {
        cur += 1;
        if (cur > max) max = cur;
      } else {
        cur = 0;
      }
    }
    if (max > best.streak) best = { team, streak: max };
  }
  return best;
}

export const computeLongestWinStreak = (s: ScheduleRow[]) => longestStreakOf(s, "W");
export const computeLongestLossStreak = (s: ScheduleRow[]) => longestStreakOf(s, "L");
