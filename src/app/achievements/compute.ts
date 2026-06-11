// Achievement unlock computation, ported faithfully from app.R lines 2741-4084
// (output$achievements_gallery server logic).

import {
  getLeagueData,
  BENCH_SLOTS,
  type StandingRow,
  type ScheduleRow,
  type StarterRow,
} from "@/lib/data";
import { ACHIEVEMENTS, FAVORITE_TEAMS, NEGATIVE_IDS } from "./definitions";

export interface OwnerResult {
  unlocked: Record<string, boolean>;
  detail: Record<string, string>;
}

export interface AchievementsModel {
  allOwners: string[];
  results: Record<string, OwnerResult>;
  achValues: Record<string, number>;
  careerGames: Record<string, number>;
}

// Mimic R's paste0(round(x, digits)) — trailing zeros dropped.
export function rnd(x: number, digits = 1): string {
  const f = 10 ** digits;
  return String(Math.round(x * f) / f);
}

const posNorm = (pos: string): string =>
  pos === "DST" || pos === "D/ST" || pos === "DEF" ? "DST" : pos;

function groupMap<T>(rows: T[], key: (r: T) => string): Map<string, T[]> {
  const m = new Map<string, T[]>();
  for (const r of rows) {
    const k = key(r);
    const arr = m.get(k);
    if (arr) arr.push(r);
    else m.set(k, [r]);
  }
  return m;
}

function uniq<T>(xs: T[]): T[] {
  return [...new Set(xs)];
}

// R rank(-x): average rank in descending order
function rankDesc(values: number[]): number[] {
  return values.map((v) => {
    let greater = 0;
    let equal = 0;
    for (const w of values) {
      if (w > v) greater++;
      else if (w === v) equal++;
    }
    return greater + (equal + 1) / 2;
  });
}

interface StarterNorm extends StarterRow {
  pos_norm: string;
}

let modelCache: AchievementsModel | null = null;

export function computeAchievementsModel(): AchievementsModel {
  if (modelCache) return modelCache;

  const { standings, schedule, drafts, starters } = getLeagueData();

  // Owner ordering: active alphabetical, legacy ("Joe") at the end
  const owners = uniq(standings.map((s) => s.owner)).sort();
  const legacy = ["Joe"];
  const active = owners.filter((o) => !legacy.includes(o));
  const allOwners = [...active, ...legacy];

  const seasonsAll = uniq(standings.map((s) => s.season)).sort((a, b) => a - b);
  const standingsBySeason = groupMap(standings, (s) => String(s.season));

  // ---- Pre-compute season-wide data for achievements ----

  // Soft Schedule: lowest PA per season winner
  const softScheduleSeasons: { season: number; owner: string }[] = [];
  for (const yr of seasonsAll) {
    const rows = [...(standingsBySeason.get(String(yr)) ?? [])].sort(
      (a, b) => a.points_against - b.points_against
    );
    if (rows.length) softScheduleSeasons.push({ season: yr, owner: rows[0].owner });
  }

  // MVP: best regular season record per season
  const mvpSeasons: { season: number; owner: string }[] = [];
  for (const yr of seasonsAll) {
    const rows = [...(standingsBySeason.get(String(yr)) ?? [])].sort(
      (a, b) => b.h2h_wins - a.h2h_wins || b.points_for - a.points_for
    );
    if (rows.length) mvpSeasons.push({ season: yr, owner: rows[0].owner });
  }

  // Ninja: bottom 3 PF but made playoffs (top 4)
  // Really Bad Beat: 2nd highest PF but missed playoffs
  const ninjaSeasons: { season: number; owner: string }[] = [];
  const rbbSeasons: { season: number; owner: string }[] = [];
  for (const yr of seasonsAll) {
    const rows = standingsBySeason.get(String(yr)) ?? [];
    const ranks = rankDesc(rows.map((r) => r.points_for));
    rows.forEach((r, i) => {
      if (r.league_rank <= 4 && ranks[i] >= 8) ninjaSeasons.push({ season: yr, owner: r.owner });
      if (ranks[i] === 2 && r.league_rank > 4) rbbSeasons.push({ season: yr, owner: r.owner });
    });
  }

  // Rock Bottom: lowest scoring week of a season
  const scheduleSeasons = uniq(schedule.map((g) => g.season));
  const rockBottomSeasons: { season: number; owner: string }[] = [];
  for (const yr of scheduleSeasons) {
    const rows = [...schedule.filter((g) => g.season === yr)].sort(
      (a, b) => a.franchise_score - b.franchise_score
    );
    if (rows.length) rockBottomSeasons.push({ season: yr, owner: rows[0].team_owner });
  }

  // Kingslayer: ended a 5-0+ start streak
  const kingslayerEvents = new Map<string, { season: number; week: number; victim: string }[]>();
  const regAll = schedule.filter((g) => g.game_type === "Regular Season" && g.result !== null);
  for (const yr of uniq(regAll.map((g) => g.season))) {
    const yrData = [...regAll.filter((g) => g.season === yr)].sort((a, b) => a.week - b.week);
    for (const team of uniq(yrData.map((g) => g.team_owner))) {
      const teamGames = yrData.filter((g) => g.team_owner === team);
      if (teamGames.length >= 6) {
        const first5 = teamGames.slice(0, 5).map((g) => g.result);
        if (first5.every((r) => r === "W")) {
          const firstLoss = teamGames.filter((g) => g.week > 5).find((g) => g.result === "L");
          if (firstLoss) {
            const ks = firstLoss.opponent_owner;
            const arr = kingslayerEvents.get(ks) ?? [];
            arr.push({ season: yr, week: firstLoss.week, victim: team });
            kingslayerEvents.set(ks, arr);
          }
        }
      }
    }
  }
  const kingslayerOwners = new Set(kingslayerEvents.keys());

  // Top scorer at each position per season (single best player)
  // Best total at each position per season (sum of starter points)
  const POSITIONS = ["QB", "RB", "WR", "TE", "K", "DST"] as const;
  type Position = (typeof POSITIONS)[number];
  const topPosSeasons: Record<Position, Set<string>> = {
    QB: new Set(), RB: new Set(), WR: new Set(), TE: new Set(), K: new Set(), DST: new Set(),
  };
  const topPosInfo: Record<Position, { season: number; owner: string; player_name: string; pts: number }[]> = {
    QB: [], RB: [], WR: [], TE: [], K: [], DST: [],
  };
  const bestPosSeasons: Record<Position, Set<string>> = {
    QB: new Set(), RB: new Set(), WR: new Set(), TE: new Set(), K: new Set(), DST: new Set(),
  };
  const bestPosInfo: Record<Position, { season: number; owner: string; pts: number }[]> = {
    QB: [], RB: [], WR: [], TE: [], K: [], DST: [],
  };

  const startersNorm: StarterNorm[] = starters.map((s) => ({ ...s, pos_norm: posNorm(s.pos) }));
  const stWithOwner = startersNorm.filter((s) => !BENCH_SLOTS.has(s.lineup_slot));

  for (const position of POSITIONS) {
    const posData = stWithOwner.filter((s) => s.pos_norm === position);
    if (!posData.length) continue;

    // Single highest-scoring player by season
    const single = new Map<string, { season: number; owner: string; player_name: string; pts: number }>();
    for (const r of posData) {
      const k = `${r.season}|${r.owner}|${r.player_name}`;
      const e = single.get(k);
      if (e) e.pts += r.player_score ?? 0;
      else single.set(k, { season: r.season, owner: r.owner, player_name: r.player_name, pts: r.player_score ?? 0 });
    }
    const singleBySeason = groupMap([...single.values()], (e) => String(e.season));
    const topRows = [...singleBySeason.entries()]
      .sort((a, b) => Number(a[0]) - Number(b[0]))
      .map(([, rows]) => [...rows].sort((a, b) => b.pts - a.pts)[0]);
    topPosInfo[position] = topRows;
    for (const r of topRows) topPosSeasons[position].add(r.owner);

    // Total at position by season
    const totals = new Map<string, { season: number; owner: string; pts: number }>();
    for (const r of posData) {
      const k = `${r.season}|${r.owner}`;
      const e = totals.get(k);
      if (e) e.pts += r.player_score ?? 0;
      else totals.set(k, { season: r.season, owner: r.owner, pts: r.player_score ?? 0 });
    }
    const totalsBySeason = groupMap([...totals.values()], (e) => String(e.season));
    const bestRows = [...totalsBySeason.entries()]
      .sort((a, b) => Number(a[0]) - Number(b[0]))
      .map(([, rows]) => [...rows].sort((a, b) => b.pts - a.pts)[0]);
    bestPosInfo[position] = bestRows;
    for (const r of bestRows) bestPosSeasons[position].add(r.owner);
  }

  // Sacko row per season (lowest wins, then lowest PF)
  const sackoBySeason = new Map<number, StandingRow>();
  for (const yr of seasonsAll) {
    const rows = [...(standingsBySeason.get(String(yr)) ?? [])].sort(
      (a, b) => a.h2h_wins - b.h2h_wins || a.points_for - b.points_for
    );
    if (rows.length) sackoBySeason.set(yr, rows[0]);
  }

  // By the Skin of his...: tied sacko record but finished higher on PF
  const skinOwners = new Set<string>();
  for (const yr of uniq(standings.map((s) => s.season))) {
    const sackoRow = sackoBySeason.get(yr);
    if (!sackoRow) continue;
    for (const s of standingsBySeason.get(String(yr)) ?? []) {
      if (s.h2h_wins === sackoRow.h2h_wins && s.owner !== sackoRow.owner && s.points_for > sackoRow.points_for) {
        skinOwners.add(s.owner);
      }
    }
  }

  // Pokedex: career distinct rostered players
  const careerSets = new Map<string, Set<string>>();
  const seasonSets = new Map<string, Set<string>>(); // owner|season
  for (const s of startersNorm) {
    let cs = careerSets.get(s.owner);
    if (!cs) careerSets.set(s.owner, (cs = new Set()));
    cs.add(s.player_name);
    const sk = `${s.owner}|${s.season}`;
    let ss = seasonSets.get(sk);
    if (!ss) seasonSets.set(sk, (ss = new Set()));
    ss.add(s.player_name);
  }
  const careerRostered = new Map<string, number>();
  for (const [o, set] of careerSets) careerRostered.set(o, set.size);
  const seasonRostered: { owner: string; season: number; distinct_players: number }[] = [];
  for (const [k, set] of seasonSets) {
    const [o, yr] = k.split("|");
    seasonRostered.push({ owner: o, season: Number(yr), distinct_players: set.size });
  }

  // Mr. Relevant: last-round pick started 10+ weeks in a season
  const maxRoundBySeason = new Map<number, number>();
  for (const dr of drafts) {
    const cur = maxRoundBySeason.get(dr.season);
    if (cur === undefined || dr.round > cur) maxRoundBySeason.set(dr.season, dr.round);
  }
  const startCounts = new Map<string, number>(); // season|owner|player -> starts
  for (const s of stWithOwner) {
    const k = `${s.season}|${s.owner}|${s.player_name}`;
    startCounts.set(k, (startCounts.get(k) ?? 0) + 1);
  }
  const mrRelevantInfo: { season: number; owner: string; player_name: string; starts: number }[] = [];
  const mrSeen = new Set<string>();
  for (const dr of drafts) {
    if (dr.round !== maxRoundBySeason.get(dr.season)) continue;
    const k = `${dr.season}|${dr.owner}|${dr.player_name}`;
    if (mrSeen.has(k)) continue;
    mrSeen.add(k);
    const starts = startCounts.get(k) ?? 0;
    if (starts >= 10) {
      mrRelevantInfo.push({ season: dr.season, owner: dr.owner, player_name: dr.player_name, starts });
    }
  }
  const mrRelevantOwners = new Set(mrRelevantInfo.map((r) => r.owner));

  // Repeat & Threepeat championships (includes Connor's 2016 championship)
  const champYearsByOwner = new Map<string, number[]>();
  for (const s of standings) {
    if (s.league_rank === 1) {
      const arr = champYearsByOwner.get(s.owner) ?? [];
      arr.push(s.season);
      champYearsByOwner.set(s.owner, arr);
    }
  }
  champYearsByOwner.set("Connor", [...(champYearsByOwner.get("Connor") ?? []), 2016]);
  const repeatOwners = new Set<string>();
  const threepeatOwners = new Set<string>();
  const repeatYears = new Map<string, [number, number]>();
  const threepeatYears = new Map<string, [number, number, number]>();
  for (const [oChk, yrsRaw] of champYearsByOwner) {
    const yrs = [...yrsRaw].sort((a, b) => a - b);
    if (yrs.length >= 2) {
      for (let i = 1; i < yrs.length; i++) {
        if (yrs[i] - yrs[i - 1] === 1) {
          repeatOwners.add(oChk);
          repeatYears.set(oChk, [yrs[i - 1], yrs[i]]);
          break;
        }
      }
    }
    if (yrs.length >= 3) {
      for (let i = 2; i < yrs.length; i++) {
        if (yrs[i] - yrs[i - 1] === 1 && yrs[i - 1] - yrs[i - 2] === 1) {
          threepeatOwners.add(oChk);
          threepeatYears.set(oChk, [yrs[i - 2], yrs[i - 1], yrs[i]]);
          break;
        }
      }
    }
  }

  // Makes the Dream Work, Singled Out, Benchwarmers, Negative, Double Negative
  const startingOnly = stWithOwner;
  const benchOnly = startersNorm.filter((s) => s.lineup_slot === "BE");

  // Dream Work: 4+ starters from same NFL team in a week
  const dreamCounts = new Map<string, { season: number; week: number; owner: string; team: string; n: number }>();
  for (const s of startingOnly) {
    const team = s.team ?? "NA";
    const k = `${s.season}|${s.week}|${s.owner}|${team}`;
    const e = dreamCounts.get(k);
    if (e) e.n += 1;
    else dreamCounts.set(k, { season: s.season, week: s.week, owner: s.owner, team, n: 1 });
  }
  const dreamEvents = [...dreamCounts.values()].filter((e) => e.n >= 4);
  const dreamWorkOwners = new Set(dreamEvents.map((e) => e.owner));

  const weeklyGroups = groupMap(startingOnly, (s) => `${s.season}|${s.week}|${s.owner}`);

  // Singled Out: all starters scored under 10 in a week (8+ starters)
  const singledEvents: { season: number; week: number; owner: string }[] = [];
  for (const [k, rows] of weeklyGroups) {
    const scores = rows.map((r) => r.player_score).filter((x): x is number => x !== null);
    const maxScore = scores.length ? Math.max(...scores) : -Infinity;
    if (maxScore < 10 && rows.length >= 8) {
      const [yr, wk, ow] = k.split("|");
      singledEvents.push({ season: Number(yr), week: Number(wk), owner: ow });
    }
  }
  const singledOutOwners = new Set(singledEvents.map((e) => e.owner));

  // Benchwarmers: total bench points > total starter points in a week
  const benchTotals = new Map<string, number>();
  for (const s of benchOnly) {
    const k = `${s.season}|${s.week}|${s.owner}`;
    benchTotals.set(k, (benchTotals.get(k) ?? 0) + (s.player_score ?? 0));
  }
  const benchwarmersEvents: { season: number; week: number; owner: string }[] = [];
  for (const [k, rows] of weeklyGroups) {
    const benchPts = benchTotals.get(k);
    if (benchPts === undefined) continue; // inner join
    const startPts = rows.reduce((acc, r) => acc + (r.player_score ?? 0), 0);
    if (benchPts > startPts) {
      const [yr, wk, ow] = k.split("|");
      benchwarmersEvents.push({ season: Number(yr), week: Number(wk), owner: ow });
    }
  }
  const benchwarmersOwners = new Set(benchwarmersEvents.map((e) => e.owner));

  // Negative scores from non-K/DST starters
  const nonKdst = startingOnly.filter((s) => !["K", "DST", "D/ST", "DEF"].includes(s.pos));
  const negRows = nonKdst.filter((s) => s.player_score !== null && s.player_score < 0);
  const negGroups = groupMap(negRows, (s) => `${s.season}|${s.week}|${s.owner}`);
  const negativeEvents: { season: number; week: number; owner: string; player_name: string; player_score: number }[] = [];
  const doubleNegEvents: { season: number; week: number; owner: string; n: number }[] = [];
  const negativeOwners = new Set<string>();
  const doubleNegativeOwners = new Set<string>();
  for (const [k, rows] of negGroups) {
    const sorted = [...rows].sort((a, b) => (a.player_score ?? 0) - (b.player_score ?? 0));
    const [yr, wk, ow] = k.split("|");
    negativeEvents.push({
      season: Number(yr), week: Number(wk), owner: ow,
      player_name: sorted[0].player_name, player_score: sorted[0].player_score ?? 0,
    });
    negativeOwners.add(ow);
    if (rows.length >= 2) {
      doubleNegEvents.push({ season: Number(yr), week: Number(wk), owner: ow, n: rows.length });
      doubleNegativeOwners.add(ow);
    }
  }

  // Consistency (3 straight) and Is Key (5 straight): scores within 10 pts of each other
  const consistencyFirst = new Map<string, number>();
  const isKeyFirst = new Map<string, number>();
  for (const oChk of uniq(schedule.map((g) => g.team_owner))) {
    const ownerWeeks = [...schedule.filter((g) => g.team_owner === oChk)].sort(
      (a, b) => a.season - b.season || a.week - b.week
    );
    for (const yr of uniq(ownerWeeks.map((g) => g.season)).sort((a, b) => a - b)) {
      const wks = ownerWeeks.filter((g) => g.season === yr).map((g) => g.franchise_score);
      if (wks.length >= 3 && !consistencyFirst.has(oChk)) {
        for (let i = 2; i < wks.length; i++) {
          const win = wks.slice(i - 2, i + 1);
          if (Math.max(...win) - Math.min(...win) <= 10) {
            consistencyFirst.set(oChk, yr);
            break;
          }
        }
      }
      if (wks.length >= 5 && !isKeyFirst.has(oChk)) {
        for (let i = 4; i < wks.length; i++) {
          const win = wks.slice(i - 4, i + 1);
          if (Math.max(...win) - Math.min(...win) <= 10) {
            isKeyFirst.set(oChk, yr);
            break;
          }
        }
      }
    }
  }
  const consistencyOwners = new Set(consistencyFirst.keys());
  const isKeyOwners = new Set(isKeyFirst.keys());

  // Perfectly Average: never the highest or lowest in any week of a season
  const perfectlyAvgOwners = new Set<string>();
  const perfectlyAvgFirst = new Map<string, number>();
  for (const yr of [...scheduleSeasons].sort((a, b) => a - b)) {
    const yrSc = schedule.filter((g) => g.season === yr);
    const highLow = new Set<string>();
    for (const wk of uniq(yrSc.map((g) => g.week))) {
      const wkSc = yrSc.filter((g) => g.week === wk);
      if (!wkSc.length) continue;
      let hi = wkSc[0];
      let lo = wkSc[0];
      for (const g of wkSc) {
        if (g.franchise_score > hi.franchise_score) hi = g;
        if (g.franchise_score < lo.franchise_score) lo = g;
      }
      highLow.add(hi.team_owner);
      highLow.add(lo.team_owner);
    }
    for (const s of uniq(yrSc.map((g) => g.team_owner))) {
      if (!highLow.has(s)) {
        perfectlyAvgOwners.add(s);
        if (!perfectlyAvgFirst.has(s)) perfectlyAvgFirst.set(s, yr);
      }
    }
  }

  // Boomer: highest scoring week of an entire season
  // Boomer Boomer: 3+ different seasons with that honor
  const boomerInfoByOwner = new Map<string, { season: number; week: number; pts: number }[]>();
  for (const yr of scheduleSeasons) {
    const yrSc = schedule.filter((g) => g.season === yr);
    if (!yrSc.length) continue;
    let top = yrSc[0];
    for (const g of yrSc) if (g.franchise_score > top.franchise_score) top = g;
    const arr = boomerInfoByOwner.get(top.team_owner) ?? [];
    arr.push({ season: yr, week: top.week, pts: top.franchise_score });
    boomerInfoByOwner.set(top.team_owner, arr);
  }
  const boomerOwners = new Set(boomerInfoByOwner.keys());
  const boomerBoomerOwners = new Set(
    [...boomerInfoByOwner.entries()].filter(([, v]) => v.length >= 3).map(([k]) => k)
  );

  // Late Bloomer: started 0-3 and made playoffs
  // Ran Out of Gas: started 3-0 and missed playoffs
  const lateBloomerSeasons = new Map<string, number[]>();
  const ranOutOfGasSeasons = new Map<string, number[]>();
  const regOnly = schedule.filter((g) => g.game_type === "Regular Season");
  for (const yr of uniq(regOnly.map((g) => g.season))) {
    const yrGames = regOnly.filter((g) => g.season === yr && g.result !== null);
    for (const oChk of uniq(yrGames.map((g) => g.team_owner))) {
      const first3 = [...yrGames.filter((g) => g.team_owner === oChk)]
        .sort((a, b) => a.week - b.week)
        .slice(0, 3);
      if (first3.length < 3) continue;
      const madePlayoffs = standings.some(
        (s) => s.season === yr && s.owner === oChk && s.league_rank <= 4
      );
      if (first3.every((g) => g.result === "L") && madePlayoffs) {
        lateBloomerSeasons.set(oChk, [...(lateBloomerSeasons.get(oChk) ?? []), yr]);
      }
      if (first3.every((g) => g.result === "W") && !madePlayoffs) {
        ranOutOfGasSeasons.set(oChk, [...(ranOutOfGasSeasons.get(oChk) ?? []), yr]);
      }
    }
  }
  const lateBloomerOwners = new Set(lateBloomerSeasons.keys());
  const ranOutOfGasOwners = new Set(ranOutOfGasSeasons.keys());

  // Dynasty That Was Promised / Historic Futility / Like We Never Left
  const dynastyPromisedOwners = new Set<string>();
  const dynastyPromisedDetail = new Map<string, string>();
  const historicFutilityOwners = new Set<string>();
  const historicFutilityDetail = new Map<string, string>();
  const neverLeftOwners = new Set<string>();
  const neverLeftDetail = new Map<string, string>();
  for (const oChk of uniq(standings.map((s) => s.owner))) {
    const os = [...standings.filter((s) => s.owner === oChk)]
      .sort((a, b) => a.season - b.season)
      .map((s) => ({ season: s.season, made: s.league_rank <= 4 }));
    if (os.length < 5) continue;
    // Historic Futility: 5 consecutive missed playoff seasons
    let run = 0;
    let firstFut: number | null = null;
    for (let i = 0; i < os.length; i++) {
      if (!os[i].made) {
        run++;
        if (run === 5) {
          firstFut = os[i - 4].season;
          break;
        }
      } else run = 0;
    }
    if (firstFut !== null) {
      historicFutilityOwners.add(oChk);
      historicFutilityDetail.set(oChk, `${firstFut}-${firstFut + 4}`);
    }
    if (os.length >= 6) {
      // Dynasty Promised: 5 in 6
      for (let i = 0; i + 5 < os.length; i++) {
        const w = os.slice(i, i + 6);
        if (w.filter((x) => x.made).length >= 5) {
          dynastyPromisedOwners.add(oChk);
          dynastyPromisedDetail.set(oChk, `${w[0].season}-${w[5].season}`);
          break;
        }
      }
      // Never Left: 3 misses then 3 makes
      for (let i = 0; i + 5 < os.length; i++) {
        const w = os.slice(i, i + 6);
        if (w.slice(0, 3).every((x) => !x.made) && w.slice(3, 6).every((x) => x.made)) {
          neverLeftOwners.add(oChk);
          neverLeftDetail.set(oChk, `${w[0].season}-${w[5].season}`);
          break;
        }
      }
    }
  }

  // Always the Bridesmaid: 3+ runner-ups, 0 titles
  const bridesmaidOwners = new Set<string>();
  const bridesmaidDetail = new Map<string, string>();
  for (const oChk of uniq(standings.map((s) => s.owner))) {
    const titles = standings.filter((s) => s.owner === oChk && s.league_rank === 1).length;
    const runners = standings.filter((s) => s.owner === oChk && s.league_rank === 2).length;
    if (runners >= 3 && titles === 0) {
      bridesmaidOwners.add(oChk);
      const yrs = standings
        .filter((s) => s.owner === oChk && s.league_rank === 2)
        .map((s) => s.season)
        .sort((a, b) => a - b);
      bridesmaidDetail.set(oChk, `Runner-up: ${yrs.join(", ")}`);
    }
  }

  // Points Machine: opoy 3+ times
  const opoyByOwner: { season: number; owner: string }[] = [];
  for (const yr of seasonsAll) {
    const rows = [...(standingsBySeason.get(String(yr)) ?? [])].sort(
      (a, b) => b.points_for - a.points_for
    );
    if (rows.length) opoyByOwner.push({ season: yr, owner: rows[0].owner });
  }
  const pointsMachineOwners = new Set<string>();
  const pointsMachineDetail = new Map<string, string>();
  for (const oChk of uniq(opoyByOwner.map((r) => r.owner))) {
    const yrs = opoyByOwner.filter((r) => r.owner === oChk).map((r) => r.season).sort((a, b) => a - b);
    if (yrs.length >= 3) {
      pointsMachineOwners.add(oChk);
      pointsMachineDetail.set(oChk, `Years: ${yrs.join(", ")}`);
    }
  }

  // All Pain, No Gain: led league in points against a season
  const paLeaders: { season: number; owner: string; league_rank: number }[] = [];
  for (const yr of seasonsAll) {
    const rows = [...(standingsBySeason.get(String(yr)) ?? [])].sort(
      (a, b) => b.points_against - a.points_against
    );
    if (rows.length) paLeaders.push({ season: yr, owner: rows[0].owner, league_rank: rows[0].league_rank });
  }
  const allPainOwners = new Set(paLeaders.map((r) => r.owner));
  const allPainDetail = new Map<string, string>();
  for (const oChk of allPainOwners) {
    const yrs = paLeaders.filter((r) => r.owner === oChk).map((r) => r.season).sort((a, b) => a - b);
    allPainDetail.set(oChk, `First: ${yrs[0]}`);
  }
  // Overcoming Adversity: led PA AND made playoffs
  const adversityRows = paLeaders.filter((r) => r.league_rank <= 4);
  const overcomingAdversityOwners = new Set(adversityRows.map((r) => r.owner));
  const overcomingAdversityDetail = new Map<string, string>();
  for (const oChk of overcomingAdversityOwners) {
    const yrs = adversityRows.filter((r) => r.owner === oChk).map((r) => r.season).sort((a, b) => a - b);
    overcomingAdversityDetail.set(oChk, `First: ${yrs[0]}`);
  }

  // Villain Arc: champion in Y, sacko in Y-1
  const villainArcOwners = new Set<string>();
  const villainArcDetail = new Map<string, string>();
  for (const cr of standings.filter((s) => s.league_rank === 1)) {
    const sackoPrev = sackoBySeason.get(cr.season - 1);
    if (sackoPrev && sackoPrev.owner === cr.owner) {
      villainArcOwners.add(cr.owner);
      if (!villainArcDetail.has(cr.owner)) {
        villainArcDetail.set(cr.owner, `Sacko ${cr.season - 1} → Champ ${cr.season}`);
      }
    }
  }

  // Never Looked Back: #1 in standings (by wins, then points_for) after every week from week 2 onward
  const neverLookedBackOwners = new Set<string>();
  const neverLookedBackDetail = new Map<string, string>();
  for (const yr of [...scheduleSeasons].sort((a, b) => a - b)) {
    const yrSc = schedule.filter((g) => g.season === yr && g.result !== null);
    if (!yrSc.length) continue;
    const regYr = yrSc.filter((g) => g.game_type === "Regular Season");
    const nWeeks = uniq(regYr.map((g) => g.week)).length;
    if (nWeeks < 3) continue;
    const leaders: string[] = [];
    for (let wk = 2; wk <= nWeeks; wk++) {
      const thru = regYr.filter((g) => g.week <= wk);
      const agg = new Map<string, { w: number; pf: number }>();
      for (const g of thru) {
        const e = agg.get(g.team_owner) ?? { w: 0, pf: 0 };
        if (g.result === "W") e.w += 1;
        e.pf += g.franchise_score;
        agg.set(g.team_owner, e);
      }
      const sorted = [...agg.entries()]
        .sort((a, b) => a[0].localeCompare(b[0]))
        .sort((a, b) => b[1].w - a[1].w || b[1].pf - a[1].pf);
      if (!sorted.length) break;
      leaders.push(sorted[0][0]);
    }
    if (leaders.length >= 2 && new Set(leaders).size === 1) {
      const oLed = leaders[0];
      neverLookedBackOwners.add(oLed);
      if (!neverLookedBackDetail.has(oLed)) neverLookedBackDetail.set(oLed, `Season: ${yr}`);
    }
  }

  // Escape Artist: in last place going into final week, finish not-last
  const escapeArtistOwners = new Set<string>();
  const escapeArtistDetail = new Map<string, string>();
  for (const yr of [...scheduleSeasons].sort((a, b) => a - b)) {
    const yrSc = schedule.filter((g) => g.season === yr && g.result !== null);
    if (!yrSc.length) continue;
    const regYr = yrSc.filter((g) => g.game_type === "Regular Season");
    const nWeeks = uniq(regYr.map((g) => g.week)).length;
    if (nWeeks < 2) continue;
    const finalWk = Math.max(...regYr.map((g) => g.week));
    const agg = new Map<string, { w: number; pf: number }>();
    for (const g of regYr.filter((x) => x.week < finalWk)) {
      const e = agg.get(g.team_owner) ?? { w: 0, pf: 0 };
      if (g.result === "W") e.w += 1;
      e.pf += g.franchise_score;
      agg.set(g.team_owner, e);
    }
    const sorted = [...agg.entries()]
      .sort((a, b) => a[0].localeCompare(b[0]))
      .sort((a, b) => a[1].w - b[1].w || a[1].pf - b[1].pf);
    if (!sorted.length) continue;
    const lastOwner = sorted[0][0];
    const endSacko = sackoBySeason.get(yr)?.owner;
    if (endSacko === undefined) continue;
    if (lastOwner !== endSacko) {
      escapeArtistOwners.add(lastOwner);
      if (!escapeArtistDetail.has(lastOwner)) escapeArtistDetail.set(lastOwner, `Season: ${yr}`);
    }
  }

  // Perfectly Wrong: every starter had a same-position bench player who scored more
  const perfectlyWrongOwners = new Set<string>();
  const perfectlyWrongDetail = new Map<string, string>();
  const pwGroups = groupMap(startersNorm, (s) => `${s.season}|${s.week}|${s.owner}`);
  for (const [key, wkData] of pwGroups) {
    const startersWk = wkData.filter((s) => !BENCH_SLOTS.has(s.lineup_slot));
    const benchWk = wkData.filter((s) => s.lineup_slot === "BE");
    if (startersWk.length < 8 || !benchWk.length) continue;
    let allWrong = true;
    for (const s of startersWk) {
      const benchSame = benchWk.filter((b) => b.pos_norm === s.pos_norm);
      if (!benchSame.length) {
        allWrong = false;
        break;
      }
      const sScore = s.player_score;
      const beaten = benchSame.some(
        (b) => b.player_score !== null && sScore !== null && b.player_score > sScore
      );
      if (!beaten) {
        allWrong = false;
        break;
      }
    }
    if (allWrong) {
      const [yr, wk, ow] = key.split("|");
      perfectlyWrongOwners.add(ow);
      if (!perfectlyWrongDetail.has(ow)) perfectlyWrongDetail.set(ow, `${yr} Wk ${wk}`);
    }
  }

  // Streaming Savant: 3 straight wins with a different QB each week
  const streamingSavantOwners = new Set<string>();
  const streamingSavantDetail = new Map<string, string>();
  {
    const qbStarts = startersNorm.filter(
      (s) => s.pos_norm === "QB" && !BENCH_SLOTS.has(s.lineup_slot)
    );
    const qbGroups = groupMap(qbStarts, (s) => `${s.season}|${s.week}|${s.owner}`);
    const resultByKey = new Map<string, string>();
    for (const g of schedule) {
      if (g.result !== null) resultByKey.set(`${g.season}|${g.week}|${g.team_owner}`, g.result);
    }
    const owWk: { season: number; week: number; owner: string; qb: string; result: string }[] = [];
    for (const [key, rows] of qbGroups) {
      let best = rows[0];
      for (const r of rows) {
        if ((r.player_score ?? -Infinity) > (best.player_score ?? -Infinity)) best = r;
      }
      const result = resultByKey.get(key);
      if (result === undefined) continue; // inner join
      const [yr, wk, ow] = key.split("|");
      owWk.push({ season: Number(yr), week: Number(wk), owner: ow, qb: best.player_name, result });
    }
    owWk.sort((a, b) =>
      a.owner.localeCompare(b.owner) || a.season - b.season || a.week - b.week
    );
    for (const oChk of uniq(owWk.map((r) => r.owner))) {
      const rows = owWk.filter((r) => r.owner === oChk);
      if (rows.length < 3) continue;
      for (let i = 0; i + 2 < rows.length; i++) {
        const trio = rows.slice(i, i + 3);
        const winTriple = trio.every((r) => r.result === "W");
        const sameSeason = new Set(trio.map((r) => r.season)).size === 1;
        const consecutive = trio[1].week - trio[0].week === 1 && trio[2].week - trio[1].week === 1;
        const diffQbs = new Set(trio.map((r) => r.qb)).size === 3;
        if (winTriple && sameSeason && consecutive && diffQbs) {
          streamingSavantOwners.add(oChk);
          streamingSavantDetail.set(oChk, `${trio[0].season} Wk ${trio[0].week}-${trio[2].week}`);
          break;
        }
      }
    }
  }

  // ---- Per-owner achievement computation (compute_achievements in app.R) ----

  function computeAchievements(o: string): OwnerResult {
    const ownerSt = standings.filter((s) => s.owner === o);
    const ownerSc = schedule.filter((g) => g.result !== null);
    const ownerScAll = ownerSc.filter((g) => g.team_owner === o);
    const regSc = ownerScAll.filter((g) => g.game_type === "Regular Season");

    // Most PF seasons
    const mostPfSeasons = opoyByOwner.filter((r) => r.owner === o);
    // Sackos
    const sackoSeasons = seasonsAll
      .filter((yr) => sackoBySeason.get(yr)?.owner === o)
      .map((yr) => ({ season: yr }));

    const careerWins = ownerSt.reduce((a, s) => a + s.h2h_wins, 0);
    const careerLosses = ownerSt.reduce((a, s) => a + s.h2h_losses, 0);
    const careerPf = ownerSt.reduce((a, s) => a + s.points_for, 0);
    const totalSeasons = ownerSt.length;
    const maxWeek = regSc.length ? Math.max(...regSc.map((g) => g.franchise_score)) : 0;
    const maxMargin = regSc.length
      ? Math.max(...regSc.map((g) => g.franchise_score - g.opponent_score))
      : 0;
    const undefeated = ownerSt.some((s) => s.h2h_losses === 0 && s.h2h_wins > 0);

    // Game of Inches: win by 1 pt or less
    const gameOfInches = regSc.some(
      (g) => g.franchise_score - g.opponent_score > 0 && g.franchise_score - g.opponent_score <= 1
    );
    // Blowout: win by 50+
    const blowout50 = regSc.some((g) => g.franchise_score - g.opponent_score >= 50);

    // Gauntlet: beat every owner in one season
    let gauntlet = false;
    for (const yr of uniq(regSc.map((g) => g.season))) {
      const beaten = new Set(
        regSc.filter((g) => g.season === yr && g.result === "W").map((g) => g.opponent_owner)
      );
      const allOppsYr = uniq(regSc.filter((g) => g.season === yr).map((g) => g.opponent_owner));
      if (allOppsYr.length > 0 && allOppsYr.every((opp) => beaten.has(opp))) {
        gauntlet = true;
        break;
      }
    }

    // Bad Beat: lose as 2nd highest scorer that week
    const losses = regSc.filter((g) => g.result === "L");
    const badBeatWeek = (row: ScheduleRow): boolean => {
      const seen = new Set<string>();
      const weekAll: ScheduleRow[] = [];
      for (const g of schedule) {
        if (g.season === row.season && g.week === row.week && !seen.has(g.team_owner)) {
          seen.add(g.team_owner);
          weekAll.push(g);
        }
      }
      weekAll.sort((a, b) => b.franchise_score - a.franchise_score);
      return weekAll.length >= 2 && weekAll[1].team_owner === o;
    };
    let badBeat = false;
    let badBeatDetail: string | null = null;
    for (const row of losses) {
      if (badBeatWeek(row)) {
        badBeat = true;
        badBeatDetail = `${row.season} Wk ${row.week}`;
        break;
      }
    }

    // Winning streaks (within season)
    const ownerGamesSorted = [...regSc].sort((a, b) => a.season - b.season || a.week - b.week);
    const seasonStreak = (yr: number): number => {
      let cur = 0;
      let m = 0;
      for (const g of ownerGamesSorted.filter((x) => x.season === yr)) {
        if (g.result === "W") {
          cur += 1;
          if (cur > m) m = cur;
        } else cur = 0;
      }
      return m;
    };
    const seasonsOfOwner = uniq(ownerGamesSorted.map((g) => g.season));
    let maxWinStreak = 0;
    for (const yr of seasonsOfOwner) maxWinStreak = Math.max(maxWinStreak, seasonStreak(yr));

    // Starter-based achievements (00 Agent, Double D's, Everyone Eats, kicker stuff)
    const ownSt = startersNorm.filter((s) => s.owner === o);
    const starting = ownSt.filter((s) => !BENCH_SLOTS.has(s.lineup_slot));

    // 00 Agent: 2+ starters with 0 in same week
    const zeroCounts = new Map<string, { season: number; week: number; n: number }>();
    for (const s of starting) {
      if (s.player_score === 0) {
        const k = `${s.season}|${s.week}`;
        const e = zeroCounts.get(k);
        if (e) e.n += 1;
        else zeroCounts.set(k, { season: s.season, week: s.week, n: 1 });
      }
    }
    const zeroWks = [...zeroCounts.values()]
      .filter((e) => e.n >= 2)
      .sort((a, b) => a.season - b.season || a.week - b.week);
    const doubleZero = zeroWks.length > 0;
    const zeroWksDetail = doubleZero ? zeroWks[0] : null;

    // Weekly min-score per week
    const ownWeekly = groupMap(starting, (s) => `${s.season}|${s.week}`);
    const weeklyMins: { season: number; week: number; minScore: number; nStart: number }[] = [];
    for (const [k, rows] of ownWeekly) {
      const scores = rows.map((r) => r.player_score).filter((x): x is number => x !== null);
      const minScore = scores.length ? Math.min(...scores) : Infinity;
      const [yr, wk] = k.split("|");
      weeklyMins.push({ season: Number(yr), week: Number(wk), minScore, nStart: rows.length });
    }
    weeklyMins.sort((a, b) => a.season - b.season || a.week - b.week);

    // Double D's: every starter 10+
    const weeklyTen = weeklyMins.filter((e) => e.nStart >= 8 && e.minScore >= 10);
    const doubleDs = weeklyTen.length > 0;
    const doubleDsDetail = doubleDs ? weeklyTen[0] : null;

    // Everyone Eats: every starter 6+ (proxy for TD)
    const weeklySix = weeklyMins.filter((e) => e.nStart >= 8 && e.minScore >= 6);
    const everyoneEats = weeklySix.length > 0;
    const eatsDetail = everyoneEats ? weeklySix[0] : null;

    // Why?: 3+ kickers on roster at same time
    const kickerCounts = new Map<string, { season: number; week: number; n: number }>();
    for (const s of ownSt) {
      if (s.pos === "K") {
        const k = `${s.season}|${s.week}`;
        const e = kickerCounts.get(k);
        if (e) e.n += 1;
        else kickerCounts.set(k, { season: s.season, week: s.week, n: 1 });
      }
    }
    const kickerWeeks = [...kickerCounts.values()]
      .filter((e) => e.n >= 3)
      .sort((a, b) => a.season - b.season || a.week - b.week);
    const why = kickerWeeks.length > 0;
    const whyDetail = why ? kickerWeeks[0] : null;

    // Top scoring starter each week (first max, ties broken by data order)
    const topStarterPos: StarterNorm[] = [];
    for (const [, rows] of ownWeekly) {
      let best = rows[0];
      for (const r of rows) {
        if ((r.player_score ?? -Infinity) > (best.player_score ?? -Infinity)) best = r;
      }
      topStarterPos.push(best);
    }
    topStarterPos.sort((a, b) => a.season - b.season || a.week - b.week);

    // Ban Kickers: kicker is top scoring starter
    const kRows = topStarterPos.filter((s) => s.pos === "K");
    const banKickers = kRows.length > 0;
    const kickerDetail = banKickers ? kRows[0] : null;

    // Wins Championships: defense is top scoring starter
    const dstRows = topStarterPos.filter((s) => ["DST", "D/ST", "DEF"].includes(s.pos));
    const winsChamps = dstRows.length > 0;
    const dstDetail = winsChamps ? dstRows[0] : null;

    // Homer: 3+ players from favorite NFL team in a draft
    const favTeam = FAVORITE_TEAMS[o];
    let homer = false;
    let homerRows: { season: number; team: string; n: number }[] = [];
    if (favTeam) {
      const counts = new Map<string, { season: number; team: string; n: number }>();
      for (const dr of drafts) {
        if (dr.owner !== o) continue;
        const team = dr.team ?? "NA";
        const k = `${dr.season}|${team}`;
        const e = counts.get(k);
        if (e) e.n += 1;
        else counts.set(k, { season: dr.season, team, n: 1 });
      }
      homerRows = [...counts.values()]
        .filter((e) => e.team === favTeam && e.n >= 3)
        .sort((a, b) => a.season - b.season);
      homer = homerRows.length > 0;
    }

    const softSchedule = softScheduleSeasons.some((r) => r.owner === o);
    const mvp = mvpSeasons.some((r) => r.owner === o);
    const ninja = ninjaSeasons.some((r) => r.owner === o);
    const reallyBadBeat = rbbSeasons.some((r) => r.owner === o);

    // ---- Compute first-unlock detail strings ----
    const d: Record<string, string> = {};

    // First win
    if (careerWins >= 1) {
      const winsDf = [...regSc.filter((g) => g.result === "W")].sort(
        (a, b) => a.season - b.season || a.week - b.week
      );
      if (winsDf.length) {
        const r = winsDf[0];
        d.first_win = `${r.season} Wk ${r.week} vs ${r.opponent_owner}`;
      }
    }
    // Champions
    const champYrs = ownerSt.filter((s) => s.league_rank === 1).map((s) => s.season).sort((a, b) => a - b);
    if (champYrs.length > 0) d.champion = `Won: ${champYrs.join(", ")}`;
    if (champYrs.length >= 2) d.dynasty = `Titles: ${champYrs.join(", ")}`;
    // Title game (any rank 1 or 2)
    const finalYrs = ownerSt.filter((s) => s.league_rank <= 2).map((s) => s.season).sort((a, b) => a - b);
    if (finalYrs.length > 0) d.title_game = `First: ${finalYrs[0]}`;
    // One seed (best record)
    const oneSeedRows = mvpSeasons.filter((r) => r.owner === o).map((r) => r.season).sort((a, b) => a - b);
    if (oneSeedRows.length > 0) d.one_seed = `First: ${oneSeedRows[0]}`;
    // Playoffs
    const playoffYrs = ownerSt.filter((s) => s.league_rank <= 4).map((s) => s.season).sort((a, b) => a - b);
    if (playoffYrs.length > 0) d.playoff_bound = `First: ${playoffYrs[0]}`;
    // MVP
    const mvpYrs = mvpSeasons.filter((r) => r.owner === o).map((r) => r.season).sort((a, b) => a - b);
    if (mvpYrs.length > 0) d.mvp = `First: ${mvpYrs[0]}`;
    // OPOY
    const opoyYrs = mostPfSeasons.map((r) => r.season).sort((a, b) => a - b);
    if (opoyYrs.length > 0) d.opoy = `First: ${opoyYrs[0]}`;
    // Century / 150 / 200 point weeks
    const bigWks = [...regSc].sort((a, b) => a.season - b.season || a.week - b.week);
    if (bigWks.length > 0) {
      const r100 = bigWks.find((g) => g.franchise_score >= 100);
      const r150 = bigWks.find((g) => g.franchise_score >= 150);
      const r200 = bigWks.find((g) => g.franchise_score >= 200);
      if (r100) d.century = `${r100.season} Wk ${r100.week} (${rnd(r100.franchise_score)} pts)`;
      if (r150) d.one_fifty = `${r150.season} Wk ${r150.week} (${rnd(r150.franchise_score)} pts)`;
      if (r200) d.two_hundred = `${r200.season} Wk ${r200.week} (${rnd(r200.franchise_score)} pts)`;
    }
    // Undefeated
    const undefYrs = ownerSt
      .filter((s) => s.h2h_losses === 0 && s.h2h_wins > 0)
      .map((s) => s.season)
      .sort((a, b) => a - b);
    if (undefYrs.length > 0) d.undefeated = `Season: ${undefYrs[0]}`;
    // Sacko
    const sackoYrs = sackoSeasons.map((r) => r.season).sort((a, b) => a - b);
    if (sackoYrs.length > 0) d.sacko = `First: ${sackoYrs[0]}`;
    if (sackoYrs.length >= 2) d.double_sacko = `Years: ${sackoYrs.join(", ")}`;
    // Runner up
    const runnerYrs = ownerSt.filter((s) => s.league_rank === 2).map((s) => s.season).sort((a, b) => a - b);
    if (runnerYrs.length > 0) d.runner_up = `First: ${runnerYrs[0]}`;
    // Big blowout
    if (maxMargin >= 75 && regSc.length > 0) {
      let r = regSc[0];
      for (const g of regSc) {
        if (g.franchise_score - g.opponent_score > r.franchise_score - r.opponent_score) r = g;
      }
      d.big_blowout = `${r.season} Wk ${r.week} vs ${r.opponent_owner} (+${rnd(r.franchise_score - r.opponent_score)})`;
    }
    // Homer
    if (homer && homerRows.length > 0) {
      const hm = homerRows[0];
      d.homer = `${hm.season} (${hm.team}, ${hm.n} picks)`;
    }
    // 00 Agent
    if (zeroWksDetail) d.double_zero = `${zeroWksDetail.season} Wk ${zeroWksDetail.week}`;
    // Game of inches
    if (gameOfInches) {
      const giRows = [...regSc.filter(
        (g) => g.franchise_score - g.opponent_score > 0 && g.franchise_score - g.opponent_score <= 1
      )].sort((a, b) => a.season - b.season || a.week - b.week);
      if (giRows.length > 0) {
        const r = giRows[0];
        d.game_of_inches = `${r.season} Wk ${r.week} vs ${r.opponent_owner} (+${rnd(r.franchise_score - r.opponent_score, 2)})`;
      }
    }
    // Gauntlet
    if (gauntlet) {
      for (const yr of uniq(regSc.map((g) => g.season)).sort((a, b) => a - b)) {
        const beaten = new Set(
          regSc.filter((g) => g.season === yr && g.result === "W").map((g) => g.opponent_owner)
        );
        const allOppsYr = uniq(regSc.filter((g) => g.season === yr).map((g) => g.opponent_owner));
        if (allOppsYr.length > 0 && allOppsYr.every((opp) => beaten.has(opp))) {
          d.gauntlet = `Season: ${yr}`;
          break;
        }
      }
    }
    // Bad Beat
    if (badBeat && badBeatDetail) d.bad_beat = badBeatDetail;
    // Really Bad Beat
    const rbbYrs = rbbSeasons.filter((r) => r.owner === o).map((r) => r.season).sort((a, b) => a - b);
    if (rbbYrs.length > 0) d.really_bad_beat = `First: ${rbbYrs[0]}`;
    // Heating up / On Fire (5+ / 10+ win streak)
    if (maxWinStreak >= 5) {
      for (const yr of [...seasonsOfOwner].sort((a, b) => a - b)) {
        const m = seasonStreak(yr);
        if (m >= 5) {
          d.heating_up = `${yr} (${m}-game streak)`;
          break;
        }
      }
    }
    if (maxWinStreak >= 10) {
      for (const yr of [...seasonsOfOwner].sort((a, b) => a - b)) {
        const m = seasonStreak(yr);
        if (m >= 10) {
          d.on_fire = `${yr} (${m}-game streak)`;
          break;
        }
      }
    }
    // Soft schedule
    const ssYrs = softScheduleSeasons.filter((r) => r.owner === o).map((r) => r.season).sort((a, b) => a - b);
    if (ssYrs.length > 0) d.soft_schedule = `First: ${ssYrs[0]}`;
    // Ninja
    const ninjaYrs = ninjaSeasons.filter((r) => r.owner === o).map((r) => r.season).sort((a, b) => a - b);
    if (ninjaYrs.length > 0) d.ninja = `First: ${ninjaYrs[0]}`;
    // Blowout (50+)
    if (blowout50 && regSc.length > 0) {
      const bRows = [...regSc.filter((g) => g.franchise_score - g.opponent_score >= 50)].sort(
        (a, b) => a.season - b.season || a.week - b.week
      );
      const r = bRows[0];
      d.blowout = `${r.season} Wk ${r.week} vs ${r.opponent_owner} (+${rnd(r.franchise_score - r.opponent_score)})`;
    }
    // Everyone Eats, Double D's, Why, Ban Kickers, Wins Champs
    if (eatsDetail) d.everyone_eats = `${eatsDetail.season} Wk ${eatsDetail.week}`;
    if (doubleDsDetail) d.double_ds = `${doubleDsDetail.season} Wk ${doubleDsDetail.week}`;
    if (whyDetail) d.why = `${whyDetail.season} Wk ${whyDetail.week}`;
    if (kickerDetail) {
      d.ban_kickers = `${kickerDetail.season} Wk ${kickerDetail.week}: ${kickerDetail.player_name} (${rnd(kickerDetail.player_score ?? 0)} pts)`;
    }
    if (dstDetail) {
      d.wins_champs = `${dstDetail.season} Wk ${dstDetail.week}: ${dstDetail.player_name} (${rnd(dstDetail.player_score ?? 0)} pts)`;
    }
    // Kingslayer
    const ksEvents = kingslayerEvents.get(o);
    if (ksEvents && ksEvents.length > 0) {
      const ev = [...ksEvents].sort((a, b) => a.season - b.season || a.week - b.week)[0];
      d.kingslayer = `${ev.season} Wk ${ev.week} vs ${ev.victim}`;
    }
    // Rock bottom
    const rbYrs = rockBottomSeasons.filter((r) => r.owner === o).map((r) => r.season).sort((a, b) => a - b);
    if (rbYrs.length > 0) d.rock_bottom = `First: ${rbYrs[0]}`;
    // Top position
    for (const pos of POSITIONS) {
      const ownInfo = topPosInfo[pos].filter((r) => r.owner === o).sort((a, b) => a.season - b.season);
      if (ownInfo.length > 0) {
        const r = ownInfo[0];
        d[`top_${pos.toLowerCase()}`] = `${r.season}: ${r.player_name} (${rnd(r.pts)} pts)`;
      }
      const ownBest = bestPosInfo[pos].filter((r) => r.owner === o).sort((a, b) => a.season - b.season);
      if (ownBest.length > 0) {
        const r = ownBest[0];
        d[`best_${pos.toLowerCase()}`] = `${r.season} (${rnd(r.pts)} total pts)`;
      }
    }
    // Skin
    if (skinOwners.has(o)) {
      for (const yr of uniq(standings.map((s) => s.season)).sort((a, b) => a - b)) {
        const sackoRow = sackoBySeason.get(yr);
        if (!sackoRow) continue;
        const same = (standingsBySeason.get(String(yr)) ?? []).filter(
          (s) => s.h2h_wins === sackoRow.h2h_wins && s.owner === o && s.points_for > sackoRow.points_for
        );
        if (same.length > 0) {
          d.skin = `First: ${yr}`;
          break;
        }
      }
    }
    // Catch 'em all
    const ce = seasonRostered
      .filter((r) => r.owner === o && r.distinct_players >= 50)
      .sort((a, b) => a.season - b.season);
    if (ce.length > 0) d.catch_em = `${ce[0].season} (${ce[0].distinct_players} players)`;
    // Pokedex counts
    const careerCnt = careerRostered.get(o);
    if (careerCnt !== undefined) {
      for (const th of [200, 300, 400, 500]) {
        if (careerCnt >= th) d[`pokedex_${th}`] = `Career: ${careerCnt} players`;
      }
    }
    // Mr. Relevant
    const mr = mrRelevantInfo.filter((r) => r.owner === o).sort((a, b) => a.season - b.season);
    if (mr.length > 0) d.mr_relevant = `${mr[0].season}: ${mr[0].player_name} (${mr[0].starts} starts)`;
    // Dream work
    const de = dreamEvents
      .filter((r) => r.owner === o)
      .sort((a, b) => a.season - b.season || a.week - b.week);
    if (de.length > 0) d.dream_work = `${de[0].season} Wk ${de[0].week} (${de[0].team})`;
    // Repeat / threepeat
    const ry = repeatYears.get(o);
    if (ry) d.repeat = `${ry[0]}-${ry[1]}`;
    const ty = threepeatYears.get(o);
    if (ty) d.threepeat = `${ty[0]}-${ty[2]}`;
    // Singled out / benchwarmers
    const se = singledEvents
      .filter((r) => r.owner === o)
      .sort((a, b) => a.season - b.season || a.week - b.week);
    if (se.length > 0) d.singled_out = `${se[0].season} Wk ${se[0].week}`;
    const be = benchwarmersEvents
      .filter((r) => r.owner === o)
      .sort((a, b) => a.season - b.season || a.week - b.week);
    if (be.length > 0) d.benchwarmers = `${be[0].season} Wk ${be[0].week}`;
    // Negative, Double-negative
    const ne = negativeEvents
      .filter((r) => r.owner === o)
      .sort((a, b) => a.season - b.season || a.week - b.week);
    if (ne.length > 0) {
      d.negative = `${ne[0].season} Wk ${ne[0].week}: ${ne[0].player_name} (${rnd(ne[0].player_score)} pts)`;
    }
    const dne = doubleNegEvents
      .filter((r) => r.owner === o)
      .sort((a, b) => a.season - b.season || a.week - b.week);
    if (dne.length > 0) d.double_negative = `${dne[0].season} Wk ${dne[0].week}`;
    // Consistency / Is Key / Perfectly Average
    if (consistencyFirst.has(o)) d.consistency = `First: ${consistencyFirst.get(o)}`;
    if (isKeyFirst.has(o)) d.is_key = `First: ${isKeyFirst.get(o)}`;
    if (perfectlyAvgFirst.has(o)) d.perfectly_avg = `First: ${perfectlyAvgFirst.get(o)}`;
    // Boomer / Boomer Boomer
    const bi = boomerInfoByOwner.get(o);
    if (bi && bi.length > 0) {
      const sorted = [...bi].sort((a, b) => a.season - b.season);
      d.boomer = `${sorted[0].season} Wk ${sorted[0].week} (${rnd(sorted[0].pts)} pts)`;
      if (sorted.length >= 3) {
        d.boomer_boomer = `Years: ${sorted.slice(0, 3).map((x) => x.season).join(", ")}`;
      }
    }
    // Late Bloomer / Ran Out of Gas
    const lb = lateBloomerSeasons.get(o);
    if (lb && lb.length > 0) d.late_bloomer = `First: ${[...lb].sort((a, b) => a - b)[0]}`;
    const rg = ranOutOfGasSeasons.get(o);
    if (rg && rg.length > 0) d.ran_out_of_gas = `First: ${[...rg].sort((a, b) => a - b)[0]}`;

    const unlocked: Record<string, boolean> = {
      first_win: careerWins >= 1,
      champion: ownerSt.some((s) => s.league_rank === 1),
      dynasty: ownerSt.filter((s) => s.league_rank === 1).length >= 2,
      title_game: ownerSt.some((s) => s.league_rank <= 2),
      one_seed: mvpSeasons.some((r) => r.owner === o),
      playoff_bound: ownerSt.some((s) => s.league_rank <= 4),
      mvp,
      opoy: mostPfSeasons.length > 0,
      century: maxWeek >= 100,
      one_fifty: maxWeek >= 150,
      two_hundred: maxWeek >= 200,
      winning_record: careerWins > careerLosses,
      veteran: totalSeasons >= 5,
      decade: totalSeasons >= 9,
      undefeated,
      sacko: sackoSeasons.length > 0,
      double_sacko: sackoSeasons.length >= 2,
      runner_up: ownerSt.some((s) => s.league_rank === 2),
      hundred_wins: careerWins >= 100,
      fourteen_k: careerPf >= 14000,
      big_blowout: maxMargin >= 75,
      homer,
      double_zero: doubleZero,
      game_of_inches: gameOfInches,
      gauntlet,
      bad_beat: badBeat,
      really_bad_beat: reallyBadBeat,
      heating_up: maxWinStreak >= 5,
      on_fire: maxWinStreak >= 10,
      soft_schedule: softSchedule,
      ninja,
      blowout: blowout50,
      everyone_eats: everyoneEats,
      double_ds: doubleDs,
      why,
      ban_kickers: banKickers,
      wins_champs: winsChamps,
      kingslayer: kingslayerOwners.has(o),
      rock_bottom: rockBottomSeasons.some((r) => r.owner === o),
      top_qb: topPosSeasons.QB.has(o),
      top_rb: topPosSeasons.RB.has(o),
      top_wr: topPosSeasons.WR.has(o),
      top_te: topPosSeasons.TE.has(o),
      top_k: topPosSeasons.K.has(o),
      top_dst: topPosSeasons.DST.has(o),
      best_qb: bestPosSeasons.QB.has(o),
      best_rb: bestPosSeasons.RB.has(o),
      best_wr: bestPosSeasons.WR.has(o),
      best_te: bestPosSeasons.TE.has(o),
      best_k: bestPosSeasons.K.has(o),
      best_dst: bestPosSeasons.DST.has(o),
      skin: skinOwners.has(o),
      catch_em: seasonRostered.some((r) => r.owner === o && r.distinct_players >= 50),
      pokedex_200: careerCnt !== undefined && careerCnt >= 200,
      pokedex_300: careerCnt !== undefined && careerCnt >= 300,
      pokedex_400: careerCnt !== undefined && careerCnt >= 400,
      pokedex_500: careerCnt !== undefined && careerCnt >= 500,
      mr_relevant: mrRelevantOwners.has(o),
      dream_work: dreamWorkOwners.has(o),
      repeat: repeatOwners.has(o),
      threepeat: threepeatOwners.has(o),
      singled_out: singledOutOwners.has(o),
      benchwarmers: benchwarmersOwners.has(o),
      negative: negativeOwners.has(o),
      double_negative: doubleNegativeOwners.has(o),
      consistency: consistencyOwners.has(o),
      is_key: isKeyOwners.has(o),
      perfectly_avg: perfectlyAvgOwners.has(o),
      boomer: boomerOwners.has(o),
      boomer_boomer: boomerBoomerOwners.has(o),
      late_bloomer: lateBloomerOwners.has(o),
      ran_out_of_gas: ranOutOfGasOwners.has(o),
      perfectly_wrong: perfectlyWrongOwners.has(o),
      dynasty_promised: dynastyPromisedOwners.has(o),
      bridesmaid: bridesmaidOwners.has(o),
      never_left: neverLeftOwners.has(o),
      never_looked_back: neverLookedBackOwners.has(o),
      points_machine: pointsMachineOwners.has(o),
      streaming_savant: streamingSavantOwners.has(o),
      escape_artist: escapeArtistOwners.has(o),
      villain_arc: villainArcOwners.has(o),
      historic_futility: historicFutilityOwners.has(o),
      all_pain: allPainOwners.has(o),
      overcoming_adversity: overcomingAdversityOwners.has(o),
    };

    const pwd = perfectlyWrongDetail.get(o);
    if (pwd) d.perfectly_wrong = pwd;
    const dpd = dynastyPromisedDetail.get(o);
    if (dpd) d.dynasty_promised = dpd;
    const bmd = bridesmaidDetail.get(o);
    if (bmd) d.bridesmaid = bmd;
    const nld = neverLeftDetail.get(o);
    if (nld) d.never_left = nld;
    const nlbd = neverLookedBackDetail.get(o);
    if (nlbd) d.never_looked_back = nlbd;
    const pmd = pointsMachineDetail.get(o);
    if (pmd) d.points_machine = pmd;
    const ssd = streamingSavantDetail.get(o);
    if (ssd) d.streaming_savant = ssd;
    const ead = escapeArtistDetail.get(o);
    if (ead) d.escape_artist = ead;
    const vad = villainArcDetail.get(o);
    if (vad) d.villain_arc = vad;
    const hfd = historicFutilityDetail.get(o);
    if (hfd) d.historic_futility = hfd;
    const apd = allPainDetail.get(o);
    if (apd) d.all_pain = apd;
    const oad = overcomingAdversityDetail.get(o);
    if (oad) d.overcoming_adversity = oad;

    return { unlocked, detail: d };
  }

  // Compute achievement results across all owners
  const results: Record<string, OwnerResult> = {};
  for (const o of allOwners) results[o] = computeAchievements(o);

  // Compute rarity and value for each achievement
  const achValues: Record<string, number> = {};
  for (const ach of ACHIEVEMENTS) {
    const count = allOwners.filter((o) => results[o].unlocked[ach.id] === true).length;
    const rarityPct = Math.round((100 * count) / allOwners.length);
    if (NEGATIVE_IDS.has(ach.id)) achValues[ach.id] = 500;
    else if (rarityPct <= 10) achValues[ach.id] = 10000;
    else if (rarityPct <= 25) achValues[ach.id] = 5000;
    else if (rarityPct <= 50) achValues[ach.id] = 2500;
    else achValues[ach.id] = 1000;
  }

  // Games = career matchups
  const careerGames: Record<string, number> = {};
  for (const o of allOwners) {
    careerGames[o] = standings
      .filter((s) => s.owner === o)
      .reduce((a, s) => a + s.h2h_wins + s.h2h_losses + (s.h2h_ties ?? 0), 0);
  }

  modelCache = { allOwners, results, achValues, careerGames };
  return modelCache;
}
