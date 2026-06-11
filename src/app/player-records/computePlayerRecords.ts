// Faithful port of the player-record computations from app.R
// output$player_records_book (lines 4166-4483).
import { StarterRow, getLeagueData } from "@/lib/data";

/** One row in a top-N record table (matches the build_top5 row contract). */
export interface Top5Row {
  player_name: string;
  pos: string;
  team: string;
  /** Value column content (R renders numbers via as.character: trailing zeros dropped). */
  value: string;
  /** Optional italic detail column. */
  extra?: string;
}

// R: paste0(round(x, d), ...) — rounds, then drops trailing zeros when printing.
const rnum = (x: number, d: number) => String(Number(x.toFixed(d)));
// R: round(x, d) kept as a numeric (used for both sorting and display).
const roundN = (x: number, d: number) => Number(x.toFixed(d));

const teamStr = (t: string | null) => t ?? "NA";

// Comparators that emulate dplyr::arrange (stable, NA last).
const cmpDesc = (a: number, b: number) => {
  const av = Number.isNaN(a) ? -Infinity : a;
  const bv = Number.isNaN(b) ? -Infinity : b;
  return av === bv ? 0 : bv > av ? 1 : -1;
};
const cmpAsc = (a: number, b: number) => {
  const av = Number.isNaN(a) ? Infinity : a;
  const bv = Number.isNaN(b) ? Infinity : b;
  return av === bv ? 0 : av > bv ? 1 : -1;
};

const validScores = (rows: StarterRow[]): number[] =>
  rows.map((r) => r.player_score).filter((x): x is number => x != null);

const sum = (xs: number[]) => xs.reduce((s, x) => s + x, 0);
const mean = (xs: number[]) => (xs.length > 0 ? sum(xs) / xs.length : NaN);
// Sample standard deviation, like R's sd()
const sd = (xs: number[]) => {
  if (xs.length < 2) return NaN;
  const m = mean(xs);
  return Math.sqrt(sum(xs.map((x) => (x - m) ** 2)) / (xs.length - 1));
};

const detailWk = (r: StarterRow) => `${r.owner} - ${r.season} W${r.week}`;

// --- grouping helpers -------------------------------------------------------
// dplyr group_by/count sorts output by the grouping keys; subsequent
// arrange() calls are stable, so ties stay in key order.

interface Group3 {
  player_name: string;
  pos: string;
  team: string | null;
  rows: StarterRow[];
}

function groupBy3(data: StarterRow[]): Group3[] {
  const map = new Map<string, Group3>();
  for (const r of data) {
    const key = `${r.player_name}${r.pos}${r.team ?? ""}`;
    let g = map.get(key);
    if (!g) {
      g = { player_name: r.player_name, pos: r.pos, team: r.team, rows: [] };
      map.set(key, g);
    }
    g.rows.push(r);
  }
  return [...map.values()].sort(
    (a, b) =>
      a.player_name.localeCompare(b.player_name) ||
      a.pos.localeCompare(b.pos) ||
      (a.team ?? "").localeCompare(b.team ?? "")
  );
}

const row3 = (g: Group3, value: string, extra?: string): Top5Row => ({
  player_name: g.player_name,
  pos: g.pos,
  team: teamStr(g.team),
  value,
  ...(extra !== undefined ? { extra } : {}),
});

/** dplyr::count(player_name, pos, team) |> arrange(desc(n)) |> head(n) */
function countTop(data: StarterRow[], n = 10): Top5Row[] {
  return groupBy3(data)
    .map((g) => ({ g, n: g.rows.length }))
    .sort((a, b) => cmpDesc(a.n, b.n))
    .slice(0, n)
    .map(({ g, n }) => row3(g, String(n)));
}

/** arrange(desc(player_score)) |> head(10), display "X pts" (+ optional suffix) */
function topGames(data: StarterRow[], suffix = " pts"): Top5Row[] {
  return [...data]
    .sort((a, b) => cmpDesc(a.player_score ?? NaN, b.player_score ?? NaN))
    .slice(0, 10)
    .map((r) => ({
      player_name: r.player_name,
      pos: r.pos,
      team: teamStr(r.team),
      value: `${rnum(r.player_score ?? 0, 1)}${suffix}`,
      extra: detailWk(r),
    }));
}

/** group_by(player, pos, team, season, owner) season totals, top n */
function seasonTop(data: StarterRow[], n: number): Top5Row[] {
  interface SG {
    player_name: string;
    pos: string;
    team: string | null;
    season: number;
    owner: string;
    pts: number;
  }
  const map = new Map<string, SG>();
  for (const r of data) {
    const key = `${r.player_name}${r.pos}${r.team ?? ""}${r.season}${r.owner}`;
    let g = map.get(key);
    if (!g) {
      g = {
        player_name: r.player_name,
        pos: r.pos,
        team: r.team,
        season: r.season,
        owner: r.owner,
        pts: 0,
      };
      map.set(key, g);
    }
    g.pts += r.player_score ?? 0;
  }
  return [...map.values()]
    .sort(
      (a, b) =>
        a.player_name.localeCompare(b.player_name) ||
        a.pos.localeCompare(b.pos) ||
        (a.team ?? "").localeCompare(b.team ?? "") ||
        a.season - b.season ||
        a.owner.localeCompare(b.owner)
    )
    .map((g) => ({ g, v: roundN(g.pts, 0) }))
    .sort((a, b) => cmpDesc(a.v, b.v))
    .slice(0, n)
    .map(({ g, v }) => ({
      player_name: g.player_name,
      pos: g.pos,
      team: teamStr(g.team),
      value: `${v} pts`,
      extra: `${g.owner} - ${g.season}`,
    }));
}

// ---------------------------------------------------------------------------

export interface PlayerRecords {
  topScores: Top5Row[];
  above50: Top5Row[];
  above40: Top5Row[];
  above30: Top5Row[];
  topBench: Top5Row[];
  benchKings: Top5Row[];
  loyalty: Top5Row[];
  nomads: Top5Row[];
  mostStarts: Top5Row[];
  mostTotalPts: Top5Row[];
  bestAvg: Top5Row[];
  busts: Top5Row[] | null;
  booms: Top5Row[] | null;
  gooseEggs: Top5Row[];
  teamHoppers: Top5Row[];
  above20: Top5Row[];
  under5: Top5Row[];
  topQb: Top5Row[];
  topRb: Top5Row[];
  topWr: Top5Row[];
  topTe: Top5Row[];
  mostConsistent: Top5Row[];
  mostVolatile: Top5Row[];
  weekJumps: Top5Row[];
  weekDrops: Top5Row[];
  seasonLeaders: Top5Row[];
  mostPopular: Top5Row[];
  benchPts: Top5Row[];
  topNonQb: Top5Row[];
  topNonQbBench: Top5Row[];
  nonQbAbove30: Top5Row[];
  nonQbAbove20: Top5Row[];
  nonQbAvg: Top5Row[];
  nonQbTotal: Top5Row[];
  nonQbSeason: Top5Row[];
  qbSeason: Top5Row[];
  rbSeason: Top5Row[];
  wrSeason: Top5Row[];
  teSeason: Top5Row[];
}

export function computePlayerRecords(): PlayerRecords {
  const data = getLeagueData();

  const starters = data.starters.filter(
    (r) => !["DST", "D/ST", "DEF", "K"].includes(r.pos)
  );
  const started = starters.filter((r) => !["BE", "IR"].includes(r.lineup_slot));
  const benched = starters.filter((r) => r.lineup_slot === "BE");

  // 1. Highest single-game starter score - top 5
  const topScores = topGames(started);

  // 2-4. Most games scoring 50+/40+/30+ points
  const above50 = countTop(started.filter((r) => (r.player_score ?? NaN) >= 50));
  const above40 = countTop(started.filter((r) => (r.player_score ?? NaN) >= 40));
  const above30 = countTop(started.filter((r) => (r.player_score ?? NaN) >= 30));

  // 5. Highest single-game bench score (left on bench)
  const topBench = topGames(benched, " pts on bench");

  // 6. Most weeks on someone's bench
  const benchKings = countTop(benched);

  // 7. Most seasons owned by the same owner
  interface LoyaltyG {
    player_name: string;
    pos: string;
    team: string | null;
    owner: string;
    seasons: Set<number>;
  }
  const loyaltyMap = new Map<string, LoyaltyG>();
  for (const r of starters) {
    const key = `${r.player_name}${r.pos}${r.team ?? ""}${r.owner}`;
    let g = loyaltyMap.get(key);
    if (!g) {
      g = {
        player_name: r.player_name,
        pos: r.pos,
        team: r.team,
        owner: r.owner,
        seasons: new Set(),
      };
      loyaltyMap.set(key, g);
    }
    g.seasons.add(r.season);
  }
  const loyalty: Top5Row[] = [...loyaltyMap.values()]
    .sort(
      (a, b) =>
        a.player_name.localeCompare(b.player_name) ||
        a.pos.localeCompare(b.pos) ||
        (a.team ?? "").localeCompare(b.team ?? "") ||
        a.owner.localeCompare(b.owner)
    )
    .sort((a, b) => cmpDesc(a.seasons.size, b.seasons.size))
    .slice(0, 10)
    .map((g) => ({
      player_name: g.player_name,
      pos: g.pos,
      team: teamStr(g.team),
      value: `${g.seasons.size} seasons`,
      extra: g.owner,
    }));

  // 8. Player on most different owners' teams
  const nomads = groupBy3(starters)
    .map((g) => ({ g, n: new Set(g.rows.map((r) => r.franchise_id)).size }))
    .sort((a, b) => cmpDesc(a.n, b.n))
    .slice(0, 10)
    .map(({ g, n }) => row3(g, String(n)));

  // 9. Most total starts across all seasons
  const mostStarts = countTop(started);

  // 10. Most total fantasy points scored as a starter
  const mostTotalPts = groupBy3(started)
    .map((g) => ({ g, v: roundN(sum(validScores(g.rows)), 0) }))
    .sort((a, b) => cmpDesc(a.v, b.v))
    .slice(0, 10)
    .map(({ g, v }) => row3(g, String(v)));

  // 11. Highest average score per start (min 10 starts)
  const avgTop = (data2: StarterRow[]): Top5Row[] =>
    groupBy3(data2)
      .filter((g) => g.rows.length >= 10)
      .map((g) => ({ g, v: roundN(mean(validScores(g.rows)), 1) }))
      .sort((a, b) => cmpDesc(a.v, b.v))
      .slice(0, 10)
      .map(({ g, v }) => row3(g, String(v)));
  const bestAvg = avgTop(started);

  // 12. Biggest bust: highest projected with lowest actual
  const projRows = started.filter(
    (r) => r.projected_score != null && r.player_score != null
  );
  const projTop = (marginFn: (proj: number, actual: number) => number): Top5Row[] =>
    projRows
      .map((r) => ({
        r,
        m: marginFn(r.projected_score as number, r.player_score as number),
      }))
      .sort((a, b) => cmpDesc(a.m, b.m))
      .slice(0, 10)
      .map(({ r }) => ({
        player_name: r.player_name,
        pos: r.pos,
        team: teamStr(r.team),
        value: `Projected ${rnum(r.projected_score as number, 0)}, scored ${rnum(r.player_score as number, 1)}`,
        extra: detailWk(r),
      }));
  const busts = projTop((proj, actual) => proj - actual);

  // 13. Biggest overperformer: lowest projected with highest actual
  const booms = projTop((proj, actual) => actual - proj);

  // 14. Most goose eggs (0 points as a starter)
  const gooseEggs = countTop(started.filter((r) => r.player_score === 0));

  // 15. Most different teams (NFL) a player appeared on
  interface HopperG {
    player_name: string;
    pos: string;
    teams: Set<string | null>;
  }
  const hopperMap = new Map<string, HopperG>();
  for (const r of starters) {
    const key = `${r.player_name}${r.pos}`;
    let g = hopperMap.get(key);
    if (!g) {
      g = { player_name: r.player_name, pos: r.pos, teams: new Set() };
      hopperMap.set(key, g);
    }
    g.teams.add(r.team);
  }
  const teamHoppers: Top5Row[] = [...hopperMap.values()]
    .sort(
      (a, b) =>
        a.player_name.localeCompare(b.player_name) || a.pos.localeCompare(b.pos)
    )
    .filter((g) => g.teams.size > 1)
    .sort((a, b) => cmpDesc(a.teams.size, b.teams.size))
    .slice(0, 10)
    .map((g) => ({
      player_name: g.player_name,
      pos: g.pos,
      team: "Multiple",
      value: String(g.teams.size),
    }));

  // 16. Most games scoring 20+ points
  const above20 = countTop(started.filter((r) => (r.player_score ?? NaN) >= 20));

  // 17. Most games scoring under 5 points (started)
  const under5 = countTop(
    started.filter((r) => r.player_score != null && r.player_score < 5)
  );

  // 18-21. Highest scoring QB/RB/WR/TE in a single game
  const topQb = topGames(started.filter((r) => r.pos === "QB"));
  const topRb = topGames(started.filter((r) => r.pos === "RB"));
  const topWr = topGames(started.filter((r) => r.pos === "WR"));
  const topTe = topGames(started.filter((r) => r.pos === "TE"));

  // 22-23. Most consistent / most boom-or-bust (std dev, min 20 starts)
  const variability = (volatile: boolean): Top5Row[] =>
    groupBy3(started)
      .filter((g) => g.rows.length >= 20)
      .map((g) => {
        const xs = validScores(g.rows);
        return { g, sd: roundN(sd(xs), 2), avg: roundN(mean(xs), 1) };
      })
      .sort((a, b) => (volatile ? cmpDesc(a.sd, b.sd) : cmpAsc(a.sd, b.sd)))
      .slice(0, 10)
      .map(({ g, sd: s, avg }) => row3(g, `SD: ${s} (avg ${avg})`));
  const mostConsistent = variability(false);
  const mostVolatile = variability(true);

  // 24-25. Biggest single-game improvement / drop vs previous week
  // arrange(player_name, season, week), lag(player_score) within (player, pos, team)
  const arranged = [...started].sort(
    (a, b) =>
      a.player_name.localeCompare(b.player_name) ||
      a.season - b.season ||
      a.week - b.week
  );
  const lagged: { r: StarterRow; prev: number }[] = [];
  {
    const last = new Map<string, number | null>();
    for (const r of arranged) {
      const key = `${r.player_name}${r.pos}${r.team ?? ""}`;
      const prev = last.get(key);
      if (last.has(key) && prev != null && r.player_score != null) {
        lagged.push({ r, prev });
      }
      last.set(key, r.player_score);
    }
  }
  const weekJumps: Top5Row[] = lagged
    .map((x) => ({ ...x, diff: (x.r.player_score as number) - x.prev }))
    .sort((a, b) => cmpDesc(a.diff, b.diff))
    .slice(0, 10)
    .map((x) => ({
      player_name: x.r.player_name,
      pos: x.r.pos,
      team: teamStr(x.r.team),
      value: `+${rnum(x.diff, 1)} pts (${rnum(x.prev, 0)} -> ${rnum(x.r.player_score as number, 0)})`,
      extra: detailWk(x.r),
    }));
  const weekDrops: Top5Row[] = lagged
    .map((x) => ({ ...x, diff: x.prev - (x.r.player_score as number) }))
    .sort((a, b) => cmpDesc(a.diff, b.diff))
    .slice(0, 10)
    .map((x) => ({
      player_name: x.r.player_name,
      pos: x.r.pos,
      team: teamStr(x.r.team),
      value: `-${rnum(x.diff, 1)} pts (${rnum(x.prev, 0)} -> ${rnum(x.r.player_score as number, 0)})`,
      extra: detailWk(x.r),
    }));

  // 26. Most points scored in a single season for one owner
  const seasonLeaders = seasonTop(started, 10);

  // 27. Most games started across all owners (most popular starter)
  const mostPopular = countTop(started);

  // 28. Most career bench points
  const benchPts = groupBy3(benched)
    .map((g) => ({ g, v: roundN(sum(validScores(g.rows)), 0) }))
    .sort((a, b) => cmpDesc(a.v, b.v))
    .slice(0, 10)
    .map(({ g, v }) => row3(g, String(v)));

  // 29-35. Non-QB records
  const nonQbStarted = started.filter((r) => r.pos !== "QB");
  const nonQbBenched = benched.filter((r) => r.pos !== "QB");
  const topNonQb = topGames(nonQbStarted);
  const topNonQbBench = topGames(nonQbBenched, " pts on bench");
  const nonQbAbove30 = countTop(
    nonQbStarted.filter((r) => (r.player_score ?? NaN) >= 30)
  );
  const nonQbAbove20 = countTop(
    nonQbStarted.filter((r) => (r.player_score ?? NaN) >= 20)
  );
  const nonQbAvg = avgTop(nonQbStarted);
  const nonQbTotal = groupBy3(nonQbStarted)
    .map((g) => ({ g, v: roundN(sum(validScores(g.rows)), 0) }))
    .sort((a, b) => cmpDesc(a.v, b.v))
    .slice(0, 10)
    .map(({ g, v }) => row3(g, String(v)));
  const nonQbSeason = seasonTop(nonQbStarted, 10);

  // 36-39. Most points in a single season BY POSITION (top 20)
  const posSeasonRecords = (position: string) =>
    seasonTop(started.filter((r) => r.pos === position), 20);
  const qbSeason = posSeasonRecords("QB");
  const rbSeason = posSeasonRecords("RB");
  const wrSeason = posSeasonRecords("WR");
  const teSeason = posSeasonRecords("TE");

  return {
    topScores, above50, above40, above30, topBench, benchKings, loyalty,
    nomads, mostStarts, mostTotalPts, bestAvg, busts, booms, gooseEggs,
    teamHoppers, above20, under5, topQb, topRb, topWr, topTe,
    mostConsistent, mostVolatile, weekJumps, weekDrops, seasonLeaders,
    mostPopular, benchPts, topNonQb, topNonQbBench, nonQbAbove30,
    nonQbAbove20, nonQbAvg, nonQbTotal, nonQbSeason,
    qbSeason, rbSeason, wrSeason, teSeason,
  };
}
