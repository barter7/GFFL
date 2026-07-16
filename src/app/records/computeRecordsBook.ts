// Faithful port of compute_records_book() from helpers.R (lines 374-642).
import { ScheduleRow, StandingRow } from "@/lib/data";
import {
  computeAlltimeStandings,
  computeLongestLossStreak,
  computeLongestWinStreak,
} from "@/lib/league";

export interface RecordEntry {
  record: string;
  holder: string;
  value: string;
  season: string;
}

// R: format(round(x, d), nsmall = d) — fixed decimal places, no thousands separator.
const nsmall = (x: number, d: number) => x.toFixed(d);

// R: paste0(round(x, d), ...) — rounds, then drops trailing zeros when printing.
const rnum = (x: number, d: number) => String(Number(x.toFixed(d)));

interface PfgRow extends StandingRow {
  games: number;
  pfg: number;
  pag: number;
  winpct: number;
  /** rank(-points_for, ties.method = "min") within the season; 1 = highest PF */
  pfSeasonRank: number;
}

export function computeRecordsBook(
  standings: StandingRow[],
  schedule: ScheduleRow[],
  finalSeasons: Set<number>
): RecordEntry[] {
  const records: RecordEntry[] = [];

  // Single-season records only consider completed seasons: a partial season's
  // totals (wins, losses, PF, PF/G) would pollute "fewest"/"lowest" records
  // and its 999-rank sentinel is meaningless for playoff/championship splits.
  // Weekly records below keep the full schedule — the converter guarantees
  // only completed weeks are present.
  const finalStandings = standings.filter((s) => finalSeasons.has(s.season));

  // Season-level PF rank + PF/G, PA/G and win% per season (pf_ranked_seasons / pfg_season)
  const rows: PfgRow[] = finalStandings.map((s) => {
    const games = s.h2h_wins + s.h2h_losses + (s.h2h_ties ?? 0);
    return {
      ...s,
      games,
      pfg: s.points_for / games,
      pag: s.points_against / games,
      // Ties count as half a win, matching ESPN and computeAlltimeStandings.
      winpct: (s.h2h_wins + 0.5 * (s.h2h_ties ?? 0)) / games,
      pfSeasonRank:
        1 +
        finalStandings.filter(
          (o) => o.season === s.season && o.points_for > s.points_for
        ).length,
    };
  });

  // add_record(): tied holders comma-separated, value from first row, unique seasons
  const addRecord = (
    label: string,
    df: PfgRow[],
    valueFn: (r: PfgRow) => string
  ) => {
    if (df.length === 0) return;
    records.push({
      record: label,
      holder: df.map((r) => r.owner).join(", "),
      value: valueFn(df[0]),
      season: [...new Set(df.map((r) => r.season))].join(", "),
    });
  };

  // "W-L (pct%)", or "W-L-T (pct%)" when the season had a tie —
  // e.g. Jack 2019 "7-6-1 (53.6%)".
  const recordStr = (r: PfgRow) => {
    const ties = r.h2h_ties ?? 0;
    const wl =
      ties > 0
        ? `${r.h2h_wins}-${r.h2h_losses}-${ties}`
        : `${r.h2h_wins}-${r.h2h_losses}`;
    return `${wl} (${(r.winpct * 100).toFixed(1)}%)`;
  };

  // === PLAYOFF/MISS RECORDS ===
  // Worst record (win%) to make playoffs (league_rank <= 4)
  const playoffTeams = rows.filter((r) => r.league_rank <= 4);
  if (playoffTeams.length > 0) {
    const minWp = Math.min(...playoffTeams.map((r) => r.winpct));
    addRecord(
      "Worst Record to Make Playoffs",
      playoffTeams.filter((r) => r.winpct === minWp),
      recordStr
    );

    // Fewest PF to make playoffs
    const minPf = Math.min(...playoffTeams.map((r) => r.points_for));
    addRecord(
      "Fewest PF to Make Playoffs",
      playoffTeams.filter((r) => r.points_for === minPf),
      (r) => nsmall(r.points_for, 1)
    );

    // Lowest PF ranking to make playoffs (highest pf_season_rank value)
    const worstRank = Math.max(...playoffTeams.map((r) => r.pfSeasonRank));
    addRecord(
      "Lowest PF Rank to Make Playoffs",
      playoffTeams.filter((r) => r.pfSeasonRank === worstRank),
      (r) => `#${r.pfSeasonRank} in PF`
    );
  }

  // Best record (win%) to miss playoffs (league_rank > 4)
  const missTeams = rows.filter((r) => r.league_rank > 4);
  if (missTeams.length > 0) {
    const maxWp = Math.max(...missTeams.map((r) => r.winpct));
    addRecord(
      "Best Record to Miss Playoffs",
      missTeams.filter((r) => r.winpct === maxWp),
      recordStr
    );

    // Most PF to miss playoffs
    const maxPf = Math.max(...missTeams.map((r) => r.points_for));
    addRecord(
      "Most PF to Miss Playoffs",
      missTeams.filter((r) => r.points_for === maxPf),
      (r) => nsmall(r.points_for, 1)
    );

    // Best PF ranking to miss playoffs (lowest pf_season_rank value)
    const bestRank = Math.min(...missTeams.map((r) => r.pfSeasonRank));
    addRecord(
      "Best PF Rank to Miss Playoffs",
      missTeams.filter((r) => r.pfSeasonRank === bestRank),
      (r) => `#${r.pfSeasonRank} in PF`
    );
  }

  // Worst record to win championship (league_rank == 1)
  const champTeams = rows.filter((r) => r.league_rank === 1);
  if (champTeams.length > 0) {
    const minWpC = Math.min(...champTeams.map((r) => r.winpct));
    addRecord(
      "Worst Record to Win Championship",
      champTeams.filter((r) => r.winpct === minWpC),
      recordStr
    );

    // Fewest PF to win championship
    const minPfC = Math.min(...champTeams.map((r) => r.points_for));
    addRecord(
      "Fewest PF to Win Championship",
      champTeams.filter((r) => r.points_for === minPfC),
      (r) => nsmall(r.points_for, 1)
    );
  }

  // Highest PF/G in a season
  const maxPfg = Math.max(...rows.map((r) => r.pfg));
  addRecord("Highest PF/G (Season)", rows.filter((r) => r.pfg === maxPfg), (r) =>
    nsmall(r.pfg, 2)
  );

  // Lowest PF/G in a season
  const minPfg = Math.min(...rows.map((r) => r.pfg));
  addRecord("Lowest PF/G (Season)", rows.filter((r) => r.pfg === minPfg), (r) =>
    nsmall(r.pfg, 2)
  );

  // Highest PA/G in a season
  const maxPag = Math.max(...rows.map((r) => r.pag));
  addRecord("Highest PA/G (Season)", rows.filter((r) => r.pag === maxPag), (r) =>
    nsmall(r.pag, 2)
  );

  // Lowest PA/G in a season
  const minPag = Math.min(...rows.map((r) => r.pag));
  addRecord("Lowest PA/G (Season)", rows.filter((r) => r.pag === minPag), (r) =>
    nsmall(r.pag, 2)
  );

  // Weekly records and streaks count ALL games including playoffs — the same
  // basis as the matchups tables, trophy-room plaques, and single-game
  // achievements (e.g. Kerley's 224.28 in the 2025 playoffs is THE record).
  const playedSchedule = schedule.filter((g) => g.result !== null);
  // Zero scores are no-show forfeits (e.g. the abandoned 2022 playoff
  // matchups) - they count for streaks/results but not for score records.
  const scoredSchedule = playedSchedule.filter(
    (g) => g.franchise_score > 0 && g.opponent_score > 0
  );

  // Most wins in a single season
  const bestSeason = [...finalStandings].sort((a, b) => b.h2h_wins - a.h2h_wins)[0];
  records.push({
    record: "Most Wins (Season)",
    holder: bestSeason.owner,
    value: String(bestSeason.h2h_wins),
    season: String(bestSeason.season),
  });

  // Fewest losses in a season (ties comma-joined like add_record entries)
  const minLosses = Math.min(...finalStandings.map((s) => s.h2h_losses));
  const fewestLossRows = finalStandings.filter((s) => s.h2h_losses === minLosses);
  records.push({
    record: "Fewest Losses (Season)",
    holder: fewestLossRows.map((r) => r.owner).join(", "),
    value: String(minLosses),
    season: [...new Set(fewestLossRows.map((r) => r.season))].join(", "),
  });

  // Most points in a season
  const mostPf = [...finalStandings].sort((a, b) => b.points_for - a.points_for)[0];
  records.push({
    record: "Most Points For (Season)",
    holder: mostPf.owner,
    value: nsmall(mostPf.points_for, 1),
    season: String(mostPf.season),
  });

  // Fewest points in a season
  const leastPf = [...finalStandings].sort((a, b) => a.points_for - b.points_for)[0];
  records.push({
    record: "Fewest Points For (Season)",
    holder: leastPf.owner,
    value: nsmall(leastPf.points_for, 1),
    season: String(leastPf.season),
  });

  if (playedSchedule.length > 0) {
    // Highest single-week score
    const highWeek = [...scoredSchedule].sort(
      (a, b) => b.franchise_score - a.franchise_score
    )[0];
    records.push({
      record: "Highest Weekly Score",
      holder: highWeek.team_owner,
      value: nsmall(highWeek.franchise_score, 2),
      season: `${highWeek.season} W${highWeek.week}`,
    });

    // Lowest single-week score
    const lowWeeks = scoredSchedule
      .filter((g) => g.franchise_score > 0)
      .sort((a, b) => a.franchise_score - b.franchise_score);
    if (lowWeeks.length > 0) {
      const lowWeek = lowWeeks[0];
      records.push({
        record: "Lowest Weekly Score",
        holder: lowWeek.team_owner,
        value: nsmall(lowWeek.franchise_score, 2),
        season: `${lowWeek.season} W${lowWeek.week}`,
      });
    }

    // Biggest blowout
    const blowouts = scoredSchedule
      .map((g) => ({ g, margin: g.franchise_score - g.opponent_score }))
      .filter((x) => x.margin > 0)
      .sort((a, b) => b.margin - a.margin);
    if (blowouts.length > 0) {
      const { g, margin } = blowouts[0];
      records.push({
        record: "Biggest Blowout",
        holder: g.team_owner,
        value: `${rnum(g.franchise_score, 1)}-${rnum(g.opponent_score, 1)} (+${rnum(margin, 1)})`,
        season: `${g.season} W${g.week}`,
      });
    }

    // Closest game
    const closeGames = scoredSchedule
      .map((g) => ({ g, margin: Math.abs(g.franchise_score - g.opponent_score) }))
      .filter((x) => x.margin > 0)
      .sort((a, b) => a.margin - b.margin);
    if (closeGames.length > 0) {
      const { g, margin } = closeGames[0];
      records.push({
        record: "Closest Game",
        holder: g.team_owner,
        value: `${rnum(g.franchise_score, 2)}-${rnum(g.opponent_score, 2)} (${rnum(margin, 2)})`,
        season: `${g.season} W${g.week}`,
      });
    }
  }

  // Winning streak
  const winStreak = computeLongestWinStreak(playedSchedule);
  records.push({
    record: "Longest Winning Streak",
    holder: winStreak.team,
    value: `${winStreak.streak} games`,
    season: "All-Time",
  });

  // Losing streak
  const loseStreak = computeLongestLossStreak(playedSchedule);
  records.push({
    record: "Longest Losing Streak",
    holder: loseStreak.team,
    value: `${loseStreak.streak} games`,
    season: "All-Time",
  });

  // All-time wins leader
  const alltime = computeAlltimeStandings(standings)[0];
  records.push({
    record: "Most All-Time Wins",
    holder: alltime.team,
    value: String(alltime.wins),
    season: "All-Time",
  });

  return records;
}
