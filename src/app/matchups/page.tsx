"use client";

import { Suspense, useEffect, useMemo, useState } from "react";
import { useSearchParams } from "next/navigation";
import Card from "@/components/Card";
import DataTable, { Column } from "@/components/DataTable";
import { getLeagueData, fmt } from "@/lib/data";

// Sync selector state into the query string without triggering navigation
// (history.replaceState keeps scroll position; router.push would not).
function updateQuery(params: Record<string, string | null>) {
  if (typeof window === "undefined") return;
  const sp = new URLSearchParams(window.location.search);
  for (const [k, v] of Object.entries(params)) {
    if (v === null) sp.delete(k);
    else sp.set(k, v);
  }
  const qs = sp.toString();
  window.history.replaceState(null, "", qs ? `?${qs}` : window.location.pathname);
}

const TYPE_CHOICES = ["All", "Regular Season", "Playoffs"];

interface MatchupRow {
  week: number;
  type: string;
  owner: string;
  score: number;
  opponent: string;
  oppScore: number;
  result: string;
}

interface TopScoreRow {
  season: number;
  week: number;
  owner: string;
  score: number;
  opponent: string;
}

interface BlowoutRow {
  season: number;
  week: number;
  winner: string;
  winScore: number;
  loser: string;
  loseScore: number;
  margin: number;
}

function MatchupsInner() {
  const { schedule, seasons } = getLeagueData();
  const seasonsDesc = useMemo(() => [...seasons].sort((a, b) => b - a), [seasons]);
  const defaultSeason = String(seasonsDesc[0]);

  // Initial state from the URL (?season=2024&type=Playoffs&wk=3), validated
  const searchParams = useSearchParams();
  const [matchupSeason, setMatchupSeason] = useState<string>(() => {
    const s = searchParams.get("season");
    if (s !== null && seasons.some((yr) => String(yr) === s)) return s;
    return defaultSeason;
  });
  const [matchupType, setMatchupType] = useState<string>(() => {
    const t = searchParams.get("type");
    return t !== null && TYPE_CHOICES.includes(t) ? t : "All";
  });
  const [matchupWeek, setMatchupWeek] = useState<string>(() => {
    const w = searchParams.get("wk");
    return w !== null && /^\d+$/.test(w) ? w : "All";
  });

  // Keep the URL shareable as the selectors change (defaults omitted)
  useEffect(() => {
    updateQuery({
      season: matchupSeason === defaultSeason ? null : matchupSeason,
      type: matchupType === "All" ? null : matchupType,
      wk: matchupWeek === "All" ? null : matchupWeek,
    });
  }, [matchupSeason, matchupType, matchupWeek, defaultSeason]);

  // Weeks available for the selected season/type (drives the Wk quick filter)
  const weekChoices = useMemo(() => {
    const yr = parseInt(matchupSeason, 10);
    const weeks = new Set<number>();
    for (const g of schedule) {
      if (g.season === yr && (matchupType === "All" || g.game_type === matchupType)) {
        weeks.add(g.week);
      }
    }
    return [...weeks].sort((a, b) => a - b);
  }, [schedule, matchupSeason, matchupType]);

  // Reset the week filter if it no longer exists for the selected season/type
  useEffect(() => {
    if (matchupWeek !== "All" && !weekChoices.includes(parseInt(matchupWeek, 10))) {
      setMatchupWeek("All");
    }
  }, [weekChoices, matchupWeek]);

  const matchupRows = useMemo<MatchupRow[]>(() => {
    const yr = parseInt(matchupSeason, 10);
    const wk = matchupWeek === "All" ? null : parseInt(matchupWeek, 10);
    return schedule
      .filter(
        (g) =>
          g.season === yr &&
          (matchupType === "All" || g.game_type === matchupType) &&
          (wk === null || g.week === wk)
      )
      .map((g) => ({
        week: g.week,
        type: g.game_type,
        owner: g.team_owner,
        score: g.franchise_score,
        opponent: g.opponent_owner,
        oppScore: g.opponent_score,
        result: g.result ?? "",
      }))
      .sort((a, b) => a.week - b.week || a.owner.localeCompare(b.owner));
  }, [schedule, matchupSeason, matchupType, matchupWeek]);

  const matchupColumns: Column<MatchupRow>[] = [
    { key: "week", label: "Week", numeric: true },
    { key: "type", label: "Type" },
    { key: "owner", label: "Owner" },
    { key: "score", label: "Score", numeric: true, render: (r) => fmt(r.score, 2) },
    { key: "opponent", label: "Opponent" },
    { key: "oppScore", label: "Opp Score", numeric: true, render: (r) => fmt(r.oppScore, 2) },
    {
      key: "result",
      label: "Result",
      render: (r) => (
        <span
          style={{
            display: "block",
            margin: "-0.5rem",
            padding: "0.5rem",
            fontWeight: "bold",
            textAlign: "center",
            ...(r.result === "W"
              ? { background: "rgba(40,167,69,0.2)", color: "#1d7a36" }
              : r.result === "L"
                ? { background: "rgba(220,53,69,0.2)", color: "#b02a37" }
                : {}),
          }}
        >
          {r.result}
        </span>
      ),
    },
  ];

  const topScores = useMemo<TopScoreRow[]>(() => {
    return [...schedule]
      .sort((a, b) => b.franchise_score - a.franchise_score)
      .slice(0, 25)
      .map((g) => ({
        season: g.season,
        week: g.week,
        owner: g.team_owner,
        score: g.franchise_score,
        opponent: g.opponent_owner,
      }));
  }, [schedule]);

  const topScoreColumns: Column<TopScoreRow>[] = [
    { key: "season", label: "Season", numeric: true },
    { key: "week", label: "Week", numeric: true },
    { key: "owner", label: "Owner" },
    { key: "score", label: "Score", numeric: true, render: (r) => fmt(r.score, 2) },
    { key: "opponent", label: "Opponent" },
  ];

  const blowouts = useMemo<BlowoutRow[]>(() => {
    return schedule
      .map((g) => ({ g, margin: g.franchise_score - g.opponent_score }))
      .filter((x) => x.margin > 0)
      .sort((a, b) => b.margin - a.margin)
      .slice(0, 25)
      .map(({ g, margin }) => ({
        season: g.season,
        week: g.week,
        winner: g.team_owner,
        winScore: g.franchise_score,
        loser: g.opponent_owner,
        loseScore: g.opponent_score,
        margin,
      }));
  }, [schedule]);

  const blowoutColumns: Column<BlowoutRow>[] = [
    { key: "season", label: "Season", numeric: true },
    { key: "week", label: "Week", numeric: true },
    { key: "winner", label: "Winner" },
    { key: "winScore", label: "Win Score", numeric: true, render: (r) => fmt(r.winScore, 2) },
    { key: "loser", label: "Loser" },
    { key: "loseScore", label: "Lose Score", numeric: true, render: (r) => fmt(r.loseScore, 2) },
    { key: "margin", label: "Margin", numeric: true, render: (r) => fmt(r.margin, 2) },
  ];

  const filterSelects = (
    <div className="d-flex gap-2 flex-wrap">
      <select
        className="form-select form-select-sm"
        style={{ width: 100 }}
        value={matchupSeason}
        onChange={(e) => setMatchupSeason(e.target.value)}
        aria-label="Season"
      >
        {seasonsDesc.map((yr) => (
          <option key={yr} value={String(yr)}>
            {yr}
          </option>
        ))}
      </select>
      <select
        className="form-select form-select-sm"
        style={{ width: 150 }}
        value={matchupType}
        onChange={(e) => setMatchupType(e.target.value)}
        aria-label="Game type"
      >
        {TYPE_CHOICES.map((t) => (
          <option key={t}>{t}</option>
        ))}
      </select>
      <select
        className="form-select form-select-sm"
        style={{ width: 90 }}
        value={matchupWeek}
        onChange={(e) => setMatchupWeek(e.target.value)}
        aria-label="Week"
      >
        <option value="All">Wk: All</option>
        {weekChoices.map((w) => (
          <option key={w} value={String(w)}>
            Wk {w}
          </option>
        ))}
      </select>
    </div>
  );

  return (
    <>
      <div className="row g-3">
        <div className="col-12">
          <Card header="Weekly Matchup Results" headerExtra={filterSelects}>
            <DataTable columns={matchupColumns} rows={matchupRows} pageSize={25} />
          </Card>
        </div>
      </div>
      <div className="row g-3 mt-0">
        <div className="col-md-6">
          <Card header="Highest Scoring Weeks (All-Time)">
            <DataTable
              columns={topScoreColumns}
              rows={topScores}
              pageSize={10}
              searchable={false}
            />
          </Card>
        </div>
        <div className="col-md-6">
          <Card header="Biggest Blowouts (All-Time)">
            <DataTable
              columns={blowoutColumns}
              rows={blowouts}
              pageSize={10}
              searchable={false}
            />
          </Card>
        </div>
      </div>
    </>
  );
}

// Static export requires useSearchParams() to live inside a <Suspense>
// boundary, otherwise the build fails with missing-suspense-with-csr-bailout.
export default function MatchupsPage() {
  return (
    <Suspense fallback={null}>
      <MatchupsInner />
    </Suspense>
  );
}
