"use client";

import { useMemo, useState } from "react";
import Card from "@/components/Card";
import DataTable, { Column } from "@/components/DataTable";
import { getLeagueData, fmt } from "@/lib/data";

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

export default function MatchupsPage() {
  const { schedule, seasons } = getLeagueData();
  const seasonsDesc = useMemo(() => [...seasons].sort((a, b) => b - a), [seasons]);

  const [matchupSeason, setMatchupSeason] = useState<string>(String(seasonsDesc[0]));
  const [matchupType, setMatchupType] = useState<string>("All");

  const matchupRows = useMemo<MatchupRow[]>(() => {
    const yr = parseInt(matchupSeason, 10);
    return schedule
      .filter(
        (g) =>
          g.season === yr && (matchupType === "All" || g.game_type === matchupType)
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
  }, [schedule, matchupSeason, matchupType]);

  const matchupColumns: Column<MatchupRow>[] = [
    { key: "week", label: "Week", numeric: true },
    { key: "type", label: "Type" },
    { key: "owner", label: "Owner" },
    { key: "score", label: "Score", numeric: true, render: (r) => fmt(r.score, 2) },
    { key: "opponent", label: "Opponent" },
    { key: "oppScore", label: "Opp Score", numeric: true, render: (r) => fmt(r.oppScore, 2) },
    { key: "result", label: "Result" },
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
    <div className="d-flex gap-2">
      <select
        className="form-select form-select-sm"
        style={{ width: 120 }}
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
        <option>All</option>
        <option>Regular Season</option>
        <option>Playoffs</option>
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
