"use client";

import { useMemo, useState } from "react";
import Card from "@/components/Card";
import DataTable, { Column } from "@/components/DataTable";
import Plot from "@/components/Plot";
import { getLeagueData, fmt } from "@/lib/data";
import { computeOwnerVsOwner } from "@/lib/league";

interface DetailRow {
  season: number;
  week: number;
  score1: number;
  score2: number;
  margin: number;
  result: string;
}

interface MatrixRow {
  owner: string;
  opponent: string;
  w: number;
  l: number;
  games: number;
  winPct: number;
  avgMargin: number;
}

export default function HeadToHeadPage() {
  const { schedule, owners } = getLeagueData();

  // Selector state (the original applies on the "Compare" button press;
  // here results pre-populate with the defaults and update on Compare)
  const [team1, setTeam1] = useState<string>(owners[0] ?? "");
  const [team2, setTeam2] = useState<string>(owners[Math.min(1, owners.length - 1)] ?? "");
  const [regOnly, setRegOnly] = useState<boolean>(true);
  const [applied, setApplied] = useState({
    team1: owners[0] ?? "",
    team2: owners[Math.min(1, owners.length - 1)] ?? "",
    regOnly: true,
  });

  const h2hGames = useMemo(() => {
    return schedule
      .filter(
        (g) =>
          g.team_owner === applied.team1 &&
          g.opponent_owner === applied.team2 &&
          (!applied.regOnly || g.game_type === "Regular Season")
      )
      .sort((a, b) => a.season - b.season || a.week - b.week);
  }, [schedule, applied]);

  // ---- Summary --------------------------------------------------------------
  const summary = useMemo(() => {
    if (h2hGames.length === 0) return null;
    const wins = h2hGames.filter((g) => g.result === "W").length;
    const losses = h2hGames.filter((g) => g.result === "L").length;
    const ties = h2hGames.filter((g) => g.result === "T").length;
    const avgScore =
      h2hGames.reduce((s, g) => s + g.franchise_score, 0) / h2hGames.length;
    const avgOpp =
      h2hGames.reduce((s, g) => s + g.opponent_score, 0) / h2hGames.length;
    return { wins, losses, ties, avgScore, avgOpp };
  }, [h2hGames]);

  // ---- Margin bar chart (h2h_plot) -------------------------------------------
  const plot = useMemo(() => {
    const margins = h2hGames.map((g) => g.franchise_score - g.opponent_score);
    const data: Record<string, unknown>[] = [
      {
        type: "bar",
        x: h2hGames.map((g) => `${g.season} W${g.week}`),
        y: margins,
        marker: {
          color: margins.map((m) => (m >= 0 ? "#28a745" : "#dc3545")),
        },
        hovertemplate: "%{y:.2f}<extra></extra>",
        showlegend: false,
      },
    ];
    const layout: Record<string, unknown> = {
      showlegend: false,
      xaxis: { type: "category", tickangle: -45, tickfont: { size: 9 } },
      yaxis: { title: { text: "Score Margin" } },
    };
    return { data, layout };
  }, [h2hGames]);

  // ---- Detail table ----------------------------------------------------------
  const detailRows = useMemo<DetailRow[]>(() => {
    return h2hGames
      .map((g) => ({
        season: g.season,
        week: g.week,
        score1: g.franchise_score,
        score2: g.opponent_score,
        margin: g.franchise_score - g.opponent_score,
        result: g.result ?? "",
      }))
      .sort((a, b) => b.season - a.season || b.week - a.week);
  }, [h2hGames]);

  const detailColumns: Column<DetailRow>[] = [
    { key: "season", label: "Season", numeric: true },
    { key: "week", label: "Week", numeric: true },
    { key: "score1", label: applied.team1, numeric: true, render: (r) => fmt(r.score1, 2) },
    { key: "score2", label: applied.team2, numeric: true, render: (r) => fmt(r.score2, 2) },
    { key: "margin", label: "Margin", numeric: true, render: (r) => fmt(r.margin, 2) },
    { key: "result", label: "Result" },
  ];

  // ---- Owner vs Owner matrix (regular season) --------------------------------
  const matrixRows = useMemo<MatrixRow[]>(() => {
    return computeOwnerVsOwner(schedule, true).map((r) => ({
      owner: r.owner,
      opponent: r.opponent,
      w: r.wins,
      l: r.losses,
      games: r.games,
      winPct: r.winPct,
      avgMargin: r.avgMargin,
    }));
  }, [schedule]);

  const matrixColumns: Column<MatrixRow>[] = [
    { key: "owner", label: "Owner" },
    { key: "opponent", label: "Opponent" },
    { key: "w", label: "W", numeric: true },
    { key: "l", label: "L", numeric: true },
    { key: "games", label: "Games", numeric: true },
    { key: "winPct", label: "Win%", numeric: true, render: (r) => r.winPct.toFixed(3) },
    { key: "avgMargin", label: "Avg Margin", numeric: true, render: (r) => fmt(r.avgMargin, 1) },
  ];

  return (
    <>
      <div className="row g-3">
        <div className="col-md-4">
          <Card header="Select Matchup">
            <div className="mb-3">
              <label className="form-label" htmlFor="h2h-team1">
                Owner 1
              </label>
              <select
                id="h2h-team1"
                className="form-select"
                value={team1}
                onChange={(e) => setTeam1(e.target.value)}
              >
                {owners.map((o) => (
                  <option key={o}>{o}</option>
                ))}
              </select>
            </div>
            <div className="mb-3">
              <label className="form-label" htmlFor="h2h-team2">
                Owner 2
              </label>
              <select
                id="h2h-team2"
                className="form-select"
                value={team2}
                onChange={(e) => setTeam2(e.target.value)}
              >
                {owners.map((o) => (
                  <option key={o}>{o}</option>
                ))}
              </select>
            </div>
            <div className="form-check">
              <input
                className="form-check-input"
                type="checkbox"
                id="h2h-reg-only"
                checked={regOnly}
                onChange={(e) => setRegOnly(e.target.checked)}
              />
              <label className="form-check-label" htmlFor="h2h-reg-only">
                Regular Season Only
              </label>
            </div>
            <button
              type="button"
              className="btn btn-primary w-100 mt-2"
              onClick={() => setApplied({ team1, team2, regOnly })}
            >
              Compare
            </button>
          </Card>
        </div>
        <div className="col-md-8">
          <Card header="Head-to-Head Record">
            {summary === null ? (
              <h5>No matchups found between these owners.</h5>
            ) : (
              <>
                <div className="text-center mb-3">
                  <h4>
                    {applied.team1} vs {applied.team2}
                  </h4>
                  <h5>
                    {summary.wins}W - {summary.losses}L
                    {summary.ties > 0 ? ` - ${summary.ties}T` : ""}
                  </h5>
                  <small>
                    Avg Score: {summary.avgScore.toFixed(1)} - {summary.avgOpp.toFixed(1)}
                  </small>
                </div>
                <Plot data={plot.data} layout={plot.layout} style={{ height: 350 }} />
              </>
            )}
          </Card>
        </div>
      </div>
      <div className="row g-3 mt-0">
        <div className="col-12">
          <Card header="Head-to-Head Matchup History">
            <DataTable
              columns={detailColumns}
              rows={detailRows}
              pageSize={20}
              searchable={false}
            />
          </Card>
        </div>
      </div>
      <div className="row g-3 mt-0">
        <div className="col-12">
          <Card header="Owner vs Owner Record Matrix (Regular Season)">
            <DataTable columns={matrixColumns} rows={matrixRows} pageSize={25} />
          </Card>
        </div>
      </div>
    </>
  );
}
