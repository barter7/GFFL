"use client";

import { Suspense, useEffect, useMemo, useState } from "react";
import { useSearchParams } from "next/navigation";
import Card from "@/components/Card";
import DataTable, { Column } from "@/components/DataTable";
import Plot from "@/components/Plot";
import { getLeagueData, fmt } from "@/lib/data";
import { computeAlltimeStandings } from "@/lib/league";

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

// styleInterval colors from app.R (good = green, mid = yellow, bad = red)
const GOOD = "rgba(40,167,69,0.2)";
const MID = "rgba(255,193,7,0.2)";
const BAD = "rgba(220,53,69,0.2)";

// R quantile (type 7) at probs 0.33 / 0.66
function quantile(values: number[], p: number): number {
  const v = [...values].sort((a, b) => a - b);
  if (v.length === 0) return 0;
  const h = (v.length - 1) * p;
  const lo = Math.floor(h);
  const hi = Math.ceil(h);
  return v[lo] + (v[hi] - v[lo]) * (h - lo);
}

// safe_breaks from app.R — handle case where all values are the same
function breaksFor(values: number[]): [number, number] {
  let b: [number, number] = [quantile(values, 0.33), quantile(values, 0.66)];
  if (b[0] === b[1]) b = [b[0] - 0.001, b[0] + 0.001];
  return b;
}

// DT::styleInterval — value <= break1 -> first color, <= break2 -> second, else third
function intervalBg(value: number, breaks: [number, number], colors: [string, string, string]): string {
  if (value <= breaks[0]) return colors[0];
  if (value <= breaks[1]) return colors[1];
  return colors[2];
}

// Fill the whole table cell with the conditional background (like DT formatStyle)
function bgCell(text: string, background: string) {
  return (
    <span style={{ display: "block", margin: "-0.5rem", padding: "0.5rem", background }}>
      {text}
    </span>
  );
}

interface AlltimeTableRow {
  team: string;
  seasons: number;
  w: number;
  l: number;
  winPct: number;
  pf: number;
  pa: number;
  pfg: number;
}

interface SeasonTableRow {
  rank: number;
  team: string;
  w: number;
  l: number;
  winPct: number;
  pf: number;
  pa: number;
  pfg: number;
}

interface CellBreaks {
  w: [number, number];
  pf: [number, number];
  pa: [number, number];
  pfg: [number, number];
}

function computeBreaks(rows: { w: number; pf: number; pa: number; pfg: number }[]): CellBreaks {
  return {
    w: breaksFor(rows.map((r) => r.w)),
    pf: breaksFor(rows.map((r) => r.pf)),
    pa: breaksFor(rows.map((r) => r.pa)),
    pfg: breaksFor(rows.map((r) => r.pfg)),
  };
}

function StandingsInner() {
  const { standings, seasons } = getLeagueData();
  const seasonsDesc = useMemo(() => [...seasons].sort((a, b) => b - a), [seasons]);

  // Initial state from the URL (?season=2023), validated against known seasons
  const searchParams = useSearchParams();
  const [season, setSeason] = useState<string>(() => {
    const s = searchParams.get("season");
    if (s !== null && seasons.some((yr) => String(yr) === s)) return s;
    return "All-Time";
  });

  // Keep the URL shareable as the selector changes (default omitted)
  useEffect(() => {
    updateQuery({ season: season === "All-Time" ? null : season });
  }, [season]);

  const isAllTime = season === "All-Time";

  // Actual season champion (league_rank 1), shown with a trophy in season view
  const championOwner = useMemo(() => {
    if (isAllTime) return null;
    const yr = parseInt(season, 10);
    return standings.find((s) => s.season === yr && s.league_rank === 1)?.owner ?? null;
  }, [standings, season, isAllTime]);

  const standingsTitle = isAllTime ? "All-Time Standings" : `${season} Standings`;
  const boxPlotTitle = isAllTime
    ? "All-Time Weekly Score Distribution"
    : `${season} Weekly Score Distribution`;

  // ---- Standings table data -------------------------------------------------
  const alltimeRows = useMemo<AlltimeTableRow[]>(() => {
    return computeAlltimeStandings(standings).map((r) => ({
      team: r.team,
      seasons: r.seasons,
      w: r.wins,
      l: r.losses,
      winPct: r.winPct,
      pf: r.pf,
      pa: r.pa,
      pfg: r.pfPerGame,
    }));
  }, [standings]);

  const seasonRows = useMemo<SeasonTableRow[]>(() => {
    if (isAllTime) return [];
    const yr = parseInt(season, 10);
    return standings
      .filter((s) => s.season === yr)
      .sort((a, b) => b.h2h_wins - a.h2h_wins || b.points_for - a.points_for)
      .map((s, i) => {
        const games = s.h2h_wins + s.h2h_losses + (s.h2h_ties ?? 0);
        return {
          rank: i + 1,
          team: s.owner,
          w: s.h2h_wins,
          l: s.h2h_losses,
          winPct: games > 0 ? s.h2h_wins / games : 0,
          pf: s.points_for,
          pa: s.points_against,
          pfg: games > 0 ? s.points_for / games : 0,
        };
      });
  }, [standings, season, isAllTime]);

  const breaks = useMemo<CellBreaks>(
    () => computeBreaks(isAllTime ? alltimeRows : seasonRows),
    [isAllTime, alltimeRows, seasonRows]
  );

  // Shared stat columns (W/PF good high, PA good low) with DT-style backgrounds
  function statColumns<T extends { w: number; l: number; winPct: number; pf: number; pa: number; pfg: number }>(): Column<T>[] {
    return [
      {
        key: "w",
        label: "W",
        numeric: true,
        render: (r) => bgCell(String(r.w), intervalBg(r.w, breaks.w, [BAD, MID, GOOD])),
      },
      { key: "l", label: "L", numeric: true },
      {
        key: "winPct",
        label: "Win%",
        numeric: true,
        render: (r) => `${(r.winPct * 100).toFixed(1)}%`,
      },
      {
        key: "pf",
        label: "PF",
        numeric: true,
        render: (r) => bgCell(fmt(r.pf, 1), intervalBg(r.pf, breaks.pf, [BAD, MID, GOOD])),
      },
      {
        key: "pa",
        label: "PA",
        numeric: true,
        render: (r) => bgCell(fmt(r.pa, 1), intervalBg(r.pa, breaks.pa, [GOOD, MID, BAD])),
      },
      {
        key: "pfg",
        label: "PF/G",
        numeric: true,
        render: (r) => bgCell(fmt(r.pfg, 1), intervalBg(r.pfg, breaks.pfg, [BAD, MID, GOOD])),
      },
    ];
  }

  const alltimeColumns: Column<AlltimeTableRow>[] = [
    { key: "team", label: "Team" },
    { key: "seasons", label: "Seasons", numeric: true },
    ...statColumns<AlltimeTableRow>(),
  ];

  const seasonColumns: Column<SeasonTableRow>[] = [
    { key: "rank", label: "Rank", numeric: true },
    {
      key: "team",
      label: "Team",
      render: (r) =>
        r.team === championOwner ? (
          <span style={{ whiteSpace: "nowrap" }}>
            {r.team}{" "}
            <span title="League Champion" aria-label="League Champion">
              🏆
            </span>
          </span>
        ) : (
          r.team
        ),
    },
    ...statColumns<SeasonTableRow>(),
  ];

  // ---- Box plot (season_points_dist_plot) -----------------------------------
  const boxPlot = useMemo(() => {
    const df = isAllTime
      ? standings
      : standings.filter((s) => s.season === parseInt(season, 10));

    // Order owners by total PF (ascending; with horizontal axis the top
    // scorer ends up at the top, matching coord_flip in the original)
    const totals = new Map<string, number>();
    for (const s of df) totals.set(s.owner, (totals.get(s.owner) ?? 0) + s.points_for);
    const ownerOrder = [...totals.entries()].sort((a, b) => a[1] - b[1]).map((e) => e[0]);

    const traces: Record<string, unknown>[] = ownerOrder.map((owner) => {
      const rows = df.filter((s) => s.owner === owner);
      return {
        type: "box",
        orientation: "h",
        x: rows.map((r) => r.points_for),
        y: rows.map(() => owner),
        name: owner,
        boxpoints: "all",
        jitter: 0.4,
        pointpos: 0,
        fillcolor: "rgba(1,51,105,0.7)", // #013369 alpha 0.7
        line: { color: "#8b6914" },
        marker: { color: "#d4a84b", size: 9, opacity: 0.9 },
        text: rows.map((r) => `${owner}<br>${r.season}: ${Math.round(r.points_for)} pts`),
        hoverinfo: "text",
        showlegend: false,
      };
    });

    const layout: Record<string, unknown> = {
      showlegend: false,
      xaxis: { title: { text: "Season Total Points" } },
      yaxis: {
        categoryorder: "array",
        categoryarray: ownerOrder,
        automargin: true,
        showgrid: false,
      },
    };

    return { traces, layout };
  }, [standings, season, isAllTime]);

  const seasonSelect = (
    <select
      className="form-select form-select-sm"
      style={{ width: "auto" }}
      value={season}
      onChange={(e) => setSeason(e.target.value)}
      aria-label="Season"
    >
      <option value="All-Time">All-Time</option>
      {seasonsDesc.map((yr) => (
        <option key={yr} value={String(yr)}>
          {yr}
        </option>
      ))}
    </select>
  );

  return (
    <div className="row">
      <div className="col-md-8">
        <Card header={standingsTitle} headerExtra={seasonSelect}>
          {isAllTime ? (
            <DataTable
              columns={alltimeColumns}
              rows={alltimeRows}
              pageSize={20}
              searchable={false}
              initialSort={{ key: "w", dir: "desc" }}
            />
          ) : (
            <DataTable
              columns={seasonColumns}
              rows={seasonRows}
              pageSize={20}
              searchable={false}
              initialSort={{ key: "w", dir: "desc" }}
            />
          )}
        </Card>
      </div>
      <div className="col-md-4">
        <Card header={boxPlotTitle}>
          <Plot
            data={boxPlot.traces}
            layout={boxPlot.layout}
            style={{ height: "clamp(300px, 85vw, 400px)" }}
          />
        </Card>
      </div>
    </div>
  );
}

// Static export requires useSearchParams() to live inside a <Suspense>
// boundary, otherwise the build fails with missing-suspense-with-csr-bailout.
export default function StandingsPage() {
  return (
    <Suspense fallback={null}>
      <StandingsInner />
    </Suspense>
  );
}
