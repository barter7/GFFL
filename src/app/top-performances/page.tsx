"use client";

// "Top Performances" tab — ported from app.R lines 301-334 (UI) and 2301-2456 (server).

import { Suspense, useEffect, useMemo, useState } from "react";
import { useSearchParams } from "next/navigation";
import Card from "@/components/Card";
import DataTable, { Column } from "@/components/DataTable";
import Plot from "@/components/Plot";
import { getLeagueData, headshotUrl, fmt, BENCH_SLOTS, StarterRow } from "@/lib/data";

const POS_CHOICES = ["All", "QB", "RB", "WR", "TE", "K", "D/ST"];

// "D/ST" is written to the URL as "DST" to keep the query string slash-free
const posToParam = (p: string) => (p === "D/ST" ? "DST" : p);
const paramToPos = (p: string) => (p === "DST" ? "D/ST" : p);

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

// ggplot2's default discrete fill palette (scales::hue_pal): evenly spaced
// hues in HCL space with c = 100, l = 65 — converted to sRGB hex.
function hclToHex(h: number, c: number, l: number): string {
  const hr = (h * Math.PI) / 180;
  const U = c * Math.cos(hr);
  const V = c * Math.sin(hr);
  const Y = l > 7.999592 ? Math.pow((l + 16) / 116, 3) : l / 903.3;
  const un = 0.1978398;
  const vn = 0.4683363;
  const u = U / (13 * l) + un;
  const v = V / (13 * l) + vn;
  const X = (9 * Y * u) / (4 * v);
  const Z = (Y * (12 - 3 * u - 20 * v)) / (4 * v);
  const rl = 3.240479 * X - 1.53715 * Y - 0.498535 * Z;
  const gl = -0.969256 * X + 1.875992 * Y + 0.041556 * Z;
  const bl = 0.055648 * X - 0.204043 * Y + 1.057311 * Z;
  const enc = (x: number) => {
    const cl = Math.max(0, Math.min(1, x));
    const s = cl <= 0.0031308 ? 12.92 * cl : 1.055 * Math.pow(cl, 1 / 2.4) - 0.055;
    return Math.round(s * 255)
      .toString(16)
      .padStart(2, "0");
  };
  return `#${enc(rl)}${enc(gl)}${enc(bl)}`;
}

function huePalette(n: number): string[] {
  // hues = seq(15, 375, length.out = n + 1)[1:n]
  return Array.from({ length: n }, (_, i) => hclToHex(15 + (i * 360) / n, 100, 65));
}

const score = (s: StarterRow) => s.player_score ?? -Infinity;

interface PosRecordRow {
  Pos: string;
  Player: string;
  Team: string;
  Score: number;
  Season: number;
  Week: number;
}

const POS_COLUMNS: Column<PosRecordRow>[] = [
  { key: "Pos", label: "Pos" },
  { key: "Player", label: "Player" },
  { key: "Team", label: "Team" },
  { key: "Score", label: "Score", numeric: true, render: (r) => fmt(r.Score, 1) },
  { key: "Season", label: "Season", numeric: true },
  { key: "Week", label: "Week", numeric: true },
];

function PerformanceRow({ row, rank }: { row: StarterRow; rank: number }) {
  const url = headshotUrl(row.player_name);
  const rankColor =
    rank === 1 ? "#FFD700" : rank === 2 ? "#C0C0C0" : rank === 3 ? "#CD7F32" : "#6c757d";

  return (
    <div
      className="d-flex align-items-center"
      style={{
        padding: "4px 2px",
        borderBottom: "1px solid #eee",
        ...(rank <= 3 ? { background: "#f8f9fa" } : {}),
      }}
    >
      {/* Rank */}
      <div
        style={{
          width: 32,
          flex: "0 0 auto",
          fontSize: 14,
          fontWeight: "bold",
          color: rankColor,
          textAlign: "center",
        }}
      >
        #{rank}
      </div>

      {/* Headshot */}
      <div style={{ width: 48, flex: "0 0 auto", display: "flex", justifyContent: "center" }}>
        {url != null && (
          // eslint-disable-next-line @next/next/no-img-element
          <img
            src={url}
            alt={row.player_name}
            style={{
              width: 44,
              height: 44,
              objectFit: "cover",
              borderRadius: "50%",
              border: "2px solid #013369",
              background: "#eee",
            }}
            onError={(e) => {
              e.currentTarget.style.display = "none";
              const sib = e.currentTarget.nextElementSibling as HTMLElement | null;
              if (sib) sib.style.display = "flex";
            }}
          />
        )}
        <div
          style={{
            width: 44,
            height: 44,
            borderRadius: "50%",
            background: "#e9ecef",
            border: "2px solid #013369",
            alignItems: "center",
            justifyContent: "center",
            display: url == null ? "flex" : "none",
            color: "#6c757d",
            fontSize: "1rem",
          }}
        >
          🏈
        </div>
      </div>

      {/* Player info */}
      <div style={{ flex: 1, minWidth: 0, marginLeft: 8 }}>
        <div
          style={{
            fontWeight: "bold",
            fontSize: 14,
            whiteSpace: "nowrap",
            overflow: "hidden",
            textOverflow: "ellipsis",
          }}
        >
          {row.player_name}
        </div>
        <div style={{ color: "#666", fontSize: 11.5, lineHeight: 1.3 }}>
          {row.pos} - {row.team ?? ""} | {row.season} Wk {row.week} | {row.owner}
        </div>
      </div>

      {/* Score */}
      <div
        style={{
          width: 56,
          flex: "0 0 auto",
          textAlign: "right",
          fontSize: 17,
          fontWeight: "bold",
          color: "#013369",
        }}
      >
        {Math.round((row.player_score ?? 0) * 10) / 10}
      </div>
    </div>
  );
}

function TopPerformancesInner() {
  const { starters, seasons } = getLeagueData();

  const seasonChoices = useMemo(
    () => ["All-Time", ...[...seasons].sort((a, b) => b - a).map(String)],
    [seasons]
  );

  // Initial state from the URL (?pos=RB&season=2023), validated
  const searchParams = useSearchParams();
  const [perfPos, setPerfPos] = useState(() => {
    const p = searchParams.get("pos");
    if (p !== null && POS_CHOICES.includes(paramToPos(p))) return paramToPos(p);
    return "All";
  });
  const [perfSeason, setPerfSeason] = useState(() => {
    const s = searchParams.get("season");
    if (s !== null && (s === "All-Time" || seasons.some((yr) => String(yr) === s)))
      return s;
    return "All-Time";
  });

  // Keep the URL shareable as the selectors change (defaults omitted)
  useEffect(() => {
    updateQuery({
      pos: perfPos === "All" ? null : posToParam(perfPos),
      season: perfSeason === "All-Time" ? null : perfSeason,
    });
  }, [perfPos, perfSeason]);

  const activeStarters = useMemo(
    () => starters.filter((s) => !BENCH_SLOTS.has(s.lineup_slot)),
    [starters]
  );

  // Filtered starters for top performances
  const filteredStarters = useMemo(() => {
    let df = activeStarters;
    if (perfSeason !== "All-Time") {
      df = df.filter((s) => s.season === Number(perfSeason));
    }
    if (perfPos !== "All") {
      if (perfPos === "D/ST") {
        df = df.filter((s) => ["DST", "D/ST", "DEF"].includes(s.pos));
      } else {
        df = df.filter((s) => s.pos === perfPos);
      }
    }
    return [...df].sort((a, b) => score(b) - score(a));
  }, [activeStarters, perfPos, perfSeason]);

  const top25 = filteredStarters.slice(0, 25);

  // Top by position (slice_max per pos, keeping ties)
  const byPosition: PosRecordRow[] = useMemo(() => {
    const best = new Map<string, StarterRow[]>();
    for (const s of activeStarters) {
      const cur = best.get(s.pos);
      if (!cur) {
        best.set(s.pos, [s]);
      } else if (score(s) > score(cur[0])) {
        best.set(s.pos, [s]);
      } else if (score(s) === score(cur[0])) {
        cur.push(s);
      }
    }
    return [...best.values()]
      .flat()
      .map((s) => ({
        Pos: s.pos,
        Player: s.player_name,
        Team: s.team ?? "",
        Score: s.player_score ?? 0,
        Season: s.season,
        Week: s.week,
      }))
      .sort((a, b) => b.Score - a.Score);
  }, [activeStarters]);

  // Most appearances in top 100 scores
  const appearances = useMemo(() => {
    const top100 = [...activeStarters].sort((a, b) => score(b) - score(a)).slice(0, 100);
    const counts = new Map<string, number>();
    for (const s of top100) {
      counts.set(s.player_name, (counts.get(s.player_name) ?? 0) + 1);
    }
    return [...counts.entries()]
      .map(([name, n]) => ({ name, n }))
      .sort((a, b) => b.n - a.n || a.name.localeCompare(b.name))
      .slice(0, 15);
  }, [activeStarters]);

  const palette = useMemo(() => huePalette(appearances.length), [appearances.length]);

  const appearancesTrace = useMemo(
    () => [
      {
        type: "bar",
        orientation: "h",
        y: appearances.map((a) => a.name),
        x: appearances.map((a) => a.n),
        marker: {
          // factor levels were reversed in the R code, so the bottom bar gets the first hue
          color: appearances.map((_, i) => palette[appearances.length - 1 - i]),
        },
        hovertemplate: "%{x}<extra></extra>",
      } as Record<string, unknown>,
    ],
    [appearances, palette]
  );

  return (
    <div>
      <div className="row">
        <div className="col-12">
          <Card
            header="Top Player Performances (All-Time)"
            headerExtra={
              <div className="d-flex gap-1 flex-wrap align-items-center">
                <select
                  className="form-select form-select-sm"
                  style={{ width: "auto" }}
                  value={perfPos}
                  onChange={(e) => setPerfPos(e.target.value)}
                  aria-label="Position"
                >
                  {POS_CHOICES.map((p) => (
                    <option key={p} value={p}>
                      {p}
                    </option>
                  ))}
                </select>
                <select
                  className="form-select form-select-sm"
                  style={{ width: "auto" }}
                  value={perfSeason}
                  onChange={(e) => setPerfSeason(e.target.value)}
                  aria-label="Season"
                >
                  {seasonChoices.map((s) => (
                    <option key={s} value={s}>
                      {s}
                    </option>
                  ))}
                </select>
              </div>
            }
          >
            {top25.length === 0 ? (
              <h5 className="text-muted text-center">
                No data available. Run fetch_data.R to cache starters data.
              </h5>
            ) : (
              <div
                style={{
                  maxHeight: "min(700px, 75vh)",
                  overflowY: "auto",
                  WebkitOverflowScrolling: "touch",
                }}
              >
                {top25.map((row, i) => (
                  <PerformanceRow
                    key={`${row.season}-${row.week}-${row.franchise_id}-${row.player_id}-${i}`}
                    row={row}
                    rank={i + 1}
                  />
                ))}
              </div>
            )}
          </Card>
        </div>
      </div>
      <div className="row">
        <div className="col-md-6">
          <Card header="Top Performers by Position">
            <DataTable
              columns={POS_COLUMNS}
              rows={byPosition}
              pageSize={15}
              searchable={false}
            />
          </Card>
        </div>
        <div className="col-md-6">
          <Card header="Most Appearances in Top 100">
            <Plot
              data={appearancesTrace}
              layout={{
                showlegend: false,
                xaxis: { title: { text: "Appearances in Top 100 Scores" } },
                yaxis: { autorange: "reversed", automargin: true },
              }}
              style={{ height: "clamp(320px, 95vw, 400px)" }}
            />
          </Card>
        </div>
      </div>
    </div>
  );
}

// Static export requires useSearchParams() to live inside a <Suspense>
// boundary, otherwise the build fails with missing-suspense-with-csr-bailout.
export default function TopPerformancesPage() {
  return (
    <Suspense fallback={null}>
      <TopPerformancesInner />
    </Suspense>
  );
}
