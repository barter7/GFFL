"use client";

// "Drafts" tab — port of app.R UI lines 118-176 / server lines 1952-2300.

import { Suspense, useEffect, useMemo, useState } from "react";
import { useSearchParams } from "next/navigation";
import Card from "@/components/Card";
import DataTable, { Column } from "@/components/DataTable";
import { getLeagueData } from "@/lib/data";
import DraftGrid from "./DraftGrid";
import PosBreakdownPlot from "./PosBreakdownPlot";
import { computeDraftValueData, DraftValueRow } from "./draftValue";

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

// Position filter for highlighting picks on the board ("DST" kept slash-free
// so the URL param stays clean)
const POS_FILTERS = [
  { label: "All", value: "All" },
  { label: "QB", value: "QB" },
  { label: "RB", value: "RB" },
  { label: "WR", value: "WR" },
  { label: "TE", value: "TE" },
  { label: "K", value: "K" },
  { label: "D/ST", value: "DST" },
];

interface DraftListRow {
  round: number;
  pick: number;
  overall: number;
  owner: string;
  player: string;
  pos: string;
  team: string;
}

const draftListColumns: Column<DraftListRow>[] = [
  { key: "round", label: "Round", numeric: true },
  { key: "pick", label: "Pick", numeric: true },
  { key: "overall", label: "Overall", numeric: true },
  { key: "owner", label: "Owner" },
  { key: "player", label: "Player" },
  { key: "pos", label: "Pos" },
  { key: "team", label: "Team" },
];

interface ValueRow {
  season: number;
  owner: string;
  player: string;
  pos: string;
  draftedAs: string;
  finishedAs: string;
  points: number;
  expectedPts: number;
  diff: number;
}

function valueColumns(diffLabel: string): Column<ValueRow>[] {
  return [
    { key: "season", label: "Season", numeric: true },
    { key: "owner", label: "Owner" },
    { key: "player", label: "Player" },
    { key: "pos", label: "Pos" },
    { key: "draftedAs", label: "Drafted As" },
    { key: "finishedAs", label: "Finished As" },
    { key: "points", label: "Points", numeric: true },
    { key: "expectedPts", label: "Expected Pts", numeric: true },
    { key: "diff", label: diffLabel, numeric: true },
  ];
}

function toValueRow(d: DraftValueRow): ValueRow {
  return {
    season: d.season,
    owner: d.owner,
    player: d.player_name,
    pos: d.pos,
    draftedAs: `${d.pos}${d.drafted_pos_rank}`,
    finishedAs: `${d.pos}${d.actual_pos_rank}`,
    points: Math.round(d.total_pts),
    expectedPts: Math.round(d.expected_pts ?? 0),
    diff: Math.round(d.value_diff ?? 0),
  };
}

function DraftsInner() {
  const { drafts, starters, seasons } = getLeagueData();
  const seasonChoices = useMemo(
    () => [...seasons].sort((a, b) => b - a),
    [seasons]
  );

  // Initial state from the URL (?season=2021&pos=RB), validated
  const searchParams = useSearchParams();
  const [season, setSeason] = useState(() => {
    const s = searchParams.get("season");
    if (s !== null && seasons.some((yr) => String(yr) === s)) return Number(s);
    return seasonChoices[0];
  });
  const [posFilter, setPosFilter] = useState<string>(() => {
    const p = searchParams.get("pos");
    return p !== null && POS_FILTERS.some((f) => f.value === p && p !== "All")
      ? p
      : "All";
  });

  // Keep the URL shareable as the selectors change (defaults omitted)
  useEffect(() => {
    updateQuery({
      season: season === seasonChoices[0] ? null : String(season),
      pos: posFilter === "All" ? null : posFilter,
    });
  }, [season, posFilter, seasonChoices]);

  const draftList = useMemo<DraftListRow[]>(
    () =>
      drafts
        .filter((d) => d.season === season)
        .sort((a, b) => a.overall - b.overall)
        .map((d) => ({
          round: d.round,
          pick: d.pick,
          overall: d.overall,
          owner: d.owner,
          player: d.player_name,
          pos: d.pos,
          team: d.team ?? "",
        })),
    [drafts, season]
  );

  const valueData = useMemo(
    () => computeDraftValueData(drafts, starters),
    [drafts, starters]
  );

  const busts = useMemo(
    () =>
      valueData
        .filter(
          (d) =>
            d.value_diff != null && d.expected_pts != null && d.expected_pts > 0
        )
        .sort((a, b) => a.value_diff! - b.value_diff!)
        .slice(0, 25)
        .map(toValueRow),
    [valueData]
  );

  const values = useMemo(
    () =>
      valueData
        .filter((d) => d.value_diff != null && d.expected_pts != null)
        .sort((a, b) => b.value_diff! - a.value_diff!)
        .slice(0, 25)
        .map(toValueRow),
    [valueData]
  );

  const accurate = useMemo(
    () =>
      valueData
        .filter(
          (d) =>
            d.value_diff != null &&
            d.expected_pts != null &&
            d.drafted_pos_rank <= 40 &&
            d.actual_pos_rank <= 40
        )
        .sort((a, b) => Math.abs(a.value_diff!) - Math.abs(b.value_diff!))
        .slice(0, 25)
        .map(toValueRow),
    [valueData]
  );

  const round1 = useMemo(() => drafts.filter((d) => d.round === 1), [drafts]);

  return (
    <>
      <div className="row mb-4">
        <div className="col-12">
          <Card
            header="Draft Board"
            headerExtra={
              <select
                className="form-select form-select-sm"
                style={{ width: 120 }}
                value={season}
                onChange={(e) => setSeason(Number(e.target.value))}
              >
                {seasonChoices.map((s) => (
                  <option key={s} value={s}>
                    {s}
                  </option>
                ))}
              </select>
            }
          >
            <div
              className="btn-group btn-group-sm mb-2 flex-wrap"
              role="group"
              aria-label="Highlight position"
            >
              {POS_FILTERS.map((f) => (
                <button
                  key={f.value}
                  type="button"
                  className={`btn ${
                    posFilter === f.value ? "btn-primary" : "btn-outline-secondary"
                  }`}
                  onClick={() => setPosFilter(f.value)}
                >
                  {f.label}
                </button>
              ))}
            </div>
            <div
              style={{
                overflowX: "auto",
                WebkitOverflowScrolling: "touch",
                minHeight: 800,
              }}
            >
              <DraftGrid
                drafts={drafts}
                starters={starters}
                season={season}
                posFilter={posFilter === "All" ? null : posFilter}
              />
            </div>
          </Card>
        </div>
      </div>

      <div className="row mb-4">
        <div className="col-12">
          <Card header="Draft Results (List)">
            <DataTable columns={draftListColumns} rows={draftList} pageSize={25} />
          </Card>
        </div>
      </div>

      <div className="row mb-4">
        <div className="col-md-6">
          <Card header="Round 1 Position Breakdown (%)">
            <PosBreakdownPlot rows={round1} yTitle="% of Round 1 Picks" />
          </Card>
        </div>
        <div className="col-md-6">
          <Card header="Overall Draft Position Breakdown (%)">
            <PosBreakdownPlot rows={drafts} yTitle="% of All Picks" />
          </Card>
        </div>
      </div>

      <div className="row mb-4">
        <div className="col-12">
          <Card header="Biggest Draft Busts">
            <p className="text-muted small">
              Players whose actual points fell well short of what their draft
              slot should have yielded
            </p>
            <DataTable
              columns={valueColumns("Pts Lost")}
              rows={busts}
              pageSize={15}
              searchable={false}
            />
          </Card>
        </div>
      </div>

      <div className="row mb-4">
        <div className="col-12">
          <Card header="Biggest Draft Values">
            <p className="text-muted small">
              Players who dramatically outscored their draft position&apos;s
              expected value
            </p>
            <DataTable
              columns={valueColumns("Pts Gained")}
              rows={values}
              pageSize={15}
              searchable={false}
            />
          </Card>
        </div>
      </div>

      <div className="row mb-4">
        <div className="col-12">
          <Card header="Closest to Draft Value">
            <p className="text-muted small">
              Players who finished closest to where they were drafted (min
              top-40 at position)
            </p>
            <DataTable
              columns={valueColumns("Pts Diff")}
              rows={accurate}
              pageSize={15}
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
export default function DraftsPage() {
  return (
    <Suspense fallback={null}>
      <DraftsInner />
    </Suspense>
  );
}
