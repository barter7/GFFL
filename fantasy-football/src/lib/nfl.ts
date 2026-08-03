// NFL depth-chart / draft-board data, produced by nflmodel/export_site.R.
//
// The site is a static export, so this JSON is imported at build
// time rather than fetched — same pattern as src/lib/data.ts.
//
// Every join key here is `gsis_id`, never a player name (nflmodel
// rule R1.13). Two players share a name often enough that name
// matching quietly mixes them up.

import raw from "@/data/nfl-depth.json";

type Columnar = { columns: string[]; rows: unknown[][] };

function toObjects<T>(data: Columnar): T[] {
  const { columns, rows } = data;
  return rows.map((row) => {
    const obj: Record<string, unknown> = {};
    columns.forEach((c, i) => (obj[c] = row[i]));
    return obj as T;
  });
}

/** A player on a 2026 depth chart. */
export interface DepthRow {
  gsis_id: string;
  team: string;
  /** Position group: QB, RB, WR, TE, OL, DL, LB, DB. */
  grp: string;
  /** Depth-chart slot within the group, 1 = starter. */
  slot: number;
  /** Specific position label (e.g. "SS", "LT"). */
  spot: string | null;
  player_name: string;
  /** NEW = on another team in 2025, ROOKIE = draft class, RET = returning. */
  status: "NEW" | "ROOKIE" | "RET";
  /** Prior team for NEW, or draft slot ("R1.24" / "UDFA") for ROOKIE. */
  from: string | null;
  snaps25: number | null;
  line_2025: string | null;
  line_career: string | null;
  /** Non-active roster status, e.g. "reserve list (IR/PUP/NFI)". */
  avail_note: string | null;
  rotowire_url: string | null;
  /** Weeks listed Out FOR INJURY in 2025 — excludes rest/coach's decision. */
  y25_weeks_out_injury: number | null;
  y25_top_injury: string | null;
  blurb?: string | null;
}

/** A 2025 contributor (250+ snaps) no longer on the roster. */
export interface OutRow {
  gsis_id: string;
  team: string;
  grp: string;
  player_name: string;
  spot: string | null;
  /** Destination team, or "FA/none". */
  to: string;
  snaps25: number | null;
  line_2025: string | null;
  line_career: string | null;
  y25_weeks_out_injury: number | null;
  y25_top_injury?: string | null;
}

/** FantasyPros ranking row (nflmodel rule R2.11). */
export interface RankRow {
  gsis_id: string;
  player: string;
  pos: "QB" | "RB" | "WR" | "TE";
  team: string;
  /** Expert consensus rank, 1QB scoring. */
  ecr1: number | null;
  /** Expert consensus rank, superflex. */
  ecr2: number | null;
  r1: number;
  r2: number;
  /** Average draft position — null until an export is present. */
  adp: number | null;
  /** "fp_id" | "name-fallback (rookie)" — how this row was joined. */
  id_source: string;
}

export type SignalDir = "up" | "down" | "watch";

export interface Signal {
  dir: SignalDir;
  /** DRAFT | SIGNED | VACATED | TGTS | CARR | HC | OC | QB | PACE | AVAIL | INJ */
  code: string;
  label: string;
  /** The evidence — usually the other player's actual 2025 line. */
  detail: string;
}

export interface SignalRow {
  gsis_id: string;
  team: string;
  grp: string;
  player: string;
  slot: number;
  signals: Signal[];
}

export interface NflMeta {
  generated: string;
  has_pff: boolean;
  /** False until a FantasyPros ADP export is dropped in. */
  has_adp: boolean;
  ecr_date: string | null;
}

export interface NflDepthData {
  depth: DepthRow[];
  out: OutRow[];
  rank: RankRow[];
  meta: NflMeta;
  /** Signals keyed by gsis_id. */
  signals: Record<string, Signal[]>;
  /** Depth rows keyed by gsis_id, for the board's roster context. */
  depthById: Record<string, DepthRow>;
  teams: string[];
}

let cached: NflDepthData | null = null;

export function getNflDepth(): NflDepthData {
  if (cached) return cached;

  const r = raw as unknown as {
    depth: Columnar;
    out: Columnar;
    rank: Columnar | null;
    sig: SignalRow[];
    meta: NflMeta;
  };

  const depth = toObjects<DepthRow>(r.depth);
  const signals: Record<string, Signal[]> = {};
  (r.sig ?? []).forEach((s) => (signals[s.gsis_id] = s.signals));
  const depthById: Record<string, DepthRow> = {};
  depth.forEach((d) => (depthById[d.gsis_id] = d));

  cached = {
    depth,
    out: toObjects<OutRow>(r.out),
    rank: r.rank ? toObjects<RankRow>(r.rank) : [],
    meta: r.meta,
    signals,
    depthById,
    teams: [...new Set(depth.map((d) => d.team))].sort(),
  };
  return cached;
}

/** Display order for position groups. */
export const POS_GROUPS = ["QB", "RB", "WR", "TE", "OL", "DL", "LB", "DB"];

/** Board position filters. FLEX is the view a draft actually uses. */
export const BOARD_POS = ["ALL", "FLEX", "QB", "RB", "WR", "TE"] as const;
export type BoardPos = (typeof BOARD_POS)[number];

export const BOARD_SORTS = [
  { key: "adp", label: "ADP", needsAdp: true },
  { key: "r1", label: "ECR — 1QB", needsAdp: false },
  { key: "r2", label: "ECR — superflex", needsAdp: false },
] as const;
export type BoardSort = (typeof BOARD_SORTS)[number]["key"];

/** Position colours are fixed by spec — all four distinct, so a row's
 *  position is legible from its rail alone. */
export const POS_COLOR: Record<string, string> = {
  QB: "#c05e85",
  RB: "#73c3a6",
  WR: "#46a2cb",
  TE: "#cc8d4a",
};

/** Returns the CSS-module class for a position, or "" for non-skill
 *  groups (OL/DL/LB/DB have no assigned colour). */
export function posClass(
  styles: Record<string, string>,
  prefix: "rail" | "head" | "ink" | "pill",
  pos: string | undefined,
): string {
  if (!pos) return "";
  return styles[`${prefix}${pos}`] ?? "";
}
