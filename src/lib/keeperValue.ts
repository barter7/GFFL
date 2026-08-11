// Pricing a keeper: what is keeping him at round N worth versus just
// drafting whoever falls to round N?
//
// VAL is the half-PPR value-over-replacement column from the GFFL draft
// board (src/data/val-board.json, a derived copy of that site's data —
// name, team, pos, VAL, ADP, ECR, VAL rank). For each round N the
// baseline is the average VAL of the players the market expects to go
// in round N (ADP inside round N, 12 picks a round). A keeper's boost
// is his own VAL minus the baseline of his cost round: keep a VAL-8
// player where VAL-3 players are the alternative and the keep is +5.
//
// MATCHING IS BY NAME — the ESPN keeper rows and the board share no id.
// Names are normalised (case, periods, apostrophes, generational
// suffixes) and matched name+position first, then name alone when it is
// unique. D/ST rows match on the team nickname ("Vikings D/ST" ->
// "Minnesota Vikings"). A keeper the 243-row board does not carry gets
// no number rather than a guessed one — off the board means his VAL is
// below the last rostered man, which is its own answer.

import boardJson from "@/data/val-board.json";

export const LEAGUE_TEAMS = 12;

interface BoardRow {
  name: string;
  team: string | null;
  pos: string;
  val: number;
  adp: number | null;
  ecr: number | null;
  vord: number | null;
}

export interface KeeperValue {
  /** the board row's VAL (half PPR), or null when he is off the board */
  val: number | null;
  /** average VAL of players with an ADP in the cost round */
  baseline: number | null;
  /** val - baseline; what the keep is worth over an ordinary pick */
  boost: number | null;
  /** how many board players set the baseline for that round */
  baseN: number;
  /** the matched board row's name (evidence the match is right) */
  matched?: string;
}

function norm(name: string): string {
  return name
    .toLowerCase()
    .normalize("NFD")
    .replace(/[̀-ͯ]/g, "")
    .replace(/[.'’]/g, "")
    .replace(/\s+(jr|sr|ii|iii|iv|v)$/i, "")
    .replace(/\s+/g, " ")
    .trim();
}

/** "Vikings D/ST" -> "vikings"; "Minnesota Vikings" -> "vikings" */
function dstKey(name: string): string {
  const words = name.replace(/\s*d\/st$/i, "").trim().split(/\s+/);
  return words[words.length - 1].toLowerCase();
}

const board: BoardRow[] = (() => {
  const { columns, rows } = boardJson as {
    columns: string[];
    rows: (string | number | null)[][];
  };
  return rows.map((r) => {
    const o: Record<string, unknown> = {};
    columns.forEach((c, i) => (o[c] = r[i]));
    return o as unknown as BoardRow;
  });
})();

export const BOARD_AS_OF: string =
  (boardJson as { meta?: { as_of?: string } }).meta?.as_of ?? "";

// name+pos first; bare name only when it is unique on the board
const byNamePos = new Map<string, BoardRow>();
const byName = new Map<string, BoardRow | "ambiguous">();
const byDst = new Map<string, BoardRow>();
for (const r of board) {
  if (r.pos === "DST") {
    byDst.set(dstKey(r.name), r);
    continue;
  }
  const n = norm(r.name);
  byNamePos.set(`${n}|${r.pos}`, r);
  byName.set(n, byName.has(n) ? "ambiguous" : r);
}

/** Average VAL of the players the market sends off in each round
 *  (ADP is round.pick, so its integer part IS the round). Rounds the
 *  ADP data no longer reaches fall back to the VAL-rank slice — picks
 *  (N-1)*12+1 .. N*12 down the board — and a round past the whole
 *  board prices at the board's floor, i.e. replacement. */
const roundBaseline = (() => {
  const byRound = new Map<number, number[]>();
  for (const r of board) {
    if (r.adp == null || r.val == null) continue;
    const rd = Math.floor(r.adp);
    const arr = byRound.get(rd) ?? [];
    arr.push(r.val);
    byRound.set(rd, arr);
  }
  const sortedByRank = [...board]
    .filter((r) => r.val != null)
    .sort((a, b) => (a.vord ?? 9999) - (b.vord ?? 9999));
  return (round: number): { baseline: number | null; n: number } => {
    const adp = byRound.get(round) ?? [];
    if (adp.length >= 3) {
      return {
        baseline: adp.reduce((s, x) => s + x, 0) / adp.length,
        n: adp.length,
      };
    }
    const slice = sortedByRank.slice(
      (round - 1) * LEAGUE_TEAMS,
      round * LEAGUE_TEAMS
    );
    if (slice.length) {
      return {
        baseline: slice.reduce((s, x) => s + x.val, 0) / slice.length,
        n: slice.length,
      };
    }
    return { baseline: 0, n: 0 };
  };
})();

export function keeperValue(
  name: string,
  pos: string,
  costRound: number
): KeeperValue {
  let row: BoardRow | undefined;
  if (pos === "DST" || /d\/st$/i.test(name)) {
    row = byDst.get(dstKey(name));
  } else {
    const n = norm(name);
    row = byNamePos.get(`${n}|${pos}`);
    if (!row) {
      const bare = byName.get(n);
      if (bare && bare !== "ambiguous") row = bare;
    }
  }
  const { baseline, n } = roundBaseline(costRound);
  if (!row || row.val == null) {
    return { val: null, baseline, boost: null, baseN: n };
  }
  return {
    val: row.val,
    baseline,
    boost: baseline == null ? null : row.val - baseline,
    baseN: n,
    matched: row.name,
  };
}
