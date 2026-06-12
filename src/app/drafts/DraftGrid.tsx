"use client";

// Port of output$draft_grid (app.R ~1957-2080): visual draft board grid by
// round/owner with position colors, season points and position rank.

import { useMemo } from "react";
import { DraftRow, StarterRow } from "@/lib/data";
import { rankMinDesc, seasonPointsFromStarters } from "./draftValue";

// Color by position
const POS_COLORS: Record<string, string> = {
  QB: "#FF6B6B",
  RB: "#4ECDC4",
  WR: "#45B7D1",
  TE: "#FFA07A",
  K: "#C9C9C9",
  DST: "#A0A0A0",
};

interface GridPick extends DraftRow {
  total_pts: number;
  pos_rank: number;
}

interface Props {
  drafts: DraftRow[];
  starters: StarterRow[];
  season: number;
  /** When set (e.g. "RB"), matching picks are highlighted and others dimmed. */
  posFilter?: string | null;
}

// D/ST appears under a few different codes in the data
function matchesPos(pos: string, filter: string): boolean {
  if (filter === "DST") return ["DST", "D/ST", "DEF"].includes(pos);
  return pos === filter;
}

export default function DraftGrid({ drafts, starters, season, posFilter = null }: Props) {
  const grid = useMemo(() => {
    const draft = drafts.filter((d) => d.season === season);
    if (draft.length === 0) return null;

    // Get season points - rostered points from starters data
    // (static site equivalent of the rv$starters_data fallback in app.R)
    const seasonPts = new Map(
      seasonPointsFromStarters(starters, season).map((p) => [
        `${p.player_name}|${p.pos}`,
        Math.round(p.total_pts),
      ])
    );

    // Compute position rank among all drafted players
    const picks: GridPick[] = draft.map((d) => ({
      ...d,
      total_pts: seasonPts.get(`${d.player_name}|${d.pos}`) ?? 0,
      pos_rank: 0,
    }));
    const byPos = new Map<string, GridPick[]>();
    for (const p of picks) {
      const arr = byPos.get(p.pos) ?? [];
      arr.push(p);
      byPos.set(p.pos, arr);
    }
    for (const arr of byPos.values()) {
      const ranks = rankMinDesc(arr.map((p) => p.total_pts));
      arr.forEach((p, i) => (p.pos_rank = ranks[i]));
    }

    // Get unique owners in draft order
    const owners = [
      ...new Set(
        picks
          .filter((p) => p.round === 1)
          .sort((a, b) => a.pick - b.pick)
          .map((p) => p.owner)
      ),
    ];
    const maxRound = Math.max(...picks.map((p) => p.round));

    return { picks, owners, maxRound };
  }, [drafts, starters, season]);

  if (!grid) {
    return <h5 className="text-muted text-center">No draft data.</h5>;
  }

  const { picks, owners, maxRound } = grid;
  const rounds = Array.from({ length: maxRound }, (_, i) => i + 1);

  return (
    <div
      style={{
        overflowX: "auto",
        maxWidth: "100%",
        WebkitOverflowScrolling: "touch",
      }}
    >
      <table style={{ borderCollapse: "collapse", width: "auto" }}>
        <thead>
          <tr>
            <th
              style={{
                padding: "4px 6px",
                background: "#222",
                color: "#fff",
                fontSize: 11,
                position: "sticky",
                left: 0,
                zIndex: 3,
              }}
            >
              RD
            </th>
            {owners.map((o) => (
              <th
                key={o}
                style={{
                  padding: "4px 6px",
                  background: "#013369",
                  color: "#fff",
                  fontSize: 10,
                  textAlign: "center",
                  minWidth: 100,
                  whiteSpace: "nowrap",
                }}
              >
                {o}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rounds.map((rd) => (
            <tr key={rd}>
              <td
                style={{
                  padding: "4px 6px",
                  fontWeight: "bold",
                  background: "#f0f0f0",
                  fontSize: 12,
                  position: "sticky",
                  left: 0,
                  zIndex: 2,
                }}
              >
                RD {rd}
              </td>
              {owners.map((o) => {
                const p = picks.find((x) => x.round === rd && x.owner === o);
                if (!p) {
                  return (
                    <td
                      key={o}
                      style={{
                        padding: "4px 6px",
                        background: "#f8f8f8",
                        border: "1px solid #ddd",
                      }}
                    />
                  );
                }
                const bg = POS_COLORS[p.pos] ?? "#eee";
                const ptsText = `${p.total_pts} pts`;
                const rankText = `${p.pos ?? ""}${p.pos_rank}`;
                return (
                  <td
                    key={o}
                    style={{
                      padding: "4px 6px",
                      background: `${bg}33`,
                      border: "1px solid #ddd",
                      fontSize: 10,
                      textAlign: "center",
                      verticalAlign: "top",
                    }}
                  >
                    <div
                      style={{
                        fontWeight: "bold",
                        fontSize: 11,
                        whiteSpace: "nowrap",
                      }}
                    >
                      {p.player_name}
                    </div>
                    <div style={{ color: "#666", fontSize: 9 }}>
                      {p.pos} - {p.team}
                    </div>
                    <div
                      style={{
                        fontWeight: "bold",
                        fontSize: 10,
                        color: "#013369",
                        marginTop: 2,
                      }}
                    >
                      {ptsText}
                    </div>
                    <div style={{ fontSize: 9, color: "#888" }}>{rankText}</div>
                  </td>
                );
              })}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
