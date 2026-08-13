// Expansion Draft pool — every player drafted in 2025 who is NOT an eligible
// 2026 keeper because he did not end the season on his drafting owner's
// roster (Constitution §1.3). Sorted by 2025 round (the round cost the pick
// carried). Where the player finished the year on a different owner's roster,
// that owner is noted.

import type { CSSProperties } from "react";
import { getLeagueData, headshotUrl } from "@/lib/data";

export const metadata = { title: "Expansion Draft" };

const KEEPER_SEASON = 2025;

function Headshot({ name, pos, size = 34 }: { name: string; pos: string; size?: number }) {
  const url = headshotUrl(name);
  if (!url || pos === "DST") {
    return (
      <div
        style={{
          width: size,
          height: Math.round(size * 0.73),
          borderRadius: 4,
          background: "#e9ecef",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          fontSize: size * 0.5,
          flex: "0 0 auto",
        }}
      >
        🏈
      </div>
    );
  }
  return (
    // eslint-disable-next-line @next/next/no-img-element
    <img
      src={url}
      alt={name}
      width={size}
      height={Math.round(size * 0.73)}
      loading="lazy"
      style={{ borderRadius: 4, background: "#e9ecef", objectFit: "cover", flex: "0 0 auto" }}
    />
  );
}

export default function ExpansionDraftPage() {
  const { drafts, starters } = getLeagueData();

  const draft25 = drafts.filter((d) => d.season === KEEPER_SEASON);
  const finalWeek = Math.max(
    ...starters.filter((s) => s.season === KEEPER_SEASON).map((s) => s.week)
  );

  // player_id -> owner whose final roster held him
  const finalHolder = new Map<number, string>();
  for (const s of starters) {
    if (s.season === KEEPER_SEASON && s.week === finalWeek) {
      finalHolder.set(s.player_id, s.owner);
    }
  }

  const pool = draft25
    .filter((d) => finalHolder.get(d.player_id) !== d.owner)
    .sort((a, b) => a.round - b.round || a.overall - b.overall)
    .map((d) => ({
      round: d.round,
      name: d.player_name,
      pos: d.pos,
      team: d.team,
      draftedBy: d.owner,
      keptLastYear: d.is_keeper === true,
      endedWith: finalHolder.get(d.player_id) ?? null,
    }));

  const rowStyle: CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: 8,
    padding: "3px 6px",
    fontSize: 13.5,
    borderBottom: "1px solid #f0f0f0",
  };

  // Group rows by round for a clean sorted layout
  const rounds = [...new Set(pool.map((p) => p.round))].sort((a, b) => a - b);

  return (
    <>
      <div className="text-center my-3">
        <h2 style={{ color: "#013369", fontFamily: "Georgia,serif" }}>Expansion Draft</h2>
        <hr style={{ borderColor: "#013369", width: 200, margin: "0 auto" }} />
        <p
          className="text-muted mt-2 mb-0"
          style={{ fontSize: 13, maxWidth: 720, marginLeft: "auto", marginRight: "auto" }}
        >
          Every player drafted in {KEEPER_SEASON} who is <strong>not</strong> an eligible
          keeper because he did not end the season on his drafting owner&apos;s roster
          (§1.3). Sorted by the round cost the pick carried. {pool.length} players.
        </p>
      </div>

      <div className="card" style={{ maxWidth: 640, margin: "0 auto" }}>
        <div className="card-body">
          {rounds.map((rd) => (
            <div key={rd} className="mb-2">
              <div
                style={{
                  background: "#013369",
                  color: "#fff",
                  fontWeight: 700,
                  fontSize: 14,
                  textAlign: "center",
                  borderRadius: 6,
                  padding: "3px 0",
                  marginBottom: 3,
                }}
              >
                Round {rd}
              </div>
              <div className="d-flex flex-column" style={{ gap: 2 }}>
                {pool
                  .filter((p) => p.round === rd)
                  .map((p, i) => (
                    <div key={p.name + i} style={rowStyle}>
                      <Headshot name={p.name} pos={p.pos} />
                      <div style={{ flex: 1, minWidth: 0 }}>
                        <div
                          style={{
                            fontWeight: 600,
                            overflow: "hidden",
                            textOverflow: "ellipsis",
                            whiteSpace: "nowrap",
                          }}
                        >
                          {p.name}
                          {p.keptLastYear && (
                            <span style={{ color: "#8a6508", fontSize: 11 }}> ★</span>
                          )}
                        </div>
                        <div className="text-muted" style={{ fontSize: 11 }}>
                          {p.pos}
                          {p.team ? ` · ${p.team}` : ""} · drafted by {p.draftedBy}
                          {p.endedWith
                            ? ` · ended with ${p.endedWith}`
                            : " · unrostered at year end"}
                        </div>
                      </div>
                      <span
                        style={{
                          fontWeight: 700,
                          color: "#013369",
                          fontSize: 15,
                          whiteSpace: "nowrap",
                        }}
                      >
                        {p.round}
                      </span>
                    </div>
                  ))}
              </div>
            </div>
          ))}
        </div>
      </div>
    </>
  );
}
