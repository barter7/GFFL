"use client";

// Hall of Fame — port of app.R output$hof_gallery (app.R lines 1324-1696)
// plus the tab header (app.R UI lines 254-263). Championship rosters use
// get_championship_rosters from helpers.R (lines 279-319), inlined below.

import { useState } from "react";
import type { CSSProperties, ReactNode } from "react";
import { getLeagueData, headshotUrl, fmt, BENCH_SLOTS, StarterRow } from "@/lib/data";
import Modal from "@/components/Modal";
import { findPhoto } from "../trophy-room/photos";

const POS_ORDER = ["QB", "RB", "WR", "TE", "K", "DST", "D/ST", "DEF"];

function posIndex(pos: string): number {
  const i = POS_ORDER.indexOf(pos);
  return i === -1 ? POS_ORDER.length : i; // unmatched positions sort last (NA in R)
}

const headshotStyle = (size: number): CSSProperties => ({
  width: size,
  height: size,
  borderRadius: "50%",
  objectFit: "cover",
  filter: "sepia(0.7) saturate(0.5) brightness(0.9) contrast(1.1)",
  border: "2px solid #8b6914",
});

const hideOnError = (e: React.SyntheticEvent<HTMLImageElement>) => {
  e.currentTarget.style.display = "none";
};

const dropdownBoxStyle: CSSProperties = {
  marginTop: 4,
  padding: 4,
  background: "#0a0a1a",
  border: "1px solid #c9a84c",
  borderRadius: 4,
};

const summaryStyle: CSSProperties = {
  color: "#d4a84b",
  fontFamily: "Georgia,serif",
  fontSize: 11,
  textAlign: "center",
  listStyle: "none",
  padding: 4,
};

function UserIcon({ size = 64 }: { size?: number }) {
  // Stand-in for shiny icon("user") (Font Awesome solid user)
  return (
    <svg width={size} height={size} viewBox="0 0 448 512" fill="currentColor" aria-hidden="true">
      <path d="M224 256a128 128 0 1 0 0-256 128 128 0 1 0 0 256zm-45.7 48C79.8 304 0 383.8 0 482.3 0 498.7 13.3 512 29.7 512l388.6 0c16.4 0 29.7-13.3 29.7-29.7 0-98.5-79.8-178.3-178.3-178.3l-91.4 0z" />
    </svg>
  );
}

/** Championship roster table (shared by the dropdown and the tap modal). */
function RosterTable({ roster, totalScore }: { roster: StarterRow[]; totalScore: number }) {
  return (
    <table style={{ width: "100%", borderCollapse: "collapse" }}>
      <tbody>
        {roster.map((p, j) => {
          const hs = headshotUrl(p.player_name);
          return (
            <tr key={j}>
              <td style={{ width: 40, padding: 3 }}>
                {hs != null && (
                  <img
                    src={hs}
                    alt=""
                    loading="lazy"
                    style={headshotStyle(32)}
                    onError={hideOnError}
                  />
                )}
              </td>
              <td
                style={{
                  color: "#d4a84b",
                  fontSize: 11,
                  textAlign: "left",
                  padding: 3,
                }}
              >
                <span style={{ fontWeight: "bold" }}>{p.player_name}</span>
                <br />
                <span style={{ color: "#8b6914", fontSize: 9 }}>
                  {p.pos} - {p.team ?? "NA"}
                </span>
              </td>
              <td
                style={{
                  color: "#d4a84b",
                  fontWeight: "bold",
                  fontSize: 12,
                  textAlign: "right",
                  padding: 3,
                }}
              >
                {Math.round(p.player_score ?? 0)}
              </td>
            </tr>
          );
        })}
        <tr style={{ borderTop: "1px solid #c9a84c" }}>
          <td style={{ padding: 3 }} />
          <td
            style={{
              color: "#d4a84b",
              fontWeight: "bold",
              fontSize: 11,
              textAlign: "left",
              padding: 3,
            }}
          >
            TOTAL
          </td>
          <td
            style={{
              color: "#d4a84b",
              fontWeight: "bold",
              fontSize: 13,
              textAlign: "right",
              padding: 3,
            }}
          >
            {totalScore}
          </td>
        </tr>
      </tbody>
    </table>
  );
}

interface SummaryLine {
  label: string;
  value: string;
}

/**
 * One champion's alcove. Tapping the bust/alcove opens a dark Modal with the
 * championship summary and the title-winning roster (the hover-free,
 * touch-first replacement for tooltips). The <details> dropdowns below the
 * pedestal are kept as-is.
 */
function BustCard({
  owner,
  yr,
  bustFile,
  photoFile,
  lazy,
  summary,
  roster,
  totalScore,
  rosterHtml,
  draftHtml,
}: {
  owner: string;
  yr: number;
  bustFile: string | null;
  photoFile: string | null;
  lazy: boolean;
  summary: SummaryLine[];
  roster: StarterRow[];
  totalScore: number;
  rosterHtml: ReactNode;
  draftHtml: ReactNode;
}) {
  const [open, setOpen] = useState(false);
  const hasBust = bustFile != null;

  return (
    <div
      className="d-inline-block text-center"
      style={{
        // calc keeps two cards + the flex gap fitting per row on phones
        width: "calc(50% - 6px)",
        minWidth: 150,
        maxWidth: 220,
        verticalAlign: "top",
        margin: 0,
      }}
    >
      {/* Tappable alcove + pedestal (cursor + title kept as a desktop bonus) */}
      <div
        role="button"
        tabIndex={0}
        aria-label={`${yr} champion ${owner} — tap for details`}
        title={`${yr} Champion — ${owner} | Tap for details`}
        onClick={() => setOpen(true)}
        onKeyDown={(e) => {
          if (e.key === "Enter" || e.key === " ") {
            e.preventDefault();
            setOpen(true);
          }
        }}
        style={{ cursor: "pointer" }}
      >
        {/* Elegant Canton alcove with warm spotlight */}
        <div
          style={{
            width: "100%",
            maxWidth: 190,
            margin: "0 auto",
            background:
              "radial-gradient(ellipse at 50% 0%, #3a2a1a 0%, #1a1008 40%, #0a0804 100%)",
            borderRadius: "50% 50% 0 0 / 20% 20% 0 0",
            padding: "20px 10px 0",
            boxShadow:
              "inset 0 20px 60px rgba(212,168,75,0.12), inset 0 -10px 20px rgba(0,0,0,0.8)",
            border: "1px solid #2a1f14",
            borderBottom: "none",
          }}
        >
          {/* Spotlight glow behind bust */}
          <div
            style={{
              position: "relative",
              background:
                "radial-gradient(ellipse at 50% 40%, rgba(212,168,75,0.1) 0%, transparent 70%)",
              padding: "10px 0",
            }}
          >
            {hasBust ? (
              <img
                src={bustFile!}
                alt={owner}
                loading={lazy ? "lazy" : undefined}
                style={{
                  width: 160,
                  maxWidth: "100%",
                  margin: "0 auto",
                  display: "block",
                  filter:
                    "drop-shadow(0 5px 20px rgba(212,168,75,0.25)) drop-shadow(0 2px 6px rgba(0,0,0,0.5))",
                }}
              />
            ) : photoFile != null ? (
              <div
                style={{
                  width: 120,
                  height: 150,
                  margin: "0 auto",
                  borderRadius: "50% 50% 45% 45% / 55% 55% 45% 45%",
                  overflow: "hidden",
                  padding: 5,
                  background:
                    "radial-gradient(ellipse at 30% 30%, #d4a84b, #8b6914, #5c4413)",
                  boxShadow: "0 5px 20px rgba(212,168,75,0.2)",
                }}
              >
                <img
                  src={photoFile}
                  alt={owner}
                  loading={lazy ? "lazy" : undefined}
                  style={{
                    width: "100%",
                    height: "100%",
                    objectFit: "cover",
                    objectPosition: "top",
                    borderRadius: "50% 50% 40% 40% / 55% 55% 45% 45%",
                    filter: "sepia(0.8) saturate(0.5) brightness(0.85) contrast(1.1)",
                  }}
                />
              </div>
            ) : (
              <div
                style={{
                  width: 120,
                  height: 150,
                  margin: "0 auto",
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "center",
                }}
              >
                <span style={{ color: "#3d2b1a", fontSize: "4rem" }}>
                  <UserIcon />
                </span>
              </div>
            )}
          </div>
        </div>

        {/* Marble pedestal with plaques */}
        <div
          style={{
            width: "100%",
            maxWidth: 190,
            margin: "0 auto",
            background:
              "linear-gradient(180deg, #2a2018, #1a1510 30%, #0f0c08 70%, #1a1510)",
            border: "1px solid #2a1f14",
            borderTop: "3px solid #8b6914",
            borderRadius: "0 0 6px 6px",
            padding: "10px 8px",
            boxShadow: "0 8px 25px rgba(0,0,0,0.6)",
          }}
        >
          {/* Name plaque - metallic gold with black engraved text */}
          <div
            style={{
              padding: 2,
              marginBottom: 5,
              background: "linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413)",
              borderRadius: 4,
              boxShadow: "0 2px 5px rgba(0,0,0,0.4)",
            }}
          >
            <div
              style={{
                background: "linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828)",
                border: "1px solid #8b6914",
                borderRadius: 3,
                padding: "6px 10px",
                textAlign: "center",
                boxShadow:
                  "inset 0 1px 3px rgba(255,255,255,0.4), inset 0 -1px 3px rgba(0,0,0,0.2)",
              }}
            >
              <div
                style={{
                  color: "#3d2b0a",
                  fontFamily: "Georgia,serif",
                  fontWeight: "bold",
                  fontSize: 15,
                  letterSpacing: 2,
                  textTransform: "uppercase",
                  textShadow: "0 1px 0 rgba(255,255,255,0.3), 0 -1px 0 rgba(0,0,0,0.2)",
                }}
              >
                {owner}
              </div>
            </div>
          </div>
          {/* Year plaque - metallic gold with black engraved text */}
          <div
            style={{
              padding: 2,
              background: "linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413)",
              borderRadius: 4,
              boxShadow: "0 2px 5px rgba(0,0,0,0.4)",
            }}
          >
            <div
              style={{
                background: "linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828)",
                border: "1px solid #8b6914",
                borderRadius: 3,
                padding: "5px 10px",
                textAlign: "center",
                boxShadow:
                  "inset 0 1px 3px rgba(255,255,255,0.4), inset 0 -1px 3px rgba(0,0,0,0.2)",
              }}
            >
              <div
                style={{
                  color: "#3d2b0a",
                  fontFamily: "Georgia,serif",
                  fontWeight: "bold",
                  fontSize: 18,
                  letterSpacing: 3,
                  textShadow: "0 1px 0 rgba(255,255,255,0.3), 0 -1px 0 rgba(0,0,0,0.2)",
                }}
              >
                {yr}
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Championship roster dropdown */}
      {rosterHtml}
      {/* Draft results dropdown */}
      {draftHtml}

      {/* Tap modal: championship summary + title-winning roster */}
      <Modal
        open={open}
        onClose={() => setOpen(false)}
        title={`${yr} Champion — ${owner}`}
        dark
        maxWidth={420}
      >
        <table style={{ width: "100%", borderCollapse: "collapse" }}>
          <tbody>
            {summary.map((l, i) => (
              <tr
                key={i}
                style={{
                  borderBottom: i < summary.length - 1 ? "1px solid #2a2a3a" : "none",
                }}
              >
                <td
                  style={{
                    padding: "6px 8px 6px 0",
                    opacity: 0.65,
                    fontSize: 13,
                    whiteSpace: "nowrap",
                    verticalAlign: "top",
                  }}
                >
                  {l.label}
                </td>
                <td style={{ padding: "6px 0", fontWeight: 600, fontSize: 13, textAlign: "right" }}>
                  {l.value}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
        {roster.length > 0 && (
          <>
            <div
              style={{
                marginTop: 14,
                marginBottom: 6,
                color: "#d4a84b",
                fontFamily: "Georgia,serif",
                fontSize: 12,
                letterSpacing: 1,
                textTransform: "uppercase",
              }}
            >
              Championship Roster
            </div>
            <div style={dropdownBoxStyle}>
              <RosterTable roster={roster} totalScore={totalScore} />
            </div>
          </>
        )}
      </Modal>
    </div>
  );
}

export default function HallOfFamePage() {
  const { standings, schedule, drafts, starters } = getLeagueData();

  // Get champion for each season using league_rank == 1
  const champRows = standings
    .filter((s) => s.league_rank === 1)
    .sort((a, b) => a.season - b.season);

  // Championship week = final week of each season's schedule
  const champWeekBySeason = new Map<number, number>();
  for (const g of schedule) {
    const cur = champWeekBySeason.get(g.season);
    if (cur == null || g.week > cur) champWeekBySeason.set(g.season, g.week);
  }

  const bustCards = champRows.map((row, idx) => {
    const owner = row.owner;
    const yr = row.season;
    const champFid = row.franchise_id;
    const champWeek = champWeekBySeason.get(yr) ?? 0;

    // Check for bust image (bust2 first, then bust, lowercase and capitalized)
    const bustFile = findPhoto(
      `${owner.toLowerCase()}_bust2`,
      `${owner}_bust2`,
      `${owner.toLowerCase()}_bust`,
      `${owner}_bust`
    );
    // Check for regular photo (headshot first, then name)
    const photoFile = findPhoto(`${owner.toLowerCase()}_headshot`, owner.toLowerCase(), owner);

    // -------------------------------------------------------------------
    // Championship roster dropdown (get_championship_rosters, helpers.R)
    // -------------------------------------------------------------------
    let rosterHtml: ReactNode = null;
    const roster: StarterRow[] = starters
      .filter(
        (s) =>
          s.season === yr &&
          s.franchise_id === champFid &&
          s.week === champWeek &&
          !BENCH_SLOTS.has(s.lineup_slot)
      )
      .sort(
        (a, b) =>
          posIndex(a.pos) - posIndex(b.pos) || (b.player_score ?? 0) - (a.player_score ?? 0)
      );

    const totalScore = Math.round(roster.reduce((acc, p) => acc + (p.player_score ?? 0), 0));

    if (roster.length > 0) {
      rosterHtml = (
        <div style={{ marginTop: 6 }}>
          <details style={{ cursor: "pointer" }}>
            <summary style={summaryStyle}>
              <span style={{ borderBottom: "1px solid #c9a84c" }}>View Roster</span>
            </summary>
            <div style={dropdownBoxStyle}>
              <RosterTable roster={roster} totalScore={totalScore} />
            </div>
          </details>
        </div>
      );
    }

    // -------------------------------------------------------------------
    // Draft results dropdown
    // -------------------------------------------------------------------
    let draftHtml: ReactNode = null;
    const champDraft = drafts
      .filter((d) => d.season === yr && d.franchise_id === champFid)
      .sort((a, b) => a.round - b.round || a.pick - b.pick);

    if (champDraft.length > 0) {
      // Compute starter stats per drafted player
      const ownerStarters = starters.filter(
        (s) => s.season === yr && s.franchise_id === champFid && !BENCH_SLOTS.has(s.lineup_slot)
      );

      draftHtml = (
        <div style={{ marginTop: 4 }}>
          <details style={{ cursor: "pointer" }}>
            <summary style={summaryStyle}>
              <span style={{ borderBottom: "1px solid #c9a84c" }}>View Draft</span>
            </summary>
            <div style={dropdownBoxStyle}>
              <table style={{ width: "100%", borderCollapse: "collapse" }}>
                <thead>
                  <tr style={{ borderBottom: "1px solid #c9a84c" }}>
                    <th style={{ color: "#8b6914", fontSize: 10, padding: 2, textAlign: "left" }}>
                      Rd
                    </th>
                    <th style={{ color: "#8b6914", fontSize: 10, padding: 2, textAlign: "left" }} />
                    <th style={{ color: "#8b6914", fontSize: 10, padding: 2, textAlign: "left" }}>
                      Player
                    </th>
                    <th style={{ color: "#8b6914", fontSize: 10, padding: 2, textAlign: "center" }}>
                      Wks
                    </th>
                    <th style={{ color: "#8b6914", fontSize: 10, padding: 2, textAlign: "right" }}>
                      Pts
                    </th>
                    <th style={{ color: "#8b6914", fontSize: 10, padding: 2, textAlign: "right" }}>
                      Chp
                    </th>
                  </tr>
                </thead>
                <tbody>
                  {champDraft.map((d, j) => {
                    const playerStarts = ownerStarters.filter(
                      (s) => s.player_name === d.player_name
                    );
                    const weeksStarted = playerStarts.length;
                    const totalPts = Math.round(
                      playerStarts.reduce((acc, s) => acc + (s.player_score ?? 0), 0)
                    );
                    const champGamePts = Math.round(
                      playerStarts
                        .filter((s) => s.week === champWeek)
                        .reduce((acc, s) => acc + (s.player_score ?? 0), 0)
                    );
                    const hs = headshotUrl(d.player_name);

                    return (
                      <tr key={j}>
                        <td style={{ color: "#8b6914", fontSize: 9, padding: 3, width: 22 }}>
                          R{d.round}
                        </td>
                        <td style={{ width: 36, padding: 3 }}>
                          {hs != null && (
                            <img
                              src={hs}
                              alt=""
                              loading="lazy"
                              style={headshotStyle(28)}
                              onError={hideOnError}
                            />
                          )}
                        </td>
                        <td
                          style={{
                            color: "#d4a84b",
                            fontSize: 10,
                            textAlign: "left",
                            padding: 3,
                          }}
                        >
                          <span style={{ fontWeight: "bold" }}>{d.player_name}</span>
                          <br />
                          <span style={{ color: "#8b6914", fontSize: 8 }}>
                            {d.pos} - {d.team ?? "NA"}
                          </span>
                        </td>
                        <td
                          style={{
                            color: "#d4a84b",
                            fontSize: 10,
                            textAlign: "center",
                            padding: 3,
                          }}
                        >
                          {weeksStarted}
                        </td>
                        <td
                          style={{ color: "#d4a84b", fontSize: 10, textAlign: "right", padding: 3 }}
                        >
                          {totalPts}
                        </td>
                        <td
                          style={{ color: "#d4a84b", fontSize: 10, textAlign: "right", padding: 3 }}
                        >
                          {champGamePts > 0 ? champGamePts : "-"}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </details>
        </div>
      );
    }

    // -------------------------------------------------------------------
    // Championship summary for the tap modal
    // -------------------------------------------------------------------
    const seasonRows = standings.filter((s) => s.season === yr);
    const pfRankSeason =
      [...seasonRows]
        .sort((a, b) => b.points_for - a.points_for)
        .findIndex((s) => s.franchise_id === champFid) + 1;
    const finalGame = schedule.find(
      (g) => g.season === yr && g.franchise_id === champFid && g.week === champWeek
    );
    const summary: SummaryLine[] = [
      { label: "Season", value: String(yr) },
      { label: "Owner", value: owner },
      { label: "Record", value: `${row.h2h_wins}-${row.h2h_losses}` },
      { label: "Points For", value: fmt(row.points_for) },
      { label: "League Rank (PF)", value: `#${pfRankSeason} of ${seasonRows.length}` },
    ];
    if (finalGame != null) {
      summary.push({
        label: `Title Game (Week ${finalGame.week})`,
        value: `${fmt(finalGame.franchise_score)}–${fmt(finalGame.opponent_score)} vs ${finalGame.opponent_owner}`,
      });
    }

    return (
      <BustCard
        key={yr}
        owner={owner}
        yr={yr}
        bustFile={bustFile}
        photoFile={photoFile}
        lazy={idx >= 2}
        summary={summary}
        roster={roster}
        totalScore={totalScore}
        rosterHtml={rosterHtml}
        draftHtml={draftHtml}
      />
    );
  });

  return (
    <>
      <div className="text-center" style={{ margin: "4px 0 10px" }}>
        <h2
          style={{
            color: "#8B6914",
            fontFamily: "Georgia,serif",
            letterSpacing: 2,
            margin: "0 0 4px",
            fontSize: "clamp(20px, 5.5vw, 28px)",
          }}
        >
          GFFL HALL OF FAME
        </h2>
        <hr style={{ borderColor: "#8B6914", width: 200, margin: "0 auto 4px" }} />
        <p
          style={{
            color: "#8B6914",
            fontStyle: "italic",
            fontSize: 13,
            margin: 0,
          }}
        >
          Tap a champion for season details
        </p>
      </div>
      <div
        style={{
          padding: "clamp(10px, 3vw, 20px)",
          background: "linear-gradient(180deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%)",
          borderRadius: 12,
          display: "flex",
          flexWrap: "wrap",
          justifyContent: "center",
          alignItems: "flex-start",
          gap: 12,
        }}
      >
        {bustCards}
      </div>
    </>
  );
}
