// Trophy Room — port of app.R output$owners_grid (app.R lines 489-1322).

import type { CSSProperties, ReactNode } from "react";
import { getLeagueData, photoUrl, fmt, ScheduleRow, StandingRow } from "@/lib/data";
import { computeAlltimeStandings } from "@/lib/league";
import { findPhoto } from "./photos";
import styles from "./styles.module.css";

export const metadata = { title: "Trophy Room — GFFL Archives" };

// ---------------------------------------------------------------------------
// Plaque styling (gold / silver / bronze by rank)
// ---------------------------------------------------------------------------

interface PlaqueStyleDef {
  bg: string;
  border: string;
  text: string;
  shadow: string;
}

const GOLD: PlaqueStyleDef = {
  bg: "linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828)",
  border: "#8b6914",
  text: "#3d2b0a",
  shadow: "rgba(212,168,75,0.3)",
};
const SILVER: PlaqueStyleDef = {
  bg: "linear-gradient(180deg, #a8a8a8, #d8d8d8, #b0b0b0, #888888)",
  border: "#666",
  text: "#2a2a2a",
  shadow: "rgba(150,150,150,0.3)",
};
const BRONZE: PlaqueStyleDef = {
  bg: "linear-gradient(180deg, #a0714a, #cd8c5c, #b07848, #7a5430)",
  border: "#5c3a20",
  text: "#2a1a0a",
  shadow: "rgba(160,113,74,0.3)",
};

function getPlaqueStyle(rank: number | undefined): PlaqueStyleDef {
  const r = rank ?? 99;
  if (r <= 3) return GOLD;
  if (r <= 6) return SILVER;
  return BRONZE;
}

// Name plaque style (always gold, matches photo width)
const NAME_STYLE = GOLD;

function Plaque({
  label,
  value,
  style,
  wide = false,
}: {
  label: string | null;
  value: string;
  style: PlaqueStyleDef;
  wide?: boolean;
}) {
  return (
    <div
      className={wide ? undefined : styles.plaqueStat}
      style={{
        padding: 2,
        margin: 1,
        minWidth: 0,
        ...(wide ? { width: "100%", display: "inline-block" } : { display: "inline-block" }),
        background: "linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413)",
        borderRadius: 4,
        boxShadow: "0 2px 5px rgba(0,0,0,0.4)",
      }}
    >
      <div
        style={{
          background: style.bg,
          border: `1px solid ${style.border}`,
          borderRadius: 3,
          padding: "4px 6px",
          textAlign: "center",
          minWidth: 0,
          boxShadow: `inset 0 1px 3px rgba(255,255,255,0.4), inset 0 -1px 3px rgba(0,0,0,0.2), 0 1px 0 ${style.shadow}`,
        }}
      >
        {label != null && (
          <div
            className={styles.plaqueLabel}
            style={{
              fontFamily: "Georgia,serif",
              textTransform: "uppercase",
              letterSpacing: "0.5px",
              color: style.text,
              opacity: 0.75,
            }}
          >
            {label}
          </div>
        )}
        <div
          className={styles.plaqueValue}
          style={{
            fontFamily: "Georgia,serif",
            fontWeight: "bold",
            color: style.text,
            whiteSpace: "nowrap",
            textShadow: "0 1px 0 rgba(255,255,255,0.3), 0 -1px 0 rgba(0,0,0,0.2)",
          }}
        >
          {value}
        </div>
      </div>
    </div>
  );
}

// ---------------------------------------------------------------------------
// Shared card chrome
// ---------------------------------------------------------------------------

const cardOuterStyle: CSSProperties = {
  background: "linear-gradient(180deg, #1a1210 0%, #2a1f18 30%, #1a1210 100%)",
  border: "3px solid #3d2b1a",
  borderRadius: 6,
  boxShadow: "inset 0 0 30px rgba(0,0,0,0.5), 0 4px 15px rgba(0,0,0,0.5)",
  overflow: "hidden",
  position: "relative",
};

const sheenStyle: CSSProperties = {
  position: "absolute",
  top: 0,
  left: 0,
  right: 0,
  bottom: 0,
  background:
    "linear-gradient(135deg, rgba(255,255,255,0.06) 0%, transparent 50%, rgba(255,255,255,0.03) 100%)",
  pointerEvents: "none",
  zIndex: 1,
};

const shelfBorderBg: CSSProperties = {
  borderBottom: "3px solid rgba(255,255,255,0.15)",
  background: "linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%)",
};

const photoShelfStyle: CSSProperties = { ...shelfBorderBg, padding: "10px 6px 8px" };

const trophyShelfStyle: CSSProperties = {
  ...shelfBorderBg,
  display: "flex",
  alignItems: "flex-end",
  padding: "4px 2px 6px",
  overflow: "hidden",
  position: "relative",
};

const mvpShelfStyle: CSSProperties = {
  ...shelfBorderBg,
  display: "flex",
  alignItems: "flex-end",
  padding: "4px 4px 6px",
  position: "relative",
};

const bannerShelfStyle: CSSProperties = {
  ...shelfBorderBg,
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  flexWrap: "wrap",
  padding: "4px 4px",
  position: "relative",
};

const sackoShelfStyle: CSSProperties = {
  ...shelfBorderBg,
  display: "flex",
  alignItems: "flex-end",
  justifyContent: "center",
  flexWrap: "wrap",
  padding: "4px 4px 6px",
  position: "relative",
};

const plaqueShelfStyle: CSSProperties = {
  background: "linear-gradient(180deg, #1a1210 0%, #2a1f18 50%, #1a1210 100%)",
  display: "flex",
  alignItems: "center",
  justifyContent: "space-around",
  flexWrap: "nowrap",
  padding: "6px 2px",
  borderTop: "2px solid #8b6914",
  overflow: "hidden",
};

const halfShelfStyle: CSSProperties = {
  flex: 1,
  display: "flex",
  alignItems: "flex-end",
  justifyContent: "center",
  flexWrap: "nowrap",
  overflow: "hidden",
  position: "relative",
  zIndex: 2,
};

const halfShelfWrapStyle: CSSProperties = {
  flex: 1,
  display: "flex",
  alignItems: "flex-end",
  justifyContent: "center",
  flexWrap: "wrap",
  position: "relative",
  zIndex: 2,
};

function UserIcon({ size = 48 }: { size?: number }) {
  // Stand-in for shiny icon("user") (Font Awesome solid user)
  return (
    <svg width={size} height={size} viewBox="0 0 448 512" fill="currentColor" aria-hidden="true">
      <path d="M224 256a128 128 0 1 0 0-256 128 128 0 1 0 0 256zm-45.7 48C79.8 304 0 383.8 0 482.3 0 498.7 13.3 512 29.7 512l388.6 0c16.4 0 29.7-13.3 29.7-29.7 0-98.5-79.8-178.3-178.3-178.3l-91.4 0z" />
    </svg>
  );
}

function PhotoBlock({ o, photoFile }: { o: string; photoFile: string | null }) {
  if (photoFile == null) {
    return (
      <div
        className={styles.ownerPhotoFrame}
        style={{
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          flexShrink: 0,
          background: "#2a1f18",
        }}
      >
        <span style={{ color: "#555", fontSize: "3rem" }}>
          <UserIcon />
        </span>
      </div>
    );
  }
  return (
    <div className={styles.ownerPhotoFrame} style={{ position: "relative", flexShrink: 0 }}>
      <div
        style={{
          position: "absolute",
          top: "10%",
          left: "10%",
          width: "80%",
          height: "80%",
          overflow: "hidden",
        }}
      >
        <img
          src={photoFile}
          alt={o}
          style={{
            width: "100%",
            height: "100%",
            objectFit: "cover",
            objectPosition: o === "Joe" ? "50% 20%" : "top",
          }}
        />
      </div>
      <img
        src={photoUrl("photos/frame.PNG")}
        alt=""
        style={{
          position: "absolute",
          top: 0,
          left: 0,
          width: "100%",
          height: "100%",
          pointerEvents: "none",
        }}
      />
    </div>
  );
}

function JerseyBlock({ jerseyFile }: { jerseyFile: string | null }) {
  return (
    <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center" }}>
      {jerseyFile != null ? (
        <img src={jerseyFile} alt="" className={styles.jerseyImg} />
      ) : (
        <div style={{ color: "#555", fontSize: 11, textAlign: "center", fontStyle: "italic" }}>
          Jersey
          <br />
          coming soon
        </div>
      )}
    </div>
  );
}

// ---------------------------------------------------------------------------
// Stats computation
// ---------------------------------------------------------------------------

function maxStreak(games: ScheduleRow[], resultVal: "W" | "L"): number {
  let cur = 0;
  let max = 0;
  for (const g of games) {
    if (g.result === resultVal) {
      cur += 1;
      if (cur > max) max = cur;
    } else {
      cur = 0;
    }
  }
  return max;
}

function rankMap(names: string[]): Map<string, number> {
  const m = new Map<string, number>();
  names.forEach((n, i) => m.set(n, i + 1));
  return m;
}

export default function TrophyRoomPage() {
  const { standings, schedule } = getLeagueData();

  // Compute stats per owner
  const alltime = computeAlltimeStandings(standings);

  // Rank owners by win% and total PF for plaque colors
  const winpctRank = rankMap([...alltime].sort((a, b) => b.winPct - a.winPct).map((r) => r.team));
  const pfRank = rankMap([...alltime].sort((a, b) => b.pf - a.pf).map((r) => r.team));

  const owners = [...new Set(standings.map((s) => s.owner))].sort();
  const byOwnerStandings = new Map<string, StandingRow[]>(
    owners.map((o) => [o, standings.filter((s) => s.owner === o)])
  );

  // Per-owner best/worst stats for ranking
  const bestRecRank = rankMap(
    owners
      .map((o) => ({ o, v: Math.max(...byOwnerStandings.get(o)!.map((r) => r.h2h_wins)) }))
      .sort((a, b) => b.v - a.v)
      .map((x) => x.o)
  );
  const bestPfRank = rankMap(
    owners
      .map((o) => ({ o, v: Math.max(...byOwnerStandings.get(o)!.map((r) => r.points_for)) }))
      .sort((a, b) => b.v - a.v)
      .map((x) => x.o)
  );

  const schedOwners = [...new Set(schedule.map((g) => g.team_owner))].sort();
  const byOwnerSchedule = new Map<string, ScheduleRow[]>(
    schedOwners.map((o) => [
      o,
      schedule
        .filter((g) => g.team_owner === o)
        .sort((a, b) => a.season - b.season || a.week - b.week),
    ])
  );
  const bestWkRank = rankMap(
    schedOwners
      .map((o) => ({ o, v: Math.max(...byOwnerSchedule.get(o)!.map((g) => g.franchise_score)) }))
      .sort((a, b) => b.v - a.v)
      .map((x) => x.o)
  );
  const wstreakRank = rankMap(
    schedOwners
      .map((o) => ({ o, v: maxStreak(byOwnerSchedule.get(o)!, "W") }))
      .sort((a, b) => b.v - a.v)
      .map((x) => x.o)
  );

  // Championships: ESPN league_rank == 1 is the playoff champion
  const champCount = new Map<string, number>();
  for (const s of standings) {
    if (s.league_rank === 1) champCount.set(s.owner, (champCount.get(s.owner) ?? 0) + 1);
  }
  // Championship appearances: league_rank 1 or 2 (made the title game)
  const appearCount = new Map<string, number>();
  for (const s of standings) {
    if (s.league_rank <= 2) appearCount.set(s.owner, (appearCount.get(s.owner) ?? 0) + 1);
  }

  const seasons = [...new Set(standings.map((s) => s.season))].sort((a, b) => a - b);

  // Sackos: worst regular season record (fewest wins, then lowest PF as tiebreaker)
  const sackoWinners = seasons.map((season) => {
    const rows = standings
      .filter((s) => s.season === season)
      .sort((a, b) => a.h2h_wins - b.h2h_wins || a.points_for - b.points_for);
    return { season, owner: rows[0].owner };
  });
  const sackoCount = new Map<string, number>();
  for (const s of sackoWinners) sackoCount.set(s.owner, (sackoCount.get(s.owner) ?? 0) + 1);

  // Playoff appearances: league_rank <= 4 (top 4 make playoffs)
  const playoffRows = standings
    .filter((s) => s.league_rank <= 4)
    .map((s) => ({ season: s.season, owner: s.owner }));
  const playoffCount = new Map<string, number>();
  for (const p of playoffRows) playoffCount.set(p.owner, (playoffCount.get(p.owner) ?? 0) + 1);

  // #1 seed: best regular season record (wins desc, PF tiebreaker)
  const oneseedRows = seasons.map((season) => {
    const rows = standings
      .filter((s) => s.season === season)
      .sort((a, b) => b.h2h_wins - a.h2h_wins || b.points_for - a.points_for);
    return { season, owner: rows[0].owner };
  });

  // GFFL trophy: most regular season points for
  const gfflPfRows = seasons.map((season) => {
    const rows = standings
      .filter((s) => s.season === season)
      .sort((a, b) => b.points_for - a.points_for);
    return { season, owner: rows[0].owner };
  });

  // Legacy owners (no longer in the league)
  const legacyOwners = ["Joe"];

  // Build owner stats for sorting (championships desc, appearances desc, playoff apps desc)
  const ownerStats = owners
    .map((o) => ({
      owner: o,
      championships: champCount.get(o) ?? 0,
      appearances: appearCount.get(o) ?? 0,
      playoffApps: playoffCount.get(o) ?? 0,
      sackos: sackoCount.get(o) ?? 0,
    }))
    .sort(
      (a, b) =>
        b.championships - a.championships ||
        b.appearances - a.appearances ||
        b.playoffApps - a.playoffApps
    );

  const activeSorted = ownerStats.filter((s) => !legacyOwners.includes(s.owner)).map((s) => s.owner);
  const legacySorted = ownerStats.filter((s) => legacyOwners.includes(s.owner)).map((s) => s.owner);

  // Function to build a trophy case for one owner
  function buildOwnerCard(o: string): ReactNode {
    const stats = alltime.find((r) => r.team === o);
    const ownerSeasons = byOwnerStandings.get(o) ?? [];
    const ownerWeeks = byOwnerSchedule.get(o) ?? [];

    const playoffYears = playoffRows.filter((p) => p.owner === o).map((p) => p.season).sort();
    const oneseedYears = oneseedRows.filter((p) => p.owner === o).map((p) => p.season).sort();
    const gfflPfYears = gfflPfRows.filter((p) => p.owner === o).map((p) => p.season).sort();

    const record = stats ? `${stats.wins}-${stats.losses}` : "0-0";
    const pf = stats ? fmt(Math.round(stats.pf), 0) : "0";

    // Per-owner season stats
    const bestSeason = [...ownerSeasons].sort(
      (a, b) => b.h2h_wins - a.h2h_wins || b.points_for - a.points_for
    )[0];
    const worstSeason = [...ownerSeasons].sort(
      (a, b) => a.h2h_wins - b.h2h_wins || a.points_for - b.points_for
    )[0];
    const mostPfSeason = [...ownerSeasons].sort((a, b) => b.points_for - a.points_for)[0];

    const bestRecordStr = bestSeason ? `${bestSeason.h2h_wins}-${bestSeason.h2h_losses}` : "N/A";
    void worstSeason; // worst record is computed in app.R but never displayed
    const mostPfStr = mostPfSeason ? fmt(Math.round(mostPfSeason.points_for), 0) : "N/A";

    // Weekly best
    const bestWeek = [...ownerWeeks].sort((a, b) => b.franchise_score - a.franchise_score)[0];
    const mostPwStr = bestWeek ? String(Math.round(bestWeek.franchise_score)) : "N/A";

    // Streaks
    const winStreakStr = String(maxStreak(ownerWeeks, "W"));

    // Plaque colors based on ranking
    const wpStyle = getPlaqueStyle(winpctRank.get(o));
    const pfStyle = getPlaqueStyle(pfRank.get(o));

    // Helper to get season info for tooltip
    const seasonTooltip = (yr: number, prefix: string): string => {
      const s = ownerSeasons.find((r) => r.season === yr);
      if (!s) return "";
      return `${prefix}${yr} | ${s.h2h_wins}-${s.h2h_losses} | #${s.league_rank}`;
    };

    // Championship trophies with tooltips
    let champYears = ownerSeasons.filter((s) => s.league_rank === 1).map((s) => s.season);
    // Add pre-data championships
    if (o === "Connor") champYears = [...new Set([2016, ...champYears])];
    champYears.sort((a, b) => a - b);

    // Hunt trophies (title game appearances) with tooltips
    const appearYears = ownerSeasons
      .filter((s) => s.league_rank <= 2)
      .map((s) => s.season)
      .sort((a, b) => a - b);

    // Sacko trophies with tooltips
    let sackoYears = sackoWinners.filter((s) => s.owner === o).map((s) => s.season);
    // Add pre-data sackos
    if (o === "Kerley") sackoYears = [...new Set([2016, ...sackoYears])];
    sackoYears.sort((a, b) => a - b);

    // Combined banners with tooltips
    const allBannerYears = [...new Set([...playoffYears, ...oneseedYears])].sort((a, b) => a - b);

    // Jersey image lookup
    const jerseyFile = findPhoto(`${o.toLowerCase()}_jersey`, `${o}_jersey`);
    const photoFile = findPhoto(`${o.toLowerCase()}_headshot`, o.toLowerCase(), o);

    return (
      <div key={o} style={cardOuterStyle}>
        <div style={sheenStyle} />

        {/* Top shelf: photo+nameplate (left) | jersey (right) */}
        <div className={styles.photoShelfRow} style={photoShelfStyle}>
          {/* Left: photo + name plaque */}
          <div
            style={{
              flex: 1,
              display: "flex",
              flexDirection: "column",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            <PhotoBlock o={o} photoFile={photoFile} />
            <div style={{ width: "100%", maxWidth: 140, marginTop: 4 }}>
              <Plaque label={null} value={o} style={NAME_STYLE} wide />
            </div>
          </div>
          {/* Right: jersey */}
          <JerseyBlock jerseyFile={jerseyFile} />
        </div>

        {/* Lombardi / Hunt split shelf */}
        <div className={styles.trophyShelf} style={trophyShelfStyle}>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelLeft}`}>CHAMPIONSHIPS</div>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelRight}`}>APPEARANCES</div>
          <div style={halfShelfStyle}>
            {champYears.map((yr) => (
              <img
                key={yr}
                src={photoUrl("photos/champion_ring.png")}
                alt=""
                className={`${styles.trophyImg} ${styles.lombardiImg}`}
                title={seasonTooltip(yr, "Champion ")}
              />
            ))}
          </div>
          <div
            style={{
              width: 1,
              background: "rgba(255,255,255,0.12)",
              alignSelf: "stretch",
              margin: "2px 1px",
              flexShrink: 0,
              zIndex: 2,
            }}
          />
          <div style={halfShelfStyle}>
            {appearYears.map((yr) => (
              <img
                key={yr}
                src={photoUrl("photos/Hunt.png")}
                alt=""
                className={`${styles.trophyImg} ${styles.huntImg}`}
                title={seasonTooltip(yr, "Title Game ")}
              />
            ))}
          </div>
        </div>

        {/* MVP (left) | GFFL (right) split shelf */}
        <div className={styles.trophyShelf} style={mvpShelfStyle}>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelLeft}`}>BEST RECORD</div>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelRight}`}>MOST POINTS</div>
          <div style={halfShelfWrapStyle}>
            {oneseedYears.map((yr) => (
              <img
                key={yr}
                src={photoUrl("photos/mvp.png")}
                alt=""
                className={`${styles.trophyImg} ${styles.mvpImg}`}
                title={seasonTooltip(yr, "MVP ")}
              />
            ))}
          </div>
          <div
            style={{
              width: 2,
              background: "rgba(255,255,255,0.12)",
              alignSelf: "stretch",
              margin: "4px 2px",
              zIndex: 2,
            }}
          />
          <div style={halfShelfWrapStyle}>
            {gfflPfYears.map((yr) => (
              <img
                key={yr}
                src={photoUrl("photos/GFFL.png")}
                alt=""
                className={`${styles.trophyImg} ${styles.gfflImg}`}
                title={seasonTooltip(yr, "Most PF ")}
              />
            ))}
          </div>
        </div>

        {/* Playoff banners shelf (full width, no divider) */}
        <div className={styles.bannerShelf} style={bannerShelfStyle}>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelFull}`}>PLAYOFF APPEARANCES</div>
          <div
            style={{
              position: "relative",
              zIndex: 2,
              display: "flex",
              flexWrap: "wrap",
              justifyContent: "center",
            }}
          >
            {allBannerYears.map((yr) => {
              const isOneseed = oneseedYears.includes(yr);
              const src = findPhoto(`${isOneseed ? "oneseed" : "playoffs"}_${yr}`);
              if (!src) return null;
              return (
                <img
                  key={yr}
                  src={src}
                  alt=""
                  className={`${styles.trophyImg} ${styles.bannerImg}`}
                  title={seasonTooltip(yr, isOneseed ? "#1 Seed " : "Playoffs ")}
                />
              );
            })}
          </div>
        </div>

        {/* Sackos shelf (full width) */}
        <div className={styles.sackoShelf} style={sackoShelfStyle}>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelFull}`}>SACKO LAPOS</div>
          <div
            style={{
              position: "relative",
              zIndex: 2,
              display: "flex",
              flexWrap: "wrap",
              justifyContent: "center",
              alignItems: "flex-end",
            }}
          >
            {sackoYears.map((yr) => (
              <img
                key={yr}
                src={photoUrl("photos/sacko.png")}
                alt=""
                className={`${styles.trophyImg} ${styles.sackoImg}`}
                title={seasonTooltip(yr, "Sacko ")}
              />
            ))}
          </div>
        </div>

        {/* Plaques shelf (bottom, full width - always 6 in one row) */}
        <div className={styles.plaqueShelf} style={plaqueShelfStyle}>
          <Plaque label="Record" value={record} style={wpStyle} />
          <Plaque label="Points" value={pf} style={pfStyle} />
          <Plaque label="Best" value={bestRecordStr} style={getPlaqueStyle(bestRecRank.get(o))} />
          <Plaque label="Pts (S)" value={mostPfStr} style={getPlaqueStyle(bestPfRank.get(o))} />
          <Plaque label="Pts (W)" value={mostPwStr} style={getPlaqueStyle(bestWkRank.get(o))} />
          <Plaque label="Streak" value={winStreakStr} style={getPlaqueStyle(wstreakRank.get(o))} />
        </div>
      </div>
    );
  }

  // Ghost card - for owners without ESPN data (Sean, Kenny)
  // fillAll = true fills every shelf with fillImage (for Honorary owners)
  function buildGhostCard(o: string, fillAll = false, fillImage: string | null = null): ReactNode {
    const oClean = o.toLowerCase().replace(/ /g, "");
    const photoFile = findPhoto(
      `${oClean}_headshot`,
      `${o.toLowerCase()}_headshot`,
      oClean,
      o.toLowerCase(),
      o
    );
    const jerseyFile = findPhoto(`${oClean}_jersey`, `${o.toLowerCase()}_jersey`, `${o}_jersey`);

    // Fill images for Honorary cards
    const fillImgs =
      fillAll && fillImage != null
        ? [0, 1, 2].map((i) => (
            <img
              key={i}
              src={fillImage}
              alt=""
              className={`${styles.trophyImg} ${styles.lombardiImg}`}
            />
          ))
        : null;

    const questionPlaque = (label: string) => (
      <Plaque label={label} value="???" style={BRONZE} />
    );

    return (
      <div key={o} style={cardOuterStyle}>
        <div style={sheenStyle} />

        {/* Top shelf: photo + jersey */}
        <div className={styles.photoShelfRow} style={photoShelfStyle}>
          <div
            style={{
              flex: 1,
              display: "flex",
              flexDirection: "column",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            <PhotoBlock o={o} photoFile={photoFile} />
            <div style={{ width: "100%", maxWidth: 140, marginTop: 4 }}>
              <Plaque label={null} value={o} style={NAME_STYLE} wide />
            </div>
          </div>
          <JerseyBlock jerseyFile={jerseyFile} />
        </div>

        {/* Lombardi / Hunt split shelf */}
        <div className={styles.trophyShelf} style={trophyShelfStyle}>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelLeft}`}>CHAMPIONSHIPS</div>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelRight}`}>APPEARANCES</div>
          <div style={halfShelfStyle}>{fillImgs}</div>
          <div
            style={{
              width: 1,
              background: "rgba(255,255,255,0.12)",
              alignSelf: "stretch",
              margin: "2px 1px",
              flexShrink: 0,
              zIndex: 2,
            }}
          />
          <div style={halfShelfStyle}>{fillImgs}</div>
        </div>

        {/* MVP / GFFL split shelf */}
        <div className={styles.trophyShelf} style={mvpShelfStyle}>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelLeft}`}>BEST RECORD</div>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelRight}`}>MOST POINTS</div>
          <div style={halfShelfWrapStyle}>{fillImgs}</div>
          <div
            style={{
              width: 2,
              background: "rgba(255,255,255,0.12)",
              alignSelf: "stretch",
              margin: "4px 2px",
              zIndex: 2,
            }}
          />
          <div style={halfShelfWrapStyle}>{fillImgs}</div>
        </div>

        {/* Playoff banners shelf */}
        <div className={styles.bannerShelf} style={bannerShelfStyle}>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelFull}`}>PLAYOFF APPEARANCES</div>
          <div style={{ position: "relative", zIndex: 2 }}>{fillImgs}</div>
        </div>

        {/* Sacko shelf */}
        <div className={styles.sackoShelf} style={sackoShelfStyle}>
          <div className={`${styles.shelfLabel} ${styles.shelfLabelFull}`}>SACKO LAPOS</div>
          <div style={{ position: "relative", zIndex: 2 }}>{fillImgs}</div>
        </div>

        {/* Plaques - all ??? */}
        <div className={styles.plaqueShelf} style={plaqueShelfStyle}>
          {questionPlaque("Record")}
          {questionPlaque("Points")}
          {questionPlaque("Best")}
          {questionPlaque("Pts (S)")}
          {questionPlaque("Pts (W)")}
          {questionPlaque("Streak")}
        </div>
      </div>
    );
  }

  // Build active owner cards (sorted by championships desc, sackos asc)
  const activeCards = activeSorted.map((o) => buildOwnerCard(o));

  // Build legacy owner cards (existing + ghost cards for Sean/Kenny)
  const legacyCards = [
    ...legacySorted.map((o) => buildOwnerCard(o)),
    buildGhostCard("Sean"),
    buildGhostCard("Kenny"),
  ];

  return (
    <div style={{ background: "#e8e0d4", padding: 20, borderRadius: 12 }}>
      <div className={styles.trophyGrid}>{activeCards}</div>
      {legacyCards.length > 0 && (
        <>
          <hr style={{ borderColor: "#aaa" }} />
          <h4 style={{ color: "#666", textAlign: "center", marginTop: 16, marginBottom: 12 }}>
            <svg
              width="20"
              height="20"
              viewBox="0 0 512 512"
              fill="currentColor"
              aria-hidden="true"
              style={{ verticalAlign: "-2px" }}
            >
              {/* Font Awesome clock-rotate-left */}
              <path d="M75 75L41 41C25.9 25.9 0 36.6 0 57.9L0 168c0 13.3 10.7 24 24 24l110.1 0c21.4 0 32.1-25.9 17-41l-30.8-30.8C155 85.5 203 64 256 64c106 0 192 86 192 192s-86 192-192 192c-40.8 0-78.6-12.7-109.7-34.4c-14.5-10.1-34.4-6.6-44.6 7.9s-6.6 34.4 7.9 44.6C151.2 495 201.7 512 256 512c141.4 0 256-114.6 256-256S397.4 0 256 0C185.3 0 121.3 28.7 75 75zm181 53c-13.3 0-24 10.7-24 24l0 104c0 6.4 2.5 12.5 7 17l72 72c9.4 9.4 24.6 9.4 33.9 0s9.4-24.6 0-33.9l-65-65 0-94.1c0-13.3-10.7-24-24-24z" />
            </svg>{" "}
            Legacy Owners
          </h4>
          <div className={styles.trophyGrid}>{legacyCards}</div>
        </>
      )}
    </div>
  );
}
