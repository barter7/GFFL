// "Player Records" tab — old-record-book style. Port of app.R
// output$player_records_book (lines 4162-4564) + UI nav_panel "Player Records"
// (lines 279-289).
import type { Metadata } from "next";
import { getLeagueData, headshotUrl } from "@/lib/data";
import { FLEURON, FLEURON_TRIPLE, RECORD_BOOK_CSS } from "../records/recordBookCss";
import { computePlayerRecords, Top5Row } from "./computePlayerRecords";
import Headshot from "./Headshot";
import Toc, { TocItem } from "./Toc";
import BackToTop from "../records/BackToTop";

export const metadata: Metadata = { title: "Player Records" };

/** Anchor id for a section title (shared by the headings and the TOC). */
function slug(title: string): string {
  return title
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

// Port of build_top5() (app.R lines 4197-4224)
function Top5Section({ title, rows }: { title: string; rows: Top5Row[] }) {
  return (
    <>
      {/* scrollMarginTop keeps anchored headings clear of the sticky navbar */}
      <div
        id={slug(title)}
        style={{
          margin: "clamp(14px, 4vw, 25px) clamp(6px, 2.5vw, 20px) 8px",
          scrollMarginTop: 72,
        }}
      >
        <div
          style={{
            fontFamily: "'IM Fell English',Georgia,serif",
            fontSize: "clamp(15px, 4.2vw, 20px)",
            color: "#3d2a10",
            fontWeight: "bold",
            letterSpacing: 1.5,
            textTransform: "uppercase",
            borderBottom: "2px solid #5c3a10",
            paddingBottom: 4,
          }}
        >
          {title}
        </div>
      </div>
      {/* Horizontal-scroll guard for narrow phones: invisible when the table
          fits; otherwise the table scrolls inside instead of the whole page. */}
      <div
        style={{
          margin: "0 clamp(6px, 2.5vw, 20px)",
          overflowX: "auto",
          WebkitOverflowScrolling: "touch",
        }}
      >
        <table
          style={{
            width: "100%",
            borderCollapse: "collapse",
          }}
        >
        <tbody>
          {rows.slice(0, 20).map((r, i) => {
            const hsUrl = headshotUrl(r.player_name);
            return (
              <tr key={i}>
                <td
                  style={{
                    color: "#8b6914",
                    fontSize: "clamp(12px, 3.2vw, 14px)",
                    padding: "3px 4px",
                    width: 28,
                    verticalAlign: "middle",
                  }}
                >
                  #{i + 1}
                </td>
                <td style={{ width: 36, padding: 3 }}>
                  {hsUrl && (
                    <Headshot
                      src={hsUrl}
                      style={{
                        width: 30,
                        height: 30,
                        borderRadius: "50%",
                        objectFit: "cover",
                        border: "2px solid #8b6914",
                      }}
                    />
                  )}
                </td>
                <td
                  style={{
                    color: "#3d2a10",
                    fontFamily: "'Cormorant Garamond',Georgia,serif",
                    fontSize: "clamp(13px, 3.6vw, 16px)",
                    fontWeight: 600,
                    padding: "3px 4px",
                    verticalAlign: "middle",
                  }}
                >
                  {r.player_name}
                  <span style={{ color: "#8b6914", fontSize: 12, marginLeft: 4 }}>
                    {r.pos} - {r.team}
                  </span>
                </td>
                <td
                  style={{
                    color: "#3d2a10",
                    fontFamily: "'Cormorant Garamond',Georgia,serif",
                    fontSize: "clamp(14px, 4vw, 18px)",
                    fontWeight: "bold",
                    textAlign: "right",
                    padding: "3px 4px",
                    verticalAlign: "middle",
                    whiteSpace: "nowrap",
                  }}
                >
                  {r.value}
                </td>
                {r.extra !== undefined && (
                  <td
                    style={{
                      color: "#5c3a10",
                      fontSize: 12,
                      fontStyle: "italic",
                      textAlign: "right",
                      padding: "3px 4px",
                      verticalAlign: "middle",
                    }}
                  >
                    {r.extra}
                  </td>
                )}
              </tr>
            );
          })}
          </tbody>
        </table>
      </div>
      <hr
        style={{
          border: "none",
          borderTop: "1px dashed rgba(92,58,16,0.25)",
          margin: "8px clamp(6px, 2.5vw, 20px)",
        }}
      />
    </>
  );
}

const Fleuron = () => <div className="fleuron">{FLEURON}</div>;

export default function PlayerRecordsPage() {
  const { finalSeasons } = getLeagueData();
  const rec = computePlayerRecords(finalSeasons);

  // Section titles in render order (must mirror the <Top5Section> calls
  // below); drives the table of contents.
  const sectionTitles: string[] = [
    "Highest Single-Game Score",
    "Most Games Scoring 50+ Points",
    "Most Games Scoring 40+ Points",
    "Most Games Scoring 30+ Points",
    "Most Total Fantasy Points (Career)",
    "Highest Avg Points Per Start (min 10)",
    "Most Starts Across All Seasons",
    "Highest Score Left on Bench",
    "Most Weeks Spent on the Bench",
    "Most Goose Eggs (0 pts as starter)",
    "Most Loyal: Seasons with Same Owner",
    "Most Nomadic: Different Owners",
    "Most NFL Teams Played For",
    "Highest Scoring QB (Single Game)",
    "Highest Scoring RB (Single Game)",
    "Highest Scoring WR (Single Game)",
    "Highest Scoring TE (Single Game)",
    "Most Games Scoring 20+ Points",
    "Most Games Under 5 Points (as starter)",
    "Most Points in a Single Season",
    "Most Popular Starter (Times Started)",
    "Most Consistent (Lowest Variability, min 20 starts)",
    "Most Boom-or-Bust (Highest Variability, min 20 starts)",
    "Biggest Week-to-Week Jump",
    "Biggest Week-to-Week Drop",
    ...(rec.busts !== null
      ? [
          "Biggest Busts (Projected vs Actual)",
          "Biggest Booms (Over-Performed Projection)",
        ]
      : []),
    "Most Career Bench Points",
    "Highest Non-QB Single-Game Score",
    "Highest Non-QB Score Left on Bench",
    "Non-QB: Most Games Scoring 30+",
    "Non-QB: Most Games Scoring 20+",
    "Non-QB: Highest Avg Per Start (min 10)",
    "Non-QB: Most Career Starter Points",
    "Non-QB: Most Points in a Season",
    "QB: Most Points in a Season (Top 20)",
    "RB: Most Points in a Season (Top 20)",
    "WR: Most Points in a Season (Top 20)",
    "TE: Most Points in a Season (Top 20)",
  ];
  const tocItems: TocItem[] = sectionTitles.map((t) => ({
    id: slug(t),
    title: t,
  }));

  return (
    <div className="record-book">
      <style>{RECORD_BOOK_CSS}</style>
      <style>{`
        .record-book .player-records th { font-family:'IM Fell English',Georgia,serif; font-size:14px; color:#5c3a10; }
      `}</style>
      <h1>The GFFL Player Records</h1>
      <div className="subtitle">
        A Catalogue of Heroes, Villains &amp; Benchwarming Legends
      </div>
      <div className="fleuron">{FLEURON_TRIPLE}</div>

      <Toc items={tocItems} />

      <Top5Section title="Highest Single-Game Score" rows={rec.topScores} />
      <Top5Section title="Most Games Scoring 50+ Points" rows={rec.above50} />
      <Top5Section title="Most Games Scoring 40+ Points" rows={rec.above40} />
      <Top5Section title="Most Games Scoring 30+ Points" rows={rec.above30} />
      <Top5Section title="Most Total Fantasy Points (Career)" rows={rec.mostTotalPts} />
      <Top5Section title="Highest Avg Points Per Start (min 10)" rows={rec.bestAvg} />
      <Top5Section title="Most Starts Across All Seasons" rows={rec.mostStarts} />

      <Fleuron />

      <Top5Section title="Highest Score Left on Bench" rows={rec.topBench} />
      <Top5Section title="Most Weeks Spent on the Bench" rows={rec.benchKings} />
      <Top5Section title="Most Goose Eggs (0 pts as starter)" rows={rec.gooseEggs} />

      <Fleuron />

      <Top5Section title="Most Loyal: Seasons with Same Owner" rows={rec.loyalty} />
      <Top5Section title="Most Nomadic: Different Owners" rows={rec.nomads} />
      <Top5Section title="Most NFL Teams Played For" rows={rec.teamHoppers} />

      <Fleuron />

      <Top5Section title="Highest Scoring QB (Single Game)" rows={rec.topQb} />
      <Top5Section title="Highest Scoring RB (Single Game)" rows={rec.topRb} />
      <Top5Section title="Highest Scoring WR (Single Game)" rows={rec.topWr} />
      <Top5Section title="Highest Scoring TE (Single Game)" rows={rec.topTe} />

      <Fleuron />

      <Top5Section title="Most Games Scoring 20+ Points" rows={rec.above20} />
      <Top5Section title="Most Games Under 5 Points (as starter)" rows={rec.under5} />
      <Top5Section title="Most Points in a Single Season" rows={rec.seasonLeaders} />
      <Top5Section title="Most Popular Starter (Times Started)" rows={rec.mostPopular} />

      <Fleuron />

      <Top5Section
        title="Most Consistent (Lowest Variability, min 20 starts)"
        rows={rec.mostConsistent}
      />
      <Top5Section
        title="Most Boom-or-Bust (Highest Variability, min 20 starts)"
        rows={rec.mostVolatile}
      />
      <Top5Section title="Biggest Week-to-Week Jump" rows={rec.weekJumps} />
      <Top5Section title="Biggest Week-to-Week Drop" rows={rec.weekDrops} />

      {rec.busts !== null && (
        <>
          <Fleuron />
          <Top5Section title="Biggest Busts (Projected vs Actual)" rows={rec.busts} />
          <Top5Section
            title="Biggest Booms (Over-Performed Projection)"
            rows={rec.booms ?? []}
          />
        </>
      )}

      <Fleuron />

      <Top5Section title="Most Career Bench Points" rows={rec.benchPts} />
      <Top5Section title="Highest Non-QB Single-Game Score" rows={rec.topNonQb} />
      <Top5Section
        title="Highest Non-QB Score Left on Bench"
        rows={rec.topNonQbBench}
      />
      <Top5Section title="Non-QB: Most Games Scoring 30+" rows={rec.nonQbAbove30} />
      <Top5Section title="Non-QB: Most Games Scoring 20+" rows={rec.nonQbAbove20} />
      <Top5Section title="Non-QB: Highest Avg Per Start (min 10)" rows={rec.nonQbAvg} />
      <Top5Section title="Non-QB: Most Career Starter Points" rows={rec.nonQbTotal} />
      <Top5Section title="Non-QB: Most Points in a Season" rows={rec.nonQbSeason} />

      <Fleuron />

      <h2
        id="seasonal-records-by-position"
        style={{
          fontFamily: "'IM Fell English',Georgia,serif",
          color: "#3d2a10",
          textAlign: "center",
          letterSpacing: 3,
          margin: "20px 0 10px",
          scrollMarginTop: 72,
        }}
      >
        SEASONAL RECORDS BY POSITION
      </h2>

      <Top5Section title="QB: Most Points in a Season (Top 20)" rows={rec.qbSeason} />
      <Top5Section title="RB: Most Points in a Season (Top 20)" rows={rec.rbSeason} />
      <Top5Section title="WR: Most Points in a Season (Top 20)" rows={rec.wrSeason} />
      <Top5Section title="TE: Most Points in a Season (Top 20)" rows={rec.teSeason} />

      <div className="fleuron">{FLEURON_TRIPLE}</div>
      <BackToTop />
    </div>
  );
}
