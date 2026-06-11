// "Player Records" tab — old-record-book style. Port of app.R
// output$player_records_book (lines 4162-4564) + UI nav_panel "Player Records"
// (lines 279-289).
import type { Metadata } from "next";
import { headshotUrl } from "@/lib/data";
import { FLEURON, FLEURON_TRIPLE, RECORD_BOOK_CSS } from "../records/recordBookCss";
import { computePlayerRecords, Top5Row } from "./computePlayerRecords";
import Headshot from "./Headshot";

export const metadata: Metadata = { title: "Player Records — GFFL Archives" };

// Port of build_top5() (app.R lines 4197-4224)
function Top5Section({ title, rows }: { title: string; rows: Top5Row[] }) {
  return (
    <>
      <div style={{ margin: "25px 20px 10px" }}>
        <div
          style={{
            fontFamily: "'IM Fell English',Georgia,serif",
            fontSize: 20,
            color: "#3d2a10",
            fontWeight: "bold",
            letterSpacing: 2,
            textTransform: "uppercase",
            borderBottom: "2px solid #5c3a10",
            paddingBottom: 4,
          }}
        >
          {title}
        </div>
      </div>
      <table
        style={{
          width: "calc(100% - 40px)",
          margin: "0 20px",
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
                    fontSize: 14,
                    padding: "4px 6px",
                    width: 30,
                    verticalAlign: "middle",
                  }}
                >
                  #{i + 1}
                </td>
                <td style={{ width: 40, padding: 4 }}>
                  {hsUrl && (
                    <Headshot
                      src={hsUrl}
                      style={{
                        width: 32,
                        height: 32,
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
                    fontSize: 16,
                    fontWeight: 600,
                    padding: 4,
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
                    fontSize: 18,
                    fontWeight: "bold",
                    textAlign: "right",
                    padding: 4,
                    verticalAlign: "middle",
                  }}
                >
                  {r.value}
                </td>
                {r.extra !== undefined && (
                  <td
                    style={{
                      color: "#5c3a10",
                      fontSize: 13,
                      fontStyle: "italic",
                      textAlign: "right",
                      padding: 4,
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
      <hr
        style={{
          border: "none",
          borderTop: "1px dashed rgba(92,58,16,0.25)",
          margin: "10px 20px",
        }}
      />
    </>
  );
}

const Fleuron = () => <div className="fleuron">{FLEURON}</div>;

export default function PlayerRecordsPage() {
  const rec = computePlayerRecords();

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
        style={{
          fontFamily: "'IM Fell English',Georgia,serif",
          color: "#3d2a10",
          textAlign: "center",
          letterSpacing: 3,
          margin: "20px 0 10px",
        }}
      >
        SEASONAL RECORDS BY POSITION
      </h2>

      <Top5Section title="QB: Most Points in a Season (Top 20)" rows={rec.qbSeason} />
      <Top5Section title="RB: Most Points in a Season (Top 20)" rows={rec.rbSeason} />
      <Top5Section title="WR: Most Points in a Season (Top 20)" rows={rec.wrSeason} />
      <Top5Section title="TE: Most Points in a Season (Top 20)" rows={rec.teSeason} />

      <div className="fleuron">{FLEURON_TRIPLE}</div>
    </div>
  );
}
