// "Records" tab — old-record-book style. Port of app.R output$records_book
// (lines 4566-4718) + UI nav_panel "Records" (lines 266-276).
import type { Metadata } from "next";
import { getLeagueData } from "@/lib/data";
import { computeRecordsBook } from "./computeRecordsBook";
import { FLEURON_TRIPLE, RECORD_BOOK_CSS } from "./recordBookCss";
import BackToTop from "./BackToTop";

export const metadata: Metadata = { title: "Records" };

export default function RecordsPage() {
  const { standings, schedule, finalSeasons } = getLeagueData();
  const records = computeRecordsBook(standings, schedule, finalSeasons);

  return (
    <div className="record-book">
      <style>{RECORD_BOOK_CSS}</style>
      <h1>The GFFL Records Book</h1>
      <div className="subtitle">A Chronicle of Glorious Deeds &amp; Dreadful Follies</div>
      <div className="fleuron">{FLEURON_TRIPLE}</div>
      {/* Horizontal-scroll guard for narrow phones: no visual change when
          the table fits, but prevents page-level sideways scroll if not. */}
      <div style={{ overflowX: "auto", WebkitOverflowScrolling: "touch" }}>
        <table className="record-table">
          <thead>
            <tr>
              <th>Record</th>
              <th>Owner</th>
              <th>Metric</th>
              <th>Season</th>
            </tr>
          </thead>
          <tbody>
            {records.map((r, i) => (
              <tr key={i}>
                <td className="cell-record">{r.record}</td>
                <td className="cell-owner">{r.holder}</td>
                <td className="cell-metric">{r.value}</td>
                <td className="cell-season">{r.season}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <div className="fleuron">{FLEURON_TRIPLE}</div>
      <BackToTop />
    </div>
  );
}
