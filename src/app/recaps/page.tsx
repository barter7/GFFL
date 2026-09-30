import Link from "next/link";
import PageHeader from "@/components/PageHeader";
import { cardUrl, listRecaps } from "@/lib/recaps";

// the index card names the newest recap's week, so the /recaps link
// reads "Week X recap is live" as the season goes on
const latest = listRecaps()[0];
const latestTitle = latest ? `Week ${latest.week} Recap is LIVE` : "Recaps";
const latestImage = { url: cardUrl("latest"), width: 1200, height: 630, alt: latestTitle };
export const metadata = {
  title: "Recaps",
  description: latest ? `${latest.title} — the newest GFFL recap.` : "GFFL weekly recaps.",
  openGraph: { title: latestTitle, description: latest?.title, type: "website", images: [latestImage] },
  twitter: { card: "summary_large_image", title: latestTitle, images: [latestImage.url] },
};

const longDate = (d?: string) =>
  d ? new Date(`${d}T12:00:00`).toLocaleDateString("en-US", { month: "long", day: "numeric", year: "numeric" }) : "";

export default function RecapsIndex() {
  const recaps = listRecaps();
  return (
    <div className="mx-auto" style={{ maxWidth: 860 }}>
      <PageHeader kicker="This Season" title="Weekly Recaps">
        Every Tuesday after Monday Night Football: every matchup, the studs, the busts and the bench crimes.
      </PageHeader>
      {recaps.length === 0 ? (
        <p className="text-muted">No recaps yet. The season is young.</p>
      ) : (
        <div className="d-flex flex-column gap-2">
          {recaps.map((r, i) => (
            <Link key={r.slug} href={`/recaps/${r.slug}`} className="gffl-recap-row">
              <span className="gffl-recap-wk">
                <small>Week</small>
                {r.week}
              </span>
              <span className="flex-grow-1 min-w-0">
                <span className="gffl-kicker">
                  {r.season} season{r.date ? ` · ${longDate(r.date)}` : ""}
                  {i === 0 && <span className="gffl-new">Latest</span>}
                </span>
                <span className="gffl-recap-title">{r.title.replace(/^Week \d+:\s*/, "")}</span>
              </span>
              <span className="gffl-recap-go" aria-hidden="true">›</span>
            </Link>
          ))}
        </div>
      )}
    </div>
  );
}
