import Link from "next/link";
import Card from "@/components/Card";
import { listRecaps } from "@/lib/recaps";

// the index card names the newest recap's week, so the /recaps link
// reads "Week X recap is live" as the season goes on
const latest = listRecaps()[0];
const latestTitle = latest ? `Week ${latest.week} Recap is LIVE` : "Recaps";
const latestImage = { url: "/og/recaps/latest.png", width: 1200, height: 630, alt: latestTitle };
export const metadata = {
  title: "Recaps",
  description: latest ? `${latest.title} — the newest GFFL recap.` : "GFFL weekly recaps.",
  openGraph: { title: latestTitle, description: latest?.title, type: "website", images: [latestImage] },
  twitter: { card: "summary_large_image", title: latestTitle, images: [latestImage.url] },
};

export default function RecapsIndex() {
  const recaps = listRecaps();
  return (
    <div className="mx-auto" style={{ maxWidth: 820 }}>
      <Card header={<span className="fw-bold">Weekly Recaps</span>}>
        {recaps.length === 0 ? (
          <p className="text-muted mb-0">No recaps yet. The season is young.</p>
        ) : (
          <div className="d-flex flex-column gap-3">
            {recaps.map((r) => (
              <Link
                key={r.slug}
                href={`/recaps/${r.slug}`}
                className="text-decoration-none border rounded p-3 d-block"
              >
                <div className="small text-uppercase fw-semibold text-muted">
                  {r.season} · Week {r.week}
                  {r.date ? ` · ${r.date}` : ""}
                </div>
                <div className="fs-5 fw-bold">{r.title}</div>
              </Link>
            ))}
          </div>
        )}
      </Card>
    </div>
  );
}
