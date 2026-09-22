import Link from "next/link";
import { notFound } from "next/navigation";
import Card from "@/components/Card";
import RecapBody from "@/components/RecapBody";
import { cardUrl, getRecap, listRecaps } from "@/lib/recaps";

export function generateStaticParams() {
  return listRecaps().map((r) => ({ slug: r.slug }));
}

export async function generateMetadata({
  params,
}: {
  params: Promise<{ slug: string }>;
}) {
  const { slug } = await params;
  const recap = getRecap(slug);
  if (!recap) return { title: "Recap" };
  // the link-preview card: a real .png under public/og/recaps/, rendered
  // by scripts/og_recaps.mjs before every build (metadataBase makes the
  // URL absolute, which iMessage requires)
  const image = { url: cardUrl(recap.slug), width: 1200, height: 630, alt: `GFFL Recap — ${recap.title}` };
  const description = `Week ${recap.week} of the ${recap.season} GFFL season, recapped.`;
  return {
    title: recap.title,
    description,
    openGraph: { title: `GFFL Recap is LIVE — ${recap.title}`, description, type: "article", images: [image] },
    twitter: { card: "summary_large_image", title: `GFFL Recap is LIVE — ${recap.title}`, description, images: [image.url] },
  };
}

export default async function RecapPage({
  params,
}: {
  params: Promise<{ slug: string }>;
}) {
  const { slug } = await params;
  const recap = getRecap(slug);
  if (!recap) notFound();
  return (
    <div className="mx-auto" style={{ maxWidth: 820 }}>
      <Card
        header={
          <div>
            <div className="small text-uppercase fw-semibold text-muted">
              {recap.season} · Week {recap.week}
              {recap.date ? ` · ${recap.date}` : ""}
            </div>
            <div className="fs-4 fw-bold">{recap.title}</div>
          </div>
        }
        headerExtra={
          <Link href="/recaps" className="btn btn-sm btn-outline-secondary">
            ← All recaps
          </Link>
        }
      >
        <RecapBody body={recap.body} />
      </Card>
    </div>
  );
}
