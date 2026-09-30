import Link from "next/link";
import { notFound } from "next/navigation";
import PageHeader from "@/components/PageHeader";
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
  // by src/og/render-cards.mjs before every build (metadataBase makes the
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
  const date = recap.date
    ? new Date(`${recap.date}T12:00:00`).toLocaleDateString("en-US", { month: "long", day: "numeric", year: "numeric" })
    : "";
  return (
    <div className="mx-auto" style={{ maxWidth: 860 }}>
      <PageHeader
        kicker={`${recap.season} · Week ${recap.week}${date ? ` · ${date}` : ""}`}
        title={recap.title}
        extra={
          <Link href="/recaps" className="btn btn-sm btn-outline-secondary">
            ← All recaps
          </Link>
        }
      />
      <article className="card">
        <div className="card-body gffl-recap-body">
          <RecapBody body={recap.body} />
        </div>
      </article>
      {/* The site is a static export, so editing happens in GitHub's
          editor (needs write access to the repo): commit, and the site
          redeploys in a couple of minutes. See RECAP_GUIDE.md. */}
      <div className="mt-2 text-end">
        <a
          href={`https://github.com/barter7/GFFL/edit/main/src/data/recaps/${recap.slug}.md`}
          target="_blank"
          rel="noopener noreferrer"
          className="small text-muted text-decoration-none"
        >
          ✎ Edit this recap
        </a>
      </div>
    </div>
  );
}
