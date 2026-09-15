import Link from "next/link";
import { notFound } from "next/navigation";
import Card from "@/components/Card";
import RecapBody from "@/components/RecapBody";
import { getRecap, listRecaps } from "@/lib/recaps";

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
  return { title: recap ? recap.title : "Recap" };
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
