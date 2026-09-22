import { ImageResponse } from "next/og";
import { getRecap, listRecaps } from "@/lib/recaps";

// The link-preview card for one recap: a "RECAP IS LIVE" banner over the
// column's title and week, so a shared link reads as an announcement in
// the group chat. Rendered once per recap at build time (static export).
export const alt = "GFFL Recap";
export const size = { width: 1200, height: 630 };
export const contentType = "image/png";
export const dynamic = "force-static";

export function generateStaticParams() {
  return listRecaps().map((r) => ({ slug: r.slug }));
}

export default async function Image({
  params,
}: {
  params: Promise<{ slug: string }>;
}) {
  const { slug } = await params;
  const recap = getRecap(slug);
  const title = recap?.title ?? "GFFL Recap";
  const sub = recap ? `${recap.season} · Week ${recap.week}` : "";
  const titleSize = title.length > 44 ? 54 : title.length > 30 ? 64 : 76;

  return new ImageResponse(
    (
      <div
        style={{
          width: "100%",
          height: "100%",
          display: "flex",
          flexDirection: "column",
          justifyContent: "space-between",
          padding: "56px 72px",
          background: "radial-gradient(circle at 50% 45%, #1a1a2e 0%, #000008 100%)",
          color: "#ffffff",
          fontFamily: "sans-serif",
        }}
      >
        {/* the banner */}
        <div style={{ display: "flex", alignItems: "center", gap: 22 }}>
          <div
            style={{
              display: "flex",
              alignItems: "center",
              gap: 14,
              background: "#dc2626",
              color: "#ffffff",
              fontSize: 34,
              fontWeight: 800,
              letterSpacing: 2,
              padding: "10px 26px",
              borderRadius: 999,
            }}
          >
            <div
              style={{
                width: 18,
                height: 18,
                borderRadius: 999,
                background: "#ffffff",
              }}
            />
            LIVE
          </div>
          <div
            style={{
              display: "flex",
              fontSize: 40,
              fontWeight: 800,
              letterSpacing: 3,
              color: "#d4af37",
            }}
          >
            GFFL RECAP IS LIVE
          </div>
        </div>

        {/* the column */}
        <div style={{ display: "flex", flexDirection: "column", gap: 18 }}>
          <div
            style={{
              display: "flex",
              fontSize: titleSize,
              fontWeight: 800,
              lineHeight: 1.1,
              maxWidth: 1056,
            }}
          >
            {title}
          </div>
          {sub && (
            <div
              style={{
                display: "flex",
                fontSize: 34,
                fontWeight: 600,
                color: "#c9c9d6",
              }}
            >
              {sub}
            </div>
          )}
        </div>

        {/* the masthead: a gold rule and the league name */}
        <div style={{ display: "flex", flexDirection: "column", gap: 14 }}>
          <div
            style={{
              display: "flex",
              height: 4,
              width: 1056,
              background: "linear-gradient(90deg, #d4af37 0%, #f5e08a 50%, #d4af37 100%)",
            }}
          />
          <div
            style={{
              display: "flex",
              fontSize: 30,
              fontWeight: 700,
              letterSpacing: 4,
              color: "#d4af37",
            }}
          >
            GROUPIES FANTASY FOOTBALL LEAGUE · EST. 2016
          </div>
        </div>
      </div>
    ),
    { ...size },
  );
}
