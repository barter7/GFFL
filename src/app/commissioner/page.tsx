// "Commissioner of the Year" tab — ported from app.R lines 349-361 (UI) and
// 2568-2641 (output$commish_gallery). Static content, rendered on the server.

import { photoUrl } from "@/lib/data";

const COMMISSIONERS: { year: number; name: string }[] = [
  ...Array.from({ length: 9 }, (_, i) => ({ year: 2016 + i, name: "Harry" })),
  { year: 2025, name: "Tom" },
];

// The R code searched www/photos at runtime for "<name>_headshot" / "<name>"
// with .png/.jpg/.jpeg/.PNG extensions; both winners resolve to these files.
const PHOTO_FILES: Record<string, string> = {
  Harry: "photos/harry_headshot.png",
  Tom: "photos/tom_headshot.png",
};

function CommishCard({ year, name }: { year: number; name: string }) {
  const photoFile = PHOTO_FILES[name];

  return (
    <div
      style={{
        display: "inline-block",
        textAlign: "center",
        margin: 10,
        verticalAlign: "top",
        width: 140,
      }}
    >
      {/* Photo */}
      {photoFile ? (
        <div style={{ position: "relative", width: 120, height: 140, margin: "0 auto" }}>
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
            {/* eslint-disable-next-line @next/next/no-img-element */}
            <img
              src={photoUrl(photoFile)}
              alt={name}
              style={{
                width: "100%",
                height: "100%",
                objectFit: "cover",
                objectPosition: "top",
              }}
            />
          </div>
          {/* eslint-disable-next-line @next/next/no-img-element */}
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
      ) : (
        <div
          style={{
            width: 120,
            height: 140,
            margin: "0 auto",
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            background: "#2a1f18",
            borderRadius: 4,
          }}
        >
          <span style={{ color: "#555", fontSize: "2.5rem" }}>👤</span>
        </div>
      )}

      {/* Gold name plaque */}
      <div
        style={{
          marginTop: 6,
          padding: 2,
          background: "linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413)",
          borderRadius: 4,
        }}
      >
        <div
          style={{
            background: "linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828)",
            border: "1px solid #8b6914",
            borderRadius: 3,
            padding: "4px 8px",
            textAlign: "center",
          }}
        >
          <div
            style={{
              color: "#3d2b0a",
              fontFamily: "Georgia,serif",
              fontWeight: "bold",
              fontSize: 13,
              textTransform: "uppercase",
              textShadow: "0 1px 0 rgba(255,255,255,0.3)",
            }}
          >
            {name}
          </div>
          <div
            style={{
              color: "#3d2b0a",
              fontFamily: "Georgia,serif",
              fontWeight: "bold",
              fontSize: 16,
              textShadow: "0 1px 0 rgba(255,255,255,0.3)",
            }}
          >
            {year}
          </div>
        </div>
      </div>
    </div>
  );
}

export default function CommissionerPage() {
  return (
    <div
      style={{
        background: "linear-gradient(180deg, #0f0f1a 0%, #1a1a2e 50%, #16213e 100%)",
        padding: "30px 20px",
        borderRadius: 12,
        textAlign: "center",
        minHeight: "80vh",
      }}
    >
      <h2
        style={{
          color: "#d4a84b",
          fontFamily: "Georgia,serif",
          letterSpacing: 3,
          marginBottom: 5,
        }}
      >
        COMMISSIONER OF THE YEAR
      </h2>
      <hr style={{ borderColor: "#8b6914", width: 200, margin: "0 auto 30px" }} />
      <div
        style={{
          display: "flex",
          flexWrap: "wrap",
          justifyContent: "center",
          gap: 10,
        }}
      >
        {COMMISSIONERS.map((c) => (
          <CommishCard key={c.year} year={c.year} name={c.name} />
        ))}
      </div>
    </div>
  );
}
