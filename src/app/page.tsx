// Super Bowl-style metallic trophy lettering: ornate Roman caps with a
// vertical gold-metal gradient fill, deep drop shadow, and an outer glow.
const titleBase: React.CSSProperties = {
  fontFamily: "'Cinzel Decorative', 'Cinzel', Georgia, serif",
  fontWeight: 900,
  lineHeight: 1.15,
  margin: 0,
  zIndex: 2,
  position: "relative",
  background:
    "linear-gradient(180deg, #fcee9f 0%, #f0d675 38%, #c8983c 70%, #8a6420 100%)",
  WebkitBackgroundClip: "text",
  backgroundClip: "text",
  color: "transparent",
  WebkitTextFillColor: "transparent",
  filter:
    "drop-shadow(0 3px 2px rgba(0,0,0,0.85)) drop-shadow(0 0 22px rgba(212,168,75,0.45))",
};

// Brighter variant for the middle line.
const titleBright: React.CSSProperties = {
  ...titleBase,
  background:
    "linear-gradient(180deg, #fff7c4 0%, #f7e27a 40%, #d4a84b 72%, #9a7228 100%)",
  WebkitBackgroundClip: "text",
  backgroundClip: "text",
};

const goldLine: React.CSSProperties = {
  height: 3,
  width: "70%",
  maxWidth: 800,
  background:
    "linear-gradient(90deg, transparent, #c9a84c 20%, #f0d675 50%, #c9a84c 80%, transparent)",
  zIndex: 2,
};

export default function Home() {
  return (
    <div
      className="dark-section"
      style={{
        // Fill the first screen (minus nav + page padding) without forcing
        // a blank scroll region past the title on phones.
        minHeight: "min(80vh, calc(100svh - 160px))",
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        justifyContent: "center",
        background:
          "radial-gradient(ellipse at center, #1a1a2e 0%, #0a0a14 70%, #000 100%)",
        padding: "clamp(24px, 6vw, 40px) 16px",
        textAlign: "center",
        position: "relative",
        overflow: "hidden",
      }}
    >
      <div
        style={{
          position: "absolute",
          inset: 0,
          background:
            "radial-gradient(ellipse at top, rgba(212,168,75,0.08), transparent 60%), radial-gradient(ellipse at bottom, rgba(212,168,75,0.05), transparent 60%)",
          pointerEvents: "none",
        }}
      />
      <div style={{ ...goldLine, marginBottom: "clamp(16px, 4vw, 30px)" }} />
      <h1 style={{ ...titleBase, letterSpacing: "clamp(4px, 1.5vw, 8px)", fontSize: "clamp(32px, 8vw, 80px)" }}>
        GROUPIES
      </h1>
      <h1 style={{ ...titleBright, letterSpacing: "clamp(5px, 1.8vw, 10px)", fontSize: "clamp(40px, 10vw, 100px)", margin: "8px 0" }}>
        FANTASY
      </h1>
      <h1 style={{ ...titleBase, letterSpacing: "clamp(4px, 1.5vw, 8px)", fontSize: "clamp(32px, 8vw, 80px)" }}>
        FUCKBOI
      </h1>
      <h1 style={{ ...titleBase, letterSpacing: "clamp(6px, 2vw, 12px)", fontSize: "clamp(28px, 7vw, 70px)", margin: "8px 0 0" }}>
        LEAGUE
      </h1>
      <div style={{ ...goldLine, marginTop: "clamp(16px, 4vw, 30px)" }} />
      <div
        style={{
          color: "#8b6914",
          fontFamily: "Georgia, serif",
          fontStyle: "italic",
          letterSpacing: 4,
          marginTop: 16,
          fontSize: "clamp(12px, 2vw, 18px)",
          zIndex: 2,
          position: "relative",
        }}
      >
        EST. 2016
      </div>
    </div>
  );
}
