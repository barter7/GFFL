const goldGlow =
  "0 2px 4px rgba(0,0,0,0.8), 0 0 30px rgba(212,168,75,0.4), 0 0 60px rgba(212,168,75,0.2)";

const titleBase: React.CSSProperties = {
  fontFamily: "'Cinzel', Georgia, serif",
  fontWeight: 900,
  textShadow: goldGlow,
  lineHeight: 1.1,
  margin: 0,
  zIndex: 2,
  position: "relative",
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
        minHeight: "85vh",
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        justifyContent: "center",
        background:
          "radial-gradient(ellipse at center, #1a1a2e 0%, #0a0a14 70%, #000 100%)",
        padding: "40px 20px",
        textAlign: "center",
        position: "relative",
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
      <div style={{ ...goldLine, marginBottom: 30 }} />
      <h1 style={{ ...titleBase, color: "#d4a84b", letterSpacing: 8, fontSize: "clamp(32px, 8vw, 80px)" }}>
        GROUPIES
      </h1>
      <h1 style={{ ...titleBase, color: "#f0d675", letterSpacing: 10, fontSize: "clamp(40px, 10vw, 100px)", margin: "10px 0" }}>
        FANTASY
      </h1>
      <h1 style={{ ...titleBase, color: "#d4a84b", letterSpacing: 8, fontSize: "clamp(32px, 8vw, 80px)" }}>
        FUCKBOI
      </h1>
      <h1 style={{ ...titleBase, color: "#d4a84b", letterSpacing: 12, fontSize: "clamp(28px, 7vw, 70px)", margin: "10px 0 0" }}>
        LEAGUE
      </h1>
      <div style={{ ...goldLine, marginTop: 30 }} />
      <div
        style={{
          color: "#8b6914",
          fontFamily: "Georgia, serif",
          fontStyle: "italic",
          letterSpacing: 4,
          marginTop: 20,
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
