// Xbox 360-style achievement gallery components, ported from app.R lines
// 3913-4154 (build_badge, build_owner_section_from_result, summary table).

import { photoUrl } from "@/lib/data";
import { ACHIEVEMENTS, type AchievementDef } from "./definitions";
import type { AchievementsModel } from "./compute";

// Basenames (without extension) available in public/photos — mirrors the
// file.exists() lookup against www/photos in app.R.
const PHOTO_FILES = new Set([
  "alex_headshot", "connor_headshot", "faz_headshot", "harry_headshot",
  "jack_headshot", "joe_headshot", "kenny_headshot", "kerley_headshot",
  "matt_headshot", "mike_headshot", "rj_headshot", "sean_headshot",
  "tom_headshot", "xingwei_headshot",
  "faz", "Alex", "Jack", "Kerley", "RJ", "Tom", "Hunt",
]);

function ownerPhoto(o: string): string | null {
  const oClean = o.toLowerCase().replace(/ /g, "");
  for (const variant of [`${oClean}_headshot`, `${o.toLowerCase()}_headshot`, o.toLowerCase(), o]) {
    if (PHOTO_FILES.has(variant)) return photoUrl(`photos/${variant}.png`);
  }
  return null;
}

// Xbox 360 style achievement badge: circle with 4 segments split by
// horizontal and vertical gaps, center circle with icon.
function Badge({
  ach,
  unlocked,
  detail,
}: {
  ach: AchievementDef;
  unlocked: boolean;
  detail?: string;
}) {
  const iconColor = unlocked ? "#a1c943" : "#444";
  const ringColor = unlocked ? "#a1c943" : "#2a2a2a";
  const bgColor = unlocked ? "#1a1a1a" : "#0a0a0a";
  const textColor = unlocked ? "#fff" : "#666";

  let tip = `${ach.name}: ${ach.desc}`;
  if (unlocked && detail) tip += `\nUnlocked: ${detail}`;

  // 4 quarter ring using conic-gradient with gaps
  const ringBg =
    `conic-gradient(from 3deg, ${ringColor} 0deg 84deg, transparent 84deg 96deg, ` +
    `${ringColor} 96deg 174deg, transparent 174deg 186deg, ` +
    `${ringColor} 186deg 264deg, transparent 264deg 276deg, ` +
    `${ringColor} 276deg 354deg, transparent 354deg 360deg)`;
  const ringMask = "radial-gradient(circle, transparent 50%, #000 51%)";

  return (
    <div
      title={tip}
      style={{
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        margin: 8,
        width: 85,
      }}
    >
      {/* Xbox 360 style ring (made a ring by cutting center with mask) */}
      <div
        style={{
          position: "relative",
          width: 70,
          height: 70,
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          background: ringBg,
          borderRadius: "50%",
          WebkitMask: ringMask,
          mask: ringMask,
        }}
      />
      {/* Center circle with icon */}
      <div
        style={{
          position: "absolute",
          marginTop: 14,
          width: 42,
          height: 42,
          borderRadius: "50%",
          background: bgColor,
          border: `1px solid ${ringColor}`,
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          boxShadow: "inset 0 0 8px rgba(0,0,0,0.5)",
        }}
      >
        <span
          style={{
            color: iconColor,
            fontSize: 20,
            // FontAwesome is not available: emoji replacements are desaturated
            // when locked to keep the locked/unlocked visual contrast.
            filter: unlocked ? undefined : "grayscale(1) brightness(0.45)",
          }}
        >
          {ach.icon}
        </span>
      </div>
      {/* Label below */}
      <div
        style={{
          marginTop: 8,
          fontSize: 10,
          textAlign: "center",
          color: textColor,
          fontFamily: "Arial,sans-serif",
          fontWeight: "bold",
          lineHeight: 1.2,
        }}
      >
        {ach.name}
      </div>
    </div>
  );
}

export function OwnerSection({ owner, model }: { owner: string; model: AchievementsModel }) {
  const { results, achValues, careerGames } = model;
  const status = results[owner].unlocked;
  const details = results[owner].detail;

  const photoFile = ownerPhoto(owner);
  const unlockedCount = Object.values(status).filter(Boolean).length;
  const gamerscore = ACHIEVEMENTS.reduce(
    (acc, a) => acc + (status[a.id] === true ? achValues[a.id] : 0),
    0
  );

  return (
    <div
      style={{
        background: "linear-gradient(180deg, #1a1a1a 0%, #0a0a0a 100%)",
        border: "2px solid #a1c943",
        borderRadius: 8,
        padding: 15,
        marginBottom: 20,
        boxShadow: "0 4px 12px rgba(0,0,0,0.5), 0 0 20px rgba(161,201,67,0.15)",
      }}
    >
      {/* Xbox 360 style gamer card header */}
      <div
        style={{
          display: "flex",
          alignItems: "center",
          marginBottom: 15,
          paddingBottom: 12,
          borderBottom: "2px solid #a1c943",
        }}
      >
        {/* Pixelated photo (square, large) */}
        {photoFile ? (
          // eslint-disable-next-line @next/next/no-img-element
          <img
            src={photoFile}
            alt={owner}
            style={{
              width: 110,
              height: 110,
              objectFit: "cover",
              objectPosition: "top",
              imageRendering: "pixelated",
              border: "3px solid #a1c943",
              marginRight: 20,
              boxShadow: "0 0 12px rgba(161,201,67,0.4)",
              filter: "contrast(1.1) saturate(1.2)",
            }}
          />
        ) : (
          <div
            style={{
              width: 110,
              height: 110,
              background: "#333",
              border: "3px solid #a1c943",
              marginRight: 20,
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            <span style={{ color: "#a1c943", fontSize: 40 }}>{"\u{1F464}"}</span>
          </div>
        )}
        {/* Gamer card stats */}
        <div style={{ flex: 1 }}>
          {/* Gamertag */}
          <div
            style={{
              color: "#a1c943",
              fontFamily: "'Courier New',monospace",
              fontWeight: "bold",
              fontSize: 28,
              textShadow: "2px 2px 0 #000",
              letterSpacing: 2,
              marginBottom: 8,
            }}
          >
            {owner.toUpperCase()}
          </div>
          {/* Stats table */}
          <table
            style={{
              fontFamily: "'Courier New',monospace",
              fontSize: 16,
              borderCollapse: "collapse",
            }}
          >
            <tbody>
              <tr>
                <td style={{ color: "#a1c943", padding: "2px 20px 2px 0", fontWeight: "bold" }}>
                  Games
                </td>
                <td style={{ color: "#fff", padding: "2px 0", fontWeight: "bold", textAlign: "right" }}>
                  {careerGames[owner]}
                </td>
              </tr>
              <tr>
                <td style={{ color: "#a1c943", padding: "2px 20px 2px 0", fontWeight: "bold" }}>
                  Gamerscore
                </td>
                <td style={{ color: "#fff", padding: "2px 0", fontWeight: "bold", textAlign: "right" }}>
                  {gamerscore.toLocaleString("en-US")}
                  <span style={{ color: "#a1c943", marginLeft: 4 }}>G</span>
                </td>
              </tr>
              <tr>
                <td style={{ color: "#a1c943", padding: "2px 20px 2px 0", fontWeight: "bold" }}>
                  Achievements
                </td>
                <td style={{ color: "#fff", padding: "2px 0", fontWeight: "bold", textAlign: "right" }}>
                  {unlockedCount} / {ACHIEVEMENTS.length}
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
      {/* Badge grid */}
      <div style={{ display: "flex", flexWrap: "wrap", justifyContent: "flex-start" }}>
        {ACHIEVEMENTS.map((ach) => (
          <Badge
            key={ach.id}
            ach={ach}
            unlocked={status[ach.id] === true}
            detail={details[ach.id]}
          />
        ))}
      </div>
    </div>
  );
}

export function SummarySection({ model }: { model: AchievementsModel }) {
  const { allOwners, results, achValues } = model;

  const thLeft: React.CSSProperties = {
    padding: "8px 10px",
    textAlign: "left",
    color: "#a1c943",
    fontSize: 13,
  };
  const thCenter: React.CSSProperties = { ...thLeft, textAlign: "center" };

  return (
    <div
      style={{
        background: "linear-gradient(180deg, #1a1a1a 0%, #0a0a0a 100%)",
        border: "2px solid #a1c943",
        borderRadius: 8,
        padding: 15,
        marginBottom: 30,
        boxShadow: "0 4px 12px rgba(0,0,0,0.5), 0 0 20px rgba(161,201,67,0.15)",
        overflowX: "auto",
      }}
    >
      <h3
        style={{
          color: "#a1c943",
          fontFamily: "Arial,sans-serif",
          textAlign: "center",
          letterSpacing: 2,
          marginBottom: 12,
        }}
      >
        ACHIEVEMENT SUMMARY
      </h3>
      <table style={{ width: "100%", borderCollapse: "collapse", fontFamily: "Arial,sans-serif" }}>
        <thead>
          <tr style={{ borderBottom: "2px solid #a1c943" }}>
            <th style={thLeft}>Achievement</th>
            <th style={thLeft}>Description</th>
            <th style={thCenter}>Unlocked</th>
            <th style={thCenter}>Rarity</th>
            <th style={thCenter}>Value</th>
            <th style={thLeft}>Who</th>
          </tr>
        </thead>
        <tbody>
          {ACHIEVEMENTS.map((ach) => {
            const unlockedBy = allOwners.filter((o) => results[o].unlocked[ach.id] === true);
            const count = unlockedBy.length;
            const who = count > 0 ? unlockedBy.join(", ") : "Nobody";
            const rarityPct = Math.round((100 * count) / allOwners.length);
            const rarityColor =
              rarityPct <= 10 ? "#ff4444"
              : rarityPct <= 25 ? "#ff8c00"
              : rarityPct <= 50 ? "#ffd700"
              : rarityPct <= 75 ? "#a1c943"
              : "#888";
            const rarityLabel =
              rarityPct <= 10 ? "LEGENDARY"
              : rarityPct <= 25 ? "RARE"
              : rarityPct <= 50 ? "UNCOMMON"
              : "COMMON";
            const valStr = `${achValues[ach.id].toLocaleString("en-US")}G`;
            return (
              <tr key={ach.id}>
                <td
                  style={{
                    padding: "6px 10px",
                    borderBottom: "1px solid #333",
                    whiteSpace: "nowrap",
                  }}
                >
                  <span style={{ color: count > 0 ? "#a1c943" : "#555", marginRight: 6 }}>
                    {ach.icon}
                  </span>
                  <span style={{ color: count > 0 ? "#fff" : "#555", fontWeight: "bold" }}>
                    {ach.name}
                  </span>
                </td>
                <td
                  style={{
                    padding: "6px 10px",
                    borderBottom: "1px solid #333",
                    color: "#aaa",
                    fontSize: 12,
                  }}
                >
                  {ach.desc}
                </td>
                <td
                  style={{
                    padding: "6px 10px",
                    borderBottom: "1px solid #333",
                    textAlign: "center",
                    fontWeight: "bold",
                    color: rarityColor,
                  }}
                >
                  {count}/{allOwners.length}
                </td>
                <td
                  style={{
                    padding: "6px 10px",
                    borderBottom: "1px solid #333",
                    textAlign: "center",
                    fontSize: 10,
                    fontWeight: "bold",
                    color: rarityColor,
                  }}
                >
                  {rarityLabel}
                </td>
                <td
                  style={{
                    padding: "6px 10px",
                    borderBottom: "1px solid #333",
                    textAlign: "center",
                    color: "#a1c943",
                    fontSize: 12,
                    fontWeight: "bold",
                  }}
                >
                  {valStr}
                </td>
                <td
                  style={{
                    padding: "6px 10px",
                    borderBottom: "1px solid #333",
                    color: "#ccc",
                    fontSize: 12,
                  }}
                >
                  {who}
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}
