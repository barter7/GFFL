"use client";

// Xbox 360-style achievement gallery components, ported from app.R lines
// 3913-4154 (build_badge, build_owner_section_from_result, summary table).
//
// Client component: adds touch-first interactivity (tap-to-open achievement
// modal, owner quick-jump bar, per-owner badge filters, gamerscore progress
// bars) on top of the original static gallery. The achievements model is
// computed on the server (page.tsx) and passed down as serializable props so
// the raw league JSON never ships to the client.

import { useState } from "react";
import Modal from "@/components/Modal";
import { ACHIEVEMENTS, type AchievementDef } from "./definitions";
import type { AchievementsModel } from "./compute";

/** owner -> resolved headshot URL (or null), built server-side in page.tsx */
export type OwnerPhotos = Record<string, string | null>;

type BadgeFilter = "all" | "unlocked" | "locked";

/** What was tapped: an achievement, optionally in the context of one owner's card. */
interface Selection {
  ach: AchievementDef;
  owner?: string;
}

// Rarity tiers — same thresholds/colors as the original summary table.
function rarityInfo(count: number, total: number) {
  const pct = Math.round((100 * count) / total);
  const color =
    pct <= 10 ? "#ff4444"
    : pct <= 25 ? "#ff8c00"
    : pct <= 50 ? "#ffd700"
    : pct <= 75 ? "#a1c943"
    : "#888";
  const label =
    pct <= 10 ? "LEGENDARY"
    : pct <= 25 ? "RARE"
    : pct <= 50 ? "UNCOMMON"
    : "COMMON";
  return { pct, color, label };
}

function ownerAnchorId(owner: string): string {
  return "owner-card-" + owner.replace(/[^A-Za-z0-9_-]/g, "");
}

// Xbox 360 style achievement badge: circle with 4 segments split by
// horizontal and vertical gaps, center circle with icon. Rendered as a
// <button> so it is a comfortable, accessible tap target (≥44px).
function Badge({
  ach,
  unlocked,
  detail,
  onTap,
}: {
  ach: AchievementDef;
  unlocked: boolean;
  detail?: string;
  onTap: () => void;
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
    <button
      type="button"
      title={tip}
      aria-label={`${ach.name} — ${unlocked ? "unlocked" : "locked"}. Tap for details.`}
      onClick={onTap}
      style={{
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        margin: 0,
        width: "100%",
        maxWidth: 96,
        minHeight: 96,
        padding: 0,
        background: "none",
        border: "none",
        font: "inherit",
        color: "inherit",
        cursor: "pointer",
        WebkitTapHighlightColor: "rgba(161,201,67,0.25)",
        touchAction: "manipulation",
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
          marginTop: 6,
          fontSize: 11,
          textAlign: "center",
          color: textColor,
          fontFamily: "Arial,sans-serif",
          fontWeight: "bold",
          lineHeight: 1.2,
        }}
      >
        {ach.name}
      </div>
    </button>
  );
}

/** Sticky owner quick-jump bar: tap a chip to smooth-scroll to that gamer card. */
function QuickJumpBar({ owners, photos }: { owners: string[]; photos: OwnerPhotos }) {
  const jump = (owner: string) => {
    document
      .getElementById(ownerAnchorId(owner))
      ?.scrollIntoView({ behavior: "smooth", block: "start" });
  };

  return (
    <div
      style={{
        position: "sticky",
        top: 0,
        zIndex: 100,
        background: "rgba(10,10,10,0.96)",
        backdropFilter: "blur(4px)",
        WebkitBackdropFilter: "blur(4px)",
        borderBottom: "1px solid #2a2a2a",
        // Bleed exactly to the page-padding edge (page padding is the same clamp)
        margin: "0 calc(-1 * clamp(10px, 3vw, 20px)) 12px",
        padding: "6px clamp(10px, 3vw, 20px)",
        display: "flex",
        gap: 6,
        overflowX: "auto",
        WebkitOverflowScrolling: "touch",
      }}
    >
      {owners.map((o) => {
        const photo = photos[o];
        return (
          <button
            key={o}
            type="button"
            onClick={() => jump(o)}
            aria-label={`Jump to ${o}'s card`}
            style={{
              display: "flex",
              alignItems: "center",
              gap: 5,
              flex: "0 0 auto",
              minHeight: 38,
              padding: "3px 10px 3px 5px",
              background: "#1a1a1a",
              border: "1px solid #3a3a3a",
              borderRadius: 19,
              color: "#e8e8e8",
              fontFamily: "'Courier New',monospace",
              fontWeight: "bold",
              fontSize: 12,
              letterSpacing: 1,
              cursor: "pointer",
              whiteSpace: "nowrap",
              touchAction: "manipulation",
            }}
          >
            {photo ? (
              // eslint-disable-next-line @next/next/no-img-element
              <img
                src={photo}
                alt=""
                width={24}
                height={24}
                style={{
                  width: 24,
                  height: 24,
                  borderRadius: "50%",
                  objectFit: "cover",
                  objectPosition: "top",
                  border: "1px solid #a1c943",
                  imageRendering: "pixelated",
                }}
              />
            ) : (
              <span
                style={{
                  width: 24,
                  height: 24,
                  borderRadius: "50%",
                  background: "#333",
                  border: "1px solid #a1c943",
                  display: "inline-flex",
                  alignItems: "center",
                  justifyContent: "center",
                  fontSize: 13,
                }}
              >
                {"\u{1F464}"}
              </span>
            )}
            {o.toUpperCase()}
          </button>
        );
      })}
    </div>
  );
}

/** Modal body shared by badge taps (owner context) and summary-row taps. */
function AchievementDetail({ sel, model }: { sel: Selection; model: AchievementsModel }) {
  const { ach, owner } = sel;
  const { allOwners, results, achValues } = model;

  const unlockedBy = allOwners.filter((o) => results[o].unlocked[ach.id] === true);
  const rarity = rarityInfo(unlockedBy.length, allOwners.length);
  const value = achValues[ach.id];

  const ownerUnlocked = owner != null && results[owner].unlocked[ach.id] === true;
  const ownerDetail = owner != null ? results[owner].detail[ach.id] : undefined;
  const others = owner != null ? unlockedBy.filter((o) => o !== owner) : unlockedBy;

  const labelStyle: React.CSSProperties = {
    color: "#a1c943",
    fontFamily: "'Courier New',monospace",
    fontWeight: "bold",
    fontSize: 12,
    letterSpacing: 1,
    textTransform: "uppercase",
  };

  return (
    <div style={{ fontFamily: "Arial,sans-serif", fontSize: 14, lineHeight: 1.5 }}>
      {/* Criteria */}
      <div style={{ marginBottom: 14 }}>
        <div style={labelStyle}>Criteria</div>
        <div style={{ color: "#e8e8e8" }}>{ach.desc}</div>
      </div>

      {/* Value + rarity */}
      <div style={{ display: "flex", gap: 24, marginBottom: 14, flexWrap: "wrap" }}>
        <div>
          <div style={labelStyle}>Value</div>
          <div style={{ color: "#a1c943", fontWeight: "bold", fontSize: 18 }}>
            {value.toLocaleString("en-US")}
            <span style={{ fontSize: 13, marginLeft: 2 }}>G</span>
          </div>
        </div>
        <div>
          <div style={labelStyle}>Rarity</div>
          <div style={{ fontWeight: "bold" }}>
            <span style={{ color: rarity.color }}>{rarity.label}</span>
            <span style={{ color: "#aaa", fontWeight: "normal", marginLeft: 8, fontSize: 13 }}>
              {unlockedBy.length}/{allOwners.length} owners
            </span>
          </div>
        </div>
      </div>

      {/* Tapped owner's status (badge-tap variant only) */}
      {owner != null && (
        <div
          style={{
            marginBottom: 14,
            padding: "10px 12px",
            borderRadius: 8,
            background: ownerUnlocked ? "rgba(161,201,67,0.12)" : "rgba(255,255,255,0.05)",
            border: `1px solid ${ownerUnlocked ? "#a1c943" : "#3a3a3a"}`,
          }}
        >
          <div style={{ fontFamily: "'Courier New',monospace", fontWeight: "bold", letterSpacing: 1 }}>
            <span style={{ color: "#a1c943" }}>{owner.toUpperCase()}</span>
            <span style={{ color: ownerUnlocked ? "#a1c943" : "#888", marginLeft: 10 }}>
              {ownerUnlocked ? "✓ UNLOCKED" : "\u{1F512} LOCKED"}
            </span>
          </div>
          {ownerUnlocked && ownerDetail && (
            <div style={{ color: "#ccc", fontSize: 13, marginTop: 4 }}>
              First unlocked: <span style={{ color: "#fff" }}>{ownerDetail}</span>
            </div>
          )}
        </div>
      )}

      {/* Who else has it */}
      <div>
        <div style={{ ...labelStyle, marginBottom: 4 }}>
          {owner != null ? "Also unlocked by" : "Unlocked by"}
        </div>
        {others.length === 0 ? (
          <div style={{ color: "#888", fontStyle: "italic" }}>
            {unlockedBy.length === 0 ? "Nobody has unlocked this yet." : "Nobody else."}
          </div>
        ) : (
          <ul style={{ margin: 0, paddingLeft: 18, color: "#ccc" }}>
            {others.map((o) => {
              const d = results[o].detail[ach.id];
              return (
                <li key={o} style={{ marginBottom: 2 }}>
                  <span style={{ color: "#fff", fontWeight: "bold" }}>{o}</span>
                  {d && <span style={{ color: "#999", fontSize: 12 }}> — {d}</span>}
                </li>
              );
            })}
          </ul>
        )}
      </div>
    </div>
  );
}

const FILTERS: { key: BadgeFilter; label: string }[] = [
  { key: "all", label: "ALL" },
  { key: "unlocked", label: "UNLOCKED" },
  { key: "locked", label: "LOCKED" },
];

export function OwnerSection({
  owner,
  model,
  photo,
  maxGold,
  onBadgeTap,
}: {
  owner: string;
  model: AchievementsModel;
  photo: string | null;
  maxGold: number;
  onBadgeTap: (ach: AchievementDef, owner: string) => void;
}) {
  const { results, achValues, careerGames } = model;
  const status = results[owner].unlocked;
  const details = results[owner].detail;

  const [filter, setFilter] = useState<BadgeFilter>("all");

  const unlockedCount = Object.values(status).filter(Boolean).length;
  const gamerscore = ACHIEVEMENTS.reduce(
    (acc, a) => acc + (status[a.id] === true ? achValues[a.id] : 0),
    0
  );
  const pct = maxGold > 0 ? Math.min(100, (100 * gamerscore) / maxGold) : 0;

  const visible = ACHIEVEMENTS.filter((a) => {
    if (filter === "unlocked") return status[a.id] === true;
    if (filter === "locked") return status[a.id] !== true;
    return true;
  });

  return (
    <div
      id={ownerAnchorId(owner)}
      style={{
        background: "linear-gradient(180deg, #1a1a1a 0%, #0a0a0a 100%)",
        border: "2px solid #a1c943",
        borderRadius: 8,
        padding: "clamp(8px, 2.5vw, 15px)",
        marginBottom: 14,
        boxShadow: "0 4px 12px rgba(0,0,0,0.5), 0 0 20px rgba(161,201,67,0.15)",
        scrollMarginTop: 64,
      }}
    >
      {/* Xbox 360 style gamer card header */}
      <div
        style={{
          display: "flex",
          alignItems: "center",
          marginBottom: 10,
          paddingBottom: 10,
          borderBottom: "2px solid #a1c943",
        }}
      >
        {/* Pixelated photo (square, large) */}
        {photo ? (
          // eslint-disable-next-line @next/next/no-img-element
          <img
            src={photo}
            alt={owner}
            loading="lazy"
            decoding="async"
            width={110}
            height={110}
            style={{
              width: "clamp(72px, 22vw, 110px)",
              height: "clamp(72px, 22vw, 110px)",
              objectFit: "cover",
              objectPosition: "top",
              imageRendering: "pixelated",
              border: "3px solid #a1c943",
              marginRight: "clamp(10px, 3vw, 20px)",
              boxShadow: "0 0 12px rgba(161,201,67,0.4)",
              filter: "contrast(1.1) saturate(1.2)",
            }}
          />
        ) : (
          <div
            style={{
              width: "clamp(72px, 22vw, 110px)",
              height: "clamp(72px, 22vw, 110px)",
              background: "#333",
              border: "3px solid #a1c943",
              marginRight: "clamp(10px, 3vw, 20px)",
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
              fontSize: "clamp(18px, 5.5vw, 28px)",
              textShadow: "2px 2px 0 #000",
              letterSpacing: 2,
              marginBottom: 6,
            }}
          >
            {owner.toUpperCase()}
          </div>
          {/* Stats table */}
          <table
            style={{
              fontFamily: "'Courier New',monospace",
              fontSize: "clamp(13px, 3.6vw, 16px)",
              borderCollapse: "collapse",
            }}
          >
            <tbody>
              <tr>
                <td style={{ color: "#a1c943", padding: "2px 14px 2px 0", fontWeight: "bold" }}>
                  Games
                </td>
                <td style={{ color: "#fff", padding: "2px 0", fontWeight: "bold", textAlign: "right" }}>
                  {careerGames[owner]}
                </td>
              </tr>
              <tr>
                <td style={{ color: "#a1c943", padding: "2px 14px 2px 0", fontWeight: "bold" }}>
                  Gamerscore
                </td>
                <td style={{ color: "#fff", padding: "2px 0", fontWeight: "bold", textAlign: "right" }}>
                  {gamerscore.toLocaleString("en-US")}
                  <span style={{ color: "#a1c943", marginLeft: 4 }}>G</span>
                </td>
              </tr>
              <tr>
                <td style={{ color: "#a1c943", padding: "2px 14px 2px 0", fontWeight: "bold" }}>
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

      {/* Gamerscore progress bar */}
      <div style={{ marginBottom: 12 }}>
        <div
          role="progressbar"
          aria-valuenow={gamerscore}
          aria-valuemin={0}
          aria-valuemax={maxGold}
          aria-label={`${owner} gamerscore progress`}
          style={{
            height: 8,
            borderRadius: 4,
            background: "#222",
            border: "1px solid #333",
            overflow: "hidden",
          }}
        >
          <div
            style={{
              width: `${pct}%`,
              height: "100%",
              background: "linear-gradient(90deg, #6f8f2a, #a1c943)",
              borderRadius: 4,
            }}
          />
        </div>
        <div
          style={{
            marginTop: 4,
            fontFamily: "'Courier New',monospace",
            fontSize: 11,
            color: "#888",
            textAlign: "right",
          }}
        >
          {gamerscore.toLocaleString("en-US")}G / {maxGold.toLocaleString("en-US")}G (
          {Math.round(pct)}%)
        </div>
      </div>

      {/* All / Unlocked / Locked filter */}
      <div style={{ display: "flex", gap: 8, marginBottom: 8, flexWrap: "wrap" }}>
        {FILTERS.map((f) => {
          const active = filter === f.key;
          const count =
            f.key === "all"
              ? ACHIEVEMENTS.length
              : f.key === "unlocked"
                ? unlockedCount
                : ACHIEVEMENTS.length - unlockedCount;
          return (
            <button
              key={f.key}
              type="button"
              onClick={() => setFilter(f.key)}
              aria-pressed={active}
              style={{
                minHeight: 38,
                padding: "4px 12px",
                borderRadius: 6,
                border: `1px solid ${active ? "#a1c943" : "#3a3a3a"}`,
                background: active ? "#a1c943" : "#141414",
                color: active ? "#0a0a0a" : "#aaa",
                fontFamily: "'Courier New',monospace",
                fontWeight: "bold",
                fontSize: 12,
                letterSpacing: 1,
                cursor: "pointer",
                touchAction: "manipulation",
              }}
            >
              {f.label} ({count})
            </button>
          );
        })}
      </div>

      {/* Badge grid — auto-fill columns use the full card width with even gaps */}
      <div
        style={{
          display: "grid",
          gridTemplateColumns: "repeat(auto-fill, minmax(76px, 1fr))",
          gap: "10px 4px",
          justifyItems: "center",
        }}
      >
        {visible.length === 0 ? (
          <div style={{ color: "#666", fontFamily: "'Courier New',monospace", padding: 12 }}>
            Nothing here.
          </div>
        ) : (
          visible.map((ach) => (
            <Badge
              key={ach.id}
              ach={ach}
              unlocked={status[ach.id] === true}
              detail={details[ach.id]}
              onTap={() => onBadgeTap(ach, owner)}
            />
          ))
        )}
      </div>
    </div>
  );
}

export function SummarySection({
  model,
  onRowTap,
}: {
  model: AchievementsModel;
  onRowTap: (ach: AchievementDef) => void;
}) {
  const { allOwners, results, achValues } = model;

  // Sticky header cells: stay pinned while the capped-height panel scrolls.
  const thLeft: React.CSSProperties = {
    padding: "6px 8px",
    textAlign: "left",
    color: "#a1c943",
    fontSize: 12,
    position: "sticky",
    top: 0,
    zIndex: 2,
    background: "#161616",
    boxShadow: "inset 0 -2px 0 #a1c943",
    whiteSpace: "nowrap",
  };
  const thCenter: React.CSSProperties = { ...thLeft, textAlign: "center" };

  return (
    <div
      style={{
        background: "linear-gradient(180deg, #1a1a1a 0%, #0a0a0a 100%)",
        border: "2px solid #a1c943",
        borderRadius: 8,
        padding: "10px 10px 6px",
        marginBottom: 16,
        boxShadow: "0 4px 12px rgba(0,0,0,0.5), 0 0 20px rgba(161,201,67,0.15)",
      }}
    >
      <h3
        style={{
          color: "#a1c943",
          fontFamily: "Arial,sans-serif",
          textAlign: "center",
          letterSpacing: 2,
          margin: "0 0 2px",
          fontSize: "clamp(16px, 4.5vw, 20px)",
        }}
      >
        ACHIEVEMENT SUMMARY
      </h3>
      <div
        style={{
          color: "#777",
          fontSize: 11,
          textAlign: "center",
          fontFamily: "Arial,sans-serif",
          marginBottom: 6,
        }}
      >
        Tap a row for details · scroll for more
      </div>
      {/* Capped-height scroll panel keeps this section short on phones */}
      <div
        className="scroll-panel"
        style={{
          overflowX: "auto",
          border: "1px solid #2a2a2a",
          borderRadius: 6,
        }}
      >
      <table style={{ width: "100%", borderCollapse: "collapse", fontFamily: "Arial,sans-serif" }}>
        <thead>
          <tr>
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
            const { color: rarityColor, label: rarityLabel } = rarityInfo(count, allOwners.length);
            const valStr = `${achValues[ach.id].toLocaleString("en-US")}G`;
            return (
              <tr
                key={ach.id}
                onClick={() => onRowTap(ach)}
                onKeyDown={(e) => {
                  if (e.key === "Enter" || e.key === " ") {
                    e.preventDefault();
                    onRowTap(ach);
                  }
                }}
                role="button"
                tabIndex={0}
                aria-label={`${ach.name} details`}
                style={{ cursor: "pointer" }}
              >
                <td
                  style={{
                    padding: "7px 8px",
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
                    padding: "7px 8px",
                    borderBottom: "1px solid #333",
                    color: "#aaa",
                    fontSize: 12,
                  }}
                >
                  {ach.desc}
                </td>
                <td
                  style={{
                    padding: "7px 8px",
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
                    padding: "7px 8px",
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
                    padding: "7px 8px",
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
                    padding: "7px 8px",
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
    </div>
  );
}

/** Top-level interactive gallery: quick-jump bar, summary, owner cards, modal. */
export default function AchievementsGallery({
  model,
  photos,
}: {
  model: AchievementsModel;
  photos: OwnerPhotos;
}) {
  const [sel, setSel] = useState<Selection | null>(null);

  // Max possible gamerscore (sum of every achievement's gold value).
  const maxGold = ACHIEVEMENTS.reduce((acc, a) => acc + (model.achValues[a.id] ?? 0), 0);

  return (
    <>
      <QuickJumpBar owners={model.allOwners} photos={photos} />

      {/* Summary section */}
      <SummarySection model={model} onRowTap={(ach) => setSel({ ach })} />

      {/* Owner cards */}
      {model.allOwners.map((o) => (
        <OwnerSection
          key={o}
          owner={o}
          model={model}
          photo={photos[o] ?? null}
          maxGold={maxGold}
          onBadgeTap={(ach, owner) => setSel({ ach, owner })}
        />
      ))}

      {/* Achievement detail modal */}
      <Modal
        open={sel !== null}
        onClose={() => setSel(null)}
        dark
        maxWidth={440}
        title={
          sel ? (
            <span>
              <span style={{ marginRight: 10 }}>{sel.ach.icon}</span>
              {sel.ach.name}
            </span>
          ) : undefined
        }
      >
        {sel && <AchievementDetail sel={sel} model={model} />}
      </Modal>
    </>
  );
}
