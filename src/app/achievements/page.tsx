// "Achievements" tab — Xbox/RPG-style achievements gallery.
// Ported from app.R lines 2642-4161 (output$achievements_gallery) and the
// nav_panel wrapper at app.R lines 292-299.

import type { Metadata } from "next";
import { computeAchievementsModel } from "./compute";
import { OwnerSection, SummarySection } from "./gallery";

export const metadata: Metadata = {
  title: "Achievements",
};

export default function AchievementsPage() {
  const model = computeAchievementsModel();

  return (
    <div
      style={{
        background: "#0a0a0a",
        padding: 20,
        borderRadius: 12,
        minHeight: "80vh",
      }}
    >
      <h2
        style={{
          color: "#a1c943",
          fontFamily: "Arial,sans-serif",
          textAlign: "center",
          letterSpacing: 3,
          marginBottom: 20,
        }}
      >
        GFFL ACHIEVEMENTS
      </h2>
      {/* Summary section */}
      <SummarySection model={model} />
      {/* Owner cards */}
      {model.allOwners.map((o) => (
        <OwnerSection key={o} owner={o} model={model} />
      ))}
    </div>
  );
}
