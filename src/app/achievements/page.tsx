// "Achievements" tab — Xbox/RPG-style achievements gallery.
// Ported from app.R lines 2642-4161 (output$achievements_gallery) and the
// nav_panel wrapper at app.R lines 292-299.
//
// Server component: computes the achievements model from the league JSON and
// resolves owner headshot URLs, then hands plain serializable props to the
// interactive client gallery (tap-to-open modals, quick-jump bar, filters).

import type { Metadata } from "next";
import { photoUrl } from "@/lib/data";
import { computeAchievementsModel } from "./compute";
import AchievementsGallery, { type OwnerPhotos } from "./gallery";

export const metadata: Metadata = {
  title: "Achievements",
};

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

export default function AchievementsPage() {
  const model = computeAchievementsModel();

  const photos: OwnerPhotos = Object.fromEntries(
    model.allOwners.map((o) => [o, ownerPhoto(o)])
  );

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
      <AchievementsGallery model={model} photos={photos} />
    </div>
  );
}
