"use client";

// "Recap Photos" tab — ported from app.R lines 337-346 (UI) and 2489-2567
// (output$recap_gallery). The R code listed www/recaps at runtime; the actual
// directory contents are hardcoded here (README.txt excluded, as in the R code).

import { useCallback, useEffect, useRef, useState } from "react";
import { photoUrl } from "@/lib/data";
import styles from "./styles.module.css";

const RECAP_FILES = [
  "5qgq9n.jpg",
  "Alex.jpeg",
  "C9994535-7AEB-4690-9231-DC24C5103FC5-8046-000008EC0AB40C24.JPEG",
  "IMG-0183.jpeg",
  "IMG-1999.jpg",
  "IMG-2096.PNG",
  "IMG-2247.PNG",
  "IMG-7229.PNG",
  "IMG_0252 (1).JPG",
  "IMG_0252.JPG",
  "IMG_0578.JPG",
  "IMG_0850 (1).PNG",
  "IMG_0850.PNG",
  "IMG_1801.jpg",
  "IMG_1966.PNG",
  "IMG_2344.PNG",
  "IMG_3185.PNG",
  "IMG_4214.jpg",
  "IMG_7161.jpg",
  "Image (1).jpg",
  "RJ.jpg",
  "The_dirty_bubble.png",
  "Week 1.jpg",
  "Week 2 2021.png",
  "Week 5 2019.jpeg",
  "Week 6 2019.jpg",
  "Week whatever.png",
  "You-Doodle-2020-10-21T22-41-54Z.JPG",
  "You_Doodle_2016-10-11T14_32_14Z (1).jpg",
  "You_Doodle_2016-10-18T14_38_31Z.jpg",
  "You_Doodle_2016-11-01T15_55_27Z.jpg",
  "You_Doodle_2016-11-15T15_37_06Z.jpg",
  "You_Doodle_2018-09-04T20_56_40Z.jpg",
  "You_Doodle_2018-10-10T17_49_48Z.jpg",
  "beamer.0.jpeg",
  "eyepatch.png",
  "f41b4632c3dc659a4801d7731cf29b0f.jpg",
  "gladiator-2.jpg",
  "harrylol.png",
  "image_6483441.JPG",
  "joe is.png",
  "parker.jpg",
  "solemnity-ofallsaints11935404907853604-2-638.jpg",
  "unnamed (1).jpg",
  "unnamed.jpg",
  "week 1 2021.png",
  "week 1.png",
  "week 12.jpg",
  "week 2.jpg",
  "week 2.png",
  "week 3 2019.jpg",
  "week 3.jpg",
  "week 4 2019.jpg",
  "week 4.jpg",
  "week 7 2019.jpg",
  "week 7.jpg",
  "week 8.jpg",
  "week 9 2019.png",
  "week1.png",
  "yeah but stills is back.jpg",
];

// Shuffle for a fun collage feel (replaces set.seed(42); sample(...) —
// deterministic so server and client render identically).
function mulberry32(seed: number): () => number {
  let a = seed >>> 0;
  return () => {
    a |= 0;
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function seededShuffle<T>(arr: T[], seed: number): T[] {
  const rand = mulberry32(seed);
  const out = [...arr];
  for (let i = out.length - 1; i > 0; i--) {
    const j = Math.floor(rand() * (i + 1));
    [out[i], out[j]] = [out[j], out[i]];
  }
  return out;
}

const SHUFFLED_FILES = seededShuffle(RECAP_FILES, 42);
const SRCS = SHUFFLED_FILES.map((f) => encodeURI(photoUrl(`recaps/${f}`)));

export default function RecapPhotosPage() {
  // Lightbox now tracks an index into SRCS (null = closed) so we can step
  // prev/next, show a counter, and preload neighbors.
  const [lightboxIdx, setLightboxIdx] = useState<number | null>(null);
  const touchStartX = useRef<number | null>(null);
  const swiped = useRef(false);
  const total = SRCS.length;

  const step = useCallback(
    (delta: number) => {
      setLightboxIdx((cur) =>
        cur === null ? null : (cur + delta + total) % total,
      );
    },
    [total],
  );

  useEffect(() => {
    const onKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape") setLightboxIdx(null);
      else if (e.key === "ArrowLeft") step(-1);
      else if (e.key === "ArrowRight") step(1);
    };
    document.addEventListener("keydown", onKeyDown);
    return () => document.removeEventListener("keydown", onKeyDown);
  }, [step]);

  useEffect(() => {
    document.body.style.overflow = lightboxIdx !== null ? "hidden" : "";
    return () => {
      document.body.style.overflow = "";
    };
  }, [lightboxIdx]);

  // Preload the adjacent images so prev/next feels instant.
  useEffect(() => {
    if (lightboxIdx === null || total < 2) return;
    [(lightboxIdx + 1) % total, (lightboxIdx - 1 + total) % total].forEach(
      (i) => {
        const img = new window.Image();
        img.src = SRCS[i];
      },
    );
  }, [lightboxIdx, total]);

  return (
    <div>
      <div className="text-center" style={{ margin: "4px 0 10px" }}>
        <h2
          style={{
            color: "#013369",
            fontFamily: "Georgia,serif",
            margin: "0 0 4px",
            fontSize: "clamp(20px, 5.5vw, 28px)",
          }}
        >
          GFFL Through The Years
        </h2>
        <hr style={{ borderColor: "#013369", width: 200, margin: "0 auto" }} />
      </div>

      {SHUFFLED_FILES.length === 0 ? (
        <h4 className="text-muted text-center">No recap photos uploaded yet.</h4>
      ) : (
        <>
          {/* Lightbox overlay */}
          <div
            className={`${styles.lightbox}${lightboxIdx !== null ? ` ${styles.open}` : ""}`}
            onClick={() => {
              // Don't treat the tail end of a swipe as a "close" tap.
              if (swiped.current) {
                swiped.current = false;
                return;
              }
              setLightboxIdx(null);
            }}
            onTouchStart={(e) => {
              touchStartX.current = e.touches[0].clientX;
            }}
            onTouchEnd={(e) => {
              if (touchStartX.current === null) return;
              const dx = e.changedTouches[0].clientX - touchStartX.current;
              touchStartX.current = null;
              if (Math.abs(dx) > 50) {
                swiped.current = true;
                step(dx < 0 ? 1 : -1);
              }
            }}
          >
            <span className={styles.lightboxClose}>&times;</span>
            {lightboxIdx !== null && (
              <>
                {/* eslint-disable-next-line @next/next/no-img-element */}
                <img src={SRCS[lightboxIdx]} alt="" />
                <button
                  type="button"
                  className={`${styles.lightboxNav} ${styles.lightboxPrev}`}
                  aria-label="Previous photo"
                  onClick={(e) => {
                    e.stopPropagation();
                    step(-1);
                  }}
                >
                  &#8249;
                </button>
                <button
                  type="button"
                  className={`${styles.lightboxNav} ${styles.lightboxNext}`}
                  aria-label="Next photo"
                  onClick={(e) => {
                    e.stopPropagation();
                    step(1);
                  }}
                >
                  &#8250;
                </button>
                <div className={styles.lightboxCounter}>
                  {lightboxIdx + 1} / {total}
                </div>
              </>
            )}
          </div>

          <div
            style={{
              padding: "clamp(6px, 2vw, 12px)",
              background: "#1a1a2e",
              borderRadius: 12,
            }}
          >
            <div className={styles.recapCollage}>
              {SHUFFLED_FILES.map((f, idx) => {
                const src = SRCS[idx];
                return (
                  <div key={f} className={styles.recapItem} style={{ breakInside: "avoid" }}>
                    {/* eslint-disable-next-line @next/next/no-img-element */}
                    <img
                      src={src}
                      alt={f}
                      loading="lazy"
                      onClick={() => setLightboxIdx(idx)}
                      style={{
                        width: "100%",
                        borderRadius: 6,
                        boxShadow: "0 2px 8px rgba(0,0,0,0.3)",
                        cursor: "pointer",
                        transition: "transform 0.2s",
                      }}
                      onMouseOver={(e) => {
                        e.currentTarget.style.transform = "scale(1.02)";
                      }}
                      onMouseOut={(e) => {
                        e.currentTarget.style.transform = "scale(1)";
                      }}
                    />
                  </div>
                );
              })}
            </div>
          </div>
        </>
      )}
    </div>
  );
}
