"use client";

// Floating "back to top" button for the long record-book pages.
// Appears once the reader has scrolled a few screens; styled to match the
// old-record-book palette. Shared by /records and /player-records.

import { useEffect, useState } from "react";

export default function BackToTop() {
  const [visible, setVisible] = useState(false);

  useEffect(() => {
    const onScroll = () => setVisible(window.scrollY > 500);
    onScroll();
    window.addEventListener("scroll", onScroll, { passive: true });
    return () => window.removeEventListener("scroll", onScroll);
  }, []);

  if (!visible) return null;

  return (
    <button
      type="button"
      aria-label="Back to top"
      onClick={() => window.scrollTo({ top: 0, behavior: "smooth" })}
      style={{
        position: "fixed",
        right: 16,
        bottom: 20,
        zIndex: 1050,
        width: 48,
        height: 48,
        borderRadius: "50%",
        border: "2px solid #8b6914",
        background: "#5c3a10",
        color: "#f4e8d0",
        fontSize: 22,
        lineHeight: 1,
        cursor: "pointer",
        boxShadow: "0 4px 14px rgba(0,0,0,0.4)",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      &#8593;
    </button>
  );
}
