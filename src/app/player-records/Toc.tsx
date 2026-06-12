"use client";

// Collapsible table of contents for the ~39 player-record sections.
// Rendered inline at the top of the page; links smooth-scroll to each
// section heading (which carry matching ids and scroll-margin for the
// sticky navbar). Styled to match the old-record-book theme.

import { MouseEvent } from "react";

export interface TocItem {
  id: string;
  title: string;
}

export default function Toc({ items }: { items: TocItem[] }) {
  const go = (e: MouseEvent<HTMLAnchorElement>, id: string) => {
    e.preventDefault();
    document.getElementById(id)?.scrollIntoView({ behavior: "smooth" });
    history.replaceState(null, "", `#${id}`);
  };

  return (
    <details
      style={{
        margin: "0 20px 24px",
        border: "1px solid rgba(92,58,16,0.45)",
        borderRadius: 4,
        background: "rgba(139,105,20,0.07)",
        fontFamily: "'Cormorant Garamond',Georgia,serif",
      }}
    >
      <summary
        style={{
          cursor: "pointer",
          padding: "10px 14px",
          fontFamily: "'IM Fell English',Georgia,serif",
          fontSize: 17,
          color: "#3d2a10",
          letterSpacing: 2,
          textTransform: "uppercase",
          userSelect: "none",
        }}
      >
        Table of Contents ({items.length} records)
      </summary>
      <ol
        style={{
          margin: 0,
          padding: "4px 14px 12px 34px",
          columnWidth: 260,
          columnGap: 24,
          color: "#8b6914",
        }}
      >
        {items.map((item) => (
          <li key={item.id} style={{ breakInside: "avoid" }}>
            <a
              href={`#${item.id}`}
              onClick={(e) => go(e, item.id)}
              style={{
                display: "block",
                padding: "5px 0",
                color: "#5c3a10",
                fontSize: 16,
                fontWeight: 600,
                textDecoration: "underline dotted rgba(92,58,16,0.5)",
              }}
            >
              {item.title}
            </a>
          </li>
        ))}
      </ol>
    </details>
  );
}
