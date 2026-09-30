"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { useEffect, useRef, useState } from "react";
import Crest from "@/components/Crest";

type Item = [href: string, label: string];

// Four sections, one row: the crest is Home, each section opens a panel.
const GROUPS: { label: string; items: Item[] }[] = [
  {
    label: "This Season",
    items: [
      ["/recaps", "Recaps"],
      ["/standings", "Standings"],
      ["/matchups", "Matchups"],
      ["/drafts", "Drafts"],
      ["/keepers", "Keepers"],
    ],
  },
  {
    label: "History",
    items: [
      ["/hall-of-fame", "Hall of Fame"],
      ["/trophy-room", "Trophy Room"],
      ["/records", "Records"],
      ["/player-records", "Player Records"],
      ["/top-performances", "Top Performances"],
      ["/head-to-head", "Head-to-Head"],
      ["/keeper-history", "Keeper History"],
    ],
  },
  {
    label: "Fun",
    items: [
      ["/achievements", "Achievements"],
      ["/recap-photos", "Recap Photos"],
      ["/commissioner", "Commissioner of the Year"],
    ],
  },
  {
    label: "League",
    items: [
      ["/constitution", "Constitution"],
      ["/expansion-draft", "Expansion Draft"],
    ],
  },
];

const isActive = (pathname: string, href: string) =>
  pathname === href || pathname.startsWith(href + "/");

export default function Nav() {
  const pathname = usePathname() ?? "/";
  const [open, setOpen] = useState<string | null>(null);
  const ref = useRef<HTMLElement>(null);

  // close on navigation, outside click and Escape
  useEffect(() => setOpen(null), [pathname]);
  useEffect(() => {
    const onDown = (e: MouseEvent) => {
      if (ref.current && !ref.current.contains(e.target as Node)) setOpen(null);
    };
    const onKey = (e: KeyboardEvent) => e.key === "Escape" && setOpen(null);
    document.addEventListener("mousedown", onDown);
    document.addEventListener("keydown", onKey);
    return () => {
      document.removeEventListener("mousedown", onDown);
      document.removeEventListener("keydown", onKey);
    };
  }, []);

  const current = GROUPS.flatMap((g) => g.items).find(([href]) => isActive(pathname, href));
  const openGroup = GROUPS.find((g) => g.label === open);

  return (
    <nav className="gffl-nav sticky-top" ref={ref}>
      <div className="gffl-nav-row">
        <Link href="/" className="gffl-nav-home" aria-label="GFFL home">
          <Crest size={30} />
          <span className="gffl-nav-wordmark">GFFL</span>
        </Link>
        <div className="gffl-nav-groups">
          {GROUPS.map((g) => {
            const here = g.items.some(([href]) => isActive(pathname, href));
            return (
              <button
                key={g.label}
                type="button"
                className={`gffl-nav-group${here ? " here" : ""}${open === g.label ? " open" : ""}`}
                aria-expanded={open === g.label}
                onClick={() => setOpen(open === g.label ? null : g.label)}
              >
                {g.label}
                <svg width="9" height="9" viewBox="0 0 10 10" aria-hidden="true">
                  <path d="M1 3l4 4 4-4" fill="none" stroke="currentColor" strokeWidth="1.6" />
                </svg>
              </button>
            );
          })}
        </div>
        {current && <span className="gffl-nav-current">{current[1]}</span>}
      </div>
      {openGroup && (
        <div className="gffl-nav-panel">
          {openGroup.items.map(([href, label]) => (
            <Link
              key={href}
              href={href}
              className={`gffl-nav-link${isActive(pathname, href) ? " active" : ""}`}
            >
              {label}
            </Link>
          ))}
        </div>
      )}
    </nav>
  );
}
