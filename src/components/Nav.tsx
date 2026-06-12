"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { useEffect, useRef } from "react";

const LINKS: [string, string][] = [
  ["/", "GFFL"],
  ["/standings", "Standings"],
  ["/drafts", "Drafts"],
  ["/matchups", "Matchups"],
  ["/head-to-head", "Head-to-Head"],
  ["/trophy-room", "Trophy Room"],
  ["/hall-of-fame", "Hall of Fame"],
  ["/records", "Records"],
  ["/player-records", "Player Records"],
  ["/achievements", "Achievements"],
  ["/top-performances", "Top Performances"],
  ["/recap-photos", "Recap Photos"],
  ["/commissioner", "Commissioner of the Year"],
];

/** Always-visible horizontally scrollable tab strip (no hamburger). */
export default function Nav() {
  const pathname = usePathname();
  const stripRef = useRef<HTMLDivElement>(null);
  const activeRef = useRef<HTMLAnchorElement>(null);

  // Keep the active tab in view when navigating.
  useEffect(() => {
    const strip = stripRef.current;
    const active = activeRef.current;
    if (!strip || !active) return;
    const target =
      active.offsetLeft - strip.clientWidth / 2 + active.clientWidth / 2;
    strip.scrollTo({ left: Math.max(0, target) });
  }, [pathname]);

  return (
    <nav className="gffl-navbar sticky-top">
      <div className="gffl-tabstrip" ref={stripRef}>
        {LINKS.map(([href, label]) => {
          const active = pathname === href;
          return (
            <Link
              key={href}
              href={href}
              ref={active ? activeRef : undefined}
              className={`gffl-tab${active ? " active" : ""}`}
            >
              {label}
            </Link>
          );
        })}
      </div>
    </nav>
  );
}
