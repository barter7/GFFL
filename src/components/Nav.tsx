"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";

const LINKS: [string, string][] = [
  ["/", "GFFL"],
  ["/standings", "Standings"],
  ["/drafts", "Drafts"],
  ["/matchups", "Matchups"],
  ["/head-to-head", "H2H"],
  ["/trophy-room", "Trophy Room"],
  ["/hall-of-fame", "Hall of Fame"],
  ["/records", "Records"],
  ["/player-records", "Player Records"],
  ["/achievements", "Achievements"],
  ["/top-performances", "Top Performances"],
  ["/recap-photos", "Recap Photos"],
  ["/constitution", "Constitution"],
  ["/commissioner", "Commissioner of the Year"],
];

/** All tabs visible at once: pills wrap onto multiple rows (no hamburger, no scroll). */
export default function Nav() {
  const pathname = usePathname();

  return (
    <nav className="gffl-navbar sticky-top">
      <div className="gffl-tabstrip">
        {LINKS.map(([href, label]) => (
          <Link
            key={href}
            href={href}
            className={`gffl-tab${pathname === href ? " active" : ""}`}
          >
            {label}
          </Link>
        ))}
      </div>
    </nav>
  );
}
