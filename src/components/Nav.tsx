"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { useState } from "react";

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

export default function Nav() {
  const pathname = usePathname();
  const [open, setOpen] = useState(false);

  return (
    <nav className="navbar navbar-expand-xl navbar-dark gffl-navbar sticky-top">
      <div className="container-fluid">
        <Link className="navbar-brand fw-bold" href="/">
          GFFL Archives
        </Link>
        <button
          className="navbar-toggler"
          type="button"
          aria-label="Toggle navigation"
          onClick={() => setOpen(!open)}
        >
          <span className="navbar-toggler-icon" />
        </button>
        <div className={`collapse navbar-collapse${open ? " show" : ""}`}>
          <ul className="navbar-nav me-auto">
            {LINKS.map(([href, label]) => (
              <li className="nav-item" key={href}>
                <Link
                  className={`nav-link${pathname === href ? " active" : ""}`}
                  href={href}
                  onClick={() => setOpen(false)}
                >
                  {label}
                </Link>
              </li>
            ))}
          </ul>
          <a
            className="nav-link text-light"
            href="https://github.com/barter7/gffl"
            target="_blank"
            rel="noreferrer"
          >
            Source
          </a>
        </div>
      </div>
    </nav>
  );
}
