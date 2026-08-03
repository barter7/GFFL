"use client";

// Shared player cell: name, arrival/roster badges, durability, news
// link, and the fantasy-stock chips. Used by both the per-team pods
// and the draft board so a player reads identically in either view.

import { useState } from "react";
import { DepthRow, Signal, SignalDir } from "@/lib/nfl";
import s from "./styles.module.css";

const ARROW: Record<SignalDir, string> = { up: "▲", down: "▼", watch: "◆" };

export function SnapCell({ snaps }: { snaps: number | null }) {
  if (snaps == null)
    return (
      <div className={s.snapWrap}>
        <span className={s.snapN}>—</span>
      </div>
    );
  // 1,100 is a full-time two-phase season; bars are comparative, not absolute
  const w = Math.max(1, Math.round((snaps / 1100) * 46));
  return (
    <div className={s.snapWrap}>
      <span className={s.snapBar} style={{ width: `${w}px` }} />
      <span className={`${s.snapN} ${s.mono}`}>{snaps}</span>
    </div>
  );
}

function Avail({ note }: { note: string | null | undefined }) {
  if (!note) return null;
  // "unsigned"/"exempt" are contract states, not injuries (nflmodel S20)
  const pending = /unsigned|exempt/.test(note);
  return (
    <span
      className={`${s.badge} ${pending ? s.bPEND : s.bAVL}`}
      title={note}
    >
      {pending ? "NOT SIGNED" : "RESERVE"}
    </span>
  );
}

function Durability({ row }: { row: Partial<DepthRow> }) {
  const w = row.y25_weeks_out_injury ?? 0;
  if (w < 2) return null;
  return (
    <span
      className={`${s.dur} ${w < 5 ? s.durMild : ""}`}
      title={`weeks listed Out for injury in 2025${
        row.y25_top_injury ? ` — mostly ${row.y25_top_injury}` : ""
      }`}
    >
      {w}w out &rsquo;25{row.y25_top_injury ? ` (${row.y25_top_injury})` : ""}
    </span>
  );
}

/** Chips plus a collapsible evidence block (tooltips don't exist on touch). */
export function StockChips({ signals }: { signals: Signal[] | undefined }) {
  const [open, setOpen] = useState(false);
  if (!signals?.length) return null;
  return (
    <>
      <span className={s.sigs}>
        {signals.map((sig, i) => (
          <button
            key={i}
            type="button"
            className={`${s.chip} ${s[sig.dir]}`}
            title={sig.detail}
            aria-expanded={open}
            onClick={() => setOpen((v) => !v)}
          >
            <span className={s.arrow}>{ARROW[sig.dir]}</span>
            {sig.label}
          </button>
        ))}
      </span>
      {open && (
        <span className={s.why}>
          {signals.map((sig, i) => (
            <div key={i}>
              <b className={s[sig.dir]} style={{ background: "none", border: 0 }}>
                {ARROW[sig.dir]} {sig.code}
              </b>
              <span>
                {sig.label} — {sig.detail}
              </span>
            </div>
          ))}
        </span>
      )}
    </>
  );
}

export default function PlayerCell({
  row,
  signals,
  extra,
}: {
  row: Partial<DepthRow> & { player_name: string };
  signals?: Signal[];
  /** Board view adds team + depth-slot context here. */
  extra?: React.ReactNode;
}) {
  return (
    <td className={s.who}>
      <span className={s.nm}>{row.player_name}</span>
      {extra}
      {row.status && row.status !== "RET" && (
        <span className={`${s.badge} ${row.status === "NEW" ? s.bNEW : s.bROOKIE}`}>
          {row.status}
        </span>
      )}
      {row.from && <span className={s.from}>{row.from}</span>}
      <Avail note={row.avail_note} />
      <Durability row={row} />
      {row.rotowire_url && (
        <a
          className={s.news}
          href={row.rotowire_url}
          target="_blank"
          rel="noopener noreferrer"
          title="RotoWire player page — news &amp; blurbs"
        >
          news ↗
        </a>
      )}
      {row.blurb && <span className={s.blurb}>{row.blurb}</span>}
      <StockChips signals={signals} />
    </td>
  );
}
