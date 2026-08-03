"use client";

// 2026 depth charts + draft board.
//
// Three views over one dataset: per-team position groups, one
// position across all 32 teams, and a flat ranked draft board.
// Everything is keyed on gsis_id (nflmodel R1.13).

import { useMemo, useState } from "react";
import Card from "@/components/Card";
import {
  BOARD_POS,
  BOARD_SORTS,
  BoardPos,
  BoardSort,
  DepthRow,
  OutRow,
  POS_GROUPS,
  getNflDepth,
} from "@/lib/nfl";
import PlayerCell, { SnapCell } from "./PlayerCell";
import s from "./styles.module.css";

type Mode = "team" | "pos" | "rank";
type StatCol = "2025" | "career";

export default function DepthChartsPage() {
  const D = useMemo(() => getNflDepth(), []);

  const [mode, setMode] = useState<Mode>("team");
  const [team, setTeam] = useState(D.teams[0]);
  const [group, setGroup] = useState("QB");
  const [stat, setStat] = useState<StatCol>("2025");
  const [changedOnly, setChangedOnly] = useState(false);
  const [availOnly, setAvailOnly] = useState(false);
  const [stockOnly, setStockOnly] = useState(false);
  const [boardPos, setBoardPos] = useState<BoardPos>("ALL");
  // ADP is unavailable until an export lands, so never start there
  const [sort, setSort] = useState<BoardSort>(D.meta.has_adp ? "adp" : "r1");

  const line = (r: Partial<DepthRow> | Partial<OutRow> | undefined) =>
    (stat === "2025" ? r?.line_2025 : r?.line_career) || null;

  // ── per-team / per-position pods ──────────────────────────
  function Pod({ team: t, grp }: { team: string; grp: string }) {
    const din = D.depth
      .filter((r) => r.team === t && r.grp === grp)
      .sort((a, b) => a.slot - b.slot);
    const dout = D.out
      .filter((r) => r.team === t && r.grp === grp)
      .sort((a, b) => (b.snaps25 ?? 0) - (a.snaps25 ?? 0));
    if (!din.length && !dout.length) return null;

    const nNew = din.filter((r) => r.status !== "RET").length;
    if (changedOnly && !nNew && !dout.length) return null;
    if (
      availOnly &&
      !din.some((r) => r.avail_note || (r.y25_weeks_out_injury ?? 0) >= 5)
    )
      return null;
    if (stockOnly && !din.some((r) => D.signals[r.gsis_id])) return null;

    const isOL = grp === "OL";
    const noGrade = isOL ? <i>no public grade</i> : "—";

    return (
      <section className={s.pod}>
        <div className={s.podHead}>
          {mode === "team" ? (
            <span className="g">{grp}</span>
          ) : (
            <>
              <span className="t">{t}</span>
              <span className="g" style={{ color: "#8b9099" }}>
                {grp}
              </span>
            </>
          )}
          <span className={s.churn}>
            {nNew ? <><b>{nNew}</b> in</> : null}
            {nNew && dout.length ? " · " : null}
            {dout.length ? <><b>{dout.length}</b> out</> : null}
            {!nNew && !dout.length ? "unchanged" : null}
          </span>
        </div>
        <table className={s.tbl}>
          <tbody>
            {din.map((r) => (
              <tr key={r.gsis_id} className={r.status !== "RET" ? s.rowNew : ""}>
                <td className={`${s.num} ${s.mono}`}>{r.slot}</td>
                <PlayerCell row={r} signals={D.signals[r.gsis_id]} />
                <td className={s.stat}>{line(r) ?? noGrade}</td>
                <td style={{ width: 88 }}>
                  <SnapCell snaps={r.snaps25} />
                </td>
              </tr>
            ))}
            {dout.length > 0 && (
              <tr>
                <td colSpan={4} className={s.gap}>
                  left the team (250+ snaps in 2025)
                </td>
              </tr>
            )}
            {dout.map((r) => (
              <tr key={r.gsis_id} className={s.rowOut}>
                <td className={s.num}>↓</td>
                <PlayerCell
                  row={{ ...r, status: undefined, from: null }}
                  extra={
                    <>
                      <span className={`${s.badge} ${s.bOUT}`}>OUT</span>
                      <span className={s.from}>→ {r.to}</span>
                    </>
                  }
                />
                <td className={s.stat}>{line(r) ?? noGrade}</td>
                <td style={{ width: 88 }}>
                  <SnapCell snaps={r.snaps25} />
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>
    );
  }

  // ── draft board ───────────────────────────────────────────
  const boardRows = useMemo(() => {
    const key: BoardSort = sort === "adp" && !D.meta.has_adp ? "r1" : sort;
    let rows = D.rank.filter((r) => {
      // an unranked player is not rank 999 — leave him out entirely
      if (key === "adp" && r.adp == null) return false;
      if (boardPos === "ALL") return true;
      if (boardPos === "FLEX") return ["RB", "WR", "TE"].includes(r.pos);
      return r.pos === boardPos;
    });
    if (stockOnly) rows = rows.filter((r) => D.signals[r.gsis_id]);
    if (availOnly)
      rows = rows.filter((r) => {
        const d = D.depthById[r.gsis_id];
        return !!d && (!!d.avail_note || (d.y25_weeks_out_injury ?? 0) >= 5);
      });
    return [...rows].sort(
      (a, b) => ((a[key] ?? 1e9) as number) - ((b[key] ?? 1e9) as number),
    );
  }, [D, sort, boardPos, stockOnly, availOnly]);

  function Board() {
    const key: BoardSort = sort === "adp" && !D.meta.has_adp ? "r1" : sort;
    const header =
      key === "adp" ? "ADP" : key === "r2" ? "ECR (superflex)" : "ECR (1QB)";
    const posn: Record<string, number> = {};
    return (
      <>
        {!D.meta.has_adp && (
          <div className={s.warn}>
            <b>Sorted by ECR, not ADP.</b> No ADP export is present, and expert
            consensus rank is not a stand-in for where players actually get
            drafted — the gap between the two is the thing worth having. Drop a
            FantasyPros ADP export into <code>data/fantasypros/</code>, re-run{" "}
            <code>export_site.R</code>, and this control switches over.
          </div>
        )}
        <div className={s.board}>
          {boardRows.length === 0 ? (
            <p className={s.empty}>
              Nothing matches that combination of position and filters.
            </p>
          ) : (
            <table className={s.tbl}>
              <thead>
                <tr>
                  <th style={{ textAlign: "right" }}>#</th>
                  <th>Pos</th>
                  <th>Player</th>
                  <th style={{ textAlign: "right" }}>{header}</th>
                  <th style={{ textAlign: "right" }}>
                    {stat === "2025" ? "2025" : "Career"}
                  </th>
                  <th style={{ textAlign: "right" }}>Snaps</th>
                </tr>
              </thead>
              <tbody>
                {boardRows.map((r, i) => {
                  posn[r.pos] = (posn[r.pos] ?? 0) + 1;
                  const d = D.depthById[r.gsis_id];
                  const val = key === "adp" ? r.adp : key === "r2" ? r.ecr2 : r.ecr1;
                  return (
                    <tr
                      key={r.gsis_id}
                      className={d?.status && d.status !== "RET" ? s.rowNew : ""}
                    >
                      <td className={`${s.ovr} ${s.mono}`}>{i + 1}</td>
                      <td className={`${s.posRank} ${s.mono}`}>
                        {r.pos}
                        {posn[r.pos]}
                      </td>
                      <PlayerCell
                        row={{ ...d, player_name: r.player }}
                        signals={D.signals[r.gsis_id]}
                        extra={
                          <>
                            <span className={s.tm}>{r.team}</span>
                            {d?.slot ? (
                              <span className={s.tm}>
                                {r.pos}
                                {d.slot} on depth chart
                              </span>
                            ) : null}
                          </>
                        }
                      />
                      <td className={`${s.val} ${s.mono}`}>
                        <b>{val ?? "—"}</b>
                      </td>
                      <td className={s.stat}>{line(d) ?? "—"}</td>
                      <td style={{ width: 88 }}>
                        <SnapCell snaps={d?.snaps25 ?? null} />
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          )}
        </div>
      </>
    );
  }

  const modeBtn = (m: Mode, label: string) => (
    <button
      key={m}
      type="button"
      className={`btn btn-sm ${mode === m ? "btn-primary" : "btn-outline-secondary"}`}
      aria-pressed={mode === m}
      onClick={() => setMode(m)}
    >
      {label}
    </button>
  );
  const toggle = (on: boolean, set: (v: boolean) => void, label: string) => (
    <button
      type="button"
      className={`btn btn-sm ${on ? "btn-primary" : "btn-outline-secondary"}`}
      aria-pressed={on}
      onClick={() => set(!on)}
    >
      {label}
    </button>
  );

  return (
    <>
      <Card
        header="2026 Depth Charts — who's new, who's gone"
        headerExtra={
          <span className="text-muted small">Generated {D.meta.generated}</span>
        }
      >
        <p className="text-muted small mb-3" style={{ maxWidth: "78ch" }}>
          Every position group with its 2026 depth chart, arrivals marked in
          place, and last year&rsquo;s contributors who left listed underneath.
          Snap bars and production lines show which changes actually matter, and
          skill players carry fantasy-stock chips — ▲ helps, ▼ hurts, ◆ watch —
          with the evidence one click away.
        </p>

        <div className={s.bar}>
          {modeBtn("team", "By team")}
          {modeBtn("pos", "By position")}
          {modeBtn("rank", "Draft board")}
          <span className={s.sep} />

          {mode === "rank" ? (
            <>
              <select
                className="form-select form-select-sm w-auto"
                aria-label="Filter the board by position"
                value={boardPos}
                onChange={(e) => setBoardPos(e.target.value as BoardPos)}
              >
                {BOARD_POS.map((p) => (
                  <option key={p} value={p}>
                    {p === "ALL"
                      ? "All positions"
                      : p === "FLEX"
                        ? "FLEX (RB/WR/TE)"
                        : p}
                  </option>
                ))}
              </select>
              <span className={s.sep} />
              <label className={s.lbl} htmlFor="board-sort">
                Sort
              </label>
              <select
                id="board-sort"
                className="form-select form-select-sm w-auto"
                value={sort}
                onChange={(e) => setSort(e.target.value as BoardSort)}
              >
                {BOARD_SORTS.map((o) => {
                  const off = o.needsAdp && !D.meta.has_adp;
                  return (
                    <option key={o.key} value={o.key} disabled={off}>
                      {o.label}
                      {off ? " — no data" : ""}
                    </option>
                  );
                })}
              </select>
            </>
          ) : (
            <select
              className="form-select form-select-sm w-auto"
              aria-label="Choose team or position"
              value={mode === "team" ? team : group}
              onChange={(e) =>
                mode === "team" ? setTeam(e.target.value) : setGroup(e.target.value)
              }
            >
              {(mode === "team" ? D.teams : POS_GROUPS).map((v) => (
                <option key={v} value={v}>
                  {v}
                </option>
              ))}
            </select>
          )}

          <span className={s.sep} />
          <button
            type="button"
            className={`btn btn-sm ${stat === "2025" ? "btn-primary" : "btn-outline-secondary"}`}
            aria-pressed={stat === "2025"}
            onClick={() => setStat("2025")}
          >
            2025 stats
          </button>
          <button
            type="button"
            className={`btn btn-sm ${stat === "career" ? "btn-primary" : "btn-outline-secondary"}`}
            aria-pressed={stat === "career"}
            onClick={() => setStat("career")}
          >
            Career (21–25)
          </button>
          <span className={s.sep} />
          {mode !== "rank" &&
            toggle(changedOnly, setChangedOnly, "Changed groups only")}
          {toggle(availOnly, setAvailOnly, "Availability flags")}
          {toggle(stockOnly, setStockOnly, "Fantasy stock only")}
        </div>

        {mode === "rank" ? (
          <Board />
        ) : (
          <div className={s.grid}>
            {mode === "team"
              ? POS_GROUPS.map((g) => <Pod key={g} team={team} grp={g} />)
              : D.teams.map((t) => <Pod key={t} team={t} grp={group} />)}
          </div>
        )}
      </Card>

      <Card header="How to read this">
        <div className={s.notes}>
          <p>
            Depth order is the latest 2026 camp snapshot — a snapshot, not a
            Week 1 guarantee. <b>NEW</b> = on another team in 2025 (the roster
            diff can&rsquo;t separate a signing from a trade); <b>ROOKIE</b>{" "}
            shows its draft slot, or UDFA. Departures are 2025 players with 250+
            snaps no longer on the roster. Bars are 2025 offensive + defensive
            snaps.
          </p>
          <p>
            <b>Availability:</b> official weekly injury reports don&rsquo;t
            exist until Week 1, so preseason status comes from the roster —{" "}
            <b>RESERVE</b> = IR/PUP/NFI, <b>NOT SIGNED</b> = unsigned or
            unreported draft pick (verified: that code appears on 17 rookies and
            0 veterans, so it is a contract status, not an injury). The red
            durability note is weeks listed <i>Out</i> for injury in 2025,
            excluding rest and coach&rsquo;s-decision rows.
          </p>
          <p>
            <b>Fantasy stock:</b> ▲ helps, ▼ hurts, ◆ changes the picture
            without a clear sign — click any chip for the evidence behind it.
            Chips cover QB/RB/WR/TE only and are computed, not editorial:{" "}
            <b>DRAFT</b> = a round 1–3 pick at his own position;{" "}
            <b>SIGNED</b>/<b>VACATED</b> = a same-position arrival or departure,
            shown with that player&rsquo;s actual 2025 line, because
            &ldquo;they signed a receiver&rdquo; means nothing until you know
            whether he saw 30 targets or 130; <b>TGTS</b>/<b>CARR</b> = the
            share of the team&rsquo;s 2025 targets or carries now held by
            players off the roster; <b>HC</b>/<b>OC</b>/<b>QB</b> = turnover
            above him; <b>PACE</b> = 2025 seconds per snap at least 1.5s off the
            league mean; <b>AVAIL</b>/<b>INJ</b> = his own roster status or 5+
            weeks lost in 2025. These are volume-context flags, not projections.
          </p>
          <p>
            <b>Draft board:</b> one flat ranked list across all 32 teams,
            filterable by position (FLEX = RB/WR/TE) and sortable by ADP or ECR.
            Each row keeps its depth-chart context — arrival badge, depth slot,
            availability, stock chips — because a rank without the roster
            situation behind it is how you end up drafting a back who lost his
            job. Ranks come from FantasyPros via the DynastyProcess mirror
            {D.meta.ecr_date ? `, scraped ${D.meta.ecr_date}` : ""}, joined on{" "}
            <code>gsis_id</code> (99% matched; the 2026 rookie class
            isn&rsquo;t in that crosswalk yet, so those rows fall back to a
            name+position match against our own roster and say so).{" "}
            <b>ADP and ECR are not the same thing</b> and this page never
            substitutes one for the other: ECR is where analysts rank a player,
            ADP is where drafters actually take him, and the gap between them is
            the exploitable part.{" "}
            {D.meta.has_adp
              ? "Both are loaded, so the ADP sort is live."
              : "Only ECR is loaded, so the ADP sort is disabled rather than quietly reordering by the wrong column."}
          </p>
          <p>
            <b>Player news:</b> each row links to that player&rsquo;s RotoWire
            page, built from the <code>rotowire_id</code> in the nflverse
            crosswalk, covering 82% of depth-chart rows — no link is shown where
            no id exists rather than a guessed URL.
            {D.meta.has_pff ? (
              <> <b>PFF grades:</b> loaded.</>
            ) : (
              <>
                {" "}
                <b>PFF grades:</b> not available — PFF is proprietary and
                appears in no nflverse dataset, so these lines use nflverse
                production instead. Offensive line is where that gap bites
                hardest.
              </>
            )}
          </p>
        </div>
      </Card>
    </>
  );
}
