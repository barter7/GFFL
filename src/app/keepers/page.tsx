// 2026 Keeper Options — computed from the 2025 draft and the Week 17 (final)
// rosters, per the constitution's keeper rules (Section 1.3):
//   * eligible = you drafted the player in 2025 AND he finished the season on
//     your roster
//   * a player who was already a keeper in 2025 costs 2 rounds more in 2026
//   * everything else on your final roster (trades, waiver pickups) and every
//     drafted player who left your roster is ineligible

import { getLeagueData, headshotUrl, DraftRow } from "@/lib/data";
import Card from "@/components/Card";

export const metadata = { title: "Keepers" };

const KEEPER_SEASON = 2025; // season being kept FROM
const TARGET_SEASON = 2026; // season being kept INTO

interface EligibleRow {
  name: string;
  pos: string;
  team: string | null;
  round: number;
  keptLastYear: boolean;
  /** round the keep occupies in 2026 (round - 2 when kept last year) */
  cost: number;
}

interface IneligibleRow {
  name: string;
  pos: string;
  team: string | null;
  /** extra context shown as muted subtext (e.g. "2025 keeper", "drafted by RJ") */
  note?: string;
}

function Headshot({ name, pos, size = 44 }: { name: string; pos: string; size?: number }) {
  const url = headshotUrl(name);
  if (!url || pos === "DST") {
    return (
      <div
        style={{
          width: size,
          height: Math.round(size * 0.73),
          borderRadius: 6,
          background: "#e9ecef",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          fontSize: size * 0.45,
          flex: "0 0 auto",
        }}
      >
        🏈
      </div>
    );
  }
  return (
    // eslint-disable-next-line @next/next/no-img-element
    <img
      src={url}
      alt={name}
      width={size}
      height={Math.round(size * 0.73)}
      loading="lazy"
      style={{ borderRadius: 6, background: "#e9ecef", objectFit: "cover", flex: "0 0 auto" }}
    />
  );
}

export default function KeepersPage() {
  const { drafts, starters, owners } = getLeagueData();

  const draft25 = drafts.filter((d) => d.season === KEEPER_SEASON);
  const finalWeek = Math.max(
    ...starters.filter((s) => s.season === KEEPER_SEASON).map((s) => s.week)
  );

  // Final rosters by owner: player_id -> {name,pos,team}
  const finalRoster = new Map<string, Map<number, { name: string; pos: string; team: string | null }>>();
  for (const s of starters) {
    if (s.season !== KEEPER_SEASON || s.week !== finalWeek) continue;
    const m = finalRoster.get(s.owner) ?? new Map();
    m.set(s.player_id, { name: s.player_name, pos: s.pos, team: s.team });
    finalRoster.set(s.owner, m);
  }

  // Who drafted each player in 2025 (for ineligible reasons)
  const draftedBy = new Map<number, DraftRow>();
  for (const d of draft25) draftedBy.set(d.player_id, d);

  const keeperOwners = owners.filter((o) => finalRoster.has(o)).sort();

  const byOwner = keeperOwners.map((owner) => {
    const roster = finalRoster.get(owner)!;
    const picks = draft25
      .filter((d) => d.owner === owner)
      .sort((a, b) => a.round - b.round);

    const eligible: EligibleRow[] = [];
    // Two ineligibility buckets, rendered as subsections with the reason as
    // the heading rather than repeated per player.
    const draftedGone: IneligibleRow[] = [];
    const notDrafted: IneligibleRow[] = [];

    for (const p of picks) {
      const kept = p.is_keeper === true;
      if (roster.has(p.player_id)) {
        eligible.push({
          name: p.player_name,
          pos: p.pos,
          team: roster.get(p.player_id)!.team ?? p.team,
          round: p.round,
          keptLastYear: kept,
          cost: kept ? p.round - 2 : p.round,
        });
      } else {
        draftedGone.push({
          name: p.player_name,
          pos: p.pos,
          team: p.team,
          note: kept ? `${KEEPER_SEASON} keeper` : undefined,
        });
      }
    }

    // On the final roster but not eligible (not drafted by this owner)
    for (const [pid, info] of roster) {
      const d = draftedBy.get(pid);
      if (d?.owner === owner) continue; // eligible, handled above
      notDrafted.push({
        name: info.name,
        pos: info.pos,
        team: info.team,
        note: d ? `drafted by ${d.owner}` : "free agent pickup",
      });
    }

    return { owner, eligible, draftedGone, notDrafted };
  });

  return (
    <>
      <div className="text-center my-3">
        <h2 style={{ color: "#013369", fontFamily: "Georgia,serif" }}>
          {TARGET_SEASON} Keeper Options
        </h2>
        <hr style={{ borderColor: "#013369", width: 200, margin: "0 auto" }} />
        <p className="text-muted mt-2 mb-0" style={{ fontSize: 14 }}>
          Eligible = drafted by you in {KEEPER_SEASON} <em>and</em> on your roster after the
          Week {finalWeek} championship (Constitution §1.3). Keeping a player a second
          consecutive year costs <strong>2 rounds more</strong> —{" "}
          <span
            style={{
              background: "#fff3cd",
              border: "1px solid #d4a84b",
              borderRadius: 4,
              padding: "0 6px",
            }}
          >
            highlighted
          </span>{" "}
          players were {KEEPER_SEASON} keepers. Declarations lock one hour before the
          draft (Sept 8, {TARGET_SEASON}, 8:00 PM ET). ESPN&apos;s Keeper Selection screen
          is the final authority.
        </p>
      </div>

      <div className="row">
        {byOwner.map(({ owner, eligible, draftedGone, notDrafted }) => (
          <div className="col-md-6 col-xl-4" key={owner}>
            <Card
              header={
                <span>
                  {owner}
                  <span className="text-muted fw-normal" style={{ fontSize: 13 }}>
                    {" "}
                    · {eligible.length} eligible
                  </span>
                </span>
              }
            >
              <div className="d-flex flex-column gap-1">
                {eligible.map((p) => (
                  <div
                    key={p.name + p.round}
                    className="d-flex align-items-center gap-2"
                    style={{
                      padding: "4px 6px",
                      borderRadius: 8,
                      ...(p.keptLastYear
                        ? { background: "#fff3cd", border: "1px solid #d4a84b" }
                        : { borderBottom: "1px solid #f0f0f0" }),
                    }}
                  >
                    <span
                      style={{
                        fontWeight: 700,
                        color: "#013369",
                        minWidth: 76,
                        fontSize: 13,
                        whiteSpace: "nowrap",
                      }}
                    >
                      {p.keptLastYear ? (
                        <>
                          Rd {p.round} <span style={{ color: "#b02a37" }}>→ {p.cost}</span>
                        </>
                      ) : (
                        <>Rd {p.round}</>
                      )}
                    </span>
                    <Headshot name={p.name} pos={p.pos} />
                    <div style={{ minWidth: 0 }}>
                      <div
                        style={{
                          fontWeight: 600,
                          fontSize: 14,
                          overflow: "hidden",
                          textOverflow: "ellipsis",
                          whiteSpace: "nowrap",
                        }}
                      >
                        {p.name}
                      </div>
                      <div className="text-muted" style={{ fontSize: 11.5 }}>
                        {p.pos}
                        {p.team ? ` · ${p.team}` : ""}
                        {p.keptLastYear && (
                          <span style={{ color: "#8a6d00" }}>
                            {" "}
                            · 1st year keeper in {KEEPER_SEASON}
                          </span>
                        )}
                      </div>
                    </div>
                  </div>
                ))}
              </div>

              {(draftedGone.length > 0 || notDrafted.length > 0) && (
                <details className="mt-3">
                  <summary
                    style={{ cursor: "pointer", fontSize: 13, fontWeight: 600, color: "#6c757d" }}
                  >
                    Ineligible ({draftedGone.length + notDrafted.length})
                  </summary>
                  {[
                    {
                      heading: "Drafted, but not on the roster at the end of the season",
                      rows: draftedGone,
                    },
                    {
                      heading: "On the roster at the end of the season, but not drafted",
                      rows: notDrafted,
                    },
                  ]
                    .filter((g) => g.rows.length > 0)
                    .map((g) => (
                      <div key={g.heading} className="mt-2" style={{ opacity: 0.8 }}>
                        <div
                          style={{
                            fontSize: 12,
                            fontWeight: 700,
                            color: "#b02a37",
                            borderBottom: "1px solid #e3c2c6",
                            paddingBottom: 2,
                            marginBottom: 4,
                          }}
                        >
                          {g.heading}
                        </div>
                        <div className="d-flex flex-column gap-1">
                          {g.rows.map((p, i) => (
                            <div
                              key={p.name + i}
                              className="d-flex align-items-center gap-2"
                              style={{ padding: "3px 6px", borderBottom: "1px solid #f4f4f4" }}
                            >
                              <Headshot name={p.name} pos={p.pos} size={36} />
                              <div style={{ minWidth: 0, fontSize: 13.5, fontWeight: 600 }}>
                                {p.name}{" "}
                                <span className="text-muted fw-normal" style={{ fontSize: 11 }}>
                                  {p.pos}
                                  {p.team ? ` · ${p.team}` : ""}
                                  {p.note ? ` · ${p.note}` : ""}
                                </span>
                              </div>
                            </div>
                          ))}
                        </div>
                      </div>
                    ))}
                </details>
              )}
            </Card>
          </div>
        ))}
      </div>
    </>
  );
}
