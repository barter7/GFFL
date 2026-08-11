// 2026 Keeper Options — computed from the 2025 draft and the Week 17 (final)
// rosters, per the constitution's keeper rules (Section 1.3):
//   * eligible = you drafted the player in 2025 AND he finished the season on
//     your roster
//   * a player who was already a keeper in 2025 costs 2 rounds more in 2026
//     (gold rows; the arrow shows old -> new cost)
//   * everything else is listed under two ineligibility headers per owner

import type { CSSProperties } from "react";
import { headshotUrl } from "@/lib/data";
import { computeKeepers, KEEPER_SEASON, TARGET_SEASON } from "@/lib/keepers";

export const metadata = { title: "Keepers" };

function Headshot({ name, pos, size = 30 }: { name: string; pos: string; size?: number }) {
  const url = headshotUrl(name);
  if (!url || pos === "DST") {
    return (
      <div
        style={{
          width: size,
          height: Math.round(size * 0.73),
          borderRadius: 4,
          background: "#e9ecef",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          fontSize: size * 0.5,
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
      style={{ borderRadius: 4, background: "#e9ecef", objectFit: "cover", flex: "0 0 auto" }}
    />
  );
}

export default function KeepersPage() {
  const { byOwner, finalWeek } = computeKeepers();

  const rowStyle = (highlight: boolean): CSSProperties => ({
    display: "flex",
    alignItems: "center",
    gap: 6,
    padding: "2px 4px",
    fontSize: 13,
    borderRadius: 6,
    ...(highlight
      ? { background: "#fff3cd", border: "1px solid #d4a84b" }
      : { borderBottom: "1px solid #f2f2f2" }),
  });

  const nameStyle: CSSProperties = {
    flex: 1,
    minWidth: 0,
    fontWeight: 600,
    overflow: "hidden",
    textOverflow: "ellipsis",
    whiteSpace: "nowrap",
  };

  return (
    <>
      <div className="text-center my-3">
        <h2 style={{ color: "#013369", fontFamily: "Georgia,serif" }}>
          {TARGET_SEASON} Keeper Options
        </h2>
        <hr style={{ borderColor: "#013369", width: 200, margin: "0 auto" }} />
        <p
          className="text-muted mt-2 mb-0"
          style={{ fontSize: 13, maxWidth: 720, marginLeft: "auto", marginRight: "auto" }}
        >
          Eligible = drafted by you in {KEEPER_SEASON} <em>and</em> on your roster after
          the Week {finalWeek} championship (§1.3).{" "}
          <span
            style={{
              background: "#fff3cd",
              border: "1px solid #d4a84b",
              borderRadius: 4,
              padding: "0 5px",
            }}
          >
            Gold rows
          </span>{" "}
          were 1st-year keepers in {KEEPER_SEASON} — keeping them again costs 2 rounds
          more (old→new). Declarations lock one hour before the draft (Sept 8,{" "}
          {TARGET_SEASON}). ESPN&apos;s Keeper Selection screen is the final authority.
        </p>
      </div>

      <div className="row g-2">
        {byOwner.map(({ owner, slot, eligible, draftedGone, notDrafted, na }) => (
          <div className="col-6 col-md-4 col-xl-3" key={owner}>
            <div className="card">
              <div
                className="card-header"
                style={{
                  background: "#013369",
                  color: "#fff",
                  textAlign: "center",
                  fontSize: 19,
                  fontWeight: 700,
                  letterSpacing: 1,
                  padding: "7px 6px",
                }}
              >
                <span style={{ opacity: 0.65, fontWeight: 600, marginRight: 6 }}>#{slot}</span>
                {owner}
              </div>
              <div className="card-body">
              {na && (
                <div
                  className="text-muted text-center"
                  style={{ fontSize: 16, fontWeight: 600, padding: "10px 0" }}
                >
                  N/A
                </div>
              )}
              <div className="d-flex flex-column" style={{ gap: 2 }}>
                {eligible.map((p) => (
                  <div key={p.name + p.round} style={rowStyle(p.keptLastYear)}>
                    <Headshot name={p.name} pos={p.pos} />
                    <span style={nameStyle}>{p.name}</span>
                    <span
                      style={{
                        fontWeight: 700,
                        color: "#013369",
                        fontSize: 15,
                        whiteSpace: "nowrap",
                      }}
                    >
                      {p.keptLastYear ? (
                        <>
                          {p.round}
                          <span style={{ color: "#b02a37" }}>→{p.cost}</span>
                        </>
                      ) : (
                        <>{p.round}</>
                      )}
                    </span>
                  </div>
                ))}
              </div>

              {(draftedGone.length > 0 || notDrafted.length > 0) && (
                <details className="mt-2">
                  <summary
                    style={{ cursor: "pointer", fontSize: 12, fontWeight: 600, color: "#6c757d" }}
                  >
                    Ineligible ({draftedGone.length + notDrafted.length})
                  </summary>
                  {[
                    {
                      heading: "Drafted, but not on roster at end of season",
                      rows: draftedGone,
                    },
                    {
                      heading: "On roster at end of season, but not drafted",
                      rows: notDrafted,
                    },
                  ]
                    .filter((g) => g.rows.length > 0)
                    .map((g) => (
                      <div key={g.heading} className="mt-2" style={{ opacity: 0.8 }}>
                        <div
                          style={{
                            fontSize: 11,
                            fontWeight: 700,
                            color: "#b02a37",
                            borderBottom: "1px solid #e3c2c6",
                            paddingBottom: 2,
                            marginBottom: 3,
                          }}
                        >
                          {g.heading}
                        </div>
                        <div className="d-flex flex-column" style={{ gap: 2 }}>
                          {g.rows.map((p, i) => (
                            <div key={p.name + i} style={rowStyle(false)}>
                              <Headshot name={p.name} pos={p.pos} size={24} />
                              <span style={{ ...nameStyle, fontSize: 12.5 }}>{p.name}</span>
                              {p.note && (
                                <span
                                  className="text-muted"
                                  style={{ fontSize: 10.5, whiteSpace: "nowrap" }}
                                >
                                  {p.note}
                                </span>
                              )}
                            </div>
                          ))}
                        </div>
                      </div>
                    ))}
                </details>
              )}
              </div>
            </div>
          </div>
        ))}
      </div>
    </>
  );
}
