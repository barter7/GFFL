// Keeper History — every keeper slot ESPN has recorded for the league,
// grouped by owner. ESPN flags keepers on the draft record (is_keeper), so
// this grows automatically as future drafts are fetched by the daily refresh.

import { getLeagueData, headshotUrl } from "@/lib/data";
import Card from "@/components/Card";

export const metadata = { title: "Keeper History" };

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

export default function KeeperHistoryPage() {
  const { drafts, owners } = getLeagueData();

  const keeps = drafts
    .filter((d) => d.is_keeper === true)
    .sort((a, b) => a.season - b.season || a.round - b.round);

  const byOwner = new Map<string, typeof keeps>();
  for (const k of keeps) {
    const arr = byOwner.get(k.owner) ?? [];
    arr.push(k);
    byOwner.set(k.owner, arr);
  }

  // Owners with keeps first (most keeps, then alphabetical), rest after
  const withKeeps = owners
    .filter((o) => byOwner.has(o))
    .sort((a, b) => byOwner.get(b)!.length - byOwner.get(a)!.length || a.localeCompare(b));
  const withoutKeeps = owners.filter((o) => !byOwner.has(o)).sort();

  const flaggedSeasons = [...new Set(keeps.map((k) => k.season))].sort((a, b) => a - b);

  return (
    <>
      <div className="text-center my-3">
        <h2 style={{ color: "#013369", fontFamily: "Georgia,serif" }}>Keeper History</h2>
        <hr style={{ borderColor: "#013369", width: 200, margin: "0 auto" }} />
        <p className="text-muted mt-2 mb-0" style={{ fontSize: 14 }}>
          Every keeper slot recorded on the ESPN draft board, by owner. Keeper slots are
          marked with a gold ★ border on the{" "}
          <a href="/drafts" style={{ color: "#013369" }}>
            Draft Board
          </a>
          . ESPN&apos;s records flag keepers in {flaggedSeasons.join(", ")} — keepers used in
          other seasons weren&apos;t recorded on the platform.
        </p>
      </div>

      <div className="row">
        {withKeeps.map((owner) => {
          const rows = byOwner.get(owner)!;
          return (
            <div className="col-md-6 col-xl-4" key={owner}>
              <Card
                header={
                  <span>
                    {owner}
                    <span className="text-muted fw-normal" style={{ fontSize: 13 }}>
                      {" "}
                      · {rows.length} keeper{rows.length === 1 ? "" : "s"}
                    </span>
                  </span>
                }
              >
                <div className="d-flex flex-column gap-1">
                  {rows.map((k) => (
                    <div
                      key={`${k.season}-${k.player_id}`}
                      className="d-flex align-items-center gap-2"
                      style={{ padding: "4px 6px", borderBottom: "1px solid #f0f0f0" }}
                    >
                      <span
                        style={{
                          fontWeight: 700,
                          color: "#8a6508",
                          minWidth: 44,
                          fontSize: 13,
                        }}
                      >
                        {k.season}
                      </span>
                      <Headshot name={k.player_name} pos={k.pos} />
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
                          ★ {k.player_name}
                        </div>
                        <div className="text-muted" style={{ fontSize: 11.5 }}>
                          {k.pos}
                          {k.team ? ` · ${k.team}` : ""} · kept as Rd {k.round} pick
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              </Card>
            </div>
          );
        })}
      </div>

      {withoutKeeps.length > 0 && (
        <p className="text-muted text-center" style={{ fontSize: 13 }}>
          No recorded keepers: {withoutKeeps.join(", ")}
        </p>
      )}
    </>
  );
}
