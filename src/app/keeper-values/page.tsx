// 2026 Keeper Value — every keeper option priced against the draft
// round it would cost. A keeper's VAL (half-PPR value over replacement,
// from the GFFL draft board) is compared with the average VAL of the
// players the market expects to still be going in that round; the
// difference is what the keep earns over simply making the pick.

import type { CSSProperties } from "react";
import { headshotUrl, fmt } from "@/lib/data";
import { computeKeepers, TARGET_SEASON } from "@/lib/keepers";
import { keeperValue, BOARD_AS_OF, KeeperValue } from "@/lib/keeperValue";

export const metadata = { title: "Keeper Value" };

const NAVY = "#013369";
const GREEN = "#1f7a4d";
const RED = "#b02a37";

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

interface PricedRow {
  name: string;
  pos: string;
  cost: number;
  keptLastYear: boolean;
  kv: KeeperValue;
}

export default function KeeperValuesPage() {
  const { byOwner } = computeKeepers();

  const priced = byOwner.map((o) => {
    const rows: PricedRow[] = o.eligible.map((p) => ({
      name: p.name,
      pos: p.pos,
      cost: p.cost,
      keptLastYear: p.keptLastYear,
      kv: keeperValue(p.name, p.pos, p.cost),
    }));
    // best keeps first; anyone the board doesn't carry sinks to the
    // bottom (off the board = below the last rostered player)
    rows.sort((a, b) => (b.kv.boost ?? -Infinity) - (a.kv.boost ?? -Infinity));
    return { ...o, rows };
  });

  const rowStyle: CSSProperties = {
    display: "flex",
    alignItems: "center",
    gap: 6,
    padding: "2px 4px",
    fontSize: 13,
    borderBottom: "1px solid #f2f2f2",
  };
  const nameStyle: CSSProperties = {
    flex: 1,
    minWidth: 0,
    fontWeight: 600,
    overflow: "hidden",
    textOverflow: "ellipsis",
    whiteSpace: "nowrap",
  };
  const num: CSSProperties = {
    width: 38,
    textAlign: "right",
    fontVariantNumeric: "tabular-nums",
    whiteSpace: "nowrap",
  };

  const boostText = (kv: KeeperValue) => {
    if (kv.boost == null) return { text: "—", color: "#6c757d" };
    const sign = kv.boost > 0 ? "+" : "";
    return {
      text: `${sign}${fmt(kv.boost)}`,
      color: kv.boost > 0 ? GREEN : kv.boost < 0 ? RED : "#6c757d",
    };
  };

  return (
    <>
      <div className="text-center my-3">
        <h2 style={{ color: NAVY, fontFamily: "Georgia,serif" }}>
          {TARGET_SEASON} Keeper Value
        </h2>
        <hr style={{ borderColor: NAVY, width: 200, margin: "0 auto" }} />
        <p
          className="text-muted mt-2 mb-0"
          style={{ fontSize: 13, maxWidth: 720, marginLeft: "auto", marginRight: "auto" }}
        >
          What each keeper option is worth over just making the pick.{" "}
          <b>VAL</b> is the player&apos;s half-PPR value over replacement from the
          GFFL draft board; <b>Rd&nbsp;avg</b> is the average VAL of the players
          the market (ADP) expects to go in his cost round; <b>+/-</b> is the
          difference — keep a +5.0 and you drafted a player five points a week
          better than that round would have handed you. Gold-row keepers are
          priced at their escalated cost. A dash means the player is outside the
          board&apos;s top 243, i.e. below replacement — keeping him costs the
          pick outright. Board as of {BOARD_AS_OF}.
        </p>
      </div>

      <div className="row g-2">
        {priced.map(({ owner, slot, rows, na }) => (
          <div className="col-6 col-md-4 col-xl-3" key={owner}>
            <div className="card">
              <div
                className="card-header"
                style={{
                  background: NAVY,
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
                {rows.length > 0 && (
                  <div className="d-flex flex-column" style={{ gap: 2 }}>
                    <div
                      style={{
                        ...rowStyle,
                        fontSize: 10.5,
                        fontWeight: 700,
                        color: "#6c757d",
                        textTransform: "uppercase",
                        letterSpacing: 0.4,
                        borderBottom: "2px solid #dee2e6",
                      }}
                    >
                      <span style={{ flex: 1 }}>Player</span>
                      <span style={num}>Rd</span>
                      <span style={num}>VAL</span>
                      <span style={num}>Avg</span>
                      <span style={{ ...num, width: 44 }}>+/-</span>
                    </div>
                    {rows.map((p) => {
                      const b = boostText(p.kv);
                      return (
                        <div key={p.name + p.cost} style={rowStyle}>
                          <Headshot name={p.name} pos={p.pos} size={26} />
                          <span style={{ ...nameStyle, fontSize: 12.5 }}>{p.name}</span>
                          <span style={{ ...num, color: NAVY, fontWeight: 700 }}>
                            {p.cost}
                          </span>
                          <span style={{ ...num, color: "#495057" }}>
                            {p.kv.val != null ? fmt(p.kv.val) : "—"}
                          </span>
                          <span style={{ ...num, color: "#868e96" }}>
                            {p.kv.baseline != null ? fmt(p.kv.baseline) : "—"}
                          </span>
                          <span
                            style={{ ...num, width: 44, color: b.color, fontWeight: 800 }}
                          >
                            {b.text}
                          </span>
                        </div>
                      );
                    })}
                  </div>
                )}
              </div>
            </div>
          </div>
        ))}
      </div>
    </>
  );
}
