// Home: the crest, then this week in the league — the latest recap, last
// week's scores, the current standings and the reigning champion.
import Link from "next/link";
import Crest from "@/components/Crest";
import { fmt, getLeagueData } from "@/lib/data";
import { listRecaps } from "@/lib/recaps";
import { findPhoto } from "./trophy-room/photos";

const wordmark: React.CSSProperties = {
  fontFamily: "'Cinzel', Georgia, serif",
  fontWeight: 900,
  letterSpacing: "0.16em",
  lineHeight: 1.25,
  margin: 0,
  fontSize: "clamp(18px, 3.6vw, 34px)",
  background: "linear-gradient(180deg, #fcee9f 0%, #f0d675 40%, #c8983c 75%, #8a6420 100%)",
  WebkitBackgroundClip: "text",
  backgroundClip: "text",
  color: "transparent",
  WebkitTextFillColor: "transparent",
  filter: "drop-shadow(0 2px 2px rgba(0,0,0,0.8))",
};

/** first real paragraph of a recap, cut at a sentence near 260 characters */
function excerpt(body: string): string {
  const para = body.split(/\n{2,}/).map((b) => b.trim()).find((b) => b && !b.startsWith("*") && !b.startsWith("#")) ?? "";
  const text = para.replace(/\*\*?/g, "");
  if (text.length <= 280) return text;
  const cut = text.slice(0, 280);
  const end = Math.max(cut.lastIndexOf(". "), cut.lastIndexOf("! "), cut.lastIndexOf("? "));
  return end > 120 ? cut.slice(0, end + 1) : cut.replace(/\s+\S*$/, "") + "…";
}

export default function Home() {
  const { standings, schedule, finalSeasons } = getLeagueData();
  const season = Math.max(...standings.map((s) => s.season));
  const recap = listRecaps()[0];

  // standings: wins, then points
  const table = standings
    .filter((s) => s.season === season)
    .sort((a, b) => b.h2h_wins - a.h2h_wins || b.points_for - a.points_for);

  // the last week where every game has a result
  const games = schedule.filter((g) => g.season === season);
  const weeks = [...new Set(games.map((g) => g.week))].sort((a, b) => b - a);
  const week = weeks.find((w) => games.filter((g) => g.week === w).every((g) => g.result));
  const results = games
    .filter((g) => g.week === week && g.franchise_id < g.opponent_id)
    .map((g) => {
      const homeWon = g.franchise_score >= g.opponent_score;
      return homeWon
        ? { w: g.team_owner, ws: g.franchise_score, l: g.opponent_owner, ls: g.opponent_score }
        : { w: g.opponent_owner, ws: g.opponent_score, l: g.team_owner, ls: g.franchise_score };
    })
    .sort((a, b) => a.ws - a.ls - (b.ws - b.ls));

  // reigning champion: rank 1 of the latest completed season
  const champSeason = Math.max(...[...finalSeasons]);
  const champ = standings.find((s) => s.season === champSeason && s.league_rank === 1);
  const bust = champ
    ? findPhoto(`${champ.owner.toLowerCase()}_bust2`, `${champ.owner.toLowerCase()}_bust`, `${champ.owner.toLowerCase()}_headshot`)
    : null;

  return (
    <>
      <section
        className="dark-section"
        style={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
          textAlign: "center",
          gap: 14,
          padding: "clamp(22px, 5vw, 44px) 16px clamp(20px, 4vw, 36px)",
          background:
            "radial-gradient(ellipse at 50% 30%, rgba(212,168,75,0.14), transparent 55%), radial-gradient(ellipse at center, #13233f 0%, #07101f 70%, #02060d 100%)",
          marginBottom: 18,
        }}
      >
        <Crest size={170} full />
        <h1 style={wordmark}>
          Groupies Fantasy <span style={{ whiteSpace: "nowrap" }}>Fuckboi League</span>
        </h1>
      </section>

      <div className="row g-3">
        <div className="col-lg-7">
          {recap && (
            <Link href={`/recaps/${recap.slug}`} className="card gffl-home-recap text-decoration-none">
              <div className="card-body">
                <div className="gffl-kicker">
                  Latest recap · {recap.season} Week {recap.week}
                </div>
                <div className="gffl-home-recap-title">{recap.title.replace(/^Week \d+:\s*/, "")}</div>
                <p className="mb-2">{excerpt(recap.body)}</p>
                <span className="gffl-home-more">Read the recap →</span>
              </div>
            </Link>
          )}

          {week != null && (
            <div className="card">
              <div className="card-header d-flex justify-content-between">
                <span>Week {week} results</span>
                <Link href={`/matchups?season=${season}`} className="gffl-home-link">
                  All matchups
                </Link>
              </div>
              <div className="card-body d-flex flex-column gap-2">
                {results.map((r) => (
                  <div key={r.w + r.l} className="gffl-home-game">
                    <span className="w">
                      <span>{r.w}</span>
                      <b>{fmt(r.ws, 2)}</b>
                    </span>
                    <span className="l">
                      <span>{r.l}</span>
                      <b>{fmt(r.ls, 2)}</b>
                    </span>
                    <span className="m">+{fmt(r.ws - r.ls, 2)}</span>
                  </div>
                ))}
              </div>
            </div>
          )}
        </div>

        <div className="col-lg-5">
          {champ && (
            <Link href="/hall-of-fame" className="card gffl-home-champ text-decoration-none">
              {bust && <img src={bust} alt={`${champ.owner}, ${champSeason} champion`} />}
              <div>
                <div className="gffl-kicker" style={{ color: "#d4a84b" }}>
                  Reigning champion
                </div>
                <div className="gffl-home-champ-name">{champ.owner}</div>
                <div className="gffl-home-champ-yr">{champSeason} GFFL Champion</div>
              </div>
            </Link>
          )}

          <div className="card">
            <div className="card-header d-flex justify-content-between">
              <span>{season} standings</span>
              <Link href={`/standings?season=${season}`} className="gffl-home-link">
                Full standings
              </Link>
            </div>
            <div className="card-body py-1">
              <table className="table table-sm gffl-table gffl-home-table mb-0">
                <thead>
                  <tr>
                    <th style={{ width: 28 }}>#</th>
                    <th>Owner</th>
                    <th className="text-end">W-L</th>
                    <th className="text-end">PF</th>
                  </tr>
                </thead>
                <tbody>
                  {table.map((s, i) => (
                    <tr key={s.owner}>
                      <td className="text-muted">{i + 1}</td>
                      <td className="fw-semibold">{s.owner}</td>
                      <td className="text-end">
                        {s.h2h_wins}-{s.h2h_losses}
                        {s.h2h_ties ? `-${s.h2h_ties}` : ""}
                      </td>
                      <td className="text-end">{fmt(s.points_for, 2)}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
