// The facts for a weekly GFFL recap (RECAP_GUIDE.md): every matchup with
// both lineups, the bench, the lineup that would have scored the most and
// whether it flips the result, draft capital, renames, and the week's
// superlatives computed across ALL matchups (so "closest" means closest).
//
//   npx tsx scripts/recap-facts.ts <season> <week> [--json out.json]
//
// Reads src/data (the daily refresh). A week appears there only once every
// matchup is final; if it's missing, dispatch update-data.yml and wait.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { BENCH_SLOTS, getLeagueData } from "../src/lib/data";

const [season, week] = process.argv.slice(2, 4).map(Number);
const jsonOut = process.argv.includes("--json") ? process.argv[process.argv.indexOf("--json") + 1] : null;
if (!season || !week) {
  console.error("usage: npx tsx scripts/recap-facts.ts <season> <week> [--json out.json]");
  process.exit(2);
}

const D = getLeagueData();
type S = (typeof D.starters)[number];
const rows = D.starters.filter((r) => r.season === season && r.week === week);
if (!rows.length) {
  console.error(`No lineups for ${season} week ${week} yet: the week isn't final in src/data (dispatch update-data.yml).`);
  process.exit(3);
}
const r2 = (n: number) => Math.round(n * 100) / 100;
const owner = (fid: number) => D.ownerByFranchise.get(`${season}|${fid}`) ?? `#${fid}`;

// which lineup slots a player can fill
const SLOT_OK: Record<string, (pos: string) => boolean> = {
  QB: (p) => p === "QB",
  RB: (p) => p === "RB",
  WR: (p) => p === "WR",
  TE: (p) => p === "TE",
  "RB/WR/TE": (p) => p === "RB" || p === "WR" || p === "TE",
  "RB/WR": (p) => p === "RB" || p === "WR",
  "WR/TE": (p) => p === "WR" || p === "TE",
  OP: (p) => ["QB", "RB", "WR", "TE"].includes(p),
  K: (p) => p === "K",
  DST: (p) => p === "D/ST" || p === "DST",
  "D/ST": (p) => p === "D/ST" || p === "DST",
};
const fits = (slot: string, pos: string) => (SLOT_OK[slot] ?? ((p: string) => p === slot))(pos);

/** the most points this roster could have started (exact: slots filled most-constrained first, by search) */
function optimal(starters: S[], bench: S[]) {
  const slots = starters.map((s) => s.lineup_slot);
  const pool = [...starters, ...bench];
  const order = slots.map((s, i) => [s, i] as const).sort((a, b) => pool.filter((p) => fits(a[0], p.pos)).length - pool.filter((p) => fits(b[0], p.pos)).length);
  let best = -1, pick: S[] = [];
  const used = new Set<number>();
  const cur: S[] = new Array(slots.length);
  const go = (k: number, sum: number) => {
    if (k === order.length) { if (sum > best) { best = sum; pick = [...cur]; } return; }
    const [slot, idx] = order[k];
    let any = false;
    for (let j = 0; j < pool.length; j++) {
      if (used.has(j) || !fits(slot, pool[j].pos)) continue;
      any = true; used.add(j); cur[idx] = pool[j];
      go(k + 1, sum + pool[j].player_score);
      used.delete(j);
    }
    if (!any) go(k + 1, sum); // an empty slot
  };
  go(0, 0);
  return { points: r2(best), lineup: pick.map((p, i) => ({ slot: slots[i], name: p?.player_name ?? "(empty)", pts: p?.player_score ?? 0 })) };
}

const draft = new Map(D.drafts.filter((d) => d.season === season).map((d) => [`${d.franchise_id}|${d.player_name}`, d]));
const draftOf = (fid: number, name: string) => {
  const d = draft.get(`${fid}|${name}`);
  return d ? `${d.is_keeper ? "keeper, " : ""}rd ${d.round} (#${d.overall})` : "not drafted by them (waivers/trade)";
};
// last week's names: the data keeps only each team's CURRENT name, so read
// them off last week's recap headings ("## Name 98.32 — Name 85.34"),
// matched to franchises by last week's scores
const lastNames = new Map<number, string>();
const prevRecap = `src/data/recaps/${season}-week-${String(week - 1).padStart(2, "0")}.md`;
if (week > 1 && existsSync(prevRecap)) {
  const prevScores = D.schedule.filter((m) => m.season === season && m.week === week - 1);
  for (const m of readFileSync(prevRecap, "utf8").matchAll(/^## (.+?) ([\d.]+) — (.+?) ([\d.]+)\s*$/gm)) {
    for (const [nm, sc] of [[m[1], m[2]], [m[3], m[4]]]) {
      const hit = prevScores.filter((x) => Math.abs(x.franchise_score - Number(sc)) < 0.006);
      if (hit.length === 1) lastNames.set(hit[0].franchise_id, nm.trim());
    }
  }
}
const nameThen = (fid: number) => lastNames.get(fid);

// records through this week
const sched = D.schedule.filter((m) => m.season === season && m.week <= week);
const rec = (fid: number) => {
  const g = sched.filter((m) => m.franchise_id === fid);
  return `${g.filter((m) => m.result === "W").length}-${g.filter((m) => m.result === "L").length}${g.some((m) => m.result === "T") ? `-${g.filter((m) => m.result === "T").length}` : ""}`;
};
const pf = (fid: number) => r2(sched.filter((m) => m.franchise_id === fid).reduce((a, m) => a + m.franchise_score, 0));

const teams = [...new Set(rows.map((r) => r.franchise_id))];
const side = (fid: number) => {
  const mine = rows.filter((r) => r.franchise_id === fid);
  const starters = mine.filter((r) => !BENCH_SLOTS.has(r.lineup_slot));
  const bench = mine.filter((r) => r.lineup_slot === "BE");
  const ir = mine.filter((r) => r.lineup_slot === "IR");
  const score = mine[0].franchise_score;
  const opt = optimal(starters, bench);
  // single bench swaps that would have gained points (eligible only)
  const swaps = bench.flatMap((b) => starters.filter((s) => fits(s.lineup_slot, b.pos) && b.player_score > s.player_score)
    .map((s) => ({ in: b.player_name, out: s.player_name, slot: s.lineup_slot, gain: r2(b.player_score - s.player_score) })))
    .sort((a, b) => b.gain - a.gain);
  return {
    fid, owner: owner(fid), name: mine[0].franchise_name, prevName: nameThen(fid),
    score, record: rec(fid), pf: pf(fid),
    starters: starters.map((s) => ({ slot: s.lineup_slot, name: s.player_name, pos: s.pos, nfl: s.team, pts: r2(s.player_score), proj: r2(s.projected_score), draft: draftOf(fid, s.player_name) })),
    bench: bench.map((s) => ({ name: s.player_name, pos: s.pos, nfl: s.team, pts: r2(s.player_score), draft: draftOf(fid, s.player_name) })).sort((a, b) => b.pts - a.pts),
    ir: ir.map((s) => s.player_name),
    benchPts: r2(bench.reduce((a, b) => a + b.player_score, 0)),
    optimal: opt.points, leftOnBench: r2(opt.points - score), bestSwaps: swaps.slice(0, 3),
  };
};

const seen = new Set<number>();
const games = D.schedule.filter((m) => m.season === season && m.week === week).flatMap((m) => {
  if (seen.has(m.franchise_id) || !teams.includes(m.franchise_id)) return [];
  seen.add(m.franchise_id); seen.add(m.opponent_id);
  const a = side(m.franchise_id), b = side(m.opponent_id);
  const [w, l] = a.score >= b.score ? [a, b] : [b, a];
  return [{ winner: w, loser: l, margin: r2(w.score - l.score),
    // would the loser's best lineup have won? (against the winner's actual score)
    loserBestLineupWins: l.optimal > w.score }];
});

const all = games.flatMap((g) => [g.winner, g.loser]);
const players = all.flatMap((t) => t.starters.map((s) => ({ ...s, team: t.name })));
const benchAll = all.flatMap((t) => t.bench.map((s) => ({ ...s, team: t.name })));
const by = <T,>(xs: T[], f: (x: T) => number) => [...xs].sort((a, b) => f(b) - f(a));
const supers = {
  highestTeam: by(all, (t) => t.score)[0], lowestTeam: by(all, (t) => -t.score)[0],
  closest: by(games, (g) => -g.margin)[0], biggestBlowout: by(games, (g) => g.margin)[0],
  topPlayers: by(players, (p) => p.pts).slice(0, 5), worstStarters: by(players.filter((p) => !["K", "D/ST", "DST"].includes(p.pos)), (p) => -p.pts).slice(0, 5),
  topBench: by(benchAll, (p) => p.pts).slice(0, 5),
  mostLeftOnBench: by(all, (t) => t.leftOnBench).slice(0, 3).map((t) => ({ team: t.name, left: t.leftOnBench })),
  flippedByBench: games.filter((g) => g.loserBestLineupWins).map((g) => ({ loser: g.loser.name, best: g.loser.optimal, winnerScored: g.winner.score })),
  renames: all.filter((t) => t.prevName && t.prevName !== t.name).map((t) => `${t.prevName} -> ${t.name}`),
  // teams last week's recap couldn't be matched to (check those names by hand)
  unmatched: week > 1 ? all.filter((t) => !t.prevName).map((t) => t.name) : [],
};
const standings = by(all, (t) => Number(t.record.split("-")[0]) * 10000 + t.pf).map((t) => ({ team: t.name, owner: t.owner, record: t.record, pf: t.pf }));

// ── print ──
const line = (s: { slot?: string; name: string; pos: string; nfl: string; pts: number; proj?: number; draft: string }) =>
  `      ${(s.slot ?? "BE").padEnd(9)}${s.name} (${s.pos}, ${s.nfl}) ${s.pts}${s.proj != null ? ` [proj ${s.proj}]` : ""} — ${s.draft}`;
console.log(`GFFL ${season} week ${week}: ${games.length} matchups\n`);
for (const g of by(games, (x) => -x.margin)) {
  console.log(`## ${g.winner.name} ${g.winner.score} — ${g.loser.name} ${g.loser.score}   (margin ${g.margin})`);
  for (const t of [g.winner, g.loser]) {
    console.log(`   ${t.name} [${t.owner}] ${t.record}, PF ${t.pf}${t.prevName && t.prevName !== t.name ? `  (renamed from "${t.prevName}")` : ""}`);
    t.starters.forEach((s) => console.log(line(s)));
    t.bench.forEach((s) => console.log(line(s)));
    if (t.ir.length) console.log(`      IR: ${t.ir.join(", ")}`);
    console.log(`      best possible lineup ${t.optimal} (left ${t.leftOnBench} on the bench)${t.bestSwaps.length ? `; best swaps: ${t.bestSwaps.map((x) => `${x.in} for ${x.out} (${x.slot}) +${x.gain}`).join("; ")}` : ""}`);
  }
  if (g.loserBestLineupWins) console.log(`   ** ${g.loser.name}'s best lineup (${g.loser.optimal}) would have beaten ${g.winner.score}`);
  console.log("");
}
console.log("## Superlatives (across all matchups)");
console.log(`   highest team: ${supers.highestTeam.name} ${supers.highestTeam.score}; lowest: ${supers.lowestTeam.name} ${supers.lowestTeam.score}`);
console.log(`   closest: ${supers.closest.winner.name} over ${supers.closest.loser.name} by ${supers.closest.margin}; biggest blowout: ${supers.biggestBlowout.winner.name} over ${supers.biggestBlowout.loser.name} by ${supers.biggestBlowout.margin}`);
console.log(`   top players: ${supers.topPlayers.map((p) => `${p.name} ${p.pts} (${p.team})`).join("; ")}`);
console.log(`   worst non-K/DST starters: ${supers.worstStarters.map((p) => `${p.name} ${p.pts} (${p.team})`).join("; ")}`);
console.log(`   top bench: ${supers.topBench.map((p) => `${p.name} ${p.pts} (${p.team})`).join("; ")}`);
console.log(`   most left on bench: ${supers.mostLeftOnBench.map((x) => `${x.team} ${x.left}`).join("; ")}`);
console.log(`   results a better lineup would have flipped: ${supers.flippedByBench.map((x) => `${x.loser} (best ${x.best} vs ${x.winnerScored})`).join("; ") || "none"}`);
console.log(`   renames since last week's recap: ${supers.renames.join("; ") || "none"}${supers.unmatched.length ? ` (couldn't match ${supers.unmatched.join(", ")}: check by hand)` : ""}`);
console.log("\n## Standings (after this week)");
standings.forEach((s, i) => console.log(`   ${i + 1}. ${s.team} [${s.owner}] ${s.record}, PF ${s.pf}`));
if (jsonOut) writeFileSync(jsonOut, JSON.stringify({ season, week, games, superlatives: supers, standings }, null, 1));
