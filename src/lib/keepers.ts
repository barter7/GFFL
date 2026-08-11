// 2026 keeper eligibility, computed from the 2025 draft and the final
// Week 17 rosters per the constitution's keeper rules (§1.3). Shared by
// the Keepers page (which renders eligibility) and the Keeper Value
// page (which prices each option) so the two can never disagree about
// who is keepable or what he costs.

import { getLeagueData, DraftRow } from "@/lib/data";

export const KEEPER_SEASON = 2025; // season being kept FROM
export const TARGET_SEASON = 2026; // season being kept INTO

// 2026 draft order. Sean and Joe are returning owners with no 2025
// roster, so they have no keeper options (rendered as N/A).
export const DRAFT_ORDER_2026 = [
  "Harry", "Jack", "Matt", "Mike", "Sean", "Tom",
  "RJ", "Kerley", "Connor", "Joe", "Faz", "Alex",
];

export interface EligibleRow {
  name: string;
  pos: string;
  round: number;
  keptLastYear: boolean;
  /** round the keep occupies in 2026 (round - 2 when kept last year) */
  cost: number;
}

export interface IneligibleRow {
  name: string;
  pos: string;
  /** tiny muted context (e.g. "2025 keeper", "via RJ", "FA") */
  note?: string;
}

export interface OwnerKeepers {
  owner: string;
  slot: number;
  eligible: EligibleRow[];
  draftedGone: IneligibleRow[];
  notDrafted: IneligibleRow[];
  na: boolean;
}

export function computeKeepers(): { byOwner: OwnerKeepers[]; finalWeek: number } {
  const { drafts, starters } = getLeagueData();

  const draft25 = drafts.filter((d) => d.season === KEEPER_SEASON);
  const finalWeek = Math.max(
    ...starters.filter((s) => s.season === KEEPER_SEASON).map((s) => s.week)
  );

  // Final rosters by owner: player_id -> {name,pos}
  const finalRoster = new Map<string, Map<number, { name: string; pos: string }>>();
  for (const s of starters) {
    if (s.season !== KEEPER_SEASON || s.week !== finalWeek) continue;
    const m = finalRoster.get(s.owner) ?? new Map();
    m.set(s.player_id, { name: s.player_name, pos: s.pos });
    finalRoster.set(s.owner, m);
  }

  // Who drafted each player in 2025 (for ineligible notes)
  const draftedBy = new Map<number, DraftRow>();
  for (const d of draft25) draftedBy.set(d.player_id, d);

  const byOwner = DRAFT_ORDER_2026.map((owner, idx): OwnerKeepers => {
    const roster = finalRoster.get(owner);
    if (!roster) {
      return { owner, slot: idx + 1, eligible: [], draftedGone: [], notDrafted: [], na: true };
    }
    const picks = draft25
      .filter((d) => d.owner === owner)
      .sort((a, b) => a.round - b.round);

    const eligible: EligibleRow[] = [];
    const draftedGone: IneligibleRow[] = [];
    const notDrafted: IneligibleRow[] = [];

    for (const p of picks) {
      const kept = p.is_keeper === true;
      if (roster.has(p.player_id)) {
        eligible.push({
          name: p.player_name,
          pos: p.pos,
          round: p.round,
          keptLastYear: kept,
          cost: kept ? p.round - 2 : p.round,
        });
      } else {
        draftedGone.push({
          name: p.player_name,
          pos: p.pos,
          note: kept ? `${KEEPER_SEASON} keeper` : undefined,
        });
      }
    }

    for (const [pid, info] of roster) {
      const d = draftedBy.get(pid);
      if (d?.owner === owner) continue;
      notDrafted.push({
        name: info.name,
        pos: info.pos,
        note: d ? `via ${d.owner}` : "FA",
      });
    }

    return { owner, slot: idx + 1, eligible, draftedGone, notDrafted, na: false };
  });

  return { byOwner, finalWeek };
}
