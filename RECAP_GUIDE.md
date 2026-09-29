# Writing the weekly GFFL Recap

Game-by-game, in the voice of the original Groupies blog (2019–2024; the
archive lives in the auction repo at `nfl_auction/data/blog/posts.json`).
Published Tuesday after Monday Night Football once the daily refresh has
recorded the week as final (dispatch `update-data.yml` if it hasn't run).

## 1. Facts

- **Lineups, scores, bench**: `src/data/starters.json` (week rows appear only
  once every matchup is final), `schedule.json`, `drafts.json` (round,
  overall, keeper). Franchise names come from the *latest* data, so compare
  against last week's recap to catch renames. They make good material.
- **Why a player boomed or busted**: `python3 ../nfl_auction/scripts/game_stories.py
  --season <S> --week <W>` prints every NFL game's score path, win-probability
  swings, outlier lines, QB stints and the week's RotoWire injury news. A
  2.5-point first-rounder reads very differently when his team scored 7, or
  when he left in the first quarter with a torn ACL.
- **Projections**: after the fact, ESPN often reports a player's projection
  as his final score. Quote a projection only when it clearly differs from
  the final.

## 2. Shape

```
*epigraph (a new one every week)*
Intro: 3–5 sentences, the week's theme. No re-explaining the bench or draft-capital rules.

## Winner score — Loser score        (one per matchup, most interesting first)
Winner paragraph, then loser paragraph. How the game went (close? decided
Monday night? decided on a bench?), the studs with the NFL context behind
them, the busts with the reason, and bench decisions only when they matter
(eligible swaps only; say when a swap would have flipped the result).

## Superlatives
**Award:** ... — keep Highest/Lowest Scoring Team and Highest Scoring Player;
invent the rest from the week.

## Standings
---
Sign-off. *Message From the Commissioner: "one or two lines."*
```

About 1,500–2,000 words. The original ran about 1,300.

## 3. Voice and freshness

- Tell the matchup's story: close, lopsided, decided on Monday night, decided on a bench.
- Give fantasy scores their NFL cause (injury, backup QB, blowout script,
  garbage time, a walk-off field goal).
- Keep running storylines alive (a repeat bench mistake, a team rename, a
  scolded player's response), but don't reuse the same *jokes* or phrases.
  Grep the last two recaps before publishing ("the log", "roast", "editorial
  policy" were overused in weeks 1–2).
- The recapper is also a manager. Self-deprecation beats gloating.

## 4. Editing a posted recap

Every recap page has **✎ Edit this recap** at the bottom. It opens the
markdown file in GitHub's editor (you must be signed in to GitHub with
write access to barter7/GFFL). Edit, use the Preview tab if you like,
then **Commit changes** to `main`. Vercel redeploys and the change is live
in about two minutes. Every edit is a commit, so nothing is ever lost:
the file's History shows each version and any of them can be restored.
