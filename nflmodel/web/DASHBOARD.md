# Props Dashboard — product spec

Target: the owner's 2024 Power BI player board, rebuilt for the web
the way PropSZN serves its K's tab (R exporter -> static JSON ->
client-side app). One player selected at a time; everything about
that player's props on one screen.

## Screens/blocks (from the 2024 board)
1. Player picker (all skill players + QBs) with headshot, pos, team
2. Matchup header: opponent, home/away, spread, total
3. Prop board: one card per market with best line + best odds per
   side (book logos), and OUR projection + edge once the model layer
   lands. Markets: rush yds/att, rec yds, receptions, rush+rec,
   longest rush/rec, anytime TD; QB set: pass yds/att/TD/comp/INT
4. Opponent defense strip: EPA (ovr/rush/pass) + ranks; PFF grades
   when the owner drops exports in data/pff/
5. Weekly log table: snap%, team playcall, WP-quartile game-script
   shares, rushing block, receiving block (targets, air yards,
   unrealized AY, shares, aDOT, NGS separation), heat-cell coloring
6. Team target-share matrix (player x week, current season)
7. Weekly bar chart: stat picker, prop line / median overlay,
   season + home-away + week-range filters

## Data contract (generate_web_data.R -> data/web/)
players.json · gamelogs/{gsis_id}.json · teams/{TEAM}_{season}.json
· defense.json · meta.json · props/ (scrape_props.R, in season)
· projections/ (model layer, later)

## Status
- web/dashboard.html — working single-file prototype (data spliced
  at /*__DATA__*/; production should fetch the JSON files instead).
  Published preview: claude.ai artifact "NFL Props Workbench".
- Pending: live props (offseason), projections (model stage),
  PFF grades (owner's local exports), site integration/hosting
  decision (PropSZN web app vs standalone).
