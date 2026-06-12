// Achievement definitions ported verbatim from app.R (achievements list, lines 2654-2739).
// FontAwesome is not loaded in the Next.js build, so each original shiny icon("...")
// name is mapped to an equivalent Unicode emoji/symbol (original FA name kept in `fa`).

export interface AchievementDef {
  id: string;
  name: string;
  desc: string;
  /** Original FontAwesome icon name from app.R */
  fa: string;
  /** Unicode replacement rendered in place of the FA icon */
  icon: string;
}

export const ACHIEVEMENTS: AchievementDef[] = [
  { id: "first_win", name: "First Blood", desc: "Win a regular season game", fa: "medal", icon: "\u{1F3C5}" },
  { id: "champion", name: "Champion", desc: "Win a championship", fa: "trophy", icon: "\u{1F3C6}" },
  { id: "dynasty", name: "Dynasty", desc: "Win 2+ championships", fa: "crown", icon: "\u{1F451}" },
  { id: "title_game", name: "Finalist", desc: "Make the championship game", fa: "flag-checkered", icon: "\u{1F3C1}" },
  { id: "one_seed", name: "Top Dog", desc: "Earn the #1 seed", fa: "medal", icon: "\u{1F3C5}" },
  { id: "playoff_bound", name: "Playoff Bound", desc: "Make the playoffs", fa: "award", icon: "\u{1F396}\u{FE0F}" },
  { id: "mvp", name: "MVP", desc: "Best regular season record", fa: "star", icon: "⭐" },
  { id: "opoy", name: "OPOY", desc: "Most regular season points in a season", fa: "fire", icon: "\u{1F525}" },
  { id: "century", name: "Century Club", desc: "Score 100+ in a single week", fa: "100", icon: "\u{1F4AF}" },
  { id: "one_fifty", name: "Elite Performer", desc: "Score 150+ in a single week", fa: "star", icon: "⭐" },
  { id: "two_hundred", name: "Unstoppable", desc: "Score 200+ in a single week", fa: "bolt", icon: "⚡" },
  { id: "winning_record", name: "Winner", desc: "All-time winning record", fa: "chart-line", icon: "\u{1F4C8}" },
  { id: "veteran", name: "Veteran", desc: "Play 5+ seasons", fa: "shield", icon: "\u{1F6E1}\u{FE0F}" },
  { id: "decade", name: "Decade of Play", desc: "Play 9+ seasons", fa: "hourglass-end", icon: "⌛" },
  { id: "undefeated", name: "Perfect Season", desc: "Go undefeated in regular season", fa: "certificate", icon: "\u{1F4DC}" },
  { id: "sacko", name: "Sacko Holder", desc: "Finish last in the regular season", fa: "poo", icon: "\u{1F4A9}" },
  { id: "double_sacko", name: "Repeat Offender", desc: "Win the Sacko 2+ times", fa: "dumpster-fire", icon: "\u{1F5D1}\u{FE0F}" },
  { id: "runner_up", name: "So Close", desc: "Finish 2nd in the championship", fa: "medal", icon: "\u{1F948}" },
  { id: "hundred_wins", name: "Centurion", desc: "Win 100+ career games", fa: "check-double", icon: "✅" },
  { id: "fourteen_k", name: "14K Club", desc: "Score 14,000+ career points", fa: "coins", icon: "\u{1FA99}" },
  { id: "big_blowout", name: "Demolition", desc: "Win a game by 75+ points", fa: "bomb", icon: "\u{1F4A3}" },
  { id: "homer", name: "Homer", desc: "Draft 3+ players from your favorite team", fa: "home", icon: "\u{1F3E0}" },
  { id: "double_zero", name: "00 Agent", desc: "Have two started players score 0 pts in a week", fa: "user-ninja", icon: "\u{1F977}" },
  { id: "game_of_inches", name: "Game of Inches", desc: "Win a matchup by 1 point or less", fa: "ruler", icon: "\u{1F4CF}" },
  { id: "gauntlet", name: "The Gauntlet", desc: "Defeat every team at least once in a season", fa: "fist-raised", icon: "✊" },
  { id: "bad_beat", name: "Bad Beat", desc: "Lose a week as 2nd-highest scoring team", fa: "heart-crack", icon: "\u{1F494}" },
  { id: "really_bad_beat", name: "Really Bad Beat", desc: "Miss playoffs as 2nd-highest PF in reg season", fa: "skull", icon: "\u{1F480}" },
  { id: "heating_up", name: "Heating Up", desc: "Win 5 weeks in a row (same season)", fa: "fire-flame-curved", icon: "\u{1F525}" },
  { id: "on_fire", name: "On Fire", desc: "Win 10 weeks in a row (same season)", fa: "fire-flame-simple", icon: "\u{1F525}" },
  { id: "soft_schedule", name: "Soft Schedule", desc: "Lowest points against in a season", fa: "feather", icon: "\u{1FAB6}" },
  { id: "ninja", name: "Ninja", desc: "Sneak into playoffs with bottom 3 PF", fa: "mask", icon: "\u{1F3AD}" },
  { id: "blowout", name: "Blowout", desc: "Win a week by 50+ points", fa: "explosion", icon: "\u{1F4A5}" },
  { id: "everyone_eats", name: "Everyone Eats", desc: "Every starter scores 6+ pts in a week", fa: "utensils", icon: "\u{1F374}" },
  { id: "double_ds", name: "Double D's", desc: "Every starter scores 10+ pts (incl DST/K)", fa: "dice-two", icon: "\u{1F3B2}" },
  { id: "why", name: "Why?", desc: "Have 3+ kickers on roster at once", fa: "question", icon: "❓" },
  { id: "ban_kickers", name: "Ban Kickers", desc: "Kicker is your highest scoring starter", fa: "futbol", icon: "⚽" },
  { id: "wins_champs", name: "Wins Championships", desc: "Defense is your highest scoring starter", fa: "shield-halved", icon: "\u{1F6E1}\u{FE0F}" },
  { id: "kingslayer", name: "Kingslayer", desc: "End the win streak of a team that started 5-0+", fa: "khanda", icon: "⚔\u{FE0F}" },
  { id: "rock_bottom", name: "Rock Bottom", desc: "Have the lowest scoring week of an entire season", fa: "arrow-down", icon: "⬇\u{FE0F}" },
  { id: "top_qb", name: "Top QB", desc: "Highest scoring single QB starter in a season", fa: "football", icon: "\u{1F3C8}" },
  { id: "top_rb", name: "Top RB", desc: "Highest scoring single RB starter in a season", fa: "person-running", icon: "\u{1F3C3}" },
  { id: "top_wr", name: "Top WR", desc: "Highest scoring single WR starter in a season", fa: "hand", icon: "✋" },
  { id: "top_te", name: "Top TE", desc: "Highest scoring single TE starter in a season", fa: "user-large", icon: "\u{1F464}" },
  { id: "top_k", name: "Top K", desc: "Highest scoring single K starter in a season", fa: "futbol", icon: "⚽" },
  { id: "top_dst", name: "Top D/ST", desc: "Highest scoring single D/ST starter in a season", fa: "shield", icon: "\u{1F6E1}\u{FE0F}" },
  { id: "best_qb", name: "Best QB Group", desc: "Most total QB starter points in a season", fa: "football", icon: "\u{1F3C8}" },
  { id: "best_rb", name: "Best RB Group", desc: "Most total RB starter points in a season", fa: "person-running", icon: "\u{1F3C3}" },
  { id: "best_wr", name: "Best WR Group", desc: "Most total WR starter points in a season", fa: "hand", icon: "✋" },
  { id: "best_te", name: "Best TE Group", desc: "Most total TE starter points in a season", fa: "user-large", icon: "\u{1F464}" },
  { id: "best_k", name: "Best K Group", desc: "Most total K starter points in a season", fa: "futbol", icon: "⚽" },
  { id: "best_dst", name: "Best D/ST Group", desc: "Most total D/ST starter points in a season", fa: "shield", icon: "\u{1F6E1}\u{FE0F}" },
  { id: "skin", name: "By the Skin of his...", desc: "Tied Sacko's record but finished higher on PF", fa: "bone", icon: "\u{1F9B4}" },
  { id: "catch_em", name: "Gotta Catch 'em All", desc: "Roster 50+ different players in a season", fa: "layer-group", icon: "\u{1F5C2}\u{FE0F}" },
  { id: "pokedex_200", name: "Pokedex 200", desc: "Roster 200+ different players lifetime", fa: "book", icon: "\u{1F4D5}" },
  { id: "pokedex_300", name: "Pokedex 300", desc: "Roster 300+ different players lifetime", fa: "book-open", icon: "\u{1F4D6}" },
  { id: "pokedex_400", name: "Pokedex 400", desc: "Roster 400+ different players lifetime", fa: "book-atlas", icon: "\u{1F4D7}" },
  { id: "pokedex_500", name: "Pokedex 500", desc: "Roster 500+ different players lifetime", fa: "books-medical", icon: "\u{1F4DA}" },
  { id: "mr_relevant", name: "Mr. Relevant", desc: "Start a last-round pick 10+ weeks in a season", fa: "thumbs-up", icon: "\u{1F44D}" },
  { id: "dream_work", name: "Makes the Dream Work", desc: "Start 4 players from same NFL team in a week", fa: "people-group", icon: "\u{1F465}" },
  { id: "repeat", name: "Repeat", desc: "Back-to-back championships", fa: "rotate-right", icon: "\u{1F501}" },
  { id: "threepeat", name: "Threepeat", desc: "Three championships in a row", fa: "infinity", icon: "♾\u{FE0F}" },
  { id: "singled_out", name: "Singled Out", desc: "All starters scored single digits in a week", fa: "1", icon: "1\u{FE0F}⃣" },
  { id: "benchwarmers", name: "Benchwarmers", desc: "Bench outscored starters in a week", fa: "couch", icon: "\u{1F6CB}\u{FE0F}" },
  { id: "negative", name: "Negative", desc: "Negative score from a non-K/DST starter", fa: "minus", icon: "➖" },
  { id: "double_negative", name: "Double-Negative", desc: "Two negative scores from non-K/DST starters in a week", fa: "circle-xmark", icon: "❌" },
  { id: "consistency", name: "Consistency", desc: "Scored within 10 pts for 3 straight weeks", fa: "equals", icon: "\u{1F7F0}" },
  { id: "is_key", name: "Is Key", desc: "Scored within 10 pts for 5 straight weeks", fa: "key", icon: "\u{1F511}" },
  { id: "perfectly_avg", name: "Perfectly Average", desc: "Never highest or lowest scoring team in a season", fa: "gauge-simple", icon: "⏲\u{FE0F}" },
  { id: "boomer", name: "Boomer", desc: "Highest scoring week of an entire season", fa: "rocket", icon: "\u{1F680}" },
  { id: "boomer_boomer", name: "Boomer Boomer", desc: "Highest scoring week of a season in 3 different years", fa: "rocket", icon: "\u{1F680}" },
  { id: "late_bloomer", name: "Late Bloomer", desc: "Start the season 0-3 and make the playoffs", fa: "seedling", icon: "\u{1F331}" },
  { id: "ran_out_of_gas", name: "Ran Out of Gas", desc: "Start the season 3-0 and miss the playoffs", fa: "gas-pump", icon: "⛽" },
  { id: "perfectly_wrong", name: "Perfectly Wrong", desc: "Start the lowest possible scoring lineup (every starter had a better bench option)", fa: "thumbs-down", icon: "\u{1F44E}" },
  { id: "dynasty_promised", name: "The Dynasty That Was Promised", desc: "Make the playoffs 5 times in 6 years", fa: "dragon", icon: "\u{1F409}" },
  { id: "bridesmaid", name: "Always the Bridesmaid", desc: "Finish 2nd three times without ever winning", fa: "ring", icon: "\u{1F48D}" },
  { id: "never_left", name: "Like We Never Left", desc: "Make playoffs 3 straight years after missing 3 straight", fa: "right-to-bracket", icon: "\u{1F6AA}" },
  { id: "never_looked_back", name: "Never Looked Back", desc: "Spent an entire season in 1st place after week 1", fa: "eye-slash", icon: "\u{1F648}" },
  { id: "points_machine", name: "Points Machine", desc: "Lead the league in total points in 3 different seasons", fa: "gauge-high", icon: "⏱\u{FE0F}" },
  { id: "streaming_savant", name: "Streaming Savant", desc: "Win 3 straight weeks using a different QB each time", fa: "rotate", icon: "\u{1F504}" },
  { id: "escape_artist", name: "Escape Artist", desc: "Avoid finishing last despite entering the final week in last place", fa: "person-running", icon: "\u{1F3C3}" },
  { id: "villain_arc", name: "Villain Arc Complete", desc: "Win a championship after being last place the previous year", fa: "hat-wizard", icon: "\u{1F9D9}" },
  { id: "historic_futility", name: "Historic Futility", desc: "Miss playoffs 5 straight seasons", fa: "face-sad-tear", icon: "\u{1F622}" },
  { id: "all_pain", name: "All Pain, No Gain", desc: "Lead the league in points against for a season", fa: "burst", icon: "\u{1F4A2}" },
  { id: "overcoming_adversity", name: "Overcoming Adversity", desc: "Make the playoffs despite having the most points against", fa: "mountain", icon: "⛰\u{FE0F}" },
];

// Team-to-owner favorite team map for Homer achievement (app.R lines 2748-2752)
export const FAVORITE_TEAMS: Record<string, string> = {
  Tom: "NYJ", RJ: "NYJ", Matt: "NYJ",
  Kerley: "NYG", Jack: "NYG", Mike: "NYG",
  Faz: "PIT", Alex: "PIT", Harry: "PIT", Connor: "PIT",
};

// Negative/embarrassing achievements get 500G; positive ones scale by rarity
// (app.R lines 4063-4066)
export const NEGATIVE_IDS = new Set([
  "sacko", "double_sacko", "double_zero", "bad_beat", "really_bad_beat",
  "rock_bottom", "why", "ban_kickers", "singled_out", "benchwarmers",
  "negative", "double_negative", "perfectly_wrong", "ran_out_of_gas",
  "historic_futility", "all_pain", "perfectly_avg",
]);
