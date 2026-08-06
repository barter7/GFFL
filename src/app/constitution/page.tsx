// GFFL Constitution — transcribed from "The_Groupies_Fantasy_Football_
// Constitution_2026" (Google Doc, Aug 2026 revision), styled after the
// Declaration of Independence. Update this page when the league adopts a
// dated revision under Section 2.3.

import type { ReactNode } from "react";
import styles from "./styles.module.css";

export const metadata = { title: "Constitution" };

function Sec({ children }: { children: ReactNode }) {
  return <h2 className={styles.sectionHead}>{children}</h2>;
}

function Sub({ children }: { children: ReactNode }) {
  return <h3 className={styles.subHead}>{children}</h3>;
}

function InShort({ children }: { children: ReactNode }) {
  return (
    <p className={styles.inShort}>
      <strong>In short</strong> — {children}
    </p>
  );
}

function Ledger({ head, rows }: { head: string[]; rows: ReactNode[][] }) {
  return (
    <div className={styles.tableWrap}>
      <table className={styles.ledger}>
        {head.length > 0 && (
          <thead>
            <tr>
              {head.map((h, i) => (
                <th key={i}>{h}</th>
              ))}
            </tr>
          </thead>
        )}
        <tbody>
          {rows.map((r, i) => (
            <tr key={i}>
              {r.map((c, j) => (
                <td key={j}>{c}</td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

const Pending = ({ children }: { children: ReactNode }) => (
  <span className={styles.footnote}>{children}</span>
);

export default function ConstitutionPage() {
  return (
    <div className={styles.parchment}>
      <div className={styles.kicker}>THE GROUPIES</div>
      <h1 className={styles.title}>Fantasy Football Constitution</h1>
      <div className={styles.subtitle}>
        In League Assembled · Season of 2026 · Est. 2016
      </div>
      <div className={styles.ornament}>❦ ❦ ❦</div>

      <Sec>Preamble</Sec>
      <InShort>this page is the whole league on one screen. Everything after it is the detail.</InShort>

      <Sub>League at a Glance</Sub>
      <Ledger
        head={[]}
        rows={[
          [<strong key="k">Setting</strong>, <strong key="v">2026 Season</strong>],
          ["League", "The Groupies"],
          ["Platform", "ESPN Fantasy Football"],
          ["Commissioner", "Tom Mikalonis"],
          ["Teams", <>12, in a single division — Ballers and Shot Callers <Pending>*Voted to switch to 12 for 2026</Pending></>],
          ["Scoring", "Head-to-head points, 0.5 PPR"],
          ["Roster", <>18 players — 11 starters, 7 bench — plus 2 IR <Pending>*Pending vote to remove a starting player</Pending></>],
          ["Keepers", "1 per team, maximum 3 consecutive seasons"],
          ["Acquisitions", <>Rolling waiver priority, 1-day claim period (no FAAB) <Pending>*Pending vote to introduce FAAB</Pending></>],
          ["Trades", <>Unlimited, no review, players only — no draft picks <Pending>*Pending vote</Pending></>],
          ["League fee", "$100 per team — $1,200 total pot, all paid out"],
          ["Draft", "Tuesday, September 8, 2026, 8:00 PM ET — snake, 90s per pick"],
          ["Regular season", "NFL Weeks 1–15"],
          ["Playoffs", <>4 teams — semifinals Week 16, championship Week 17 <Pending>*After expanding to 12 teams, pending vote</Pending></>],
        ]}
      />

      <Sub>Order of Precedence</Sub>
      <p className={styles.dropcap}>
        This constitution governs The Groupies Fantasy Football League. Where this
        document and the ESPN platform settings conflict, <strong>this document
        controls</strong> and the commissioner will correct the platform settings to
        match. Where this document is silent, the platform settings control.
      </p>
      <p>Where both are silent, Section 2.3 tells you what happens next.</p>

      <hr className={styles.rule} />

      <Sec>Section 1 : Roster Breakdown</Sec>
      <InShort>
        18-man rosters, 11 starters, half-PPR. Every player locks at his own kickoff,
        so there is no single weekly deadline. One keeper per team, for up to three
        seasons. Trades are unlimited and process instantly, but draft picks are never
        traded. Waivers run on rolling priority with a one-day claim period.
      </InShort>

      <Sub>1.1 : Positional Breakdown</Sub>
      <p>
        <strong>18 Total Players</strong> — 11 starters and 7 bench — plus{" "}
        <strong>2 Injured Reserve (IR) slots</strong> that do not count against the 18.
      </p>
      <Ledger
        head={["Position", "Starters", "Roster Max"]}
        rows={[
          ["Quarterback (QB)", "1", "4"],
          ["Running Back (RB)", "2", "9"],
          ["Wide Receiver (WR)", "2", "9"],
          ["Tight End (TE)", "1", "3"],
          ["Running Back / Wide Receiver (RB/WR)", "1", "—"],
          ["Flex (RB / WR / TE)", "2", "—"],
          ["Team Defense / Special Teams (D/ST)", "1", "3"],
          ["Place Kicker (K)", "1", "3"],
          ["Bench (BE)", "7", "—"],
          ["Injured Reserve (IR)", "—", "2"],
        ]}
      />
      <p>
        &quot;Roster Max&quot; is the maximum number of players at that position a team may
        carry at any one time. These maximums are enforced by the platform.
      </p>
      <p>
        An IR slot may only hold a player carrying an NFL designation the platform
        recognizes as IR-eligible. When such a player is activated he must be moved off
        IR before he can be started; if the roster is full, a corresponding roster move
        must be made first. IR slot rules defer to ESPN&apos;s default settings, which
        don&apos;t allow you to add a new player or trade while a healthy player occupies
        one of your IR slots. Lineups can still be edited.
      </p>
      <p>
        The player universe is the NFL. There is no individual defensive player scoring
        — team defense/special teams is used instead.
      </p>

      <Sub>1.2 : Lineups &amp; Lock Times</Sub>
      <p>
        Players <strong>lock individually at the scheduled kickoff of their own game</strong>.
        There is no single weekly lineup deadline.
      </p>
      <p>
        The practical effect: an owner may move any player who has not yet kicked off,
        at any point during the week, including after Thursday night and after the
        early Sunday window. A player whose game has kicked off cannot be moved into or
        out of the starting lineup, whether he scored four touchdowns or none.
      </p>
      <p>
        Lineup Protection is <strong>off</strong>. The platform will not swap out an
        inactive or injured starter. Fielding a legal, active lineup is entirely the
        owner&apos;s responsibility, and no points are awarded for players left on the
        bench.
      </p>
      <p>
        The commissioner will not retroactively adjust a lineup or a final score for
        any reason other than a documented platform failure affecting the whole league.
      </p>
      <p>
        It is the responsibility of each owner to stay active throughout the season and
        field a valid lineup every week. Starting players on BYE, players on IR, or
        leaving a lineup spot empty is against the spirit of the league and subject to
        penalty. Current rules require a beer chug to be recorded for each infraction,
        further voting required if we want to enforce additional penalties.
      </p>
      <p>Beer chug rules also apply to healthy players who score 0 or less in a game.</p>

      <Sub>1.3 : Keepers</Sub>
      <p>
        Before each season you will have the option to retain <strong>1 player</strong>{" "}
        from your previous season&apos;s roster. You are not required to keep anyone.
      </p>
      <p>Keeper Rules:</p>
      <ul>
        <li>
          <strong>Eligible keepers meet the following criteria</strong>
          <ul>
            <li>You drafted the player the previous season</li>
            <li>
              The player ended the previous season on your roster* (
              <em><strong>The Christian Kirk Rule</strong></em>)
            </li>
            <li>You have not kept the player for the last 3 consecutive seasons</li>
          </ul>
        </li>
      </ul>
      <p className={styles.footnote}>
        *Ending previous season on your roster means they were still on your roster at
        the conclusion of last year&apos;s championship game and still appear as an option
        on your &quot;Keeper Selection&quot; screen in the ESPN app.
      </p>
      <ul>
        <li>
          <strong>A first-year keeper&apos;s cost is the round they were drafted the prior
          year.</strong> If you draft a player in round 6 of 2025 and want to keep him in
          2026, he would count as your 6th round pick
        </li>
        <li>
          <strong>Keeper cost increases by 2 rounds each consecutive year kept.</strong>{" "}
          Following the above example, if you kept that same player a second time in
          2027, he would count as your 4th round pick.
        </li>
        <li>
          <strong>You can only keep a player for a maximum of 3 consecutive seasons (
          <em>The Alvin Kamara Rule</em>).</strong> Continuing the example, that player
          could be kept in 2028 as your 2nd round pick. This would be the 3rd
          consecutive season. In 2029, you would not have the option to keep that
          player at any cost and they would be free to be drafted by anyone
        </li>
        <li>
          The restriction is 3 <em>consecutive</em> years and that clock resets the
          first season the player is not kept by that owner (<em>The Alvin Kamara Rule
          2</em>). You wouldn&apos;t be able to keep that player in 2029, but if you drafted
          them naturally that year, the clock would reset and they could be kept in
          2030, 2031 and 2032.
        </li>
        <li>
          <strong>Players first acquired via free agency are not eligible keepers (The
          Alvin Kamara Rule 3).</strong> Historically, undrafted free agent acquisitions
          could be kept as 17th round picks the following year, which proved to be way
          too advantageous for breakout rookies. This rule was revised so that you
          could keep free agent pickups as long as <em>somebody</em> drafted them, but
          it was an admin nightmare to track and still seemed to go against the spirit
          of the rule.
        </li>
        <li>
          <strong>Players first acquired via trade are not eligible keepers.</strong>{" "}
          This was historically permitted and was voted against in 2025.
        </li>
        <li>
          <strong>Keeper decisions are due one hour before the draft begins.</strong> An
          owner who has not declared by the lock forfeits his keeper for that season.
        </li>
        <li>
          <strong>Keeper rights attach to the franchise, not the person.</strong> A
          replacement owner inherits the outgoing owner&apos;s keeper history, including
          seasons already used.
        </li>
      </ul>

      <Sub>1.4 : Trading</Sub>
      <p>
        Owners may exchange players at any time between the completion of the draft and
        the trade deadline. As long as both parties come to an agreement with no foul
        play, the trade stands.
      </p>
      <p>
        There is no limit on the number of trades a team may make. Trades process{" "}
        <strong>immediately</strong> upon acceptance; there is no waiting period.
      </p>
      <p>
        Trades involving money, future considerations, or anything outside the players
        on the two rosters are prohibited.
      </p>

      <Sub>1.4.1 : Trade Collusion</Sub>
      <p>
        Any transaction structured to benefit one team at the deliberate expense of the
        other participating owner&apos;s own competitiveness is prohibited. If any owner is
        suspected of accepting or offering bribes, or of trading to prop up another
        owner&apos;s team, he is subject to removal from the league.
      </p>
      <p>Any trade found to be collusive is reversed and both rosters restored.</p>
      <p>
        A lopsided trade is not automatically a collusive one. Owners are entitled to
        make bad trades.
      </p>
      <p>
        Any trades with guaranteed trade-backs, &quot;bench-sharing&quot;, etc. are considered
        unfair and not permitted.
      </p>

      <Sub>1.4.2 : Trade Deadline</Sub>
      <p>
        The trade market opens once the draft is complete. The trade deadline for the
        2026 season is <strong>Monday, November 9, 2026 at 1:00 PM ET</strong>. No trade
        may be executed after that time, for any reason. The deadline for future
        seasons is set by the commissioner and announced before Week 1.
      </p>

      <Sub>1.4.3 : Trade Restrictions</Sub>
      <p>
        <strong>Draft picks may not be traded</strong> — not in the current season, and
        not for future seasons. All trades are player-for-player.
      </p>
      <p>
        Owners remain bound by the roster maximums in Section 1.1. A trade that would
        put a team over a positional maximum or over the 18-man roster limit cannot be
        processed until the receiving owner makes room.
      </p>

      <Sub>1.4.4 : Trade Review Process</Sub>
      <p>
        There is no automatic review period and no owner veto. Trades are final on
        acceptance.
      </p>
      <p>
        Any owner may report a suspected collusive trade to the commissioner within{" "}
        <strong>48 hours</strong> of it processing.
      </p>
      <p>
        Where the commissioner is a party to the trade in question, he recuses himself
        and the remaining eleven owners decide.
      </p>

      <Sub>1.5 : Waiver Wire</Sub>
      <p>
        Every dropped player passes through a <strong>1-day waiver period</strong>{" "}
        before becoming a free agent. There is no limit on the number of acquisitions a
        team may make in a season.
      </p>
      <p>
        The league does not use a Free Agent Acquisition Budget. Claims are settled by
        waiver order, not by bidding.
      </p>
      <p>
        Once the Championship comes to an end, transactions (including trades) cannot
        be made until after next year&apos;s draft. In the event that the fantasy platform
        allows these to happen, they will be reversed and any players obtained will not
        be viable keepers.
      </p>
      <p>ESPN&apos;s Undroppable Players List is observed.</p>
      <p>
        Transactions are <strong>not</strong> locked for teams eliminated from playoff
        contention. Eliminated owners retain full roster rights and remain bound by
        Section 2.6.
      </p>

      <Sub>1.5.1 : Waiver Order</Sub>
      <p>
        The waiver wire begins the season in <strong>reverse order of the draft</strong>.
        The order changes only when an owner makes a successful claim, at which point
        that owner is sent to the bottom of the order. <strong>The order never
        resets</strong> during the season.
      </p>

      <Sub>1.6 : Divisional Breakdown</Sub>
      <p>
        The league is a single conference and a single division:{" "}
        <strong>Ballers and Shot Callers</strong>. All 12 teams sit in it.
      </p>
      <p>
        There is no divisional play, no divisional standings, and no automatic playoff
        berth attached to a division. The schedule is generated by the platform.
      </p>

      <hr className={styles.rule} />

      <Sec>Section 2 : Ownership Expectations</Sec>
      <InShort>
        $100 a team, due the day of the draft, and all $1,200 of it goes back out in
        prizes. Owners are added and removed by majority vote. Rule changes are settled
        before the draft, never mid-season. Set a real lineup every week, even when you
        are out of it.
      </InShort>

      <Sub>2.1 : League Fee</Sub>
      <p>
        The league fee is <strong>$100 per team</strong>, for a total pot of{" "}
        <strong>$1,200</strong>.
      </p>
      <p>
        Fees are payable to the commissioner via Venmo at <strong>@TomMikalonis</strong>.
      </p>
      <p>
        The deadline for the 2026 season is <strong>Tuesday, September 8, 2026</strong>{" "}
        — the day of the draft. An owner who has not paid by the deadline is subject to
        losing ownership of his team and may be removed from the draft and replaced.
      </p>
      <p>
        The commissioner holds all league funds and is accountable for the full $1,200
        balance from collection through payout. League funds are not to be spent, lent,
        invested, or wagered in the interim.
      </p>
      <p>
        The league fee may be changed between seasons by a vote under Section 2.3. It
        is fixed once the season begins and may not be changed mid-season for any
        reason.
      </p>

      <Sub>2.2 : Removal</Sub>
      <p>
        The owners may vote to remove any owner from the league at any time where they
        feel that owner is damaging the integrity or competitiveness of the league. The
        owner in question may present a defense before the vote is held. A majority
        removes.
      </p>
      <p>
        If an owner is removed <strong>before</strong> the season starts, he receives a
        full refund of his league fee. If he is removed <strong>during</strong> the
        season, he does not. The season starts once the draft is complete.
      </p>
      <p>
        If an owner is removed mid-season, the commissioner manages the team until a
        replacement is found. No roster transactions occur for that team in the
        interim.
      </p>

      <Sub>2.3 : Voting</Sub>
      <p>
        Each season, new rules and regulations are brought to the owners{" "}
        <strong>before the start of the draft</strong>. They must be decided before the
        draft begins to take effect for that season. A rule that misses that window
        takes effect the following season unless passed unanimously.
      </p>
      <p>
        All rules are voted on and require a <strong>majority vote</strong> — more than
        50% of voting owners — to be approved. Proposals are submitted to the
        commissioner, who is not obligated to bring every proposal to a vote.
      </p>
      <p>
        Proposals for the following season should reach the commissioner by{" "}
        <strong>August 15</strong>.
      </p>
      <p>
        Where this constitution is silent or ambiguous, the commissioner may issue a
        ruling. Any owner may appeal a ruling within 48 hours; a majority of all owners
        overturns it. No ruling reverses the result of a matchup that has already been
        finalized.
      </p>
      <p>
        Every adopted change is reflected in a dated revision of this document and
        distributed to all owners before the draft.
      </p>

      <Sub>2.4 : Prize Money</Sub>
      <p>Prize money is distributed as follows:</p>
      <Ledger
        head={["Finish", "Payout", "Determined By"]}
        rows={[
          ["1st Place", "$500", "Winner, Week 17 Championship"],
          ["2nd Place", "$250", "Loser, Week 17 Championship"],
          ["3rd Place", "$100", "Winner, Week 17 Third-Place Game"],
          ["Weekly Prizes", "$10 per Week", "Highest scorer each week during regular season"],
          [<strong key="t">Total</strong>, <strong key="p">$1,200</strong>, "—"],
        ]}
      />
      <p>
        Payouts are distributed within <strong>two weeks</strong> of the conclusion of
        the league championship.
      </p>

      <Sub>2.5 : Last Place Punishment</Sub>
      <p>
        Last place is determined by <strong>regular season record</strong>, with the
        lowest Total Points For breaking a tie. Consolation ladder results do not
        apply.
      </p>
      <p>
        The last-place owner must buy a belt/ring for the winning owner. Honestly, need
        to revisit this one.
      </p>

      <Sub>2.6 : Competitive Integrity</Sub>
      <p>
        This is a competitive league. It is not a cut-throat one. Every owner is
        expected to field the best lineup available to him every week of the season,
        including weeks in which his own season is already over.
      </p>
      <p>
        <strong>Tanking is prohibited.</strong> An owner may not knowingly submit a
        legal starting lineup designed to lose — for example, benching healthy starters
        in favor of players on bye, players ruled out, or players with no scheduled
        game.
      </p>
      <p>
        There is currently no rule whether eliminated owners can make waiver claims
        during the playoffs and this is pending a vote.
      </p>
      <p>
        This section targets deliberate tanking, not poor judgment. Owners are free to
        be bad at fantasy football.
      </p>

      <hr className={styles.rule} />

      <Sec>Section 3 : Drafting</Sec>
      <InShort>
        snake draft on ESPN, 90 seconds a pick, order drawn at random in front of the
        league. If you cannot make it, set your queue or find a stand-in — either way,
        the picks that get made are yours to keep.
      </InShort>

      <Sub>3.1 : Draft Order</Sub>
      <p>
        The method of draft order is to be determined by the prior year&apos;s winner, with
        the caveat that the method must be random. No methods will be permitted that
        involve skill, outside knowledge, etc. and anything unclear is subject to a
        vote. The process must be conducted live and recorded or witnessed by the
        league so that there is evidence no foul play has occurred.
      </p>
      <p>The commissioner then enters the resulting order into the platform manually.</p>

      <Sub>3.2 : Draft Date, Location &amp; Time</Sub>
      <p>
        The 2026 draft will be held{" "}
        <strong>Tuesday, September 8, 2026 at 8:00 PM ET</strong>, online through the
        ESPN platform. Format is a <strong>snake draft</strong> with{" "}
        <strong>90 seconds per pick</strong>.
      </p>
      <p>
        In future seasons the draft is held after the final preseason game and before
        the start of NFL Week 1. The commissioner offers multiple dates and times;
        whichever the majority of owners are available for becomes the official draft
        date.
      </p>
      <p>
        If you cannot make the draft, you are responsible for either setting your own
        autodraft queue or finding someone to draft for you, and you must notify the
        commissioner which. If you do neither, the platform autodrafts on your behalf.{" "}
        <strong>Autodraft results are final</strong> and are not grounds for a redraft
        or a roster adjustment.
      </p>
      <p>
        A draft will not be restarted once the first pick is made, except by unanimous
        consent of all owners present.
      </p>

      <hr className={styles.rule} />

      <Sec>Section 4 : Scoring System</Sec>
      <InShort>
        ESPN default scoring at half a point per reception. Changing any of it takes a
        vote, and the vote has to happen before the season starts.
      </InShort>
      <p>
        We play in a <strong>12-team, half-point-per-reception (0.5 PPR)</strong>{" "}
        head-to-head fantasy football league on ESPN.
      </p>
      <p>
        The full scoring breakdown is listed in the league settings on the ESPN
        platform. ESPN default values apply in all respects not modified by a league
        vote.
      </p>
      <p>
        Scoring settings are locked once the regular season begins and may not be
        changed mid-season.
      </p>

      <Sub>4.1 : Voting on Scoring System</Sub>
      <p>
        Each season before the start of the draft, owners may bring scoring proposals
        to the commissioner. If a proposal is well received, the commissioner puts it
        to a league vote. A majority approves. It must be approved before the start of
        the season to take effect.
      </p>

      <hr className={styles.rule} />

      <Sec>Section 5 : Postseason</Sec>
      <InShort>
        a 15-week regular season, then the top four teams play semifinals in Week 16
        and the championship in Week 17. No byes, no reseeding. Everyone else plays a
        consolation ladder for pride. Week 18 does not count.
      </InShort>

      <Sub>5.1 : Playoffs</Sub>
      <p>
        The regular season is <strong>15 weeks</strong>, beginning NFL Week 1, with one
        matchup per week.
      </p>
      <p>
        At the end of the season <strong>4 teams make the playoffs</strong> — the four
        best records, seeded 1 through 4. There are no division winners and no
        automatic berths, because the league is a single division. There are no
        first-round byes.
      </p>
      <p>
        Each playoff matchup lasts one week and the winner advances. The bracket does
        not reseed after the semifinals, and there is no home field advantage.
      </p>
      <Ledger
        head={["Week", "Round", "Matchups"]}
        rows={[
          ["Week 16", "Semifinals", "Seed 1 vs. Seed 4; Seed 2 vs. Seed 3"],
          ["Week 17", "Championship", "Semifinal winners"],
          ["Week 17", "Third-Place Game", "Semifinal losers"],
        ]}
      />
      <p>Week 18 is not included.</p>

      <Sub>5.2 : Playoff Tiebreakers</Sub>
      <p>
        A playoff matchup cannot end in a tie. If two teams tie in the playoffs, the
        tiebreakers go as follows:
      </p>
      <ul>
        <li>1. Bench points scored</li>
        <li>2. Higher seed advances</li>
      </ul>
      <p>
        Regular season matchups have <strong>no tiebreaker</strong> — a tie is recorded
        as a tie for both teams.
      </p>

      <Sub>5.3 : Seeding Tiebreakers</Sub>
      <p>If two teams finish with the same record, the tiebreakers go as follows:</p>
      <ul>
        <li>1. Total Points For</li>
        <li>2. Head-to-head record</li>
        <li>3. Total Points Against (fewer is better)</li>
      </ul>
      <p>
        There is no division record tiebreaker, as all teams are in one division.
      </p>

      <Sub>5.4 : Consolation Ladder</Sub>
      <p>
        Teams that do not qualify for the playoffs play out a consolation ladder. It
        carries no prize money and does not affect the determination of last place
        under Section 2.5. It exists so that every owner still has a game to watch in
        Weeks 16 and 17.
      </p>

      <hr className={styles.rule} />

      <Sec>Appendix A : 2026 Key Dates</Sec>
      <Ledger
        head={["Event", "Date"]}
        rows={[
          ["League fee deadline", "Tuesday, September 8, 2026"],
          ["Keeper declarations lock", "One hour before the draft"],
          ["Draft", "Tuesday, September 8, 2026, 8:00 PM ET"],
          ["Regular season", "NFL Weeks 1–15"],
          ["Trade deadline", "Monday, November 9, 2026, 1:00 PM ET"],
          ["Semifinals", "NFL Week 16"],
          ["Championship & third-place game", "NFL Week 17"],
          ["Payouts distributed", "Within two weeks of the championship"],
        ]}
      />

      <Sec>Appendix B : Keeper Tracker</Sec>
      <p>
        Players who have been kept 3 consecutive seasons and are <strong>not</strong>{" "}
        eligible to be kept the following year:
      </p>
      <Ledger
        head={["Player", "Season 1", "Season 2", "Season 3"]}
        rows={[["None — first year tracked under this constitution", "—", "—", "—"]]}
      />

      <hr className={styles.rule} />

      <Sec>Adoption</Sec>
      <p className={styles.dropcap}>
        This constitution supersedes all prior versions of the Groupies league rules.
        It takes effect upon distribution to the league and remains in force until
        amended under Section 2.3.
      </p>
      <p>
        By joining or returning to the league, each owner agrees that he has read this
        constitution and accepts all payments, deadlines, punishments, and rulings made
        under it.
      </p>

      <div className={styles.signature}>
        <div className={styles.signName}>Tom Mikalonis</div>
        <div className={styles.signRole}>— Commissioner —</div>
        <div className={styles.signRole}>
          The Groupies Fantasy Football League — August 4, 2026
        </div>
        <div className={styles.ornament} style={{ marginTop: 18 }}>❦</div>
      </div>
    </div>
  );
}
