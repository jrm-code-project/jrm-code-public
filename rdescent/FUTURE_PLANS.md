# Recursive Descent — Future Plans

Changes and enhancements we'd like to make to `rdescent`, the "Recursive
Descent" corporate-themed roguelike. Unlike `TECHNICAL_DEBT.md` (which
tracks *problems with existing code*), this document tracks *new
gameplay/features* we haven't built yet. Several of these are already
partially designed in code docstrings (cited below) so that adding the
stat/slot in question required no later rework — only the system that
*consults* the value remains to be written.

---

## 1. Monster Classes & Faction Aggression — DONE

Orc/"Code Monkey" and Troll/"Internet Troll" now spawn with FACTION
`:disgruntled-dev` (previously the generic `:monster`), and a new
"Middle Manager" monster (`enemies/middle-manager.lisp`) spawns with
FACTION `:management` starting at depth 3, gated into
`*rdescent-monster-spawn-table*` (`dungeon.lisp`). `hygiene-banded-
disposition` and `synergy-pacify-chance` (`entities.lisp`) implement
the two formulas below; `spawn-time-disposition` (`dungeon.lisp`)
applies both, in order, to every freshly spawned monster inside
`spawn-monsters-for-level`, using the player's own HYGIENE/SYNERGY
Corporate RPG Stats threaded in from `make-initial-state`/`use-stairs`:

- **Hygiene > 14** — "You look like a Suit." Management enemies become
  *Neutral* and ignore the player; Disgruntled Devs stay *Hostile*
  (they assume you're about to assign them Jira tickets).
- **Hygiene < 8** — "You look feral." Devs become *Neutral* (you're one
  of the pack); Management stays *Hostile*.
- **8–14 inclusive** — everyone hates you equally (original behavior).
- **SYNERGY's Pacify Chance** (`(MAX 0 (SYNERGY ability-modifier *
  5))%`) is then rolled once per still-Hostile spawn, independently of
  the Hygiene band: an otherwise-Hostile enemy instead spawns Neutral
  and simply wanders, ignoring the player.

## 2. Planned Bestiary

A depth-tiered enemy roster to give the dungeon a sense of escalation as
the player descends, replacing (or building on top of) the current
Orc/Troll-only spawn table. Several entries below depend on systems from
§1/§6/§8 (factions, status effects, traps) that don't exist yet, so this
list doubles as a forcing function for prioritizing those.

**The Cubicle Farm (Early Game Fodder)**
- **The Code Monkey** — basic melee grunt. Flings bad code and steals
  your USB cables. (Already implemented, as "Orc".)
- **The Internet Troll** — ranged annoyance. Chips your health with
  pedantic "Well, actually…" attacks and Sea-Lioning. (Already
  implemented, as "Troll".)
- **The Over-Caffeinated Intern** — very fast, very low HP. Panics
  constantly, dropping random useless items (promotional lanyards) when
  killed.
- **The Recruiter** — aggressively paths toward the player to offer a
  "rockstar opportunity," draining Energy if they catch you.

**The Open Office (Mid-Game Terrors)**
- **The Project Manager (Scrum Master)** — buffs other enemies by
  assigning them Jira tickets (increases their Speed); attacks with
  "Daily Standups" that freeze the player in place for one turn.
- **The Middle Manager** — spawns with 1d4 Code Monkeys; if the player
  kills the Code Monkeys, the Middle Manager claims credit for their
  deaths and gains a stat boost.
- **The SecOps Auditor** — high armor, ignores the player's HYGIENE-
  based stealth; attacks by revoking sudo privileges (disables item use
  for 3 turns).
- **The Focus Group (swarm enemy)** — moves as a blob; low individual
  damage, but blocks corridors and constantly inflicts a "Design by
  Committee" confusion debuff.

**The Glass-Walled Meeting Rooms (Late-Game Elites)**
- **"Karen" the Enterprise Customer** — high-HP tank; screams for the
  manager. Attacks bypass armor and directly damage the player's
  SENIORITY (Willpower). At 0 HP she doesn't die — she demands a refund
  and teleports away.
- **The HR Business Partner** — terrifying stealth enemy, invisible
  until adjacent. Melee attack is the "Performance Improvement Plan
  (PIP)," which permanently drains max HP if not healed quickly.
- **The Legal Counsel** — spellcaster; throws "Cease and Desist"
  area-of-effect spells that silence the player (no items/skills) and
  create physical walls of red tape on the map.
- **The Hiring Manager** — can summon Recruiters from off-screen; tries
  to trap the player in a "Culture Fit Interview" that drains Energy to
  0.
- **The Agile Coach** — mobile support unit; heals/buffs other enemies
  by forcing them to do "Trust Falls."

**The Executive Washroom / Top Floor (Bosses)**
- **The VP of Synergistic Alignments (VP of Engineering)** — mini-boss;
  every turn shifts elemental affinity based on whatever tech blog they
  read that morning (Blockchain, AI, Web3), forcing the player to
  constantly change attack strategy.
- **The "Rockstar" 10x Developer** — rogue-like anti-player unit; moves
  twice as fast as the player and deals massive damage, but drops
  legendary loot. Nominally on the player's side, but their code is so
  toxic it damages anyone standing near them.
- **The CEO (final boss)** — has a massive "Golden Parachute" shield;
  continuously summons Middle Managers. On defeat, doesn't die — resigns
  with a severance package, triggering the self-destruct sequence for
  the whole company (the endgame escape run).

Notes:  Bosses are singletons in their classes and have unique names
and flavor text.  Ideas for bosses include the CEO, Jimothy, Bigfoot,
Elizabeth Holmes, Clippy, Sam Bankman-Fried, Hide the Pain Harold,
Elon Musk (ultimate boss), Mark Zuckerberg (ultimate vice boss).

## 3. Graduated Enemy Difficulty by Depth — DONE

`spawn-monsters-for-level` (`rdescent/engine.lisp`) currently spawns
0–2 monsters per room, Orc-vs-Troll at a flat 80/20 split, on *every*
level regardless of depth — `level` is passed in and used only to seed
the deterministic RNG and to tag each spawned monster's own `level`
slot, never to scale count, composition, or strength. There is
currently no sense of escalating difficulty as the player descends.
Planned:

- **Depth-gate the §2 bestiary tiers**: the Cubicle Farm roster (Code
  Monkey/Troll, Over-Caffeinated Intern, Recruiter) should be the only
  enemies eligible on early levels; Open Office tier (Scrum Master,
  Middle Manager, SecOps Auditor, Focus Group) phases in at a
  mid-range depth; Glass-Walled Meeting Rooms tier (Karen, HR Business
  Partner, Legal Counsel, Hiring Manager, Agile Coach) at deeper
  levels; Executive Washroom bosses reserved for the final level(s).
  Concretely: a per-tier `min-depth` (and optionally `max-depth`, so
  early fodder can taper off rather than persisting forever) used to
  filter the eligible spawn-candidate list before rolling.
- **Scale spawn count with depth**: increase the 0–2-per-room range
  (e.g. toward 1–3, then 2–4) as `level` increases, so later floors
  feel more crowded — while keeping in mind `TECHNICAL_DEBT.md` item
  #32's note that higher density increases the chance of coordinate
  collisions silently dropping a spawn; the existing skip-count warning
  log should keep pace with any density increase.
- **Scale enemy strength with depth**, not just tier eligibility: even
  within one bestiary tier, a later-level spawn of the same monster
  type could roll modestly higher HP/damage than an earlier-level one
  (e.g. a small linear or `ability-modifier`-style scaling factor keyed
  off `level`), so grinding on an early floor doesn't trivially outpace
  the difficulty curve.
- This is squarely a `spawn-monsters-for-level` change: it already
  takes `level` as a parameter and is already covered by deterministic-
  RNG regression tests (per its own docstring), so a depth-scaling
  rewrite has an existing test harness to build on rather than needing
  one from scratch.

Depth-gating (`spawn-table-entry`'s own `min-depth`/`max-depth`) and
spawn-count scaling (`spawn-count-for-level`'s 0-2 → 1-3 → 2-4
progression) were already implemented ahead of this section as part of
`spawn-table-entry`/`spawn-table-choice`'s own generalized machinery
(see §21's build order). The only remaining bullet — scaling enemy
*strength*, not just eligibility/count, with depth — is now
implemented via `monster-depth-scale-factor` (a small, linear-in-
`level` multiplier, capped at `*rdescent-monster-depth-scaling-cap*`
so it can never produce absurd, unkillable monsters even at this
tier's deepest possible `level`) and `scale-monster-stats-for-level`,
which multiplies a freshly spawned monster's own base `max-hp`/`hp`/
`power` by that factor. Wired into `spawn-monsters-for-level`'s own
per-monster pipeline, right alongside the existing
`spawn-time-disposition` pass. XP reward is deliberately left
untouched — it's each monster factory's own fixed, literal reward, not
a derived stat that should inflate alongside depth. A single shared
linear factor (rather than a bespoke scaling constant per monster
class) was chosen so every bestiary entry's baseline nudges upward
together as `level` increases, matching how the existing tier-gating
and spawn-count scaling are also single shared mechanisms rather than
per-monster-type tuning tables.

## 4. Room Generation Modifiers by Depth — DONE

Room *type*, not just enemy tier, should scale with depth: right now
the procedural generator (per its docstring in `engine.lisp`) produces
a single, undifferentiated distribution of room shapes for every
level. §18's "Open Office" Stealth Penalty already sketches an
"Open Office Concept" room archetype (no walls, footsteps always
alert the whole room) versus a "Cubicle Farm"/"Server Room" archetype
(walls/corridors mask movement) — this section makes that split an
explicit, depth-gated generation parameter rather than a one-off
mechanic description.

- **Levels 1–5 ("Cubicle Maze"):** dense, low-visibility, high-wall-
  count room layouts (small rooms, narrow corridors) matching the
  Cubicle Farm bestiary tier (§3) — easy to retreat into a doorway
  and fight enemies one at a time.
- **Levels ~6–14 ("Open Office" transition):** a mix of walled rooms
  and larger, sparser open-plan rooms, phasing in as the Open Office
  bestiary tier phases in (§3), giving the stealth penalty from §18
  somewhere to actually apply.
- **Levels ~15–20 ("Agile Workspace"):** wide-open, wall-light floor
  plans by default — few chokepoints, long sightlines, multiple
  enemies able to converge on the player at once. Deliberately the
  most dangerous room-generation profile, pairing with the Glass-
  Walled Meeting Rooms bestiary tier (§3).
- **Levels 21+ ("Executive Suite"):** a return to more structured,
  compartmentalized layouts (private offices, boardrooms) gating
  access to the Executive Washroom bosses (§2) and the eventual win
  condition (§5).
- **Implementation approach:** parameterize the existing room-shape
  generator with a `level`-derived "room profile" (wall density /
  min-max room size / open-plan probability) the same way
  `spawn-monsters-for-level` is being parameterized by depth in §3 —
  ideally the same `min-depth`/`max-depth` tier-gating helper can back
  both the monster-eligibility and room-profile lookups so the two
  systems can't drift out of sync with each other.

`room-kind-weights-for-level`/`choose-room-kind` (ARCHITECTURE_PLAN.md
§6) already implemented the room-*type* half of this section ahead of
time — shallow levels favor `:cubicle`, `:open-office`'s share grows
(up to a 60% cap) as `level` increases, and `:server-room` holds a
constant 20% share at every depth. The remaining physical-parameter
half is now implemented via `room-size-multiplier-for-kind`: a
`:cubicle` room's own `min-size`/`max-size` range is scaled down
(0.7x, a denser "Cubicle Maze"), an `:open-office` room's is scaled up
(1.4x, a wider-open "Agile Workspace"), and `:server-room` stays at
the plain baseline. `generate-dungeon`'s own room-placement loop
clamps the scaled range to never exceed `(min width height) - 2`, so a
caller passing a small grid (several existing test fixtures do) can
never end up with a room straddling the edge. Deliberately reuses
`choose-room-kind`'s *already* depth-biased kind distribution rather
than adding a second, independent `level`-keyed size table: since
`:open-office`'s share already grows with `level`, rooms trend larger
(and, since a fixed-size grid holds proportionally thinner walls
between bigger rooms, less wall-dense) with depth for free, without a
second mechanism that could drift out of sync with the first. The
"Levels 21+ return to more structured layouts" bullet is *not*
implemented — `room-kind-weights-for-level`'s own distribution caps
`:open-office` at 60% and never decreases it again past that point (a
design decision already made and left alone by this section, since
§21's build order treats the room-*kind* distribution as already
complete); only the physical size/wall-density parameters were this
section's own remaining scope.

## 5. Win Condition / Level Progression

§3 and §4 define an escalating difficulty curve but never say where it
ends — there's currently no defined "you won" state; a run just
continues descending indefinitely (or ends in death). Planned:

- **A fixed final depth** (e.g. floor 25, "The Executive Washroom /
  Top Floor" per §2's tier naming) rather than infinite procedural
  descent — the dungeon generator refuses to generate a floor 26 and
  instead places the endgame encounter.
- **The endgame encounter:** confront the CEO (§2's final boss) behind
  a locked door (§9) requiring either a full key set or a
  Performance-Review-style stat check (§18's mechanic #7) to enter.
  Defeating (or out-negotiating) the CEO doesn't just end the fight —
  per §2's CEO flavor text, "they don't actually die, they resign,"
  triggering a scripted "self-destruct sequence" escape sequence.
- **The escape run:** a timed final sequence (reusing §18's "Agile
  Sprint" doom-clock mechanic #1, but as a one-time scripted event
  rather than a per-floor timer) where the player races back up
  through a partially-collapsing/alarm-lit version of the tower to an
  extraction point, carrying "the Source Code" (§15's Source Code of
  the Universe, or a dedicated quest-item flag if that legendary
  wasn't found this run) as the literal win-condition payload.
- **Win-state bookkeeping:** a simple `game-state` flag (`:victory`)
  distinct from the existing death/game-over state, so the client can
  render a distinct "you escaped with the Source Code" screen instead
  of reusing the death screen with different text; also a natural hook
  for a future leaderboard/run-summary feature (turns taken, floors
  cleared, gear found) if one is ever wanted.
- **Open question:** should losing to (or failing to escape from) the
  CEO be a normal death, or a softer "fired" ending distinct from dying
  to a random Code Monkey on floor 3? Worth deciding before the
  encounter is implemented, since it affects whether death-state
  handling needs a second branch.

## 6. Status Effects / Debuffs Framework

"Vague Re-Org Memo" (Confusion, `confused-turns`) is the only status
effect today, and it's hand-implemented end-to-end (its own slot, its
own AI interception in `confused-entity-turn`, its own item). Planned:

- Generalize into a small, reusable status-effect framework — e.g. a
  list of `(effect-name . turns-remaining)` pairs on `entity` instead of
  one bespoke `confused-turns` slot — so future effects ("Burnout",
  "Scope Creep", "Analysis Paralysis") don't each need their own
  hand-rolled slot and AI special-case.
- Wire up **SENIORITY**'s *Deflection Chance* (`SENIORITY ability-
  modifier * 10%` — see `entity`'s docstring): the player's chance to
  shrug off a newly-inflicted debuff entirely, rolled once per
  infliction attempt (SENIORITY 16 → 30%; SENIORITY 10 → 0%).
- Candidate new debuffs once the framework exists:
  - **"Burnout"** — reduced Speed/Energy regen for N turns.
  - **"Scope Creep"** — next attack's cost increases (spends more
    Energy than normal).
  - **"Impostor Syndrome"** — temporarily reduced PIVOT (dodge chance).

## 7. Varied Attack Effects (beyond flavor text) — DONE

The Code-Monkey/Internet-Troll attack-flavor system (`entity-attack-
flavor-pool`/`random-attack-flavor-text`) currently only swaps in
cosmetic sentences ("The Troll CCs your manager!") with zero mechanical
difference — explicitly called out as groundwork in its own docstring
and in `TECHNICAL_DEBT.md`. Planned: give individual flavor lines (or
groups of them) real mechanical side effects once the status-effect
framework above exists, e.g.:

- "The Troll demands peer-reviewed evidence!" → inflicts a short
  "Analysis Paralysis" debuff (see §6) alongside normal damage.
- "The Code Monkey submits a 500-line Bash script!" → chance to also
  inflict Confusion (re-using `cast-reorg-memo`'s existing mechanic).
- "The Troll flags your Jira ticket as 'Needs More Info'!" → no damage,
  but delays the player's next Energy tick (a pure annoyance effect,
  matching the joke).

This should be additive to the existing `resolve-attack-on-player`/
`build-combat-messages` pipeline (see `TECHNICAL_DEBT.md` items #38/#39)
rather than reintroducing a parallel damage-resolution path.

**Implemented:** `rdescent/mechanics.lisp`'s `mechanical-attack-flavor`
struct (custom constructor `make-mechanical-attack-flavor`) wraps a
flavor-pool entry's `text` (rendered exactly as before, via the same
`render-attack-flavor`) with an optional `on-hit-effect` (a plist
`:kind`/`:turns`/`:magnitude`/`:chance`, applied only on a successful
non-lethal hit, gated by both the pre-existing Seniority Deflection
Chance roll inside `apply-status-effect` and its own `:chance` roll —
default 1.0), an `always-effect` (same plist shape, applied
unconditionally regardless of hit/dodge/damage), and a `force-no-damage`
flag (skips `resolve-attack-on-player` entirely for a pure-annoyance
attack). `random-attack-flavor-text` now returns four values `text
on-hit-effect always-effect force-no-damage` instead of one — every
pre-existing caller that only bound the first value is unaffected.
`process-enemy-turns`' `attacking` branch was rewritten to consume all
four and apply them via `apply-status-effect`. Three concrete flavor
lines were wired up (`rdescent/enemies/troll.lisp`/`rdescent/enemies/
orc.lisp`): the Troll's "demands peer-reviewed evidence!" inflicts a
new `:analysis-paralysis` status effect (see `effective-dodge-chance`,
`rdescent/entities.lisp`, which reduces `pivot-dodge-chance` by
`*rdescent-analysis-paralysis-dodge-penalty*` — 15 — while active,
consulted by `resolve-attack` instead of calling `pivot-dodge-chance`
directly); the Code Monkey's "submits a 500-line script!" has a 35%
chance to also inflict Confusion, re-using the existing `:confused`
mechanic/`*rdescent-confusion-ticks*`; and the Troll's "flags your Jira
ticket as 'Needs More Info'" is now a `force-no-damage` attack that
unconditionally inflicts a new `:distracted` status effect, consulted
by a new `advance-entity-tick` (`rdescent/mechanics.lisp`, factored out
of `reduce-tick`) which skips that entity's next `accrue-energy` call
while `:distracted` is active — "delays the player's next Energy tick,"
exactly as planned. Both new status-effect kinds leave `magnitude` as
`nil` (they are not HP-draining effects); their behavior lives entirely
in the dedicated helpers above, mirroring the pre-existing `:confused`
pattern rather than the generic per-tick HP-delta mechanism.

Every tick-based duration above is derived from a real-world *seconds*
value via `*rdescent-tick-seconds*` (moved to `rdescent/entities.lisp`
so every downstream `defparameter` can reference it directly at load
time, rather than only from a function body evaluated later) —
`*rdescent-confusion-ticks*`/`*rdescent-analysis-paralysis-ticks*` are
now `(round (/ seconds *rdescent-tick-seconds*))`, not bare tick-count
literals. This mattered: at 50ms/tick, a status effect's
`ticks-remaining` is decremented once per *engine* tick (every
`reduce-tick`), which happens far more often than any monster/player's
own turn (gated by `entity-energy`/`entity-speed`) — the original
`*rdescent-confusion-ticks*` of 10 (a bare literal) amounted to only
half a real second, expiring before even one attack cycle from a Troll
(who only acts once every ~1.5 seconds), making it nearly useless
against slower monsters. `*rdescent-distraction-ticks*` is
deliberately left as a literal `1` — its entire purpose is to skip
exactly one engine tick's worth of Energy income, so it is
intrinsically tick-scoped rather than seconds-scoped.

The effects above fire rarely (a few percent of attacks, since they're
one entry among many in a flavor pool, often gated by their own
`:chance` roll too) and, before this addition, produced *no* visible
symptom beyond that one flavor/damage line — `:analysis-paralysis` and
`:distracted` in particular have no other observable consequence until
something actually consults them, making a freshly-inflicted debuff
easy to miss entirely. Fixed by adding an explicit callout: `rdescent/
mechanics.lisp`'s `status-effect-callout-text` maps a status-effect
`kind` to a dedicated "\*\*\* You feel Confused! \*\*\*"-style
announcement string (rendered in a new `*rdescent-status-effect-
callout-color*`, the same red already used for "\*\*\* Reincarnated!
\*\*\*"); `newly-applied-effect-kinds` diffs the player's
`active-effects` immediately before/after an attack to find which
kinds were *freshly* attached (as opposed to one already active whose
duration merely got refreshed); `status-effect-callout-messages`
combines the two into a `messages` list. `process-enemy-turns`'
attacking branch now computes this diff around every place it can
attach a status effect — including a monster's own built-in
`resolve-attack` method (e.g. the Troll's pre-existing 25% flat
Confused chance), not just a flavor-pool entry's `on-hit-effect`/
`always-effect` — and prepends the resulting callout(s) to that turn's
message-log entries (ahead of the ordinary combat/flavor line, since
the message-log is newest-first), so a freshly-inflicted debuff is
always announced the turn it lands, regardless of which code path
attached it.

## 8. Traps & Hidden Enemies — DONE

Implemented per this section's own "traps-as-stationary-entities"
design: `trap-fixture` (`entities.lisp`, a `fixture` subclass) with a
`hidden-p` slot (initform `t`), reusing `fixture`'s own `blocks-
movement nil`/`render-order 0` defaults so stepping onto one is an
ordinary move, not a bump-attack. `move-player`'s open-floor branch
looks up `trap-at` after moving and, if found, triggers it via the
existing `resolve-attack-on-player` (`commands.lisp`, `TECHNICAL_
DEBT.md` #38/#39) — the same dodge/damage/death math as an ordinary
monster's attack — pushes `build-combat-messages`-based combat
messages, and unconditionally reveals the trap (flips `hidden-p` to
`nil`) win, lose, or dodge. `render-grid` (`server.lisp`) skips
drawing a still-`hidden-p` trap's own char, so it renders as plain
floor until revealed. `seniority-detection-chance` (`entities.lisp`,
`SENIORITY * 5%` exactly as designed below) is rolled once per still-
hidden trap within the player's FOV on every `move-player` call, via
the new `maybe-reveal-hidden-entities` pass (a thin wrapper around the
renamed `move-player-inner`) — discovering a trap without triggering
it. `make-broken-deployment-trap` is the one implemented archetype so
far (`entities.lisp`); `spawn-traps-for-level`/`*rdescent-trap-spawn-
table*` (`dungeon.lisp`, mirroring `spawn-fixtures-for-level`'s own
"rare landmark" pattern as an independent spawn table/chance, not
folded into the fixture table) place at most one per level with 20%
probability, wired into both `make-initial-state` (`mechanics.lisp`)
and `use-stairs` (`actions.lisp`).
Deliberately diverges from `ARCHITECTURE_PLAN.md` §3's own suggestion
that `trap-fixture` be `is-alive t` ("so it can be attacked and
destroyed once revealed") — `process-enemy-turns`' acting-entities
filter gates solely on `is-alive` (not entity type), so an `is-alive
t` trap would incorrectly get an ordinary AI turn (and, with the
default `:neutral` disposition, wander) — see `trap-fixture`'s own
docstring for the full rationale. "Attack the trap to disarm it" and
the "Micromanager" hidden-enemy archetype (an enemy that starts hidden
rather than a pure-damage trap) remain unimplemented stretch goals,
not required by this section's own core ask.

No trap tiles or invisible/stealthed enemies exist yet. Planned:

- **Traps-as-stationary-entities:** rather than a special tile type on
  the `game-map`, implement traps as ordinary `entity` instances (or a
  new `trap` subclass) that are invisible/hidden until detected, never
  move (no AI turn/pathfinding — they simply sit at their spawn `x`/
  `y`), and whose only behavior is triggering an attack against
  whatever entity steps onto their tile (checked the same way
  `move-player`/`process-enemy-turns` already detect a collision with a
  blocking entity, rather than needing a separate tile-effect system).
  This reuses the existing collision-detection and `resolve-attack-on-
  player`/combat-message machinery (see `TECHNICAL_DEBT.md` items
  #38/#39) instead of inventing a parallel "tile effect" pipeline —
  triggering a trap is just an attack from an entity that happens to
  never move and starts hidden.
  - A damaging trap archetype — "Broken Deployment" (steps on a bad
    build, takes damage) is the working placeholder name from the
    original design notes.
  - A "Micromanager" enemy type that starts hidden/invisible until
    detected or until it acts, distinct from a pure-damage trap in that
    it could otherwise behave like a normal (if stationary) hostile
    entity once revealed.
  - Once revealed (via detection, see below, or by triggering), a trap
    entity should render visibly on the map for the rest of the run —
    matching how a discovered secret is typically permanent in
    roguelikes — rather than re-hiding itself.
- Wire up **SENIORITY**'s *Detection Chance* (`SENIORITY * 5%` — see
  `entity`'s docstring): the player's chance to notice a trap or hidden
  enemy the instant it enters FOV, rather than being surprised by it
  (SENIORITY 18 → 90%; SENIORITY 10 → 50%).

## 9. Keys & Locked Doors — DONE

Implemented per this section's own "locked-door tile, per-level
single-use key" design: a locked door is a `TILE`-level concern (not
an entity) — the `TILE` class (`entities.lisp`) gained `locked-key-id`/
`locked-key-name` slots, `*rdescent-locked-door-char*` (`#\+`), and the
`:corporate-badge`/"Corporate Badge" constants for the one implemented
archetype so far (mirroring §8's "only one trap archetype" precedent).
Keys are ordinary `GROUND-ITEM`s (`make-ground-key`, payload `(cons
:key key-id)`, char `#\*`) that `grab-item` (`actions.lisp`) routes
into a new per-player `:keys-held` flag (`keys-held`/`key-held-p`/
`add-key-held`/`remove-key-held`, `mechanics.lisp`, built on the
existing `game-state-flag`/`set-game-state-flag` machinery) rather
than ordinary `inventory` — so keys are tracked and spent independently
of item slots. `move-player-inner` (`actions.lisp`) gained two new
door-aware `cond` clauses ahead of the wall-block check: holding the
matching key unlocks-and-moves in one step (spending the key via
`remove-key-held`, recording a permanent per-player `:doors-opened`
flag via `add-door-opened`, at ordinary move energy cost), while
lacking it blocks movement with a "the door is locked" message and no
energy spent; a door already in `:doors-opened` is thereafter treated
as passable — critically, the underlying shared `TILE`'s own
`WALKABLE`/`CHAR` are never mutated, since dungeon geometry is cached
and shared across players (`*dungeon-cache*`), so "opened" is tracked
entirely as per-player game state, not shared tile mutation. `server.
lisp`'s new `tile-render-char` helper renders an opened door (per the
current player's own `:doors-opened`) as plain floor.

Generation-time reachability is enforced structurally rather than via
a BFS: `generate-dungeon` (`dungeon.lisp`) tracks each tunnel's own
"elbow" (the L-corner tile, now returned by `dig-tunnel` as a second
value) alongside its room list, and the new `place-locked-door` helper
probabilistically (`*rdescent-locked-door-chance*`, 0.25) converts one
tunnel elbow into a `LOCKED-DOOR` (a plain struct: `x`, `y`, `key-id`,
`key-name`, `safe-room-count`), requiring at least 3 total rooms and
only picking among elbows *after* the first tunnel (`j >= 1`) so that
`safe-room-count` (`= j + 1`) is always `>= 2` — guaranteeing the
player's own spawn room (`rooms[0]`) is never the *only* room "before"
the door. The dungeon topology is a strict linear chain (each room
connects only to the previous one), so "reachable without passing
through the door" reduces to "in one of the first `safe-room-count`
rooms" — no general graph search needed. The new `spawn-keys-for-level`
(mirroring `spawn-traps-for-level`'s shape, but never rolling its own
placement chance — that's entirely decided already by whether
`generate-dungeon` returned a non-nil `LOCKED-DOOR`) places the
matching key inside one of `rooms[1..safe-room-count - 1]` — excluding
`rooms[0]`, exactly like every other spawn function in this file, so
nothing (traps, monsters, keys) ever spawns in the player's own spawn
room. Wired into both `make-initial-state` (`mechanics.lisp`) and
`use-stairs` (`actions.lisp`). Full `persistence.lisp` round-tripping:
`serialize-payload`/`deserialize-payload` gained a `:key` case, and
`serialize-tile`/`deserialize-tile` round-trip `locked-key-id`/
`locked-key-name`.

The Root Password Post-It Note (§15's Rare Tier) "skeleton key"
exception remains an unimplemented stretch goal, not required by this
section's own core ask.

No locked doors or keys exist yet — every tile is either walkable or a
plain wall. Planned:

- Add a `locked-door` tile/tile-attribute to `game-map` (or a `door`
  entity, if doors ever need their own state beyond open/closed) that
  blocks movement like a wall until the player holds the matching key,
  distinct from ordinary walls so it can be rendered/telegraphed
  differently (e.g. a distinct `char`/color) once discovered.
- Keys are ordinary pickup items (a new `rdescent-item` subclass, or
  possibly folded into the `collection-log` machinery from §16 if we
  want them silently tracked rather than cluttering `inventory`) that
  the dungeon generator places somewhere reachable before its matching
  locked door on the same level — e.g. "Corporate Badge" (opens a
  generic office door), "Master Key Fob" (opens a supply closet),
  "Admin Credentials" (opens a server room).
  - Keys should most naturally be per-level (spend a key on the door
    it opens, then it's gone) rather than permanent inventory items, to
    avoid a growing "key ring" of stale keys from earlier floors.
- Generation-time constraint: whichever room/area a locked door gates
  off must never contain the only copy of its own key (the standard
  roguelike "don't lock the player out of progress" rule) — the
  dungeon generator needs a reachability check placing the key in a
  room accessible *without* passing through the door it unlocks.
- The Root Password Post-It Note (§15's Rare Tier) is a planned
  "skeleton key" exception to this system — it bypasses locked doors
  (and safes/encrypted terminals) entirely without consuming a
  level-specific key.

## 10. Vendors / Shops — DONE

Implemented per this section's own "vendor entity the player interacts
with to spend RSU" design, mirroring §8/§9's own "one archetype first"
precedent: a single `VENDOR-FIXTURE` archetype (`entities.lisp`), "the
Vending Machine" (char `#\=`, message-color `"#27ae60"`, a cash-
register green — factory `make-vending-machine`), subclassing the same
`FIXTURE` as `SHRINE-FIXTURE`/`TRAP-FIXTURE` but carrying *no*
per-instance mutable state of its own: unlike a shrine's finite
`USE-COUNT`, a vendor's "stock" is the single shared, fixed
`*RDESCENT-VENDOR-STOCK-TABLE*` constant (5 entries: Kombucha 50 RSU,
Scroll of PIP 300, Vague Re-Org Memo 400, Reply-All Bomb 500, A Stack
of Unread Memos 800 — the first purchasable `EQUIPPABLE-ITEM`), always
fully available regardless of how many players have already bought
from it. Wired into `*rdescent-fixture-spawn-table*` (`dungeon.lisp`)
as a 4th entry alongside the 3 shrine kinds, reusing the existing "at
most one FIXTURE per level, 20% chance" spawn plumbing rather than a
separate table — safe because that table's own contract was already
"at most one FIXTURE of any kind", never "at most one SHRINE-FIXTURE
specifically".

SYNERGY's own *Price Modifier* is `synergy-price-modifier` (`entities.
lisp`, next to `synergy-pacify-chance`): `1.0 - (ability-modifier(
synergy) * 0.05)`, deliberately unclamped (a low SYNERGY really can
raise a price above base). `vendor-item-price` applies it to a stock
entry's own `base-price`, floored at 1 RSU via an outer `(max 1
(round ...))` so no purchase is ever free.

The free-look/paid-purchase split is enforced by two separate code
paths: `interact-with-fixture` (`actions.lisp`) gained a
`VENDOR-FIXTURE` method that spends *no* energy and mutates *no*
player state — it merely pushes `vendor-stock-listing-text`'s
formatted wares/prices onto `message-log` (a shop-window "look", not a
shrine's own walk-up-and-consume interaction). Actually buying is a
separate reducer, `purchase-item` (`actions.lisp`), mirroring
`grab-item`'s own validation/branch shape: alive/energy checks, a
`VENDOR-FIXTURE`-at-position check ("There is nothing here to buy
from."), an `item-index` bounds check ("That item isn't for sale
here."), an RSU-affordability check via `vendor-item-price` ("You
can't afford the ~A (~D RSU)."), then either incrementing `kombucha`
(gated by `rdescent-tier-kombucha-limit`, "You can't carry any more
Kombucha!") or appending a freshly `funcall`ed `RDESCENT-ITEM` onto
`inventory` (gated by `rdescent-tier-inventory-limit`, "Your inventory
is full!") — deducting RSU and `*rdescent-move-energy-cost*` energy
only on success, leaving the `VENDOR-FIXTURE` itself completely
untouched (no `USE-COUNT` to decrement). Driven by a new
`PURCHASE-COMMAND` (`commands.lisp`, `{"action": "purchase",
"item-index": <integer>}`), dispatched via both
`execute-immediate-command`/`execute-queued-command` exactly like
`DROP-COMMAND`. Full `persistence.lisp` round-tripping:
`entity-class-tag`/`deserialize-entity-from-class-tag` gained a
`:vendor-fixture` case (no `entity-subclass-extra-alist` override
needed, since `VENDOR-FIXTURE` has no extra slots) — deliberately
avoiding the pre-existing (still unaddressed) `TRAP-FIXTURE`
round-trip gap described in §8, where a fixture subclass with no
explicit serialization method silently falls through to a plain
`:entity` tag on save/restore, losing its class identity.

## 11. NPCs & Quest Givers — DONE

Implemented per this section's own "one archetype first" precedent
(mirroring §8/§9/§10): a single non-mutable `NPC-FIXTURE` archetype
(`entities.lisp`), "The Disgruntled IT Guy" (char `#\I`,
message-color `"#7f8c8d"`, a weary helpdesk gray — deliberately
distinct from `MIDDLE-MANAGER`'s own `"#3498db"` corporate blue so the
two are never visually confused in the message log — factory
`make-disgruntled-it-guy`), subclassing the same `FIXTURE` as
`SHRINE-FIXTURE`/`VENDOR-FIXTURE` but carrying only a single read-only
`NPC-KIND` slot (`:DISGRUNTLED-IT-GUY` today). Unlike a shrine's
`USE-COUNT`, all quest *progress* lives entirely in per-player
`GAME-STATE` flags (`:IT-GUY-QUEST-ACCEPTED-P`/`:IT-GUY-QUEST-KILLS`/
`:IT-GUY-QUEST-REWARD-CLAIMED-P`, `mechanics.lisp`), exactly mirroring
§9's own `:KEYS-HELD`/`:DOORS-OPENED` — so two different players (or
the same player replaying) each get independent quest progress against
the same shared NPC instance. Wired into `*rdescent-fixture-spawn-
table*` (`dungeon.lisp`) as a 5th entry alongside the 3 shrine kinds
and 1 vendor kind, reusing the existing "at most one FIXTURE per
level, 20% chance" spawn plumbing rather than a separate table, same
as §10's own vendor addition.

The quest itself honors this section's own literal "Kill 5 Middle
Managers" example text: `record-middle-manager-kills` (`mechanics.
lisp`) increments `:IT-GUY-QUEST-KILLS` by a caller-supplied count,
called from all three combat-resolution call sites that can kill a
monster (`move-player-inner`'s bump-attack branch, `apply-item`'s
`TARGETED-ITEM`/`AREA-EFFECT-ITEM` methods, `actions.lisp`), each
already filtering that count down to only the `(typep target
'middle-manager)` kills from that action, alongside that same action's
own XP grant — so quest progress can never diverge from a player's
already-tracked kills. `*rdescent-it-guy-quest-kill-target*` (5) and
`*rdescent-it-guy-quest-reward-rsu*` (1000, `entities.lisp`) are the
only tunables; the reward is a flat RSU windfall via the existing
`get-rsu`/`update-entity :rsu` machinery rather than a new Root
Password Post-It Note item (§15's own skeleton-key reward remains an
unimplemented stretch goal, exactly as already noted for §9's own
locked-door system) — no new `rdescent-item` subclass needed. Kills
are tracked across the whole run once accepted (not reset per-floor,
despite this section's own "on this floor" phrasing), consistent with
how §9's keys/doors are also tracked per-player rather than per-level.

`interact-with-fixture` (`actions.lisp`) gained an `NPC-FIXTURE`
method dispatching to `interact-npc`/`interact-disgruntled-it-guy`, a
small dialogue state machine driven entirely by the flag helpers above
(`it-guy-quest-accepted-p`/`it-guy-quest-complete-p`/`it-guy-quest-
reward-claimed-p`): first-ever visit offers the quest; an accepted-
but-incomplete visit reports progress ("~D/~D Middle Managers down");
a complete-but-unclaimed visit pays out the RSU reward and marks it
claimed; any visit after that is a free, no-op dismissal ("We're
square. Don't push it.") — like talking to a `VENDOR-FIXTURE`, this
never spends energy or otherwise mutates `PLAYER` outside the single
reward-payout branch. Full `persistence.lisp` round-tripping:
`entity-class-tag`/`entity-subclass-extra-alist`/`deserialize-entity-
from-class-tag` all gained an `:npc-fixture` case, including (unlike
`VENDOR-FIXTURE`) an `entity-subclass-extra-alist` override to
preserve `NPC-KIND` across a save/restore round trip, since
`NPC-FIXTURE` (unlike `VENDOR-FIXTURE`) does carry one extra slot.

The Jaded Sysadmin fetch-quest and Terrified New Hire escort mission
described below remain unimplemented stretch goals, exactly as this
section originally scoped them — only the single Disgruntled IT Guy
archetype was built, mirroring §9's own deferral of its skeleton-key
stretch goal.

Every fixture introduced so far (§10's vendors, §12's shrines) is a
transaction, not a conversation — there's no NPC who hands out an
objective. Planned:

- **The Disgruntled IT Guy** — a friendly, stationary (or slow-
  wandering) NPC entity, immune to hostile targeting, who offers a
  simple side-quest on interaction: e.g. "Kill 5 Middle Managers on
  this floor and I'll give you a Root Password Post-It Note" (§15's
  Rare Tier). Accepting the quest sets a per-floor counter tracked
  against §1's faction/kill data; reaching the target and returning to
  the NPC (or an automatic check on next interaction) grants the
  reward and clears the quest.
- Other NPC-flavored quest-givers in the same vein: a **Jaded Sysadmin**
  who wants a specific Scavenger Hunt item (§16) fetched from
  elsewhere on the floor, or a **Terrified New Hire** who needs to be
  escorted to the stairs (a timed escort mission, reusing §1's
  pathfinding/aggro machinery in reverse — the NPC follows the player
  instead of hunting them, and dies permanently if left adjacent to a
  hostile enemy for too long).
- **Implementation approach:** a new non-hostile `entity` subclass
  (or a `friendly-npc` mixin) that never enters `process-enemy-turns`'
  attack-resolution branch, plus a lightweight per-`game-state` "active
  quests" alist (`quest-id -> (progress . target)`) checked whenever a
  relevant event fires (e.g. an enemy-kill event already produced by
  the combat reducer) — no new global quest-log UI required beyond a
  status line/toast on progress and completion.

## 12. Free-Standing Shrines (Break-Room Dispensers)

Distinct from §10's paid vendors: fixed, interactable, *free*
map fixtures that dispense a consumable on demand rather than being
picked up once and carried in `inventory` — the office equivalent of a
shrine/fountain in a traditional roguelike. No RSU cost, no stock to
run out of (or, optionally, a per-floor use limit to keep them from
trivializing resource management — see below).

- **The Espresso Machine** — interact to restore Energy on the spot
  (an on-demand version of Tier 2's Breakroom Coffee/Artisan Latte, §17),
  without needing to carry/consume an inventory item first.
- **The Kombucha Bar** — interact to heal HP via the existing
  `kombucha-heal-amount`/CAFFEINE-TOLERANCE formula, functioning as a
  free-standing, walk-up version of drinking a carried Kombucha
  (`drink-potion`) rather than a pickup.
- **The Water Cooler** — ties into §13's Branded Corporate Yeti Mug
  off-hand item: refills it (or, without that item equipped, just a
  minor flat heal/Energy tick) — a mundane, always-available fallback
  option compared to the tactical Tier 3 pharmacy items.
- Possible balancing lever: a per-shrine, per-floor use-limit (e.g. 3
  charges before the machine reads "OUT OF ORDER" until the next
  floor) so these don't fully obsolete the Tier 1/2 pharmacy items
  (§17) or make floors trivially survivable by camping next to one.
- **Implementation approach:** likely a new `ground-item`-adjacent
  fixture class (or a stationary, non-hostile `entity` subclass, same
  "reuse the collision-on-step pattern" idea as §8's traps, but
  triggered by an explicit interact action rather than a hostile
  attack) placed by the dungeon generator, with its own `use-count`
  slot decremented on each interaction rather than being consumed and
  removed from the map like an ordinary item pickup.

## 13. Equipment System & Armory — DONE

Implemented as an additive content pass on top of the already-built
equipment plumbing: all four slots (`:weapon`/`:body`/`:head`/
`:off-hand`) now have a full concrete §13 catalog, with stat bonuses
and weapon metadata carried by stateless `equippable-item` subclasses
and recomputed through the existing `effective-power`/
`effective-defense`/`effective-max-hp`/`effective-dodge-chance`/field-
of-view seams rather than any new cached stat layer. Every new item
also has a ground-loot wrapper, vendor catalog entry, and
`persistence.lisp` class-tag round trip, so the armory participates in
the same spawn/shop/save pipelines as the pre-existing consumables.

The mechanically wireable parts are implemented for real: reach and
multi-hit weapons use `weapon-reach`/`weapon-hits-per-turn` (now
honored by combat resolution itself), Blue-Light Blocking Glasses grant
`+2 DOMAIN-KNOWLEDGE` plus a flat `+1` FOV radius on top of
`domain-knowledge-fov-radius`, Patagonia Fleece Vest / Agile Scrum
Master Certificate / Branded Corporate Yeti Mug / Unwashed Hoodie feed
directly into the already-existing SYNERGY / CAFFEINE-TOLERANCE /
HYGIENE formulas, Headphones of Noise-Canceling reuse the same
`:confused` deflection seam Buzzword Immunity already uses, and the
YubiKey of Second Factors is the section's flagship bespoke mechanic:
once per floor, a lethal hit shatters it, leaves the player at 1 HP,
teleports them to a random safe tile, and resets on the next floor via
`use-stairs`.

Deliberate simplifications/deferments, following the same
implementation-summary style as §16's own Buzzword Immunity scoping
note:

- No ammo, reload, durability, or retrieval tracking — Rubber Band
  Gatling Gun, Nerf Retaliator, Whiteboard Marker of Dominance, Can of
  Compressed Air, and USB Drive Shuriken all work every turn because no
  separate ammo/durability subsystem exists yet.
- No crit-chance system — Razor-Sharp Aluminum Mousepad's crit identity
  is approximated as a `:pivot` bonus, feeding the already-wired dodge
  formula.
- No movement-speed penalty subsystem — Severed Server Rack Rail keeps
  only its heavy damage profile.
- No line-pierce, cone, or radial weapon targeting primitive —
  Telescoping Pointer's pierce, Mechanical Keyboard's sonic splash, Can
  of Compressed Air's cone, and "Reply-All" Blunderbuss' spread remain
  single-target approximations.
- No separate weapon Energy-per-shot resource or two-handed occupancy
  rule — Megaphone of "Let's Take This Offline", "Reply-All"
  Blunderbuss, and Keyboard of Kinesis use the shared attack scheduler
  only; the four-slot equipment model still allows an `:off-hand`
  alongside any `:weapon`.
- No forced-move AI, summon-AI, elemental damage typing, flee-in-
  terror AI, or self-backfire turn-skip hook — Laser Pointer of
  Redirection, HR Whistleblower, AWS Certified Solutions Architect
  Plaque, Agile Scrum Master Certificate's fear aura, and Stack
  Overflow Plagiarized Script's 5% backfire are therefore documented as
  passive-stat approximations instead of new one-off subsystems.
- Neck/amulet items share the existing `:head` slot rather than a fifth
  dedicated slot, because the equipment model remains intentionally
  four-slot.

The implemented catalog is:

**The Arsenal (Weapon / Main Hand)**

*Melee (blunt & slashing):*
- **Keyboard of Kinesis (Epic)** — two-handed (no shield/off-hand);
  high damage, grants a +2 modifier to PIVOT (ergonomic split design),
  and a chance to inflict "Carpal Tunnel" (slows enemy attack speed).
- **Red Swingline Stapler** — low base damage, but 10% chance on
  hit to inflict "Bleed" (paper cuts) and panic the target.
- **3-Foot Ethernet Cable (Cat 6)** — whip; fast attack speed, long
  reach (attacks up to 2 tiles away) — good for keeping Code Monkeys at
  bay.
- **Severed Server Rack Rail** — heavy mace; massive damage, but
  slows movement speed. Capable of one-shotting an Over-Caffeinated
  Intern.
- **Razor-Sharp Aluminum Mousepad** — dagger; low base damage, but
  very high critical-hit chance — an excellent high-PIVOT rogue build.
- **Telescoping Pointer (Laser Inactive)** — spear; attacks 2 tiles
  away and pierces through the first enemy to hit the one behind it.
- **A Stack of Unread Memos (Hardbound)** — club; standard blunt
  instrument, often dropped by Middle Managers (see §2's bestiary).
- **Whiteboard Marker of Dominance** — high damage, but has a
  durability mechanic (runs out of ink over time); bypasses Project
  Manager armor.
- **Mechanical Keyboard (Cherry MX Blue)** — area-of-effect sonic
  damage in a 1-tile radius on swing, because it's obnoxiously loud.

*Ranged (requires ammunition or limited durability before breaking):*
- **Rubber Band Gatling Gun** — rapid fire, 3 shots/turn at very
  low damage each; excellent against Focus Group swarms (see §2). Uses
  Rubber Bands as ammo.
- **Nerf Retaliator (Office Modded)** — sniper; high damage, long
  range, but requires a full turn to reload between shots. Uses Foam
  Darts as ammo.
- **Can of Compressed Air** — AoE blaster; shoots a 3-tile cone of
  freezing air, low damage, pushes enemies back one tile and inflicts
  "Brain Freeze" (stun).
- **USB Drive Shuriken** — thrown; a 128MB thumb drive that does
  decent damage and embeds itself in the enemy — must walk over their
  corpse to retrieve it.

*"Magic"/Psychic (spends Energy instead of dealing physical damage):*
- **Megaphone of "Let's Take This Offline"** — two-handed; casts a
  localized silence field. Enemies hit cannot use special attacks or
  call for backup for 5 turns.
- **Laser Pointer of Redirection** — wand; zero damage, but forces
  an enemy to walk in the pointed direction on their next turn (a
  targeted distraction).
- **The "Reply-All" Blunderbuss** — shotgun; fires a massive cone of
  unread emails for high psychic damage, but has massive recoil,
  draining 20 Energy per shot.
- **HR Whistleblower** — artifact; summons an invulnerable HR Rep
  NPC that slowly chases the nearest Middle Manager enemy (see §2) and
  terminates them instantly on contact. Usable once per floor.

**The Dress Code (Body Armor)**
- **Startup Green T-Shirt** (Light Armor) — +1 HP Armor; no
  protection against HR, but Code Monkeys treat the wearer as friendly
  for the first 3 turns.
- **Patagonia Fleece Vest** (Medium Armor) — the mid-level-VC
  uniform; +3 Armor and a passive SYNERGY bonus.
- **Unwashed Hoodie** (Heavy Armor) — +4 Armor, but a permanent -3
  HYGIENE debuff (a walking tank the whole map can smell — ties
  directly into §1's Hygiene faction-hostility mechanic).
- **Ironed Button-Down** (Suit Armor) — very low physical
  protection, but massive resistance to "Performance Improvement Plan"
  (PIP) psychic attacks (see §2's HR Business Partner).

**Peripherals & Accessories (Head / Neck)**
- **Headphones of Noise-Canceling** (Headgear) — total immunity to
  "Sea-Lioning"/"Let's Synergize" confusion debuffs (see §6).
- **Lanyard of the VIP** (Amulet) — reduces SecOps Auditor aggro
  radius to zero ("you belong here").
- **Blue-Light Blocking Glasses** (Headgear) — +2 DOMAIN-KNOWLEDGE,
  and +1 FOV radius on top of `domain-knowledge-fov-radius`'s existing
  bonus.
- **YubiKey of Second Factors** (Amulet) — once per floor, a fatal
  attack shatters the YubiKey instead of killing the player, leaving
  them at 1 HP and teleporting to a random safe tile.

**The Resume Fillers (Off-hand / Shields)**
- **AWS Certified Solutions Architect Plaque** (Shield) — blocks 50% of
  damage from "Cloud Migration"/"Database Outage" elemental attacks.
- **Agile Scrum Master Certificate** (Shield) — no physical defense,
  but Code Monkeys actively flee from an equipping player in terror.
- **Branded Corporate Yeti Mug** (Off-hand) — refillable at a water
  cooler; slows the passive drain of CAFFEINE-TOLERANCE.
- **Stack Overflow Plagiarized Script** (Off-hand Tome) — massive
  bonus to the next attack, but a 5% chance to backfire and skip the
  player's own turn.

## 14. Class Abilities

If a class/build-choice system is ever added (beyond the current
single undifferentiated player), SYNERGY's Pacify Chance could also
become an *active* skill ("Let's circle back and align on this
synergy") rather than a passive per-spawn roll — see `entity`'s
docstring for the original phrasing of this idea.

## 15. Rare & Legendary Loot — DONE

Beyond ordinary equippable gear (§13), a small set of unique,
game-altering drops — the kind of find that reshapes an entire run
rather than just nudging a stat. These should be rare, likely one-per-
seed or guaranteed-unique-per-run drops (a dedicated `unique-item`
flag/registry preventing two copies from ever coexisting in the same
`game-state`), and several depend on systems from other sections
(factions/§1, status effects/§6, vendors/§10, equipment/§13) that don't
exist yet.

**Rare Tier (purple drops — highly specific, game-altering effects)**
- **The "Out of Office" Auto-Responder** (Accessory) — when HP drops
  below 10%, become invisible to all enemies and drop aggro for 5
  turns (they assume you went on PTO); recharges on reaching a new
  floor.
- **The Root Password Post-It Note** (Consumable) — instantly unlocks
  any locked door, safe, or encrypted terminal, no alarms or
  hacking-minigame required (a skeleton key).
- **The Noise-Canceling AirPods Pro** (Headgear) — total immunity to
  all sonic/psychic attacks (Sea-Lioning, Buzzwords — see §6); no
  longer wakes sleeping enemies when walking near them.
- **The Platinum Corporate Amex** (Off-hand) — every vending
  machine/procurement desk (§10) is entirely free; you expense
  everything to the company.
- **The Pager of Dread** (Weapon/Wand) — targets an enemy and triggers
  an SEV-1 incident on their pager: massive psychic damage-over-time as
  they scramble to fix a non-existent server outage.

**Legendary Tier (gold drops — game-breaking artifacts)**
- **The B0FH's LART (Luser Attitude Readjustment Tool)** (Melee
  Weapon) — a heavily reinforced length of CAT-5 cable, stained with
  the tears of a thousand marketing executives. Devastating physical
  damage; any enemy hit is instantly silenced and loses all armor for
  the rest of the fight.
- **The Source Code of the Universe** (Two-Handed Tome) — a glowing
  three-ring binder of the original, undocumented C code running the
  company. Changes the primary attack to a ranged "Code Injection" that
  rewrites an enemy's faction alignment (§1) on the fly — shoot a
  hostile Code Monkey and permanently turn it into a friendly pet that
  fights alongside you.
- **The C-Suite Keycard** (Amulet) — a slick matte-black RFID badge
  humming with unearned authority. Treated as the CEO: all Middle-
  Manager-tier enemies and below (§2) immediately flee in terror,
  allowing the first ten levels to be walked through entirely
  unbothered.
- **The Golden Parachute** (Body Armor) — a tailored, bulletproof
  Italian silk suit. On what would be fatal damage, instead of dying
  you're teleported to the next floor's stairwell, fully healed, with
  your inventory filled with rare items (your "severance package") —
  then the item itself is destroyed (one-time use).
- **The Mechanical Keyboard of the Ancients (IBM Model M)** (Weapon/
  Shield) — weighs 15 pounds and echoes through the halls like gunfire.
  Acts as both a heavy blunt weapon and a shield; every step taken
  while equipped emits a "Click-Clack" sonic wave that permanently
  stuns enemies within a 3-tile radius by sheer, deafening typing
  volume.

Implemented as an additive content pass on top of §13's armory
plumbing, mapping all ten Rare/Legendary flavor-slots onto the same
four real equip slots (`:weapon`/`:body`/`:head`/`:off-hand`) and the
same ground-loot/persistence pipelines, but deliberately *not* added
to the vendor stock table (§10) — these are meant to be found, not
bought. A dedicated `unique-item` registry (`*rdescent-unique-item-
classes*`/`filter-out-owned-unique-items`) scans the player's own
inventory/equipment plus every visited-or-current level snapshot and
strips any of these ten classes from a freshly generated floor's loot
if a copy is already owned anywhere in the run, wired into
`use-stairs`' fresh-floor-generation branch only. `entity-disposition-
toward` (previously a no-op) is now the shared seam for both the "Out
of Office" Auto-Responder's invisibility and the C-Suite Keycard's
mass-fleeing effect. `resolve-attack` gained a `:convert-to-ally`
on-hit-effect special case for the Source Code of the Universe, and
`maybe-trigger-yubikey-save` (§13) now falls through to a new
`maybe-trigger-golden-parachute-save` when the YubiKey doesn't
trigger (or isn't equipped), chaining the two death-saves.

Deliberate simplifications/deferments, following the same
implementation-summary style as §13's own scoping note:

- The Golden Parachute teleports the player to a random *safe tile on
  the current floor*, not "the next floor's stairwell" — no existing
  primitive advances the player a floor outside of `use-stairs`, and a
  safe-tile teleport is exactly what the YubiKey death-save (§13)
  already does.
- The Source Code of the Universe's converted monster becomes a
  passive `:friendly`/`:companion` (the same disposition/faction the
  Office Doge companion uses) rather than an actively-controlled
  "fights alongside you" ally with its own AI — no such active-ally
  combat AI exists yet.
- The B0FH's LART's on-hit rider only applies `:armor-stripped` (forces
  `effective-defense` to a flat 0); there is no silence/status-cleanse
  mechanic to model "silenced" against.
- The Noise-Canceling AirPods Pro deflects `:confused`/`:stunned`
  infliction (reusing Buzzword Immunity's/Modafinil's own seam) but
  there is no sleeping-enemy/stealth-detection subsystem for "no longer
  wakes sleeping enemies."
- The C-Suite Keycard's "CEO outranks Middle-Manager-tier-and-below"
  check is approximated as an XP-threshold comparison
  (`*rdescent-c-suite-keycard-max-xp*`, the Middle Manager's own XP)
  rather than an explicit monster-class check, since `entities.lisp`
  (where the check lives) compiles before the concrete monster classes
  in `enemies/`.
- Uniqueness enforcement only prevents a second copy from ever spawning
  on the ground; it is not threaded into monster-kill drop tables at
  all, since none of these ten items are monster drops.

## 16. Scavenger Hunt Collectibles — DONE

Implemented as designed: a `collection-log` (a plain list of keyword
item-ids) added directly to `entity` (`entities.lisp`) rather than a
per-`game-state` flag like §9's `:keys-held`/§11's IT Guy quest flags
— unlike those, the functions each set-bonus needed to plug into
(`resolve-attack`, `effective-defense`, `effective-dodge-chance`,
`apply-status-effect`) only ever take an `entity`, never a
`game-state`, so an entity slot avoids threading `state` through call
sites that don't currently have it. A new `auto-pickup-item (entity)`
class (char `#\~`, the one glyph this codebase wasn't already using)
represents a collectible lying on the floor; `make-collectible`
factory looks up its name/message-color from two new catalog tables,
`*rdescent-collectible-catalog*` (all 23 items, verbatim from this
section's own item lists) and `*rdescent-collectible-sets*` (the 4 set
names/bonus names/colors) — `collectible-set-item-ids`/
`collection-set-complete-p` are the shared predicates every named
set-bonus predicate (`hardware-emulation-active-p`/`gnu-omniscience-
active-p`/`peripheral-vision-active-p`/`buzzword-immunity-active-p`)
delegates to.

Auto-pickup itself is a new post-processing pass,
`maybe-auto-pickup-collectible`, piped through `move-player` (the
public wrapper) alongside §8's existing `maybe-reveal-hidden-entities`
— exactly the same "one pass covers every one of `move-player-inner`'s
cond branches" precedent, so no individual branch needed to change.
It `adjoin`s the found item's id onto the player's `collection-log`
(idempotent — a duplicate pickup, e.g. once the deterministic spawn
index wraps past depth 23, is a harmless no-op), removes the entity,
and pushes a "You found ~A! [n/total]" message, plus a second "Set
Complete: ~A!" message the exact turn a set's last item lands.
`spawn-collectibles-for-level` (`dungeon.lisp`) mirrors §8's
`spawn-traps-for-level` shape exactly (own 0.3 spawn chance, 10-attempt
collision retry in a random non-spawn room) but — unlike every other
spawn-for-level function — picks its item *deterministically* via
`(mod (1- level) 23)` rather than a weighted `spawn-table-choice`,
guaranteeing no two concurrently-generated levels ever offer the same
collectible. Wired into both `make-initial-state` (`mechanics.lisp`)
and `use-stairs`'s fresh-depth branch (`actions.lisp`), same as every
other spawn-for-level function.

All four set bonuses are wired as "computed from current state"
predicates, never a one-time stat change (this section's own explicit
requirement): **Hardware Emulation** adds a flat
`*rdescent-hardware-emulation-defense-bonus*` (5) inside
`effective-defense`; **GNU/Omniscience** is a third new pass,
`maybe-reveal-full-map` (piped through `move-player` after the other
two), which forces `explored` to an all-1s bit-vector overriding
whatever FOV `move-player-inner` just computed, again without
touching any of its ~10 branches; **Peripheral Vision** folds a flat
`*rdescent-peripheral-vision-pivot-bonus*` (5, this implementation's
own choice — the section text doesn't specify a PIVOT amount, only
the dodge-percent) into the underlying PIVOT stat and adds a separate
flat `*rdescent-peripheral-vision-dodge-bonus*` (15%, as specified),
both inside `effective-dodge-chance`; **Buzzword Immunity** is an
unconditional deflection guard inside `apply-status-effect` whenever
`kind` is `:confused`, ORed alongside the existing `seniority-
deflection-chance` roll — deliberately *not* scoped to "Management-
tier enemies" as this section's text specifies, since
`apply-status-effect` (the single universal entry point every
debuff-inflicting call site already funnels through) has no notion of
which entity is inflicting an effect, only which is receiving one;
threading an inflicter-faction parameter through every existing call
site purely to narrow this one bonus would be far more invasive than
warranted, so this grants a simpler, strictly stronger immunity
regardless of source (mirroring this same session's own §11 IT Guy
precedent of preferring a universally-correct simplification over an
inflicter-specific detail the surrounding code has no existing seam
for).

Full `persistence.lisp` round-tripping: `collection-log` joins
`serialize-entity`/`deserialize-entity`'s common alist (via
`opt-entry`, mapping each keyword through `deserialize-keyword` on the
way back, exactly like `status-effect`'s own `kind`), and
`entity-class-tag`/`entity-subclass-extra-alist`/`deserialize-entity-
from-class-tag` all gained a new `:auto-pickup-item` case for its own
`item-id` slot.

## 17. The Corporate Pharmacy (Consumables) — DONE

Kombucha (via `drink-potion`/`kombucha-heal-amount`) is currently the
only consumable, with a single flat-ish heal formula. Planned: a full
tiered pharmacy of throwaway consumables replacing/supplementing it,
covering HP healing, Energy/Stamina recovery, and high-risk/high-reward
stat-altering "hard stuff." Several entries need small extensions to
the existing item pipeline: a temporary-buff/debuff mechanism (build on
§6's status-effect framework rather than a one-off slot per item), a
"restore Energy" effect (new — everything today only spends Energy,
via `*rdescent-move-energy-cost*`/`*rdescent-attack-energy-cost*`), and
a "skip next turn"/"lose control of next turn" effect (the Quadruple
Shot Espresso's crash, the Baggie of Blow's Comedown).

**Tier 1: The Communal Fridge (basic HP healing)**
- **The Stale Croissant** — heals 5 HP. Flaky, dry, but it's calories.
- **The Day-Old Breakroom Pizza** — heals 15 HP. Coagulated cheese, but
  fills the void.
- **Someone Else's Tupperware Lunch** — heals 25 HP, but 10% chance to
  inflict "Food Poisoning" (lose 1 HP/turn for 10 turns). You stole
  Linda's tuna salad.
- **The "Happy Birthday" Sheet Cake** — heals 10 HP. Abandoned in a
  conference room, 80% fondant.
- **A Handful of Free Office Almonds** — heals 3 HP. Unsalted,
  unroasted, purely for survival.

**Tier 2: Liquid Focus (Energy/Stamina recovery)**
- **The TGIF Leftover Beer** — restores 10 Energy, but -1 DOMAIN-
  KNOWLEDGE debuff for 20 turns (slightly buzzed at your desk).
- **The Breakroom Coffee (Burnt)** — restores 15 Energy. Tastes like
  battery acid.
- **The Artisan Latte** — restores 30 Energy. Procured from the
  hipster cafe downstairs.
- **The Quadruple Shot Espresso (Venti)** — restores 50 Energy and a
  temporary speed boost, but crashes (skips a turn) when it wears off.
- **The Warm Monster Energy Drink** — restores 40 Energy. Found behind
  a monitor, completely flat.
- **The "Smart Water"** — restores 20 Energy and cleanses the "Food
  Poisoning" debuff.

**Tier 3: The Desk Drawer Pharmacy (hard stuff — buffs & trade-offs)**

*The tactical nukes of the consumable world — they don't just heal,
they alter stats, usually with a serious downside.*

- **Discarded Adderall (10mg)** — instantly restores Energy to max and
  grants +2 DOMAIN-KNOWLEDGE for 50 turns; lose 10 HP when it wears off.
- **Modafinil** — total immunity to sleep/stun mechanics from HR for
  100 turns. You do not blink. You do not yawn.
- **Dexedrine Spansule** — "The Zone": for 20 turns, all attacks are
  guaranteed critical hits, but HYGIENE immediately drops to 0
  (sweating profusely — ties into §1's Hygiene faction-hostility
  mechanic).
- **The Baggie of Blow (Executive Grade)** — the ultimate panic button:
  full heal, max Energy, +5 SYNERGY and +5 PIVOT. 10 turns later, a
  catastrophic "Comedown" debuff halves movement speed and applies -2
  to all stats for 50 turns.
- **Unmarked Nootropic Stack** — a roll of the dice: 50% chance of a
  massive intelligence boost, 50% chance of violent illness that drains
  HP to 1.
- **Microdose Tab (LSD)** — doubles FOV radius (stacking with
  `domain-knowledge-fov-radius`) and makes hidden doors/traps (see §8)
  glow neon colors — you can see the matrix of the corporate
  architecture.

**Implemented:** `rdescent/entities.lisp`'s `consumable-item` class
(subclass of `rdescent-item`, slots `heal-amount`/`energy-restore`/
`effect`/`cleanse-kind`/`stat-overrides`/`flavor-text`) plus the
`define-consumable-item` macro (mirroring `define-armory-equippable-
item`'s declarative shape) generate all 16 ordinary Tier 1-3 items;
the Unmarked Nootropic Stack's 50/50 gamble needed a bespoke class and
`apply-item` method instead. `rdescent/mechanics.lisp`'s
`tick-status-effect`/`tick-status-effects` gained a new `expire-into`
slot/mechanism: when a status effect's `ticks-remaining` hits 0, an
`expire-into` plist (if present) is spliced in as a freshly-chained
successor effect on that same tick (used for Adderall's Focus->Crash
and the Espresso's speed-boost->skip-turn chain) — the chained
effect's own duration/magnitude only actually applies starting the
*following* tick, exactly like any other effect attached via
`entity-with-effect`. Self-administered buffs/debuffs go through
`entity-with-effect` (unconditional attach), never through
`apply-status-effect` (which rolls Seniority Deflection Chance) — a
player choosing to dose themselves should never have it "deflected."
Energy-restoring items are NOT capped by `*rdescent-max-banked-
energy*` (that cap only governs passive `accrue-energy`); a direct
`:energy` override can exceed it, matching the pre-existing Espresso
Machine shrine. Three scope reductions from the original write-up:
Dexedrine's "guaranteed critical hits" became a flat `effective-power`
bonus (:the-zone) since no crit-multiplier subsystem exists; the
Comedown's "halves movement speed" became a doubled
`effective-attack-energy-cost` only (raw move-energy cost untouched);
the Microdose Tab's "glow neon colors" became a guaranteed-detection
bypass in `maybe-reveal-hidden-entities` (no rendering change). All 17
items are wired into `*rdescent-vendor-stock-table*`,
`*rdescent-item-spawn-table*` (Tier 1 depth 1+, Tier 2 depth 2+, Tier
3 depth 3+, Baggie of Blow depth 4+), and persistence via
`define-consumable-item-persistence`.

## 18. Whiteboard Mechanics Grab-Bag

A batch of standalone gameplay-loop ideas ("throw it on the whiteboard,
Boss") — each is a self-contained system, several of which build on
sections above once those exist.

**1. The "Agile Sprint" Timer (the doom clock)** — every floor is a
"Two-Week Sprint": a per-level turn budget to find the stairs (the
"Deliverable"). If the counter hits zero first, "Crunch Time" kicks
in — all enemies get a large speed/damage buff, the screen takes on a
red tint, and CAFFEINE-TOLERANCE drains twice as fast. You are
officially working the weekend. (A new per-`game-state` counter,
decremented by `reduce-tick` alongside the existing energy-regen pass;
"Crunch Time" is itself effectively a temporary global debuff, so it
could reuse §6's status-effect framework applied to every hostile
entity at once rather than being a one-off global flag.)

**2. Environmental Hazards: the "All-Hands Meeting"** — a periodic
global event: for 50 turns, all Management/HR-faction enemies (§1)
migrate toward a specific room. Getting caught in that room or in the
hallway by the swarm inflicts a "Boredom" debuff that paralyzes the
player for 5 turns while they listen to synergy metrics. (Needs §1's
faction system for "which enemies migrate," and a room-targeted
group-AI override distinct from ordinary per-enemy pathfinding.)

**3. The "Reply-All" Chain Reaction (emergent chaos)** — using a Reply-
All Bomb (an existing `area-effect-item`) has a 10% chance to trigger a
chain reaction: other enemies caught in the blast radius become
Confused (reusing the existing `cast-reorg-memo`/`confused-turns`
mechanic) and fire off their own Reply-All attacks in random
directions, potentially turning a quiet cubicle farm into a
cross-firing warzone of automated emails and out-of-office replies.
(Builds directly on the existing Reply-All Bomb and Confusion systems
— no new primitives needed, just a chained trigger.)

**4. The "Open Office" Stealth Penalty** — room type dictates
acoustics. Cubicle Farm/Server Room rooms mask footsteps; stepping into
an "Open Office Concept" room (no walls) or eating a consumable there
instantly alerts every enemy in the room, forcing the player to sneak
around the edges by the potted plants. (Needs a `room-type`/acoustics
tag on generated rooms, distinct from the plain rectangular rooms
`generate-dungeon` produces today, and a noise/alert-radius concept
that doesn't exist yet — related to, but a level below, the
`*rdescent-monster-fov-radius*` detection `move-player`/
`process-enemy-turns` already use.)

**5. Faction Warfare (the turf war)** — once monsters have factions
(§1), they shouldn't just hate the player; they should hate each other.
An HR Rep crossing paths with a Disgruntled Dev in a hallway should
trigger monster-vs-monster combat (reusing the same collision/attack
machinery §8's stationary trap entities and the existing Confusion
monster-vs-monster branch already exercise), letting the player kite a
high-level Suit into a room full of Code Monkeys, close the door, and
collect the loot afterward.

**6. The "Mandatory Training" Trap Tile** — a hidden trap (§8),
typically placed in bottlenecks near the stairs, that locks the player
in place for 3 turns ("You must complete mandatory Phishing Awareness
Training") while a slideshow plays, leaving them vulnerable to
wandering enemies. A concrete instance of §8's general "stationary,
initially-hidden entity that triggers on step" pattern, but a
turn-skip effect instead of a damage attack.

**7. The Performance Review (end-of-floor boss gate)** — the stairs
down are sometimes locked behind a door guarded by the player's
"Direct Manager" (building on §9's locked-door/key system, but gated
by a stat/dialogue check instead of a key item). High enough SYNERGY,
or enough collected "Completed Jira Tickets" (a new specific loot type,
candidate for §16's collection-log machinery), lets the player pass
without a fight; failing the check puts them on a PIP and forces a
fight while heavily debuffed.

## 19. Save/Restore via Signed Client-Side State — DONE

Implemented as a compressed, HMAC-signed client-side blob, addressing
the original ~290KB fragment-size problem (see history below) by
compressing before signing rather than just re-raising hunchensocket's
fragment limits. `rdescent/persistence.lisp`'s `pack-save-state`
JSON-encodes `serialize-game-state`'s alist (`babel:string-to-octets`),
zlib-compresses it (`salza2:compress-data`), HMAC-SHA256-signs the
compressed bytes (`sign-payload`, keyed off `*save-state-hmac-key*` --
`RDESCENT_SAVE_SECRET` env var, with a `LOCAL_DEV_KEY_CHANGE_ME_IN_PROD`
local-dev fallback), and base64-encodes the result
(`cl-base64:usb8-array-to-base64-string`) for the client to stash in
`localStorage`. `unpack-save-state` reverses every step
(`verify-and-extract-payload` rejects a tampered/mismatched-signature
blob before anything is decompressed) and calls
`deserialize-game-state`, which itself guards against loading an
incompatible save via `check-save-format-version`/
`*rdescent-save-format-version*`. `rdescent/server.lisp` handles an
incoming `{"action": "save"}` WebSocket packet by calling
`pack-save-state` and replying with a `save-payload` packet;
`rdescent.js`'s `s`/`S` keypress sends the `save` action and its
`save-payload` handler stashes the returned base64 blob in
`window.localStorage` under `rdescent_save`. `restore` is a
first-class command (`restore-command` in `commands.lisp`, parsed
from an incoming `{"action": "restore", "payload": ...}` packet) with
both `execute-immediate-command` and `execute-queued-command` methods
that call `unpack-save-state` and swap in the reconstructed
`game-state`; `rdescent.js`'s `r`/`R` keypress (`restoreGame`) reads
the stashed blob back out of `localStorage`, refuses client-side (with
a message-log error, never sent) if it's grown to 64KiB or larger, and
otherwise sends it as a `restore` action followed by a "*** Game
Restored ***" message-log line.

No autosave or autorestore-on-reload was implemented -- a deliberate
choice: the player must explicitly request a save (and later, a
restore) rather than the client silently persisting/reloading state on
every connection.

History: an earlier version of this feature (uncompressed) was
removed after the restore payload's serialized-game-state blob ran to
~290KB, comfortably exceeding hunchensocket's default 64KiB WebSocket
fragment limit and crashing the connection (manifesting as "arrow keys
don't work" after a page reload once a save existed in `localStorage`).
Rather than just raising hunchensocket's fragment/message size limits
and accepting ~290KB of bandwidth per round trip, the feature was
reimplemented with zlib compression added to the pipeline above, per
this section's own original recommendation.

## 20. Additional Ideas (not yet formally designed)

- More Ground Items beyond Stock Options/Kombucha/Scroll of PIP/Reply-
  All Bomb/Vague Re-Org Memo — e.g. a "Performance Improvement Plan"
  cursed item, a "Free Lunch" minor heal.
- Depth-gating the §2 bestiary tiers and scaling spawn count/strength
  by depth is now its own section — see §3, "Graduated Enemy
  Difficulty by Depth."
- More meme-based features:  a "This is fine" room, which is on fire,
  but if you sit at the table you regain health anyway.  "Hide the
  Pain Harold." "The Sparkle Pony" from Burning Man, a free-love
  filthy hippie chick who gives you "The Itch" and drops
  a baggie of mystery powder when defeated.  "The Floor is Lava" room,
  which just saps health (rapidly) if you step on the floor.
  Cursed NFT items that are of no value, but take up inventory space
  and cannot be dropped.  The Ayn Rand objectivist.  
- YouTube NPCS. **Lauri (The Hydraulic Press Channel)**, crushes
  random items out of your inventory.  MrBeast, PewDiePie,  Mark Rober
  (glitter bombs), Jake and Logan Paul.
- `Manuals' that just provide flavor text.  Used to guide users
  through the game, and provide hints.  Could be used to provide
  backstory for the game. Manual at end of anonymous level would tell
  you to create a free account to continue playing.   Manual at end of
  free game would tell you to purchase a membership.

## 21. Suggested Build Order

`ARCHITECTURE_PLAN.md`'s own §11 build order is now fully complete, so
several sections below are already partially (or, in a couple of
cases, fully) satisfied by that foundation: §1 is now fully done (see
its own section) — HYGIENE's sliding-scale hostility swing and
SYNERGY's Pacify Chance are wired into spawn-time disposition
assignment via `spawn-time-disposition`, and a new "Middle Manager"
:MANAGEMENT-faction monster exists. §6's status-effect framework
(`active-effects`) is fully built and SENIORITY's Deflection Chance is
already wired into `process-enemy-turns`'/`resolve-attack-on-player`'s
attack-resolution paths — only new debuff *kinds* remain. §3's spawn
tables already respect per-tier `min-depth`/`max-depth` gates and
`spawn-count-for-level` already graduates monster density with depth
— only per-spawn stat scaling (HP/damage growing with depth) is
missing. §4's `room-kind-weights-for-level`/`choose-room-kind` already
bias room-kind selection by depth — only physical
generation-parameter scaling (room size, wall density) beyond kind
selection remains. §12 (shrines) and §13 (equipment system *and*
content pass) are now fully implemented, with only the explicitly
documented simplifications in §13 itself deferred. §19 (save/restore)
was implemented and later removed entirely (see its own section).

Recommended sequencing (each phase assumes the previous is done and
tested, per this project's existing FiveAM convention):

1. **§1 Faction Aggression — DONE.**

3. **§22 Companion Pet: The Office Doge** — a natural fit right after
   Faction Aggression lands, since Doge reuses the same `disposition`
   data model (`:friendly` by construction, like `npc-fixture`) and
   the existing `(eq disposition :friendly) ...` branch in
   `commands.lisp`'s movement/attack resolution, just adding a fourth
   "swap places with a bonded companion" case alongside it. Small,
   self-contained, and a fun, low-risk morale win before the larger
   Traps/Keys/Vendors systems below.
4. **§7 Varied Attack Effects** — purely additive to the
   already-existing flavor-text pool and `active-effects` framework;
   wire specific attack-flavor lines to specific status-effect
   infliction. Low risk, no new primitives, immediate content payoff.
5. **§8 Traps & Hidden Enemies — DONE.** The first genuinely new system in
   this order: a stationary, initially-hidden `entity` subclass that
   reuses existing collision-detection and
   `resolve-attack-on-player`/combat-message machinery rather than a
   parallel tile-effect system (per `TECHNICAL_DEBT.md` #38/#39). Also
   wires SENIORITY's Detection Chance. Needed before §9 (the Root
   Password's skeleton-key exception presumes locked doors exist) and
   §18.6 (Mandatory Training Trap is a concrete trap instance).
6. **§9 Keys & Locked Doors — DONE.** A `locked-door`-attributed `TILE`
   plus per-level `GROUND-ITEM` keys (tracked via a per-player
   `:keys-held` flag, not `inventory`) and a structural (non-BFS)
   dungeon-gen reachability constraint — the linear tunnel-elbow chain
   guarantees the key always lands in a room reachable before its own
   door. Required by §5 (the CEO's locked boss room) and §18.7 (the
   Performance Review gate).
7. **§10 Vendors/Shops — DONE.** Reused the fixture+interact pattern
   already proven by shrines (§12), folded into the same fixture-spawn
   table (a 4th entry, still "at most one per level"), and the
   existing RSU economy; wired SYNERGY's Price Modifier
   (`synergy-price-modifier`/`vendor-item-price`). A new
   `purchase-item` reducer + `PURCHASE-COMMAND` handle the actual
   spend, while `interact-with-fixture`'s free-look branch just prints
   the wares.
8. **§16 Scavenger Hunt Collectibles — DONE.** A `collection-log` slot
   plus an `auto-pickup-item` class and the same "computed from
   current state" set-bonus pattern the Corporate RPG Stats already
   use. Implemented ahead of §11's quest-givers (next), though §11 was
   ultimately built against §1's `MIDDLE-MANAGER` kill-count instead of
   referencing a specific collectible.
9. **§11 NPCs & Quest Givers — DONE.** Implemented the kill-quest half
   of this section (the Disgruntled IT Guy's "Kill 5 Middle Managers"
   quest) directly against §1's `MIDDLE-MANAGER` monster — the
   fetch-quest/escort-mission stretch goals that would have used
   §16's collectibles remain unimplemented, exactly as this section's
   own text scoped them as optional.
10. **§13 content pass — DONE.** Filled the existing four-slot
    equipment system with the full Arsenal/Dress Code/Peripherals/
    Resume Fillers catalog, wired every direct stat/reach/status/FOV
    hook the engine already exposed, and documented the remaining
    out-of-scope flavor mechanics (ammo, AoE/cones, forced-move/summon
    AI, etc.) as deliberate simplifications rather than one-off new
    subsystems.
11. **§17 The Corporate Pharmacy — DONE.** Added the "restore Energy"
    effect (a direct, uncapped `:energy` override) and status effects'
    new `expire-into` chaining mechanism (for "skip/lose next turn"-
    style crashes) as the two new primitives this section needed;
    otherwise reused the same declarative `define-*-item`-macro content
    pass established by §13.
12. **§15 Rare & Legendary Loot — DONE.** Added a `unique-item`
    registry/ground-spawn filter, extended `entity-disposition-toward`
    from a no-op into the shared seam for both the "Out of Office"
    invisibility and C-Suite Keycard mass-fleeing effects, gave
    `resolve-attack` a `:convert-to-ally` on-hit special case, and
    chained a new `maybe-trigger-golden-parachute-save` behind §13's
    YubiKey death-save.
13. **§3/§4, remaining scaling work — DONE.** Added
    `monster-depth-scale-factor`/`scale-monster-stats-for-level`
    (§3's per-spawn HP/POWER depth scaling, wired into
    `spawn-monsters-for-level`) and `room-size-multiplier-for-kind`
    (§4's physical room-size scaling, layered on top of the
    already-depth-biased `choose-room-kind`, wired into
    `generate-dungeon`'s room-placement loop and clamped to the
    requested WIDTH/HEIGHT).
14. **§2 Bestiary expansion** — the full depth-tiered monster roster
    (Recruiter through CEO); explicitly deferred until last among the
    "core" sections because its own text says it depends on §1/§6/§8,
    and it's a large, mostly-mechanical content backlog best tackled
    once every mechanic a monster might reference (factions, debuffs,
    traps, keys, loot) already exists.
15. **§5 Win Condition / Level Progression** — the most
    dependency-heavy section (needs §2's CEO, §9's locked door, §15's
    Root Password/skeleton-key, and §18.1's doom-clock); a natural
    capstone once its prerequisites are in place. Uses the existing
    `flags` infrastructure for the `:victory` state.
16. **§18 remaining Whiteboard mechanics** — several of these have no
    dependency on the phases above and can be pulled forward opportunistically
    as quick wins whenever convenient (§18.1 Agile Sprint timer and
    §18.3 Reply-All Chain Reaction need nothing beyond
    already-completed infrastructure); the rest are naturally
    delivered alongside the system they concretize (§18.4 Open Office
    Stealth Penalty with room-acoustics tuning, §18.5 Faction Warfare
    right after step 2, §18.6 Mandatory Training Trap right after step
    5, §18.7 Performance Review Gate right after step 6).
17. **§14 Class Abilities** — deferred indefinitely; the section
    itself is speculative ("if a class/build-choice system is ever
    added") with no concrete design to build toward yet.

## 22. Companion Pet: The Office Doge — DONE

**Implemented** (§21 phase 3): `rdescent/entities.lisp`'s `companion`
class (a plain `entity` subclass with one new `bonded` slot, read via
`companion-bonded-p`, defaulting `:faction :companion`/`:disposition
:friendly` via `:default-initargs`) plus its `make-doge`/`companion-p`/
`bonded-companion-p`/`find-bonded-companion` helpers. `rdescent/
dungeon.lisp`'s `spawn-doge-for-level` places at most one still-wild
Doge per level (gated by `*rdescent-doge-spawn-chance*`, mirroring
`spawn-fixtures-for-level`'s own rare-landmark placement strategy),
skipped entirely whenever the player already has a bonded Doge
following them, and guaranteed (bypassing that chance roll) via its
own `guaranteed-p` argument whenever `use-stairs` generates a fresh
depth for a player with no bonded Doge -- "you'll always find a new
one, but never two at once." `rdescent/actions.lisp`'s `move-player` bonds a
still-wild Doge for free (no `energy` cost, a flavor message, no
attack) on collision, and swaps places with an already-bonded Doge
(at the ordinary move cost) instead of ever attacking it -- "you
cannot attack it." `use-stairs` carries an alive bonded Doge across
every depth transition (extracted from the departing level's entities
before that level's snapshot is captured, re-appended -- relevel'd,
next to the player -- to whichever entities the new depth ends up
with), so it follows the player from level to level exactly like the
player's own entity does. `rdescent/commands.lisp`'s `process-enemy-
turns` dispatches a bonded Doge to its own `companion-ai-turn` (attack
the nearest hostile monster in reach; else approach the nearest one
within `*rdescent-companion-aggro-radius*`; else simply follow the
player) ahead of the ordinary disposition-based dispatch, and its
generic hostile-monster branch will attack an adjacent bonded Doge
instead of the player when only the Doge is in reach -- a killed Doge
becomes an inert corpse with its `bonded` slot cleared, so
`find-bonded-companion` no longer finds one and a fresh wild Doge can
appear on some later level. `rdescent/persistence.lisp` round-trips
`companion`'s own `bonded` slot through save/restore, alongside every
ordinary `entity` field.

A stray "Office Doge" that the player can discover, befriend, and keep
as a loyal companion for the rest of the run — the one entity in the
dungeon that fights *for* you instead of against you.

- **Discovery:** Doge spawns like any other `entity` on the floor, at a
  modest per-level chance, using `disposition :friendly` from
  construction (the same pattern already established by
  `npc-fixture`, §11/`ARCHITECTURE_PLAN.md`) rather than the
  `:hostile` default `initialize-instance :after ((enemy enemy) ...)`
  gives every other `enemy` subclass. It's visually distinct on the
  playing-field grid (its own glyph/`message-color`, e.g. a warm gold)
  and shows up in the `?` legend and §16-style flavor text the first
  time it's spotted, so the player recognizes it as special rather
  than just another neutral fixture.
- **Bonding:** walking adjacent to (or onto) Doge for the first time
  "adopts" it — flip a `game-state`/player flag (reusing the `flags`
  infrastructure from `ARCHITECTURE_PLAN.md` §9, e.g. `:has-doge`)
  and from that point on Doge's AI switches from "wander" to "follow
  the player, path toward the nearest hostile entity within range and
  attack it." This reuses the existing enemy-turn energy/speed
  scheduling (`entity-energy`/`entity-speed`, see `*rdescent-move-
  energy-cost*`/`*rdescent-attack-energy-cost*`'s docstring) rather
  than inventing a parallel turn system — Doge is simply an `entity`
  whose AI target-selection is inverted (nearest `:hostile`-disposed
  enemy instead of the player) and whose movement destination biases
  toward the player's own position when no enemy is in range.
- **Cannot be attacked by the player:** the movement/attack-resolution
  path that currently checks `entity-disposition-toward` before
  deciding whether stepping into an occupied tile is a move-block, an
  attack, or (per `commands.lisp`'s existing `(eq disposition
  :friendly) ...` handling) a "talk to NPC" no-op needs a fourth
  branch for a bonded companion: swap positions with the player (so
  Doge doesn't wall you into a corner as it follows) rather than
  resolving as either combat or an NPC-interaction prompt.
- **Doge can be killed:** Doge is *not* invincible — a hostile
  `enemy`'s attack can still reduce its HP to 0 via the same
  `resolve-attack-on-player`-adjacent combat resolution path used for
  every other `entity`, just re-targeted at Doge instead of the
  player. Losing Doge mid-level clears the `:has-doge` flag and drops
  a small in-fiction message-log line (a moment of genuine loss, in
  keeping with the game's deadpan corporate humor — "Doge has been
  let go in the latest round of restructuring.").
- **Replacement on the next level:** if the player currently has no
  Doge (never adopted one, or lost one to combat), the next `use-
  stairs` transition (`actions.lisp`'s `use-stairs`) guarantees a
  fresh Doge spawn on the new level regardless of the normal per-level
  spawn-chance roll — mirroring the "you'll always find a new one, but
  never two at once" framing in the request. If the player already has
  a bonded Doge, it persists across `use-stairs` the same way the
  player entity itself does (carried over in the new level's
  `game-state` rather than re-spawned), so descending doesn't strand
  a loyal companion on the floor above.
- **Balance considerations:** Doge should be a meaningful but not
  run-trivializing damage source — low HP/attack relative to the
  player's own numbers (so it dies to sustained enemy fire rather than
  soaking every hit), and it should not itself grant XP/loot on death
  (killing your own pet, even accidentally via friendly fire that
  doesn't exist, should never be incentivized). No `active-effects`
  (§6) or equipment (§13) interactions are needed for a first version;
  those are natural follow-ups (e.g. a "Doge Treats" consumable that
  heals it) once the base mechanic is proven out.

---

*Cross-reference: `TECHNICAL_DEBT.md` tracks code-quality issues in the
already-implemented mechanics; this file tracks the mechanics
themselves that remain to be implemented. Several items above
(SENIORITY, SYNERGY, HYGIENE's formulas) are already fully specified in
`engine.lisp`'s `entity` class docstring — implementing the consuming
system is the only remaining work, no `entity` slot or default value
changes are needed.*
