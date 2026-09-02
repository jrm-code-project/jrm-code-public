# Recursive Descent — Engine & Server Architecture Plan

`FUTURE_PLANS.md` describes *what* we want to build; this document is
about *what has to change in `engine.lisp`/`server.lisp` first* so
that most of those features can be added as incremental, additive
work rather than each one requiring its own one-off hack. Where
`TECHNICAL_DEBT.md` tracks problems with code that already exists,
this tracks **structural gaps** — places where today's data model or
control flow is too narrow (hardcoded to exactly the mechanics that
exist right now) to hang a planned feature off of without a rewrite.

Each section below: what exists today (with citations), which
`FUTURE_PLANS.md` sections it blocks, and the concrete refactor
proposed. Section 12 gives a suggested build order, since several of
these are prerequisites for others.

---

## 1. Generalized Status-Effect Framework

**Today:** `entity` has exactly one status-effect slot,
`confused-turns` (`engine.lisp:358`), a bare countdown integer
consulted by exactly two places (`process-enemy-turns`'s confused
branch and `confused-entity-turn`). There is no way to represent a
second simultaneous effect, no magnitude (only a duration), and no
generic "is this entity affected by X" query — every future debuff
would otherwise need its own hardcoded slot plus its own hardcoded
branch in `process-enemy-turns`/`move-player`, the same copy-paste
problem `TECHNICAL_DEBT.md` already flagged once for D&D modifiers.

**Blocks:** §6 Status Effects/Debuffs Framework, §5 Varied Attack
Effects, §17 Pharmacy (Food Poisoning, Comedown, Adderall/Modafinil
buffs), §15 Rare/Legendary items with timed effects (Out-of-Office
invisibility window), §18 Whiteboard mechanics (Boredom paralysis,
Sea-Lioning confusion), and SENIORITY's still-unimplemented Deflection
Chance (`entity`'s own docstring already specifies the formula).

**Proposed change:**
- Replace `confused-turns` with a single `active-effects` slot: a
  list of immutable `status-effect` instances, each with `kind`
  (a keyword — `:confused`, `:burnout`, `:food-poisoning`, `:slow`,
  `:crit-boost`, ...), `turns-remaining`, and an optional `magnitude`
  (e.g. the per-tick HP drain amount for Food Poisoning). Confusion
  becomes just the first `kind` rather than a bespoke slot.
- One generic entry point, `entity-effect (ent kind)`, returns the
  matching `status-effect` or NIL — replaces every direct
  `entity-confused-turns` read.
- One generic apply/tick function, `tick-status-effects (ent)`,
  called from `accrue-energy`/`reduce-tick`'s existing per-entity
  sweep: decrements every effect's `turns-remaining`, applies any
  per-tick `magnitude` (HP drain, HP regen), drops expired effects,
  and returns the updated entity — this is the one place a new debuff
  with a per-tick side effect gets wired in, not a new branch in
  `reduce-tick` itself.
- `process-enemy-turns`'s confused-branch dispatch generalizes to "if
  `(entity-effect ent :confused)`, run `confused-entity-turn`" —
  mechanically identical to today, just reading through the new
  generic slot instead of the old dedicated one.
- SENIORITY's Deflection Chance (already documented in `entity`'s own
  docstring) becomes the single gate a new `apply-status-effect
  (ent kind turns &optional magnitude)` helper rolls before actually
  attaching an effect — the one function every debuff-inflicting
  attack/item/trap calls, so Deflection Chance is automatically
  correct for all of them rather than needing to be re-derived per
  effect.

## 2. Faction & Aggression Model

**Today:** `enemy` (`engine.lisp:573`) is a marker subclass with no
faction/allegiance data at all — `process-enemy-turns` unconditionally
treats every `is-alive` entity in FOV as a hostile, distance-gated
melee-or-approach attacker of the player specifically. There is no
concept of an enemy being Neutral, Hostile to another *enemy*, or
friendly to the player.

**Blocks:** §1 Monster Classes & Faction Aggression (HYGIENE's
already-specified Suit/feral hostility bands), §18 Faction Warfare
turf war and "All-Hands Meeting" event, §11 NPCs & Quest Givers
(a non-hostile actor needs to *not* fall into the existing attack
branch), §2's bestiary entries that flee rather than fight (Code
Monkeys fleeing the Agile Scrum Master Certificate, the CEO's
Middle-Manager-below flee-in-terror per the C-Suite Keycard).

**Proposed change:**
- Add a `faction` slot to `entity` (default `:neutral`) and a
  `disposition` slot — one of `:hostile`, `:neutral`, `:friendly`,
  `:fleeing` — computed once at spawn time from faction rules (and,
  per HYGIENE's docstring, the player's own HYGIENE band) rather than
  hardcoded.
- A single `entity-disposition-toward (ent target)` predicate
  `process-enemy-turns` consults before choosing an AI branch —
  hostile-and-in-range attacks, hostile-out-of-range approaches,
  neutral wanders (a new, simple random-walk branch, reusing
  `confused-random-step`'s undirected movement rather than a new
  primitive), friendly does nothing hostile (quest-giver NPCs, §11),
  fleeing moves *away* from its aggressor (mirrors the existing
  greedy-approach step with the sign flipped).
- Faction-vs-faction combat (§18 turf war) becomes: when resolving an
  entity's turn, check for another **non-player** entity adjacent
  whose faction this one is hostile toward, and run the same
  `resolve-attack-on-player`-style combat math against it instead —
  this requires generalizing `resolve-attack-on-player` (see §5 below)
  to attack any entity, not just the player specifically.

## 3. Fixture / Interactable Entity Hierarchy

**Today:** the only "non-hostile, non-player" entities are
`ground-item` (picked up automatically by walking over it —
`grab-item`) and the stairs markers (`make-stairs-up`/`make-stairs-
down`, walked over to trigger `use-stairs`). There is no entity type
representing something the player must *deliberately act on* — no
vendor, shrine, trap, locked door, key, or NPC currently has anywhere
to live in the class hierarchy.

**Blocks:** §8 Vendors/Shops, §9 Free-Standing Shrines, §11 NPCs &
Quest Givers, §8's Traps & Hidden Enemies (stationary triggered
entities), §9 Keys & Locked Doors — essentially every fixture-flavored
section in `FUTURE_PLANS.md`.

**Proposed change:** a new intermediate class, `fixture` (subclass of
`entity`, sibling to `enemy`/`ground-item`), for anything stationary
and non-hostile-by-default that the player interacts with via an
explicit action rather than colliding with:
- `fixture` itself carries no new slots beyond `entity`'s own; it
  exists purely so `process-enemy-turns`'s `acting-entities` filter
  can exclude it (`FIXTURE`s never get an AI turn, like `ground-item`
  already never does via `is-alive nil`) without needing a new
  per-type special case.
- **`trap-fixture`** (§8): `is-alive t` so it can be attacked and
  destroyed once revealed, invisible until triggered — rendering
  needs a `hidden-p` reader consulted by whatever draws the grid, and
  SENIORITY's Detection Chance (already documented) is the roll that
  flips `hidden-p` to false the moment it enters FOV. Stepping onto
  its tile reuses `resolve-attack-on-player`-style combat math (§2's
  generalization already gets us "attack anyone, not just the
  player") rather than a parallel damage-dealing code path.
- **`shrine-fixture`** (§9)/**`vendor-fixture`** (§8): both need a
  `use-count` (or infinite) slot and an `interact` command handler
  (see §7 below) rather than being triggered by movement at all.
- **`npc-fixture`** (§11): `disposition :friendly` by construction (so
  §2's faction model already keeps `process-enemy-turns` from ever
  attacking through it), plus a `quest` slot (see §9 below).
- **`door-fixture`**/**locked-door tile** (§9): this one is arguably a
  `tile` concern rather than an `entity` — a locked door blocks
  movement like a wall but becomes walkable once the player holds the
  matching key; needs a new `tile` subclass (or a `locked` slot added
  to the existing `tile` class, keyed off which key ID unlocks it)
  rather than an `entity`, since `blocking-entity-at` already only
  ever consults entities, and `map-tile-ref` is the actual walkability
  check `move-player` uses.

## 4. Equipment System & Dynamic Stat Computation

**Today:** `power`/`defense`/`max-hp` (`engine.lisp:340-352`) are flat
per-entity numbers baked in at construction (`make-orc`/`make-troll`/
`make-initial-state`) and never recomputed — there is no equipment
slot on `entity` at all, and `inventory` items are strictly one-shot
consumables (`use-item` always removes the item from the list).

**Blocks:** §13 Equipment System & Armory (all four gear slots), §12
Rare & Legendary Loot (most of which are equippables with passive,
always-on effects rather than one-shot consumables), §14 Class
Abilities if any ability is meant to be a passive-while-equipped
effect.

**Proposed change:**
- Add an `equipment` slot to `entity`: an alist/plist keyed by slot
  name (`:weapon`, `:body`, `:head`, `:off-hand`), each value either
  NIL or an `equippable-item` (a new `rdescent-item` subclass,
  alongside the existing `targeted-item`/`area-effect-item`, carrying
  its own `slot`, `stat-bonuses` plist, and optional `on-hit-effect`).
- **Do not** store `power`/`defense`/`max-hp` as the effective combat
  numbers anymore for the player specifically — introduce
  `effective-power (ent)`/`effective-defense (ent)`/`effective-max-hp
  (ent)` functions that start from the entity's own base slot value
  and fold in every equipped item's `stat-bonuses`, and change every
  *combat* call site (`resolve-attack-on-player`, `process-enemy-
  turns`'s attacking branch, `move-player`'s melee branch) to call
  these instead of the raw `power`/`defense`/`max-hp` readers. This
  keeps `entity`'s own base slots meaningful (a monster's innate
  stats) while making gear purely additive, computed fresh every time
  rather than mutated into the base slot (matching the "never mutate,
  always derive" discipline the rest of the file already follows for
  `update-entity`).
- `equip-item`/`unequip-item` reducers (see §7, new commands) move an
  item between `inventory` and `equipment` — no combat-math change
  needed at the point of equipping, since `effective-*` recomputes
  from `equipment` on every read.
- Weapon reach/rate-of-fire (§13's Ethernet Cable whip, Telescoping
  Pointer spear, Rubber Band Gatling Gun) is a property of whichever
  `equippable-item` occupies `:weapon`, consumed by §5's generalized
  attack resolution below rather than being a fixed melee-range-1
  assumption.

## 5. Generalized Attack/Weapon Resolution

**Today:** all combat is hardcoded melee, range 1, one hit per turn:
`resolve-attack-on-player` and `process-enemy-turns`'s attacking
branch both assume adjacency (`distance <= 1`), a single damage
computation, and no on-hit side effect beyond damage. `move-player`'s
own melee-into-an-enemy branch duplicates a third variant of the same
math.

**Blocks:** §13's ranged/reach weapons (Ethernet Cable whip: 2-tile
reach; Telescoping Pointer: pierces through to a second target;
Rubber Band Gatling Gun: 3 hits/turn; Reply-All Blunderbuss: cone
AoE), §12's on-hit status effects (LART silences and strips armor;
Carpal-Tunnel-inflicting Kinesis keyboard), §14 Class Abilities if any
are attack-shaped.

**Proposed change:**
- Extract a single `resolve-attack (attacker defender)` function
  (generalizing today's player-only `resolve-attack-on-player`, per
  §2's faction combat need above) that reads `attacker`'s equipped
  weapon (§4) for `reach` (default 1, i.e. today's behavior),
  `hits-per-turn` (default 1), and `on-hit-effect` (default none,
  otherwise dispatched through §1's `apply-status-effect`), and
  `defender`'s dodge/armor stats — this becomes the one place damage
  math and on-hit effects are computed, called by `move-player`'s
  melee branch, `process-enemy-turns`'s attacking branch, and any
  faction-vs-faction combat, instead of three independently
  hand-derived copies.
- `process-enemy-turns`'s `distance <= 1` attack-range gate generalizes
  to `distance <= (weapon-reach (effective-weapon ent))` so a
  reach-2 monster (none currently planned, but the framework should
  not assume only the player gets ranged options) or a future player
  Telescoping Pointer both fall out of the same check.
- Cone/AoE weapons (Reply-All Blunderbuss, Compressed Air) reuse
  `area-effect-item`'s existing Chebyshev-radius-scan pattern
  (`engine.lisp:794`) rather than inventing a new geometry primitive —
  the main new piece of geometry work is a *cone* (a subset of tiles
  within radius *and* within some angular arc of the attacker's facing/
  target direction), which today's item system has no notion of at
  all (items only ever have a single aimed-at tile, not a facing).

## 6. Dungeon Metadata: Room Types & Depth-Parameterized Generation

**Today:** `generate-dungeon` (`engine.lisp:1694`) produces a flat
`tile` array (`walkable`/`char` only, `engine.lisp:600`) with no
persisted notion of "this region is a Cubicle Farm vs. an Open Office"
— `rect-room` (`engine.lisp:1591`) is explicitly documented as
"purely a generation-time scratch value — never stored in a GAME-MAP
or GAME-STATE." Room shape/size is uniform across all depths, and
`spawn-monsters-for-level` ignores `level` entirely for count/
composition (already flagged, this is exactly `FUTURE_PLANS.md` §3's
own analysis).

**Blocks:** §4 Room Generation Modifiers by Depth, §18's Open Office
Stealth Penalty (which needs to know, per room, whether footsteps are
masked), §3 Graduated Enemy Difficulty by Depth.

**Proposed change:**
- Extend `tile` with a `room-kind` slot (default `nil`/corridor) —
  `:cubicle`, `:open-office`, `:server-room`, etc. — stamped by
  `dig-room` at carve time based on a `level`-derived room-profile
  table (wall density / min-max size / open-plan probability, per §4)
  rather than existing default uniform rectangles for every depth.
  This is additive to `tile`, not a new persisted structure, so the
  existing `*dungeon-cache*` (keyed on `(tier . level)`,
  `engine.lisp:1649`) continues to work unchanged — room type is a
  deterministic, immutable function of `(tier, level, seed)` exactly
  like the tile layout already is, so it is safe to cache alongside
  it.
- **Important distinction to preserve:** room *type* is
  cacheable/shared dungeon geometry (like the tile layout itself),
  but any *fixture with mutable per-player state* (a shrine's
  `use-count`, an NPC's quest-progress, a trap's `hidden-p` once
  triggered) must **not** be baked into the shared cached `game-map` —
  it has to live in each player's own `(get-entities state)` list,
  exactly the way monsters and ground items already do today via
  `spawn-monsters-for-level`/`spawn-items-for-level`. The temptation
  to "just add a locked-door flag to the shared tile" has to be
  resisted for anything a specific player's actions can change; only
  the room-*type* tag (never mutated post-generation) is safe to
  share.
- A `room-acoustics (room-kind)` lookup (`:cubicle`/`:server-room` →
  `:muffled`, `:open-office` → `:loud`) is the one new predicate §18's
  stealth mechanic needs — consulted wherever a "does this action
  alert nearby enemies" check would go (a new concern; no code path
  currently checks this at all, so this is new logic, not a
  refactor of existing logic).

## 7. Depth-Aware Spawn Tables

**Today:** `spawn-monsters-for-level` (`engine.lisp:1758`) hardcodes a
flat 0–2-per-room count and an 80/20 Orc/Troll split regardless of
`level`; `spawn-items-for-level` (`engine.lisp:1800`) similarly has no
depth gating. Both already take `level` as a parameter (used only for
RNG seeding) so no signature change is needed, only new internal
logic.

**Blocks:** §3 Graduated Enemy Difficulty by Depth (already scoped in
`FUTURE_PLANS.md` in detail), and, transitively, every bestiary tier
in §2, every pharmacy/equipment drop table in §12/§13/§17, and every
fixture type in §3 once fixtures exist (§3 above).

**Proposed change:**
- A single, declarative `*rdescent-spawn-table*` structure — a list of
  `(kind min-depth max-depth weight factory)` entries covering
  monsters, ground items, *and* (once §3 exists) fixtures — replacing
  the currently-separate, currently-hardcoded monster/item spawn
  logic with one depth-filtered weighted-choice helper both functions
  call. This is the natural place to hang §2's tier-gating rule
  ("Cubicle Farm roster eligible early, Executive Washroom bosses only
  at the final level(s)") and §12/§13/§17's "rare items only drop past
  depth N" rules without a bespoke `cond` ladder per feature.
- Spawn *count* scaling with depth (§3's 0–2 → 1–3 → 2–4 progression)
  and enemy strength scaling (a depth-derived HP/power multiplier)
  become parameters read from the same table/profile rather than
  literals in `spawn-monsters-for-level`'s body.
- Locked doors (§9) need a **generation-time reachability
  constraint** unlike anything the spawner does today (every current
  spawn is placement-order-independent; a locked door must guarantee
  its key is reachable *without* passing through the door it locks) —
  this is new graph-reachability logic (a flood-fill/BFS over the
  room-connectivity graph `generate-dungeon` already builds via
  `dig-tunnel`, checked before finalizing key/door placement), not a
  generalization of existing spawn logic.

## 8. Command Layer Extensions

**Today:** `rdescent-command` (`engine.lisp:2683`) is already a clean,
extensible CLOS hierarchy — one subclass per command, dispatched via
`execute-immediate-command`/`execute-queued-command` generic
functions rather than a `case` on a keyword. This is the *one* part
of the engine already shaped correctly for future growth; no
structural change is needed here, only new subclasses following the
existing pattern.

**Blocks (by omission, not by design flaw):** §8 Vendors (needs a
"purchase" command), §9 Shrines (needs an "interact" command), §11
NPCs (needs "accept-quest"/"talk"), §13 Equipment (needs "equip"/
"unequip"), §9 Locked Doors (needs "unlock", or simply reuses
`move-player`'s existing walk-into-a-tile flow if a held key silently
auto-unlocks — a design choice to make explicitly rather than
accidentally).

**Proposed change:** add each new command as a new
`rdescent-command` subclass plus one method per generic function,
exactly matching the existing `move-command`/`use-item-command`/
`drop-command` pattern (`engine.lisp:2694-2734`) and
`parse-rdescent-command`'s existing `cond`/`when-let*` shape
(`engine.lisp:2793`). Concretely:
- `interact-command` (no payload — like `grab-command`, the target
  fixture is inferred from the player's current position, reusing the
  existing "what's on this tile" lookup pattern `blocking-entity-at`/
  `ground-item-at` already establish, generalized to a
  `fixture-at (state x y level)` helper) — dispatches on the
  found fixture's own class (shrine vs. vendor vs. NPC vs. locked
  door) inside the reducer, not inside the command itself.
- `equip-command`/`unequip-command` (an `item-index`, like
  `drop-command`).
- `accept-quest-command`/`purchase-command` as needed once §11/§8 are
  actually implemented — deferred until those systems exist, but the
  slot to extend is now obvious.

## 9. Game-State Extensibility: Flags, Logs, and Win/Lose State

**Today:** `game-state` (`engine.lisp:616`) has a fixed, hand-enumerated
set of slots (`player`, `entities`, `map`, `current-depth`, `levels`,
`explored`, `message-log`). There is no generalized "arbitrary
per-run flag/counter" bag, no quest log, no collection-log, no keys-
held set, no per-floor turn counter, no active-global-event state, and
no win/loss flag beyond the player's own `is-alive`.

**Blocks:** §16 Scavenger Hunt Collectibles (`collection-log`), §9
Keys & Locked Doors (keys held), §11 NPCs & Quest Givers (quest
progress), §5 Win Condition (a `:victory` flag distinct from death),
§18's Agile Sprint doom-clock (a per-floor turn counter) and All-Hands
Meeting event (a global timed-event flag).

**Proposed change:** rather than adding one bespoke slot per feature
(which is exactly how `game-state` would organically sprawl into an
unmaintainable pile of one-off accessors), add a single `flags` slot:
an immutable `fset:map` from keyword to arbitrary immutable value,
following the same "persistent functional map, updated via
`update-game-state`-style copy-on-write" discipline `levels` already
established (`engine.lisp:616`'s own docstring specifically justifies
`fset:map` for exactly this "extend without breaking the functional-
update discipline" reason). Concretely:
- `collection-log` → `(fset:@ (get-flags state) :collection-log)`, a
  bitmask/set of collected scavenger-hunt item IDs.
- `keys-held` → a set of key IDs.
- `quest-log` → an alist of `(quest-id . progress)`.
- `turn-counter`/`sprint-deadline` → a plain integer, decremented by
  `reduce-tick` alongside `accrue-energy`'s existing per-tick sweep.
- `victory`/`game-over-reason` → a keyword, checked by
  `apply-rdescent-command`/`advance-game-state`'s existing `is-alive`
  short-circuit, generalized to "is-alive and no terminal flag set."
- This keeps `game-state`'s own class definition stable (no repeated
  `defclass` edits, no repeated `update-game-state` signature changes)
  while still giving every future system a well-defined, immutable,
  independently-testable place to keep its data — new features become
  "read/write one more keyword in `flags`" rather than "add a slot to
  the core state class and thread it through every constructor."

## 10. Persistence: Pure Serialize/Deserialize

**Today:** the only existing persistence is
`rdescent-suspend-game-state`/`rdescent-resume-game-state`
(`server.lisp:387-413`) — an **in-memory**, ephemeral hash table keyed
by session ID with a 5-minute grace period, entirely lost on server
restart, and never touching the network or disk. There is no function
anywhere that turns a `game-state` into plain, JSON-safe data (or
back), because nothing has ever needed to leave the Lisp image.

**Blocks:** §19 Save/Restore via Signed Client-Side State outright —
that section's HMAC-signing design is entirely about *how to trust* a
serialized blob; it has an unstated prerequisite of *being able to
produce one* at all, which does not exist today.

**Proposed change:**
- A pure `serialize-game-state (state)` → plist/alist function
  (walking `player`, `entities`, `current-depth`, `levels`, `explored`,
  and (once §9 lands) `flags`) and its inverse
  `deserialize-game-state (plist)` → `game-state`, round-tripping
  through `cl-json` (already a dependency, used elsewhere for command
  parsing). This is a substantial function precisely because
  `game-state` intentionally holds rich CLOS objects (entities,
  items, an `fset:map`) rather than plain data — every `entity`
  subclass (`enemy`, `ground-item`, and once §3 lands `fixture` and
  its own subclasses) needs a stable, versioned tag in the serialized
  form so `deserialize-game-state` can reconstruct the right class,
  and every `rdescent-item` subclass needs the same for `inventory`/
  `equipment` contents.
- A `save-format-version` field baked into the serialized blob from
  day one, even before any format change actually happens — cheap
  insurance so a future engine change (a new `entity` slot, a renamed
  class) can detect and reject (or migrate) an old client-held save
  instead of silently deserializing garbage.
- Once this exists, §19's HMAC-signing work is a thin wrapper: sign
  `serialize-game-state`'s output, verify-then-`deserialize-game-state`
  on restore — no further engine change needed beyond this section.

## 11. Suggested Build Order

Several of the above are prerequisites for others; recommended
sequencing (each phase assumes the previous is done and tested):

1. **§1 Status Effects** and **§2 Faction/Disposition** — smallest,
   most self-contained data-model additions; needed by almost
   everything else.
2. **§6 Room Metadata** and **§7 Depth-Aware Spawn Tables** — the
   `FUTURE_PLANS.md` §3/§4 work already scoped in detail; mostly
   additive to existing generation functions, low risk.
3. **§3 Fixture Hierarchy** and **§8 Command Layer Extensions** — these
   two are really one unit of work (a new fixture type is useless
   without the command that lets a player act on it); build them
   together, one fixture type at a time (shrines first — simplest,
   §9 already fully designed — then vendors, then NPCs, then traps,
   then locked doors, in roughly that order of complexity).
4. **§4 Equipment System** and **§5 Generalized Attack Resolution** —
   these two are also one unit of work in practice (equipment is
   inert without combat math that reads it); do this after fixtures
   so vendors/shrines have something to sell/dispense into an
   already-working equipment slot.
5. **§9 Game-State Flags** — can actually be done any time after step
   1, but is listed here because most of its concrete uses
   (collection-log, quest-log, keys-held) only become *useful* once
   the corresponding feature (scavenger hunt, quests, locked doors)
   exists; the `flags` slot itself is cheap to add early, though, and
   doing so *before* the features land avoids ever having to retrofit
   `game-state`'s class definition later.
6. **§10 Serialization** — last, since it needs every other data
   model (equipment, fixtures, flags) to be reasonably stable first;
   redoing serialization every time a new slot is added elsewhere is
   wasted effort if done too early.

Each phase should land with its own test coverage (per this project's
existing FiveAM convention, `tests/tests.lisp`) before the next phase
builds on it, exactly as the tick/energy-system and technical-debt
work earlier this project cycle was validated against the full suite
at each step.

---

*Cross-reference: `FUTURE_PLANS.md` tracks the gameplay features this
document exists to enable; `TECHNICAL_DEBT.md` tracks quality issues
in code that already exists. This document is scoped narrowly to
*structural* engine/server changes — it does not re-describe any
gameplay mechanic already fully specified in `FUTURE_PLANS.md`, only
the data-model/control-flow gaps that stand between today's engine and
building those mechanics.*
