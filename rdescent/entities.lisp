;;; -*- Lisp -*-

;;; Pure, immutable value types for "Recursive Descent": the CLOS
;;; classes ENTITY/ENEMY/TILE/GAME-MAP/GAME-STATE, plus their pure
;;; factories (MAKE-STAIRS-UP/MAKE-STAIRS-DOWN/SPAWN-STAIRS-FOR-LEVEL,
;;; the FUTURE_PLANS.md §16 scavenger-hunt COLLECTIBLE-ITEM/
;;; COLLECTIBLE-SET/AUTO-PICKUP-ITEM/MAKE-COLLECTIBLE) and the small
;;; pure helpers (UPDATE-ENTITY, XY-TO-INDEX, HEALTH-PERCENTAGE,
;;; GROUP-INVENTORY-FOR-DISPLAY, FORMAT-XP-FOR-HTML/FORMAT-RSU-FOR-HTML
;;; -- all at the end of this engine's own file chain, see RDESCENT/
;;; ENTITY-HELPERS.LISP's own header comment) and game-balance
;;; constants that operate directly on them. The concrete ENEMY
;;; subclasses (ORC/TROLL) each live in their own file --
;;; RDESCENT/ENEMIES/ORC.LISP/RDESCENT/ENEMIES/TROLL.LISP -- alongside their own
;;; MAKE-ORC/MAKE-TROLL factory and every method specialized on them,
;;; so that adding a new monster type never requires editing this file.
;;;
;;; The RDESCENT-ITEM inventory-item hierarchy (TARGETED-ITEM/
;;; AREA-EFFECT-ITEM/CONSUMABLE-ITEM/EQUIPPABLE-ITEM and their many
;;; concrete subclasses) and GROUND-ITEM now live under RDESCENT/ITEMS/
;;; -- see RDESCENT/ITEMS/BASE.LISP's own header comment for that
;;; subdirectory's own file map -- so that this file stays focused on
;;; the core entity/map/game-state value types rather than growing
;;; without bound alongside every new item this game adds.
;;;
;;; This is the first of several files this engine was split across
;;; (originally a single ENGINE.LISP) -- see RDESCENT/ITEMS/*.LISP (the
;;; item hierarchy, see above), RDESCENT/ENTITY-HELPERS.LISP (the
;;; small pure entity helpers/factories listed above, loaded right
;;; after RDESCENT/ITEMS/* since GROUP-INVENTORY-FOR-DISPLAY needs
;;; EQUIPPABLE-ITEM), RDESCENT/MECHANICS.LISP (message-log/combat-
;;; message/attack-flavor text, tick/energy/healing reducers, and the
;;; MAKE-INITIAL-STATE/MAKE-INITIAL-MAP factories), RDESCENT/ENEMIES/
;;; ORC.LISP/RDESCENT/ENEMIES/TROLL.LISP (the concrete ENEMY subclasses,
;;; one file per subclass -- see their own header comments), RDESCENT/
;;; DUNGEON.LISP (procedural dungeon generation and field-of-view),
;;; RDESCENT/ACTIONS.LISP (the player-action reducers MOVE-PLAYER/
;;; USE-STAIRS/DRINK-POTION/USE-ITEM/GRAB-ITEM/DROP-ITEM), and
;;; RDESCENT/COMMANDS.LISP (command parsing/dispatch, combat
;;; resolution, PROCESS-ENEMY-TURNS, and the top-level APPLY-RDESCENT-
;;; COMMAND/ADVANCE-GAME-STATE reducers) for the rest. None of these
;;; files have a Hunchensocket dependency, spawn threads, or generate
;;; JSON packets -- see RDESCENT/SERVER.LISP for the imperative I/O
;;; shell built on top of the values and reducers defined across all
;;; of them.

(in-package "JRM-CODE-PROJECT")

;;; Forward declarations: these two special variables are defined
;;; further down the RDESCENT load order -- *RDESCENT-ITEM-DURABILITY-
;;; LOSS-PER-HIT* in RDESCENT/ITEMS/BASE.LISP and *RDESCENT-C-SUITE-
;;; KEYCARD-MAX-XP* in RDESCENT/ITEMS/LOOT.LISP -- but this file's
;;; APPLY-EQUIPMENT-WEAR/ENTITY-DISPOSITION-TOWARD reference them
;;; already. ASDF loads the whole system before either function is
;;; ever called, so this is safe at runtime; the DECLAIM below just
;;; tells the compiler these symbols will be special variables so it
;;; doesn't emit spurious "undefined variable" warnings for a load-
;;; order forward reference that resolves before any call site runs.
(declaim (special *rdescent-item-durability-loss-per-hit*
                   *rdescent-c-suite-keycard-max-xp*))

(defun rdescent-safe-log-warning (format-string &rest args)
  "Best-effort diagnostic logging for code in this file that may run
outside an ordinary Hunchentoot request thread (the game loop and
registry actor threads spawned below, in particular). HUNCHENTOOT:
LOG-MESSAGE* has been observed in production to itself signal
UNBOUND-VARIABLE when called from a thread with no live request/
acceptor bound -- exactly the kind of thread this file's background
loops run on -- so a failure to log must never be allowed to escape
and take down the caller (which, for BROADCAST-STATE/START-GAME-LOOP,
would defeat the entire point of wrapping calls in HANDLER-CASE in the
first place: see TECHNICAL_DEBT.md items #28-29). This wraps the
LOG-MESSAGE* call in its own IGNORE-ERRORS and falls back to a plain
FORMAT on *ERROR-OUTPUT* if it fails, so callers always get *some*
diagnostic output rather than silently losing both the original error
and the logging attempt itself."
  (unless (ignore-errors (apply #'hunchentoot:log-message* :warning format-string args) t)
    (ignore-errors
      (apply #'format *error-output* (concatenate 'string "~&rdescent: " format-string "~%") args))))

(defparameter *rdescent-field-width* 100
  "Width, in characters, of the Recursive Descent playing field.")

(defparameter *rdescent-field-height* 33
  "Height, in lines, of the Recursive Descent playing field.")

(defparameter *rdescent-tick-seconds* 0.05
  "Heartbeat interval, in seconds, between successive TICK-ALL-CLIENTS
calls (see RDESCENT/SERVER.LISP's START-GAME-LOOP/RDESCENT-TICK-
EVENTS, the only place this actually paces real wall-clock time) -- a
hardcoded 20 ticks/second (50ms), matching the player's ENTITY-SPEED
of 50 ENERGY/tick (so *RDESCENT-MOVE-ENERGY-COST* of 100 recovers
every 2 ticks, i.e. 10 moves/second) and each monster's own slower
ENTITY-SPEED (see MAKE-ORC/MAKE-TROLL). Deliberately defined here,
in ENTITIES.LISP (loaded first, see JRM-CODE-PROJECT.ASD's \"rdescent\"
module -- SERVER.LISP, which actually consumes it for the game loop,
loads last), rather than alongside START-GAME-LOOP where it
conceptually belongs, precisely so that every other DEFPARAMETER in
this file (and RDESCENT/MECHANICS.LISP/RDESCENT/ENEMIES/*.LISP) that
needs to convert a real-world effect duration in *seconds* into an
engine-tick count (see, e.g., *RDESCENT-CONFUSION-TICKS*/
*RDESCENT-ANALYSIS-PARALYSIS-TICKS* below) can do so directly in its
own DEFPARAMETER form via (ROUND (/ SECONDS *RDESCENT-TICK-SECONDS*))
-- a DEFPARAMETER's value form evaluates immediately at load time
(unlike a function body, which only evaluates when called), so it
cannot forward-reference a special variable bound later in load
order. Every STATUS-EFFECT's TICKS-REMAINING (see TICK-STATUS-
EFFECTS, RDESCENT/MECHANICS.LISP) is decremented once per *engine*
tick -- i.e. once every *RDESCENT-TICK-SECONDS* real seconds -- not
once per monster/player turn (turns happen far less often than
ticks, gated by ENTITY-ENERGY/ENTITY-SPEED), so a debuff duration
expressed as a small tick count under the mistaken assumption that
\"one tick\" means \"one turn\" would in practice expire almost
instantly; every tick-based duration in this codebase must therefore
be derived from an intended real-world *seconds* duration via this
constant, not chosen as a bare tick-count literal.")

(defparameter *rdescent-default-membership-tier* "CONS"
  "Membership tier assumed for a connected client when no valid JWT
(cookie or Authorization header) is presented, matching this
codebase's existing free-tier default (see AUTH.LISP/ADMIN.LISP's
similar fallback to \"CONS\" via USER-MEMBERSHIP-TIER).")

(defun rdescent-tier-max-depth (tier)
  "Return the deepest dungeon LEVEL a client presenting membership TIER
is permitted to descend to: 8 if TIER is NIL (\"none\" -- no JWT was
presented at all, distinct from an explicit \"CONS\" tier claim), 128
for \"CONS\", 1024 for \"CADR\", 65536 for \"LAMBDA\", and 8 (the same
as no JWT) for any other unrecognized tier string, treating it as the
lowest/free tier rather than granting unbounded depth. This is the
JWT-driven gatekeeper on how far a connection may ever descend --
called once at CLIENT-CONNECTED time (see
RDESCENT-CLIENT-RAW-TIER-FROM-REQUEST in RDESCENT/SERVER.LISP) to
stamp RDESCENT-CLIENT's MAX-DEPTH slot for the lifetime of that
connection. Pure and dependency-free so it can be unit tested
directly, independent of any JWT/HTTP plumbing."
  (cond ((null tier) 8)
        ((string-equal tier "CONS") 128)
        ((string-equal tier "CADR") 1024)
        ((string-equal tier "LAMBDA") 65536)
        (t 8)))

(defun rdescent-tier-inventory-limit (tier)
  "Return the maximum number of RDESCENT-ITEMs a client presenting
membership TIER may carry in their player's own INVENTORY: 5 if TIER
is NIL (\"none\" -- no JWT was presented at all) or any other
unrecognized tier string (treated as the lowest/free tier, mirroring
RDESCENT-TIER-MAX-DEPTH's own fallback), 10 for \"CONS\" (the free
tier), and 25 for \"CADR\" or any higher tier (\"LAMBDA\") -- a paid
subscription buys more pockets as well as more depth. Enforced by
GRAB-ITEM, which refuses to add a picked-up item to an already-full
INVENTORY (KOMBUCHA charges are exempt -- see GRAB-ITEM -- since they
are tracked by a separate counter, not an INVENTORY list slot). Pure
and dependency-free so it can be unit tested directly, independent of
any JWT/HTTP plumbing."
  (cond ((null tier) 5)
        ((string-equal tier "CONS") 10)
        ((or (string-equal tier "CADR") (string-equal tier "LAMBDA")) 25)
        (t 5)))

(defun rdescent-tier-kombucha-limit (tier)
  "Return the maximum number of Kombuchas a client presenting
membership TIER may carry in their player's own KOMBUCHA counter: 5 if
TIER is NIL (\"none\" -- no JWT was presented at all) or any other
unrecognized tier string (treated as the lowest/free tier, mirroring
RDESCENT-TIER-MAX-DEPTH's/RDESCENT-TIER-INVENTORY-LIMIT's own
fallback), 10 for \"CONS\" (the free tier), and 25 for \"CADR\" or any
higher tier (\"LAMBDA\") -- the same 5/10/25 schedule as
RDESCENT-TIER-INVENTORY-LIMIT, just applied to the separate KOMBUCHA
counter rather than the INVENTORY list. Enforced by GRAB-ITEM, which
refuses to add a picked-up Kombucha once the player's KOMBUCHA count
is already at this limit. Pure and dependency-free so it can be unit
tested directly, independent of any JWT/HTTP plumbing."
  (cond ((null tier) 5)
        ((string-equal tier "CONS") 10)
        ((or (string-equal tier "CADR") (string-equal tier "LAMBDA")) 25)
        (t 5)))

(defparameter *rdescent-fov-radius* 5
  "Base player sight radius, in tiles, before adding the DOMAIN-
KNOWLEDGE-derived adjustment (see DOMAIN-KNOWLEDGE-FOV-RADIUS). Used
by MOVE-PLAYER's COMPUTE-FOV call after every step to refresh
GAME-STATE's EXPLORED mask, and by every other COMPUTE-FOV call site
that needs the player's own field of view (MAKE-INITIAL-STATE, etc.)
-- all of them go through DOMAIN-KNOWLEDGE-FOV-RADIUS rather than this
constant directly. Note this is distinct from
*RDESCENT-MONSTER-FOV-RADIUS*, which governs how far monsters can
detect the player and does NOT scale with DOMAIN-KNOWLEDGE.")

(defun ability-modifier (stat)
  "Return the classic D&D ability-score modifier for a given
\"Corporate RPG Stat\" value (see ENTITY's docstring and ROLL-STAT):
(FLOOR (- STAT 10) 2). A STAT of 16 yields +3; 10 (the average
4d6-drop-lowest roll) yields +0; 6 yields -2; 4 yields -3. Shared by
every Corporate-Stat scaling function that derives its effect from
this modifier (DOMAIN-KNOWLEDGE-FOV-RADIUS, KOMBUCHA-HEAL-AMOUNT,
DOMAIN-KNOWLEDGE-BONUS-DAMAGE, and any future scaling function for the
still-unwired SENIORITY/SYNERGY/HYGIENE stats -- see this file's
design notes and TECHNICAL_DEBT.md item #40) rather than each
inlining (FLOOR (- STAT 10) 2) independently, so the formula itself
only ever needs to change in one place."
  (floor (- stat 10) 2))

(defun domain-knowledge-fov-radius (domain-knowledge)
  "Return the player's field-of-view radius, in tiles, for a given
DOMAIN-KNOWLEDGE Corporate RPG Stat (see ENTITY's docstring):
*RDESCENT-FOV-RADIUS* (5) plus ABILITY-MODIFIER of DOMAIN-KNOWLEDGE --
knowing the codebase (domain) lets you spot the bugs (monsters) before
they spot you. A DOMAIN-KNOWLEDGE of 16 (+3 modifier) yields radius 8;
10 (the average roll, +0 modifier) yields the standard radius 5; 4 (-3
modifier) yields radius 2. Called everywhere COMPUTE-FOV is invoked
for the player's own field of view (MOVE-PLAYER, MAKE-INITIAL-STATE)
in place of *RDESCENT-FOV-RADIUS* directly. NOT used by
PROCESS-ENEMY-TURNS -- monsters' own detection radius is fixed (see
*RDESCENT-MONSTER-FOV-RADIUS*), independent of the player's
DOMAIN-KNOWLEDGE."
  (+ *rdescent-fov-radius* (ability-modifier domain-knowledge)))

(defparameter *rdescent-monster-fov-radius* 6
  "Fixed sight radius, in tiles, used by PROCESS-ENEMY-TURNS to decide
which entities are close enough to the player to be offered a turn
(via COMPUTE-FOV from the player's position). Deliberately NOT derived
from the player's DOMAIN-KNOWLEDGE (see DOMAIN-KNOWLEDGE-FOV-RADIUS,
which instead governs the player's own rendered/explored field of
view) -- a savvier player should see further, but that shouldn't also
make monsters notice them from further away.")

(defparameter *rdescent-move-energy-cost* 100
  "ENERGY cost of a single move action (a step onto open floor). See
ENTITY's ENERGY/SPEED slots' docstring for the energy-based turn
scheduler this and *RDESCENT-ATTACK-ENERGY-COST* drive: an entity may
only move once its ENTITY-ENERGY balance is at least this amount, and
that amount is then deducted from it (see MOVE-PLAYER and
PROCESS-ENEMY-TURNS).")

(defparameter *rdescent-attack-energy-cost* 150
  "ENERGY cost of a single attack action (a melee bump into a
BLOCKING-ENTITY-AT). 50% higher than *RDESCENT-MOVE-ENERGY-COST*,
reflecting that attacking takes more time/effort to recover from than
simply moving. See ENTITY's ENERGY/SPEED slots' docstring for the
energy-based turn scheduler this and *RDESCENT-MOVE-ENERGY-COST*
drive.")

(defparameter *rdescent-kombucha-heal-amount* 5
  "Base HP restored by a single Kombucha charge, before adding the
CAFFEINE-TOLERANCE-derived adjustment (see KOMBUCHA-HEAL-AMOUNT) --
see DRINK-POTION and ENTITY's KOMBUCHA slot. Drinking one costs
*RDESCENT-MOVE-ENERGY-COST* ENERGY -- the same as a single move
action -- rather than getting its own dedicated cost constant, since
drinking is documented as taking exactly one turn/tick, same as
moving.")

(defun kombucha-heal-amount (caffeine-tolerance)
  "Return the HP a single Kombucha charge restores for a given
CAFFEINE-TOLERANCE Corporate RPG Stat (see ENTITY's docstring):
*RDESCENT-KOMBUCHA-HEAL-AMOUNT* (5) plus ABILITY-MODIFIER of
CAFFEINE-TOLERANCE -- a high tolerance means your body is incredibly
efficient at processing healing items. A CAFFEINE-TOLERANCE of 16 (+3
modifier) heals 8 HP; 10 (the average roll, +0 modifier) heals the
standard 5; 6 (-2 modifier) heals only 3 HP. Called by DRINK-POTION on
the player's own CAFFEINE-TOLERANCE."
  (+ *rdescent-kombucha-heal-amount* (ability-modifier caffeine-tolerance)))

(defparameter *rdescent-player-base-max-hp* 10
  "Flat base term of CAFFEINE-TOLERANCE-MAX-HP's Max-HP formula (10 +
CAFFEINE-TOLERANCE * 2) -- see that function.")

(defparameter *rdescent-caffeine-tolerance-hp-per-point* 2
  "Per-point multiplier of CAFFEINE-TOLERANCE-MAX-HP's Max-HP formula
(10 + CAFFEINE-TOLERANCE * 2) -- see that function.")

(defun caffeine-tolerance-max-hp (caffeine-tolerance)
  "Return the player's starting MAX-HP for a given CAFFEINE-TOLERANCE
Corporate RPG Stat (see ENTITY's docstring):
*RDESCENT-PLAYER-BASE-MAX-HP* (10) plus CAFFEINE-TOLERANCE times
*RDESCENT-CAFFEINE-TOLERANCE-HP-PER-POINT* (2) -- a CAFFEINE-TOLERANCE
of 18 yields 46 Max HP (an absolute tank, vibrating through space and
time); 10 (the average roll) yields the standard baseline 30; 5 yields
only 20 (frail -- one strong cup of coffee or a harsh email will kill
you). Called once by MAKE-INITIAL-STATE to compute the player's
starting MAX-HP/HP; CAFFEINE-TOLERANCE is never re-consulted for this
afterward, so a mid-game change to it (there currently is none) would
not retroactively resize MAX-HP."
  (+ *rdescent-player-base-max-hp* (* caffeine-tolerance *rdescent-caffeine-tolerance-hp-per-point*)))

(defparameter *rdescent-use-item-energy-cost* *rdescent-attack-energy-cost*
  "ENERGY cost of using an inventory item (see USE-ITEM) -- a Scroll of
PIP or a Reply-All Bomb is an offensive action, so it shares
*RDESCENT-ATTACK-ENERGY-COST* (150) rather than the cheaper
*RDESCENT-MOVE-ENERGY-COST* DRINK-POTION reuses for its own
non-offensive turn cost.")

(defparameter *rdescent-pip-damage* 20
  "Flat damage a Scroll of PIP deals to its single target (see
USE-ITEM/SCROLL-OF-PIP). Unlike ordinary melee combat (MOVE-PLAYER/
PROCESS-ENEMY-TURNS), this damage is not reduced by the target's
DEFENSE -- a PIP is psychic, not physical, and bypasses armor
entirely. See DOMAIN-KNOWLEDGE-BONUS-DAMAGE for the additional bonus/
penalty applied on top of this base value.")

(defparameter *rdescent-reply-all-radius* 3
  "Chebyshev-distance blast radius, in tiles, of a Reply-All Bomb (see
USE-ITEM/REPLY-ALL-BOMB): every IS-ALIVE, BLOCKS-MOVEMENT entity whose
Chebyshev distance from the bomb's target tile is <= this radius takes
*RDESCENT-REPLY-ALL-DAMAGE*.")

(defparameter *rdescent-reply-all-damage* 15
  "Flat damage a Reply-All Bomb deals to each entity caught within
*RDESCENT-REPLY-ALL-RADIUS* of its target tile (see USE-ITEM), also
bypassing DEFENSE like *RDESCENT-PIP-DAMAGE* -- set lower than a
Scroll of PIP's single-target damage since a Reply-All Bomb can hit
several enemies at once. See DOMAIN-KNOWLEDGE-BONUS-DAMAGE for the
additional bonus/penalty applied on top of this base value.")

(defparameter *rdescent-reply-all-chain-reaction-chance-percent* 10
  "Percent chance (out of a (RANDOM 100) roll), rolled once per Reply-
All Bomb detonation, that FUTURE_PLANS.md §18.3's \"Reply-All Chain
Reaction\" triggers: every entity caught in the original blast
that survived it becomes Confused (via APPLY-STATUS-EFFECT, respecting
SENIORITY's Deflection Chance exactly like CAST-REORG-MEMO) and fires
off its own Reply-All-sized explosion -- same *RDESCENT-REPLY-ALL-
RADIUS*/*RDESCENT-REPLY-ALL-DAMAGE*, but without the casting player's
own DOMAIN-KNOWLEDGE-BONUS-DAMAGE, since this second wave isn't cast by
the player -- centered on its own tile, potentially catching other
still-standing entities in a second round of damage (see APPLY-ITEM's
AREA-EFFECT-ITEM method/REPLY-ALL-CHAIN-REACTION, RDESCENT/
ACTIONS.LISP). Deliberately a single wave, not a recursive cascade --
entities newly hit by the chain wave do not themselves trigger a
further chain -- keeping the effect \"emergent chaos\" rather than an
unbounded, potentially-infinite cascade.")

(defparameter *rdescent-confusion-seconds* 3.0
  "Real-world duration, in seconds, a successful Confusion inflicting
attack/item confuses its target for -- see *RDESCENT-CONFUSION-TICKS*
below, which converts this into an engine-tick count via
*RDESCENT-TICK-SECONDS*. The single source of truth for how long
\"Confused\" lasts regardless of what inflicted it (a Vague Re-Org
Memo -- see USE-ITEM/REORG-MEMO/CAST-REORG-MEMO -- a Troll's own
RESOLVE-ATTACK method, or a Code Monkey's \"submits a 500-line
script!\" attack flavor -- see RDESCENT/ENEMIES/ORC.LISP/TROLL.LISP),
so every source of Confusion feels consistent.")

(defparameter *rdescent-confusion-ticks* (round (/ *rdescent-confusion-seconds* *rdescent-tick-seconds*))
  "Number of *engine* ticks (see *RDESCENT-TICK-SECONDS* -- NOT
monster/player turns, which happen far less often, gated by ENTITY-
ENERGY/ENTITY-SPEED) a Confusion-inflicting attack/item confuses its
target for -- *RDESCENT-CONFUSION-SECONDS* (3.0) converted to a tick
count. The initial value ENTITY-CONFUSED-TICKS is set to on a
successful cast/hit, counted down by 1 every engine tick (see
TICK-STATUS-EFFECTS, RDESCENT/MECHANICS.LISP) regardless of whether
the confused entity has actually gotten a confused turn yet (see
PROCESS-ENEMY-TURNS/CONFUSED-ENTITY-TURN) -- until it reaches 0 and
the entity's usual attack-or-approach AI resumes. Deriving this from
*RDESCENT-CONFUSION-SECONDS* rather than picking a bare tick-count
literal matters: at *RDESCENT-TICK-SECONDS* (50ms/tick), a naively
small tick count (e.g. 10, if someone mistook \"ticks\" for \"turns\")
would expire in half a second -- less than even one attack cycle for
a Troll (whose own SPEED means it only acts once every 1.5 real
seconds) -- making the effect nearly useless against slower monsters.")

(defparameter *rdescent-analysis-paralysis-seconds* 6.0
  "Real-world duration, in seconds, the Troll's \"demands peer-
reviewed evidence!\" attack flavor's :ANALYSIS-PARALYSIS STATUS-EFFECT
lasts for once inflicted (FUTURE_PLANS.md §7, \"Varied Attack
Effects\" -- see RDESCENT/ENEMIES/TROLL.LISP) -- see *RDESCENT-
ANALYSIS-PARALYSIS-TICKS* below, which converts this into an
engine-tick count. Deliberately longer than *RDESCENT-CONFUSION-
SECONDS* (3.0): unlike Confusion, Analysis Paralysis never steals a
turn outright, only quietly lowers EFFECTIVE-DODGE-CHANCE, so it
needs to persist across several of the afflicted entity's own turns
(and, in particular, several of a slow monster's own ~1-1.5-second
attack cycles) to meaningfully matter.")

(defparameter *rdescent-analysis-paralysis-ticks*
  (round (/ *rdescent-analysis-paralysis-seconds* *rdescent-tick-seconds*))
  "Number of *engine* ticks (see *RDESCENT-TICK-SECONDS* -- NOT
monster/player turns) the Troll's \"demands peer-reviewed evidence\"
attack flavor's :ANALYSIS-PARALYSIS STATUS-EFFECT lasts for once
inflicted -- *RDESCENT-ANALYSIS-PARALYSIS-SECONDS* (6.0) converted to
a tick count, counted down by TICK-STATUS-EFFECTS (RDESCENT/
MECHANICS.LISP) once per engine tick, same as any other STATUS-
EFFECT's own TICKS-REMAINING -- unlike *RDESCENT-CONFUSION-TICKS*
above, this is not tied to the confused-entity-turn-per-turn
convention, since :ANALYSIS-PARALYSIS never intercepts the affected
entity's own turn; it only lowers EFFECTIVE-DODGE-CHANCE for as long
as it remains attached.")

(defparameter *rdescent-distraction-ticks* 1
  "Number of game ticks the Troll's \"flags your Jira ticket\" attack
flavor's :DISTRACTED STATUS-EFFECT lasts for once inflicted
(FUTURE_PLANS.md §7, \"Varied Attack Effects\" -- see RDESCENT/
ENEMIES/TROLL.LISP): exactly enough to cause ADVANCE-ENTITY-TICK
(RDESCENT/MECHANICS.LISP) to skip precisely one ACCRUE-ENERGY call --
\"delays the player's next Energy tick\", per that section's own
framing -- before TICK-STATUS-EFFECTS drops the effect on the very
next tick. Deliberately NOT derived from a real-world *seconds*
duration via *RDESCENT-TICK-SECONDS* like *RDESCENT-CONFUSION-TICKS*/
*RDESCENT-ANALYSIS-PARALYSIS-TICKS* above -- this effect's entire
purpose is to skip exactly one engine tick's worth of Energy income,
so its \"duration\" is intrinsically defined in ticks, not seconds,
and would be wrong at any other value regardless of how fast or slow
*RDESCENT-TICK-SECONDS* itself is.")

(defparameter *rdescent-carpal-tunnel-seconds* 5.0
  "Real-world duration, in seconds, of §13's :CARPAL-TUNNEL debuff,
primarily inflicted by The Keyboard of Kinesis. The mechanical effect
is a doubled attack ENERGY cost (see EFFECTIVE-ATTACK-ENERGY-COST), so
it needs to persist across several engine ticks to actually slow an
afflicted target's turn cadence."
  )

(defparameter *rdescent-carpal-tunnel-ticks*
  (round (/ *rdescent-carpal-tunnel-seconds* *rdescent-tick-seconds*))
  "Number of engine ticks §13's :CARPAL-TUNNEL status effect lasts
for once inflicted -- *RDESCENT-CARPAL-TUNNEL-SECONDS* converted to a
tick count."
  )

(defparameter *rdescent-bleed-seconds* 4.0
  "Real-world duration, in seconds, of §13's :BLEED debuff, primarily
inflicted by The Red Swingline Stapler. BLEED is implemented as a
small damage-over-time effect using STATUS-EFFECT's existing MAGNITUDE
slot (see TICK-STATUS-EFFECTS) rather than a new panic/flee state."
  )

(defparameter *rdescent-bleed-ticks*
  (round (/ *rdescent-bleed-seconds* *rdescent-tick-seconds*))
  "Number of engine ticks §13's :BLEED status effect lasts for once
inflicted -- *RDESCENT-BLEED-SECONDS* converted to a tick count."
  )

(defparameter *rdescent-bleed-damage-per-tick* -1
  "Per-tick HP delta attached as :BLEED's STATUS-EFFECT MAGNITUDE:
-1 HP each engine tick until the effect expires."
  )

(defparameter *rdescent-stunned-ticks* 2
  "Engine-tick duration of §13's :STUNNED debuff, primarily inflicted
by The Can of Compressed Air. PROCESS-ENEMY-TURNS consumes the effect
the first time the stunned entity has enough ENERGY to act, skipping
that turn entirely; a value of 2 keeps the effect present long enough
for the very next turn check to still see it even if a tick elapses
before then."
  )

;;; §17 The Corporate Pharmacy: STATUS-EFFECT kinds attached by the
;;; new tiered consumable catalog (CONSUMABLE-ITEM, RDESCENT/
;;; ACTIONS.LISP's APPLY-ITEM method) -- every duration below follows
;;; the same *-SECONDS*/*-TICKS* pattern as *RDESCENT-CONFUSION-TICKS*
;;; et al. above, converting a real-world duration into an engine-tick
;;; count via *RDESCENT-TICK-SECONDS* rather than a bare tick literal.

(defparameter *rdescent-food-poisoning-seconds* 6.0
  "Real-world duration of Someone Else's Tupperware Lunch's Food
Poisoning debuff, once its 10% chance actually triggers.")

(defparameter *rdescent-food-poisoning-ticks*
  (round (/ *rdescent-food-poisoning-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :FOOD-POISONING -- *RDESCENT-FOOD-
POISONING-SECONDS* converted to a tick count.")

(defparameter *rdescent-food-poisoning-damage-per-tick* -1
  "Per-tick HP delta attached as :FOOD-POISONING's STATUS-EFFECT
MAGNITUDE, mirroring §13's :BLEED (*RDESCENT-BLEED-DAMAGE-PER-TICK*).")

(defparameter *rdescent-buzzed-seconds* 12.0
  "Real-world duration of the TGIF Leftover Beer's :BUZZED debuff.")

(defparameter *rdescent-buzzed-ticks*
  (round (/ *rdescent-buzzed-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :BUZZED -- *RDESCENT-BUZZED-SECONDS*
converted to a tick count.")

(defparameter *rdescent-buzzed-domain-knowledge-penalty* 1
  "Flat DOMAIN-KNOWLEDGE penalty EFFECTIVE-DOMAIN-KNOWLEDGE subtracts
while :BUZZED is active -- \"slightly buzzed at your desk.\"")

(defparameter *rdescent-caffeinated-seconds* 8.0
  "Real-world duration of the Quadruple Shot Espresso's :CAFFEINATED
speed-boost buff, before it chains (via STATUS-EFFECT's own
EXPIRE-INTO) into a one-tick :DISTRACTED \"crash.\"")

(defparameter *rdescent-caffeinated-ticks*
  (round (/ *rdescent-caffeinated-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :CAFFEINATED -- *RDESCENT-CAFFEINATED-
SECONDS* converted to a tick count.")

(defparameter *rdescent-modafinil-seconds* 60.0
  "Real-world duration of Modafinil's :MODAFINIL-IMMUNITY buff --
\"total immunity to sleep/stun mechanics ... for 100 turns\" scaled
down to a real-world duration like every other effect here, consulted
by APPLY-STATUS-EFFECT to shrug off any incoming :CONFUSED/:STUNNED
infliction outright, the same way Buzzword Bingo Chips' collectible
set bonus already does for :CONFUSED alone.")

(defparameter *rdescent-modafinil-ticks*
  (round (/ *rdescent-modafinil-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :MODAFINIL-IMMUNITY -- *RDESCENT-MODAFINIL-
SECONDS* converted to a tick count.")

(defparameter *rdescent-the-zone-seconds* 12.0
  "Real-world duration of Dexedrine Spansule's :THE-ZONE buff. The
plan text's \"guaranteed critical hits\" is deliberately simplified to
a flat EFFECTIVE-POWER bonus (*RDESCENT-THE-ZONE-POWER-BONUS*) --
there is no independent critical-hit-multiplier subsystem in combat
resolution to hook a guaranteed-crit flag into, so this reuses the
existing damage-bonus seam instead of inventing one, exactly like
§13's own documented simplifications.")

(defparameter *rdescent-the-zone-ticks*
  (round (/ *rdescent-the-zone-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :THE-ZONE -- *RDESCENT-THE-ZONE-SECONDS*
converted to a tick count.")

(defparameter *rdescent-the-zone-power-bonus* 8
  "Flat EFFECTIVE-POWER bonus while :THE-ZONE is active -- see
*RDESCENT-THE-ZONE-SECONDS*'s own docstring for why this stands in
for the plan text's \"guaranteed critical hits.\"")

(defparameter *rdescent-adderall-focus-seconds* 30.0
  "Real-world duration of Discarded Adderall's (and, on a lucky roll,
the Unmarked Nootropic Stack's) :ADDERALL-FOCUS buff, before it chains
(via EXPIRE-INTO) into a one-tick :ADDERALL-CRASH HP hit.")

(defparameter *rdescent-adderall-focus-ticks*
  (round (/ *rdescent-adderall-focus-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :ADDERALL-FOCUS -- *RDESCENT-ADDERALL-
FOCUS-SECONDS* converted to a tick count.")

(defparameter *rdescent-adderall-focus-domain-knowledge-bonus* 2
  "Flat DOMAIN-KNOWLEDGE bonus EFFECTIVE-DOMAIN-KNOWLEDGE adds while
:ADDERALL-FOCUS is active.")

(defparameter *rdescent-adderall-crash-hp-delta* -10
  "One-shot HP hit applied via :ADDERALL-CRASH's STATUS-EFFECT
MAGNITUDE -- a 1-tick effect (see :ADDERALL-FOCUS's own EXPIRE-INTO)
so TICK-STATUS-EFFECTS' ordinary per-tick MAGNITUDE-drain mechanism
applies it exactly once before the effect itself expires, giving
\"lose 10 HP when it wears off\" for free with no separate one-shot
mechanism.")

(defparameter *rdescent-executive-high-seconds* 6.0
  "Real-world duration of the Baggie of Blow's :EXECUTIVE-HIGH buff,
before it chains (via EXPIRE-INTO) into the much longer :COMEDOWN
debuff -- \"10 turns later, a catastrophic Comedown.\"")

(defparameter *rdescent-executive-high-ticks*
  (round (/ *rdescent-executive-high-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :EXECUTIVE-HIGH -- *RDESCENT-EXECUTIVE-
HIGH-SECONDS* converted to a tick count.")

(defparameter *rdescent-executive-high-stat-bonus* 5
  "Flat +SYNERGY/+PIVOT bonus (EFFECTIVE-SYNERGY/EFFECTIVE-PIVOT)
while :EXECUTIVE-HIGH is active.")

(defparameter *rdescent-comedown-seconds* 30.0
  "Real-world duration of the Baggie of Blow's :COMEDOWN debuff --
\"halves movement speed and applies -2 to all stats for 50 turns.\"
The movement-speed half is deliberately simplified to doubling
EFFECTIVE-ATTACK-ENERGY-COST (the same existing seam §13's :CARPAL-
TUNNEL already doubles) rather than touching every one of the many
*RDESCENT-MOVE-ENERGY-COST* call sites across ACTIONS.LISP; the
\"-2 to all stats\" is likewise simplified to EFFECTIVE-SYNERGY/
EFFECTIVE-PIVOT alone (the two stats :EXECUTIVE-HIGH itself buffed).")

(defparameter *rdescent-comedown-ticks*
  (round (/ *rdescent-comedown-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :COMEDOWN -- *RDESCENT-COMEDOWN-SECONDS*
converted to a tick count.")

(defparameter *rdescent-comedown-stat-penalty* 2
  "Flat -SYNERGY/-PIVOT penalty (EFFECTIVE-SYNERGY/EFFECTIVE-PIVOT)
while :COMEDOWN is active -- see *RDESCENT-COMEDOWN-SECONDS*.")

(defparameter *rdescent-microdosing-seconds* 40.0
  "Real-world duration of the Microdose Tab (LSD)'s :MICRODOSING buff:
doubles EFFECTIVE-FOV-RADIUS and guarantees (rather than merely rolls
SENIORITY-DETECTION-CHANCE for) hidden TRAP-FIXTURE detection while
active, via MAYBE-REVEAL-HIDDEN-ENTITIES (RDESCENT/ACTIONS.LISP). The
plan text's \"hidden doors/traps glow neon colors\" is deliberately
left as flavor text rather than a new rendering hook -- guaranteed
detection already gets every hidden trap in sight revealed the very
next turn, which is the mechanically meaningful half of the effect.")

(defparameter *rdescent-microdosing-ticks*
  (round (/ *rdescent-microdosing-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :MICRODOSING -- *RDESCENT-MICRODOSING-
SECONDS* converted to a tick count.")

(defparameter *rdescent-pharmacy-full-energy-restore* 300
  "ENERGY value Discarded Adderall (\"instantly restores Energy to
max\") and the Baggie of Blow (\"max Energy\") set the player's own
ENTITY-ENERGY to outright, rather than adding a fixed amount on top --
the same generous \"full tank\" value already used by the Espresso
Machine shrine (*RDESCENT-ESPRESSO-ENERGY-RESTORE*), reused here since
neither consumable's own base ENERGY level should matter to how full
\"max\" ends up meaning.")

(defparameter *rdescent-domain-knowledge-damage-per-modifier-point* 5
  "Per-modifier-point multiplier of DOMAIN-KNOWLEDGE-BONUS-DAMAGE's
formula, ABILITY-MODIFIER of DOMAIN-KNOWLEDGE times this constant --
see that function.")

(defun domain-knowledge-bonus-damage (domain-knowledge)
  "Return the bonus (or penalty) damage a given DOMAIN-KNOWLEDGE
Corporate RPG Stat (see ENTITY's docstring) adds to the player's own
targeted/area-effect items (Scroll of PIP, Reply-All Bomb): ABILITY-
MODIFIER of DOMAIN-KNOWLEDGE, times
*RDESCENT-DOMAIN-KNOWLEDGE-DAMAGE-PER-MODIFIER-POINT* (5). A
DOMAIN-KNOWLEDGE of 16 (+3 modifier) yields +15 damage; 10 (the
average roll, +0 modifier) yields 0 (no change); 6 (-2 modifier)
yields -10 (a penalty). If you know the domain, you know the
codebase -- your PIPs land harder. Callers (APPLY-ITEM's
TARGETED-ITEM/AREA-EFFECT-ITEM methods) clamp the result at 0 via MAX
so a low enough roll never turns a damaging item into a heal."
  (* (ability-modifier domain-knowledge) *rdescent-domain-knowledge-damage-per-modifier-point*))

(defparameter *rdescent-max-banked-energy* *rdescent-attack-energy-cost*
  "Hard cap on any entity's ENTITY-ENERGY balance, enforced by
ACCRUE-ENERGY every tick. Without this cap, an entity that currently
qualifies for no turn at all in PROCESS-ENEMY-TURNS -- most commonly a
monster sitting outside the player's field of view, since
PROCESS-ENEMY-TURNS only ever offers a turn to entities currently
visible via COMPUTE-FOV -- would keep accruing ENERGY from REDUCE-TICK
completely unboundedly for as long as it stays unseen. The moment it
came back into view it would have banked far more than one action's
worth (e.g. an Internet Troll left off-screen for 30 real seconds
accrues enough ENERGY for ~20 back-to-back attacks), and since PROCESS-ENEMY-TURNS
only spends one action's cost per entity per tick, that stockpile
would then get spent one action every single *RDESCENT-TICK-SECONDS*
tick (50ms) until it ran back down -- a jarring burst of rapid-fire
moves/attacks completely disconnected from the entity's own
ENTITY-SPEED, rather than the smooth, steady cadence the speed is
supposed to guarantee. Capping the balance at
*RDESCENT-ATTACK-ENERGY-COST* (the more expensive of the two action
costs) means no entity can ever bank more than a single action's worth
in reserve, however long it goes unseen or otherwise idle, so every
entity's pace stays governed purely by its own ENTITY-SPEED regardless
of how sporadically PROCESS-ENEMY-TURNS actually gets to act on it.")

(defparameter *rdescent-heal-ticks* 200
  "Number of game ticks of HEAL-PROGRESS an entity must accumulate to
regenerate one point of HP: at the current *RDESCENT-TICK-SECONDS*
(0.05s/50ms, see RDESCENT/SERVER.LISP), 200 ticks is 10 real seconds,
i.e. natural healing at a rate of 1 HP per 10 seconds. See
ACCRUE-HEALING/REDUCE-TICK.")

(defun bandwidth-damage-multiplier (bandwidth)
  "Return the multiplier applied to damage the player takes from a
successful enemy attack, for a given BANDWIDTH Corporate RPG Stat (see
ENTITY's docstring): 1.0 - ((BANDWIDTH - 10) * 0.05). A BANDWIDTH of
18 yields a 0.6x multiplier (40% less damage taken); 10 (the average
roll) yields the neutral 1.0x; 2 yields a 1.4x multiplier (40% more
damage taken). Called by PROCESS-ENEMY-TURNS on the player's own
BANDWIDTH before subtracting damage from HP; the caller is
responsible for rounding to the nearest integer and flooring at 0."
  (- 1.0 (* (- bandwidth 10) 0.05)))

(defun pivot-dodge-chance (pivot)
  "Return the flat percent chance (0-100, a real number) that the
player dodges an incoming enemy attack entirely (0 damage), for a
given PIVOT Corporate RPG Stat (see ENTITY's docstring): PIVOT * 2.5.
A PIVOT of 18 yields 45%; 10 (the average roll) yields 25%; 4 yields
10%. Called by PROCESS-ENEMY-TURNS, which rolls (RANDOM 100) against
this before computing any damage at all -- POWER/DEFENSE/BANDWIDTH
never enter into it once a dodge succeeds."
  (* pivot 2.5))

(defparameter *rdescent-analysis-paralysis-dodge-penalty* 15
  "Flat percentage-point reduction applied to PIVOT-DODGE-CHANCE (see
EFFECTIVE-DODGE-CHANCE) while an ENTITY has an active :ANALYSIS-
PARALYSIS STATUS-EFFECT -- FUTURE_PLANS.md §7's \"Varied Attack
Effects\", inflicted by the Troll's \"demands peer-reviewed evidence\"
attack flavor (see RDESCENT/ENEMIES/TROLL.LISP). \"Frozen by
analysis\": too busy demanding a citation to react quickly enough to
dodge.")

(defun seniority-deflection-chance (seniority)
  "Return the flat percent chance (0-100, a real number) that an
entity shrugs off an incoming status effect entirely, for a given
SENIORITY Corporate RPG Stat (see ENTITY's docstring): ABILITY-
MODIFIER of SENIORITY, times 10. A SENIORITY of 16 (+3 modifier)
yields 30%; 10 (the average roll, +0 modifier) yields 0% (the average
roll always \"takes the assignment\"); 4 (-3 modifier) yields -30%,
which APPLY-STATUS-EFFECT's (< (RANDOM 100) ...) check treats
identically to 0% (RANDOM 100 is never negative, so a negative chance
can never be rolled under). Called by APPLY-STATUS-EFFECT before
attaching any STATUS-EFFECT -- the single, uniform gate every future
debuff-inflicting attack/item/trap passes through, per SENIORITY's own
long-planned Deflection Chance formula (see ENTITY's docstring) and
ARCHITECTURE_PLAN.md §1. Every monster and any player predating a
future SENIORITY-granting feature has the default SENIORITY of 0,
i.e. a 0% Deflection Chance -- effects always land until some feature
actually raises this stat above the default."
  (* (ability-modifier seniority) 10))

(defun seniority-detection-chance (seniority)
  "Return the flat percent chance (0-100, a real number) that the
player notices a hidden TRAP-FIXTURE (see GET-HIDDEN-P) the instant it
enters their field of view, for a given SENIORITY Corporate RPG Stat
(see ENTITY's docstring): SENIORITY * 5 -- deliberately a flat
multiple of the raw stat, not its usual D&D-style ABILITY-MODIFIER (a
0 SENIORITY, the default for both monsters and any player predating a
SENIORITY-granting feature, correctly yields a 0% Detection Chance
rather than ABILITY-MODIFIER's negative-then-clamped result). A
SENIORITY of 18 yields 90%; 10 (the average roll) yields 50%; 3 (the
minimum possible ROLL-STAT result) yields 15%. Called by MAYBE-REVEAL-
HIDDEN-ENTITIES (RDESCENT/COMMANDS.LISP), once per still-HIDDEN-P
TRAP-FIXTURE that falls within the player's own just-computed
VISIBLE-MASK, per SENIORITY's own long-planned Detection Chance
formula and FUTURE_PLANS.md §8."
  (* seniority 5))

(defun synergy-pacify-chance (synergy)
  "Return the flat percent chance (0-100, a real number) that an
otherwise-:HOSTILE freshly spawned monster instead spawns :NEUTRAL and
simply wanders, ignoring the player, for a given player SYNERGY
Corporate RPG Stat (see ENTITY's docstring): (MAX 0 (ABILITY-MODIFIER
of SYNERGY times 5)). A SYNERGY of 18 (+4 modifier) yields 20%; 10
(the average roll, +0 modifier) yields 0% (the average roll pacifies
nothing -- \"everyone wants a piece of you\"); 4 (-3 modifier) would
yield -15%, clamped to 0 by the outer MAX so a low SYNERGY never
produces a negative chance (a negative chance is never actually
dangerous, since RANDOM never returns a value below 0, but MAX keeps
the return value itself a meaningful percent rather than a nonsense
negative one). Called once per freshly spawned monster by
SPAWN-MONSTERS-FOR-LEVEL (via SPAWN-TIME-DISPOSITION), per SYNERGY's
own long-planned Pacify Chance formula and FUTURE_PLANS.md §1."
  (max 0 (* (ability-modifier synergy) 5)))

(defun synergy-price-modifier (synergy)
  "Return the multiplier VENDOR-ITEM-PRICE applies to a VENDOR-STOCK-
ENTRY's own BASE-PRICE, for a given player SYNERGY Corporate RPG Stat
(see ENTITY's docstring): 1.0 - (ABILITY-MODIFIER of SYNERGY times
0.05). A SYNERGY of 16 (+3 modifier) yields 0.85, a 15% discount
(\"CC'd the right director\"); 10 (the average roll, +0 modifier)
yields 1.0, normal prices; 6 (-2 modifier) yields 1.10, a 10% \"asshole
tax\" for IT hating you -- unlike SYNERGY-PACIFY-CHANCE, this is
deliberately *not* clamped at any bound: a very low SYNERGY really can
make a purchase cost more than its BASE-PRICE, and a very high one
really can approach (but, per VENDOR-ITEM-PRICE's own MAX 1 floor,
never reach) free. Every monster and any player predating a future
SYNERGY-granting feature has the default SYNERGY of 0 (a -5 modifier,
1.25x prices) -- see VENDOR-ITEM-PRICE, the only caller, for how this
combines with a given VENDOR-STOCK-ENTRY's own BASE-PRICE (FUTURE_
PLANS.md §10, \"Vendors / Shops\")."
  (- 1.0 (* (ability-modifier synergy) 0.05)))

;;; --- FUTURE_PLANS.md §16: Scavenger Hunt Collectibles -----------
;;;
;;; Four named sets of collectible items, scattered one per dungeon
;;; level (see SPAWN-COLLECTIBLES-FOR-LEVEL, RDESCENT/DUNGEON.LISP),
;;; auto-picked-up the instant the player steps onto their tile (see
;;; MAYBE-AUTO-PICKUP-COLLECTIBLE, RDESCENT/ACTIONS.LISP) rather than
;;; requiring an explicit GRAB-ITEM command like an ordinary
;;; GROUND-ITEM. Collecting every item in a set grants that set's own
;;; permanent passive bonus (see HARDWARE-EMULATION-ACTIVE-P/GNU-
;;; OMNISCIENCE-ACTIVE-P/PERIPHERAL-VISION-ACTIVE-P/BUZZWORD-IMMUNITY-
;;; ACTIVE-P below) for the rest of that playthrough -- tracked via
;;; each entity's own COLLECTION-LOG slot (a plain list of keyword
;;; item-ids), not a GAME-STATE flag like FUTURE_PLANS.md §9's
;;; KEYS-HELD/DOORS-OPENED, because RESOLVE-ATTACK/EFFECTIVE-DEFENSE/
;;; EFFECTIVE-DODGE-CHANCE/APPLY-STATUS-EFFECT (the functions each
;;; set's own bonus plugs into) only ever take an ENTITY, never a
;;; GAME-STATE, as an argument.

(defstruct (collectible-item (:constructor make-collectible-item (id name set)))
  "One entry in *RDESCENT-COLLECTIBLE-CATALOG*: ID is a unique keyword
identifying this item (e.g. :HOLLERITH-PUNCH-CARDS), NAME its
human-readable display string, and SET the keyword id (see
COLLECTIBLE-SET-ID) of the *RDESCENT-COLLECTIBLE-SETS* entry this item
belongs to."
  id name set)

(defstruct (collectible-set (:constructor make-collectible-set (id name bonus-name message-color)))
  "One entry in *RDESCENT-COLLECTIBLE-SETS*: ID is a unique keyword
identifying this set (e.g. :RELICS-OF-ANCIENT-MEMORY), NAME its
human-readable display string, BONUS-NAME the human-readable name of
the permanent passive bonus completing it grants (used in the \"set
completed\" message, see MAYBE-AUTO-PICKUP-COLLECTIBLE), and
MESSAGE-COLOR the CSS color both an individual item pickup message and
this set's own completion-announcement message are displayed in."
  id name bonus-name message-color)

(defparameter *rdescent-collectible-sets*
  (list (make-collectible-set :relics-of-ancient-memory "Relics of Ancient Memory"
                               "Hardware Emulation" "#8e6f4e")
        (make-collectible-set :pantheon-of-the-beard "Pantheon of the Beard"
                               "GNU/Omniscience" "#dcdcdc")
        (make-collectible-set :cursed-peripherals-of-yesteryear "Cursed Peripherals of Yesteryear"
                               "Peripheral Vision" "#c0392b")
        (make-collectible-set :outdated-buzzword-bingo-chips "Outdated Buzzword Bingo Chips"
                               "Buzzword Immunity" "#f39c12"))
  "The four FUTURE_PLANS.md §16 collectible sets, in catalog order. See
COLLECTIBLE-SET/*RDESCENT-COLLECTIBLE-CATALOG*.")

(defparameter *rdescent-collectible-catalog*
  (list (make-collectible-item :hollerith-punch-cards "A Stack of Hollerith Punch Cards" :relics-of-ancient-memory)
        (make-collectible-item :nine-track-magnetic-tape "A Spool of 9-Track Magnetic Tape" :relics-of-ancient-memory)
        (make-collectible-item :five-quarter-floppy-disk "A 5-1/4-Inch Floppy Disk (\"DO NOT OVERWRITE\")" :relics-of-ancient-memory)
        (make-collectible-item :three-half-floppy-disk "A 3-1/2-Inch Floppy Disk (Save Icon)" :relics-of-ancient-memory)
        (make-collectible-item :magnetic-core-memory-module "A Module of Magnetic Core Memory" :relics-of-ancient-memory)
        (make-collectible-item :iomega-zip-disk "An Iomega Zip Disk" :relics-of-ancient-memory)
        (make-collectible-item :stallman-card "The Richard Stallman Card (Mint Condition)" :pantheon-of-the-beard)
        (make-collectible-item :knuth-card "The Donald Knuth Card (Holographic)" :pantheon-of-the-beard)
        (make-collectible-item :hopper-card "The Grace Hopper Card" :pantheon-of-the-beard)
        (make-collectible-item :lovelace-card "The Ada Lovelace Card" :pantheon-of-the-beard)
        (make-collectible-item :torvalds-card "The Linus Torvalds Card" :pantheon-of-the-beard)
        (make-collectible-item :sussman-abelson-dual-card "The Sussman & Abelson (SICP) Dual-Card" :pantheon-of-the-beard)
        (make-collectible-item :hockey-puck-mouse "The Apple \"Hockey Puck\" Mouse" :cursed-peripherals-of-yesteryear)
        (make-collectible-item :power-glove "The Nintendo Power Glove (Corporate Edition)" :cursed-peripherals-of-yesteryear)
        (make-collectible-item :ibm-trackpoint "An Original IBM TrackPoint (The Red Nub)" :cursed-peripherals-of-yesteryear)
        (make-collectible-item :spacenavigator "A 3Dconnexion SpaceNavigator" :cursed-peripherals-of-yesteryear)
        (make-collectible-item :dusty-kinect-sensor "A Microsoft Kinect Sensor Covered in Dust" :cursed-peripherals-of-yesteryear)
        (make-collectible-item :web-2.0-buzzword-chip "\"Web 2.0\"" :outdated-buzzword-bingo-chips)
        (make-collectible-item :synergy-buzzword-chip "\"Synergy\"" :outdated-buzzword-bingo-chips)
        (make-collectible-item :gamification-buzzword-chip "\"Gamification\"" :outdated-buzzword-bingo-chips)
        (make-collectible-item :blockchain-buzzword-chip "\"The Blockchain\"" :outdated-buzzword-bingo-chips)
        (make-collectible-item :metaverse-buzzword-chip "\"Metaverse\"" :outdated-buzzword-bingo-chips)
        (make-collectible-item :big-data-buzzword-chip "\"Big Data\"" :outdated-buzzword-bingo-chips))
  "All 23 FUTURE_PLANS.md §16 collectible items, in a fixed catalog
order SPAWN-COLLECTIBLES-FOR-LEVEL indexes into deterministically (via
(MOD (1- LEVEL) (LENGTH *RDESCENT-COLLECTIBLE-CATALOG*))) so no two
concurrently-generated dungeon levels can ever spawn the same item.")

(defun find-collectible-item (item-id)
  "Return the COLLECTIBLE-ITEM in *RDESCENT-COLLECTIBLE-CATALOG* whose
ID is ITEM-ID, or NIL if none matches."
  (find item-id *rdescent-collectible-catalog* :key #'collectible-item-id))

(defun find-collectible-set (set-id)
  "Return the COLLECTIBLE-SET in *RDESCENT-COLLECTIBLE-SETS* whose ID
is SET-ID, or NIL if none matches."
  (find set-id *rdescent-collectible-sets* :key #'collectible-set-id))

(defun collectible-set-item-ids (set-id)
  "Return a list of every COLLECTIBLE-ITEM-ID in
*RDESCENT-COLLECTIBLE-CATALOG* belonging to SET-ID (a keyword, e.g.
:RELICS-OF-ANCIENT-MEMORY) -- the full membership roster
COLLECTION-SET-COMPLETE-P checks ENT's own COLLECTION-LOG against."
  (loop for item in *rdescent-collectible-catalog*
        when (eq (collectible-item-set item) set-id)
        collect (collectible-item-id item)))

(defun collection-set-complete-p (ent set-id)
  "T if every item in SET-ID's own COLLECTIBLE-SET-ITEM-IDS is present
in ENT's own COLLECTION-LOG (see GET-COLLECTION-LOG), NIL otherwise --
the single predicate every named set-bonus predicate below
(HARDWARE-EMULATION-ACTIVE-P etc.) delegates to."
  (every (lambda (id) (member id (get-collection-log ent))) (collectible-set-item-ids set-id)))

(defparameter *rdescent-hardware-emulation-defense-bonus* 5
  "Flat DEFENSE bonus (see EFFECTIVE-DEFENSE) granted once an entity's
own Relics of Ancient Memory collectible set is complete (see
HARDWARE-EMULATION-ACTIVE-P) -- \"Hardware Emulation\", FUTURE_
PLANS.md §16: you've collected so many obsolete relics that modern
threats bounce off you like they're running in a compatibility
layer.")

(defparameter *rdescent-peripheral-vision-pivot-bonus* 5
  "Flat PIVOT Corporate RPG Stat bonus folded into PIVOT-DODGE-CHANCE
(see EFFECTIVE-DODGE-CHANCE) once an entity's own Cursed Peripherals
of Yesteryear collectible set is complete (see
PERIPHERAL-VISION-ACTIVE-P) -- FUTURE_PLANS.md §16 does not specify an
exact PIVOT amount for \"Peripheral Vision\", only the flat dodge-
percent bonus below; 5 is this implementation's own reasonable choice,
on top of (not instead of) that flat bonus.")

(defparameter *rdescent-peripheral-vision-dodge-bonus* 15
  "Flat additional percentage-point dodge-chance bonus (see
EFFECTIVE-DODGE-CHANCE) granted once an entity's own Cursed Peripherals
of Yesteryear collectible set is complete (see
PERIPHERAL-VISION-ACTIVE-P) -- \"Peripheral Vision\", FUTURE_PLANS.md
§16's own specified +15% dodge chance.")

(defun hardware-emulation-active-p (ent)
  "T if ENT's own Relics of Ancient Memory collectible set is complete
(see COLLECTION-SET-COMPLETE-P) -- grants
*RDESCENT-HARDWARE-EMULATION-DEFENSE-BONUS* via EFFECTIVE-DEFENSE."
  (collection-set-complete-p ent :relics-of-ancient-memory))

(defun gnu-omniscience-active-p (ent)
  "T if ENT's own Pantheon of the Beard collectible set is complete
(see COLLECTION-SET-COMPLETE-P) -- \"GNU/Omniscience\", FUTURE_
PLANS.md §16: the entire dungeon map is permanently revealed, as if
every tile had already been explored (see MAYBE-REVEAL-FULL-MAP,
RDESCENT/ACTIONS.LISP)."
  (collection-set-complete-p ent :pantheon-of-the-beard))

(defun peripheral-vision-active-p (ent)
  "T if ENT's own Cursed Peripherals of Yesteryear collectible set is
complete (see COLLECTION-SET-COMPLETE-P) -- grants
*RDESCENT-PERIPHERAL-VISION-PIVOT-BONUS*/*RDESCENT-PERIPHERAL-VISION-
DODGE-BONUS* via EFFECTIVE-DODGE-CHANCE."
  (collection-set-complete-p ent :cursed-peripherals-of-yesteryear))

(defun buzzword-immunity-active-p (ent)
  "T if ENT's own Outdated Buzzword Bingo Chips collectible set is
complete (see COLLECTION-SET-COMPLETE-P) -- \"Buzzword Immunity\",
FUTURE_PLANS.md §16: ENT unconditionally deflects any :CONFUSED
STATUS-EFFECT applied via APPLY-STATUS-EFFECT. FUTURE_PLANS.md §16's
own text scopes this to debuffs \"inflicted by Management-tier
enemies\", but APPLY-STATUS-EFFECT (the single, universal entry point
every debuff-inflicting attack/item/trap already funnels through, see
ARCHITECTURE_PLAN.md §1) has no notion of *which entity* is inflicting
an effect, only which entity is receiving one -- threading an
inflicter-faction parameter through every existing call site (CAST-
REORG-MEMO, TROLL.LISP's RESOLVE-ATTACK override) purely to narrow
this one set-bonus would be far more invasive than the bonus itself
warrants. This implementation instead grants a simpler, strictly
stronger immunity to :CONFUSED regardless of source (mirroring this
same session's precedent of preferring a universally-correct
simplification over exactly replicating an inflicter-specific detail
that the surrounding code has no existing seam for -- see the
Disgruntled IT Guy quest, FUTURE_PLANS.md §11)."
  (collection-set-complete-p ent :outdated-buzzword-bingo-chips))

(defparameter *rdescent-blue-light-blocking-glasses-fov-bonus* 1
  "Flat field-of-view radius bonus granted on top of
DOMAIN-KNOWLEDGE-FOV-RADIUS when an entity has The Blue-Light Blocking
Glasses equipped in its :HEAD slot (see BLUE-LIGHT-BLOCKING-GLASSES-
ACTIVE-P/EFFECTIVE-FOV-RADIUS) -- FUTURE_PLANS.md §13's explicit
\"+1 FOV radius on top\" rider, intentionally modeled as a separate
flat term rather than by inflating :DOMAIN-KNOWLEDGE alone.")

(defun head-slot-item-active-p (ent item-class)
  "T if ENT's own :HEAD equipment slot currently holds an instance of
ITEM-CLASS, NIL otherwise. The Equipment System deliberately only has
four slots (:WEAPON/:BODY/:HEAD/:OFF-HAND, see ENTITY's EQUIPMENT slot
docstring), so §13's neck/amulet-style accessories are implemented as
ordinary :HEAD-slot equippables sharing that slot with literal
headgear."
  (typep (equipped-item ent :head) item-class))

(defun noise-canceling-headphones-active-p (ent)
  "T if ENT currently has Headphones of Noise-Canceling equipped in
its :HEAD slot -- the equipment-based counterpart to §16's
BUZZWORD-IMMUNITY-ACTIVE-P set bonus, used by APPLY-STATUS-EFFECT to
deflect :CONFUSED exactly along the same existing seam rather than
inventing a second, item-specific confusion system."
  (head-slot-item-active-p ent 'headphones-of-noise-canceling))

(defun blue-light-blocking-glasses-active-p (ent)
  "T if ENT currently has The Blue-Light Blocking Glasses equipped in
its :HEAD slot. EFFECTIVE-FOV-RADIUS consults this to add
*RDESCENT-BLUE-LIGHT-BLOCKING-GLASSES-FOV-BONUS* on top of the item's
ordinary :DOMAIN-KNOWLEDGE stat bonus."
  (head-slot-item-active-p ent 'blue-light-blocking-glasses))

(defun yubikey-of-second-factors-active-p (ent)
  "T if ENT currently has The YubiKey of Second Factors equipped in
its :HEAD slot. The YubiKey's once-per-floor death save is implemented
in RDESCENT/ACTIONS.LISP's own player-specific rescue helper, which
checks this predicate before consuming the item."
  (head-slot-item-active-p ent 'yubikey-of-second-factors))

(defun hygiene-banded-disposition (faction hygiene current-disposition)
  "Return the DISPOSITION a freshly spawned monster of FACTION should
have, given the player's own HYGIENE Corporate RPG Stat (see ENTITY's
docstring) and the monster's own CURRENT-DISPOSITION (its factory/
ENEMY default, before any HYGIENE-banded adjustment) -- FUTURE_PLANS.md
§1's sliding-scale faction hostility. If CURRENT-DISPOSITION is
anything other than :HOSTILE (e.g. a :FRIENDLY NPC or COMPANION),
it's returned unchanged -- this banding only ever softens an otherwise-
hostile monster, never hardens a non-hostile one. Otherwise: above
HYGIENE 14 (\"You look like a Suit\"), a :MANAGEMENT monster turns
:NEUTRAL (it assumes you're one of them) while a :DISGRUNTLED-DEV
monster stays :HOSTILE (it assumes you're about to assign it Jira
tickets); below HYGIENE 8 (\"You look feral\"), a :DISGRUNTLED-DEV
monster turns :NEUTRAL (you're one of the pack) while a :MANAGEMENT
monster stays :HOSTILE; between 8 and 14 inclusive, or for any FACTION
other than :MANAGEMENT/:DISGRUNTLED-DEV, CURRENT-DISPOSITION is
returned unchanged (today's original, pre-HYGIENE-banding behavior).
Called once per freshly spawned monster by SPAWN-MONSTERS-FOR-LEVEL
(via SPAWN-TIME-DISPOSITION), before SYNERGY-PACIFY-CHANCE's
independent pacify roll -- see that function's docstring."
  (cond ((not (eq current-disposition :hostile)) current-disposition)
        ((> hygiene 14) (case faction (:management :neutral) (t current-disposition)))
        ((< hygiene 8) (case faction (:disgruntled-dev :neutral) (t current-disposition)))
        (t current-disposition)))

(defclass status-effect ()
  ((kind :initarg :kind :reader status-effect-kind)
   (ticks-remaining :initarg :ticks-remaining :reader status-effect-ticks-remaining)
   (magnitude :initarg :magnitude :reader status-effect-magnitude :initform nil)
   (expire-into :initarg :expire-into :reader status-effect-expire-into :initform nil))
  (:documentation "An immutable timed effect attached to an ENTITY
(see its ACTIVE-EFFECTS slot/GET-ACTIVE-EFFECTS) -- the generalized
replacement for the earlier bespoke CONFUSED-TICKS slot (see
TECHNICAL_DEBT.md-style copy-paste concerns and ARCHITECTURE_PLAN.md
§1). KIND is a keyword identifying which effect this is (e.g.
:CONFUSED -- currently the only KIND ever constructed, by CAST-REORG-
MEMO/APPLY-STATUS-EFFECT; future debuffs like :FOOD-POISONING or
:BURNOUT, and buffs like :CRIT-BOOST, are simply new KIND values, not
new ENTITY slots). TICKS-REMAINING is a positive integer counting down
by 1 every game tick (see TICK-STATUS-EFFECTS) until it reaches 0, at
which point the effect is dropped from ACTIVE-EFFECTS entirely rather
than lingering at 0 -- ENTITY-EFFECT's absence is how callers detect
an effect has ended, mirroring the old CONFUSED-TICKS > 0 convention.
MAGNITUDE is an optional per-tick numeric effect (currently: a
straight HP delta, negative to drain, positive to regenerate -- see
TICK-STATUS-EFFECTS) -- NIL (the default) for effects like :CONFUSED
that only matter via their KIND's presence and never touch HP
directly. EXPIRE-INTO is an optional (:KIND :TICKS-REMAINING
[:MAGNITUDE]) plist (NIL by default): the moment this effect's own
TICKS-REMAINING counts down to 0 and it would ordinarily just be
dropped, TICK-STATUS-EFFECTS instead attaches a fresh STATUS-EFFECT
built from EXPIRE-INTO in its place -- e.g. §17's Quadruple Shot
Espresso attaches a temporary :CAFFEINATED buff whose own EXPIRE-INTO
chains straight into a one-tick :DISTRACTED \"crash\", so a throwaway
consumable's whole buff-then-debuff arc can be expressed as a single
STATUS-EFFECT-CHAIN with no new per-tick bookkeeping machinery. Never
mutated in place, like every other value type in this file -- a
changed TICKS-REMAINING/MAGNITUDE is always a fresh STATUS-EFFECT
instance (see REPLACE-EFFECT-IN-LIST/TICK-STATUS-EFFECT)."))

(defclass entity ()
  ((x :initarg :x :reader get-x)
   (y :initarg :y :reader get-y)
   (char :initarg :char :reader get-char)
   (level :initarg :level :reader get-level :initform 1)
   (name :initarg :name :reader get-name :initform nil)
   (blocks-movement :initarg :blocks-movement :reader get-blocks-movement :initform nil)
   (max-hp :initarg :max-hp :reader max-hp :initform nil)
   (hp :initarg :hp :reader hp :initform nil)
   (defense :initarg :defense :reader defense :initform 0)
   (power :initarg :power :reader power :initform 0)
   (render-order :initarg :render-order :reader render-order :initform 1)
   (is-alive :initarg :is-alive :reader is-alive :initform t)
   (energy :initarg :energy :reader entity-energy :initform 0)
   (speed :initarg :speed :reader entity-speed :initform 0)
   (heal-progress :initarg :heal-progress :reader entity-heal-progress :initform 0)
   (active-effects :initarg :active-effects :reader get-active-effects :initform nil)
   (xp :initarg :xp :reader get-xp :initform 0)
   (message-color :initarg :message-color :reader entity-message-color :initform "white")
   (kombucha :initarg :kombucha :reader get-kombucha :initform 0)
   (rsu :initarg :rsu :reader get-rsu :initform 0)
   (bandwidth :initarg :bandwidth :reader get-bandwidth :initform 10)
   (pivot :initarg :pivot :reader get-pivot :initform 0)
   (caffeine-tolerance :initarg :caffeine-tolerance :reader get-caffeine-tolerance :initform 10)
   (domain-knowledge :initarg :domain-knowledge :reader get-domain-knowledge :initform 10)
   (seniority :initarg :seniority :reader get-seniority :initform 0)
   (synergy :initarg :synergy :reader get-synergy :initform 0)
   (hygiene :initarg :hygiene :reader get-hygiene :initform 0)
   (faction :initarg :faction :reader get-faction :initform :neutral)
   (disposition :initarg :disposition :reader get-disposition :initform :neutral)
   (inventory :initarg :inventory :reader get-inventory :initform nil)
   (equipment :initarg :equipment :reader get-equipment :initform nil)
   (collection-log :initarg :collection-log :reader get-collection-log :initform nil))
  (:documentation "An immutable positioned, drawable thing on the
playing field (the player, and eventually NPCs/items). X/Y are grid
coordinates; CHAR is the single character used to render it; LEVEL is
the dungeon depth (1-indexed) this entity currently occupies -- only
entities sharing a GAME-STATE's player's LEVEL are drawn by
RENDER-GRID (see GENERATE-DUNGEON). NAME is a human-readable string
(e.g. \"Orc\") used in message-log text (see MOVE-PLAYER's melee
message); BLOCKS-MOVEMENT is a generalized boolean saying whether
other entities (namely the player) cannot walk through this entity's
cell -- see BLOCKING-ENTITY-AT. MAX-HP/HP are the entity's maximum and
current hit points; DEFENSE and POWER are the flat damage-reduction
and damage-dealing stats used by MOVE-PLAYER's/PROCESS-ENEMY-TURNS'
combat math (damage dealt is (MAX 0 (- attacker's POWER defender's
DEFENSE))). RENDER-ORDER is an integer draw-priority used by
RENDER-GRID to decide which entity wins when two share a cell -- 0 for
corpses (drawn first, so any living actor sharing that cell is drawn
on top of it) and 1 for living actors; IS-ALIVE is a generalized
boolean, NIL once an entity's HP has been reduced to 0 or below (see
MOVE-PLAYER's death transformation, which also flips CHAR to #\\%,
RENDER-ORDER to 0, and BLOCKS-MOVEMENT to NIL, turning a dead entity
into an inert corpse). ENERGY is this entity's current action-point
balance (starts at 0 for every entity via MAKE-ORC/MAKE-TROLL/
MAKE-INITIAL-STATE's player, read via ENTITY-ENERGY) and SPEED (read
via ENTITY-SPEED -- named to avoid colliding with CL:SPEED, the
standard OPTIMIZE-declaration quality symbol) is the fixed amount of
ENERGY
this entity accrues each 50ms game tick -- both drive the tick-based
turn scheduler that lets faster entities (e.g. the player, at 50
ENERGY/tick) act more often than slower ones (code monkeys at 10,
Internet Trolls at 5): an entity may act once it has accumulated enough ENERGY to cover
an action's cost (100 for a move, 150 for an attack), and that cost is
then deducted from its balance. HEAL-PROGRESS (read via
ENTITY-HEAL-PROGRESS) is a separate tick counter, currently only ever
advanced for the player (see ACCRUE-HEALING/REDUCE-TICK), tracking
progress toward this entity's next point of natural HP regeneration:
it counts up by 1 every tick and, once it reaches
*RDESCENT-HEAL-TICKS* (200 ticks, i.e. 10 real seconds at the current
50ms tick rate), HP is increased by 1 (never past MAX-HP) and
HEAL-PROGRESS resets to 0. ACTIVE-EFFECTS (read via
GET-ACTIVE-EFFECTS) is a list of STATUS-EFFECT instances currently
attached to this entity, starting empty (NIL) for every entity -- the
generalized replacement for an earlier design with a single bespoke
CONFUSED-TICKS countdown slot (see TECHNICAL_DEBT.md-style copy-paste
concerns this was written to avoid, and ARCHITECTURE_PLAN.md §1): any
number of distinct-KIND effects (e.g. :CONFUSED, and in the future
:FOOD-POISONING, :BURNOUT, :CRIT-BOOST, ...) can be attached
simultaneously, each with its own TICKS-REMAINING countdown and
optional per-tick MAGNITUDE. ENTITY-EFFECT (ENT KIND) looks up a
specific effect (or NIL if absent) -- e.g. (PLUSP (ENTITY-CONFUSED-
TURNS ENT)), a convenience reader over (ENTITY-EFFECT ENT :CONFUSED),
is how PROCESS-ENEMY-TURNS still detects confusion. Every tick,
TICK-STATUS-EFFECTS (called from REDUCE-TICK, alongside ACCRUE-
ENERGY/ACCRUE-HEALING) decrements every attached effect's TURNS-
REMAINING by 1, drops any that reach 0, and applies any non-NIL
MAGNITUDE as a one-time HP delta before doing so -- the single hook a
future per-tick debuff/buff plugs into, rather than a new hardcoded
branch in REDUCE-TICK itself. New effects are attached via APPLY-
STATUS-EFFECT (ENT KIND TICKS-REMAINING &OPTIONAL MAGNITUDE), which
first rolls the target's own SENIORITY-derived SENIORITY-DEFLECTION-
CHANCE -- the single gate every future debuff-inflicting attack/item/
trap should call through, so Deflection Chance (see SENIORITY below)
is automatically and uniformly applied rather than needing to be
re-derived per effect. CAST-REORG-MEMO (the Vague Re-Org Memo item)
is, as of this writing, the only caller that ever attaches an effect
(:CONFUSED, for *RDESCENT-CONFUSION-TICKS* turns), and CONFUSED-
ENTITY-TURN is still the only AI branch PROCESS-ENEMY-TURNS special-
cases on effect presence, mechanically unchanged from the earlier
CONFUSED-TICKS design -- only the underlying storage is now generic. XP (read via GET-XP, since this is the
:READER every other GAME-STATE-adjacent slot in this file uses -- see
package.lisp's GET-... convention) means two different things
depending on which kind of entity it's on: for the player, it is that
player's own accumulated experience total, starting at 0 (see
MAKE-INITIAL-STATE); for a monster (e.g. MAKE-ORC/MAKE-TROLL), it is
instead the fixed XP reward that monster's death is intended to grant
the player, set once at spawn time and never itself increased or
decreased. (Awarding a monster's XP to the player upon a kill is
handled by MOVE-PLAYER's combat branch, which adds the slain target's
GET-XP onto the player's own via UPDATE-ENTITY.) MESSAGE-COLOR (read
via ENTITY-MESSAGE-COLOR) is the CSS color used for message-log
entries about this entity (e.g. \"The code monkey hits you!\" vs
\"The Internet Troll hits you!\", so a player can tell at a glance
which kind of monster a combat message refers to), set once
per-instance at construction time
by each factory (MAKE-ORC/MAKE-TROLL set their own; the default
\"white\" applies to the player and any other entity, like the stairs,
that never appears as the subject of a colored combat message) --
this replaces an earlier design where the color was looked up from a
separate alist keyed on the entity's NAME string, which could
silently drift out of sync with the factories (see
TECHNICAL_DEBT.md item #36). KOMBUCHA (read via GET-KOMBUCHA) is a
count of healing-potion charges this entity is carrying -- currently
only ever meaningful for the player (see MAKE-INITIAL-STATE, which
seeds it to 3, and DRINK-POTION, the only reducer that reads/
decrements it); monsters simply keep the default 0 and never consume
it. RSU (read via GET-RSU) is this entity's own gold/loot currency
balance -- RDESCENT's answer to the traditional dungeon-crawler gold
pile -- currently only ever meaningful for the player (see
MAKE-INITIAL-STATE, which seeds it to 0, and GRAB-ITEM, the only
reducer that increases it, upon picking up a Stock Option
GROUND-ITEM -- see MAKE-GROUND-STOCK-OPTION); monsters keep the
default 0 and never accumulate it. Displayed alongside XP in the
player-stats panel (see RDESCENT-PLAYER-STATS-PACKET/FORMAT-RSU-FOR-HTML).
BANDWIDTH/PIVOT/CAFFEINE-TOLERANCE/DOMAIN-KNOWLEDGE/SENIORITY/SYNERGY/
HYGIENE (read via GET-BANDWIDTH/GET-PIVOT/GET-CAFFEINE-TOLERANCE/
GET-DOMAIN-KNOWLEDGE/GET-SENIORITY/GET-SYNERGY/GET-HYGIENE) are the
seven \"Corporate RPG Stats\" -- integer attributes rolled once at
character creation via ROLL-STAT (4d6, drop the lowest die, sum the
remaining three -- the classic tabletop-RPG ability-score method) and
never changed afterward. Only ever meaningful for the player (see
MAKE-INITIAL-STATE, which rolls all seven); monsters keep the default
of 10 (BANDWIDTH, CAFFEINE-TOLERANCE, DOMAIN-KNOWLEDGE) or 0 (the
other four). Displayed in the player-stats sidebar's stat-row grid
(see RDESCENT-PLAYER-STATS-PACKET's \"val-...\" fields and
RENDER-RDESCENT-PAGE's #player-corporate-stats markup in views.lisp).
Only one of the seven (HYGIENE) is flavor-only -- no game mechanic
currently reads it back. BANDWIDTH, PIVOT, CAFFEINE-TOLERANCE, and
DOMAIN-KNOWLEDGE are already mechanically wired up; SENIORITY and
SYNERGY each have mechanics *designed* but not yet implemented (no
status-effect/debuff, trap/hidden-enemy, vendor/shop, or monster-
aggression system exists yet for either to plug into -- see below):
BANDWIDTH scales damage the player takes from a successful enemy
attack via BANDWIDTH-DAMAGE-MULTIPLIER (see PROCESS-ENEMY-TURNS), so
its default is 10 (the average 4d6-drop-lowest roll) rather than 0,
keeping that multiplier neutral (1.0x) for any ENTITY built without an
explicit :BANDWIDTH (e.g. test fixtures, or monsters, though monsters
never consult it since the multiplier only ever applies to the
player); PIVOT gives the player a flat percent chance (PIVOT * 2.5,
see PIVOT-DODGE-CHANCE) of dodging an incoming enemy attack entirely
(0 damage), rolled fresh before each attack in PROCESS-ENEMY-TURNS --
its default of 0 is safe as-is (0% dodge chance, matching pre-PIVOT
behavior for any ENTITY built without an explicit :PIVOT); CAFFEINE-
TOLERANCE has two mechanical effects, both computed from its own D&D-
style ability modifier, (FLOOR (- CAFFEINE-TOLERANCE 10) 2): it
determines the player's starting MAX-HP via CAFFEINE-TOLERANCE-MAX-HP
(10 + CAFFEINE-TOLERANCE * 2 at current defaults), computed once by
MAKE-INITIAL-STATE and baked into the player's own MAX-HP/HP slots
like any other ENTITY, and it determines how much HP each Kombucha
charge restores via KOMBUCHA-HEAL-AMOUNT (see DRINK-POTION), consulted
fresh every time the player drinks one. Its default is 10 (the
average 4d6-drop-lowest roll, +0 modifier) rather than 0, keeping both
of those neutral for any ENTITY built without an explicit :CAFFEINE-
TOLERANCE -- CAFFEINE-TOLERANCE itself is never re-consulted when
computing MAX-HP after character creation, though, so a mid-game
change to it (there currently is none) would not retroactively resize
MAX-HP. DOMAIN-KNOWLEDGE determines bonus (or penalty) damage on the
player's own targeted/area-effect items (Scroll of PIP, Reply-All
Bomb) via DOMAIN-KNOWLEDGE-BONUS-DAMAGE (see APPLY-ITEM's TARGETED-
ITEM/AREA-EFFECT-ITEM methods): its own D&D-style ability modifier,
(FLOOR (- DOMAIN-KNOWLEDGE 10) 2), times 5, added to the item's base
damage constant and clamped at 0 (via MAX). Its default is 10 (the
average 4d6-drop-lowest roll, +0 modifier, 0 bonus damage) rather than
0 (which would instead read as a -20 penalty), keeping item damage
neutral (equal to the item's own base damage constant) for any ENTITY
built without an explicit :DOMAIN-KNOWLEDGE (e.g. test fixtures);
monsters never consult it since only the player wields items.
DOMAIN-KNOWLEDGE also ties directly to the player's own FOV radius via
DOMAIN-KNOWLEDGE-FOV-RADIUS (*RDESCENT-FOV-RADIUS* (5) plus that same
ability modifier): a DOMAIN-KNOWLEDGE of 16 gives radius 8, 10 gives
the standard 5, and 4 gives radius 2 -- if you know the domain, you
know the codebase, and you can anticipate where the bugs (monsters)
are hiding. Consulted by MOVE-PLAYER (every COMPUTE-FOV call, to
refresh EXPLORED) and MAKE-INITIAL-STATE (the player's starting FOV);
NOT consulted by PROCESS-ENEMY-TURNS, whose own monster-detection
radius is fixed at *RDESCENT-MONSTER-FOV-RADIUS* regardless of the
player's DOMAIN-KNOWLEDGE, so a savvier player seeing further doesn't
also make monsters notice them from further away.
SENIORITY is designed to have two effects, both via its own D&D-
style ability modifier, (FLOOR (- SENIORITY 10) 2): a Deflection
Chance of that modifier times 10% (see SENIORITY-DEFLECTION-CHANCE) --
when a status effect/debuff (e.g. \"Burnout\", \"Confusion\") is about
to be inflicted, this is the target's chance to shrug it off entirely,
rolled by APPLY-STATUS-EFFECT before ever attaching one (SENIORITY 16,
+3 modifier, => 30%; SENIORITY 10, +0 modifier, => 0%, i.e. the
average roll always \"takes the assignment\") -- WIRED UP as of
ARCHITECTURE_PLAN.md §1 (see ACTIVE-EFFECTS above), CAST-REORG-MEMO
being the first caller to go through APPLY-STATUS-EFFECT rather than
attaching :CONFUSED unconditionally; and a flat Detection Chance of
(SENIORITY * 5)% (PLANNED, NOT YET IMPLEMENTED -- there is no trap/
hidden-enemy system in RDESCENT yet to consult it) -- when traps (e.g.
a damaging \"Broken Deployment\" tile) or invisible enemies (e.g. a
\"Micromanager\") are introduced, this is intended to be the player's
chance to notice one the moment it enters their FOV rather than being
surprised by it (SENIORITY 18 => 90%; SENIORITY 10 => 50%). Both
formulas are intentionally recorded here ahead of the systems that
will consult them, so SENIORITY's default (0, like HYGIENE) and
ROLL-STAT range (3-18) already produce sane Deflection/Detection
percentages once those systems exist -- no ENTITY slot or default
change is needed to wire up Detection Chance later.
SYNERGY has two effects, both via its own D&D-style ability modifier,
(FLOOR (- SYNERGY 10) 2): a Price Modifier of 1.0 - (that modifier *
0.05) -- when vendors (vending machines, an IT procurement desk, an HR
requisition portal) are introduced, all their prices are multiplied by
this (SYNERGY 16, +3 modifier, => 0.85, a 15% discount for having CC'd
the right director; SYNERGY 10, +0 modifier, => 1.0, normal prices;
SYNERGY 6, -2 modifier, => 1.10, a 10% \"asshole tax\" for IT hating
you) -- STILL PLANNED, NOT YET IMPLEMENTED, since there is no vendor/
shop system in RDESCENT yet to consult it (see FUTURE_PLANS.md §10);
and a Pacify Chance of (MAX 0 (that modifier * 5))% -- WIRED UP as of
FUTURE_PLANS.md §1 (see SYNERGY-PACIFY-CHANCE) -- rolled once per
freshly spawned monster by SPAWN-MONSTERS-FOR-LEVEL (via SPAWN-TIME-
DISPOSITION): an otherwise-:HOSTILE enemy instead spawns :NEUTRAL and
simply wanders, ignoring the player entirely (SYNERGY 18 => 20%;
SYNERGY 10 => 0%, i.e. the average roll pacifies nothing -- \"everyone
wants a piece of you\"). SYNERGY's default (0, like HYGIENE) and
ROLL-STAT range (3-18) produce sane Price Modifier/Pacify Chance
values -- no ENTITY slot or default change was needed to wire up
Pacify Chance.
HYGIENE is a sliding scale for faction hostility, WIRED UP as of
FUTURE_PLANS.md §1 (see HYGIENE-BANDED-DISPOSITION, consulted once per
freshly spawned monster by SPAWN-MONSTERS-FOR-LEVEL): HYGIENE isn't
just about smell, it's about looking the part. Above 14, you look like
a Suit -- :MANAGEMENT enemies turn :NEUTRAL and ignore you, but
:DISGRUNTLED-DEV enemies stay :HOSTILE (they think you're going to
assign them Jira tickets). Below 8, you look feral -- :DISGRUNTLED-DEV
enemies turn :NEUTRAL (you're one of the pack), but :MANAGEMENT enemies
stay :HOSTILE. Between 8 and 14 inclusive, everyone hates you equally
(the original, pre-HYGIENE-banding behavior). HYGIENE's default (0)
and ROLL-STAT range (3-18) mean any ENTITY built without an explicit
:HYGIENE (e.g. test fixtures, or SPAWN-MONSTERS-FOR-LEVEL's own default
HYGIENE of 10 for callers that don't yet thread the player's real
HYGIENE through) lands in or above the neutral 8-14 band, never
triggering the \"feral\" swing by surprise.
FACTION (read via GET-FACTION, default :NEUTRAL) and DISPOSITION (read
via GET-DISPOSITION, default :NEUTRAL for a plain ENTITY, but :HOSTILE
for any ENEMY that doesn't explicitly override it -- see ENEMY's own
INITIALIZE-INSTANCE :AFTER) are the data-model half of RDESCENT's
faction/aggression model (see ARCHITECTURE_PLAN.md §2): FACTION is an
allegiance keyword (e.g. :DISGRUNTLED-DEV for MAKE-ORC/MAKE-TROLL,
:MANAGEMENT for MAKE-MIDDLE-MANAGER -- see FUTURE_PLANS.md §1) set once
at spawn time by a factory; DISPOSITION is one of :HOSTILE, :NEUTRAL,
:FRIENDLY, or :FLEEING -- the actual behavior PROCESS-ENEMY-TURNS
dispatches an entity's AI turn on (see ENTITY-DISPOSITION-TOWARD, and
PROCESS-ENEMY-TURNS' own docstring for what each of the four values
does). A factory's own DISPOSITION (or ENEMY's :HOSTILE default) is
only the *starting point*: SPAWN-MONSTERS-FOR-LEVEL (via SPAWN-TIME-
DISPOSITION) applies HYGIENE-BANDED-DISPOSITION's Suit/feral swing and
then SYNERGY-PACIFY-CHANCE's independent pacify roll on top of it once
per freshly spawned monster, so the DISPOSITION actually stored on a
monster once it's in GAME-STATE's ENTITIES can already differ from its
factory's own default. ENTITY-DISPOSITION-TOWARD itself still simply
returns ENT's own (already-finalized) DISPOSITION slot verbatim,
ignoring its TARGET argument entirely -- it remains the single seam a
future target-specific rule (e.g. faction-vs-faction turf war) could
plug into later, without requiring any change to PROCESS-ENEMY-TURNS'
own call site.
INVENTORY (read via GET-INVENTORY) is a plain list of RDESCENT-ITEM
instances this entity is carrying -- currently only ever meaningful
for the player (see MAKE-INITIAL-STATE, which seeds it with a starter
set of items, and USE-ITEM, the only reducer that reads/removes from
it); monsters keep the default NIL and never carry items. EQUIPMENT
(read via GET-EQUIPMENT, ARCHITECTURE_PLAN.md §4) is a plist keyed by
gear slot (:WEAPON/:BODY/:HEAD/:OFF-HAND), each value either NIL (that
slot is empty) or an EQUIPPABLE-ITEM currently worn/wielded there --
see EQUIPPED-ITEM/EQUIP-ITEM/UNEQUIP-ITEM. Unlike INVENTORY, an item
in EQUIPMENT is not carried inert -- its own STAT-BONUSES are folded
into this entity's EFFECTIVE-POWER/EFFECTIVE-DEFENSE/EFFECTIVE-MAX-HP
every time those are read, and (if it's the :WEAPON slot) its own
reach/hits-per-turn/on-hit-effect are consulted by RESOLVE-ATTACK
(see ARCHITECTURE_PLAN.md §5) -- POWER/DEFENSE/MAX-HP themselves are
deliberately never mutated to reflect equipped gear (matching the
\"never mutate, always derive\" discipline UPDATE-ENTITY already
follows), so an entity's own base slots stay meaningful (a monster's
innate stats) while gear is purely additive, recomputed fresh on every
read. Currently only ever meaningful for the player (see EQUIP-ITEM/
UNEQUIP-ITEM, the only reducers that populate it); monsters keep the
default NIL (no equipped gear at all) and their EFFECTIVE-* values are
therefore always identical to their raw POWER/DEFENSE/MAX-HP. Never
mutated in place -- see UPDATE-ENTITY. COLLECTION-LOG (read via
GET-COLLECTION-LOG, FUTURE_PLANS.md §16 \"Scavenger Hunt
Collectibles\") is a plain list of keyword collectible item-ids (e.g.
:HOLLERITH-PUNCH-CARDS -- see *RDESCENT-COLLECTIBLE-CATALOG*) this
entity has silently auto-picked-up via MAYBE-AUTO-PICKUP-COLLECTIBLE
(RDESCENT/ACTIONS.LISP) -- currently only ever meaningful for the
player; monsters keep the default NIL. Once every item in a given
collectible set's own COLLECTIBLE-SET-ITEM-IDS is present here (see
COLLECTION-SET-COMPLETE-P), that set's permanent passive bonus (e.g.
HARDWARE-EMULATION-ACTIVE-P) becomes active -- computed fresh from
this list on every read, the same \"computed from current state\"
pattern EFFECTIVE-POWER/EFFECTIVE-DEFENSE already use for equipped
gear, rather than ever being baked into another slot as a one-time
stat change."))

(defclass enemy (entity)
  ()
  (:documentation "A monster-flavored ENTITY (see MAKE-ORC/MAKE-TROLL,
which now MAKE-INSTANCE this class instead of bare ENTITY): behaves
exactly like ENTITY, except that INITIALIZE-INSTANCE :AFTER below
defaults two of its inherited slots whenever a caller constructs one
without supplying the corresponding initarg explicitly: XP defaults to
that same instance's own HP -- a reasonable fallback reward (tougher/
higher-HP monsters naturally grant more XP) for any enemy spawned
without a hand-tuned XP value; and DISPOSITION defaults to :HOSTILE
(rather than plain ENTITY's :NEUTRAL default) -- an ENEMY is presumed
hostile unless a factory says otherwise, matching every monster's
behavior before FACTION/DISPOSITION existed at all (see ENTITY's own
docstring/ARCHITECTURE_PLAN.md §2). MAKE-ORC/MAKE-TROLL still pass
their own fixed :XP (10/25) explicitly, so the XP default only ever
applies to an ENEMY constructed without one; neither currently passes
an explicit :DISPOSITION, so both rely on this :HOSTILE default,
preserving their exact pre-existing always-hostile behavior. (ENTITY's
XP/DISPOSITION slots themselves keep their ordinary :INITFORM
defaults, since :INITFORM/:INITARG options are inherited from the
most specific class that specifies them (see CLHS 7.5.3) -- ENEMY
cannot simply omit :INITFORM on its own slot definitions to make them
unbound, hence this explicit INITARGS check instead of SLOT-BOUNDP.)"))

(defmethod initialize-instance :after ((enemy enemy) &rest initargs &key xp disposition)
  "If ENEMY was constructed without an explicit :XP initarg, default
its XP slot to its own (just-initialized) HP; if constructed without
an explicit :DISPOSITION initarg, default its DISPOSITION slot to
:HOSTILE -- see ENEMY's docstring. Checks INITARGS directly (rather
than SLOT-BOUNDP) because ENTITY's :INITFORM defaults for XP/
DISPOSITION are inherited and always leave both slots bound
regardless of whether either initarg was actually supplied."
  (declare (ignore xp disposition))
  (unless (getf initargs :xp)
    (setf (slot-value enemy 'xp) (hp enemy)))
  (unless (getf initargs :disposition)
    (setf (slot-value enemy 'disposition) :hostile)))

(defclass companion (entity)
  ((bonded :initarg :bonded :reader companion-bonded-p :initform nil)
   (wander-dx :initarg :wander-dx :reader companion-wander-dx :initform 0)
   (wander-dy :initarg :wander-dy :reader companion-wander-dy :initform 0)
   (wander-ticks :initarg :wander-ticks :reader companion-wander-ticks :initform 0))
  (:default-initargs :faction :companion :disposition :friendly)
  (:documentation "The \"Office Doge\" companion pet ENTITY
(FUTURE_PLANS.md §22, \"Companion Pet: The Office Doge\"). Behaves
like ENTITY except: FACTION/DISPOSITION default to :COMPANION/
:FRIENDLY (via :DEFAULT-INITARGS, exactly like FIXTURE's own pattern
below, rather than ENEMY's INITIALIZE-INSTANCE :AFTER, since neither
default here depends on any other just-initialized slot), and it
carries four new slots. WANDER-DX/WANDER-DY/WANDER-TICKS (read via
COMPANION-WANDER-DX/COMPANION-WANDER-DY/COMPANION-WANDER-TICKS) let a
bonded, idle (leashed, nothing-to-fight) Doge commit to a single
wander direction for several turns in a row (see COMMANDS.LISP's
COMPANION-WANDER-STEP/*RDESCENT-COMPANION-WANDER-PERSISTENCE*) instead
of re-rolling a fresh random direction every single tick, which used
to make Doge look like it was jittering in place rather than
wandering. WANDER-TICKS counts down the remaining turns left committed
to (WANDER-DX . WANDER-DY); once it reaches 0 (or the committed
direction becomes invalid -- a wall, another entity, or straying
outside the leash), a fresh direction/persistence count is rolled.
BONDED (read via COMPANION-BONDED-P): NIL for a
still-wild, not-yet-discovered Doge simply wandering a level like any
other neutral-ish ENTITY (see SPAWN-DOGE-FOR-LEVEL); T once the player
has walked into it and adopted it (see MOVE-PLAYER's own COMPANION
branch), at which point it stops being a per-level fixture and starts
following the player everywhere -- including across USE-STAIRS depth
transitions, exactly like the player's own ENTITY does (see USE-STAIRS'
own COMPANION-carryover handling) -- and fighting on their behalf (see
COMPANION-AI-TURN, PROCESS-ENEMY-TURNS' own COMPANION dispatch). A
bonded COMPANION can still be killed by a hostile monster (see
PROCESS-ENEMY-TURNS' own ATTACKING-COMPANION branch) -- if so, it is
simply left behind as an ordinary corpse on whatever level it died on
(not carried further), and FIND-BONDED-COMPANION no longer finds one,
so the next level the player descends to gets its own chance at a
fresh wild Doge (see USE-STAIRS/SPAWN-DOGE-FOR-LEVEL's HAS-COMPANION-P
gate) -- \"you cannot attack it, but if it is killed you will find a
new one on the next level,\" per the feature's own design."))

(defun companion-p (ent)
  "Return true if ENT is a COMPANION (bonded or still-wild) -- see
COMPANION's own docstring. A thin, named wrapper around (TYPEP ENT
'COMPANION) purely so call sites read as intent (\"is this the Office
Doge\") rather than a bare TYPEP."
  (typep ent 'companion))

(defun bonded-companion-p (ent)
  "Return true if ENT is a COMPANION that has already been adopted by
the player (COMPANION-BONDED-P is true) -- see COMPANION's own
docstring. A still-wild (undiscovered) COMPANION is COMPANION-P but
not BONDED-COMPANION-P."
  (and (companion-p ent) (companion-bonded-p ent)))

(defun find-bonded-companion (entities)
  "Return the first BONDED-COMPANION-P entity in ENTITIES, or NIL if
the player currently has no bonded Office Doge. There is at most one
bonded COMPANION in any GAME-STATE's ENTITIES at a time (MOVE-PLAYER's
own bonding branch never creates a second one while one is already
bonded -- see SPAWN-DOGE-FOR-LEVEL's HAS-COMPANION-P gate, which
likewise never spawns a fresh wild Doge while one is already bonded),
so the first match is unambiguous."
  (find-if #'bonded-companion-p entities))

(defparameter *rdescent-doge-max-hp* 8
  "MAX-HP/HP a freshly spawned Office Doge COMPANION starts with (see
MAKE-DOGE) -- deliberately low relative to the player's own starting
MAX-HP (typically in the 20s-30s, see CAFFEINE-TOLERANCE-MAX-HP) or
even a code monkey's 10, so Doge is a real, meaningful combat
participant that can actually die to sustained enemy fire (per
FUTURE_PLANS.md §22's own \"if it is killed\" framing) rather than an
unkillable escort.")

(defparameter *rdescent-doge-power* 3
  "POWER a freshly spawned Office Doge COMPANION starts with (see
MAKE-DOGE) -- comparable to a code monkey's own POWER 3, so Doge is a
genuinely useful (but not run-trivializing) damage source.")

(defparameter *rdescent-doge-speed* 30
  "SPEED a freshly spawned Office Doge COMPANION starts with (see
MAKE-DOGE/ENTITY's own ENERGY/SPEED slot docstring) -- faster than any
monster today (an Internet Troll's SPEED 5 or a code monkey's SPEED
10) so Doge, true to a dog's own energy, keeps up with and can even
outrun the player's own SPEED 50, without quite matching it.")

(defun make-doge (x y level &key bonded)
  "Pure factory: return a fresh COMPANION representing the Office Doge
at (X, Y) on LEVEL (FUTURE_PLANS.md §22) -- char #\\d, NAME \"Office
Doge\", BLOCKS-MOVEMENT T (so it occupies a real tile like any other
actor -- MOVE-PLAYER's own COMPANION branch special-cases colliding
with it rather than ever treating that collision as an ordinary
attack, per the feature's own \"you cannot attack it\" requirement),
*RDESCENT-DOGE-MAX-HP* MAX-HP/HP, 0 DEFENSE, *RDESCENT-DOGE-POWER*
POWER, RENDER-ORDER 1, IS-ALIVE T, ENERGY 0, SPEED
*RDESCENT-DOGE-SPEED*, XP 0 (a COMPANION is never killed by the player
for a reward -- see MOVE-PLAYER's own COMPANION branch, which never
falls through to RESOLVE-ATTACK at all), MESSAGE-COLOR \"#f1c40f\"
(gold) -- and, per COMPANION's own :DEFAULT-INITARGS, FACTION
:COMPANION/DISPOSITION :FRIENDLY. BONDED (see COMPANION-BONDED-P)
defaults to NIL -- a freshly discovered, still-wild Doge -- but
SPAWN-DOGE-FOR-LEVEL never passes T here either; only MOVE-PLAYER's
own bonding branch (via UPDATE-ENTITY, not a fresh MAKE-DOGE call)
ever flips an existing Doge's BONDED to T, so this KEY argument exists
mainly for tests that want to construct an already-bonded COMPANION
directly without going through a full bonding interaction."
  (make-instance 'companion :x x :y y :char #\d :name "Office Doge" :blocks-movement t :level level
                            :max-hp *rdescent-doge-max-hp* :hp *rdescent-doge-max-hp* :defense 0
                            :power *rdescent-doge-power* :render-order 1 :is-alive t
                            :energy 0 :speed *rdescent-doge-speed* :xp 0
                            :message-color "#f1c40f" :bonded bonded))

(defclass fixture (entity)
  ()
  (:default-initargs :blocks-movement nil :render-order 0 :is-alive nil)
  (:documentation "An ENTITY representing a stationary, non-hostile
map object the player interacts with via an explicit INTERACT-COMMAND
(see INTERACT-FIXTURE/FIXTURE-AT) rather than by colliding with it --
ARCHITECTURE_PLAN.md §3's Fixture/Interactable Entity Hierarchy. FIXTURE
itself carries no new slots beyond ENTITY's own; it exists purely so
PROCESS-ENEMY-TURNS' acting-entities filter can exclude it (a FIXTURE
never gets an AI turn, like GROUND-ITEM already never does) without
needing a new per-type special case -- see FIXTURE's own default
IS-ALIVE NIL, exactly GROUND-ITEM's own convention. BLOCKS-MOVEMENT
defaults to NIL (like GROUND-ITEM/the stairs markers) so the player can
walk onto a FIXTURE's tile -- INTERACT-COMMAND, like GRAB-COMMAND,
infers its target from the player's own current position rather than
from a payload, so the player must be able to stand on the same cell.
RENDER-ORDER defaults to 0 (drawn underneath the player/monsters,
mirroring GROUND-ITEM/the stairs markers) so a FIXTURE never hides a
living actor standing on its tile.

See SHRINE-FIXTURE below for the first concrete subclass (per
ARCHITECTURE_PLAN.md §11's build order, shrines first -- simplest,
already fully designed in FUTURE_PLANS.md §12). Future subclasses
(VENDOR-FIXTURE/NPC-FIXTURE/TRAP-FIXTURE, per ARCHITECTURE_PLAN.md §3)
each override whichever of these :DEFAULT-INITARGS don't fit their own
behavior -- e.g. a future TRAP-FIXTURE would override IS-ALIVE to T so
it can be attacked and destroyed once revealed."))

(defclass shrine-fixture (fixture)
  ((shrine-kind :initarg :shrine-kind :reader get-shrine-kind)
   (use-count :initarg :use-count :reader get-use-count :initform nil))
  (:documentation "A FIXTURE representing a free-standing, always-
walk-up-able dispenser of some free consumable effect -- ARCHITECTURE_
PLAN.md §3/§9, FUTURE_PLANS.md §12's \"Free-Standing Shrines (Break-
Room Dispensers)\": distinct from a future paid VENDOR-FIXTURE (no RSU
cost) and from a carried consumable (no INVENTORY slot is ever
touched) -- interacting (see INTERACT-SHRINE) applies the effect
directly to the player standing on the shrine's own tile.

SHRINE-KIND (read via GET-SHRINE-KIND) selects which effect INTERACT-
SHRINE applies: :ESPRESSO (The Espresso Machine -- restores ENERGY on
the spot, an on-demand version of drinking a Kombucha's ENERGY-turn
economics without carrying one), :KOMBUCHA-BAR (The Kombucha Bar --
heals HP via the exact same KOMBUCHA-HEAL-AMOUNT/CAFFEINE-TOLERANCE
formula DRINK-POTION already uses, a free-standing walk-up version of
drinking a carried Kombucha), or :WATER-COOLER (The Water Cooler -- a
minor flat HP heal; FUTURE_PLANS.md §13's Branded Corporate Yeti Mug
off-hand item is meant to upgrade this shrine's effect once §4's
Equipment System exists, but until then it's simply the plainest,
always-available fallback option). See MAKE-ESPRESSO-MACHINE/
MAKE-KOMBUCHA-BAR/MAKE-WATER-COOLER for the concrete factories.

USE-COUNT (read via GET-USE-COUNT) is the remaining number of charges
before the shrine reads \"OUT OF ORDER\" (FUTURE_PLANS.md §12's
suggested balancing lever, so shrines don't fully obsolete the Tier
1/2 pharmacy items or make floors trivially survivable by camping next
to one) -- decremented by 1 on every successful INTERACT-SHRINE call,
via UPDATE-ENTITY's own :USE-COUNT keyword (mirroring GROUND-ITEM's
:PAYLOAD convention: a slot not shared with plain ENTITY/FIXTURE
instances, so not accepted as a MAKE-INSTANCE initarg for a
non-SHRINE-FIXTURE class, must be carried over explicitly by
UPDATE-ENTITY or it would silently go unbound on the very next
REDUCE-TICK sweep). Defaults to NIL, meaning unlimited charges -- the
concrete factories all pass an explicit finite *RDESCENT-SHRINE-USE-
LIMIT*, but a caller (e.g. a future \"blessed\" shrine variant) can
still construct an infinite-use one directly."))

(defparameter *rdescent-shrine-use-limit* 3
  "Default USE-COUNT (see SHRINE-FIXTURE) every MAKE-ESPRESSO-MACHINE/
MAKE-KOMBUCHA-BAR/MAKE-WATER-COOLER factory passes: 3 charges before
the shrine reads \"OUT OF ORDER\" until the next floor -- FUTURE_
PLANS.md §12's own suggested balancing lever, chosen as a middle
ground between \"unlimited\" (which would obsolete the Tier 1/2
pharmacy items) and \"one-shot\" (which would make a shrine barely
better than a Kombucha ground-item pickup).")

(defparameter *rdescent-espresso-energy-restore* 300
  "ENERGY restored by a successful INTERACT-SHRINE call against an
:ESPRESSO shrine (see SHRINE-FIXTURE/MAKE-ESPRESSO-MACHINE), applied
*on top of* the flat *RDESCENT-MOVE-ENERGY-COST* every INTERACT-
FIXTURE call already deducts for spending this turn -- so a successful
espresso interaction is a net ENERGY gain (300 - 100 = +200), enough
for two extra moves, worth the trip to stand on the machine's tile.")

(defparameter *rdescent-water-cooler-heal-amount* 2
  "Flat HP healed by a successful INTERACT-SHRINE call against a
:WATER-COOLER shrine (see SHRINE-FIXTURE/MAKE-WATER-COOLER) -- a
small, CAFFEINE-TOLERANCE-independent amount (unlike :KOMBUCHA-BAR's
own KOMBUCHA-HEAL-AMOUNT-derived heal), intentionally the plainest,
least tactically interesting shrine effect until FUTURE_PLANS.md §13's
Branded Corporate Yeti Mug (an off-hand item, not yet implemented --
see ARCHITECTURE_PLAN.md §4's Equipment System) gives it a reason to
be better than this fallback.")

(defun make-espresso-machine (x y level)
  "Pure factory: return a fresh SHRINE-FIXTURE at (X, Y) on LEVEL
representing an Espresso Machine (see SHRINE-FIXTURE's own docstring
for what interacting with one does) -- char #\\&, NAME \"Espresso
Machine\", MESSAGE-COLOR \"#c0392b\" (a coffee-dark red-brown, the
color INTERACT-SHRINE's messages about this shrine are displayed in),
SHRINE-KIND :ESPRESSO, USE-COUNT *RDESCENT-SHRINE-USE-LIMIT* (3)."
  (make-instance 'shrine-fixture :x x :y y :char #\& :name "Espresso Machine" :level level
                                 :message-color "#c0392b" :shrine-kind :espresso
                                 :use-count *rdescent-shrine-use-limit*))

(defun make-kombucha-bar (x y level)
  "Pure factory: return a fresh SHRINE-FIXTURE at (X, Y) on LEVEL
representing a Kombucha Bar (see SHRINE-FIXTURE's own docstring for
what interacting with one does) -- char #\\&, NAME \"Kombucha Bar\",
MESSAGE-COLOR \"green\" (matching MAKE-GROUND-KOMBUCHA's own pickup
message color), SHRINE-KIND :KOMBUCHA-BAR, USE-COUNT
*RDESCENT-SHRINE-USE-LIMIT* (3)."
  (make-instance 'shrine-fixture :x x :y y :char #\& :name "Kombucha Bar" :level level
                                 :message-color "green" :shrine-kind :kombucha-bar
                                 :use-count *rdescent-shrine-use-limit*))

(defun make-water-cooler (x y level)
  "Pure factory: return a fresh SHRINE-FIXTURE at (X, Y) on LEVEL
representing a Water Cooler (see SHRINE-FIXTURE's own docstring for
what interacting with one does) -- char #\\&, NAME \"Water Cooler\",
MESSAGE-COLOR \"#3498db\" (a plain, unglamorous blue, the color
INTERACT-SHRINE's messages about this shrine are displayed in),
SHRINE-KIND :WATER-COOLER, USE-COUNT *RDESCENT-SHRINE-USE-LIMIT* (3)."
  (make-instance 'shrine-fixture :x x :y y :char #\& :name "Water Cooler" :level level
                                 :message-color "#3498db" :shrine-kind :water-cooler
                                 :use-count *rdescent-shrine-use-limit*))

(defclass trap-fixture (fixture)
  ((hidden-p :initarg :hidden-p :reader get-hidden-p :initform t))
  (:documentation "A FIXTURE representing a stationary, initially-
hidden hazard (FUTURE_PLANS.md §8, \"Traps & Hidden Enemies\";
ARCHITECTURE_PLAN.md §3's own TRAP-FIXTURE sketch) -- e.g. MAKE-BROKEN-
DEPLOYMENT-TRAP. Reuses FIXTURE's existing BLOCKS-MOVEMENT NIL default
(so the player's own step onto its tile is an ordinary, uninterrupted
move rather than a MOVE-PLAYER melee bump -- see BLOCKING-ENTITY-AT,
which only ever finds BLOCKS-MOVEMENT T entities) so that same step can
instead trigger the trap: MOVE-PLAYER's own open-floor branch looks up
TRAP-AT after moving there and, if found, runs RESOLVE-ATTACK-ON-
PLAYER against it exactly like an ordinary monster's attack (ATTACKER's
own POWER/DEFENSE, both plain ENTITY slots, are all RESOLVE-ATTACK
needs -- no new combat code path). Unlike ARCHITECTURE_PLAN.md §3's own
sketch (\"IS-ALIVE T so it can be attacked and destroyed once
revealed\"), TRAP-FIXTURE deliberately keeps FIXTURE's inherited
IS-ALIVE NIL default: PROCESS-ENEMY-TURNS' ACTING-ENTITIES filter gates
solely on IS-ALIVE (plus visibility), so an IS-ALIVE T trap would be
swept into that same loop and given an ordinary AI turn -- ENTITY-
DISPOSITION-TOWARD's :NEUTRAL default (no ENEMY-style :HOSTILE
override) would then send it down NEUTRAL-WANDER-TURN, in direct
violation of FUTURE_PLANS.md §8's own \"never move (no AI turn/
pathfinding -- they simply sit at their spawn x/y)\" requirement.
Keeping IS-ALIVE NIL sidesteps that entirely, at the cost of not (yet)
supporting \"attack the trap to disarm it\" as a separate player
action -- a future extension, not required by FUTURE_PLANS.md §8's own
text.
HIDDEN-P (read via GET-HIDDEN-P, default T) is whether this trap is
still concealed: RENDER-GRID (RDESCENT/SERVER.LISP) skips drawing a
HIDDEN-P trap's own CHAR entirely (the tile underneath renders as if
nothing were there), so the player has no visual warning before
stepping on one for the first time. HIDDEN-P flips to NIL -- and stays
NIL for the rest of the run, matching how a discovered secret is
permanent in most roguelikes -- either the moment the trap actually
triggers (MOVE-PLAYER's own open-floor branch always reveals a trap it
just triggered, hit or dodged) or independently, the instant it enters
the player's field of view, via MAYBE-REVEAL-HIDDEN-ENTITIES rolling
SENIORITY-DETECTION-CHANCE against the player's own SENIORITY Corporate
RPG Stat (FUTURE_PLANS.md §8's Detection Chance formula) -- a keen-eyed
(high-SENIORITY) player can spot and route around a trap before ever
triggering it. Not a plain ENTITY slot, so (like SHRINE-FIXTURE's own
USE-COUNT/GROUND-ITEM's PAYLOAD) UPDATE-ENTITY must carry it over
explicitly via its own :HIDDEN-P keyword, or a TRAP-FIXTURE passed
through an ordinary REDUCE-TICK sweep would silently forget whether
it had already been revealed."))

(defparameter *rdescent-broken-deployment-power* 4
  "POWER (see MAKE-BROKEN-DEPLOYMENT-TRAP/TRAP-FIXTURE) dealt by a
Broken Deployment trap's trigger, run through the same RESOLVE-ATTACK-
ON-PLAYER math as an ordinary monster's attack -- roughly between a
code monkey's (3) and an Internet Troll's (4), since a trap you walk
into blind is meant to sting but not be a guaranteed significant HP
loss on its own.")

(defun make-broken-deployment-trap (x y level)
  "Pure factory: return a fresh TRAP-FIXTURE at (X, Y) on LEVEL
representing a Broken Deployment -- FUTURE_PLANS.md §8's own working
placeholder name for its damaging trap archetype (\"steps on a bad
build, takes damage\") -- char #\\^ (only ever drawn once GET-HIDDEN-P
is NIL -- see TRAP-FIXTURE's own docstring), NAME \"Broken
Deployment\", MESSAGE-COLOR \"#e74c3c\" (a warning red, the color
combat messages about this trap triggering are displayed in), POWER
*RDESCENT-BROKEN-DEPLOYMENT-POWER* (4), HIDDEN-P T (FIXTURE's own
default, passed explicitly here for clarity)."
  (make-instance 'trap-fixture :x x :y y :char #\^ :name "Broken Deployment" :level level
                                :message-color "#e74c3c" :power *rdescent-broken-deployment-power*
                                :hidden-p t))

(defclass vendor-fixture (fixture)
  ()
  (:documentation "A FIXTURE representing a paid, always-in-stock
vending machine -- ARCHITECTURE_PLAN.md §3/§8, FUTURE_PLANS.md §10
(\"Vendors / Shops\"): distinct from a free SHRINE-FIXTURE (every
purchase costs RSU) and from a carried consumable (an unpurchased item
never occupies an INVENTORY slot). Unlike SHRINE-FIXTURE, VENDOR-
FIXTURE carries no USE-COUNT of its own -- its \"stock\" is the fixed,
shared *RDESCENT-VENDOR-STOCK-TABLE* catalog (RSU, not a physical
charge count, is the resource that gates repeat purchases; a vending
machine is always restocked), so a second player -- or the same
player returning later -- finds the exact same wares still for sale.
Interacting (INTERACT-WITH-FIXTURE, RDESCENT/ACTIONS.LISP) merely
prints the current wares/prices to MESSAGE-LOG (a free \"look\", no
ENERGY spent, mirroring a shop window rather than a shrine's own
walk-up-and-consume interaction) -- PURCHASE-ITEM (a separate reducer,
driven by PURCHASE-COMMAND) is what actually spends RSU/ENERGY and
grants an item. See MAKE-VENDING-MACHINE for the one concrete factory
implemented so far (mirroring FUTURE_PLANS.md §8/§9's own \"one
archetype first\" precedent for TRAP-FIXTURE/keys)."))

(defstruct vendor-stock-entry
  "One row of *RDESCENT-VENDOR-STOCK-TABLE* (FUTURE_PLANS.md §10):
NAME is this entry's human-readable display name (used verbatim in
VENDOR-STOCK-LISTING-TEXT/PURCHASE-ITEM's own messages); BASE-PRICE is
its RSU cost before SYNERGY-PRICE-MODIFIER is applied (see
VENDOR-ITEM-PRICE); PAYLOAD is either the keyword :KOMBUCHA (PURCHASE-
ITEM increments the buyer's own KOMBUCHA counter, subject to
RDESCENT-TIER-KOMBUCHA-LIMIT, exactly like a free Kombucha GROUND-ITEM
pickup -- see GRAB-ITEM) or a function-designator of no arguments
returning a freshly constructed RDESCENT-ITEM (PURCHASE-ITEM appends
it to the buyer's own INVENTORY, subject to RDESCENT-TIER-INVENTORY-
LIMIT, exactly like picking up that same item off the floor) -- never
itself mutated or shared across purchases, so two players (or the same
player buying twice) each get their own fresh instance. All slots
:READ-ONLY -- entries are never mutated after construction, mirroring
SPAWN-TABLE-ENTRY's own convention."
  (name nil :read-only t)
  (base-price nil :read-only t)
  (payload nil :read-only t))

(defun vendor-item-price (base-price synergy &optional free-p)
  "Return the actual RSU cost of a VENDOR-STOCK-ENTRY whose own
BASE-PRICE is BASE-PRICE, for a buyer with SYNERGY Corporate RPG Stat
SYNERGY (see SYNERGY-PRICE-MODIFIER) -- (MAX 1 (ROUND (* BASE-PRICE
(SYNERGY-PRICE-MODIFIER SYNERGY)))). The outer MAX 1 guarantees even a
very high SYNERGY's steep discount never rounds an item's price down
to 0 (RSU 0 would make a purchase entirely free, e.g. an infinite-use
Kombucha tap) -- the smallest a purchase can ever cost is 1 RSU,
*unless* FREE-P is true (see PLATINUM-CORPORATE-AMEX-ACTIVE-P,
FUTURE_PLANS.md §15), in which case every purchase costs a flat 0 RSU
regardless of BASE-PRICE/SYNERGY.
Called by both VENDOR-STOCK-LISTING-TEXT (to display each entry's
current price to a specific player) and PURCHASE-ITEM (to actually
charge it), so the listed price and the charged price are always
exactly the same number for the same player at the same moment."
  (if free-p
      0
      (max 1 (round (* base-price (synergy-price-modifier synergy))))))

(defun make-vending-machine (x y level)
  "Pure factory: return a fresh VENDOR-FIXTURE at (X, Y) on LEVEL
representing a Vending Machine (see VENDOR-FIXTURE's own docstring for
what interacting with/purchasing from one does) -- char #\\=, NAME
\"Vending Machine\", MESSAGE-COLOR \"#27ae60\" (a cash-register green,
the color INTERACT-WITH-FIXTURE's wares listing and PURCHASE-ITEM's
own purchase messages about this vendor are displayed in)."
  (make-instance 'vendor-fixture :x x :y y :char #\= :name "Vending Machine" :level level
                                 :message-color "#27ae60"))

(defclass npc-fixture (fixture)
  ((npc-kind :initarg :npc-kind :reader get-npc-kind))
  (:documentation "A FIXTURE representing a stationary, friendly NPC
who offers dialogue/a quest on interaction, rather than a shrine's
consume-on-touch effect or a vendor's paid catalog -- ARCHITECTURE_
PLAN.md §3, FUTURE_PLANS.md §11 (\"NPCs & Quest Givers\"). Unlike
SHRINE-FIXTURE/VENDOR-FIXTURE, an NPC-FIXTURE's own instance carries no
mutable state at all (no USE-COUNT, no per-instance stock) -- all
quest *progress* (accepted/kill-count/reward-claimed) lives on the
per-player GAME-STATE itself (see IT-GUY-QUEST-ACCEPTED-P/IT-GUY-
QUEST-KILLS/IT-GUY-QUEST-REWARD-CLAIMED-P, MECHANICS.LISP), exactly
like KEYS-HELD/DOORS-OPENED already do for §9's keys -- so a second
player, or the same player revisiting later, always finds the exact
same NPC standing there regardless of anyone else's own quest state.

NPC-KIND (read via GET-NPC-KIND) selects which NPC-FIXTURE-specific
INTERACT-WITH-FIXTURE method applies -- currently only :DISGRUNTLED-
IT-GUY exists (see MAKE-DISGRUNTLED-IT-GUY), mirroring SHRINE-FIXTURE's
own SHRINE-KIND-selects-the-effect convention and FUTURE_PLANS.md
§8/§9/§10's own \"one archetype first\" precedent for TRAP-FIXTURE/
keys/VENDOR-FIXTURE."))

(defparameter *rdescent-it-guy-quest-kill-target* 5
  "The number of MIDDLE-MANAGER kills (see RECORD-MIDDLE-MANAGER-
KILLS, MECHANICS.LISP) the Disgruntled IT Guy's own quest requires
before its reward becomes claimable -- FUTURE_PLANS.md §11's own
literal \"Kill 5 Middle Managers on this floor\" example (\"on this
floor\" is not separately enforced -- kills made on any level after
accepting still count, exactly like §9's KEYS-HELD/DOORS-OPENED are
already tracked per-player rather than per-level).")

(defparameter *rdescent-it-guy-quest-reward-rsu* 1000
  "The flat RSU windfall IT-GUY-QUEST-COMPLETE-P's reward pays out
(see INTERACT-WITH-FIXTURE's own NPC-FIXTURE method) -- a plain RSU
grant via the existing GET-RSU/UPDATE-ENTITY machinery, rather than a
new Root Password Post-It Note item (FUTURE_PLANS.md §15's own
skeleton-key reward remains an unimplemented stretch goal, exactly as
already noted for §9's own locked-door system) -- keeping this quest's
reward mechanism entirely reuse of plumbing this codebase already
has.")

(defun make-disgruntled-it-guy (x y level)
  "Pure factory: return a fresh NPC-FIXTURE at (X, Y) on LEVEL
representing the Disgruntled IT Guy (FUTURE_PLANS.md §11's own
example quest-giver) -- char #\\I, NAME \"The Disgruntled IT Guy\",
NPC-KIND :DISGRUNTLED-IT-GUY, MESSAGE-COLOR \"#7f8c8d\" (a weary
helpdesk gray, deliberately distinct from MIDDLE-MANAGER's own
\"#3498db\" corporate blue so the two are never visually confused in
the message log). See INTERACT-WITH-FIXTURE's own NPC-FIXTURE method
for what talking to him actually does (offering, tracking, and paying
out his kill-quest)."
  (make-instance 'npc-fixture :x x :y y :char #\I :name "The Disgruntled IT Guy" :level level
                              :npc-kind :disgruntled-it-guy :message-color "#7f8c8d"))

(defclass plaque-fixture (fixture)
  ((plaque-text :initarg :plaque-text :reader get-plaque-text))
  (:documentation "A FIXTURE representing a stationary commemorative
plaque placed on a dungeon tier's own ultimate, final LEVEL (see
RDESCENT-TIER-MAX-DEPTH/SPAWN-PLAQUE-FOR-LEVEL, RDESCENT/DUNGEON.LISP)
-- congratulating the player for reaching the bottom of this tier's
dungeon and, per this feature's own design, nudging them toward the
next tier up. Unlike SHRINE-FIXTURE/VENDOR-FIXTURE/NPC-FIXTURE, a
PLAQUE-FIXTURE has no gameplay effect at all: interacting with it (the
't' INTERACT-COMMAND -- see INTERACT-WITH-FIXTURE's own PLAQUE-FIXTURE
method) never spends ENERGY or touches PLAYER, it only pushes a \"You
read the ~A.\" message onto MESSAGE-LOG and sets the :PLAQUE-TEXT
GAME-STATE flag to this fixture's own PLAQUE-TEXT, which RDESCENT-
OUTBOUND-PACKETS (RDESCENT/SERVER.LISP) turns into a one-shot \"plaque\"
packet the client (/js/rdescent.js) uses to pop open a read-only modal
window -- see TICK-ALL-CLIENTS' own flag-clearing step for why that
packet is only ever sent once per read rather than resent every tick.

PLAQUE-TEXT (read via GET-PLAQUE-TEXT) is the exact congratulatory
message shown in that modal -- see MAKE-PLAQUE, which selects between
*RDESCENT-ANONYMOUS-TIER-PLAQUE-TEXT*/*RDESCENT-FREE-TIER-PLAQUE-TEXT*
based on which TIER's final LEVEL this plaque was spawned on."))

(defparameter *rdescent-anonymous-tier-plaque-text*
  "Congratulations, intrepid contractor! You have survived to the
bottom of the anonymous trial dungeon and cleared every floor it has
to offer. If you enjoyed the descent, open a free account to unlock a
much larger dungeon with a far wider variety of monsters and items --
there is plenty more waiting below floor 8."
  "The PLAQUE-TEXT (see PLAQUE-FIXTURE) MAKE-PLAQUE selects for a
plaque spawned on the anonymous tier's own final LEVEL (RDESCENT-TIER-
MAX-DEPTH of NIL -- floor 8) -- congratulates the player on finishing
the anonymous trial dungeon and invites them to open a free account
for the larger \"CONS\" tier dungeon (RDESCENT-TIER-MAX-DEPTH 128).")

(defparameter *rdescent-free-tier-plaque-text*
  "Congratulations, valued employee! You have survived to the bottom
of the dungeon and cleared every floor a free account has to offer. A
subscription tier with an even larger dungeon is coming soon -- contact
jrm for details if you'd like early access."
  "The PLAQUE-TEXT (see PLAQUE-FIXTURE) MAKE-PLAQUE selects for a
plaque spawned on the free (\"CONS\") tier's own final LEVEL (RDESCENT-
TIER-MAX-DEPTH of \"CONS\" -- floor 128) -- congratulates the player on
finishing the free dungeon and invites them to inquire about a
not-yet-launched paid subscription tier. Also the fallback text for
any higher tier's own final LEVEL (\"CADR\"/\"LAMBDA\") until each gets
its own dedicated congratulatory text -- see MAKE-PLAQUE.")

(defun make-plaque (x y level tier)
  "Pure factory: return a fresh PLAQUE-FIXTURE at (X, Y) on LEVEL
(expected to be TIER's own RDESCENT-TIER-MAX-DEPTH -- see SPAWN-PLAQUE-
FOR-LEVEL, which enforces this) -- char #\\P, NAME \"Commemorative
Plaque\", MESSAGE-COLOR \"#f1c40f\" (a celebratory badge-gold, matching
GROUND-ITEM keys' own color), PLAQUE-TEXT selected by TIER:
*RDESCENT-ANONYMOUS-TIER-PLAQUE-TEXT* when TIER is NIL, or
*RDESCENT-FREE-TIER-PLAQUE-TEXT* for \"CONS\" (and, for now, any other/
higher tier too, until each gets its own dedicated text -- see that
parameter's own docstring)."
  (make-instance 'plaque-fixture :x x :y y :char #\P :name "Commemorative Plaque" :level level
                                 :message-color "#f1c40f"
                                 :plaque-text (if (null tier)
                                                  *rdescent-anonymous-tier-plaque-text*
                                                  *rdescent-free-tier-plaque-text*)))

(defclass tile ()
  ((walkable :initarg :walkable :reader get-walkable)
   (char :initarg :char :reader get-char)
   (room-kind :initarg :room-kind :reader get-room-kind :initform nil)
   (locked-key-id :initarg :locked-key-id :reader get-locked-key-id :initform nil)
   (locked-key-name :initarg :locked-key-name :reader get-locked-key-name :initform nil))
  (:documentation "An immutable single cell of a GAME-MAP: WALKABLE is
a generalized boolean saying whether an entity may move onto this
tile, and CHAR is the character used to render it (e.g. #\\. for
floor, #\\# for a wall). Never mutated in place -- a differently-typed
cell is always a fresh TILE instance.

ROOM-KIND (see ARCHITECTURE_PLAN.md §6) tags which kind of room this
floor tile belongs to -- one of :CUBICLE, :OPEN-OFFICE, :SERVER-ROOM,
or NIL (the default, meaning either a wall tile or a corridor tile
carved by DIG-TUNNEL, neither of which belongs to any specific room).
ROOM-KIND is stamped once, at generation time, by DIG-ROOM (from the
enclosing RECT-ROOM's own ROOM-KIND, itself rolled by CHOOSE-ROOM-KIND
in GENERATE-DUNGEON) and is never mutated afterward -- it is
deterministic, immutable dungeon geometry exactly like WALKABLE/CHAR
already are, so it is safe to memoize in *DUNGEON-CACHE* alongside the
rest of the TILE array. See ROOM-ACOUSTICS for the one predicate
derived from ROOM-KIND today (used by a future §18 Open-Office Stealth
Penalty); nothing with *mutable* per-player state (a shrine's use
count, a trap's hidden-p) may ever be stored here -- see §6's own
\"important distinction\" about what is safe to bake into shared,
cached dungeon geometry versus what must live in a player's own
GAME-STATE entities list instead.

LOCKED-KEY-ID/LOCKED-KEY-NAME (FUTURE_PLANS.md §9, \"Keys & Locked
Doors\") mark this tile as a locked door rather than plain corridor
floor: LOCKED-KEY-ID is the keyword a player's own KEYS-HELD flag
(ARCHITECTURE_PLAN.md §9) must contain to unlock it, LOCKED-KEY-NAME
its human-readable display name (e.g. \"Corporate Badge\"), used in
MOVE-PLAYER's own \"you need a ~A\" message. Both NIL for every
ordinary tile. Like ROOM-KIND, these are stamped once at generation
time (by PLACE-LOCKED-DOOR) and never mutated -- WALKABLE stays NIL
forever even after a specific player unlocks it, since \"this player
has opened this door\" is *mutable per-player state* and so must live
in that player's own GAME-STATE :DOORS-OPENED flag instead (see
DOOR-OPENED-P), never in this shared, cross-player-cached TILE."))

(defclass game-map ()
  ((tiles :initarg :tiles :reader get-tiles))
  (:documentation "An immutable playing-field layout: TILES is a 2D
array of TILE instances, indexed (AREF TILES Y X), i.e. row (Y) first,
then column (X). See MAKE-INITIAL-MAP for the pure factory that builds
one."))

(defclass game-state ()
  ((player :initarg :player :reader get-player)
   (entities :initarg :entities :reader get-entities :initform nil)
   (map :initarg :map :reader get-map)
   (current-depth :initarg :current-depth :reader get-current-depth :initform 1)
   (levels :initarg :levels :reader get-levels :initform (fset:empty-map)
           :documentation "An immutable FSET:MAP from dungeon depth
(a positive integer) to a DUNGEON-LEVEL-SNAPSHOT (MAP/ENTITIES/
EXPLORED) for that depth, persisting every level this player has ever
visited -- monsters, corpses, dropped loot, and fog-of-war included --
so returning to a previously-visited depth restores it exactly as it
was left, rather than needing a fresh GENERATE-DUNGEON call or losing
that level's state to any mutable hash-table. Kept as an FSET:MAP
rather than a standard CL hash table specifically so GAME-STATE's
functional-update discipline (see UPDATE-GAME-STATE) extends cleanly
to per-level data too: FSET:MAP is itself immutable/persistent, so
FSET:WITH returns a fresh map sharing structure with the old one
instead of mutating it in place, matching how every other GAME-STATE
slot is only ever replaced via a fresh instance, never mutated.
Populated for depth 1 by MAKE-INITIAL-STATE; entries for other levels
are added/refreshed as the player uses stairs (see USE-STAIRS).")
   (explored :initarg :explored :reader get-explored
             :initform (make-array (* *rdescent-field-width* *rdescent-field-height*)
                                   :element-type 'bit :initial-element 0))
   (flags :initarg :flags :reader get-flags :initform (fset:empty-map)
          :documentation "An immutable FSET:MAP from keyword to
arbitrary immutable value, holding whatever ad-hoc per-run data a
feature needs without requiring its own dedicated GAME-STATE slot
(ARCHITECTURE_PLAN.md §9) -- e.g. a future :COLLECTION-LOG (scavenger-
hunt item IDs collected), :KEYS-HELD (a set of key IDs),
:QUEST-LOG (an alist of quest-id . progress), :TURN-COUNTER/
:SPRINT-DEADLINE (a per-floor doom-clock integer), or
:GAME-OVER-REASON (a keyword distinct from the player's own IS-ALIVE,
consulted by GAME-ACTIVE-P/APPLY-RDESCENT-COMMAND/APPLY-PLAYER-
COMMAND/ADVANCE-GAME-STATE's shared game-over short-circuit -- see
those functions). Read via GAME-STATE-FLAG, written via
SET-GAME-STATE-FLAG (both RDESCENT/MECHANICS.LISP), which wrap
FSET:LOOKUP/FSET:WITH the same way GET-LEVELS/UPDATE-GAME-STATE
already do for per-depth dungeon snapshots -- kept as an FSET:MAP
rather than a plain CL hash table for exactly the same functional-
update-discipline reason GET-LEVELS's own docstring gives: FSET:WITH
returns a fresh map sharing structure with the old one rather than
mutating it in place, so a new flag can be introduced by any future
feature without ever touching GAME-STATE's own class definition or
UPDATE-GAME-STATE's signature again. Empty (no flags at all) for
every GAME-STATE MAKE-INITIAL-STATE produces today -- nothing yet
reads or writes any flag.")
   (message-log :initarg :message-log :reader get-message-log :initform nil))
  (:documentation "Server-authoritative game state for a single
connected player: PLAYER is that player's own ENTITY, ENTITIES is the
list of any other drawable entities sharing the field (monsters spawned
by SPAWN-MONSTERS-FOR-LEVEL), MAP is the (currently shared, immutable)
GAME-MAP layout for CURRENT-DEPTH, CURRENT-DEPTH is the dungeon depth
(1-indexed) the player currently occupies, LEVELS is an immutable
FSET:MAP persisting every GAME-MAP the player has visited by depth (see
its own slot docstring), and EXPLORED is a flat WIDTH*HEIGHT bit-vector
(indexed via XY-TO-INDEX) recording every tile this player has ever
had in sight -- 1 once seen, and never cleared back to 0, so
previously explored-but-currently-dark tiles can still be drawn dimly
(\"remembered\") rather than as blank shroud. FLAGS is an immutable
FSET:MAP of arbitrary per-run data (see its own slot docstring/
ARCHITECTURE_PLAN.md §9), empty until some future feature reads or
writes one. MESSAGE-LOG is a list of
human-readable strings (newest first) -- combat/flavor text such as
\"You kick the Orc...\" or an enemy's turn announcement -- capped to
its newest *RDESCENT-MESSAGE-LOG-MAX-LENGTH* entries by
UPDATE-GAME-STATE so a long-lived connection can't accumulate an
unbounded list, and read (uncapped, just the most recent handful) by
RDESCENT-OUTBOUND-PACKETS (see its docstring). Each RDESCENT-CLIENT
owns its own GAME-STATE instance (see RDESCENT-CLIENT-GAME-STATE) so
players never share or clobber each other's position, fog-of-war
progress, or message log. Like ENTITY/TILE/GAME-MAP, a GAME-STATE is
never mutated in place -- functions that need to change it (see
MOVE-PLAYER, APPLY-RDESCENT-COMMAND) return a fresh instance via
UPDATE-GAME-STATE instead. See MAKE-INITIAL-STATE for the pure factory
that builds the starting GAME-STATE for a newly connected client."))

(defun xy-to-index (x y &optional (width *rdescent-field-width*))
  "Return the flat bit-vector index for cell (X, Y) of a WIDTH-wide
grid: (+ X (* Y WIDTH)). Used to address GAME-STATE's flat EXPLORED
bit-vector and the fresh visibility bit-vectors returned by
COMPUTE-FOV. WIDTH defaults to *RDESCENT-FIELD-WIDTH*, the row width
every production GAME-STATE's EXPLORED mask is laid out with, but can
be overridden (e.g. by tests exercising a smaller playing field)."
  (+ x (* y width)))

(defun copy-instance (instance &rest overrides)
  "Return a fresh instance of INSTANCE's own class (MAKE-INSTANCE
(CLASS-OF INSTANCE) ...), with every one of INSTANCE's own class-slots
that (a) has at least one :INITARG and (b) is currently SLOT-BOUNDP on
INSTANCE copied over verbatim, then OVERRIDES (a flat plist of further
keyword/value pairs, exactly as if passed directly to MAKE-INSTANCE)
layered on top. CLHS 7.1.2's own GETF-style \"the first (leftmost)
initialization argument for a given name wins\" rule for INITIALIZE-
INSTANCE means an OVERRIDES entry for some initarg always beats the
copied default for that same initarg, no matter which order this
function's own internal APPEND puts them in.

Introspects INSTANCE's own class via SB-MOP:CLASS-SLOTS/SB-MOP:SLOT-
DEFINITION-NAME/SB-MOP:SLOT-DEFINITION-INITARGS (SBCL's own bundled
Meta-Object Protocol -- no external CLOSER-MOP dependency needed)
rather than a hand-maintained list of slot names, so this function --
unlike a hand-rolled per-class copier -- requires zero changes when a
class gains, loses, or renames a slot: whatever CLASS-SLOTS/SLOT-
BOUNDP report right now is exactly what gets copied, automatically,
for every present and future ENTITY subclass alike. This is the fix
for the exact bug UPDATE-ENTITY's own previous incarnation (a single
function with 30-odd named &KEY parameters plus a TYPEP-gated
CONS'd-on tail for every FIXTURE/GROUND-ITEM/COMPANION subclass'
own extra slots) was chronically prone to: forgetting to thread a
freshly added subclass slot through by hand meant it silently went
UNBOUND the very next ordinary REDUCE-TICK sweep touched that entity.
A slot with no :INITARG at all (there are none in this codebase's own
ENTITY hierarchy today, but the check is a harmless no-op guard
against ever adding one) is simply skipped, matching how MAKE-INSTANCE
could never have re-supplied it anyway. See UPDATE-ENTITY, ENTITY's
own functional-update idiom (MAKE-INSTANCE rather than mutating a
:READER-only slot in place), for the sole call site in this file."
  (let ((defaults '()))
    (dolist (slot (sb-mop:class-slots (class-of instance)))
      (let ((slot-name (sb-mop:slot-definition-name slot))
            (initarg (first (sb-mop:slot-definition-initargs slot))))
        (when (and initarg (slot-boundp instance slot-name))
          (setf defaults (list* initarg (slot-value instance slot-name) defaults)))))
    (apply #'make-instance (class-of instance) (append overrides defaults))))

(defun update-entity (ent &rest overrides
                       &key (confused-ticks nil confused-ticks-supplied-p)
                       &allow-other-keys)
  "Return a fresh instance of ENT's own class (ENTITY or any subclass
-- SHRINE-FIXTURE, GROUND-ITEM, COMPANION, PLAQUE-FIXTURE, ..., today
or in the future) with every one of ENT's own slots -- every plain
ENTITY slot (X/Y/CHAR/LEVEL/NAME/.../COLLECTION-LOG) *and* whatever
subclass-specific slots ENT's own concrete class happens to add
(PAYLOAD/SHRINE-KIND+USE-COUNT/BONDED+WANDER-*/HIDDEN-P/ITEM-ID/
PLAQUE-TEXT/...) -- copied from ENT except where overridden by the
corresponding keyword argument, exactly like a direct MAKE-INSTANCE
call. This is a thin wrapper around COPY-INSTANCE (see its own
docstring for the SB-MOP-based mechanism that makes this generic
across every present and future ENTITY subclass, with zero per-slot
maintenance here) that adds exactly one piece of ENTITY-specific
sugar: CONFUSED-TICKS.

CONFUSED-TICKS is a backward-compatible convenience keyword, not a
real slot (see ACTIVE-EFFECTS/ENTITY's own docstring, ARCHITECTURE_
PLAN.md §1): if supplied, it replaces (or, if <= 0, removes) ENT's
:CONFUSED entry within ACTIVE-EFFECTS (via REPLACE-EFFECT-IN-LIST),
layered on top of whatever ACTIVE-EFFECTS value this same call already
carries (an explicit :ACTIVE-EFFECTS override if the caller passed
one, or ENT's own current ACTIVE-EFFECTS otherwise) -- so existing
callers that only ever adjusted confusion via :CONFUSED-TICKS (e.g.
CONFUSED-ENTITY-TURN/PROCESS-ENEMY-TURNS) continue to work completely
unchanged, while a caller introducing a different KIND of effect (or
touching ACTIVE-EFFECTS directly) uses the generic :ACTIVE-EFFECTS
keyword instead (see ENTITY-WITH-EFFECT/APPLY-STATUS-EFFECT). Since
:CONFUSED-TICKS is not itself a real initarg for any ENTITY subclass,
it is stripped out of OVERRIDES before being forwarded to COPY-
INSTANCE/MAKE-INSTANCE -- passing it straight through would otherwise
signal an invalid-initargs error."
  (let ((overrides (alexandria:remove-from-plist overrides :confused-ticks)))
    (when confused-ticks-supplied-p
      (setf overrides
            (list* :active-effects
                   (replace-effect-in-list
                    (getf overrides :active-effects (get-active-effects ent))
                    :confused confused-ticks)
                   overrides)))
    (apply #'copy-instance ent overrides)))

(defun equipped-item (ent slot)
  "Return the EQUIPPABLE-ITEM currently occupying ENT's EQUIPMENT SLOT
(one of :WEAPON/:BODY/:HEAD/:OFF-HAND), or NIL if that slot is empty
(or ENT's EQUIPMENT plist has no entry for SLOT at all -- GETF's own
default). See ENTITY's EQUIPMENT slot/EQUIP-ITEM/UNEQUIP-ITEM."
  (getf (get-equipment ent) slot))

(defun equipment-with-slot (equipment slot item)
  "Return a fresh copy of EQUIPMENT (a plist, see ENTITY's EQUIPMENT
slot) with SLOT set to ITEM, replacing any existing entry for SLOT.
If ITEM is NIL, SLOT is instead omitted from the result entirely
(equivalent to empty, per GETF's own default, but keeps the plist from
accumulating dead NIL entries every time something is unequipped).
EQUIPMENT itself is never modified -- like every value type in this
file, this always returns a new plist. Used by EQUIP-ITEM/
UNEQUIP-ITEM (RDESCENT/ACTIONS.LISP)."
  (let ((without (loop for (s v) on equipment by #'cddr
                       unless (eq s slot)
                       append (list s v))))
    (if item (list* slot item without) without)))

(defun apply-equipment-wear (defender damage)
  "If DEFENDER (an ENTITY) took real (DAMAGE > 0) combat damage and has
at least one non-empty EQUIPMENT slot, pick one such slot uniformly at
random, reduce that item's own DURABILITY by
*RDESCENT-ITEM-DURABILITY-LOSS-PER-HIT* (via ITEM-WITH-DURABILITY,
floored at 0), and if that reaches 0, remove it from EQUIPMENT entirely
-- destroying it outright, even if it's :CURSED, since this edits the
EQUIPMENT plist directly rather than going through UNEQUIP-ITEM's own
cursed-item rejection (see UNEQUIP-ITEM's own docstring; that
restriction is about a *player* choosing to take an item off, not an
item that has simply broken).
Returns two values: DEFENDER unchanged (or with the worn/broken item's
slot updated in a fresh copy, per this file's usual \"never mutate\"
convention) and the broken item's own GET-ITEM-NAME if it was destroyed
this call, or NIL if nothing broke (including when DAMAGE is 0/DEFENDER
has no equipment at all, the common case for most monsters).
Called from RESOLVE-ATTACK, the single per-blow combat chokepoint, so
every real attack in the game (player attacking a monster, a monster or
trap attacking the player, the companion attacking or being attacked,
etc.) applies this uniformly."
  (let ((equipped-slots (loop for (slot item) on (get-equipment defender) by #'cddr
                               when item collect slot)))
    (if (or (<= damage 0) (null equipped-slots))
        (values defender nil)
        (let* ((slot (nth (random (length equipped-slots)) equipped-slots))
               (item (equipped-item defender slot))
               (new-durability (max 0 (- (get-durability item)
                                          *rdescent-item-durability-loss-per-hit*))))
          (if (<= new-durability 0)
              (values (update-entity defender
                                      :equipment (equipment-with-slot (get-equipment defender) slot nil))
                      (get-item-name item))
              (values (update-entity defender
                                      :equipment (equipment-with-slot (get-equipment defender) slot
                                                                       (item-with-durability item new-durability)))
                      nil))))))

(defun scale-stat-bonus-for-modifier (value modifier)
  "Scale VALUE (a raw STAT-BONUSES entry read off some equipped
EQUIPPABLE-ITEM) according to MODIFIER (:NORMAL/:CURSED/:BLESSED --
see ITEM-MODIFIER): a :CURSED item's own bonus is negated, turning a
would-be buff into a debuff (and a would-be debuff, e.g. Unwashed
Hoodie's own -3 :HYGIENE, into a bonus of the same magnitude -- exactly
backwards from what the item was going for, which is the whole point
of CURSED); a :BLESSED item's own bonus is enhanced by an extra 50% of
its own magnitude (rounded, same direction); any other MODIFIER (i.e.
:NORMAL) passes VALUE through unchanged. A stat the item's own
STAT-BONUSES plist doesn't even mention is already 0 by the time this
is called (see STAT-BONUS-TOTAL's own GETF default), and 0 scales to 0
either way, so this never conjures a bonus for a stat the item never
touched."
  (case modifier
    (:cursed (- value))
    (:blessed (+ value (round value 2)))
    (t value)))

(defun stat-bonus-total (ent stat)
  "Sum STAT (a keyword, e.g. :POWER/:DEFENSE/:MAX-HP -- one of
EQUIPPABLE-ITEM's own STAT-BONUSES plist keys) across every non-NIL
item currently in ENT's EQUIPMENT (see ENTITY's EQUIPMENT slot),
defaulting any item's own missing STAT entry to 0 (via GETF), and
scaling each item's own raw entry through SCALE-STAT-BONUS-FOR-MODIFIER
according to that item's own ITEM-MODIFIER (so a :CURSED item's bonus
comes out negated and a :BLESSED item's enhanced). Returns 0 for an ENT
with no EQUIPMENT at all (e.g. every monster today, and any player
before their first EQUIP-ITEM) -- the neutral value EFFECTIVE-POWER/
EFFECTIVE-DEFENSE/EFFECTIVE-MAX-HP fold this onto."
  (loop for (slot item) on (get-equipment ent) by #'cddr
        when item
        sum (scale-stat-bonus-for-modifier (getf (get-stat-bonuses item) stat 0)
                                            (item-modifier item))))

(defun effective-pivot (ent)
  "Return ENT's raw PIVOT plus the total :PIVOT bonus contributed by
equipped items, plus *RDESCENT-EXECUTIVE-HIGH-STAT-BONUS* while an
:EXECUTIVE-HIGH effect is active, minus *RDESCENT-COMEDOWN-STAT-
PENALTY* while a :COMEDOWN effect is active (§17's Baggie of Blow's
buff/crash pair). §13's Razor-Sharp Aluminum Mousepad and Laser
Pointer of Redirection both model their evasiveness/utility through
this existing dodge-linked stat rather than introducing a brand-new
critical-hit or forced-movement subsystem."
  (+ (get-pivot ent) (stat-bonus-total ent :pivot)
     (if (entity-effect ent :executive-high) *rdescent-executive-high-stat-bonus* 0)
     (if (entity-effect ent :comedown) (- *rdescent-comedown-stat-penalty*) 0)))

(defun effective-caffeine-tolerance (ent)
  "Return ENT's raw CAFFEINE-TOLERANCE plus the total
:CAFFEINE-TOLERANCE bonus from equipped items. DRINK-POTION consults
this instead of GET-CAFFEINE-TOLERANCE directly so §13's Branded
Corporate Yeti Mug can plug into the already-wired kombucha-healing
formula even though no passive drain-rate subsystem exists yet."
  (+ (get-caffeine-tolerance ent) (stat-bonus-total ent :caffeine-tolerance)))

(defun effective-domain-knowledge (ent)
  "Return ENT's raw DOMAIN-KNOWLEDGE plus the total
:DOMAIN-KNOWLEDGE bonus from equipped items, plus *RDESCENT-ADDERALL-
FOCUS-DOMAIN-KNOWLEDGE-BONUS* while an :ADDERALL-FOCUS effect is
active, minus *RDESCENT-BUZZED-DOMAIN-KNOWLEDGE-PENALTY* while a
:BUZZED effect is active (§17's Discarded Adderall / TGIF Leftover
Beer). EFFECTIVE-FOV-RADIUS and the existing psychic-damage
consumables (Scroll of PIP / Reply-All Bomb) both consult this so
§13's Blue-Light Blocking Glasses affect the same stat-driven seams
the base attribute already powers."
  (+ (get-domain-knowledge ent) (stat-bonus-total ent :domain-knowledge)
     (if (entity-effect ent :adderall-focus) *rdescent-adderall-focus-domain-knowledge-bonus* 0)
     (if (entity-effect ent :buzzed) (- *rdescent-buzzed-domain-knowledge-penalty*) 0)))

(defun effective-seniority (ent)
  "Return ENT's raw SENIORITY plus the total :SENIORITY bonus from
equipped items. APPLY-STATUS-EFFECT and hidden-trap detection both
consult this so a future or present accessory can influence the
already-existing deflection/detection formulas without a special-case
branch at each call site."
  (+ (get-seniority ent) (stat-bonus-total ent :seniority)))

(defun effective-synergy (ent)
  "Return ENT's raw SYNERGY plus the total :SYNERGY bonus from
equipped items, plus *RDESCENT-EXECUTIVE-HIGH-STAT-BONUS* while an
:EXECUTIVE-HIGH effect is active, minus *RDESCENT-COMEDOWN-STAT-
PENALTY* while a :COMEDOWN effect is active (§17's Baggie of Blow's
buff/crash pair -- see EFFECTIVE-PIVOT, its exact PIVOT-flavored
counterpart). Vendor pricing and fresh-depth pacify rolls consult this
instead of GET-SYNERGY directly so §13's Patagonia Fleece Vest and
Agile Scrum Master Certificate hook into already-existing SYNERGY
mechanics rather than inventing new passive systems."
  (+ (get-synergy ent) (stat-bonus-total ent :synergy)
     (if (entity-effect ent :executive-high) *rdescent-executive-high-stat-bonus* 0)
     (if (entity-effect ent :comedown) (- *rdescent-comedown-stat-penalty*) 0)))

(defun effective-hygiene (ent)
  "Return ENT's raw HYGIENE plus the total :HYGIENE bonus from
equipped items. Fresh-depth spawn-time disposition consults this so
§13's Unwashed Hoodie can exert a real, persistent downside through
the already-existing faction-hostility rule."
  (+ (get-hygiene ent) (stat-bonus-total ent :hygiene)))

(defun effective-fov-radius (ent)
  "Return the field-of-view radius COMPUTE-FOV should use for ENT:
DOMAIN-KNOWLEDGE-FOV-RADIUS of EFFECTIVE-DOMAIN-KNOWLEDGE, plus the
flat *RDESCENT-BLUE-LIGHT-BLOCKING-GLASSES-FOV-BONUS* while The
Blue-Light Blocking Glasses are equipped, all doubled while a
:MICRODOSING effect is active (§17's Microdose Tab). This keeps §13's
flat +1 FOV bonus layered additively before the multiplicative
doubling is applied last, mirroring §16's own separate Peripheral
Vision dodge bonus style."
  (let ((base (+ (domain-knowledge-fov-radius (effective-domain-knowledge ent))
                 (if (blue-light-blocking-glasses-active-p ent)
                     *rdescent-blue-light-blocking-glasses-fov-bonus*
                     0))))
    (if (entity-effect ent :microdosing) (* base 2) base)))

(defun effective-power (ent)
  "Return ENT's POWER plus the total :POWER STAT-BONUS-TOTAL
contributed by everything currently in its EQUIPMENT (ARCHITECTURE_
PLAN.md §4), plus *RDESCENT-THE-ZONE-POWER-BONUS* while a :THE-ZONE
effect is active (§17's Dexedrine Spansule, standing in for the plan
text's \"guaranteed critical hits\" -- see *RDESCENT-THE-ZONE-SECONDS*'s
own docstring) -- the value every *combat* call site (RESOLVE-ATTACK,
and through it MOVE-PLAYER's melee branch/PROCESS-ENEMY-TURNS'
attacking branch) should read instead of the raw POWER reader, so
equipped gear is purely additive and recomputed fresh on every read
rather than ever being baked into POWER itself. Identical to (POWER
ENT) for any ENT with no EQUIPMENT and no :THE-ZONE effect (every
monster today, and any player before their first EQUIP-ITEM)."
  (+ (power ent) (stat-bonus-total ent :power)
     (if (entity-effect ent :the-zone) *rdescent-the-zone-power-bonus* 0)))

(defun effective-defense (ent)
  "Return ENT's DEFENSE plus the total :DEFENSE STAT-BONUS-TOTAL
contributed by everything currently in its EQUIPMENT, plus
*RDESCENT-HARDWARE-EMULATION-DEFENSE-BONUS* if ENT's own Relics of
Ancient Memory collectible set is complete (see
HARDWARE-EMULATION-ACTIVE-P, FUTURE_PLANS.md §16) -- see
EFFECTIVE-POWER, its exact DEFENSE-flavored counterpart. Returns a
flat 0, ignoring all of the above, while ENT has an active
:ARMOR-STRIPPED STATUS-EFFECT (The B0FH's LART's on-hit rider,
FUTURE_PLANS.md §15) -- \"loses all armor\" is modeled as an outright
override rather than a subtraction, so it applies just as absolutely
to a bare-defense monster as to one stacked with equipment bonuses."
  (if (entity-effect ent :armor-stripped)
      0
      (+ (defense ent) (stat-bonus-total ent :defense)
         (if (hardware-emulation-active-p ent) *rdescent-hardware-emulation-defense-bonus* 0))))

(defun effective-max-hp (ent)
  "Return ENT's MAX-HP plus the total :MAX-HP STAT-BONUS-TOTAL
contributed by everything currently in its EQUIPMENT -- see
EFFECTIVE-POWER/EFFECTIVE-DEFENSE, its exact MAX-HP-flavored
counterpart. Returns NIL, rather than signalling, if ENT's own MAX-HP
is NIL (ENTITY's own :INITFORM default for any entity constructed
without an explicit :MAX-HP, e.g. a bare test fixture) -- exactly
mirroring MAX-HP's own possible NIL value, so this is a safe drop-in
replacement anywhere MAX-HP is read as a heal/regen cap (see
DRINK-POTION/INTERACT-SHRINE)."
  (and (max-hp ent) (+ (max-hp ent) (stat-bonus-total ent :max-hp))))

(defun effective-weapon (ent)
  "Return the EQUIPPABLE-ITEM currently occupying ENT's :WEAPON
EQUIPMENT slot, or NIL if unarmed -- see WEAPON-REACH/WEAPON-HITS-
PER-TURN/WEAPON-ON-HIT-EFFECT, which each treat a NIL weapon as the
neutral \"today's fixed melee-range-1, one-hit, no-on-hit-effect\"
default (ARCHITECTURE_PLAN.md §5)."
  (equipped-item ent :weapon))

(defun weapon-reach (weapon)
  "Return WEAPON's own GET-WEAPON-REACH, or 1 (today's fixed melee
range) if WEAPON is NIL (unarmed -- see EFFECTIVE-WEAPON). Consulted
by PROCESS-ENEMY-TURNS' attack-range gate in place of the earlier
hardcoded (<= DISTANCE 1) check, so a future reach-2+ weapon/monster
falls out of the same check without a new special case."
  (if weapon (get-weapon-reach weapon) 1))

(defun weapon-hits-per-turn (weapon)
  "Return WEAPON's own GET-WEAPON-HITS-PER-TURN, or 1 if WEAPON is NIL
(unarmed -- see EFFECTIVE-WEAPON). RESOLVE-ATTACK-VOLLEY (RDESCENT/
COMMANDS.LISP) consults this to repeat a single attack action's hit
resolution that many times, aggregating the damage into one logical
attack-turn while still re-rolling dodge/on-hit logic per blow, so
rapid-fire weapons like §13's Rubber Band Gatling Gun can reuse the
existing combat math rather than needing a parallel ranged-weapon
subsystem."
  (if weapon (get-weapon-hits-per-turn weapon) 1))

(defun weapon-on-hit-effect (weapon)
  "Return WEAPON's own GET-ON-HIT-EFFECT (an (:KIND ... :TURNS ...
[:MAGNITUDE ...]) plist), or NIL if WEAPON is NIL (unarmed) or WEAPON
simply has no ON-HIT-EFFECT of its own -- see EFFECTIVE-WEAPON/
RESOLVE-ATTACK, which feeds a non-NIL result straight into
APPLY-STATUS-EFFECT on a successful, non-lethal hit."
  (and weapon (get-on-hit-effect weapon)))

(defun effective-attack-energy-cost (ent)
  "Return the ENERGY cost of one attack action for ENT. Ordinarily this
is just *RDESCENT-ATTACK-ENERGY-COST*, but an active :CARPAL-TUNNEL or
:COMEDOWN effect (§17's Baggie of Blow crash) each double it,
approximating \"slows enemy attack speed\"/\"halves movement speed\"
through the existing energy scheduler rather than inventing
per-entity attack-cooldown state or mutating a weapon's own HITS-
PER-TURN in place; a :CAFFEINATED effect (§17's Quadruple Shot
Espresso) instead halves it (rounded up) for the opposite \"jittery,
fast\" effect. Both a :CARPAL-TUNNEL/:COMEDOWN doubling and a
:CAFFEINATED halving stack independently (each is its own
multiplicative factor) rather than one suppressing the other, since
nothing in §17's own text suggests they are mutually exclusive."
  (* *rdescent-attack-energy-cost*
     (if (or (entity-effect ent :carpal-tunnel) (entity-effect ent :comedown)) 2 1)
     (if (entity-effect ent :caffeinated) 1/2 1)))

(defun replace-effect-in-list (effects kind ticks-remaining &optional magnitude expire-into)
  "Return a fresh list of STATUS-EFFECT instances: EFFECTS with any
existing KIND entry removed, and a new STATUS-EFFECT of KIND/
TICKS-REMAINING/MAGNITUDE/EXPIRE-INTO consed on -- unless
TICKS-REMAINING is not positive, in which case KIND is simply absent
from the result entirely (fully removing the effect once its countdown
reaches 0, or refusing to attach one that never had any turns to begin
with). EFFECTS itself is never modified -- like every value type in
this file, this always returns a new list."
  (let ((others (remove kind effects :key #'status-effect-kind)))
    (if (plusp ticks-remaining)
        (cons (make-instance 'status-effect :kind kind :ticks-remaining ticks-remaining
                                            :magnitude magnitude :expire-into expire-into)
              others)
        others)))

(defun entity-effect (ent kind)
  "Return ENT's STATUS-EFFECT instance of KIND (see ENTITY's
ACTIVE-EFFECTS slot), or NIL if ENT has no such effect currently
attached. The one generic entry point for \"is ENT affected by KIND\"
-- e.g. (ENTITY-EFFECT ENT :CONFUSED) replaces a direct slot read
against an earlier bespoke CONFUSED-TICKS slot (see
ARCHITECTURE_PLAN.md §1)."
  (find kind (get-active-effects ent) :key #'status-effect-kind))

(defun effective-dodge-chance (ent)
  "Return ENT's own PIVOT-DODGE-CHANCE (folding in
*RDESCENT-PERIPHERAL-VISION-PIVOT-BONUS* to the underlying PIVOT stat
first if ENT's own Cursed Peripherals of Yesteryear collectible set is
complete, see PERIPHERAL-VISION-ACTIVE-P/FUTURE_PLANS.md §16), reduced
by *RDESCENT-ANALYSIS-PARALYSIS-DODGE-PENALTY* (floored at 0) while
ENT has an active :ANALYSIS-PARALYSIS STATUS-EFFECT (see
ENTITY-EFFECT), then further increased by a flat
*RDESCENT-PERIPHERAL-VISION-DODGE-BONUS* under that same Peripheral
Vision set-bonus -- the seam RESOLVE-ATTACK's dodge roll consults
(RDESCENT/COMMANDS.LISP) instead of calling PIVOT-DODGE-CHANCE
directly, so a flavor-specific debuff (FUTURE_PLANS.md §7, \"Varied
Attack Effects\") can affect combat outcomes without RESOLVE-ATTACK
itself needing to know anything about where the debuff came from."
  (+ (max 0 (- (pivot-dodge-chance (+ (effective-pivot ent)
                                       (if (peripheral-vision-active-p ent)
                                           *rdescent-peripheral-vision-pivot-bonus*
                                           0)))
               (if (entity-effect ent :analysis-paralysis) *rdescent-analysis-paralysis-dodge-penalty* 0)))
     (if (peripheral-vision-active-p ent) *rdescent-peripheral-vision-dodge-bonus* 0)))

(defun entity-confused-ticks (ent)
  "Return the number of turns remaining in ENT's :CONFUSED STATUS-
EFFECT (see ENTITY-EFFECT), or 0 if ENT is not currently confused.
Convenience wrapper kept for readability at every existing call site
that only ever cared about confusion specifically (CONFUSED-ENTITY-
TURN/PROCESS-ENEMY-TURNS/CAST-REORG-MEMO) -- the underlying storage is
the generic ACTIVE-EFFECTS list, not a dedicated slot."
  (let ((effect (entity-effect ent :confused)))
    (if effect (status-effect-ticks-remaining effect) 0)))

(defun entity-with-effect (ent kind ticks-remaining &optional magnitude expire-into)
  "Return a fresh copy of ENT (via UPDATE-ENTITY) with its KIND
STATUS-EFFECT replaced (or removed, if TICKS-REMAINING <= 0) --
see REPLACE-EFFECT-IN-LIST. Unlike APPLY-STATUS-EFFECT, this
unconditionally attaches the effect with no SENIORITY-DEFLECTION-
CHANCE roll -- callers that need Deflection Chance applied (i.e. any
caller actually *inflicting* a new effect on a target, as opposed to
just ticking/removing an existing one) should call APPLY-STATUS-EFFECT
instead."
  (update-entity ent :active-effects (replace-effect-in-list (get-active-effects ent) kind ticks-remaining magnitude expire-into)))

(defun apply-status-effect (ent kind ticks-remaining &optional magnitude expire-into)
  "Attempt to inflict a KIND status effect (TICKS-REMAINING/MAGNITUDE/
EXPIRE-INTO, see STATUS-EFFECT) on ENT. This is the single entry point
every future debuff-inflicting attack/item/trap should call (see
ARCHITECTURE_PLAN.md §1) -- SENIORITY-DEFLECTION-CHANCE is rolled
against ENT's own SENIORITY first, so Deflection Chance is
automatically and uniformly correct for every effect rather than
needing to be re-derived per caller. Returns two values NEW-ENT
DEFLECTED-P: if (RANDOM 100) rolls under ENT's own SENIORITY-
DEFLECTION-CHANCE, OR if KIND is :CONFUSED and ENT's own Outdated
Buzzword Bingo Chips collectible set is complete (see
BUZZWORD-IMMUNITY-ACTIVE-P, FUTURE_PLANS.md §16 -- an unconditional
deflection, regardless of source, not merely a probabilistic one like
ordinary Deflection Chance), OR if KIND is :CONFUSED or :STUNNED and
ENT has an active :MODAFINIL-IMMUNITY STATUS-EFFECT (§17's Modafinil,
\"total immunity to sleep/stun mechanics ... You do not blink. You do
not yawn.\") or has The Noise-Canceling AirPods Pro equipped (see
AIRPODS-PRO-ACTIVE-P, FUTURE_PLANS.md §15 -- another unconditional
deflection, permanent for as long as the item stays equipped rather
than a temporary buff), the effect is shrugged off entirely --
NEW-ENT is ENT unchanged and DEFLECTED-P is T; otherwise NEW-ENT has
the KIND effect attached via ENTITY-WITH-EFFECT and DEFLECTED-P is
NIL. Every monster, and any player predating a future SENIORITY-
granting feature, has the default SENIORITY of 0 (a 0% Deflection
Chance), so this is a pure no-op behavior change today for every
existing caller (e.g. CAST-REORG-MEMO) -- it's the wiring, not yet a
meaningfully consulted stat, until some future SENIORITY-granting
feature exists."
  (if (or (< (random 100) (seniority-deflection-chance (effective-seniority ent)))
          (and (eq kind :confused)
               (or (buzzword-immunity-active-p ent)
                   (noise-canceling-headphones-active-p ent)))
          (and (member kind '(:confused :stunned))
               (or (entity-effect ent :modafinil-immunity)
                   (airpods-pro-active-p ent))))
      (values ent t)
      (values (entity-with-effect ent kind ticks-remaining magnitude expire-into) nil)))

(defun entity-disposition-toward (ent target)
  "Return ENT's disposition toward TARGET -- one of :HOSTILE,
:NEUTRAL, :FRIENDLY, or :FLEEING -- consulted by PROCESS-ENEMY-TURNS
before choosing an AI branch for ENT's turn (see ARCHITECTURE_PLAN.md
§2). This normally just returns ENT's own (already-finalized)
DISPOSITION slot, ignoring TARGET entirely -- set once at spawn time
by its factory (see MAKE-ORC/MAKE-TROLL and ENEMY's own :HOSTILE
default), then possibly further adjusted by SPAWN-MONSTERS-FOR-LEVEL's
HYGIENE-BANDED-DISPOSITION/SYNERGY-PACIFY-CHANCE roll before the
monster is ever added to GAME-STATE's ENTITIES (see FUTURE_PLANS.md
§1) -- it exists as the single seam a future target-specific rule
(e.g. faction-vs-faction turf war) plugs into later, without requiring
any change to PROCESS-ENEMY-TURNS' own call site -- only this
function's body would need to grow a TARGET-dependent branch.
FUTURE_PLANS.md §15 is the first such TARGET-dependent rule: an
otherwise :HOSTILE ENT is overridden --
  - to :NEUTRAL if TARGET has an active :OUT-OF-OFFICE STATUS-EFFECT
    (The \"Out of Office\" Auto-Responder, MAYBE-TRIGGER-OUT-OF-
    OFFICE), or
  - to :FLEEING if TARGET has The C-Suite Keycard equipped (see
    C-SUITE-KEYCARD-ACTIVE-P) *and* ENT's own GET-XP is at or below
    *RDESCENT-C-SUITE-KEYCARD-MAX-XP*
-- checked in that order, so a target simultaneously invisible (Out of
Office) and holding the Keycard just reads as :NEUTRAL (no need to
flee from someone you can't even perceive)."
  (let ((disposition (get-disposition ent)))
    (cond
      ((not (eq disposition :hostile)) disposition)
      ((entity-effect target :out-of-office) :neutral)
      ((and (c-suite-keycard-active-p target)
            (<= (get-xp ent) *rdescent-c-suite-keycard-max-xp*))
       :fleeing)
      (t disposition))))

