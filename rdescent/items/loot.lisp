;;; -*- Lisp -*-

;;; FUTURE_PLANS.md §15's Rare & Legendary Loot: ten unique, game-
;;; altering EQUIPPABLE-ITEM drops (5 Rare, 5 Legendary), each defined
;;; via RDESCENT/ITEMS/EQUIPMENT.LISP's own DEFINE-ARMORY-EQUIPPABLE-
;;; ITEM macro exactly like that file's own ordinary armory items.
;;; Loaded after RDESCENT/ITEMS/EQUIPMENT.LISP, whose macro this file
;;; depends on.

(in-package "JRM-CODE-PROJECT")

;;; Rare & Legendary Loot (FUTURE_PLANS.md §15)
;;;
;;; Ten unique, game-altering drops (5 Rare, 5 Legendary), each mapped
;;; onto EQUIPPABLE-ITEM's existing four-slot system (:WEAPON/:BODY/
;;; :HEAD/:OFF-HAND) -- exactly like §13's own amulet/accessory items
;;; before them (see HEAD-SLOT-ITEM-ACTIVE-P's own docstring), the
;;; plan text's Accessory/Headgear/Amulet/Off-hand/Two-Handed-Tome/
;;; Weapon-Shield flavor slots are folded into whichever of the four
;;; real slots fits best rather than inventing new equipment
;;; primitives. Several items needed a new, narrow STATUS-EFFECT kind
;;; or a new seam in an existing function (documented at each site);
;;; a few mechanics from the plan text have no existing primitive to
;;; build on at all and are deliberately simplified, exactly like every
;;; prior section's own "no elemental damage typing"/"no ammo system"
;;; simplifications:
;;;   - The Golden Parachute's "teleported to the next floor's
;;;     stairwell" reuses MAYBE-TRIGGER-YUBIKEY-SAVE's own existing
;;;     RANDOM-SAFE-PLAYER-TELEPORT-DESTINATION primitive (a random
;;;     safe tile on the *current* floor) rather than threading TIER/
;;;     MAX-DEPTH through the death-save call chain to actually
;;;     advance a dungeon depth.
;;;   - The Source Code of the Universe's faction-flip "shoot a
;;;     hostile Code Monkey and permanently turn it into a friendly
;;;     pet that fights alongside you" flips DISPOSITION/FACTION to
;;;     :FRIENDLY/:COMPANION (so it stops attacking, exactly like an
;;;     NPC-FIXTURE), but does not grant it the bespoke COMPANION
;;;     class' own active-combat AI -- that AI is reserved for the
;;;     Office Doge (§22) alone; a converted monster becomes a
;;;     permanently passive ally, not an active one.
;;;   - The B0FH's LART's "silenced" rider has no distinct mechanic to
;;;     attach (this engine has no spellcasting/verbal-ability system
;;;     for a silence to suppress) and is omitted; only its "loses all
;;;     armor" rider is modeled, via a new :ARMOR-STRIPPED STATUS-
;;;     EFFECT EFFECTIVE-DEFENSE consults.
;;;   - The Noise-Canceling AirPods Pro's "no longer wakes sleeping
;;;     enemies" rider is omitted -- monsters have no sleeping/waking
;;;     state in this engine at all.
;;;   - The C-Suite Keycard's "all Middle-Manager-tier enemies and
;;;     below flee" is keyed off GET-XP (this engine's existing
;;;     per-monster difficulty proxy) rather than a monster class
;;;     TYPEP check, both because ENTITIES.LISP is compiled before any
;;;     concrete monster class exists (RDESCENT/ENEMIES/*.LISP load
;;;     after it) and because XP is already the codebase's own
;;;     established "how tough is this monster" scale (see
;;;     MONSTER-RSU-DROP-AMOUNT/MAYBE-DROP-MONSTER-ITEM); today, with
;;;     only Orc/Middle-Manager/Troll in the bestiary (§2's full
;;;     roster not yet built), this affects every monster at or below
;;;     the Middle Manager's own XP.
;;;   - Uniqueness ("never two copies in the same GAME-STATE") is
;;;     enforced only at ground-spawn time (see FILTER-OUT-OWNED-
;;;     UNIQUE-ITEMS, RDESCENT/DUNGEON.LISP), not via monster-kill
;;;     drops -- none of these ten items are added to the ordinary
;;;     monster item-drop table, only to *RDESCENT-ITEM-SPAWN-TABLE*'s
;;;     own low-weight, deep-only ground finds (plus the Golden
;;;     Parachute's own severance-package payout, drawn from the
;;;     *ordinary* armory drop table, deliberately excluded from this
;;;     uniqueness check since those are common gear, not the unique
;;;     items themselves).

(defparameter *rdescent-out-of-office-seconds* 5.0
  "Real-world duration of The Out-of-Office Auto-Responder's
:OUT-OF-OFFICE invisibility/aggro-drop effect once triggered.")

(defparameter *rdescent-out-of-office-ticks*
  (round (/ *rdescent-out-of-office-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :OUT-OF-OFFICE -- *RDESCENT-OUT-OF-OFFICE-
SECONDS* converted to a tick count.")

(defparameter *rdescent-out-of-office-hp-threshold-percent* 10
  "HP percentage (of EFFECTIVE-MAX-HP) at or below which MAYBE-
TRIGGER-OUT-OF-OFFICE (RDESCENT/ACTIONS.LISP) arms The Out-of-Office
Auto-Responder, if equipped and not already used this floor.")

(defparameter *rdescent-sev1-incident-seconds* 3.0
  "Real-world duration of The Pager of Dread's on-hit :SEV1-INCIDENT
damage-over-time effect -- mirrors §13's :BLEED (*RDESCENT-BLEED-
SECONDS*), just a punchier name for a legendary weapon's own DoT.")

(defparameter *rdescent-sev1-incident-ticks*
  (round (/ *rdescent-sev1-incident-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :SEV1-INCIDENT -- *RDESCENT-SEV1-INCIDENT-
SECONDS* converted to a tick count.")

(defparameter *rdescent-sev1-incident-damage-per-tick* -1
  "Per-tick HP delta attached as :SEV1-INCIDENT's STATUS-EFFECT
MAGNITUDE, mirroring §13's :BLEED (*RDESCENT-BLEED-DAMAGE-PER-TICK*).")

(defparameter *rdescent-armor-stripped-seconds* 8.0
  "Real-world duration of The B0FH's LART's on-hit :ARMOR-STRIPPED
debuff -- see EFFECTIVE-DEFENSE, which returns a flat 0 (ignoring both
raw DEFENSE and any equipped gear's own :DEFENSE STAT-BONUSES) for as
long as this effect remains attached.")

(defparameter *rdescent-armor-stripped-ticks*
  (round (/ *rdescent-armor-stripped-seconds* *rdescent-tick-seconds*))
  "Engine-tick duration of :ARMOR-STRIPPED -- *RDESCENT-ARMOR-
STRIPPED-SECONDS* converted to a tick count.")

(defparameter *rdescent-lart-power-bonus* 10
  "Flat +POWER STAT-BONUSES entry for The B0FH's LART -- a heavily
reinforced length of CAT-5 cable, this engine's single hardest-hitting
melee weapon.")

(defparameter *rdescent-c-suite-keycard-max-xp* 18
  "The maximum GET-XP a monster can have and still be made to flee by
The C-Suite Keycard (see ENTITY-DISPOSITION-TORWARD) -- set to the
Middle Manager's own fixed :XP (RDESCENT/ENEMIES/MIDDLE-MANAGER.LISP),
so \"Middle-Manager-tier enemies and below\" is expressed as an XP
ceiling rather than a monster-class TYPEP check (see this section's
own preamble comment for why).")

(defparameter *rdescent-mechanical-keyboard-of-the-ancients-power-bonus* 8
  "Flat +POWER STAT-BONUSES entry for The Mechanical Keyboard of the
Ancients (IBM Model M).")

(defparameter *rdescent-mechanical-keyboard-of-the-ancients-defense-bonus* 5
  "Flat +DEFENSE STAT-BONUSES entry for The Mechanical Keyboard of the
Ancients (IBM Model M) -- its own \"acts as both a heavy blunt weapon
and a shield\" rider, folded into ordinary STAT-BONUSES rather than
occupying a second :OFF-HAND slot simultaneously (this engine's
equipment system has no dual-slot-occupancy primitive).")

(defparameter *rdescent-click-clack-stun-radius* 3
  "Chebyshev-distance radius (in tiles) MAYBE-CLICK-CLACK-STUN
(RDESCENT/ACTIONS.LISP) applies its :STUNNED effect within, around the
player's own new position after each move, while The Mechanical
Keyboard of the Ancients is equipped in the :WEAPON slot.")

(defparameter *rdescent-click-clack-stun-ticks* *rdescent-stunned-ticks*
  "Engine-tick duration of the :STUNNED effect MAYBE-CLICK-CLACK-STUN
applies -- identical to §13's own *RDESCENT-STUNNED-TICKS* (The Can of
Compressed Air), since this is the same STATUS-EFFECT kind, just
inflicted passively by footfall instead of by a direct hit.")

(defun equipped-slot-item-active-p (ent slot item-class)
  "T if ENT's own SLOT equipment slot currently holds an instance of
ITEM-CLASS, NIL otherwise -- the general form of HEAD-SLOT-ITEM-
ACTIVE-P (its own docstring explains why §13's neck/amulet items share
the :HEAD slot with literal headgear), generalized to any of the four
equipment slots so §15's own Off-Hand/Body/Weapon-slot rare items
don't need a bespoke per-slot predicate each."
  (typep (equipped-item ent slot) item-class))

(defun out-of-office-auto-responder-active-p (ent)
  "T if ENT currently has The Out-of-Office Auto-Responder equipped in
its :OFF-HAND slot. MAYBE-TRIGGER-OUT-OF-OFFICE (RDESCENT/
ACTIONS.LISP) consults this before arming the effect."
  (equipped-slot-item-active-p ent :off-hand 'out-of-office-auto-responder))

(defun airpods-pro-active-p (ent)
  "T if ENT currently has The Noise-Canceling AirPods Pro equipped in
its :HEAD slot -- APPLY-STATUS-EFFECT consults this to deflect both
:CONFUSED and :STUNNED infliction attempts, exactly like an active
:MODAFINIL-IMMUNITY STATUS-EFFECT (§17), just via permanent equipment
instead of a temporary buff."
  (equipped-slot-item-active-p ent :head 'airpods-pro-noise-canceling))

(defun platinum-corporate-amex-active-p (ent)
  "T if ENT currently has The Platinum Corporate Amex equipped in its
:OFF-HAND slot. VENDOR-ITEM-PRICE consults this (via its own FREE-P
parameter, threaded through from PURCHASE-ITEM/VENDOR-STOCK-LISTING-
TEXT) to make every vending-machine purchase cost 0 RSU."
  (equipped-slot-item-active-p ent :off-hand 'platinum-corporate-amex))

(defun c-suite-keycard-active-p (ent)
  "T if ENT currently has The C-Suite Keycard equipped in its
:OFF-HAND slot. ENTITY-DISPOSITION-TOWARD consults this to force any
sufficiently low-XP (see *RDESCENT-C-SUITE-KEYCARD-MAX-XP*) hostile
monster's disposition to :FLEEING instead of its own stored value."
  (equipped-slot-item-active-p ent :off-hand 'c-suite-keycard))

(defun golden-parachute-active-p (ent)
  "T if ENT currently has The Golden Parachute equipped in its :BODY
slot. MAYBE-TRIGGER-GOLDEN-PARACHUTE-SAVE (RDESCENT/ACTIONS.LISP)
consults this before consuming its once-ever death save."
  (equipped-slot-item-active-p ent :body 'golden-parachute))

(defun mechanical-keyboard-of-the-ancients-active-p (ent)
  "T if ENT currently has The Mechanical Keyboard of the Ancients (IBM
Model M) equipped in its :WEAPON slot. MAYBE-CLICK-CLACK-STUN
(RDESCENT/ACTIONS.LISP) consults this before stunning every hostile
entity within *RDESCENT-CLICK-CLACK-STUN-RADIUS* of the player's own
new position after each move."
  (equipped-slot-item-active-p ent :weapon 'mechanical-keyboard-of-the-ancients))

(define-armory-equippable-item out-of-office-auto-responder
  "The \"Out of Office\" Auto-Responder"
  :off-hand
  nil
  "Rare-tier accessory (folded into the :OFF-HAND slot -- see this
section's own preamble). Grants no passive stat bonus; its real
mechanic (become invisible to every hostile and drop their aggro for
*RDESCENT-OUT-OF-OFFICE-TICKS* once HP falls to or below
*RDESCENT-OUT-OF-OFFICE-HP-THRESHOLD-PERCENT* of EFFECTIVE-MAX-HP, at
most once per floor) is wired in RDESCENT/ACTIONS.LISP's own
MAYBE-TRIGGER-OUT-OF-OFFICE, a new MOVE-PLAYER post-processing pass
alongside MAYBE-REVEAL-HIDDEN-ENTITIES/MAYBE-AUTO-PICKUP-COLLECTIBLE."
  )

(define-armory-equippable-item airpods-pro-noise-canceling
  "The Noise-Canceling AirPods Pro"
  :head
  nil
  "Rare-tier headgear granting total immunity to :CONFUSED and
:STUNNED infliction (APPLY-STATUS-EFFECT consults AIRPODS-PRO-
ACTIVE-P), covering both of §6's sonic/psychic attack flavors
(Sea-Lioning/Buzzwords). Distinct from §13's own Headphones of
Noise-Canceling (:CONFUSED-only) rather than replacing it -- a player
can only ever wear one :HEAD-slot item at a time regardless. The plan
text's \"no longer wakes sleeping enemies\" rider is omitted -- see
this section's own preamble."
  )

(define-armory-equippable-item platinum-corporate-amex
  "The Platinum Corporate Amex"
  :off-hand
  nil
  "Rare-tier off-hand accessory. Grants no passive stat bonus; its
real mechanic (every VENDOR-FIXTURE purchase costs 0 RSU) is wired via
PLATINUM-CORPORATE-AMEX-ACTIVE-P, consulted by VENDOR-ITEM-PRICE's own
FREE-P parameter at both PURCHASE-ITEM and VENDOR-STOCK-LISTING-TEXT."
  )

(define-armory-equippable-item pager-of-dread
  "The Pager of Dread"
  :weapon
  nil
  "Rare-tier ranged weapon/wand: real reach 5 (mirroring the Rubber
Band Gatling Gun's own reach-4 precedent -- no projectile/line-of-
sight system exists, so \"ranged\" simply means a larger WEAPON-REACH)
with an on-hit :SEV1-INCIDENT damage-over-time effect (\"massive
psychic damage as they scramble to fix a non-existent server
outage\") -- mirroring §13's own :BLEED mechanism exactly."
  :weapon-reach 5
  :on-hit-effect (list :kind :sev1-incident :turns *rdescent-sev1-incident-ticks*
                       :magnitude *rdescent-sev1-incident-damage-per-tick*))

(define-armory-equippable-item b0fhs-lart
  "The B0FH's LART (Luser Attitude Readjustment Tool)"
  :weapon
  (list :power *rdescent-lart-power-bonus*)
  "Legendary melee weapon: a heavily reinforced length of CAT-5 cable
granting this engine's single largest +POWER STAT-BONUSES entry, with
an on-hit :ARMOR-STRIPPED effect (EFFECTIVE-DEFENSE returns a flat 0
while it's attached to the defender -- \"loses all armor for the rest
of the fight\"). The plan text's \"silenced\" rider is omitted -- see
this section's own preamble."
  :on-hit-effect (list :kind :armor-stripped :turns *rdescent-armor-stripped-ticks*))

(define-armory-equippable-item source-code-of-the-universe
  "The Source Code of the Universe"
  :weapon
  nil
  "Legendary two-handed tome (folded into the ordinary single :WEAPON
slot -- this engine's equipment system has no dual-slot-locking
primitive, matching §13's own \"two-handed slot-locking ... omitted\"
precedent) whose primary attack is a ranged \"Code Injection\": its
on-hit :CONVERT-TO-ALLY effect is special-cased directly inside
RESOLVE-ATTACK (RDESCENT/COMMANDS.LISP) rather than going through the
ordinary APPLY-STATUS-EFFECT path -- a successful, non-lethal hit
permanently flips the target's own DISPOSITION/FACTION to
:FRIENDLY/:COMPANION instead of attaching a STATUS-EFFECT. See this
section's own preamble for why a converted monster becomes a
permanently *passive* ally rather than an actively fighting one."
  :weapon-reach 5
  :on-hit-effect (list :kind :convert-to-ally))

(define-armory-equippable-item c-suite-keycard
  "The C-Suite Keycard"
  :off-hand
  nil
  "Legendary amulet (folded into the :OFF-HAND slot -- see this
section's own preamble) granting no passive stat bonus; its real
mechanic (every sufficiently low-XP hostile monster's disposition
becomes :FLEEING instead of :HOSTILE) is wired directly into
ENTITY-DISPOSITION-TOWARD via C-SUITE-KEYCARD-ACTIVE-P/
*RDESCENT-C-SUITE-KEYCARD-MAX-XP*, reusing the existing :FLEEING AI
branch (PROCESS-ENEMY-TURNS' FLEEING-TURN) rather than inventing a new
terror/flee mechanic."
  )

(define-armory-equippable-item golden-parachute
  "The Golden Parachute"
  :body
  nil
  "Legendary body armor granting no passive stat bonus; its flagship
once-ever (not once-per-floor, unlike the YubiKey) death save is wired
in RDESCENT/ACTIONS.LISP's own MAYBE-TRIGGER-GOLDEN-PARACHUTE-SAVE: a
lethal hit instead fully heals the wearer, teleports them to a random
safe tile on the current floor (see this section's own preamble for
why not literally \"the next floor's stairwell\"), fills their
INVENTORY with a small severance package of ordinary armory gear, and
destroys the Golden Parachute itself (unequips it, one-time use,
never resettable)."
  )

(define-armory-equippable-item mechanical-keyboard-of-the-ancients
  "The Mechanical Keyboard of the Ancients (IBM Model M)"
  :weapon
  (list :power *rdescent-mechanical-keyboard-of-the-ancients-power-bonus*
        :defense *rdescent-mechanical-keyboard-of-the-ancients-defense-bonus*)
  "Legendary weapon/shield: a 15-pound mechanical keyboard granting
both a +POWER and a +DEFENSE STAT-BONUSES entry at once (its own
\"acts as both a heavy blunt weapon and a shield\" rider, folded into
one :WEAPON-slot item rather than occupying :OFF-HAND too -- see this
section's own preamble). Its \"Click-Clack\" sonic stun is wired as a
new MOVE-PLAYER post-processing pass, MAYBE-CLICK-CLACK-STUN
(RDESCENT/ACTIONS.LISP): every hostile entity within
*RDESCENT-CLICK-CLACK-STUN-RADIUS* tiles of the player's own new
position after each move is inflicted :STUNNED, for as long as this
item remains equipped."
  )

