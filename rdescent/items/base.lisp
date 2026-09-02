;;; -*- Lisp -*-

;;; The RDESCENT-ITEM inventory-item base hierarchy: RDESCENT-ITEM
;;; itself, its TARGETED-ITEM/AREA-EFFECT-ITEM/CONSUMABLE-ITEM/
;;; EQUIPPABLE-ITEM subclasses, and this file's own small handful of
;;; bespoke concrete items (SCROLL-OF-PIP/REPLY-ALL-BOMB/REORG-MEMO/
;;; STACK-OF-UNREAD-MEMOS) that predate the DEFINE-ARMORY-EQUIPPABLE-
;;; ITEM/DEFINE-CONSUMABLE-ITEM macros used by every later item file.
;;;
;;; First of RDESCENT/ITEMS/*.LISP, the subdirectory the RDESCENT-ITEM
;;; hierarchy and every concrete item now live under (split out of the
;;; original monolithic RDESCENT/ENTITIES.LISP -- see that file's own
;;; header comment for the rest of the engine's file map). Loaded
;;; directly after RDESCENT/ENTITIES.LISP itself. See RDESCENT/ITEMS/
;;; EQUIPMENT.LISP (the DEFINE-ARMORY-EQUIPPABLE-ITEM macro and every
;;; FUTURE_PLANS.md §13 armory item), RDESCENT/ITEMS/LOOT.LISP
;;; (FUTURE_PLANS.md §15's ten unique Rare/Legendary drops, built on
;;; EQUIPMENT.LISP's own macro), RDESCENT/ITEMS/CONSUMABLES.LISP (the
;;; DEFINE-CONSUMABLE-ITEM macro, FUTURE_PLANS.md §17's three-tier
;;; Corporate Pharmacy catalog, and *RDESCENT-VENDOR-STOCK-TABLE*), and
;;; RDESCENT/ITEMS/GROUND-ITEMS.LISP (the GROUND-ITEM class itself,
;;; every MAKE-GROUND-* factory/DEFINE-GROUND-*-ITEM macro, and the
;;; key/stock-option/severance-package ground loot that doesn't fit the
;;; RDESCENT-ITEM hierarchy) for the rest.

(in-package "JRM-CODE-PROJECT")

(defclass rdescent-item ()
  ((name :initarg :name :reader get-item-name :initform nil))
  (:documentation "An immutable inventory item a player's ENTITY can
carry (see ENTITY's INVENTORY slot/GET-INVENTORY) and USE-ITEM can
consume. NAME (read via GET-ITEM-NAME) is the human-readable string
sent to the client verbatim in the \"inventory\" packet (see
RDESCENT-INVENTORY-PACKET, RDESCENT/SERVER.LISP) and used in
USE-ITEM's own message-log text. This base class is never itself
instantiated -- see its two subclasses, TARGETED-ITEM and
AREA-EFFECT-ITEM, which USE-ITEM dispatches on via ETYPECASE to decide
whether an item needs a single BLOCKING-ENTITY-AT target or damages
every qualifying entity within a blast radius. Like every other value
type in this file, an RDESCENT-ITEM is immutable once constructed --
there is nothing to update in place, since an item carries no mutable
state of its own (a used-up item is simply removed from its owner's
INVENTORY list, not mutated)."))

(defclass targeted-item (rdescent-item)
  ()
  (:documentation "An RDESCENT-ITEM that, when used (see USE-ITEM),
requires a single BLOCKING-ENTITY-AT the (TARGET-X, TARGET-Y) tile the
player aimed at -- if no such entity is there, the item is not
consumed and STATE is returned unchanged. See SCROLL-OF-PIP and
REORG-MEMO, the concrete subclasses so far."))

(defclass area-effect-item (rdescent-item)
  ()
  (:documentation "An RDESCENT-ITEM that, when used (see USE-ITEM),
damages every IS-ALIVE, BLOCKS-MOVEMENT entity within
*RDESCENT-REPLY-ALL-RADIUS* (Chebyshev distance) of the (TARGET-X,
TARGET-Y) tile the player aimed at, regardless of whether any entity
is actually in range -- the item is always consumed once used, unlike
a TARGETED-ITEM. See REPLY-ALL-BOMB, the only concrete subclass so
far."))

(defclass consumable-item (rdescent-item)
  ((heal-amount :initarg :heal-amount :reader get-heal-amount :initform nil)
   (energy-restore :initarg :energy-restore :reader get-energy-restore :initform nil)
   (effect :initarg :effect :reader get-consumable-effect :initform nil)
   (cleanse-kind :initarg :cleanse-kind :reader get-cleanse-kind :initform nil)
   (stat-overrides :initarg :stat-overrides :reader get-stat-overrides :initform nil)
   (flavor-text :initarg :flavor-text :reader get-flavor-text :initform nil))
  (:documentation "§17's Corporate Pharmacy catalog: an RDESCENT-ITEM
that, when used (see USE-ITEM), always acts on the player alone --
TARGET-X/TARGET-Y are ignored entirely, unlike TARGETED-ITEM/AREA-
EFFECT-ITEM -- self-administering some combination of the following,
all in one APPLY-ITEM call (RDESCENT/ACTIONS.LISP):
  - HEAL-AMOUNT: NIL (no healing), the keyword :FULL (heal to
    EFFECTIVE-MAX-HP outright), or a positive integer HP amount added
    and clamped at EFFECTIVE-MAX-HP via MIN -- never lets HP exceed
    its cap, unlike ENERGY-RESTORE below.
  - ENERGY-RESTORE: NIL (no restore), the keyword :FULL (set ENERGY to
    *RDESCENT-PHARMACY-FULL-ENERGY-RESTORE*, the same generous \"full
    tank\" value the Espresso Machine shrine already uses), or a
    positive integer ENERGY amount added with no upper clamp --
    ENERGY has no hard cap outside of ACCRUE-ENERGY's own passive
    *RDESCENT-MAX-BANKED-ENERGY* ceiling (see DRINK-POTION/
    INTERACT-SHRINE, which already exceed it this same way).
  - EFFECT: NIL, or a (:CHANCE <0-100> :KIND <keyword>
    :TICKS-REMAINING <integer> [:MAGNITUDE <integer>]
    [:EXPIRE-INTO <plist>]) plist -- CHANCE (default 100, i.e.
    unconditional) is rolled first; on success the STATUS-EFFECT is
    attached via ENTITY-WITH-EFFECT (not APPLY-STATUS-EFFECT -- a
    self-administered effect never rolls its own SENIORITY-
    DEFLECTION-CHANCE against the very player choosing to take it).
    A failed CHANCE roll is a pure no-op for this slot alone; every
    other slot (HEAL-AMOUNT/ENERGY-RESTORE/etc.) still applies
    regardless.
  - CLEANSE-KIND: NIL, or a status-effect keyword to strip outright
    (via ENTITY-WITH-EFFECT ... 0), e.g. The Smart Water's Food
    Poisoning cure.
  - STAT-OVERRIDES: NIL, or a plist of permanent ENTITY slot/value
    overrides applied via UPDATE-ENTITY, e.g. Dexedrine Spansule's
    permanent HYGIENE 0 (\"you stop showering\").
  - FLAVOR-TEXT: the message-log line describing the outcome, pushed
    in the player's own ENTITY-MESSAGE-COLOR.
Every CONSUMABLE-ITEM is always consumed once used (like AREA-EFFECT-
ITEM, unlike TARGETED-ITEM's own miss-doesn't-consume rule) -- there is
no \"target\" to miss. See DEFINE-CONSUMABLE-ITEM, the macro that
generates every concrete Tier 1-3 pharmacy item from this one
declarative shape, and UNMARKED-NOOTROPIC-STACK, the one item bespoke
enough (a genuine 50/50 either/or branch) to need its own APPLY-ITEM
method instead."))

(defgeneric item-modifier (item)
  (:documentation
   "Return ITEM's own :NORMAL/:CURSED/:BLESSED modifier (see
EQUIPPABLE-ITEM's own docstring). Every RDESCENT-ITEM answers this
uniformly -- a plain (non-EQUIPPABLE-ITEM) item like SCROLL-OF-PIP has
no notion of its own modifier at all, and this default :METHOD simply
returns :NORMAL for it, so SERIALIZE-ITEM can call ITEM-MODIFIER on any
RDESCENT-ITEM without an ETYPECASE of its own -- while EQUIPPABLE-ITEM
below overrides this with its own MODIFIER slot reader.")
  (:method ((item rdescent-item)) :normal))

(defgeneric get-durability (item)
  (:documentation
   "Return ITEM's own current durability, or NIL if ITEM has no notion
of durability at all (a plain non-EQUIPPABLE-ITEM like SCROLL-OF-PIP).
Every RDESCENT-ITEM answers this uniformly via this default :METHOD,
so SERIALIZE-ITEM can call GET-DURABILITY on any RDESCENT-ITEM without
an ETYPECASE of its own -- while EQUIPPABLE-ITEM below overrides this
with its own DURABILITY slot reader. See EQUIPPABLE-ITEM's own class
docstring.")
  (:method ((item rdescent-item)) nil))

(defgeneric get-max-durability (item)
  (:documentation
   "Return ITEM's own max durability, or NIL if ITEM has no notion of
durability at all -- the GET-DURABILITY counterpart, see its own
docstring.")
  (:method ((item rdescent-item)) nil))

(defparameter *rdescent-default-item-durability* 30
  "Fixed DURABILITY/MAX-DURABILITY (see EQUIPPABLE-ITEM's own docstring)
every armory MAKE-* factory defaults to when no explicit :DURABILITY/
:MAX-DURABILITY is passed -- deterministic on purpose (unlike the
genuinely random value a freshly *spawned* item actually gets, see
RANDOMIZE-NEWLY-SPAWNED-EQUIPPABLE-ITEM) so direct callers, most
importantly this engine's own test suite, can keep relying on a fixed,
predictable DURABILITY without threading an explicit value through
every MAKE-* call.")

(defparameter *rdescent-item-durability-min* 15
  "Lower bound (inclusive) of the DURABILITY/MAX-DURABILITY range
RANDOMIZE-NEWLY-SPAWNED-EQUIPPABLE-ITEM rolls for a freshly spawned
EQUIPPABLE-ITEM -- see EQUIPPABLE-ITEM's own docstring.")

(defparameter *rdescent-item-durability-max* 40
  "Upper bound (inclusive) of the DURABILITY/MAX-DURABILITY range
RANDOMIZE-NEWLY-SPAWNED-EQUIPPABLE-ITEM rolls for a freshly spawned
EQUIPPABLE-ITEM -- see EQUIPPABLE-ITEM's own docstring.")

(defparameter *rdescent-item-durability-loss-per-hit* 1
  "How much DURABILITY a single equipped item -- chosen at random
among every non-NIL slot in the defending ENTITY's own EQUIPMENT --
loses each time that ENTITY takes actual (non-zero, non-dodged)
combat damage. See APPLY-EQUIPMENT-WEAR, RESOLVE-ATTACK's own
integration point for this mechanic.")

(defgeneric item-cloaked-p (item)
  (:documentation
   "Return true if ITEM's own :CURSED/:BLESSED modifier prefix (see
GET-ITEM-NAME's :AROUND method on EQUIPPABLE-ITEM below) should stay
hidden from its displayed name. Every RDESCENT-ITEM answers this
uniformly -- a plain (non-EQUIPPABLE-ITEM) item like SCROLL-OF-PIP has
no notion of cloaking at all, and this default :METHOD simply returns
NIL for it (harmless, since such an item never has a modifier prefix
to hide in the first place) -- while EQUIPPABLE-ITEM below overrides
this with its own CLOAKED slot reader.")
  (:method ((item rdescent-item)) nil))

(defclass equippable-item (rdescent-item)
  ((equip-slot :initarg :equip-slot :reader get-equip-slot)
   (stat-bonuses :initarg :stat-bonuses :reader get-stat-bonuses :initform nil)
   (weapon-reach :initarg :weapon-reach :reader get-weapon-reach :initform 1)
   (weapon-hits-per-turn :initarg :weapon-hits-per-turn :reader get-weapon-hits-per-turn :initform 1)
   (on-hit-effect :initarg :on-hit-effect :reader get-on-hit-effect :initform nil)
   (modifier :initarg :modifier :reader item-modifier :initform :normal)
   (cloaked :initarg :cloaked :reader item-cloaked-p :initform t)
   (max-durability :initarg :max-durability :reader get-max-durability
                   :initform *rdescent-default-item-durability*)
   (durability :initarg :durability :reader get-durability :initform nil))
  (:documentation "An RDESCENT-ITEM that, unlike TARGETED-ITEM/
AREA-EFFECT-ITEM, is never consumed by USE-ITEM -- instead it is moved
between an entity's INVENTORY and EQUIPMENT via EQUIP-ITEM/UNEQUIP-ITEM
(ARCHITECTURE_PLAN.md §4) and, while equipped, passively contributes to
whichever entity has it equipped for as long as it stays equipped.
EQUIP-SLOT (read via GET-EQUIP-SLOT) is one of :WEAPON/:BODY/:HEAD/
:OFF-HAND -- which key of ENTITY's EQUIPMENT plist this item occupies;
EQUIP-ITEM refuses to equip an EQUIPPABLE-ITEM into any slot but its
own. STAT-BONUSES (read via GET-STAT-BONUSES) is a plist of the same
shape as EFFECTIVE-POWER/EFFECTIVE-DEFENSE/EFFECTIVE-MAX-HP's own
lookup keys (e.g. (:POWER 5 :DEFENSE 2)) -- every non-NIL slot's own
STAT-BONUSES entry for a given stat is summed together (see
STAT-BONUS-TOTAL) and added onto the wearer's raw POWER/DEFENSE/
MAX-HP each time the corresponding EFFECTIVE-* function is read;
absent keys default to 0 (via GETF), so an item that only cares about
one stat (e.g. \"+3 Armor\", DEFENSE only) simply omits the others.
WEAPON-REACH/WEAPON-HITS-PER-TURN/ON-HIT-EFFECT (read via
GET-WEAPON-REACH/GET-WEAPON-HITS-PER-TURN/GET-ON-HIT-EFFECT) are only
ever consulted when this item is equipped in the :WEAPON slot (see
EFFECTIVE-WEAPON/WEAPON-REACH/WEAPON-HITS-PER-TURN/WEAPON-ON-HIT-
EFFECT) -- WEAPON-REACH (default 1, today's fixed melee-range-1
assumption) is how far PROCESS-ENEMY-TURNS' attack-range gate lets a
wielder strike from; WEAPON-HITS-PER-TURN (default 1) is intended for
a future rapid-fire weapon (e.g. FUTURE_PLANS.md §13's Rubber Band
Gatling Gun) to land more than one RESOLVE-ATTACK per turn -- no
caller multiplies by it yet, since no weapon sets it above 1; and
ON-HIT-EFFECT (default NIL) is an optional (:KIND <keyword> :TURNS
<integer> [:MAGNITUDE <integer>]) plist RESOLVE-ATTACK feeds straight
into APPLY-STATUS-EFFECT on a successful, non-lethal hit (e.g. a
future LART item silencing its target) -- a plain unarmed/gearless
attacker's EFFECTIVE-WEAPON is NIL, and WEAPON-REACH/WEAPON-HITS-PER-
TURN/WEAPON-ON-HIT-EFFECT all treat a NIL weapon as reach 1/1 hit per
turn/no on-hit effect, exactly matching pre-equipment-system combat.
The concrete subclasses below now populate FUTURE_PLANS.md §13's
Arsenal/Dress Code/Peripherals/Resume Fillers content pass almost
entirely through these existing generic hooks (STAT-BONUSES, reach,
multi-hit, on-hit status effects). Where the plan text called for a
mechanic this engine still lacks a primitive for (ammo, two-handed
slot-locking, cone/AoE weapon resolution, forced movement, NPC
summoning, etc.), those subclasses' own docstrings spell out the
deliberate simplification used instead.
MAX-DURABILITY/DURABILITY (read via GET-MAX-DURABILITY/GET-DURABILITY)
are this item's wear-and-tear stat: MAX-DURABILITY is fixed at
construction (*RDESCENT-DEFAULT-ITEM-DURABILITY* for a direct MAKE-*
call, or a random *RDESCENT-ITEM-DURABILITY-MIN*..*RDESCENT-ITEM-
DURABILITY-MAX* roll for one actually spawned into the world -- see
RANDOMIZE-NEWLY-SPAWNED-EQUIPPABLE-ITEM, mirroring MODIFIER's own
spawn-time-only randomization below) and never changes thereafter;
DURABILITY starts equal to MAX-DURABILITY (an :AFTER method on
INITIALIZE-INSTANCE fills it in whenever the :DURABILITY initarg isn't
explicitly supplied) and is reduced by APPLY-EQUIPMENT-WEAR each time
its wearer takes a real hit in combat (RESOLVE-ATTACK) -- when it
reaches 0, the item is removed from EQUIPMENT entirely (destroyed),
even if it's :CURSED (unlike an ordinary player-initiated UNEQUIP-ITEM,
this isn't going through that function's own cursed rejection at all,
so the same "can't take off a cursed item" restriction cannot apply to
an item that has simply broken). Since DURABILITY genuinely mutates
over an item's lifetime -- the one slot on this class that isn't
either fixed-per-subclass or fixed-per-instance-at-spawn -- reducing
it always goes through the pure ITEM-WITH-DURABILITY updater (never a
SETF), consistent with this file's \"never mutate, always derive\"
discipline elsewhere.
MODIFIER (read via ITEM-MODIFIER, default :NORMAL) is a per-instance
:NORMAL/:CURSED/:BLESSED property -- unlike EQUIP-SLOT/STAT-BONUSES/
WEAPON-REACH, this is genuinely per-instance state (any two instances
of, say, KEYBOARD-OF-KINESIS may carry different MODIFIERs), rolled
randomly at the moment an equippable item is actually spawned (see
RANDOM-ITEM-MODIFIER/RANDOMIZE-NEWLY-SPAWNED-EQUIPPABLE-ITEM and their
call sites in PURCHASE-ITEM/SPAWN-ITEMS-FOR-LEVEL) rather than fixed by the
concrete subclass. :CURSED (queried via the derived predicate
ITEM-CURSED-P, i.e. (EQ (ITEM-MODIFIER ITEM) :CURSED)) marks an item
UNEQUIP-ITEM refuses to remove once equipped -- see UNEQUIP-ITEM's own
docstring for the exact rejection message, and EQUIP-ITEM's for why a
cursed item already occupying a slot also blocks equipping something
*else* into that same slot (the implicit unequip an ordinary slot-swap
performs would otherwise silently bypass the same restriction) -- and
negates (via SCALE-STAT-BONUS-FOR-MODIFIER) every one of the item's own
STAT-BONUSES entries when STAT-BONUS-TOTAL sums them, turning each
would-be buff into a debuff (and vice versa). :BLESSED (queried via
ITEM-BLESSED-P) is the opposite: no UNEQUIP-ITEM restriction, and every
STAT-BONUSES entry is enhanced by an extra 50% of its own magnitude.
:NORMAL leaves STAT-BONUSES untouched and carries no UNEQUIP-ITEM
restriction. Because MODIFIER is real per-instance state, it does need
its own PERSISTENCE.LISP handling -- see SERIALIZE-ITEM/DESERIALIZE-
ITEM/MAKE-ITEM-FROM-CLASS-TAG's own :MODIFIER key/argument, exactly the
future need PERSISTENCE.LISP's RDESCENT-ITEM section header comment
anticipated; DURABILITY/MAX-DURABILITY are persisted the same way, via
that same trio's own :DURABILITY/:MAX-DURABILITY key/arguments. GET-ITEM-NAME is likewise specialized (:AROUND) on
EQUIPPABLE-ITEM to prepend \"Cursed \"/\"Blessed \" to the base NAME
whenever MODIFIER isn't :NORMAL and the item isn't CLOAKED (see
CLOAKED below), so GROUP-INVENTORY-FOR-DISPLAY's own
name-based grouping naturally keeps a Cursed/Blessed instance separate
from a :NORMAL one of the same underlying item once uncloaked.
CLOAKED (read via ITEM-CLOAKED-P, default T) hides that
\"Cursed \"/\"Blessed \" prefix from a freshly rolled item until the
player has actually equipped it -- so a player can't tell a cursed
item from a blessed (or normal) one just by picking it up or looking
at their inventory, only by risking wearing it (see EQUIP-ITEM,
RDESCENT/ACTIONS.LISP, which returns a fresh ITEM-UNCLOAKED copy of
whatever it equips). Like MODIFIER, CLOAKED is genuinely per-instance
state (rolled true at spawn, then flipped false forever the first time
that specific instance is equipped -- ITEM-UNCLOAKED never re-cloaks
an already-uncloaked item), so it needs its own PERSISTENCE.LISP
handling too -- see SERIALIZE-ITEM/DESERIALIZE-ITEM/MAKE-ITEM-FROM-
CLASS-TAG's own :CLOAKED key/argument."))

(defun item-cursed-p (item)
  "Return true if ITEM's own ITEM-MODIFIER is :CURSED -- see
EQUIPPABLE-ITEM's own docstring for what that means for EQUIP-ITEM/
UNEQUIP-ITEM/STAT-BONUS-TOTAL."
  (eq (item-modifier item) :cursed))

(defun item-blessed-p (item)
  "Return true if ITEM's own ITEM-MODIFIER is :BLESSED -- see
EQUIPPABLE-ITEM's own docstring for what that means for
STAT-BONUS-TOTAL (an extra 50% enhancement of each of the item's own
STAT-BONUSES entries)."
  (eq (item-modifier item) :blessed))

(defmethod get-item-name :around ((item equippable-item))
  "Prepend \"Cursed \"/\"Blessed \" to ITEM's own base NAME (the primary
GET-ITEM-NAME method's result) when ITEM-MODIFIER isn't :NORMAL and
ITEM isn't ITEM-CLOAKED-P -- see EQUIPPABLE-ITEM's own docstring. A
still-CLOAKED item always shows its plain base NAME regardless of its
own MODIFIER, so a player can't tell a cursed item from a blessed (or
normal) one until they actually equip it."
  (let ((base (call-next-method)))
    (if (item-cloaked-p item)
        base
        (case (item-modifier item)
          (:cursed (format nil "Cursed ~A" base))
          (:blessed (format nil "Blessed ~A" base))
          (t base)))))

(defclass scroll-of-pip (targeted-item)
  ()
  (:default-initargs :name "Scroll of PIP")
  (:documentation "A single-target offensive item: USE-ITEM deals
*RDESCENT-PIP-DAMAGE* (20, bypassing DEFENSE) to the BLOCKING-ENTITY-AT
the aimed-at tile. See MAKE-SCROLL-OF-PIP."))

(defclass reply-all-bomb (area-effect-item)
  ()
  (:default-initargs :name "Reply-All Bomb")
  (:documentation "An area-of-effect offensive item: USE-ITEM deals
*RDESCENT-REPLY-ALL-DAMAGE* (15, bypassing DEFENSE) to every
qualifying entity within *RDESCENT-REPLY-ALL-RADIUS* tiles of the
aimed-at tile. See MAKE-REPLY-ALL-BOMB."))

(defclass reorg-memo (targeted-item)
  ()
  (:default-initargs :name "Vague Re-Org Memo")
  (:documentation "A single-target debuff item, unlike SCROLL-OF-PIP/
REPLY-ALL-BOMB's flat damage: USE-ITEM sets the BLOCKING-ENTITY-AT the
aimed-at tile's ENTITY-CONFUSED-TICKS to *RDESCENT-CONFUSION-TICKS*
(10), via CAST-REORG-MEMO, so it staggers around at random instead of
pursuing the player for its next several turns (see
CONFUSED-ENTITY-TURN/PROCESS-ENEMY-TURNS). Deals no damage of its own.
See MAKE-REORG-MEMO."))

(defclass stack-of-unread-memos (equippable-item)
  ()
  (:default-initargs :name "A Stack of Unread Memos (Hardbound)"
                     :equip-slot :weapon
                     :stat-bonuses (list :power 5))
  (:documentation "The first concrete EQUIPPABLE-ITEM (ARCHITECTURE_
PLAN.md §4, FUTURE_PLANS.md §13's Arsenal section) -- a proof-of-
concept \"standard blunt instrument\" weapon, deliberately the simplest
possible EQUIPPABLE-ITEM: a flat +5 :POWER STAT-BONUSES entry in its
own :WEAPON EQUIP-SLOT, and none of WEAPON-REACH/WEAPON-HITS-PER-TURN/
ON-HIT-EFFECT overridden from EQUIPPABLE-ITEM's own neutral defaults
(reach 1, one hit per turn, no on-hit effect) -- exercising the
equip/unequip/EFFECTIVE-POWER pipeline end-to-end without any of that
extra complexity. See MAKE-STACK-OF-UNREAD-MEMOS."))

(defun make-scroll-of-pip ()
  "Pure factory: return a fresh SCROLL-OF-PIP, a TARGETED-ITEM named
\"Scroll of PIP\" (see USE-ITEM for what using one does)."
  (make-instance 'scroll-of-pip))

(defun make-reply-all-bomb ()
  "Pure factory: return a fresh REPLY-ALL-BOMB, an AREA-EFFECT-ITEM
named \"Reply-All Bomb\" (see USE-ITEM for what using one does)."
  (make-instance 'reply-all-bomb))

(defun make-reorg-memo ()
  "Pure factory: return a fresh REORG-MEMO, a TARGETED-ITEM named
\"Vague Re-Org Memo\" (see USE-ITEM/CAST-REORG-MEMO for what using one
does)."
  (make-instance 'reorg-memo))

(defun make-stack-of-unread-memos (&key (modifier :normal) max-durability durability (cloaked t))
  "Pure factory: return a fresh STACK-OF-UNREAD-MEMOS, an
EQUIPPABLE-ITEM named \"A Stack of Unread Memos (Hardbound)\" that
grants +5 EFFECTIVE-POWER when equipped in the player's :WEAPON slot
(see EQUIP-ITEM/EFFECTIVE-POWER). MODIFIER (default :NORMAL) is passed
straight through as the new instance's own ITEM-MODIFIER -- see
EQUIPPABLE-ITEM's own docstring for what :CURSED/:BLESSED do. CLOAKED
(default T) is passed straight through as the new instance's own
ITEM-CLOAKED-P (see EQUIPPABLE-ITEM's own class docstring).
MAX-DURABILITY/DURABILITY (default NIL, meaning \"use EQUIPPABLE-ITEM's
own class default/derive from MAX-DURABILITY\") are only forwarded to
MAKE-INSTANCE when explicitly supplied, so a direct/test call keeps
this item's usual deterministic *RDESCENT-DEFAULT-ITEM-DURABILITY*."
  (apply #'make-instance 'stack-of-unread-memos :modifier modifier :cloaked cloaked
         (append (when max-durability (list :max-durability max-durability))
                 (when durability (list :durability durability)))))

