;;; -*- Lisp -*-

;;; The MIDDLE-MANAGER entity subclass -- RDESCENT's first :MANAGEMENT-
;;; faction bestiary entry (FUTURE_PLANS.md §1, "Monster Classes &
;;; Faction Aggression"), added so HYGIENE's Suit/feral hostility
;;; swing (see HYGIENE-BANDED-DISPOSITION, RDESCENT/ENTITIES.LISP) is
;;; actually visible in play, distinct from the existing :DISGRUNTLED-
;;; DEV "Code Monkey"/"Internet Troll" pair. Its DEFCLASS/MAKE-MIDDLE-
;;; MANAGER factory, and its ENTITY-ATTACK-FLAVOR-POOL method plus the
;;; attack-flavor content that pool draws from. See RDESCENT/ENEMIES/
;;; ORC.LISP for the analogous file for Orcs, and this system's own
;;; header comment (RDESCENT/ENTITIES.LISP) for the full file map.
;;; Adding a brand new ENTITY subclass should mean adding one new file
;;; like this one, rather than touching ENTITIES.LISP/MECHANICS.LISP/
;;; COMMANDS.LISP separately.

(in-package "JRM-CODE-PROJECT")

(defclass middle-manager (enemy)
  ()
  (:documentation "A concrete subclass of ENEMY representing a Middle
Manager. Allows CLOS dispatch for specialized attacks (e.g.
RESOLVE-ATTACK), mirroring ORC/TROLL's own docstring/rationale --
today only ENTITY-ATTACK-FLAVOR-POOL is specialized; RESOLVE-ATTACK
falls back to ENTITY's own generic method, unlike TROLL's Confusion-
inflicting override."))

(defun make-middle-manager (x y level)
  "Pure factory: return a fresh ENTITY representing a Middle Manager at
(X, Y) on LEVEL -- char #\\M, NAME \"Middle Manager\", BLOCKS-MOVEMENT
T (so the player cannot simply walk through it -- see BLOCKING-ENTITY-
AT/MOVE-PLAYER), 14 MAX-HP/HP, 1 DEFENSE, 3 POWER, RENDER-ORDER 1,
IS-ALIVE T, ENERGY 0, SPEED 8 (accrues 100 ENERGY -- enough for one
move -- every 12.5 ticks/625ms), XP 18 -- between a code monkey's (10)
and an Internet Troll's (25), the fixed experience reward the player
is awarded (see MOVE-PLAYER) upon slaying this Middle Manager --
MESSAGE-COLOR \"#3498db\" (a corporate blue), the color combat messages
about this Middle Manager are displayed in (see ENTITY-MESSAGE-COLOR)
-- and FACTION :MANAGEMENT (DISPOSITION defaults to :HOSTILE via
ENEMY's own INITIALIZE-INSTANCE :AFTER, exactly like MAKE-ORC/MAKE-
TROLL -- see ARCHITECTURE_PLAN.md §2/FUTURE_PLANS.md §1, whose
HYGIENE-banded hostility swing/SYNERGY Pacify Chance are applied on
top of this default by SPAWN-MONSTERS-FOR-LEVEL, not by this
factory). *RDESCENT-MONSTER-SPAWN-TABLE* (RDESCENT/DUNGEON.LISP) gates
this factory to MIN-DEPTH 3, so a Middle Manager never appears on the
very first level or two."
  (make-instance 'middle-manager :x x :y y :char #\M :name "Middle Manager" :blocks-movement t :level level
                                  :max-hp 14 :hp 14 :defense 1 :power 3 :render-order 1 :is-alive t
                                  :energy 0 :speed 8 :xp 18 :message-color "#3498db" :faction :management))

;;; ------------------------------------------------------------------
;;; Attack-flavor text (see RDESCENT/MECHANICS.LISP's ENTITY-ATTACK-
;;; FLAVOR-POOL/RANDOM-ATTACK-FLAVOR-TEXT/RENDER-ATTACK-FLAVOR, the
;;; shared machinery that consumes these).

(defparameter *rdescent-middle-manager-attack-flavors*
  (list "The Middle Manager schedules a meeting to discuss scheduling meetings!"
        "The Middle Manager forwards you an email with \"URGENT!!!\" in the subject line!"
        "The Middle Manager asks for a status update on the status update!"
        "The Middle Manager CCs the entire department on a one-line reply!"
        "The Middle Manager circles back to touch base and align on synergies!"
        "The Middle Manager adds \"per my last email\" to a passive-aggressive reply-all!"
        "The Middle Manager schedules a mandatory 6am stand-up across three time zones!"
        "The Middle Manager asks you to take this offline, then never follows up!"
        "The Middle Manager requests a deck with more bullet points!"
        "The Middle Manager reorganizes the team chart for the third time this quarter!"
        "The Middle Manager asks for a \"quick sync\" that lasts 45 minutes!"
        "The Middle Manager sends a calendar invite with no agenda!"
        "The Middle Manager asks for a \"deep dive\" on a shallow topic!")
  "Purely cosmetic pool of attack-flavor sentences PROCESS-ENEMY-TURNS
may pick from (via RANDOM-ATTACK-FLAVOR-TEXT) when a Middle Manager
attacks the player -- see ENTITY-ATTACK-FLAVOR-POOL. Every entry is a
plain, already-complete sentence -- unlike ORC/TROLL's pools, none of
these are wired to a mechanical side effect (FUTURE_PLANS.md §7); a
Middle Manager's attacks are purely cosmetic annoyance today.")

(defmethod entity-attack-flavor-pool ((ent middle-manager))
  *rdescent-middle-manager-attack-flavors*)
