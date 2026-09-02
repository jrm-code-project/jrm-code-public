;;; -*- Lisp -*-

;;; The TROLL entity subclass (an Internet Troll) -- its DEFCLASS/
;;; MAKE-TROLL factory, and every method specialized on TROLL: its
;;; RESOLVE-ATTACK method (a 25% chance to inflict CONFUSED on a
;;; successful, non-lethal hit -- see RDESCENT/COMMANDS.LISP for the
;;; shared RESOLVE-ATTACK generic/ENTITY method every attacker/defender
;;; pair goes through first), and its ENTITY-ATTACK-FLAVOR-POOL method,
;;; plus the attack-flavor content that method's pool draws from. See
;;; RDESCENT/ENEMIES/ORC.LISP for the analogous file for ORCs, and this
;;; system's own header comment (RDESCENT/ENTITIES.LISP) for the full
;;; file map. Adding a brand new ENTITY subclass should mean adding one
;;; new file like this one, rather than touching ENTITIES.LISP/
;;; MECHANICS.LISP/COMMANDS.LISP separately.

(in-package "JRM-CODE-PROJECT")

(defclass troll (enemy)
  ()
  (:documentation "A concrete subclass of ENEMY representing an Internet Troll.
Allows CLOS dispatch for specialized attacks (e.g. RESOLVE-ATTACK)."))

(defun make-troll (x y level)
  "Pure factory: return a fresh ENTITY representing an Internet Troll
at (X, Y) on LEVEL -- char #\\T, NAME \"Internet Troll\",
BLOCKS-MOVEMENT T (so the player cannot simply walk through it -- see
BLOCKING-ENTITY-AT/MOVE-PLAYER), 16 MAX-HP/HP, 1 DEFENSE, 4 POWER,
RENDER-ORDER 1, IS-ALIVE T, ENERGY 0, SPEED 5 (accrues 100 ENERGY --
enough for one move -- every 20 ticks/1000ms, i.e. once a second), XP
25 -- the fixed experience reward the player is awarded (see
MOVE-PLAYER) upon slaying this Internet Troll, higher than a code
monkey's since Internet Trolls are tougher/hit harder -- MESSAGE-COLOR
\"#9b59b6\" (purple), the color combat messages about this Internet
Troll are displayed in (see ENTITY-MESSAGE-COLOR) -- and FACTION
:DISGRUNTLED-DEV (DISPOSITION defaults to :HOSTILE via ENEMY's own
INITIALIZE-INSTANCE :AFTER, unchanged from its pre-FACTION/
DISPOSITION behavior -- see ARCHITECTURE_PLAN.md §2/FUTURE_PLANS.md
§1, whose HYGIENE-banded hostility swing/SYNERGY Pacify Chance are
applied on top of this default by SPAWN-MONSTERS-FOR-LEVEL, not by
this factory)."
  (make-instance 'troll :x x :y y :char #\T :name "Internet Troll" :blocks-movement t :level level
                        :max-hp 16 :hp 16 :defense 1 :power 4 :render-order 1 :is-alive t
                        :energy 0 :speed 5 :xp 25 :message-color "#9b59b6" :faction :disgruntled-dev))

(defmethod resolve-attack ((attacker troll) defender)
  "Trolls have a 25% chance to inflict CONFUSED for
*RDESCENT-CONFUSION-SECONDS* on a successful, non-lethal hit -- see
*RDESCENT-CONFUSION-TICKS* (RDESCENT/ENTITIES.LISP), the single
source of truth for how long Confusion lasts regardless of source, so
this stays consistent with a Vague Re-Org Memo's own Confusion (see
CAST-REORG-MEMO) and the Code Monkey's \"submits a 500-line script!\"
attack flavor (RDESCENT/ENEMIES/ORC.LISP)."
  (multiple-value-bind (damage dies new-defender) (call-next-method)
    (if (and damage (> damage 0) (not dies) (< (random 1.0) 0.25))
        (values damage dies (apply-status-effect new-defender :confused *rdescent-confusion-ticks*))
        (values damage dies new-defender))))

;;; ------------------------------------------------------------------
;;; Attack-flavor text (see RDESCENT/MECHANICS.LISP's ENTITY-ATTACK-
;;; FLAVOR-POOL/RANDOM-ATTACK-FLAVOR-TEXT/RENDER-ATTACK-FLAVOR, the
;;; shared machinery that consumes these).

(defparameter *rdescent-troll-pedantic-words*
  '("done" "bug" "interface" "not working" "technical debt" "scalable" "MVP" "synergy" "low-hanging fruit" "pivot" "disruptive" "blocker" "refactor" "legacy code" "bandwidth" "touch base" "actionable" "deep dive" "circle back" "onboarding" "alignment" "air" "water")
  "Words TROLL-DEFINITION-DEMAND smugly asks you to define.")

(defun troll-definition-demand ()
  "Return a fully-rendered attack-flavor sentence in which the Troll
smugly asks you to define a random entry of
*RDESCENT-TROLL-PEDANTIC-WORDS*."
  (format nil "The Troll smugly asks you to define \"~A.\"" (random-choice *rdescent-troll-pedantic-words*)))

(defparameter *rdescent-troll-corrections*
  '("it's supposed to work that way" "the interface is working as intended" "technical debt is a myth" "scalability is a non-issue" "MVP is just a buzzword" "onboarding is unnecessary" "alignment is overrated" "it's *GNU*/Linux, not Linux" "it's just Talking Heads, not *The* Talking Heads" "you're off by 1" "Einstein never said that" "gravity is not a force, it's curvature of spacetime" "if this were written in Rust, it would be memory-safe" "the code is already optimized" "the API is self-documenting" "the documentation is correct, you just don't understand it" "the compiler is right, you're wrong" "you only *think* you understand it.." "you're doing it wrong.." "the error lies with the user" "that's undefined behavior")
  "Pedantic corrections the Troll makes with a smug \"Well, actually...\".")

(defun troll-actually-corrects ()
  "Return a fully-rendered attack-flavor sentence in which the Troll
corrects you with a pedantic \"Well, actually...\"."
  (format nil "The Troll says, \"Well, actually, ~A.\"" (random-choice *rdescent-troll-corrections*)))

(defparameter *rdescent-troll-attack-flavors*
  (list "The Internet Troll forwards your sarcastic Slack message to HR!"
        #'troll-actually-corrects
        "The Troll insists you're using \"less\" when you meant \"fewer.\""
        "The Troll points out a typo in a comment from three commits ago."
        "The Internet Troll starts playing Devil's Advocate about the new PTO policy."
        "The Troll CCs your manager!"
        (make-mechanical-attack-flavor
         "The Troll demands peer-reviewed evidence!"
         :on-hit-effect (list :kind :analysis-paralysis :turns *rdescent-analysis-paralysis-ticks*))
        #'troll-definition-demand
        (make-mechanical-attack-flavor
         "The Troll flags your Jira ticket as \"Needs More Info\" without leaving a comment!"
         :force-no-damage t
         :always-effect (list :kind :distracted :turns *rdescent-distraction-ticks*))
        "The Troll unearths a three-year-old Slack thread to use out of context!"
        "The Troll replies-all with just \"+1\"."
        "The Troll asks if you've tried turning it off and on again.")
  "Purely cosmetic pool of attack-flavor sentences PROCESS-ENEMY-TURNS
may pick from (via RANDOM-ATTACK-FLAVOR-TEXT) when an Internet Troll
attacks the player -- see *RDESCENT-CODE-MONKEY-ATTACK-FLAVORS* (in
RDESCENT/ENEMIES/ORC.LISP) for the same idea applied to code monkeys, and
ENTITY-ATTACK-FLAVOR-POOL for how PROCESS-ENEMY-TURNS picks the right
pool for a given attacker. Two entries carry real mechanical side
effects (FUTURE_PLANS.md §7, \"Varied Attack Effects\"), each wrapped in
a MECHANICAL-ATTACK-FLAVOR (RDESCENT/MECHANICS.LISP): \"demands peer-
reviewed evidence!\" inflicts :ANALYSIS-PARALYSIS on a successful non-
lethal hit (see EFFECTIVE-DODGE-CHANCE, RDESCENT/ENTITIES.LISP -- a
demand for citations makes you second-guess your next dodge), and
\"flags your Jira ticket as 'Needs More Info'\" is a pure-annoyance
attack (FORCE-NO-DAMAGE T -- no damage roll at all) that unconditionally
inflicts :DISTRACTED (see ADVANCE-ENTITY-TICK, RDESCENT/MECHANICS.LISP
-- delays the player's next Energy tick by one heartbeat, exactly like
being pulled away to write a status update). Every other entry, and the
Troll's own separate RESOLVE-ATTACK method's 25% flat CONFUSED chance
above, remain unchanged.")

(defmethod entity-attack-flavor-pool ((ent troll))
  *rdescent-troll-attack-flavors*)
