;;; -*- Lisp -*-

;;; The ORC entity subclass (a code monkey) -- its DEFCLASS/MAKE-ORC
;;; factory, and every method specialized on ORC (today just
;;; ENTITY-ATTACK-FLAVOR-POOL, plus the attack-flavor content that
;;; method's pool draws from). See RDESCENT/ENEMIES/TROLL.LISP for the
;;; analogous file for TROLLs, and this system's own header comment
;;; (RDESCENT/ENTITIES.LISP) for the full file map. Adding a brand new
;;; ENTITY subclass should mean adding one new file like this one,
;;; rather than touching ENTITIES.LISP/MECHANICS.LISP/COMMANDS.LISP
;;; separately.

(in-package "JRM-CODE-PROJECT")

(defclass orc (enemy)
  ()
  (:documentation "A concrete subclass of ENEMY representing a code
monkey. Allows CLOS dispatch for specialized attacks (e.g.
RESOLVE-ATTACK), mirroring TROLL's own docstring/rationale."))

(defun make-orc (x y level)
  "Pure factory: return a fresh ENTITY representing a code monkey at
(X, Y) on LEVEL -- char #\\c, NAME \"code monkey\", BLOCKS-MOVEMENT T
(so the player cannot simply walk through it -- see
BLOCKING-ENTITY-AT/MOVE-PLAYER), 10 MAX-HP/HP, 0 DEFENSE, 3 POWER,
RENDER-ORDER 1, IS-ALIVE T, ENERGY 0, SPEED 10 (accrues 100 ENERGY --
enough for one move -- every 10 ticks/500ms, i.e. twice a second), XP
10 -- the fixed experience reward the player is awarded (see
MOVE-PLAYER) upon slaying this code monkey -- MESSAGE-COLOR
\"#e67e22\" (orange), the color combat messages about this code
monkey are displayed in (see ENTITY-MESSAGE-COLOR) -- and FACTION
:DISGRUNTLED-DEV (DISPOSITION defaults to :HOSTILE via ENEMY's own
INITIALIZE-INSTANCE :AFTER, unchanged from its pre-FACTION/
DISPOSITION behavior -- see ARCHITECTURE_PLAN.md §2/FUTURE_PLANS.md
§1, whose HYGIENE-banded hostility swing/SYNERGY Pacify Chance are
applied on top of this default by SPAWN-MONSTERS-FOR-LEVEL, not by
this factory)."
  (make-instance 'orc :x x :y y :char #\c :name "code monkey" :blocks-movement t :level level
                        :max-hp 10 :hp 10 :defense 0 :power 3 :render-order 1 :is-alive t
                        :energy 0 :speed 10 :xp 10 :message-color "#e67e22" :faction :disgruntled-dev))

;;; ------------------------------------------------------------------
;;; Attack-flavor text (see RDESCENT/MECHANICS.LISP's ENTITY-ATTACK-
;;; FLAVOR-POOL/RANDOM-ATTACK-FLAVOR-TEXT/RENDER-ATTACK-FLAVOR, the
;;; shared machinery that consumes these).

(defparameter *rdescent-vocalizations*
  '("chatters" "screeches" "squeals" "yells" "shouts" "howls" "screams" "growls" "snarls" "hisses" "grunts" "moans" "whines" "barks" "roars" "types")
  "Verbs describing the vocalizations of a monster attacking the player.")

(defparameter *rdescent-vocalization-adverbs*
  '("angrily" "excitedly" "furiously" "loudly" "menacingly" "venomously" "viciously" "ferociously" "savagely" "wildly" "frantically" "hysterically" "desperately" "insanely" "psychotically" "dementedly" "madly" "crazily" "uncontrollably" "violently" "aggressively" "threateningly" "ominously" "maliciously" "spitefully" "vengefully" "weirdly" "alarmingly" "recursively" "iteratively")
  "Adverbs describing the vocalizations of a monster attacking the player.")

(defparameter *rdescent-shitty-languages*
  '("Fortran" "COBOL" "PHP" "Visual Basic" "Perl" "Brainfuck" "AppleScript" "Mumps" "Assembly" "JavaScript" "Objective-C" "MATLAB" "Lisp" "Prolog" "Haskell" "Scala" "Erlang" "Forth" "Ada" "Smalltalk" "D" "R" "Lua" "Tcl" "Groovy" "ColdFusion" "VBA" "ActionScript" "Delphi" "RPG" "ABAP" "PL/I" "J#" "XSLT" "PostScript" "Logo" "Modula-2" "Algol" "Rust" "Dart" "Crystal" "Nim" "Elixir" "Clojure" "Julia" "Kotlin" "TypeScript")
  "Languages CODE-MONKEY-LANGUAGE-TAUNT chatters at you in.")

(defun code-monkey-rewrites ()
  "Return a fully-rendered attack-flavor sentence in which the Code
Monkey rewrites your code in a random way."
  (format nil "The Code Monkey ~A rewrites your code in ~A!" 
          (random-choice *rdescent-vocalization-adverbs*)
          (random-choice *rdescent-shitty-languages*)))

(defun code-monkey-language-taunt ()
  "Return a fully-rendered attack-flavor sentence in which the Code
Monkey chatters angrily in a random entry of
*RDESCENT-SHITTY-LANGUAGES*."
  (format nil "The Code Monkey ~A ~A at you in ~A!" 
          (random-choice *rdescent-vocalizations*)
          (random-choice *rdescent-vocalization-adverbs*)
          (random-choice *rdescent-shitty-languages*)))

(defparameter *rdescent-shitty-scripting-languages*
  '("Bash" "Perl" "Python" "PowerShell" "AWK" "Vibe Coded" "Tcl" "Lua" "R" "PHP" "Ruby" "Cold Fusion" "Groovy" "Scheme")
  "Languages CODE-MONKEY-SCRIPT-DUMP's 500-line script is written in.")

(defun code-monkey-script-dump ()
  "Return a fully-rendered attack-flavor sentence in which the Code
Monkey submits a 500-line script in a random entry of
*RDESCENT-SHITTY-SCRIPTING-LANGUAGES*."
  (format nil "The Code Monkey submits a 500-line ~A script!" (random-choice *rdescent-shitty-scripting-languages*)))

(defun code-monkey-monads ()
  "Return a fully-rendered attack-flavor sentence in which the Code
Monkey plays with his monads"
  (format nil "The Code Monkey ~A plays with his monads!" (random-choice *rdescent-vocalization-adverbs*)))

(defun code-monkey-eats ()
  "Return a fully-rendered attack-flavor sentence in which the Code
Monkey eats your lunch"
  (format nil "The Code Monkey eats ~A!"
          (random-choice '("your lunch" "the last bagel" "the last slice of pizza" "your protein bar"
                           "something he picked of his foot"))))

(defparameter *rdescent-code-monkey-attack-flavors*
  (list "The Code Monkey flings poo at you!"
        #'code-monkey-rewrites
        #'code-monkey-language-taunt
        (make-mechanical-attack-flavor
         #'code-monkey-script-dump
         :on-hit-effect (list :kind :confused :turns *rdescent-confusion-ticks* :chance 0.35))
        #'code-monkey-monads
        #'code-monkey-eats
        "The Code Monkey raises a PR with whitespace-only changes!"
        "The Code Monkey tries to resolve a merge conflict by deleting the repository!"
        "The Code Monkey pushes directly to the main branch on a Friday afternoon!"
        "The Code Monkey proudly shows you a twelve-level deep nested loop!"
        "The Code Monkey \"borrows\" your USB cable."
        "The Code Monkey copy-pastes from Stack Overflow without reading it!"
        "The Code Monkey renames all your variables to single letters!"
        "The Code Monkey adds a TODO comment and calls it done.")
  "Purely cosmetic pool of attack-flavor sentences PROCESS-ENEMY-TURNS
may pick from (via RANDOM-ATTACK-FLAVOR-TEXT) when a code monkey
attacks the player -- see ENTITY-ATTACK-FLAVOR-POOL. Each entry is
either a plain, already-complete sentence, a designator for a
function of no arguments that renders one on the fly (e.g. to splice
in a randomly chosen scripting language), or a MECHANICAL-ATTACK-
FLAVOR (RDESCENT/MECHANICS.LISP) pairing rendered text with a real
mechanical side effect (FUTURE_PLANS.md §7, \"Varied Attack Effects\")
-- see RANDOM-ATTACK-FLAVOR-TEXT. \"The Code Monkey submits a 500-line
~A script!\" (CODE-MONKEY-SCRIPT-DUMP) has a 35% chance, on a
successful non-lethal hit, to additionally inflict CONFUSED on the
player for *RDESCENT-CONFUSION-TICKS* -- re-using CAST-REORG-MEMO's
own existing Confusion mechanic, per §7's own \"chance to also inflict
Confusion\" bullet: a 500-line script dump is exactly the kind of
incomprehensible mess that leaves you confused. Every other entry
remains purely cosmetic, with zero mechanical difference.")

(defmethod entity-attack-flavor-pool ((ent orc))
  *rdescent-code-monkey-attack-flavors*)
