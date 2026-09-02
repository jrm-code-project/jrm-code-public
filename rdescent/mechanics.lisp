;;; -*- Lisp -*-

;;; Message-log/combat-message/attack-flavor text (LOG-MESSAGE,
;;; ADD-LOG-MESSAGE/APPEND-LOG-MESSAGES, BUILD-COMBAT-MESSAGES,
;;; RANDOM-ATTACK-FLAVOR-TEXT and the per-monster-type flavor pools),
;;; the DUNGEON-LEVEL-SNAPSHOT value type, the per-tick energy/healing
;;; reducers (ACCRUE-ENERGY/ACCRUE-HEALING/REDUCE-TICK), and the
;;; top-level game/map factories (MAKE-INITIAL-MAP/MAKE-INITIAL-STATE/
;;; MAKE-DETERMINISTIC-RANDOM-STATE) plus the "Corporate RPG Stats"
;;; rolling machinery (ROLL-STAT/*RDESCENT-CORPORATE-STAT-ACCESSORS*)
;;; that MAKE-INITIAL-STATE uses to build a fresh player.
;;;
;;; Second of several files this engine was split across (originally a
;;; single ENGINE.LISP) -- see RDESCENT/ENTITIES.LISP for the value
;;; types/factories this file builds on top of (ENTITY/GAME-STATE/
;;; GAME-MAP/etc.), and RDESCENT/DUNGEON.LISP, RDESCENT/ACTIONS.LISP,
;;; RDESCENT/COMMANDS.LISP for the rest. See RDESCENT/ENTITIES.LISP's
;;; own header comment for the full file map.

(in-package "JRM-CODE-PROJECT")

(defstruct dungeon-level-snapshot
  "An immutable snapshot of everything about a single dungeon LEVEL
that must survive the player leaving it via stairs and (possibly much
later) returning: MAP is that LEVEL's GAME-MAP, ENTITIES is the list
of every non-player ENTITY that was on it (monsters, corpses, and
stairs -- so an Internet Troll left half-dead, or a corpse, is exactly where the
player left it on return), and EXPLORED is that LEVEL's own
WIDTH*HEIGHT fog-of-war bit-vector (fog-of-war is per-level, not
global, since a tile explored on depth 3 says nothing about depth 4).
This is GAME-STATE's LEVELS FSET:MAP's value type: LEVELS maps a
depth (integer) to a DUNGEON-LEVEL-SNAPSHOT, populated by
MAKE-INITIAL-STATE for depth 1 and updated every time the player uses
a staircase (see USE-STAIRS) to record the level they are leaving
before switching MAP/ENTITIES/EXPLORED over to the destination depth's
own (freshly generated, or previously-saved) snapshot. All three slots
are :READ-ONLY, matching every other immutable value in this file."
  (map nil :read-only t)
  (entities nil :read-only t)
  (explored nil :read-only t))

(defstruct (log-message (:constructor make-log-message (text &optional (color "white"))))
  "A single MESSAGE-LOG entry: TEXT is the message string, COLOR is the
CSS color it should be rendered in (defaults to \"white\"). Plain
strings are still accepted anywhere a MESSAGE-LOG entry is expected --
see LOG-ENTRY-TEXT/LOG-ENTRY-COLOR below -- so existing call sites that
push bare strings keep working unchanged and simply render white."
  (text "" :read-only t)
  (color "white" :read-only t))

(defun log-entry-text (entry)
  "Return ENTRY's message text, whether ENTRY is a LOG-MESSAGE or a
plain string (backward-compatible: every existing MESSAGE-LOG push
site predates LOG-MESSAGE and still pushes bare strings)."
  (if (log-message-p entry) (log-message-text entry) entry))

(defun log-entry-color (entry)
  "Return ENTRY's display color, defaulting to \"white\" for plain-
string entries that never specified one."
  (if (log-message-p entry) (log-message-color entry) "white"))

(defun strip-log-repeat-suffix (text)
  "If TEXT ends with a repeat-count marker of the form \" (xN)\"
appended by ADD-LOG-MESSAGE, return two values: TEXT with that suffix
removed, and N (an integer >= 2). Otherwise return TEXT unchanged and
NIL, meaning \"no marker yet\" (an implicit repeat count of 1)."
  (let ((pos (search " (x" text :from-end t)))
    (if (and pos (> (length text) 0) (char= (char text (1- (length text))) #\)))
        (let* ((num-str (subseq text (+ pos 3) (1- (length text))))
               (count (and (every #'digit-char-p num-str) (plusp (length num-str))
                           (parse-integer num-str))))
          (if count
              (values (subseq text 0 pos) count)
              (values text nil)))
        (values text nil))))

(defun add-log-message (log text &optional (color "white"))
  "Return a new MESSAGE-LOG (newest-first) with TEXT/COLOR prepended
onto LOG. If LOG's current head entry has the exact same COLOR and the
exact same TEXT (ignoring any existing repeat-count marker), the two
messages are collapsed into a single entry whose text gets (or bumps)
a trailing \" (xN)\" marker instead of appending a duplicate line --
so a Troll hitting for the same damage twice in a row becomes \"...
(x2)\" rather than two identical lines cluttering the log."
  (let ((top (first log)))
    (if (and top (string= (log-entry-color top) color))
        (multiple-value-bind (base-text count) (strip-log-repeat-suffix (log-entry-text top))
          (if (string= base-text text)
              (cons (make-log-message (format nil "~A (x~D)" text (1+ (or count 1))) color)
                    (rest log))
              (cons (make-log-message text color) log)))
        (cons (make-log-message text color) log))))

(defun append-log-messages (log messages)
  "Return a new MESSAGE-LOG (newest-first) built by prepending each
entry of MESSAGES (itself newest-first, matching every existing call
site's convention -- e.g. MOVE-PLAYER's \"You have slain...\" ahead of
\"You hit...\") onto LOG one at a time via ADD-LOG-MESSAGE, oldest of
MESSAGES first so the final, newest entry is the one checked against
LOG's original head for repeat-collapsing. Entries of MESSAGES may be
plain strings (implicitly white, per LOG-ENTRY-TEXT/LOG-ENTRY-COLOR) or
LOG-MESSAGE structs."
  (reduce (lambda (acc entry)
            (add-log-message acc (log-entry-text entry) (log-entry-color entry)))
          (reverse messages)
          :initial-value log))

(defun build-combat-messages (name damage dies color hit-format no-damage-format
                               &optional slain-entry)
  "Return a MESSAGES list (newest-first, per APPEND-LOG-MESSAGES's
convention) describing the outcome of one combat blow involving an
entity named NAME, shared by both MOVE-PLAYER's player-attacks-monster
branch and PROCESS-ENEMY-TURNS' monster-attacks-player branch -- the
two near-identical \"no damage / hit / hit-and-died\" shapes each used
to build independently, duplicating the branching logic (see
TECHNICAL_DEBT.md item #37). DAMAGE is the damage dealt (0 meaning no
damage); DIES is whether the attack's target died as a result; COLOR
is the CSS color (see ENTITY-MESSAGE-COLOR) applied to the ordinary
hit/no-damage message. HIT-FORMAT and NO-DAMAGE-FORMAT are the
caller's own FORMAT control strings for the two ordinary outcomes --
HIT-FORMAT takes NAME's value(s) (see below) then ~D DAMAGE (e.g.
\"You hit the ~A for ~D damage!\" or \"The ~A hits you for ~D
damage!\"), NO-DAMAGE-FORMAT takes only NAME's value(s). NAME is
normally a single value (an entity name, or any other already-rendered
piece of text, e.g. PROCESS-ENEMY-TURNS' own attack-flavor sentence)
substituted for HIT-FORMAT/NO-DAMAGE-FORMAT's one ~A placeholder, but
may also be a list of values for a control string that needs more than
one ~A substitution before DAMAGE (e.g. CONFUSED-ENTITY-TURN's
attacker-stumbles-into-another-monster wording, which needs both the
attacker's and the target's name). Either way, every value NAME
denotes is always passed to FORMAT as an ordinary *argument* (via
APPLY), never spliced into the control string itself -- so an
arbitrary entity name or attack-flavor sentence can never be
misinterpreted as a FORMAT directive merely because it happens to
contain a literal `~` (see TECHNICAL_DEBT.md item #39; this callers-
never-build-control-strings-from-data discipline is the whole point of
this function's design, not just an accident of its current callers).
When DIES and DAMAGE is positive, SLAIN-ENTRY (a plain string or a
LOG-MESSAGE, already fully formatted by the caller -- e.g. a random
Monty-Python-style obituary line (see RANDOM-OBITUARY-MESSAGE) for the
player's own kill, or the plain white
\"You have died...\" for the player's own death) is prepended ahead of
the ordinary hit message, matching each caller's established
newest-first ordering."
  (let ((name-args (if (listp name) name (list name))))
    (cond
      ((zerop damage) (list (make-log-message (apply #'format nil no-damage-format name-args) color)))
      (dies (list slain-entry (make-log-message (apply #'format nil hit-format (append name-args (list damage))) color)))
      (t (list (make-log-message (apply #'format nil hit-format (append name-args (list damage))) color))))))

(defparameter *rdescent-status-effect-callout-color* "#bf616a"
  "CSS color STATUS-EFFECT-CALLOUT-TEXT's announcement messages are
displayed in -- the same red already used by \"*** Reincarnated! ***\"
(see RDESCENT/COMMANDS.LISP) for other noteworthy, attention-grabbing
state changes, so a freshly-inflicted debuff stands out from ordinary
combat/flavor text rather than blending into it.")

(defun item-break-messages (broken-item-names)
  "Return a MESSAGES list (newest-first, see APPEND-LOG-MESSAGES's own
convention) with one \"Your ~A breaks!\" entry (in *RDESCENT-STATUS-
EFFECT-CALLOUT-COLOR*, matching this codebase's other attention-
grabbing state-change callouts) per name in BROKEN-ITEM-NAMES, or NIL
if BROKEN-ITEM-NAMES is empty/NIL (the common case -- most hits don't
break anything). BROKEN-ITEM-NAMES is either a single item name or a
list of them, exactly as returned by RESOLVE-ATTACK/RESOLVE-ATTACK-
VOLLEY/RESOLVE-ATTACK-ON-PLAYER's own fourth value (see
APPLY-EQUIPMENT-WEAR, ENTITIES.LISP, for the durability rules that
produce these names). Every real combat call site appends this
function's result onto its own BUILD-COMBAT-MESSAGES output."
  (mapcar (lambda (name) (make-log-message (format nil "Your ~A breaks!" name)
                                            *rdescent-status-effect-callout-color*))
          (if (listp broken-item-names) (remove nil broken-item-names)
              (list broken-item-names))))

(defun monster-death-drop-messages (drop)
  "Return a MESSAGES list (newest-first, see APPEND-LOG-MESSAGES's own
convention) with one \"It drops a ~A!\" entry (in DROP's own
MESSAGE-COLOR) announcing DROP -- a freshly rolled Severance Package
GROUND-ITEM, see MAYBE-DROP-MONSTER-RSU/ENTITIES.LISP -- or NIL if DROP
is NIL (the common case, since a monster's RSU drop chance is
deliberately small). Every real \"a monster just died\" call site
(MOVE-PLAYER's melee kill, APPLY-ITEM's Scroll of PIP kill, CONFUSED-
ENTITY-TURN's stumble-kill, COMPANION-AI-TURN's own attack) prepends
this function's result ahead of its own BUILD-COMBAT-MESSAGES/
ITEM-BREAK-MESSAGES output, so the drop announcement reads as the
newest (most recent) event, after the kill itself."
  (when drop (list (make-log-message (format nil "It drops a ~A!" (get-name drop))
                                      (entity-message-color drop)))))


(defun status-effect-callout-text (kind)
  "Return a player-facing \"*** ... ***\" announcement string for a
STATUS-EFFECT KIND freshly attached to the player (see NEWLY-APPLIED-
EFFECT-KINDS below), or NIL for a KIND with no dedicated announcement
yet. Without this, a freshly-inflicted debuff would be invisible
beyond whatever attack-flavor sentence happened to cause it --
:ANALYSIS-PARALYSIS (lowers EFFECTIVE-DODGE-CHANCE, RDESCENT/
ENTITIES.LISP) and :DISTRACTED (delays the next ACCRUE-ENERGY tick,
see ADVANCE-ENTITY-TICK) in particular have *no* other visible symptom
at all until their consequence is actually consulted, unlike
:CONFUSED's own obviously-visible random staggering (see CONFUSED-
ENTITY-TURN) -- but all three get an explicit callout here for
consistency, so every freshly-attached debuff is announced the same
way regardless of how subtle its consequence is."
  (case kind
    (:confused "*** You feel Confused! ***")
    (:analysis-paralysis "*** You feel a wave of Analysis Paralysis coming on! ***")
    (:distracted "*** You feel Distracted! ***")
    (:carpal-tunnel "*** You feel Carpal Tunnel setting in! ***")
    (:bleed "*** You start Bleeding! ***")
    (:stunned "*** You are Stunned! ***")
    (t nil)))

(defun newly-applied-effect-kinds (before after)
  "Return the list of STATUS-EFFECT KINDs present in AFTER's own
ACTIVE-EFFECTS but absent from BEFORE's -- i.e. effects freshly
attached between BEFORE and AFTER (as opposed to one already active,
whose duration may simply have been refreshed/extended, or one that
just expired). Used by PROCESS-ENEMY-TURNS' attacking branch
(FUTURE_PLANS.md §7) to decide which STATUS-EFFECT-CALLOUT-TEXT
announcements, if any, to add to this turn's combat messages, by
comparing the player's own ACTIVE-EFFECTS immediately before and after
RESOLVE-ATTACK-ON-PLAYER/APPLY-STATUS-EFFECT ran -- this single
before/after diff naturally covers every way a STATUS-EFFECT can get
attached during an attack (a monster's own built-in RESOLVE-ATTACK
method, e.g. the Troll's 25% flat Confused chance, as well as a
flavor-pool entry's ON-HIT-EFFECT/ALWAYS-EFFECT) without each needing
its own separate bookkeeping."
  (let ((before-kinds (mapcar #'status-effect-kind (get-active-effects before))))
    (remove-if (lambda (k) (member k before-kinds)) (mapcar #'status-effect-kind (get-active-effects after)))))

(defun status-effect-callout-messages (before after)
  "Return a MESSAGES list (see BUILD-COMBAT-MESSAGES/APPEND-LOG-
MESSAGES's own newest-first convention) of STATUS-EFFECT-CALLOUT-TEXT
announcements for every KIND newly attached between BEFORE and AFTER
(see NEWLY-APPLIED-EFFECT-KINDS) that has a dedicated announcement --
NIL if there are none (either nothing new was attached, or none of
what was has a dedicated callout yet). Each is rendered in
*RDESCENT-STATUS-EFFECT-CALLOUT-COLOR*, as a MAKE-LOG-MESSAGE."
  (mapcar (lambda (text) (make-log-message text *rdescent-status-effect-callout-color*))
          (remove nil (mapcar #'status-effect-callout-text (newly-applied-effect-kinds before after)))))

(defun random-choice (choices)
  "Return a uniformly random element of CHOICES (a non-empty list).
Thin wrapper around (NTH (RANDOM (LENGTH CHOICES)) CHOICES) so callers
that pick among several flavor strings/generators (see
RANDOM-ATTACK-FLAVOR-TEXT and its callers) don't each repeat this
idiom."
  (nth (random (length choices)) choices))

(defparameter *rdescent-obituary-formats*
  '("The ~A has ceased to be!"
    "The ~A is deceased."
    "The ~A has met its maker!"
    "The ~A has shuffled off this mortal coil!"
    "The ~A is bereft of life!"
    "The ~A rests in peace!"
    "This is an ex-~A!"
    "The ~A has bought the farm!")
  "Pool of Monty Python \"Dead Parrot\" sketch-style obituary FORMAT
control strings RANDOM-OBITUARY-MESSAGE picks from to announce a
monster the player has personally slain, replacing what used to be a
single fixed \"You have slain the ~A!\" message -- each takes NAME's
one ~A substitution exactly like that old fixed string did.")

(defun random-obituary-message (name color)
  "Return a MAKE-LOG-MESSAGE in COLOR announcing that the entity named
NAME has died, its text chosen uniformly at random from
*RDESCENT-OBITUARY-FORMATS* (see RANDOM-CHOICE) -- used in place of a
single fixed \"You have slain the ~A!\" message wherever the player's
own action (melee, a targeted item, or an area-effect item) kills a
monster outright, so repeated kills don't all read identically."
  (make-log-message (format nil (random-choice *rdescent-obituary-formats*) name) color))

(defparameter *rdescent-dodge-phrases*
  '(("dodges out of the way!" . "dodge out of the way!")
    ("parries the blow!" . "parry the blow!")
    ("blocks the attack!" . "block the attack!")
    ("evades the strike!" . "evade the strike!")
    ("sidesteps the attack!" . "sidestep the attack!"))
  "Pool of (THIRD-PERSON . SECOND-PERSON) phrase pairs RANDOM-DODGE-
PHRASE picks from to announce a completely-missed attack (DAMAGE NIL
from RESOLVE-ATTACK-VOLLEY/RESOLVE-ATTACK-ON-PLAYER, i.e. the defender
avoided it outright, as opposed to a landed hit dealing 0 damage) --
replacing what used to be a single fixed \"dodges out of the way!\"/
\"dodge out of the way!\" ending, so repeated misses don't all read
identically. THIRD-PERSON is used when the defender is some other
entity (\"...but it ~A\"); SECOND-PERSON is used when the defender is
the player themselves (\"...but you ~A\").")

(defun random-dodge-phrase (perspective)
  "Return one phrase, chosen uniformly at random from *RDESCENT-DODGE-
PHRASES* (see RANDOM-CHOICE), already conjugated for PERSPECTIVE:
:THIRD (\"dodges out of the way!\", for some other entity avoiding an
attack) or :SECOND (\"dodge out of the way!\", for the player
avoiding one) -- the caller splices the result onto the end of its own
\"...but it/you ~A\" sentence via FORMAT's ~A, so the returned string
already ends in \"!\" and needs no further punctuation."
  (let ((chosen (random-choice *rdescent-dodge-phrases*)))
    (if (eq perspective :third) (car chosen) (cdr chosen))))

(defparameter *rdescent-hit-verb-phrases*
  '(("hits" . "hit")
    ("strikes" . "strike")
    ("kicks" . "kick")
    ("elbows" . "elbow")
    ("claws at" . "claw at")
    ("punches" . "punch")
    ("smacks" . "smack"))
  "Pool of (THIRD-PERSON . SECOND-PERSON) verb pairs RANDOM-HIT-VERB
picks from to describe a landed attack, so a combat log doesn't read
\"hit\"/\"hits\" for every single blow. THIRD-PERSON is the verb form
used when the attacker is some other entity (\"The ~A ~A you...\");
SECOND-PERSON is the base form used when the attacker is the player
themselves (\"You ~A the ~A...\"). Callers pick one verb per attack
(via RANDOM-HIT-VERB) and splice it into both their own hit and
no-damage FORMAT control strings (via CONCATENATE, not FORMAT itself,
so the verb's plain text can never be misread as a stray FORMAT
directive), so a single blow reads consistently whichever of the two
outcomes it turns out to be.")

(defun random-hit-verb (perspective)
  "Return one verb, chosen uniformly at random from *RDESCENT-HIT-
VERB-PHRASES* (see RANDOM-CHOICE), already conjugated for PERSPECTIVE:
:THIRD (\"hits\", \"strikes\", etc., for some other entity landing a
blow) or :SECOND (\"hit\", \"strike\", etc., for the player landing
one)."
  (let ((chosen (random-choice *rdescent-hit-verb-phrases*)))
    (if (eq perspective :third) (car chosen) (cdr chosen))))

(defstruct (mechanical-attack-flavor
            (:constructor make-mechanical-attack-flavor
                (text &key on-hit-effect always-effect force-no-damage)))
  "An attack-flavor pool entry (see ENTITY-ATTACK-FLAVOR-POOL) that,
unlike a plain string or function-designator entry, additionally
carries a real mechanical side effect (FUTURE_PLANS.md §7, \"Varied
Attack Effects (beyond flavor text)\") -- most pool entries remain
plain strings/functions with zero mechanical difference; only the
handful that need one are wrapped in this struct. TEXT is rendered
exactly like an ordinary entry (see RENDER-ATTACK-FLAVOR) -- a plain
string, or a function-designator called with no arguments to build one
on the fly. ON-HIT-EFFECT, if non-NIL, is a plist (:KIND K :TURNS N
:MAGNITUDE M :CHANCE P) describing a STATUS-EFFECT PROCESS-ENEMY-
TURNS' attacking branch attempts to inflict on the player via APPLY-
STATUS-EFFECT (same SENIORITY-DEFLECTION-CHANCE gate every other
inflicted effect goes through) -- but only when this attack actually
lands (DAMAGE > 0, not DIES), mirroring an EQUIPPABLE-ITEM's own
WEAPON-ON-HIT-EFFECT exactly (:TURNS -- really a TICKS-REMAINING count
in engine ticks, same key name as WEAPON-ON-HIT-EFFECT's own plist),
plus an additional CHANCE (0.0-1.0, default 1.0) roll on top, since a
flavor-specific effect is meant to be an occasional extra, not a
guaranteed one, unless a given entry overrides that by omitting
:CHANCE. ALWAYS-EFFECT, if non-NIL, is the same shape of plist but is
inflicted unconditionally, regardless of hit/dodge/damage -- paired
with FORCE-NO-DAMAGE T for a pure-annoyance attack that deals no
damage at all (e.g. the Troll's \"flags your Jira ticket\" flavor,
which never rolls RESOLVE-ATTACK-ON-PLAYER at all; see PROCESS-ENEMY-
TURNS' attacking branch)."
  text
  on-hit-effect
  always-effect
  force-no-damage)

(defun render-attack-flavor (flavor)
  "Return FLAVOR as a plain string: if FLAVOR is a MECHANICAL-ATTACK-
FLAVOR (see that struct), render its own TEXT field instead of FLAVOR
itself. Either way, if the resulting TEXT is a function (or
function-designator symbol), call it with no arguments and return its
result; otherwise (an already-complete string) return it unchanged.
See *RDESCENT-CODE-MONKEY-ATTACK-FLAVORS* (RDESCENT/ENEMIES/ORC.LISP)/
*RDESCENT-TROLL-ATTACK-FLAVORS* (RDESCENT/ENEMIES/TROLL.LISP), whose entries
mix all these forms."
  (let ((text (if (mechanical-attack-flavor-p flavor) (mechanical-attack-flavor-text flavor) flavor)))
    (if (or (functionp text) (and (symbolp text) (fboundp text)))
        (funcall text)
        text)))

(defgeneric entity-attack-flavor-pool (ent)
  (:documentation "Return the list of attack-flavor sentences/generators (see
*RDESCENT-CODE-MONKEY-ATTACK-FLAVORS*/*RDESCENT-TROLL-ATTACK-FLAVORS*,
in RDESCENT/ENEMIES/ORC.LISP and RDESCENT/ENEMIES/TROLL.LISP respectively) appropriate
for ENT's own class, or NIL if ENT isn't one of the currently-flavored
monster types (in which case PROCESS-ENEMY-TURNS falls back to its own
generic \"The ~A hits you...\" wording). Dispatches on ENT's class
(ORC/TROLL, see MAKE-ORC/MAKE-TROLL) rather than its NAME string, so a
monster's flavor pool is always tied to what it *is*, not merely how
it happens to be named. ORC/TROLL each add their own :METHOD in their
own file alongside their DEFCLASS.")
  (:method ((ent entity)) nil))

(defun random-attack-flavor-text (ent)
  "Return four values TEXT ON-HIT-EFFECT ALWAYS-EFFECT FORCE-NO-DAMAGE
for a fully-rendered, random attack-flavor pool entry (see ENTITY-
ATTACK-FLAVOR-POOL), or NIL NIL NIL NIL if ENT has no dedicated flavor
pool. TEXT is the rendered sentence (see RENDER-ATTACK-FLAVOR); the
remaining three values are only ever non-NIL for a chosen entry that
is a MECHANICAL-ATTACK-FLAVOR (FUTURE_PLANS.md §7) -- every existing
caller that only cares about TEXT (the common case) simply ignores
them, exactly as before. Called once per attack by PROCESS-ENEMY-TURNS
to pick fresh flavor text (and whatever mechanical effect goes with
it) each time, rather than a fixed message per monster type."
  (let ((pool (entity-attack-flavor-pool ent)))
    (if (null pool)
        (values nil nil nil nil)
        (let ((chosen (random-choice pool)))
          (values (render-attack-flavor chosen)
                  (and (mechanical-attack-flavor-p chosen) (mechanical-attack-flavor-on-hit-effect chosen))
                  (and (mechanical-attack-flavor-p chosen) (mechanical-attack-flavor-always-effect chosen))
                  (and (mechanical-attack-flavor-p chosen) (mechanical-attack-flavor-force-no-damage chosen)))))))


(defparameter *rdescent-message-log-max-length* 100
  "Hard cap on the number of entries GAME-STATE's MESSAGE-LOG may ever
hold. Without a cap, a client that stays connected indefinitely (the
message-log is per-client and never cleared, only ever grown by
MOVE-PLAYER's kick message and PROCESS-ENEMY-TURNS' placeholder
flavor text -- see RDESCENT-OUTBOUND-PACKETS' docstring for why it
isn't truncated on every read) would accumulate an unbounded linked
list for as long as the WebSocket connection stays open -- a slow but
real per-connection memory leak. Only the newest
*RDESCENT-MESSAGE-LOG-MAX-LENGTH* entries (MESSAGE-LOG is newest-
first) are ever kept; older entries are silently dropped by
UPDATE-GAME-STATE, well past the handful RDESCENT-OUTBOUND-PACKETS
actually renders to the client (5), so this is generous headroom, not
a user-visible truncation.")

(defun update-game-state (state &key (player (get-player state))
                                   (entities (get-entities state))
                                   (map (get-map state))
                                   (current-depth (get-current-depth state))
                                   (levels (get-levels state))
                                   (explored (get-explored state))
                                   (flags (get-flags state))
                                   (message-log (get-message-log state)))
  "Return a fresh instance of STATE's own class (GAME-STATE or a
subclass) with PLAYER/ENTITIES/MAP/CURRENT-DEPTH/LEVELS/EXPLORED/
FLAGS/MESSAGE-LOG copied from STATE except where overridden by the
corresponding keyword argument. The GAME-STATE analogue of
UPDATE-ENTITY: STATE itself is never mutated, so callers can always
treat both the argument and the return value as independent,
individually-valid snapshots. MESSAGE-LOG is capped to its newest
*RDESCENT-MESSAGE-LOG-MAX-LENGTH* entries here -- the one place every
caller that grows the log (MOVE-PLAYER, PROCESS-ENEMY-TURNS)
necessarily passes through -- so a long-lived client connection can
never accumulate an unbounded message-log list."
  (make-instance (class-of state)
                 :player player
                 :entities entities
                 :map map
                 :current-depth current-depth
                 :levels levels
                 :explored explored
                 :flags flags
                 :message-log (if (> (length message-log) *rdescent-message-log-max-length*)
                                  (subseq message-log 0 *rdescent-message-log-max-length*)
                                  message-log)))

(defun game-state-flag (state key &optional default)
  "Return the value of KEY in STATE's own FLAGS map (ARCHITECTURE_
PLAN.md §9), or DEFAULT (NIL unless supplied) if KEY has never been
set. Thin wrapper around FSET:LOOKUP, mirroring how USE-STAIRS/
ACTIONS.LISP already read GET-LEVELS by depth -- the read half of
SET-GAME-STATE-FLAG."
  (multiple-value-bind (value found-p) (fset:lookup (get-flags state) key)
    (if found-p value default)))

(defun set-game-state-flag (state key value)
  "Return a fresh GAME-STATE derived from STATE (via UPDATE-GAME-STATE)
with KEY set to VALUE in its own FLAGS map (ARCHITECTURE_PLAN.md §9),
via FSET:WITH -- STATE's own FLAGS map is never mutated, only replaced
by a fresh one sharing structure with the old. The write half of
GAME-STATE-FLAG."
  (update-game-state state :flags (fset:with (get-flags state) key value)))

;;; Keys & Locked Doors (FUTURE_PLANS.md §9)
;;;
;;; Both :KEYS-HELD and :DOORS-OPENED are plain lists (never an
;;; FSET:SET), matching how this codebase already stores other small
;;; per-player collections (INVENTORY, ACTIVE-EFFECTS) as plain lists
;;; rather than reaching for a persistent-set library for what's
;;; realistically always a handful of elements.

(defun keys-held (state)
  "Return STATE's own :KEYS-HELD GAME-STATE flag (ARCHITECTURE_PLAN.md
§9): a plain list of keyword key-ids (e.g.
*RDESCENT-CORPORATE-BADGE-KEY-ID*) GRAB-ITEM's own :KEY payload branch
has added, defaulting to NIL (no keys held) if never set."
  (game-state-flag state :keys-held nil))

(defun key-held-p (state key-id)
  "T if KEY-ID is a member of STATE's own KEYS-HELD list, OR if this
player has ever used The Root Password Post-It Note (STATE's own
:SKELETON-KEY-ACTIVE flag, FUTURE_PLANS.md §15 -- a permanent,
never-consumed \"master key\" that unlocks *every* locked door for the
rest of the run, regardless of KEY-ID), NIL otherwise. MOVE-PLAYER
consults this to decide whether a player may unlock a door tagged with
KEY-ID."
  (or (game-state-flag state :skeleton-key-active nil)
      (and (member key-id (keys-held state)) t)))

(defun add-key-held (state key-id)
  "Return a fresh GAME-STATE derived from STATE with KEY-ID added to
its own :KEYS-HELD flag (deduplicated via PUSHNEW/ADJOIN semantics --
picking up the same key twice, though it should never actually happen
since GRAB-ITEM removes the ground item once collected, still can't
grow the list). Called by GRAB-ITEM's own :KEY payload branch."
  (set-game-state-flag state :keys-held (adjoin key-id (keys-held state))))

(defun remove-key-held (state key-id)
  "Return a fresh GAME-STATE derived from STATE with KEY-ID removed
from its own :KEYS-HELD flag. Called by MOVE-PLAYER once a player
spends a key to unlock its matching door -- keys are single-use here
(FUTURE_PLANS.md §9), which is exactly why :DOORS-OPENED (see
DOOR-OPENED-P) must independently remember the door stays open
afterward: without it, a consumed key would make a previously-opened
door impassable again on a later visit -- a soft-lock bug this
function's own removal would otherwise invite."
  (set-game-state-flag state :keys-held (remove key-id (keys-held state))))

(defun doors-opened (state)
  "Return STATE's own :DOORS-OPENED GAME-STATE flag (ARCHITECTURE_
PLAN.md §9): a plain list of (LEVEL X Y) triples recording every
locked-door TILE this specific player has ever unlocked, defaulting to
NIL (no doors opened) if never set. Recording *every* door ever opened
(not just the current level's) is what lets a player revisit an
earlier level's door without it appearing locked again, even though
the underlying TILE itself (shared, cached dungeon geometry -- see
TILE's own docstring) never actually changes WALKABLE/CHAR -- avoiding
what would otherwise be a soft-lock bug for a one-shot, consumed key."
  (game-state-flag state :doors-opened nil))

(defun door-opened-p (state level x y)
  "T if this specific player (per STATE's own :DOORS-OPENED flag) has
already unlocked the locked door at (X, Y) on LEVEL, NIL otherwise.
MOVE-PLAYER/RENDER-GRID both consult this: once T, the door behaves
like -- and, in RENDER-GRID, is drawn like -- ordinary open floor for
this player, even though the shared TILE itself never changes."
  (and (member (list level x y) (doors-opened state) :test #'equal) t))

(defun add-door-opened (state level x y)
  "Return a fresh GAME-STATE derived from STATE with (LEVEL X Y) added
to its own :DOORS-OPENED flag. Called by MOVE-PLAYER once a player
spends their matching key to unlock a door."
  (set-game-state-flag state :doors-opened (adjoin (list level x y) (doors-opened state) :test #'equal)))

;;; NPCs & Quest Givers (FUTURE_PLANS.md §11)
;;;
;;; Quest *progress* is tracked entirely as per-player GAME-STATE flags
;;; (mirroring :KEYS-HELD/:DOORS-OPENED above), never as mutable state
;;; on the NPC-FIXTURE entity itself -- see NPC-FIXTURE's own
;;; docstring (ENTITIES.LISP) for why. Currently only the Disgruntled
;;; IT Guy's own kill-quest exists.

(defun it-guy-quest-accepted-p (state)
  "T if this player has accepted the Disgruntled IT Guy's quest (per
STATE's own :IT-GUY-QUEST-ACCEPTED-P flag), NIL otherwise (the
default: never offered/accepted yet). Set by ACCEPT-IT-GUY-QUEST."
  (game-state-flag state :it-guy-quest-accepted-p nil))

(defun it-guy-quest-kills (state)
  "Return this player's own :IT-GUY-QUEST-KILLS flag -- the number of
monster kills recorded (see RECORD-MONSTER-KILLS) since the Disgruntled
IT Guy's quest was last accepted -- defaulting to 0 if never set."
  (game-state-flag state :it-guy-quest-kills 0))

(defun it-guy-quest-reward-claimed-p (state)
  "T if this player has already collected the Disgruntled IT Guy's
quest reward (per STATE's own :IT-GUY-QUEST-REWARD-CLAIMED-P flag),
NIL otherwise. Once T, RECORD-MONSTER-KILLS stops incrementing
IT-GUY-QUEST-KILLS (further kills no longer matter -- the quest is
over) and INTERACT-WITH-FIXTURE's own NPC-FIXTURE method greets the
player without offering the quest again."
  (game-state-flag state :it-guy-quest-reward-claimed-p nil))

(defun it-guy-quest-complete-p (state)
  "T if this player has accepted the Disgruntled IT Guy's quest and
IT-GUY-QUEST-KILLS has reached *RDESCENT-IT-GUY-QUEST-KILL-TARGET*
(ENTITIES.LISP), NIL otherwise -- consulted by INTERACT-WITH-FIXTURE's
own NPC-FIXTURE method to decide whether talking to him pays out the
reward on this visit."
  (and (it-guy-quest-accepted-p state)
       (>= (it-guy-quest-kills state) *rdescent-it-guy-quest-kill-target*)))

(defun accept-it-guy-quest (state)
  "Return a fresh GAME-STATE derived from STATE with the Disgruntled
IT Guy's quest freshly accepted: :IT-GUY-QUEST-ACCEPTED-P set T and
:IT-GUY-QUEST-KILLS reset to 0 (so re-accepting after an earlier
completed-and-claimed run -- there is no way to do so today, since the
quest is one-shot per player, but this keeps the reset semantics
obviously correct if that ever changes -- always starts counting from
zero, never carrying over a stale count from before)."
  (set-game-state-flag (set-game-state-flag state :it-guy-quest-accepted-p t) :it-guy-quest-kills 0))

(defun claim-it-guy-quest-reward (state)
  "Return a fresh GAME-STATE derived from STATE with the Disgruntled
IT Guy's quest marked :IT-GUY-QUEST-REWARD-CLAIMED-P T, so
INTERACT-WITH-FIXTURE's own NPC-FIXTURE method never pays out the
reward RSU more than once for the same player."
  (set-game-state-flag state :it-guy-quest-reward-claimed-p t))

(defun yubikey-used-this-floor-p (state)
  "T if this player's §13 YubiKey death-save has already been spent on
the current floor, NIL otherwise. Stored as a numeric GAME-STATE flag
(:YUBIKEY-USED-THIS-FLOOR = 1 or 0) so it survives persistence's plain
data round trip without needing any special boolean-value encoding."
  (plusp (game-state-flag state :yubikey-used-this-floor 0)))

(defun mark-yubikey-used-this-floor (state)
  "Return a fresh GAME-STATE derived from STATE with the current
floor's YubiKey protection marked spent."
  (set-game-state-flag state :yubikey-used-this-floor 1))

(defun reset-yubikey-used-this-floor (state)
  "Return a fresh GAME-STATE derived from STATE with the current
floor's YubiKey protection reset to unused, *and* The \"Out of
Office\" Auto-Responder's own once-per-floor use (:OUT-OF-OFFICE-
USED-THIS-FLOOR, FUTURE_PLANS.md §15) reset alongside it -- both share
the exact same \"once per floor\" lifecycle (USE-STAIRS calls this on
every successful floor transition, fresh or revisited), so one shared
reset call is reused rather than threading a second near-identical
reset through USE-STAIRS' own 3 call sites."
  (set-game-state-flag
   (set-game-state-flag state :yubikey-used-this-floor 0)
   :out-of-office-used-this-floor 0))

(defun out-of-office-used-this-floor-p (state)
  "T if this player's §15 Out-of-Office Auto-Responder invisibility
trigger has already been spent on the current floor, NIL otherwise --
mirrors YUBIKEY-USED-THIS-FLOOR-P exactly, stored as its own numeric
GAME-STATE flag (:OUT-OF-OFFICE-USED-THIS-FLOOR = 1 or 0)."
  (plusp (game-state-flag state :out-of-office-used-this-floor 0)))

(defun mark-out-of-office-used-this-floor (state)
  "Return a fresh GAME-STATE derived from STATE with the current
floor's Out-of-Office Auto-Responder trigger marked spent."
  (set-game-state-flag state :out-of-office-used-this-floor 1))

(defun record-middle-manager-kills (state count)
  "Return a fresh GAME-STATE derived from STATE with the Disgruntled IT
Guy's own :IT-GUY-QUEST-KILLS flag incremented by COUNT (usually 1,
but an AREA-EFFECT-ITEM detonation can kill several MIDDLE-MANAGERs at
once) MIDDLE-MANAGER kills -- called by every combat-resolution call
site that can kill a monster (MOVE-PLAYER-INNER's bump-attack branch,
APPLY-ITEM's TARGETED-ITEM/AREA-EFFECT-ITEM methods, ACTIONS.LISP)
with COUNT already filtered down to only the MIDDLE-MANAGERs killed by
that action (see each call site's own TYPEP check), exactly once per
action, immediately alongside that same action's own XP grant, so
quest progress can never diverge from the player's already-tracked
kills. A COUNT of 0 (nothing MIDDLE-MANAGER died), no quest currently
accepted, or a quest whose reward has already been claimed
(IT-GUY-QUEST-REWARD-CLAIMED-P) all leave STATE's own FLAGS untouched
-- this exists as the one seam a future kill-quest's own progress
tracking would extend into, without requiring any change to a combat
call site's own logic beyond calling this function with the right
COUNT."
  (if (and (> count 0)
           (it-guy-quest-accepted-p state)
           (not (it-guy-quest-reward-claimed-p state)))
      (set-game-state-flag state :it-guy-quest-kills (+ count (it-guy-quest-kills state)))
      state))

(defun game-over-reason (state)
  "Return STATE's own :GAME-OVER-REASON flag (ARCHITECTURE_PLAN.md §9)
-- a keyword such as :VICTORY once some future win-condition feature
sets one, or NIL if the run has no terminal flag set yet (every
GAME-STATE today, since nothing yet writes this flag). Distinct from
the player's own IS-ALIVE: death is tracked on the ENTITY itself, but
a non-death ending (e.g. reaching the final floor and escaping) has no
ENTITY-level equivalent, hence a GAME-STATE-level flag instead. See
GAME-ACTIVE-P, which folds this together with IS-ALIVE into the one
predicate every command reducer's short-circuit actually consults."
  (game-state-flag state :game-over-reason))

(defun game-active-p (state)
  "Return true if STATE's game is still ongoing: STATE's own PLAYER is
IS-ALIVE *and* GAME-OVER-REASON is NIL. This is the generalized form of
the plain (IS-ALIVE (GET-PLAYER STATE)) check APPLY-RDESCENT-COMMAND/
APPLY-PLAYER-COMMAND/ADVANCE-GAME-STATE each used to short-circuit on
before ARCHITECTURE_PLAN.md §9 -- a dead player and a still-alive but
otherwise-concluded run (a future :VICTORY GAME-OVER-REASON, once some
win-condition feature sets one) should both stop the game the same
way: no further command or PROCESS-ENEMY-TURNS runs, and the returned
STATE is simply the frozen final snapshot. Every existing caller of
IS-ALIVE (GET-PLAYER STATE) as a game-over check should consult this
function instead, so a future non-death ending automatically gets the
exact same short-circuit treatment death already does, with no
per-caller edits needed when that feature lands."
  (and (is-alive (get-player state)) (not (game-over-reason state))))

(defun accrue-energy (ent)
  "Return a fresh copy of ENT (via UPDATE-ENTITY) with its ENERGY
increased by its own ENTITY-SPEED, capped at
*RDESCENT-MAX-BANKED-ENERGY* so an entity that goes unseen/idle for a
long stretch (see PROCESS-ENEMY-TURNS' FOV gating) can never stockpile
more than a single action's worth of ENERGY -- without this cap, a
long-hidden monster would otherwise burst through a whole backlog of
banked actions the instant it came back into play (one per tick until
drained) rather than acting at its own steady ENTITY-SPEED pace. Pure
per-entity helper factored out of REDUCE-TICK so the \"one tick's
worth of energy income\" rule lives in exactly one place regardless of
whether it is being applied to the player or to an ENTITIES-list
monster."
  (update-entity ent :energy (min *rdescent-max-banked-energy*
                                 (+ (entity-energy ent) (entity-speed ent)))))

(defun advance-entity-tick (ent)
  "Return a fresh copy of ENT describing one game tick's worth of
passive advancement -- REDUCE-TICK's own per-entity step, factored out
so it can be shared between STATE's PLAYER (which additionally gets
ACCRUE-HEALING layered on top by REDUCE-TICK itself) and every
ordinary monster in ENTITIES. Ordinarily this is simply (ACCRUE-ENERGY
(TICK-STATUS-EFFECTS ENT)) -- but if ENT currently has an active
:DISTRACTED STATUS-EFFECT (see ENTITY-EFFECT), inflicted by, e.g., the
Troll's \"flags your Jira ticket\" attack flavor (FUTURE_PLANS.md §7,
\"Varied Attack Effects\" -- see RDESCENT/ENEMIES/TROLL.LISP), this
tick's ACCRUE-ENERGY is skipped entirely -- \"delays the player's next
Energy tick\", exactly as promised -- while TICK-STATUS-EFFECTS itself
still always runs, so :DISTRACTED's own countdown (and every other
active effect's) keeps advancing regardless. The :DISTRACTED check is
made against ENT *before* TICK-STATUS-EFFECTS runs (not the
already-ticked result), so a freshly-inflicted :DISTRACTED effect
(see *RDESCENT-DISTRACTION-TICKS*) reliably blocks exactly one
ACCRUE-ENERGY call -- the very next REDUCE-TICK after it was
inflicted -- before TICK-STATUS-EFFECTS drops it."
  (if (entity-effect ent :distracted)
      (tick-status-effects ent)
      (accrue-energy (tick-status-effects ent))))

(defun tick-status-effect (effect)
  "Return a fresh STATUS-EFFECT with EFFECT's TICKS-REMAINING
decremented by 1, preserving its KIND/MAGNITUDE/EXPIRE-INTO -- or NIL
if EFFECT has just expired (its TICKS-REMAINING was already 1,
decrementing to 0; see TICK-STATUS-EFFECTS, which is responsible for
noticing EFFECT's own EXPIRE-INTO in that case and chaining a fresh
STATUS-EFFECT in its place). Pure; EFFECT itself is never mutated.
Called once per tick, per attached effect, by TICK-STATUS-EFFECTS."
  (let ((remaining (1- (status-effect-ticks-remaining effect))))
    (and (plusp remaining)
         (make-instance 'status-effect :kind (status-effect-kind effect)
                                       :ticks-remaining remaining
                                       :magnitude (status-effect-magnitude effect)
                                       :expire-into (status-effect-expire-into effect)))))

(defun tick-status-effects (ent)
  "Return a fresh copy of ENT (via UPDATE-ENTITY) with every one of
its ACTIVE-EFFECTS advanced by one tick (see TICK-STATUS-EFFECT):
each effect's TICKS-REMAINING is decremented by 1 and any that expire
this tick are dropped from the result -- unless the just-expired
effect has its own EXPIRE-INTO (see STATUS-EFFECT), in which case a
fresh STATUS-EFFECT built from that plist is chained on in its place
(via REPLACE-EFFECT-IN-LIST) instead of simply vanishing -- e.g. §17's
Quadruple Shot Espresso's temporary :CAFFEINATED buff chaining into a
one-tick :DISTRACTED \"crash\" the moment it wears off. Before that, if
ENT is currently alive and has an HP to affect, every attached
effect's non-NIL MAGNITUDE (see STATUS-EFFECT) is applied once as a
straight HP delta -- negative to drain (e.g. §17's Food Poisoning
debuff), positive to regenerate -- summed across every such effect,
clamped at MAX-HP, and, if the total brings HP to or below 0, ENT is
marked dead exactly like ordinary combat damage (CHAR #\\%, IS-ALIVE
NIL, BLOCKS-MOVEMENT NIL, RENDER-ORDER 0): a per-tick status effect
can kill, same as an attack. This is the single hook a future per-tick
debuff/buff plugs into (see ARCHITECTURE_PLAN.md §1), rather than a
new hardcoded branch in REDUCE-TICK itself, which calls this once per
tick for every entity, alongside ACCRUE-ENERGY/ACCRUE-HEALING. If ENT
has no ACTIVE-EFFECTS at all, ENT is returned completely unchanged
(no pointless UPDATE-ENTITY round-trip). Pure; ENT itself is never
mutated."
  (let ((active (get-active-effects ent)))
    (if (null active)
        ent
        (let* ((current-hp (hp ent))
               (drainable (and (is-alive ent) current-hp))
               (new-hp (if drainable
                           (reduce (lambda (h effect)
                                     (if (status-effect-magnitude effect)
                                         (min (or (max-hp ent) h) (+ h (status-effect-magnitude effect)))
                                         h))
                                   active :initial-value current-hp)
                           current-hp))
               (dies (and drainable (<= new-hp 0)))
               (expiring-chains (loop for effect in active
                                      when (and (not (plusp (1- (status-effect-ticks-remaining effect))))
                                                (status-effect-expire-into effect))
                                      collect (status-effect-expire-into effect)))
               (new-effects (reduce (lambda (effects chain)
                                      (replace-effect-in-list effects (getf chain :kind)
                                                              (getf chain :ticks-remaining)
                                                              (getf chain :magnitude)
                                                              (getf chain :expire-into)))
                                    expiring-chains
                                    :initial-value (remove nil (mapcar #'tick-status-effect active)))))
          (cond
            (dies (update-entity ent :active-effects new-effects :hp (max 0 new-hp)
                                 :char #\% :is-alive nil :blocks-movement nil :render-order 0))
            ((and drainable (/= new-hp current-hp)) (update-entity ent :active-effects new-effects :hp new-hp))
            (t (update-entity ent :active-effects new-effects)))))))

(defun accrue-healing (ent)
  "Return a fresh copy of ENT (via UPDATE-ENTITY) with one tick's
worth of natural HP regeneration applied: if ENT is already dead
(IS-ALIVE NIL) or already at full health (HP = MAX-HP), ENT is
returned unchanged (HEAL-PROGRESS included -- no point accumulating
progress a living, undamaged entity will never spend). Otherwise
HEAL-PROGRESS is incremented by 1, and once it reaches
*RDESCENT-HEAL-TICKS* HP is increased by 1 (never past MAX-HP) and
HEAL-PROGRESS resets to 0, so healing happens in discrete 1-HP steps
spaced *RDESCENT-HEAL-TICKS* ticks apart (10 real seconds at the
current tick rate) rather than fractionally every tick. Pure
per-entity helper factored out of REDUCE-TICK, mirroring
ACCRUE-ENERGY, though currently only ever applied to the player (see
REDUCE-TICK) -- monsters don't yet regenerate."
  (if (or (not (is-alive ent)) (null (hp ent)) (null (max-hp ent)) (>= (hp ent) (max-hp ent)))
      ent
      (let ((progress (1+ (entity-heal-progress ent))))
        (if (>= progress *rdescent-heal-ticks*)
            (update-entity ent :hp (min (max-hp ent) (1+ (hp ent))) :heal-progress 0)
            (update-entity ent :heal-progress progress)))))

(defun reduce-tick (state)
  "Return a fresh GAME-STATE derived from STATE in which every entity
-- STATE's PLAYER as well as every monster in (GET-ENTITIES STATE) --
has had its own ACTIVE-EFFECTS advanced by one tick and its ENERGY
increased by its own ENTITY-SPEED, unless a :DISTRACTED STATUS-EFFECT
says otherwise this tick (see ADVANCE-ENTITY-TICK), with no other
change to STATE, and in which STATE's PLAYER has additionally had one
tick's worth of natural HP regeneration applied (see ACCRUE-HEALING):
1 HP is restored every *RDESCENT-HEAL-TICKS* ticks (10 real seconds)
until the player is back at full MAX-HP, and progress toward the next
point resets once spent. This is the reducer for the periodic :TICK
event (see RDESCENT-TICK-EVENTS/START-GAME-LOOP in RDESCENT/
SERVER.LISP): every *RDESCENT-TICK-SECONDS*, each entity's
action-point balance grows by its own fixed SPEED, independent of
whether the player (or any monster) actually acts that tick -- it is
MOVE-PLAYER's and PROCESS-ENEMY-TURNS' job to gate actions on (and
deduct from) that accumulated ENERGY, not this function's. Pure: a
fresh GAME-STATE (via UPDATE-GAME-STATE) is always returned, built
from freshly ADVANCE-ENTITY-TICK'd (plus ACCRUE-HEALING'd, for the
player) PLAYER and ADVANCE-ENTITY-TICK'd ENTITIES; STATE itself is
never mutated."
  (update-game-state state
                     :player (accrue-healing (advance-entity-tick (get-player state)))
                     :entities (mapcar #'advance-entity-tick (get-entities state))))

(defun map-tile-ref (map x y)
  "Return the TILE at (X, Y) in MAP, or NIL if (X, Y) is outside MAP's
bounds. Pure: MAP's TILES array is never written to after MAKE-INITIAL-MAP
constructs it."
  (let ((tiles (get-tiles map)))
    (destructuring-bind (height width) (array-dimensions tiles)
      (when (and (<= 0 x (1- width)) (<= 0 y (1- height)))
        (aref tiles y x)))))

(defparameter *rdescent-astar-max-expansions* 4000
  "Safety cap on the number of nodes ASTAR-NEXT-STEP will expand while
searching for a path before giving up and reporting no path found --
bounds the worst-case cost of a single AI turn's pathfinding call
(e.g. a monster whose goal is entirely walled off, or unreachable
across the whole level, which would otherwise force the search to
flood every remaining reachable tile) to a small, fixed amount of
work. Sized to comfortably exceed *RDESCENT-FIELD-WIDTH* *
*RDESCENT-FIELD-HEIGHT* (100 * 33 = 3300) -- the total number of
cells in the entire playing field -- rather than merely the distance
a hostile monster's own chase (bounded by *RDESCENT-MONSTER-FOV-RADIUS*,
6) could require: unlike a hostile monster's approach, a bonded Office
Doge COMPANION's own \"follow the player\" goal in COMPANION-AI-TURN
is deliberately *not* gated by FOV/distance at all (see
PROCESS-ENEMY-TURNS' own ACTING-ENTITIES filter, which always includes
a COMPANION-P entity regardless of visibility), so if Doge ever falls
far behind the player -- e.g. after finishing off a fight elsewhere on
the level -- its path back can legitimately span a large fraction of
the whole map. A cap sized only for a short FOV-bounded chase (the
previous value, 400) could be exhausted by such a long-but-real path,
making ASTAR-NEXT-STEP wrongly report no path at all -- and since the
distance to the player never shrinks while Doge does nothing, this
would leave it permanently frozen, unable to ever catch up. Even at
this larger size, a single search remains cheap: this map has at most
3300 reachable cells, so a full flood in the worst case is still a
small, fixed amount of work for a turn-based game.")

(defun astar-tie-breaker (from-x from-y to-x to-y nx ny)
  "Return a non-negative integer scoring how far off the straight line
from (FROM-X, FROM-Y) to (TO-X, TO-Y) the candidate cell (NX, NY) sits
(Amit Patel's cross-product tie-break; see
https://www.redblobgames.com/pathfinding/a-star/implementation.html#tie-breaking).
ASTAR-NEXT-STEP adds this (scaled down far below the weight of one
real step) into every neighbor's F-SCORE so that whenever two or more
routes are truly equal-cost, the one hugging closest to the direct
line toward the goal wins consistently. This score depends only on
FROM/TO/NX/NY's static geometry -- never on OPEN's incidental
discovery order, which real-cost ties would otherwise leave to settle
arbitrarily -- so the same equal-cost route is chosen turn after turn
instead of flip-flopping between equally-good alternatives as other
entities elsewhere on the map perturb the search's exploration order
from one turn to the next."
  (let ((dx1 (- nx to-x)) (dy1 (- ny to-y))
        (dx2 (- from-x to-x)) (dy2 (- from-y to-y)))
    (abs (- (* dx1 dy2) (* dx2 dy1)))))

(defun astar-next-step (map from-x from-y to-x to-y &key blocked-p)
  "Return two values (STEP-DX STEP-DY), each in {-1,0,1} (never both
0), describing the single 8-directional step an entity standing at
(FROM-X, FROM-Y) on MAP should take along the shortest walkable path
toward (TO-X, TO-Y), computed via A* search. This replaces the old
greedy single-step heuristic PROCESS-ENEMY-TURNS/COMPANION-AI-TURN
used to compute directly as (SIGNUM DX)/(SIGNUM DY): that heuristic
had no way to route *around* an obstacle sitting directly on the
straight-line diagonal toward its target, so a monster standing
diagonally across an L-shaped corridor bend or a doorway from its
goal could get permanently wedged against the wall corner -- every
turn recomputing the exact same blocked diagonal step -- even though
a walkable route existed by first stepping orthogonally to either
side. A* fixes this by actually searching MAP's walkable tiles for
the true shortest path rather than assuming the straight line toward
the goal is ever walkable.

Returns (VALUES NIL NIL) if FROM and TO already name the same cell,
or if no walkable path from FROM to TO exists within
*RDESCENT-ASTAR-MAX-EXPANSIONS* node expansions (including if TO
itself turns out to be unreachable at all, e.g. sealed behind a
locked door with no key) -- callers should treat that exactly like
the old code's own \"blocked this turn, stay put\" fallback.

Nodes are (X . Y) conses. The heuristic is Chebyshev distance
(MAX (ABS DX) (ABS DY)), admissible and consistent for this graph's
uniform per-step cost of 1 in any of the 8 directions (matching every
existing single-step AI branch's own implicit \"one step, one turn,
regardless of direction\" cost model -- diagonal moves are never
charged extra here, mirroring the old greedy code they replace).
BLOCKED-P, if supplied, is called as (FUNCALL BLOCKED-P X Y) for each
candidate neighbor cell already confirmed walkable (per MAP-TILE-REF/
GET-WALKABLE) and should return true to additionally treat that cell
as impassable for this search -- callers use this to keep the search
from routing through cells currently occupied by other entities,
without MAP itself needing to know anything about ENTITIES. (TO-X,
TO-Y) itself is never excluded via BLOCKED-P even if BLOCKED-P would
otherwise say so, since the goal is typically the target entity's own
occupied square -- callers only want the first step *towards* it, not
a path that actually crosses it.

This function does no corner-cutting restriction beyond the old code's
own (a diagonal step is allowed whenever its own destination tile is
walkable, regardless of whether the two tiles flanking that diagonal
are walls) -- deliberately unchanged from prior behavior, since
fixing corner-cutting was not this function's purpose and would be an
unrelated, separately-decided rule change."
  (if (and (= from-x to-x) (= from-y to-y))
      (values nil nil)
      (let* ((start (cons from-x from-y))
             (goal (cons to-x to-y))
             (open (list start))
             (came-from (make-hash-table :test 'equal))
             (g-score (make-hash-table :test 'equal))
             (f-score (make-hash-table :test 'equal))
             (expansions 0)
             ;; TIE-BREAK-SCALE inflates every real G/H unit so the tiny
             ;; cross-product tie-breaker (ASTAR-TIE-BREAKER, bounded by
             ;; roughly field-width * field-height) added below can never
             ;; overturn a genuine cost difference of even 1 step -- it
             ;; only decides among otherwise-equal-cost candidates.
             (tie-break-scale (* 4 (max *rdescent-field-width* *rdescent-field-height*)
                                 (max *rdescent-field-width* *rdescent-field-height*))))
        (setf (gethash start g-score) 0)
        (setf (gethash start f-score) (* tie-break-scale (max (abs (- from-x to-x)) (abs (- from-y to-y)))))
        (loop
          (when (null open) (return (values nil nil)))
          (when (>= expansions *rdescent-astar-max-expansions*) (return (values nil nil)))
          (incf expansions)
          (let ((current (let ((best (first open)) (best-f (gethash (first open) f-score)))
                           (dolist (node (rest open) best)
                             (let ((f (gethash node f-score)))
                               (when (< f best-f) (setf best node best-f f)))))))
            (if (equal current goal)
                (let ((node goal) (step nil))
                  (loop while (not (equal node start))
                        do (setf step node)
                           (setf node (gethash node came-from)))
                  (return (values (- (car step) from-x) (- (cdr step) from-y))))
                (progn
                  (setf open (delete current open :test #'equal :count 1))
                  (loop for ddy from -1 to 1
                        do (loop for ddx from -1 to 1
                                 unless (and (zerop ddx) (zerop ddy))
                                 do (let* ((nx (+ (car current) ddx))
                                           (ny (+ (cdr current) ddy))
                                           (neighbor (cons nx ny))
                                           (is-goal (equal neighbor goal))
                                           (tile (map-tile-ref map nx ny)))
                                      (when (and tile (get-walkable tile)
                                                 (or is-goal (not (and blocked-p (funcall blocked-p nx ny)))))
                                        (let ((tentative-g (1+ (gethash current g-score))))
                                          (when (< tentative-g (gethash neighbor g-score most-positive-fixnum))
                                            (setf (gethash neighbor came-from) current)
                                            (setf (gethash neighbor g-score) tentative-g)
                                            (setf (gethash neighbor f-score)
                                                  (+ (* tie-break-scale
                                                        (+ tentative-g (max (abs (- nx to-x)) (abs (- ny to-y)))))
                                                     (astar-tie-breaker from-x from-y (car goal) (cdr goal) nx ny)))
                                            (pushnew neighbor open :test #'equal))))))))))))))

(defparameter *rdescent-flow-field-radius* (+ *rdescent-monster-fov-radius* 2)
  "Chebyshev-distance radius PLAYER-FLOW-FIELD floods out to around its
own source cell (always the player's current position -- see
PROCESS-ENEMY-TURNS). Sized a couple of cells past *RDESCENT-MONSTER-
FOV-RADIUS* (the radius PROCESS-ENEMY-TURNS' own ACTING-ENTITIES
filter already requires a hostile to be within, to act at all this
tick) so every entity that could possibly consult the field this tick
is guaranteed to have an entry for its own current cell, with a little
slack besides for routing a short detour around a nearby wall corner
without running off the flooded region's edge. Keeping this bounded
(rather than flooding the entire, possibly 100x33, map) is what makes
computing the field cheap enough to do fresh every single tick: at
most (1+ (* 2 RADIUS)) squared cells are ever visited, a small, fixed
amount of work independent of the map's real size.")

(defun player-flow-field (map from-x from-y &key (radius *rdescent-flow-field-radius*))
  "Return an EQL hash table mapping XY-TO-INDEX's flat cell index to
the minimum number of 8-directional walkable steps from (FROM-X,
FROM-Y) -- in practice always the player's own current position, see
PROCESS-ENEMY-TURNS -- to that cell, for every walkable cell within
Chebyshev RADIUS of (FROM-X, FROM-Y): a single flood-fill BFS (a
Dijkstra map/flow field, since every step costs the same 1 regardless
of direction, exactly like ASTAR-NEXT-STEP's own cost model). A cell
outside RADIUS, unreachable from FROM-X/FROM-Y within it, or simply
not walkable, has no entry at all -- callers should treat a missing
entry as \"infinitely far / no path\", mirroring ASTAR-NEXT-STEP's own
(VALUES NIL NIL) \"no path found\" contract.

This exists to replace PROCESS-ENEMY-TURNS' old approach of calling
ASTAR-NEXT-STEP once per pursuing hostile monster, every tick -- fine
for one monster, but a full independent A* search apiece means a room
of, say, 15 monsters all converging on the player runs 15 separate
searches every tick, each potentially expanding hundreds of nodes.
Real-time strategy games and roguelikes alike solve this with exactly
this technique: flood outward from the *target* once, then every
pursuer's own next step is just \"look at my up-to-8 neighbors, walk
towards whichever has the smallest distance-to-target value\" --
O(1) per monster afterward, however many there are, since they all
share this one, already-computed field; see FLOW-FIELD-NEXT-STEP.

Deliberately does NOT know anything about ENTITIES occupying cells
(unlike ASTAR-NEXT-STEP's own BLOCKED-P) -- it floods purely over
MAP's static walls, since this same field is shared read-only across
every monster acting this tick, and different monsters can have
different other-entities-in-the-way constraints (e.g. one monster
standing where another wants to step). FLOW-FIELD-NEXT-STEP is what
each monster's own turn calls to fold its own BLOCKED-P check in on
top of this shared field, exactly mirroring how ASTAR-NEXT-STEP's
BLOCKED-P callback already worked."
  (let* ((side (1+ (* 2 radius)))
         (queue (make-array (* side side) :fill-pointer 0))
         (distances (make-hash-table :test 'eql))
         (head 0))
    (setf (gethash (xy-to-index from-x from-y) distances) 0)
    (vector-push (cons from-x from-y) queue)
    (loop while (< head (fill-pointer queue))
          do (let* ((current (aref queue head))
                    (cx (car current)) (cy (cdr current))
                    (d (gethash (xy-to-index cx cy) distances)))
               (incf head)
               (loop for ddy from -1 to 1
                     do (loop for ddx from -1 to 1
                              unless (and (zerop ddx) (zerop ddy))
                              do (let ((nx (+ cx ddx)) (ny (+ cy ddy)))
                                   (when (and (<= (max (abs (- nx from-x)) (abs (- ny from-y))) radius)
                                              (not (gethash (xy-to-index nx ny) distances))
                                              (let ((tile (map-tile-ref map nx ny)))
                                                (and tile (get-walkable tile))))
                                     (setf (gethash (xy-to-index nx ny) distances) (1+ d))
                                     (vector-push (cons nx ny) queue)))))))
    distances))

(defun flow-field-next-step (map flow-field from-x from-y to-x to-y &key blocked-p)
  "Return two values (STEP-DX STEP-DY), each in {-1,0,1} (never both
0), describing the single 8-directional step an entity standing at
(FROM-X, FROM-Y) on MAP should take to make progress towards (TO-X,
TO-Y), using FLOW-FIELD (a PLAYER-FLOW-FIELD built with source
(TO-X, TO-Y)) instead of running its own independent search --
see PLAYER-FLOW-FIELD's own docstring for why. Chooses, among
(FROM-X, FROM-Y)'s up to 8 walkable, non-BLOCKED-P neighbors (TO-X,
TO-Y itself always exempted from BLOCKED-P, exactly like ASTAR-
NEXT-STEP), whichever has the smallest FLOW-FIELD distance strictly
less than (FROM-X, FROM-Y)'s own -- ties broken via ASTAR-TIE-BREAKER
so the same equal-cost route is chosen consistently turn after turn
rather than flip-flopping with FLOW-FIELD's own hash-table iteration
order.

Returns (VALUES NIL NIL) if FROM and TO already name the same cell, if
FROM-X/FROM-Y itself has no FLOW-FIELD entry (outside FLOW-FIELD's own
flooded radius, or genuinely unreachable from the source it was built
from), or if no qualifying neighbor exists -- e.g. every neighbor
closer to the goal happens to be occupied by another entity right now.
Callers should treat this exactly like ASTAR-NEXT-STEP's own \"blocked
this turn, stay put\" fallback."
  (block flow-field-next-step
    (when (and (= from-x to-x) (= from-y to-y))
      (return-from flow-field-next-step (values nil nil)))
    (let ((current-distance (gethash (xy-to-index from-x from-y) flow-field)))
      (when (null current-distance)
        (return-from flow-field-next-step (values nil nil)))
      (let ((best-dx nil) (best-dy nil) (best-distance nil) (best-tie nil))
        (loop for ddy from -1 to 1
              do (loop for ddx from -1 to 1
                       unless (and (zerop ddx) (zerop ddy))
                       do (let* ((nx (+ from-x ddx))
                                 (ny (+ from-y ddy))
                                 (is-goal (and (= nx to-x) (= ny to-y)))
                                 (tile (map-tile-ref map nx ny)))
                            (when (and tile (get-walkable tile)
                                       (or is-goal (not (and blocked-p (funcall blocked-p nx ny)))))
                              (let ((d (gethash (xy-to-index nx ny) flow-field)))
                                (when (and d (< d current-distance))
                                  (let ((tie (astar-tie-breaker from-x from-y to-x to-y nx ny)))
                                    (when (or (null best-distance)
                                              (< d best-distance)
                                              (and (= d best-distance) (< tie best-tie)))
                                      (setf best-dx ddx best-dy ddy best-distance d best-tie tie)))))))))
        (return-from flow-field-next-step (values best-dx best-dy))))))

(defun room-acoustics (room-kind)
  "Return :MUFFLED or :LOUD for ROOM-KIND (a TILE's GET-ROOM-KIND value
-- :CUBICLE, :OPEN-OFFICE, :SERVER-ROOM, or NIL), per
ARCHITECTURE_PLAN.md §6/§18: :CUBICLE and :SERVER-ROOM (dense walls,
partitions, humming equipment) are :MUFFLED, as is NIL (a corridor
tile -- narrow, off-the-record hallway chatter doesn't carry); only
:OPEN-OFFICE (no walls, no partitions -- a single unbroken floor plan)
is :LOUD. This is the one predicate the future §18 Open-Office Stealth
Penalty needs -- consulted wherever a \"does this action alert nearby
enemies\" check would go -- a new concern with no existing call site
yet; nothing in today's engine calls ROOM-ACOUSTICS."
  (if (eq room-kind :open-office) :loud :muffled))

(defun make-initial-map (&key (width *rdescent-field-width*)
                           (height *rdescent-field-height*))
  "Pure factory: return a fresh GAME-MAP instance, a WIDTH x HEIGHT
grid of floor TILEs (walkable, #\\.), with a small rectangular block of
wall TILEs (not walkable, #\\#) a few rows below center for collision
testing (kept off MAKE-INITIAL-STATE's exact-center player spawn point
so a new player never starts on a wall). Every cell is its own
freshly-made TILE instance -- no sharing -- even though TILEs are
immutable, simply so each cell is visibly independent of its
neighbors.
NOTE: not called anywhere in production code -- production dungeons
are always built via GENERATE-DUNGEON's procedural room/tunnel
generator instead. This is a test-only helper (tests/tests.lisp) for
building simple, deterministic all-floor-plus-one-wall-block maps to
exercise movement/collision/FOV logic in isolation from procedural
generation."
  (let ((tiles (make-array (list height width))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref tiles y x) (make-instance 'tile :walkable t :char #\.))))
    ;; A small wall block for collision testing, offset from the exact
    ;; center so it never overlaps MAKE-INITIAL-STATE's player spawn
    ;; point (which sits at the field's exact center) -- players must
    ;; always begin on walkable ground.
    (let* ((wall-width 4)
           (wall-height 2)
           (wall-left (- (floor width 2) (floor wall-width 2)))
           (wall-top (min (- height wall-height)
                          (+ (floor height 2) 3))))
      (dotimes (dy wall-height)
        (dotimes (dx wall-width)
          (setf (aref tiles (+ wall-top dy) (+ wall-left dx))
                (make-instance 'tile :walkable nil :char #\#)))))
    (make-instance 'game-map :tiles tiles)))

(defparameter *rdescent-corporate-stat-accessors*
  (list (cons "bandwidth" #'get-bandwidth)
        (cons "pivot" #'get-pivot)
        (cons "caffeine-tolerance" #'get-caffeine-tolerance)
        (cons "domain-knowledge" #'get-domain-knowledge)
        (cons "seniority" #'get-seniority)
        (cons "synergy" #'get-synergy)
        (cons "hygiene" #'get-hygiene))
  "The single canonical list of the player's seven Corporate RPG
Stats (see ENTITY's docstring), each entry a (NAME . ACCESSOR) pair
where NAME is the stat's lowercase kebab-case name (matching both the
ENTITY slot name and the \"val-<NAME>\" DOM element ID convention
used by /js/rdescent.js -- see RENDER-RDESCENT-PAGE's
#player-corporate-stats markup in views.lisp) and ACCESSOR is the
corresponding GET-<NAME> reader function. RDESCENT-PLAYER-STATS-
PACKET (rdescent/server.lisp) iterates this list once to build its
\"val-<NAME>\" JSON fields instead of hand-writing one CONS per stat,
so adding, removing, or renaming a Corporate RPG Stat only requires
updating this one list (plus the ENTITY slot itself and the
client-side JS/HTML markup) rather than also hunting down a parallel
hand-written CONS clause in the packet-building code (see
TECHNICAL_DEBT.md item #41).")

(defun roll-stat ()
  "Return a single \"Corporate RPG Stat\" value via the classic
tabletop-RPG 4d6-drop-lowest method: roll four 6-sided dice (RANDOM 6
returns 0-5, so 1+ shifts each roll into the 1-6 range), sort them
ascending, drop the lowest (CDR after SORT), and sum the remaining
three highest -- yielding a value between 3 (three 1s) and 18 (three
6s), with a bell-curve bias toward the upper-middle of that range
(unlike a flat 3d6 sum) because the single dropped die is always the
worst of the four. Used once per stat, per player, by MAKE-INITIAL-
STATE to roll BANDWIDTH/PIVOT/CAFFEINE-TOLERANCE/DOMAIN-KNOWLEDGE/
SENIORITY/SYNERGY/HYGIENE (see ENTITY's docstring) -- each call is
independent, so a player's seven stats are not correlated with one
another."
  (let ((rolls (mapcar #'1+ (list (random 6) (random 6) (random 6) (random 6)))))
    (fold-left #'+ 0 (cdr (sort rolls #'<)))))

(defun make-initial-state (&optional (tier *rdescent-default-membership-tier*))
  "Pure factory: return a fresh GAME-STATE for a newly connected
client -- TIER's (defaulting to *RDESCENT-DEFAULT-MEMBERSHIP-TIER*)
level-1 dungeon (via GENERATE-DUNGEON) as its MAP, a player ENTITY
placed at the center of the dungeon's first generated room (rather
than the field's raw geometric center, which could land inside a
corridor rather than a room) with char #\\@, and an ENTITIES list of
monsters deterministically spawned for that dungeon's rooms (see
SPAWN-MONSTERS-FOR-LEVEL, which already always skips this same first
room so the player never spawns sharing a room with a monster) plus
any staircases for LEVEL 1 (see SPAWN-STAIRS-FOR-LEVEL -- LEVEL 1
never gets a Stairs-Up, since there's nothing above it, but does get a
Stairs-Down as long as TIER's RDESCENT-TIER-MAX-DEPTH exceeds 1), plus
any loot procedurally spawned for the same rooms (see
SPAWN-ITEMS-FOR-LEVEL, appended after the monsters/stairs so it never
places an item on a cell either of them already occupies). The
player starts with 3 KOMBUCHA charges (see DRINK-POTION for how
they're spent) and a starter INVENTORY of two Scrolls of PIP and one
Reply-All Bomb (see USE-ITEM for how they're spent), seeded here
purely for immediate testing -- ground items are the in-game way to
acquire more of either now (see GRAB-ITEM). The player's seven
\"Corporate RPG Stats\" (BANDWIDTH/PIVOT/CAFFEINE-TOLERANCE/DOMAIN-
KNOWLEDGE/SENIORITY/SYNERGY/HYGIENE -- see ENTITY's docstring) are each
independently rolled here via ROLL-STAT (4d6, drop the lowest); the
rolled CAFFEINE-TOLERANCE then determines the player's own starting
MAX-HP/HP (via CAFFEINE-TOLERANCE-MAX-HP, rather than a flat 30), and
the rolled DOMAIN-KNOWLEDGE determines the radius of the player's
initial COMPUTE-FOV (via DOMAIN-KNOWLEDGE-FOV-RADIUS, rather than a
flat *RDESCENT-FOV-RADIUS*).
EXPLORED starts out already OR'd with the player's initial
COMPUTE-FOV so the very first render a client receives shows its
starting field of view lit up, rather than an all-shroud map that only
reveals itself after the player's first move. LEVELS is seeded with a
single entry -- depth 1 mapped to a DUNGEON-LEVEL-SNAPSHOT of this same
MAP/ENTITIES/EXPLORED -- so returning to depth 1 later (see USE-STAIRS)
finds its own starting snapshot already recorded, exactly as if the
player had left and come back."
  (multiple-value-bind (map rooms locked-door) (generate-dungeon tier 1)
    (let* ((spawn-room (first rooms))
           (player-x (if spawn-room (rect-room-center-x spawn-room) (floor *rdescent-field-width* 2)))
           (player-y (if spawn-room (rect-room-center-y spawn-room) (floor *rdescent-field-height* 2)))
           (bandwidth (roll-stat))
           (pivot (roll-stat))
           (caffeine-tolerance (roll-stat))
           (domain-knowledge (roll-stat))
           (seniority (roll-stat))
           (synergy (roll-stat))
           (hygiene (roll-stat))
           (player-max-hp (caffeine-tolerance-max-hp caffeine-tolerance))
           (initial-fov (compute-fov map player-x player-y (domain-knowledge-fov-radius domain-knowledge)))
           (spawned-monsters-and-stairs
             (append (spawn-monsters-for-level tier 1 rooms hygiene synergy)
                     (spawn-stairs-for-level tier 1 rooms (rdescent-tier-max-depth tier))))
           (spawned-items (spawn-items-for-level tier 1 rooms spawned-monsters-and-stairs))
           (spawned-fixtures (spawn-fixtures-for-level tier 1 rooms
                                                        (append spawned-monsters-and-stairs spawned-items)))
           (spawned-traps
             (spawn-traps-for-level tier 1 rooms
                                    (append spawned-monsters-and-stairs spawned-items spawned-fixtures)))
           (spawned-collectibles
             (spawn-collectibles-for-level tier 1 rooms
                                           (append spawned-monsters-and-stairs spawned-items
                                                   spawned-fixtures spawned-traps)))
           (initial-entities
             (append spawned-monsters-and-stairs
                     spawned-items
                     spawned-fixtures
                     spawned-traps
                     spawned-collectibles
                     (spawn-keys-for-level tier 1 rooms locked-door
                                           (append spawned-monsters-and-stairs spawned-items
                                                   spawned-fixtures spawned-traps spawned-collectibles))
                     (spawn-doge-for-level tier 1 rooms
                                           (append spawned-monsters-and-stairs spawned-items)
                                           nil))))
      (make-instance 'game-state
                     :player (make-instance 'entity :x player-x :y player-y :char #\@ :level 1
                                                    :max-hp player-max-hp :hp player-max-hp :defense 2 :power 5
                                                    :render-order 1 :is-alive t
                                                    :energy 0 :speed 50 :kombucha 3
                                                    :bandwidth bandwidth
                                                    :pivot pivot
                                                    :caffeine-tolerance caffeine-tolerance
                                                    :domain-knowledge domain-knowledge
                                                    :seniority seniority
                                                    :synergy synergy
                                                    :hygiene hygiene
                                                    :inventory (list (make-scroll-of-pip)
                                                                     (make-scroll-of-pip)
                                                                     (make-reply-all-bomb)))
                     :entities initial-entities
                     :map map
                     :current-depth 1
                     :levels (fset:with (fset:empty-map) 1
                                        (make-dungeon-level-snapshot
                                         :map map :entities initial-entities :explored initial-fov))
                     :explored initial-fov))))

;;; Procedural dungeon generation
;;;
;;; Each (TIER, LEVEL) pair gets its own persistent, reproducible
;;; dungeon layout: MAKE-DETERMINISTIC-RANDOM-STATE seeds SBCL's PRNG
;;; from a hash of "TIER-LEVEL" so that GENERATE-DUNGEON, called any
;;; number of times for the same pair (from any thread, any process
;;; restart), always carves the identical rooms/tunnels. Generation
;;; itself -- DIG-ROOM/DIG-TUNNEL -- works by mutating a private,
;;; freshly-allocated 2D array of TILEs that never escapes
;;; GENERATE-DUNGEON; only the finished, immutable GAME-MAP built from
;;; it is ever returned or shared, so this is "local mutation, global
;;; immutability": the usual functional-core discipline elsewhere in
;;; this file is preserved from the perspective of every caller, even
;;; though the dungeon-carving algorithm itself is naturally imperative.

(defun make-deterministic-random-state (tier level)
  "Return a fresh *RANDOM-STATE* deterministically seeded from TIER (a
string) and LEVEL (an integer), so that GENERATE-DUNGEON always
carves the identical layout for the same (TIER, LEVEL) pair. Combines
the two into a single string, hashes it with SXHASH (portable, but
this function is SBCL-specific because of SB-EXT:SEED-RANDOM-STATE,
which -- unlike CL:MAKE-RANDOM-STATE -- accepts an integer seed
directly, without needing to first construct and mutate a scratch
random state)."
  (sb-ext:seed-random-state (sxhash (format nil "~A-~D" tier level))))
