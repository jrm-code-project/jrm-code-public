;;; -*- Lisp -*-

;;; The parsed-command hierarchy (RDESCENT-COMMAND and its concrete
;;; subclasses, PARSE-RDESCENT-COMMAND), combat resolution shared
;;; between the player and monsters (RESOLVE-ATTACK-ON-PLAYER,
;;; CONFUSED-ENTITY-TURN/CONFUSED-RANDOM-STEP), the monster-AI reducer
;;; PROCESS-ENEMY-TURNS, and the top-level command-dispatch reducers
;;; (EXECUTE-IMMEDIATE-COMMAND/EXECUTE-QUEUED-COMMAND and the
;;; APPLY-RDESCENT-COMMAND/APPLY-PLAYER-COMMAND/ADVANCE-GAME-STATE
;;; entry points RDESCENT/SERVER.LISP calls).
;;;
;;; Last of several files this engine was split across (originally a
;;; single ENGINE.LISP) -- see RDESCENT/ENTITIES.LISP's own header
;;; comment for the full file map. This file is the outermost layer,
;;; built on top of every other engine file (RDESCENT/ENTITIES.LISP,
;;; RDESCENT/MECHANICS.LISP, RDESCENT/DUNGEON.LISP, and
;;; RDESCENT/ACTIONS.LISP) -- see RDESCENT/SERVER.LISP for the
;;; imperative I/O shell built on top of this one.

(in-package "JRM-CODE-PROJECT")

(defclass rdescent-command ()
  ()
  (:documentation "Base class for an immutable, parsed client command.
Produced by PARSE-RDESCENT-COMMAND, consumed via the EXECUTE-QUEUED-
COMMAND/EXECUTE-IMMEDIATE-COMMAND generic functions (see
APPLY-PLAYER-COMMAND/APPLY-RDESCENT-COMMAND), which dispatch on each
concrete subclass below rather than on a KIND keyword -- adding a new
command in the future only requires a new subclass plus a method on
each generic function, not an edit to either dispatcher. See each
subclass's own docstring for what data it carries."))

(defclass drink-command (rdescent-command)
  ()
  (:documentation "A parsed {\"action\": \"drink\"} client command.
Carries no data of its own -- DRINK-POTION needs no payload beyond
STATE itself."))

(defclass drop-command (rdescent-command)
  ((item-index :initarg :item-index :reader item-index))
  (:documentation "A parsed {\"action\": \"drop\", \"item-index\":
<integer>} client command. ITEM-INDEX is the index into the player's
own INVENTORY of the item to drop (see DROP-ITEM)."))

(defclass equip-command (rdescent-command)
  ((item-index :initarg :item-index :reader item-index))
  (:documentation "A parsed {\"action\": \"equip\", \"item-index\":
<integer>} client command (ARCHITECTURE_PLAN.md §4/§8). ITEM-INDEX is
the index into the player's own INVENTORY of the item to equip (see
EQUIP-ITEM), reusing the same ITEM-INDEX reader name as
DROP-COMMAND/USE-ITEM-COMMAND, per this file's convention of sharing
reader names across sibling command classes that carry the same kind
of data."))

(defclass grab-command (rdescent-command)
  ()
  (:documentation "A parsed {\"action\": \"grab\"} client command.
Carries no data of its own -- GRAB-ITEM needs no payload beyond STATE
and TIER, the GROUND-ITEM (if any) being looked up from the player's
own current position (see GRAB-ITEM)."))

(defclass interact-command (rdescent-command)
  ()
  (:documentation "A parsed {\"action\": \"interact\"} client command
(ARCHITECTURE_PLAN.md §3/§8). Carries no data of its own -- like
GRAB-COMMAND, the target FIXTURE is inferred from the player's own
current position, not from anything the client sends (see
INTERACT-FIXTURE/FIXTURE-AT)."))

(defclass message-log-command (rdescent-command)
  ((text :initarg :text :reader command-text))
  (:documentation "A parsed {\"action\": \"message-log\", \"text\":
<string>} client command, letting any front-end feature (e.g. the
client's own \"save to browser\" confirmation) ask the server to
append an arbitrary line to the message log. TEXT is the raw,
untrusted client-supplied string -- EXECUTE-QUEUED-COMMAND is
responsible for truncating and sanitizing it (see
SANITIZE-MESSAGE-LOG-TEXT) before it ever reaches the MESSAGE-LOG,
same as any other untrusted free text this codebase logs."))

(defclass move-command (rdescent-command)
  ((direction :initarg :direction :reader command-direction))
  (:documentation "A parsed {\"action\": \"move\", \"direction\":
<string>} client command. DIRECTION is the raw direction string (see
MOVE-PLAYER)."))

(defclass purchase-command (rdescent-command)
  ((item-index :initarg :item-index :reader item-index))
  (:documentation "A parsed {\"action\": \"purchase\", \"item-index\":
<integer>} client command (FUTURE_PLANS.md §10, \"Vendors / Shops\").
ITEM-INDEX is the index into *RDESCENT-VENDOR-STOCK-TABLE* of the item
to buy from whatever VENDOR-FIXTURE the player is currently standing
on (see PURCHASE-ITEM), reusing the same ITEM-INDEX reader name as
DROP-COMMAND/EQUIP-COMMAND/USE-ITEM-COMMAND, per this file's
convention of sharing reader names across sibling command classes that
carry the same kind of data."))

(defclass restore-command (rdescent-command)
  ((payload :initarg :payload :reader command-payload))
  (:documentation "A parsed {\"action\": \"restore\", \"payload\":
<base64 string>} client command, sent when the player presses 'r'/'R'
(see rdescent.js's RESTOREGAME) to load the base64 save blob it
already found in the browser's own \"rdescent_save\" localStorage key
(written there by PACK-SAVE-STATE's own SAVE-PAYLOAD packet) -- never
sent automatically. PAYLOAD is that raw base64 string, unpacked by
EXECUTE-QUEUED-COMMAND/EXECUTE-IMMEDIATE-COMMAND via UNPACK-SAVE-STATE,
which entirely replaces STATE with the restored GAME-STATE rather than
updating it in place."))

(defclass unequip-command (rdescent-command)
  ((slot :initarg :slot :reader command-slot))
  (:documentation "A parsed {\"action\": \"unequip\", \"slot\":
<string>} client command (ARCHITECTURE_PLAN.md §4/§8). SLOT is one of
the keywords :WEAPON/:BODY/:HEAD/:OFF-HAND (see UNEQUIP-ITEM/ENTITY's
EQUIPMENT slot) -- PARSE-RDESCENT-COMMAND maps the client's raw slot
string onto one of these four keywords via an explicit whitelist
rather than INTERN, so an attacker-controlled string can never grow
this image's symbol table."))

(defclass use-item-command (rdescent-command)
  ((item-index :initarg :item-index :reader item-index)
   (target-x :initarg :target-x :reader target-x)
   (target-y :initarg :target-y :reader target-y))
  (:documentation "A parsed {\"action\": \"use-item\", \"item-index\":
<integer>, \"target-x\": <integer>, \"target-y\": <integer>} client
command. ITEM-INDEX/TARGET-X/TARGET-Y are all integers (see
USE-ITEM)."))

(defclass use-stairs-command (rdescent-command)
  ()
  (:documentation "A parsed {\"action\": \"use-stairs\"} client
command. Carries no data of its own -- direction of travel is inferred
from whether the player is standing on a \"Stairs Up\" or \"Stairs
Down\" entity, not from anything the client sends (see USE-STAIRS)."))

(defparameter *rdescent-max-message-log-text-length* 200
  "Maximum length, in characters, of a client-supplied MESSAGE-LOG-
COMMAND's TEXT that EXECUTE-QUEUED-COMMAND will actually append to the
MESSAGE-LOG -- see SANITIZE-MESSAGE-LOG-TEXT, which truncates to this
length before the text is ever turned into a LOG-MESSAGE. Generous
enough for any legitimate status line, but well short of
*RDESCENT-MAX-COMMAND-LENGTH* (which bounds the whole raw JSON
message), so a client can't use this field alone to blow past that
outer limit.")

(defun sanitize-message-log-text (text)
  "Return TEXT (a raw, untrusted MESSAGE-LOG-COMMAND payload string)
made safe to store as a MESSAGE-LOG entry: every character that is not
GRAPHIC-CHAR-P (i.e. every control character -- newlines, tabs, and
the like -- that could otherwise be used to smuggle extra visual lines
or terminal/log-injection sequences into the log) is dropped, and the
result is truncated to at most *RDESCENT-MAX-MESSAGE-LOG-TEXT-LENGTH*
characters. This does not HTML-escape TEXT -- that still happens at
render time, in RDESCENT-MESSAGE-LOG-HTML (RDESCENT/SERVER.LISP), same
as every other MESSAGE-LOG entry -- it only guards against control
characters and unbounded length, the two hazards specific to accepting
arbitrary client-supplied text into the log at all."
  (let ((cleaned (remove-if-not #'graphic-char-p text)))
    (subseq cleaned 0 (min (length cleaned) *rdescent-max-message-log-text-length*))))

(defparameter *rdescent-max-command-length* (* 96 1024)
  "Maximum length, in characters, of a raw WebSocket text message this
server will attempt to JSON-decode in PARSE-RDESCENT-COMMAND. Every
command except RESTORE-COMMAND is well under 80 characters (e.g.
{\"action\":\"use-item\",\"item-index\":<integer>,\"target-x\":
<integer>,\"target-y\":<integer>}), so this generous-looking 96KB
ceiling exists solely to accommodate {\"action\":\"restore\",
\"payload\":<base64 string>}: the client only ever sends a RESTORE
payload it already confirmed is under 64KB (see rdescent.js's
RESTOREGAME, which itself refuses to send anything bigger and instead
logs \"*** Error: Saved Game too large ***\" via MESSAGE-LOG-COMMAND),
so 96KB comfortably covers a 64KB base64 payload plus its small
{\"action\":...,\"payload\":...} JSON wrapper, with headroom to spare.
Anything longer than this is rejected before it ever reaches
CL-JSON:DECODE-JSON-FROM-STRING, so a connected client cannot force
this read thread to spend unbounded CPU/memory parsing an arbitrarily
large payload.")

(defun parse-rdescent-command (raw-json-string)
  "Parse RAW-JSON-STRING (a raw WebSocket text message) into an
RDESCENT-COMMAND instance, or return NIL if it is too long, malformed
JSON, or doesn't match a recognized command shape. This function never
signals: any JSON parse error is caught internally, logged as a
diagnostic, and treated as \"no command\", so callers never need their
own error handling around it. (The diagnostic log call is the one
deliberate, isolated side effect at this I/O boundary; the rest of
this function's logic -- and everything downstream of its NIL/
RDESCENT-COMMAND result -- is pure.) Messages longer than
*RDESCENT-MAX-COMMAND-LENGTH* are rejected outright, before
CL-JSON:DECODE-JSON-FROM-STRING ever sees them. Six shapes are
recognized: {\"action\": \"move\", \"direction\": <string>}, producing
a MOVE-COMMAND with DIRECTION the direction string; {\"action\":
\"use-stairs\"}, producing a USE-STAIRS-COMMAND; {\"action\": \"drink\"},
producing a DRINK-COMMAND (see DRINK-POTION); {\"action\":
\"use-item\", \"item-index\": <integer>, \"target-x\": <integer>,
\"target-y\": <integer>}, producing a USE-ITEM-COMMAND with
ITEM-INDEX/TARGET-X/TARGET-Y (see USE-ITEM) -- any of the three fields
missing or not an integer falls through to the NIL fallback exactly
like :MOVE's missing/non-string DIRECTION does; {\"action\": \"grab\"},
producing a GRAB-COMMAND (see GRAB-ITEM); {\"action\": \"drop\",
\"item-index\": <integer>}, producing a DROP-COMMAND with ITEM-INDEX
(see DROP-ITEM) -- a missing/non-integer ITEM-INDEX falls through to
the NIL fallback exactly like USE-ITEM-COMMAND's own fields; and
{\"action\": \"interact\"}, producing an INTERACT-COMMAND (see
INTERACT-FIXTURE); {\"action\": \"equip\", \"item-index\": <integer>},
producing an EQUIP-COMMAND with ITEM-INDEX (see EQUIP-ITEM) -- a
missing/non-integer ITEM-INDEX falls through to the NIL fallback
exactly like DROP-COMMAND's own field; and {\"action\": \"unequip\",
\"slot\": <string>}, producing an UNEQUIP-COMMAND whose SLOT is one of
the keywords :WEAPON/:BODY/:HEAD/:OFF-HAND, mapped from the client's
raw \"weapon\"/\"body\"/\"head\"/\"off-hand\" string via an explicit
whitelist (see UNEQUIP-COMMAND) -- any other slot string, or a missing/
non-string SLOT field, falls through to the NIL fallback; and
{\"action\": \"message-log\", \"text\": <string>}, producing a
MESSAGE-LOG-COMMAND with TEXT the raw string (see MESSAGE-LOG-COMMAND
and SANITIZE-MESSAGE-LOG-TEXT for how it is truncated/sanitized before
being logged) -- a missing/non-string TEXT falls through to the NIL
fallback exactly like MOVE-COMMAND's own DIRECTION field; and
{\"action\": \"purchase\", \"item-index\": <integer>}, producing a
PURCHASE-COMMAND with ITEM-INDEX (see PURCHASE-ITEM) -- a missing/
non-integer ITEM-INDEX falls through to the NIL fallback exactly like
DROP-COMMAND's/EQUIP-COMMAND's own field; and
{\"action\": \"restore\", \"payload\": <string>}, producing a
RESTORE-COMMAND with PAYLOAD the raw base64 string (see RESTORE-COMMAND/
UNPACK-SAVE-STATE) -- a missing/non-string PAYLOAD falls through to the
NIL fallback exactly like MESSAGE-LOG-COMMAND's own TEXT field. PAYLOAD
itself is not validated here beyond being a string -- an invalid/
tampered/corrupt base64 blob is instead caught by UNPACK-SAVE-STATE
signaling an error when EXECUTE-QUEUED-COMMAND actually tries to
restore it, which TICK-ALL-CLIENTS' own HANDLER-CASE (RDESCENT/
SERVER.LISP) already treats as \"skip this client's tick\" rather than
letting it crash the shared game-loop thread.
Internally, the decode-then-validate steps are threaded via ALEXANDRIA:WHEN-LET*
(already a project dependency, per JRM-CODE-PROJECT.ASD), this file's
convention for chaining fallible pure steps without a hand-rolled
monad helper: each binding only proceeds if every earlier one was
non-NIL, so an malformed/missing field anywhere in the chain falls
straight through to the NIL fallback below."
  (if (> (length raw-json-string) *rdescent-max-command-length*)
      (progn
        (rdescent-safe-log-warning "ignoring oversized message (~D characters, max ~D)"
                                    (length raw-json-string) *rdescent-max-command-length*)
        nil)
      (handler-case
          (let* ((packet (cl-json:decode-json-from-string raw-json-string))
                 (command-name (cdr (assoc :action packet))))
            (cond
              ((equal command-name "drink")
               (make-instance 'drink-command))
              ((equal command-name "drop")
               (when-let* ((item-index (cdr (assoc :item-index packet)))
                           (item-index (and (integerp item-index) item-index)))
                 (make-instance 'drop-command :item-index item-index)))
              ((equal command-name "equip")
               (when-let* ((item-index (cdr (assoc :item-index packet)))
                           (item-index (and (integerp item-index) item-index)))
                 (make-instance 'equip-command :item-index item-index)))
              ((equal command-name "grab")
               (make-instance 'grab-command))
              ((equal command-name "interact")
               (make-instance 'interact-command))
              ((equal command-name "message-log")
               (when-let* ((text (cdr (assoc :text packet)))
                           (text (and (stringp text) text)))
                 (make-instance 'message-log-command :text text)))
              ((equal command-name "move")
               (when-let* ((direction (cdr (assoc :direction packet)))
                           (direction (and (stringp direction) direction)))
                 (make-instance 'move-command :direction direction)))
              ((equal command-name "purchase")
               (when-let* ((item-index (cdr (assoc :item-index packet)))
                           (item-index (and (integerp item-index) item-index)))
                 (make-instance 'purchase-command :item-index item-index)))
              ((equal command-name "restore")
               (when-let* ((payload (cdr (assoc :payload packet)))
                           (payload (and (stringp payload) payload)))
                 (make-instance 'restore-command :payload payload)))
              ((equal command-name "unequip")
               (when-let* ((slot-string (cdr (assoc :slot packet)))
                           (slot-string (and (stringp slot-string) slot-string))
                           (slot (cond ((string= slot-string "body") :body)
                                       ((string= slot-string "head") :head)
                                       ((string= slot-string "off-hand") :off-hand)
                                       ((string= slot-string "weapon") :weapon)
                                       (t nil))))
                 (make-instance 'unequip-command :slot slot)))
              ((equal command-name "use-item")
               (when-let* ((item-index (cdr (assoc :item-index packet)))
                           (item-index (and (integerp item-index) item-index))
                           (target-x (cdr (assoc :target-x packet)))
                           (target-x (and (integerp target-x) target-x))
                           (target-y (cdr (assoc :target-y packet)))
                           (target-y (and (integerp target-y) target-y)))
                 (make-instance 'use-item-command :item-index item-index
                                                  :target-x target-x :target-y target-y)))
              ((equal command-name "use-stairs")
               (make-instance 'use-stairs-command))
              (t
               (rdescent-safe-log-warning "ignoring unrecognized command-name ~S" command-name))))
        (error (e)
          (rdescent-safe-log-warning "ignoring malformed message ~S -- ~A" raw-json-string e)
          nil))))

(defun confused-random-step ()
  "Return two values DX DY, the direction a confused entity staggers
in this turn (see CONFUSED-ENTITY-TURN): each independently and
uniformly a random integer in {-1, 0, 1} (via (1- (RANDOM 3))) --
unlike PROCESS-ENEMY-TURNS' own greedy SIGNUM step toward the player,
a confused entity's step is completely undirected. Both values may be
0 at once (the entity simply staggers in place this turn)."
  (values (1- (random 3)) (1- (random 3))))

(defgeneric resolve-attack (attacker defender)
  (:documentation "Return four values DAMAGE DIES NEW-DEFENDER BROKEN-ITEM-NAME describing the outcome
of ATTACKER's melee attack against DEFENDER (ARCHITECTURE_PLAN.md §5)
-- the single source of truth for \"how hard does an attack land\",
shared by every combat call site in this codebase (MOVE-PLAYER's
melee branch, PROCESS-ENEMY-TURNS' ordinary attacking branch,
CONFUSED-ENTITY-TURN's stumble-into-a-target branch, whether the
target is the player or another monster) rather than each
independently re-deriving the same dodge-chance/damage-scaling math.

EFFECTIVE-DODGE-CHANCE (ENTITIES.LISP -- PIVOT-DODGE-CHANCE, reduced
while DEFENDER has an active :ANALYSIS-PARALYSIS STATUS-EFFECT, see
FUTURE_PLANS.md §7) is rolled against DEFENDER first -- if (RANDOM
100) is less than that percent, the attack is dodged entirely, and
this function returns NIL NIL DEFENDER NIL (DAMAGE of NIL is the caller's
signal the attack was dodged; DEFENDER itself is returned EQ-unchanged,
so a caller can always treat the third value as \"the defender after
this attack\" regardless of which branch was taken; a dodged attack
can never wear down any equipment either, hence BROKEN-ITEM-NAME NIL).

Otherwise DAMAGE is (MAX 0 (- attacker's EFFECTIVE-POWER defender's
EFFECTIVE-DEFENSE)) -- so any equipped gear on either side is folded
in automatically -- scaled by DEFENDER's own BANDWIDTH-DAMAGE-
MULTIPLIER and rounded to the nearest integer (floored at 0); DIES is
true if that leaves DEFENDER's HP at or below 0. NEW-DEFENDER is
DEFENDER with HP reduced by DAMAGE -- deliberately *not* yet turned
into a corpse, win or lose, even when DIES is true: a dead player and
a dead monster are represented completely differently (players flip
CHAR to #\\% and IS-ALIVE to NIL but stay themselves; monsters turn
into a wholly separate inert corpse ENTITY with a new NAME/RENDER-
ORDER/BLOCKS-MOVEMENT, and can award the killer XP) and RESOLVE-ATTACK
has no way to tell which kind of ENTITY DEFENDER is (there is no
distinct player class -- the player is just whatever ENTITY GET-PLAYER
happens to return) -- so that cosmetic/reward transformation is left
entirely to each caller, exactly as it always has been.

If the hit was successful, non-lethal (not DIES), and DAMAGE is
greater than 0, ATTACKER's own EFFECTIVE-WEAPON's WEAPON-ON-HIT-EFFECT
(if any) is additionally applied to NEW-DEFENDER via APPLY-STATUS-
EFFECT before it is returned (a lethal hit has no defender left to
inflict a lingering effect on, and a 0-damage hit -- e.g. an
overwhelmed DEFENSE stat -- is not a *hit* worth an on-hit effect
either), *except* when that ON-HIT-EFFECT's :KIND is :CONVERT-TO-ALLY
(The Source Code of the Universe, FUTURE_PLANS.md §15), in which case
NEW-DEFENDER instead has its own DISPOSITION/FACTION permanently
flipped to :FRIENDLY/:COMPANION via UPDATE-ENTITY -- a special case
entirely outside APPLY-STATUS-EFFECT's own STATUS-EFFECT machinery,
since converting a monster into an ally is not itself a timed debuff.

Finally, whenever DAMAGE is greater than 0 (a real, connecting,
non-dodged hit, whether or not it kills), APPLY-EQUIPMENT-WEAR
(ENTITIES.LISP) is given a chance to wear down one of NEW-DEFENDER's
own equipped items by *RDESCENT-ITEM-DURABILITY-LOSS-PER-HIT* -- see
its own docstring for the full rules, including outright destroying
(even a :CURSED) item whose durability reaches 0. NEW-DEFENDER is
further updated to reflect this (equipment always returned via
APPLY-EQUIPMENT-WEAR's own first value even when nothing broke), and
BROKEN-ITEM-NAME is that item's own GET-ITEM-NAME if one broke this
call, or NIL otherwise -- a caller building its own attack message can
append something like \"Your ~A breaks!\" only when this is non-NIL.")
  (:method ((attacker entity) (defender entity))
    (if (< (random 100) (effective-dodge-chance defender))
        (values nil nil defender nil)
        (let* ((damage (max 0 (round (* (max 0 (- (effective-power attacker) (effective-defense defender)))
                                        (bandwidth-damage-multiplier (get-bandwidth defender))))))
               (new-hp (- (hp defender) damage))
               (dies (<= new-hp 0))
               (new-defender (update-entity defender :hp new-hp))
               (on-hit-effect (and (> damage 0) (not dies) (weapon-on-hit-effect (effective-weapon attacker))))
               (new-defender (cond
                              ((and on-hit-effect
                                    (eq (getf on-hit-effect :kind) :convert-to-ally)
                                    (< (random 1.0) (or (getf on-hit-effect :chance) 1.0)))
                               ;; The Source Code of the Universe (FUTURE_PLANS.md
                               ;; §15) permanently flips a converted monster's own
                               ;; DISPOSITION/FACTION rather than attaching an
                               ;; ordinary STATUS-EFFECT -- see this file's own
                               ;; §15 preamble comment for why it becomes a
                               ;; passive, not actively-fighting, ally.
                               (update-entity new-defender :disposition :friendly :faction :companion))
                              ((and on-hit-effect
                                    (< (random 1.0) (or (getf on-hit-effect :chance) 1.0)))
                               (apply-status-effect new-defender (getf on-hit-effect :kind)
                                                    (getf on-hit-effect :turns) (getf on-hit-effect :magnitude)))
                              (t new-defender)))
               (broken-item-name nil))
          (when (> damage 0)
            (multiple-value-bind (worn-defender broken) (apply-equipment-wear new-defender damage)
              (setf new-defender worn-defender
                    broken-item-name broken)))
          (values damage dies new-defender broken-item-name)))))

(defun resolve-attack-volley (attacker defender)
  "Return four values DAMAGE DIES NEW-DEFENDER BROKEN-ITEM-NAMES describing one attack
action by ATTACKER against DEFENDER, aggregating across ATTACKER's own
WEAPON-HITS-PER-TURN. Each individual blow reuses RESOLVE-ATTACK's
ordinary dodge/damage/on-hit-effect/equipment-wear logic; DAMAGE is the
summed damage across every blow that landed, NIL only if every blow was
dodged, and 0 if at least one blow connected but all were reduced to
zero damage by DEFENSE. Resolution stops early once DEFENDER dies.
BROKEN-ITEM-NAMES is a list (possibly empty) of every distinct item
name RESOLVE-ATTACK reported broke across this volley's blows (a
multi-hit weapon could plausibly break more than one of DEFENDER's
equipped items in a single turn)."
  (let ((current-defender defender)
        (total-damage 0)
        (saw-non-dodge-hit nil)
        (dies nil)
        (broken-item-names nil))
    (dotimes (i (weapon-hits-per-turn (effective-weapon attacker)))
      (declare (ignorable i))
      (unless dies
        (multiple-value-bind (damage hit-dies new-defender broken) (resolve-attack attacker current-defender)
          (setf current-defender new-defender)
          (when damage
            (setf saw-non-dodge-hit t
                  total-damage (+ total-damage damage)))
          (when broken
            (push broken broken-item-names))
          (when hit-dies
            (setf dies t)))))
    (values (if saw-non-dodge-hit total-damage nil) dies current-defender (nreverse broken-item-names))))

(defun resolve-attack-on-player (ent player)
  "Return four values DAMAGE DIES NEW-PLAYER BROKEN-ITEM-NAMES describing the outcome
of ENT's melee attack against PLAYER -- a thin wrapper around the
generalized RESOLVE-ATTACK-VOLLEY (ARCHITECTURE_PLAN.md §5) that
additionally performs the player-specific death transformation
RESOLVE-ATTACK itself deliberately leaves to its caller: if DIES,
NEW-PLAYER's CHAR is flipped to #\\% and IS-ALIVE to NIL (players
don't turn into a generic corpse ENTITY like monsters do) on top of
RESOLVE-ATTACK-VOLLEY's own already-HP-reduced NEW-DEFENDER; otherwise
NEW-PLAYER is exactly RESOLVE-ATTACK-VOLLEY's own NEW-DEFENDER,
unchanged. BROKEN-ITEM-NAMES is passed straight through from
RESOLVE-ATTACK-VOLLEY -- see its own docstring.
Shared by PROCESS-ENEMY-TURNS' own ordinary attacking branch and
CONFUSED-ENTITY-TURN's random-stumble-into-the-player branch (see
TECHNICAL_DEBT.md item #38)."
  (multiple-value-bind (damage dies new-player broken-item-names) (resolve-attack-volley ent player)
    (values damage dies (if dies (update-entity new-player :char #\% :is-alive nil) new-player) broken-item-names)))
(defun confused-entity-turn (state ent map level cost)
  "Return a fresh GAME-STATE derived from STATE describing ENT's turn
while confused (ENTITY-CONFUSED-TICKS ENT > 0, per PROCESS-ENEMY-
TURNS' own gating, which also guarantees ENT has already saved up at
least COST ENERGY -- *RDESCENT-MOVE-ENERGY-COST*, the flat cost of a
confused turn regardless of what it turns into -- before calling this
function). COST is deducted from its ENERGY no matter what happens next
-- a confused monster spends its turn staggering whether or not that
stagger actually moves it, or lands a blow, anywhere. CONFUSED-RANDOM-STEP
picks ENT's uniformly random (DX, DY) direction for this turn:
  - If DX and DY are both 0 (CONFUSED-RANDOM-STEP can, and does 1/9th
    of the time, pick no displacement at all) or the resulting cell is
    off a walkable TILE (per MAP-TILE-REF), ENT simply stays put -- no
    message, mirroring PROCESS-ENEMY-TURNS' own silent blocked-move
    handling. (DX=DY=0 is deliberately never treated as a
    BLOCKING-ENTITY-AT hit on ENT's own cell -- ENT's own unmodified
    entry is still present in STATE's own ENTITIES at that point in
    the computation, so without this guard a confused entity that
    happens to stagger \"in place\" would find and attack itself.)
  - Else if a BLOCKING-ENTITY-AT occupies that cell -- the player, or
    another monster ENT stumbles into; unlike PROCESS-ENEMY-TURNS' own
    ordinary AI, a confused entity's random stagger is not aimed only
    at the player -- ENT attacks it via RESOLVE-ATTACK (dodge chance/
    damage scaling identical for either kind of target, since
    RESOLVE-ATTACK itself no longer distinguishes \"is this the
    player\"). If the target dies, it becomes an inert corpse exactly
    like an ordinary kill (see MOVE-PLAYER/PROCESS-ENEMY-TURNS) -- or,
    when the target is the player, the player's own death CHAR/
    IS-ALIVE flip and \"You have died...\" message happen instead.
    Killing another monster this way awards the player no XP (ENT did
    the killing, not the player).
  - Else the cell is open floor: ENT simply moves there."
  (multiple-value-bind (dx dy) (confused-random-step)
    (let* ((step-x (+ (get-x ent) dx))
           (step-y (+ (get-y ent) dy))
           (stagger-in-place (and (zerop dx) (zerop dy)))
           (tile (map-tile-ref map step-x step-y))
           (target (and (not stagger-in-place)
                        tile
                        (get-walkable tile)
                        (blocking-entity-at state step-x step-y level))))
      (cond
        ((or stagger-in-place (not (and tile (get-walkable tile))))
         (update-game-state
          state
          :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                ent
                                (get-entities state))))
        ((eq target (get-player state))
         (let ((player (get-player state)))
           (multiple-value-bind (damage dies resolved-player broken-item-names) (resolve-attack-on-player ent player)
             (if (null damage)
                 (update-game-state
                  state
                  :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                        ent
                                        (get-entities state))
                  :message-log (append-log-messages
                                (get-message-log state)
                                (list (make-log-message
                                       (format nil "The ~A, confused, stumbles at you but you ~A"
                                               (get-name ent) (random-dodge-phrase :second))
                                       (entity-message-color ent)))))
                 (multiple-value-bind (rescued-state new-player yubikey-saved-p save-message)
                     (maybe-trigger-yubikey-save state resolved-player map level)
                   (let ((messages
                           (append
                            (build-combat-messages
                             (get-name ent) damage (and dies (not yubikey-saved-p)) (entity-message-color ent)
                             "The ~A, confused, stumbles into you and hits you for ~D damage!"
                             "The ~A, confused, stumbles into you but does no damage!"
                             "You have died...")
                            (item-break-messages broken-item-names)
                            (if yubikey-saved-p
                                (list (make-log-message
                                       save-message
                                       *rdescent-status-effect-callout-color*))
                                nil))))
                     (update-game-state
                      rescued-state
                      :player new-player
                      :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                            ent
                                            (get-entities rescued-state))
                      :message-log (append-log-messages (get-message-log rescued-state) messages))))))))
        (target
         (multiple-value-bind (damage dies new-target broken-item-names) (resolve-attack-volley ent target)
           (if (null damage)
               (update-game-state
                state
                :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                      ent
                                      (get-entities state))
                :message-log (append-log-messages
                              (get-message-log state)
                              (list (make-log-message
                                     (format nil "The ~A, confused, stumbles at the ~A but it ~A"
                                             (get-name ent) (get-name target) (random-dodge-phrase :third))
                                     (entity-message-color ent)))))
               (let* ((new-target (if dies
                                      (update-entity new-target :char #\% :name (format nil "remains of ~A" (get-name target))
                                                                :blocks-movement nil :render-order 0 :is-alive nil)
                                      new-target))
                      (target-name (get-name target))
                      (drop (and dies (maybe-drop-monster-loot new-target)))
                      (messages
                        (append
                         (monster-death-drop-messages drop)
                         (build-combat-messages
                          (list (get-name ent) target-name) damage dies (entity-message-color ent)
                          "The ~A, confused, stumbles into the ~A and hits it for ~D damage!"
                          "The ~A, confused, stumbles into the ~A but does no damage!"
                          (make-log-message (format nil "The ~A collapses!" target-name)
                                            (entity-message-color ent)))
                         (item-break-messages broken-item-names))))
                 (update-game-state
                  state
                  :entities (let ((updated (substitute new-target target
                                                        (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                                                    ent
                                                                    (get-entities state)))))
                              (if drop (cons drop updated) updated))
                  :message-log (append-log-messages (get-message-log state) messages))))))
        (t
         (update-game-state
          state
          :entities (substitute (update-entity ent :x step-x :y step-y :energy (- (entity-energy ent) cost))
                                ent
                                (get-entities state))))))))
(defun neutral-wander-turn (state ent map level cost)
  "Return a fresh GAME-STATE derived from STATE describing ENT's turn
while its ENTITY-DISPOSITION-TOWARD the player is :NEUTRAL (see
PROCESS-ENEMY-TURNS/ARCHITECTURE_PLAN.md §2): ENT takes a single
uniformly random step (reusing CONFUSED-RANDOM-STEP's undirected
movement -- mechanically identical to CONFUSED-ENTITY-TURN's own
stagger, since a neutral entity ignoring the player and a confused
entity unable to aim at anything move the same way). Unlike
CONFUSED-ENTITY-TURN, ENT never attacks on this branch, even if its
random step happens to land on an occupied cell -- a :NEUTRAL entity
\"simply wanders, ignoring the player entirely\" (see FACTION/
DISPOSITION's own docstring), so a blocked destination (whether
that's a wall or another entity) is treated exactly like an
off-map/unwalkable step: ENT stays put, silently. COST is always
deducted from ENT's ENERGY regardless of whether the step actually
moves it, matching every other AI branch's blocked-step handling."
  (multiple-value-bind (dx dy) (confused-random-step)
    (let* ((step-x (+ (get-x ent) dx))
           (step-y (+ (get-y ent) dy))
           (tile (map-tile-ref map step-x step-y))
           (remaining-energy (- (entity-energy ent) cost)))
      (if (and tile (get-walkable tile) (not (blocking-entity-at state step-x step-y level)))
          (update-game-state
           state
           :entities (substitute (update-entity ent :x step-x :y step-y :energy remaining-energy)
                               ent (get-entities state)))
          (update-game-state
           state
           :entities (substitute (update-entity ent :energy remaining-energy)
                               ent (get-entities state)))))))

(defun fleeing-turn (state ent map level cost dx dy)
  "Return a fresh GAME-STATE derived from STATE describing ENT's turn
while its ENTITY-DISPOSITION-TOWARD the player is :FLEEING (see
PROCESS-ENEMY-TURNS/ARCHITECTURE_PLAN.md §2). DX/DY are the player-
relative deltas PROCESS-ENEMY-TURNS already computed for its own
distance check ((- player's coord ENT's coord)).

Unlike a hostile monster's approach (ASTAR-NEXT-STEP), fleeing has no
single fixed destination cell to search a path toward -- ENT just
wants to put more distance between itself and the player each turn --
so this doesn't run a full A* search. But a plain single-direction
greedy step (the old SIGNUM (- DX))/(SIGNUM (- DY)) approach this
replaces) suffers the exact same wall-corner problem ASTAR-NEXT-STEP
was introduced to fix for hostiles (see its own docstring): a fleeing
entity backed into an L-shaped corner or a doorway, with a wall
sitting directly on its one straight-line away-from-player diagonal,
would recompute that same blocked step every turn and freeze in
place -- or, worse, as the player's own position shifts slightly from
turn to turn, flip repeatedly between two different blocked diagonals
that both happen to read as \"away\" on alternating turns, visibly
dancing in place rather than actually escaping.

To avoid both failure modes, this evaluates all eight neighboring
cells (plus staying put) that are walkable, not BLOCKING-ENTITY-AT-
occupied, and not the player's own square (BLOCKING-ENTITY-AT alone
doesn't cover that -- the player lives in GAME-STATE's own PLAYER
slot, not its ENTITIES list -- so this checks NEW-DX/NEW-DY against
(0, 0) explicitly, mirroring COMPANION-AI-TURN/PROCESS-ENEMY-TURNS'
own \"never step onto the player's cell\" guard on their approach
steps), and picks whichever candidate leaves ENT farthest (by squared
Euclidean distance, so a diagonal step that increases both axes'
separation is preferred over one that only increases a single axis')
from the player's current position -- routing around a corner instead
of freezing against it whenever any escape route exists. Ties
(including against staying put) are broken in favor of the original
straight-away SIGNUM direction when it's among the tied candidates,
and otherwise by each candidate's own fixed scan order (DY then DX,
both -1 to 1), so the choice stays deterministic and stable turn to
turn rather than flip-flopping among equally-good options as the
player's own position wobbles by a cell or two.

Never attacks -- fleeing is purely evasive, even if every escape step
would otherwise land ENT adjacent to the player or another entity (a
fully cornered ENT with no strictly-farther cell available just stays
put, silently, like every other movement branch). COST is always
deducted from ENT's ENERGY regardless of whether the step actually
moves it."
  (let* ((preferred-dx (signum (- dx)))
         (preferred-dy (signum (- dy)))
         (current-distance-sq (+ (* dx dx) (* dy dy)))
         (best-x (get-x ent))
         (best-y (get-y ent))
         (best-distance-sq current-distance-sq)
         (best-is-preferred t))
    (loop for cddy from -1 to 1
          do (loop for cddx from -1 to 1
                   unless (and (zerop cddx) (zerop cddy))
                   do (let* ((cx (+ (get-x ent) cddx))
                             (cy (+ (get-y ent) cddy))
                             (tile (map-tile-ref map cx cy))
                             (new-dx (- dx cddx))
                             (new-dy (- dy cddy)))
                        (when (and tile (get-walkable tile)
                                   (not (and (zerop new-dx) (zerop new-dy)))
                                   (not (blocking-entity-at state cx cy level)))
                          (let* ((new-distance-sq (+ (* new-dx new-dx) (* new-dy new-dy)))
                                 (is-preferred (and (= cddx preferred-dx) (= cddy preferred-dy))))
                            (when (or (> new-distance-sq best-distance-sq)
                                      (and (= new-distance-sq best-distance-sq)
                                           is-preferred (not best-is-preferred)))
                              (setf best-x cx best-y cy
                                    best-distance-sq new-distance-sq
                                    best-is-preferred is-preferred)))))))
    (update-game-state
     state
     :entities (substitute (update-entity ent :x best-x :y best-y :energy (- (entity-energy ent) cost))
                         ent (get-entities state)))))


(defun nearest-hostile-to (ent entities level)
  "Return whichever member of ENTITIES is nearest (Chebyshev distance)
to ENT's own (X, Y) on LEVEL, among those that are not ENT itself, are
IS-ALIVE, share LEVEL, and whose ENTITY-DISPOSITION-TOWARD is :HOSTILE
(see ARCHITECTURE_PLAN.md §2) -- COMPANION-AI-TURN's own target-
selection helper (FUTURE_PLANS.md §22): an Office Doge picks whichever
hostile monster is currently closest to *itself*, not the player, to
chase or bite. Ties are broken by ENTITIES' own list order (the first
strictly-closer candidate found wins, so an earlier-equidistant
candidate is preferred over a later one). Returns NIL if no hostile
candidate shares LEVEL at all."
  (let (best best-distance)
    (dolist (candidate entities best)
      (when (and (not (eq candidate ent))
                 (is-alive candidate)
                 (= (get-level candidate) level)
                 (eq (entity-disposition-toward candidate ent) :hostile))
        (let ((distance (max (abs (- (get-x candidate) (get-x ent)))
                              (abs (- (get-y candidate) (get-y ent))))))
          (when (or (null best) (< distance best-distance))
            (setf best candidate best-distance distance)))))))

(defparameter *rdescent-companion-aggro-radius* 6
  "Radius (Chebyshev distance, matching *RDESCENT-MONSTER-FOV-RADIUS*'s
own convention) within which a bonded Office Doge COMPANION will break
off following the player to approach a hostile monster it cannot yet
reach for a melee attack -- see COMPANION-AI-TURN. A hostile monster
farther away than this is simply not worth Doge's attention yet; it
keeps following the player instead, and will naturally close the
distance if the player's own path brings the monster back within
range on some later turn.")

(defparameter *rdescent-companion-leash-radius* 2
  "Radius (Chebyshev distance) within which a bonded Office Doge
COMPANION considers itself close enough to the player and stops
closing the distance further -- see COMPANION-AI-TURN. Once within
this leash, and with no hostile target to approach, Doge wanders a
little (COMPANION-WANDER-STEP) rather than beelining onto the
player's own heels every single turn, so it feels like a living
companion rather than a shadow glued to the player's tile. Doge will
still leave this leash whenever a hostile monster within
*RDESCENT-COMPANION-AGGRO-RADIUS* needs chasing down for a melee
attack (see COMPANION-AI-TURN's GOAL selection) -- the leash only
governs its idle, nothing-to-fight behavior.")

(defparameter *rdescent-companion-wander-persistence* 5
  "Number of additional turns a bonded Office Doge COMPANION commits to
staying on its currently chosen wander direction (see COMPANION-
WANDER-STEP) before rolling a fresh random one, once it has nothing to
fight and is already within *RDESCENT-COMPANION-LEASH-RADIUS* of the
player. Without this, COMPANION-WANDER-STEP used to re-roll an
independent uniform random direction every single tick (CONFUSED-
RANDOM-STEP's own distribution), which visibly looked like Doge was
jittering/dancing in place rather than casually exploring nearby --
committing to one direction for several turns in a row (tracked via
ENT's own COMPANION-WANDER-DX/COMPANION-WANDER-DY/COMPANION-WANDER-
TICKS slots) instead reads as Doge actually wandering off to sniff
around before doubling back.")

(defun companion-wander-destination-ok-p (state ent map level player dx dy)
  "Return true if a bonded Office Doge COMPANION ENT stepping by (DX DY)
this turn would land on an in-bounds, walkable (MAP-TILE-REF/GET-
WALKABLE) cell that is not occupied by another entity (BLOCKING-
ENTITY-AT), not PLAYER's own tile, and still within *RDESCENT-
COMPANION-LEASH-RADIUS* (Chebyshev distance) of PLAYER's current
position -- the single shared validity check COMPANION-WANDER-STEP
uses whether it is re-trying ENT's already-committed direction or
trying a freshly rolled candidate."
  (let* ((new-x (+ (get-x ent) dx))
         (new-y (+ (get-y ent) dy))
         (tile (map-tile-ref map new-x new-y)))
    (and tile (get-walkable tile)
         (not (blocking-entity-at state new-x new-y level))
         (not (and (= new-x (get-x player)) (= new-y (get-y player))))
         (<= (max (abs (- new-x (get-x player))) (abs (- new-y (get-y player))))
             *rdescent-companion-leash-radius*))))

(defun companion-wander-step (state ent map level player)
  "Return three values (STEP-DX STEP-DY NEW-TICKS), each of STEP-DX/
STEP-DY in {-1,0,1} (possibly both 0), describing a single undirected
step for a bonded Office Doge COMPANION ENT to wander this turn while
idle (see COMPANION-AI-TURN's own leashed-and-nothing-to-fight
branch) -- or (VALUES NIL NIL 0) if no step satisfying every
constraint below could be found, in which case the caller should
treat ENT as simply staying put this turn (and re-roll fresh next
turn, via NEW-TICKS of 0).
Rather than independently re-rolling a fresh random direction every
single call (which used to make Doge look like it was jittering in
place -- see *RDESCENT-COMPANION-WANDER-PERSISTENCE*'s own docstring),
first tries to keep going in ENT's own currently committed direction
(COMPANION-WANDER-DX/COMPANION-WANDER-DY) as long as it still has
COMPANION-WANDER-TICKS remaining AND that direction still satisfies
COMPANION-WANDER-DESTINATION-OK-P (it may not any more -- a wall,
another entity, or the leash itself may have moved into the way).
Only once that committed direction runs out or stops being valid does
it roll a fresh one: tries up to 8 independently random directions
(CONFUSED-RANDOM-STEP's own uniform {-1,0,1}x{-1,0,1} distribution, so
ENT may sometimes 'choose' to stagger in place even when a real step
would have been available -- deliberately mirroring CONFUSED-RANDOM-
STEP's own undirected feel rather than always preferring motion), and
commits to the first satisfying COMPANION-WANDER-DESTINATION-OK-P for
*RDESCENT-COMPANION-WANDER-PERSISTENCE* further turns. The caller
(COMPANION-AI-TURN) is responsible for persisting NEW-TICKS (along
with STEP-DX/STEP-DY, since those become ENT's new committed
direction) onto the moved entity via UPDATE-ENTITY's own :WANDER-DX/
:WANDER-DY/:WANDER-TICKS keywords."
  (if (and (> (companion-wander-ticks ent) 0)
           (companion-wander-destination-ok-p state ent map level player
                                               (companion-wander-dx ent) (companion-wander-dy ent)))
      (values (companion-wander-dx ent) (companion-wander-dy ent) (1- (companion-wander-ticks ent)))
      (dotimes (attempt 8 (values nil nil 0))
        (declare (ignorable attempt))
        (multiple-value-bind (dx dy) (confused-random-step)
          (when (companion-wander-destination-ok-p state ent map level player dx dy)
            (return (values dx dy (1- *rdescent-companion-wander-persistence*))))))))

(defun companion-ai-turn (state ent map level cost player visible-p)
  "Return a fresh GAME-STATE derived from STATE describing a bonded
Office Doge COMPANION ENT's own turn (FUTURE_PLANS.md §22) --
PROCESS-ENEMY-TURNS' own dispatch for any BONDED-COMPANION-P ent,
checked before its ordinary ENTITY-DISPOSITION-TOWARD-based dispatch
(a COMPANION defaults to :FRIENDLY disposition -- see COMPANION's own
docstring -- which would otherwise send it down the same do-nothing
:FRIENDLY branch other, actually-friendly NPCs get). Picks its own
target via NEAREST-HOSTILE-TO (the hostile monster nearest to ENT's
own position, not the player's):
  - If one exists and is within its EFFECTIVE-WEAPON's WEAPON-REACH,
    ENT attacks it via the fully generic RESOLVE-ATTACK -- identical
    dodge/damage/on-hit-effect handling to any other attacker, and a
    killed target becomes an inert corpse exactly like any other
    monster kill (though, like CONFUSED-ENTITY-TURN's own monster-on-
    monster kills, awards the player no XP, since the player didn't
    land the blow).
  - Else if one exists within *RDESCENT-COMPANION-AGGRO-RADIUS* (but
    out of melee reach), ENT takes a single A*-pathed step towards it
    (ASTAR-NEXT-STEP), mirroring PROCESS-ENEMY-TURNS' own
    hostile-approach step -- ENT will leave *RDESCENT-COMPANION-LEASH-
    RADIUS* to do this; the leash below only governs its idle,
    nothing-to-fight behavior.
  - Else if PLAYER is farther away than *RDESCENT-COMPANION-LEASH-
    RADIUS* (Chebyshev distance), ENT closes the distance: a single
    step towards the player's own current position via ASTAR-NEXT-
    STEP (its own independent search, unlike PROCESS-ENEMY-TURNS'
    hostile-approach step -- there is only ever one bonded companion,
    so the O(1)-per-monster FLOW-FIELD/FLOW-FIELD-NEXT-STEP shortcut's
    perf benefit doesn't matter here, and FLOW-FIELD's own bounded
    flood radius, sized only to cover *RDESCENT-MONSTER-FOV-RADIUS*-
    gated hostiles, is too small to guarantee an entry for a companion
    that has drifted arbitrarily far behind), exactly as before -- so
    Doge always catches back up if it ever falls behind.
  - Else (no hostile to chase, and already within leash range of
    PLAYER) ENT wanders a little instead of beelining onto the
    player's heels every turn: COMPANION-WANDER-STEP picks a single
    undirected step that stays within the leash, or no step at all if
    none qualifies (or CONFUSED-RANDOM-STEP itself 'chooses' to
    stagger in place) -- so a bonded Doge with nothing to fight feels
    like it's exploring nearby rather than shadowing the player's
    exact tile.
Either way COST (already chosen by PROCESS-ENEMY-TURNS -- the attack
or move cost, according to which of the above this turn turns out to
be) is deducted from ENT's ENERGY; any movement step is silently
skipped (COST still spent) if ASTAR-NEXT-STEP/FLOW-FIELD-NEXT-STEP/
COMPANION-WANDER-STEP finds no step at all (GOAL unreachable, every
route blocked by other entities, or no wander step satisfies every
constraint) or if its first step would land exactly on the player's
own tile -- mirroring PROCESS-ENEMY-TURNS' own hostile-approach guard
exactly, so Doge never tries to shove past or stand on top of its
owner.
VISIBLE-P -- whether ENT's own tile lay within the player's field of
view as of PROCESS-ENEMY-TURNS' up-front VISIBLE-MASK snapshot for
this tick (see its own docstring) -- gates only the attack-branch's
own combat messages (lunge-missed / bite-hit-or-kill): a Doge fighting
somewhere out of the player's sight still fights exactly the same
(damage, kills, ENERGY spend are all unaffected), but the player isn't
shown blow-by-blow text about a fight they can't actually see, mirroring
how any other unseen entity's actions are never narrated either."
  (let* ((target (nearest-hostile-to ent (get-entities state) level))
         (reach (weapon-reach (effective-weapon ent)))
         (target-distance (and target (max (abs (- (get-x target) (get-x ent)))
                                            (abs (- (get-y target) (get-y ent))))))
         (player-distance (max (abs (- (get-x player) (get-x ent)))
                               (abs (- (get-y player) (get-y ent))))))
    (cond
      ((and target (<= target-distance reach))
       (multiple-value-bind (damage dies new-target broken-item-names) (resolve-attack-volley ent target)
         (if (null damage)
             (update-game-state
              state
              :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                    ent (get-entities state))
              :message-log (if visible-p
                               (append-log-messages
                                (get-message-log state)
                                (list (make-log-message
                                       (format nil "The ~A lunges at the ~A but it ~A"
                                               (get-name ent) (get-name target) (random-dodge-phrase :third))
                                       (entity-message-color ent))))
                               (get-message-log state)))
             (let* ((new-target (if dies
                                    (update-entity new-target :char #\% :name (format nil "remains of ~A" (get-name target))
                                                              :blocks-movement nil :render-order 0 :is-alive nil)
                                    new-target))
                    (target-name (get-name target))
                    (drop (and dies (maybe-drop-monster-loot new-target)))
                    (messages
                      (append
                       (monster-death-drop-messages drop)
                       (build-combat-messages
                        (list (get-name ent) target-name) damage dies (entity-message-color ent)
                        "The ~A bites the ~A for ~D damage!"
                        "The ~A bites the ~A but does no damage!"
                        (make-log-message (format nil "The ~A collapses!" target-name) (entity-message-color ent)))
                       (item-break-messages broken-item-names))))
               (update-game-state
                state
                :entities (let ((updated (substitute new-target target
                                                      (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                                                  ent (get-entities state)))))
                            (if drop (cons drop updated) updated))
                :message-log (if visible-p
                                 (append-log-messages (get-message-log state) messages)
                                 (get-message-log state)))))))
      (t
       (let* ((chasing (and target (<= target-distance *rdescent-companion-aggro-radius*)))
              (wandering (and (not chasing) (<= player-distance *rdescent-companion-leash-radius*)))
              (remaining-energy (- (entity-energy ent) cost)))
         (multiple-value-bind (step-dx step-dy new-wander-ticks)
             (cond
               (wandering (companion-wander-step state ent map level player))
               (chasing (astar-next-step map (get-x ent) (get-y ent) (get-x target) (get-y target)
                                         :blocked-p (lambda (x y) (blocking-entity-at state x y level))))
               (t (astar-next-step map (get-x ent) (get-y ent) (get-x player) (get-y player)
                                   :blocked-p (lambda (x y) (blocking-entity-at state x y level)))))
           (let* ((step-x (and step-dx (+ (get-x ent) step-dx)))
                  (step-y (and step-dy (+ (get-y ent) step-dy)))
                  ;; Only WANDERING steps touch ENT's committed-direction
                  ;; slots (WANDER-DX/WANDER-DY/WANDER-TICKS) -- see
                  ;; COMPANION-WANDER-STEP's own docstring. Chasing/
                  ;; following via ASTAR-NEXT-STEP leaves them untouched
                  ;; so Doge resumes wherever it left off wandering
                  ;; (or simply re-rolls, if that direction is now
                  ;; stale/invalid) the next time it goes idle again.
                  (wander-args (when wandering
                                 (list :wander-dx (or step-dx 0) :wander-dy (or step-dy 0)
                                       :wander-ticks (or new-wander-ticks 0)))))
             (if (and step-x step-y
                      (not (and (= step-x (get-x player)) (= step-y (get-y player)))))
                 (update-game-state
                  state
                  :entities (substitute (apply #'update-entity ent :x step-x :y step-y :energy remaining-energy
                                                wander-args)
                                        ent (get-entities state)))
                 (update-game-state
                  state
                  :entities (substitute (apply #'update-entity ent :energy remaining-energy wander-args)
                                        ent (get-entities state)))))))))))

(defun process-enemy-turns (state tier level &key map visible-mask)
  "Return a fresh GAME-STATE derived from STATE in which every entity
in (GET-ENTITIES STATE) that shares the player's current LEVEL, is
IS-ALIVE, and currently lies within the player's field of view (per
COMPUTE-FOV, at *RDESCENT-MONSTER-FOV-RADIUS* from the player's
current position -- a fixed radius deliberately decoupled from the
player's own DOMAIN-KNOWLEDGE-scaled FOV, see that constant's
docstring -- entities in the dark can't see the player either, and
dead entities -- corpses -- never act) is offered a turn, threaded via
FOLD-LEFT so each enemy's state changes (damage dealt to the player,
its own movement, or its own ENERGY spend) are visible to the next
enemy processed in the same tick rather than being clobbered by it.
Each qualifying entity's turn is gated by its own accumulated
ENTITY-ENERGY, exactly like MOVE-PLAYER gates the player's. If ENT's
own ENTITY-CONFUSED-TICKS is > 0 (see CAST-REORG-MEMO/REORG-MEMO),
its turn is entirely CONFUSED-ENTITY-TURN's -- gated by the flat
*RDESCENT-MOVE-ENERGY-COST*, regardless of distance to the player --
staggering a random direction rather than the distance-gated
attack-or-approach logic described below, which only ever applies to
an unconfused entity. Otherwise, ENTITY-DISPOSITION-TOWARD (see
ARCHITECTURE_PLAN.md §2) gates which AI branch an unconfused ENT gets:
:FRIENDLY simply spends *RDESCENT-MOVE-ENERGY-COST* and does nothing
else (a placeholder for future quest-giver dialogue -- see
FUTURE_PLANS.md's NPCs & Quest Givers section); :NEUTRAL gets a
NEUTRAL-WANDER-TURN (a random, undirected stagger that never attacks
anything, reusing CONFUSED-RANDOM-STEP's movement -- an entity that
simply ignores the player); :FLEEING gets a FLEEING-TURN (a single
greedy step directly *away* from the player, the sign-flipped mirror
of the ordinary hostile-approach step below); and :HOSTILE (every
MAKE-ORC/MAKE-TROLL today, via ENEMY's own default) falls through to
the distance-gated attack-or-approach logic that existed before
FACTION/DISPOSITION at all, described next. A BONDED-COMPANION-P
Office Doge (FUTURE_PLANS.md §22) is dispatched to its own
COMPANION-AI-TURN before any of the above -- checked ahead of the
ordinary disposition switch since COMPANION defaults to :FRIENDLY,
which would otherwise send it down the same do-nothing branch other
NPCs get -- and a hostile monster that ends up within melee reach of
the player's bonded companion, but not of the player, attacks the
companion instead of approaching (see the ATTACKING-COMPANION branch
below); a killed companion becomes an inert corpse and its BONDED
slot is cleared, exactly like the player's own death is represented
differently from an ordinary monster's, so FIND-BONDED-COMPANION no
longer finds one and a fresh wild Doge can appear on some later level
(see USE-STAIRS/SPAWN-DOGE-FOR-LEVEL). Both COMPANION-AI-TURN's own
attack messages and ATTACKING-COMPANION's ordinary hit/no-damage blow-
by-blow text are only appended to MESSAGE-LOG when the companion's own
tile lay within VISIBLE-MASK as of this tick's up-front snapshot --
Doge fighting (or being fought) somewhere the player can't currently
see still fights exactly the same, it's simply not narrated, matching
how no other unseen entity's actions ever are; losing the companion
permanently is the one exception, always announced regardless of
visibility, mirroring \"You have died...\" always being shown for the
player's own death. For a :HOSTILE ENT,
Chebyshev
distance to the player's *current* position (as of that entity's own
turn, i.e. it sees any earlier enemy's effect on the player this tick)
determines whether the entity would attack (distance <= (WEAPON-REACH
(EFFECTIVE-WEAPON ENT)) -- today always 1, melee range including
diagonals, since no monster has any EQUIPMENT/EQUIPPED weapon yet; see
ARCHITECTURE_PLAN.md §4/§5) or move (distance beyond that reach),
which in turn determines the ENERGY cost of that turn --
*RDESCENT-ATTACK-ENERGY-COST* (150) or *RDESCENT-MOVE-ENERGY-COST*
(100) respectively. If the entity's ENTITY-ENERGY is below that cost,
it has not yet saved up enough to act this pass and its turn is
skipped entirely -- STATE is passed through unmodified for that entity
(no ENERGY is spent, and it tries again once REDUCE-TICK has topped it
up further). Otherwise the cost is deducted from the entity's ENERGY
(via UPDATE-ENTITY/SUBSTITUTE, alongside whatever else that turn
changes) and:
  - If attacking: first, EFFECTIVE-DODGE-CHANCE (PIVOT-DODGE-CHANCE,
    reduced while the player has an active :ANALYSIS-PARALYSIS
    STATUS-EFFECT -- FUTURE_PLANS.md §7) is rolled against the
    player -- if (RANDOM 100) is less than that
    percent, the attack is dodged entirely: 0 damage is dealt (no HP
    change) but a random dodge/parry/block/evade message (see RANDOM-
    DODGE-PHRASE) is still pushed so the player gets feedback, and ENERGY is
    still spent as normal. Otherwise, damage is (MAX 0 (- entity's
    EFFECTIVE-POWER player's EFFECTIVE-DEFENSE)) -- folding in any
    EQUIPMENT either side has -- then scaled by the player's own
    BANDWIDTH-DAMAGE-MULTIPLIER and rounded to the nearest integer; if
    that damage is > 0, the player's HP is reduced (via UPDATE-ENTITY)
    and a hit message is pushed -- ENTITY-ATTACK-FLAVOR-POOL/RANDOM-
    ATTACK-FLAVOR-TEXT picks a fresh random cosmetic flavor sentence
    for ENT's own monster type each attack (e.g. \"The Code Monkey
    flings poo at you! You take ~D damage!\" or \"The Troll CCs your
    manager! You take ~D damage!\") when ENT is a currently-flavored
    type (code monkey/Internet Troll), falling back to a random hit
    verb (see RANDOM-HIT-VERB, e.g. \"The ~A hits/strikes/kicks/
    elbows/claws at/punches/smacks you for ~D damage!\") for any other
    monster type. This flavor text is purely cosmetic for now -- laying the
    groundwork for future attacks that carry distinct mechanical
    effects (e.g. debuffs), see TECHNICAL_DEBT.md -- it does not
    currently change damage, hit chance, or anything else. If the
    player's new HP is <= 0, the player dies -- CHAR
    flips to #\\% and IS-ALIVE to NIL (players don't turn into a
    generic corpse ENTITY like monsters do; there's only ever one
    player, so RENDER-ORDER doesn't matter for it) and a \"You have
    died...\" message is pushed too. If damage is 0, the same flavor-
    or-fallback wording is still pushed but reporting no damage taken,
    for feedback.
  - If moving: the entity takes a single step towards the player's
    current position along a shortest-walkable-route-consistent
    direction, computed by FLOW-FIELD-NEXT-STEP against FLOW-FIELD (a
    PLAYER-FLOW-FIELD flood-filled once, up front, from the player's
    own position -- shared read-only by every hostile's own approach
    step this tick, rather than each running its own independent A*
    search: see PLAYER-FLOW-FIELD's own docstring for why this
    matters once many hostiles are pursuing at once) -- treating any
    cell with a BLOCKING-ENTITY-AT as impassable, exactly like the
    ASTAR-NEXT-STEP-based version this replaced. This still routes
    *around* an obstacle sitting directly on the straight diagonal
    line to the player (an L-shaped corridor bend, or a doorway
    approached diagonally) rather than the old greedy (SIGNUM DX)/
    (SIGNUM DY) single-step heuristic's own failure mode, which had no
    way to do so and so could leave a monster permanently wedged
    against a wall corner. If FLOW-FIELD-NEXT-STEP finds a step (a
    path exists within FLOW-FIELD's own flooded radius) and it doesn't
    land exactly on the player's own tile, the entity's ENTITY is
    replaced with one at the new position (its ENERGY cost still
    deducted); otherwise it stays
    put this turn (no message -- movement is silent, matching
    MOVE-PLAYER on an unrecognized/blocked step) but its ENERGY cost
    is still spent, since the AI did commit to (attempt) a move this
    turn.
MAP and VISIBLE-MASK are optional pre-computed values -- if the caller
(e.g. APPLY-RDESCENT-COMMAND, via MOVE-PLAYER's extra return values)
already generated TIER/LEVEL's dungeon and computed the player's
current field of view for this same command, it can pass them in here
directly rather than this function recomputing both from scratch (see
TECHNICAL_DEBT.md item #31). When omitted, both are computed here
exactly as before, so this function remains independently correct and
callable on its own (as the existing unit tests do). Note VISIBLE-MASK
is computed once, up front, from the player's position *before* any
enemy in this call has acted -- an enemy that moves into or out of
sight this same tick is still processed according to its visibility at
the start of the tick, not after every intermediate step.
MESSAGE-LOG is newest-first (see GAME-STATE's docstring); this
function pushes each enemy's own message(s) onto it as that enemy's
turn is processed (in list order, the first entity in GET-ENTITIES
first), so with a plain (state, message) push per turn, the *first*
entity processed naturally ends up as the most recent/topmost message,
matching PROCESS-ENEMY-TURNS' historical (pre-combat) ordering
convention and the order entities are iterated everywhere else in this
file (e.g. RENDER-GRID)."
  (let* ((player0 (get-player state))
         (level* (get-level player0))
         (map (or map (generate-dungeon tier level)))
         (visible-mask (or visible-mask
                           (compute-fov map (get-x player0) (get-y player0) *rdescent-monster-fov-radius*)))
         (width *rdescent-field-width*)
         (flow-field (player-flow-field map (get-x player0) (get-y player0)))
         (acting-entities
           (remove-if-not (lambda (ent)
                            (and (= (get-level ent) level*)
                                 (is-alive ent)
                                 (or (companion-p ent)
                                     (= 1 (bit visible-mask (xy-to-index (get-x ent) (get-y ent) width))))))
                          (get-entities state))))
    (fold-left
     (lambda (state ent)
       (let* ((player (get-player state))
              (disposition (entity-disposition-toward ent player))
              (confused (plusp (entity-confused-ticks ent)))
              (companion (bonded-companion-p ent))
              (dx (- (get-x player) (get-x ent)))
              (dy (- (get-y player) (get-y ent)))
              (distance (max (abs dx) (abs dy)))
              (attacking (and (not confused)
                              (not companion)
                              (eq disposition :hostile)
                              (<= distance (weapon-reach (effective-weapon ent)))))
              (companion-target (and companion (nearest-hostile-to ent (get-entities state) level*)))
              (companion-target-distance (and companion-target
                                              (max (abs (- (get-x companion-target) (get-x ent)))
                                                   (abs (- (get-y companion-target) (get-y ent))))))
              (companion-attacking (and companion-target
                                        (<= companion-target-distance (weapon-reach (effective-weapon ent)))))
              (bonded-companion (and (not companion) (find-bonded-companion (get-entities state))))
              (distance-to-companion (and bonded-companion
                                          (is-alive bonded-companion)
                                          (= (get-level bonded-companion) level*)
                                          (max (abs (- (get-x bonded-companion) (get-x ent)))
                                               (abs (- (get-y bonded-companion) (get-y ent))))))
              (attacking-companion (and (not confused)
                                        (not companion)
                                        (not attacking)
                                        (eq disposition :hostile)
                                        distance-to-companion
                                        (<= distance-to-companion (weapon-reach (effective-weapon ent)))))
              (companion-visible-p (and bonded-companion
                                        (= 1 (bit visible-mask
                                                  (xy-to-index (get-x bonded-companion) (get-y bonded-companion) width)))))
              (cost (cond (confused *rdescent-move-energy-cost*)
                          (companion (if companion-attacking
                                         (effective-attack-energy-cost ent)
                                         *rdescent-move-energy-cost*))
                          (attacking (effective-attack-energy-cost ent))
                          (attacking-companion (effective-attack-energy-cost ent))
                          (t *rdescent-move-energy-cost*))))
         (cond
           ((not (is-alive player)) state)
           ((< (entity-energy ent) cost) state)
           ((entity-effect ent :stunned)
            (update-game-state
             state
             :entities (substitute (update-entity (entity-with-effect ent :stunned 0)
                                                  :energy (- (entity-energy ent) cost))
                                   ent
                                   (get-entities state))))
           (confused
            (confused-entity-turn state ent map level* cost))
           (companion
            (companion-ai-turn state ent map level* cost player
                               (= 1 (bit visible-mask (xy-to-index (get-x ent) (get-y ent) width)))))
           ((eq disposition :friendly)
            (update-game-state
             state
             :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                   ent
                                   (get-entities state))))
           ((eq disposition :neutral)
            (neutral-wander-turn state ent map level* cost))
           ((eq disposition :fleeing)
            (fleeing-turn state ent map level* cost dx dy))
           (attacking
            (multiple-value-bind (flavor on-hit-effect always-effect force-no-damage) (random-attack-flavor-text ent)
              (if force-no-damage
                  (let* ((new-player (if always-effect
                                         (apply-status-effect player (getf always-effect :kind)
                                                              (getf always-effect :turns) (getf always-effect :magnitude))
                                         player))
                         (callout-messages (status-effect-callout-messages player new-player)))
                    (update-game-state
                     state
                     :player new-player
                     :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                           ent
                                           (get-entities state))
                     :message-log (append-log-messages
                                   (get-message-log state)
                                   (append callout-messages
                                           (list (make-log-message (format nil "~A You take no damage!" flavor)
                                                                   (entity-message-color ent)))))))
                  (multiple-value-bind (damage dies resolved-player broken-item-names) (resolve-attack-on-player ent player)
                    (if (null damage)
                        (update-game-state
                         state
                         :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                               ent
                                               (get-entities state))
                         :message-log (append-log-messages
                                       (get-message-log state)
                                       (list (make-log-message
                                              (format nil "The ~A attacks, but you ~A" (get-name ent) (random-dodge-phrase :second))
                                              (entity-message-color ent)))))
                        (multiple-value-bind (rescued-state rescued-player yubikey-saved-p save-message)
                            (maybe-trigger-yubikey-save state resolved-player map level*)
                          (let* ((new-player (if (and on-hit-effect (> damage 0) (not dies)
                                                      (< (random 1.0) (or (getf on-hit-effect :chance) 1.0)))
                                                 (apply-status-effect rescued-player (getf on-hit-effect :kind)
                                                                      (getf on-hit-effect :turns) (getf on-hit-effect :magnitude))
                                                 rescued-player))
                                 (callout-messages (status-effect-callout-messages player new-player))
                                 (messages
                                   (append
                                    callout-messages
                                    (let ((color (entity-message-color ent)))
                                      (if flavor
                                          (build-combat-messages
                                           flavor damage (and dies (not yubikey-saved-p)) color
                                           "~A You take ~D damage!"
                                           "~A You take no damage!"
                                           "You have died...")
                                          (let ((verb (random-hit-verb :third)))
                                            (build-combat-messages
                                             (get-name ent) damage (and dies (not yubikey-saved-p)) color
                                             (concatenate 'string "The ~A " verb " you for ~D damage!")
                                             (concatenate 'string "The ~A " verb " you but does no damage!")
                                             "You have died..."))))
                                    (item-break-messages broken-item-names)
                                    (if yubikey-saved-p
                                        (list (make-log-message
                                               save-message
                                               *rdescent-status-effect-callout-color*))
                                        nil))))
                            (update-game-state
                             rescued-state
                             :player new-player
                             :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                                   ent
                                                   (get-entities rescued-state))
                             :message-log (append-log-messages (get-message-log rescued-state) messages)))))))))
           (attacking-companion
            (multiple-value-bind (damage dies new-companion broken-item-names) (resolve-attack-volley ent bonded-companion)
              (if (null damage)
                  (update-game-state
                   state
                   :entities (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                         ent
                                         (get-entities state))
                   :message-log (if companion-visible-p
                                    (append-log-messages
                                     (get-message-log state)
                                     (list (make-log-message
                                            (format nil "The ~A attacks your ~A, but it ~A"
                                                    (get-name ent) (get-name bonded-companion) (random-dodge-phrase :third))
                                            (entity-message-color ent))))
                                    (get-message-log state)))
                  (let* ((new-companion (if dies
                                            (update-entity new-companion :char #\% :name (format nil "remains of ~A" (get-name bonded-companion))
                                                                         :blocks-movement nil :render-order 0 :is-alive nil :bonded nil)
                                            new-companion))
                         (companion-name (get-name bonded-companion))
                         (verb (random-hit-verb :third))
                         (messages
                           (append
                            (build-combat-messages
                             (list (get-name ent) companion-name) damage dies (entity-message-color ent)
                             (concatenate 'string "The ~A " verb " your ~A for ~D damage!")
                             (concatenate 'string "The ~A " verb " your ~A but it does no damage!")
                             (make-log-message (format nil "Your ~A has been let go..." companion-name)
                                               (entity-message-color ent)))
                            (item-break-messages broken-item-names)))
                         ;; Losing a bonded companion permanently is
                         ;; always announced (mirroring "You have
                         ;; died..." always being shown) even if it
                         ;; happened out of sight; the ordinary
                         ;; hit/no-damage blow-by-blow text is not.
                         (messages (if companion-visible-p
                                      messages
                                      (and dies (list (first messages))))))
                    (update-game-state
                     state
                     :entities (substitute new-companion bonded-companion
                                           (substitute (update-entity ent :energy (- (entity-energy ent) cost))
                                                       ent
                                                       (get-entities state)))
                     :message-log (append-log-messages (get-message-log state) messages))))))
           (t
            (let ((remaining-energy (- (entity-energy ent) cost)))
              (multiple-value-bind (step-dx step-dy)
                  (flow-field-next-step map flow-field (get-x ent) (get-y ent) (get-x player) (get-y player)
                                       :blocked-p (lambda (x y) (blocking-entity-at state x y level*)))
                (let* ((step-x (and step-dx (+ (get-x ent) step-dx)))
                       (step-y (and step-dy (+ (get-y ent) step-dy))))
                  (update-game-state
                   state
                   :entities (substitute (if (and step-x step-y
                                                 (not (and (= step-x (get-x player))
                                                           (= step-y (get-y player)))))
                                            (update-entity ent :x step-x :y step-y :energy remaining-energy)
                                            (update-entity ent :energy remaining-energy))
                                        ent
                                        (get-entities state))))))))))
     state
     (reverse acting-entities))))

(defgeneric execute-immediate-command (command state tier level max-depth)
  (:documentation "Execute COMMAND's action against STATE (given the
player's current TIER/LEVEL and MAX-DEPTH), then run PROCESS-ENEMY-
TURNS, returning the resulting fresh GAME-STATE. This is
APPLY-RDESCENT-COMMAND's dispatcher: one method per concrete
RDESCENT-COMMAND subclass, plus a NULL fallback (for a missing/
unrecognized command) that simply runs PROCESS-ENEMY-TURNS with no
preceding action. Unlike EXECUTE-QUEUED-COMMAND, every method here
invokes PROCESS-ENEMY-TURNS itself, matching APPLY-RDESCENT-COMMAND's
historical single-call move-then-let-the-monsters-immediately-react
semantics."))

(defmethod execute-immediate-command ((command drink-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (let ((moved-state (drink-potion state)))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command drop-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (let ((moved-state (drop-item state (item-index command))))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command purchase-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (let ((moved-state (purchase-item state tier (item-index command))))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command equip-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (let ((moved-state (equip-item state (item-index command))))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command grab-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (let ((moved-state (grab-item state tier)))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command interact-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (let ((moved-state (interact-fixture state)))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command move-command) state tier level max-depth)
  (declare (ignore max-depth))
  ;; MOVE-PLAYER's extra MAP return value is threaded straight into
  ;; PROCESS-ENEMY-TURNS instead of it recomputing the same dungeon
  ;; from scratch for what is almost always the same tier/level within
  ;; this single command (see TECHNICAL_DEBT.md item #31). Its
  ;; VISIBLE-MASK return value is NOT reused, though: that mask is
  ;; computed at the player's own DOMAIN-KNOWLEDGE-scaled FOV radius,
  ;; whereas PROCESS-ENEMY-TURNS needs one at the fixed
  ;; *RDESCENT-MONSTER-FOV-RADIUS* -- the two are no longer the same
  ;; concept, so PROCESS-ENEMY-TURNS must recompute its own.
  (multiple-value-bind (moved-state map visible-mask)
      (move-player state tier level (command-direction command))
    (declare (ignore visible-mask))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state))
                         :map map)))

(defmethod execute-immediate-command ((command restore-command) state tier level max-depth)
  (declare (ignore state level max-depth))
  (let ((restored (unpack-save-state (command-payload command))))
    (process-enemy-turns restored tier (get-level (get-player restored)))))

(defmethod execute-immediate-command ((command unequip-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (let ((moved-state (unequip-item state (command-slot command))))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command use-item-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (let ((moved-state (use-item state (item-index command)
                                (target-x command) (target-y command))))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command use-stairs-command) state tier level max-depth)
  ;; USE-STAIRS may change LEVEL entirely (a depth transition), so
  ;; PROCESS-ENEMY-TURNS is always re-pointed at GET-PLAYER's own
  ;; (possibly new) LEVEL afterward rather than the LEVEL this call was
  ;; originally given.
  (let ((moved-state (use-stairs state tier level max-depth)))
    (process-enemy-turns moved-state tier (get-level (get-player moved-state)))))

(defmethod execute-immediate-command ((command null) state tier level max-depth)
  (declare (ignore level max-depth))
  (process-enemy-turns state tier (get-level (get-player state))))

(defun apply-rdescent-command (state tier level command &optional (max-depth (rdescent-tier-max-depth tier)))
  "Return the GAME-STATE that results from applying COMMAND to STATE,
given the player's current TIER and LEVEL (needed by MOVE-PLAYER to
collide against the correct dungeon) and MAX-DEPTH (needed by
USE-STAIRS to gate descent -- defaults to RDESCENT-TIER-MAX-DEPTH of
TIER, so existing callers that only ever operated within a single
tier's own cap need not pass it explicitly). Dispatches to
EXECUTE-IMMEDIATE-COMMAND, a generic function specialized on COMMAND's
own class (or on NULL, for a missing/unrecognized command), whose
methods each run the command's specific action followed by
PROCESS-ENEMY-TURNS -- see that generic function's docstring and each
method's own for exact semantics. Pure: a reducer in the (state,
command) -> state shape. If the game is already over (GAME-ACTIVE-P is
NIL -- the player is dead, or some future GAME-OVER-REASON flag is set,
e.g. from a fatal hit taken during a previous PROCESS-ENEMY-TURNS
call), STATE is returned completely unchanged and neither the
command's action nor PROCESS-ENEMY-TURNS runs at all -- a corpse can't
move, and dead players don't get to keep taking their monsters' turns
for them either; the game is simply over until a fresh GAME-STATE
(e.g. a new connection) replaces this one."
  (if (game-active-p state)
      (execute-immediate-command command state tier level max-depth)
      state))

(defgeneric execute-queued-command (command state tier level max-depth)
  (:documentation "Execute COMMAND's action against STATE (given the
player's current TIER/LEVEL and MAX-DEPTH), returning the resulting
fresh GAME-STATE. This is APPLY-PLAYER-COMMAND's dispatcher: one
method per concrete RDESCENT-COMMAND subclass, plus a NULL fallback
(for a missing/unrecognized command) that simply returns STATE
unchanged. Unlike EXECUTE-IMMEDIATE-COMMAND, none of these methods run
PROCESS-ENEMY-TURNS -- ADVANCE-GAME-STATE triggers enemy AI only once
per heartbeat, after every queued command has been applied (see
APPLY-PLAYER-COMMAND's own docstring)."))

(defmethod execute-queued-command ((command drink-command) state tier level max-depth)
  (declare (ignore tier level max-depth))
  (drink-potion state))

(defmethod execute-queued-command ((command drop-command) state tier level max-depth)
  (declare (ignore tier level max-depth))
  (drop-item state (item-index command)))

(defmethod execute-queued-command ((command purchase-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (purchase-item state tier (item-index command)))

(defmethod execute-queued-command ((command equip-command) state tier level max-depth)
  (declare (ignore tier level max-depth))
  (equip-item state (item-index command)))

(defmethod execute-queued-command ((command grab-command) state tier level max-depth)
  (declare (ignore level max-depth))
  (grab-item state tier))

(defmethod execute-queued-command ((command interact-command) state tier level max-depth)
  (declare (ignore tier level max-depth))
  (interact-fixture state))

(defmethod execute-queued-command ((command message-log-command) state tier level max-depth)
  (declare (ignore tier level max-depth))
  (update-game-state
   state
   :message-log (append-log-messages (get-message-log state)
                                     (list (make-log-message (sanitize-message-log-text (command-text command)))))))

(defmethod execute-queued-command ((command move-command) state tier level max-depth)
  (declare (ignore max-depth))
  (move-player state tier level (command-direction command)))

(defmethod execute-queued-command ((command restore-command) state tier level max-depth)
  (declare (ignore state tier level max-depth))
  (unpack-save-state (command-payload command)))

(defmethod execute-queued-command ((command use-item-command) state tier level max-depth)
  (declare (ignore tier level max-depth))
  (use-item state (item-index command) (target-x command) (target-y command)))

(defmethod execute-queued-command ((command use-stairs-command) state tier level max-depth)
  (use-stairs state tier level max-depth))

(defmethod execute-queued-command ((command unequip-command) state tier level max-depth)
  (declare (ignore tier level max-depth))
  (unequip-item state (command-slot command)))

(defmethod execute-queued-command ((command null) state tier level max-depth)
  (declare (ignore tier level max-depth))
  state)

(defun apply-player-command (state tier level command &optional (max-depth (rdescent-tier-max-depth tier)))
  "Return the GAME-STATE that results from applying a single
player-input COMMAND (an RDESCENT-COMMAND, or NIL) to STATE's own
PLAYER, given the player's current TIER and LEVEL (needed to collide
against/generate the correct dungeon) and MAX-DEPTH (needed by
USE-STAIRS to gate descent -- defaults to RDESCENT-TIER-MAX-DEPTH of
TIER). Dispatches to EXECUTE-QUEUED-COMMAND, a generic function
specialized on COMMAND's own class (or on NULL, for a missing/
unrecognized command, which simply returns STATE unchanged) -- see
that generic function's docstring and each method's own for exact
semantics.
Unlike APPLY-RDESCENT-COMMAND, this function never itself runs
PROCESS-ENEMY-TURNS: ADVANCE-GAME-STATE folds this function over every
RDESCENT-COMMAND a client's INPUT-QUEUE accumulated since the previous
game-loop heartbeat (see TICK-GAME-STATE, RDESCENT/SERVER.LISP), so a
burst of queued moves -- a player mashing input faster than the tick
rate -- is applied one at a time, each still gated by (and spending)
the player's own accumulated ENERGY exactly as a lone MOVE-PLAYER call
would (see *RDESCENT-MOVE-ENERGY-COST*/*RDESCENT-ATTACK-ENERGY-COST*),
with enemy AI triggered only once per heartbeat by ADVANCE-GAME-STATE's
own trailing PROCESS-ENEMY-TURNS call rather than once per queued
command. If the game is already over (GAME-ACTIVE-P is NIL), STATE is
returned unchanged -- a corpse can't move."
  (if (game-active-p state)
      (execute-queued-command command state tier level max-depth)
      state))

(defun advance-game-state (state tier commands &optional (max-depth (rdescent-tier-max-depth tier)))
  "Return the fresh GAME-STATE that results from applying one full
game-loop heartbeat to STATE: every RDESCENT-COMMAND in COMMANDS
(oldest first -- as drained from a client's own INPUT-QUEUE by
RDESCENT-DRAIN-INPUT-QUEUE/TICK-GAME-STATE, RDESCENT/SERVER.LISP) is
folded over STATE via APPLY-PLAYER-COMMAND (given MAX-DEPTH, this
connection's JWT-derived depth ceiling, defaulting to
RDESCENT-TIER-MAX-DEPTH of TIER -- see TICK-GAME-STATE, which passes
its client's own MAX-DEPTH explicitly), so a player mashing input
faster than the tick rate gets every one of those queued commands
applied in order -- each still individually gated by (and spending)
the player's own accumulated ENERGY -- rather than only the most
recent one being honored. Once every queued command has been applied,
REDUCE-TICK runs exactly once, accruing every entity's ENERGY (the
player included) by its own ENTITY-SPEED, followed by exactly one
PROCESS-ENEMY-TURNS pass, letting any monster that has saved up enough
ENERGY act.
This is the pure reducer TICK-GAME-STATE applies once per
*RDESCENT-TICK-SECONDS* heartbeat, for each connected client
independently -- it, not APPLY-RDESCENT-COMMAND, is production's real
per-tick entry point now that TEXT-MESSAGE-RECEIVED only enqueues
commands rather than applying them immediately (APPLY-RDESCENT-COMMAND
itself remains for any caller -- and the existing unit tests -- that
still wants a single command's move-then-let-the-monsters-immediately-
react semantics in one call, e.g. outside the queued/heartbeat model).
If the game is already over (GAME-ACTIVE-P is NIL) when this is
called, neither the queued COMMANDS, REDUCE-TICK, nor
PROCESS-ENEMY-TURNS run -- matching APPLY-RDESCENT-COMMAND's own
short-circuit -- but STATE is not simply returned unchanged: its own
:DEAD-TICKS flag (defaulting to 0) is incremented by one, so a corpse
that keeps ticking through successive dead heartbeats is counted.
Once that count reaches 100, this function reincarnates the player
instead of returning STATE at all -- MAKE-INITIAL-STATE builds a fresh
GAME-STATE (dropping TIER's now-concluded run entirely, including
:DEAD-TICKS itself, which is absent/0 on a fresh state), with a
\"*** Reincarnated! ***\" MESSAGE-LOG entry prepended so the client
sees why its whole game state just reset.
The trailing PROCESS-ENEMY-TURNS pass is likewise skipped -- returning
TICKED-STATE as-is -- if REDUCE-TICK's own TICK-STATUS-EFFECTS just
killed the player this same heartbeat (GAME-ACTIVE-P false on
TICKED-STATE even though it was true on STATE), or, once some future
feature sets one, if REDUCE-TICK's tick just set a terminal
GAME-OVER-REASON flag directly (e.g. a doom-clock deadline) -- either
way, a monster doesn't get to take a free turn against an already-
concluded run. In that branch, TICKED-STATE's own :DEAD-TICKS flag is
reset to 0 first, so the 100-heartbeat reincarnation countdown always
starts counting from this run's moment of death, not from some earlier
run's leftover count."
  (if (game-active-p state)
      (let* ((moved-state (fold-left (lambda (s command)
                                       (apply-player-command s tier (get-level (get-player s)) command max-depth))
                                     state commands))
             (ticked-state (reduce-tick moved-state)))
        (if (game-active-p ticked-state)
            (process-enemy-turns ticked-state tier (get-level (get-player ticked-state)))
            (update-game-state ticked-state :flags (fset:with (get-flags ticked-state) :dead-ticks 0))))
      (let* ((flags (get-flags state))
             (dead-ticks (or (fset:lookup flags :dead-ticks) 0))
             (next-ticks (1+ dead-ticks)))
        (if (>= next-ticks 100)
            (let ((new-state (make-initial-state tier)))
              (update-game-state new-state 
                                 :message-log (append-log-messages 
                                               (get-message-log new-state) 
                                               (list (make-log-message "*** Reincarnated! ***" "#bf616a")))))
            (update-game-state state :flags (fset:with flags :dead-ticks next-ticks))))))
