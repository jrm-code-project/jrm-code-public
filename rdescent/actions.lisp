;;; -*- Lisp -*-

;;; The pure player-action reducers that turn a GAME-STATE plus a
;;; single action's parameters into a new GAME-STATE: MOVE-PLAYER,
;;; USE-STAIRS, DRINK-POTION, CAST-REORG-MEMO/USE-ITEM, GRAB-ITEM,
;;; DROP-ITEM, and INTERACT-FIXTURE/INTERACT-SHRINE, plus the small
;;; position-lookup predicates they share (DIRECTION-DELTA/
;;; BLOCKING-ENTITY-AT/STAIRS-ENTITY-AT/GROUND-ITEM-AT/FIXTURE-AT).
;;;
;;; Fourth of several files this engine was split across (originally a
;;; single ENGINE.LISP) -- see RDESCENT/ENTITIES.LISP's own header
;;; comment for the full file map. These reducers call into
;;; RDESCENT/DUNGEON.LISP (GENERATE-DUNGEON/COMPUTE-FOV) and
;;; RDESCENT/MECHANICS.LISP (message-log helpers, REDUCE-TICK) as well
;;; as RDESCENT/ENTITIES.LISP's own value types. See
;;; RDESCENT/COMMANDS.LISP for the command-dispatch layer built on top
;;; of these reducers, and for PROCESS-ENEMY-TURNS/combat resolution.

(in-package "JRM-CODE-PROJECT")

(defun direction-delta (direction)
  "Return (VALUES DX DY) for DIRECTION (one of the strings \"up\",
\"down\", \"left\", or \"right\"), or (VALUES 0 0) for any other,
unrecognized string. Pure lookup, factored out of MOVE-PLAYER so the
direction -> displacement mapping can be tested and read independently
of the bounds/collision logic that consumes it."
  (cond
    ((string= direction "up") (values 0 -1))
    ((string= direction "down") (values 0 1))
    ((string= direction "left") (values -1 0))
    ((string= direction "right") (values 1 0))
    (t (values 0 0))))

(defvar *entities-spatial-index-cache*
  (make-hash-table :test 'eq :weakness :key)
  "A memoization cache mapping an ENTITIES list (as returned by
GET-ENTITIES on some GAME-STATE, compared by EQ/object identity) to
its own per-tile spatial index -- an EQL hash table from XY-TO-INDEX's
flat cell index to the list of entities occupying that cell. See
ENTITIES-SPATIAL-INDEX/ENTITIES-AT-CELL (below), which BLOCKING-
ENTITY-AT/STAIRS-ENTITY-AT/GROUND-ITEM-AT/TRAP-AT/AUTO-PICKUP-ITEM-AT/
FIXTURE-AT all now query instead of linearly scanning the full
ENTITIES list -- a real cost once a level's floor has accumulated many
dropped items/corpses, since every one of those lookups runs at least
once per monster's own turn, every single game tick.

Safe to key by EQ/object identity because every GAME-STATE mutation in
this codebase goes through UPDATE-GAME-STATE, which always replaces
:ENTITIES with a *fresh* list object (via SUBSTITUTE/REMOVE/CONS/
APPEND) rather than mutating an existing list in place -- see GAME-
STATE's own class docstring (\"never mutated in place\"). So if two
calls see an EQ-identical ENTITIES list, its contents are guaranteed
unchanged, meaning a cached index for it can never go stale. Using
:WEAKNESS :KEY means a superseded ENTITIES list (and its now-useless
cached index) becomes eligible for GC as soon as nothing else
references it anymore -- typically within the very same tick that
replaced it -- so this cache can never accumulate stale entries for
state that no longer exists, and needs no manual invalidation
anywhere.")

(defun entities-spatial-index (entities)
  "Return (building and caching if not already cached) ENTITIES' own
per-tile spatial index: an EQL hash table mapping XY-TO-INDEX's flat
cell index to the list of entities in ENTITIES occupying that cell, in
the same relative order as ENTITIES itself. See *ENTITIES-SPATIAL-
INDEX-CACHE*'s own docstring for why memoizing by EQ/object identity
is safe.

Building the index from scratch is O(N) in (LENGTH ENTITIES), the same
single pass as a linear scan -- but every subsequent call for the same
ENTITIES list object is O(1), amortizing that one-time cost across
every lookup a single game tick performs against it (often many: one
or more per monster's own turn). This is what turns BLOCKING-ENTITY-
AT/STAIRS-ENTITY-AT/GROUND-ITEM-AT/TRAP-AT/AUTO-PICKUP-ITEM-AT/
FIXTURE-AT from an O(N) scan apiece into an O(1) lookup apiece, without
requiring any other code in the engine to remember to keep a separate
spatial structure in sync -- this index is purely *derived* from
ENTITIES on demand, the same way RENDER-GRID derives HTML from
GAME-STATE, never manually threaded through or updated anywhere else."
  (or (gethash entities *entities-spatial-index-cache*)
      (let ((index (make-hash-table :test 'eql)))
        (dolist (ent (reverse entities))
          (push ent (gethash (xy-to-index (get-x ent) (get-y ent)) index)))
        (setf (gethash entities *entities-spatial-index-cache*) index))))

(defun entities-at-cell (state x y)
  "Return the (typically empty, or very short -- 0, 1, or occasionally
2-3 when e.g. a monster stands on a dropped item) list of entities in
(GET-ENTITIES STATE) at (X, Y), looked up via ENTITIES-SPATIAL-INDEX
rather than scanning every entity on the level. Order matches
GET-ENTITIES' own relative order among same-cell entities. Every one
of BLOCKING-ENTITY-AT/STAIRS-ENTITY-AT/GROUND-ITEM-AT/TRAP-AT/AUTO-
PICKUP-ITEM-AT/FIXTURE-AT calls this instead of FIND-IF-ing directly
over (GET-ENTITIES STATE), so each only ever has to filter this tiny
per-cell bucket rather than the whole level's entity list."
  (gethash (xy-to-index x y) (entities-spatial-index (get-entities state))))

(defun stairs-entity-at (state x y level)
  "Return the first entity in (GET-ENTITIES STATE) at (X, Y) on LEVEL
whose NAME is \"Stairs Up\" or \"Stairs Down\" (see MAKE-STAIRS-UP/
MAKE-STAIRS-DOWN), or NIL if none is found. Unlike BLOCKING-ENTITY-AT,
this deliberately does NOT filter on GET-BLOCKS-MOVEMENT -- stairs are
BLOCKS-MOVEMENT NIL, purely a marker tile the player walks onto -- so
this is USE-STAIRS' own lookup for \"is the player currently standing
on a staircase\", independent of (and safely coexisting with) the
collision lookup MOVE-PLAYER uses."
  (find-if (lambda (ent) (and (= (get-level ent) level)
                              (member (get-name ent) '("Stairs Up" "Stairs Down") :test #'string=)))
           (entities-at-cell state x y)))

(defun blocking-entity-at (state x y level)
  "Return the first entity in (GET-ENTITIES STATE) at (X, Y) on LEVEL
whose GET-BLOCKS-MOVEMENT is true, or NIL if none is found (an empty
cell, or one occupied only by non-blocking entities). Used by
MOVE-PLAYER to decide whether a step is a normal walk, a melee bump, or
(implicitly, since a blocking entity can never share a tile with a
wall) simply impossible."
  (find-if (lambda (ent) (and (= (get-level ent) level)
                              (get-blocks-movement ent)))
           (entities-at-cell state x y)))

(defun ground-item-at (state x y level)
  "Return the first GROUND-ITEM in (GET-ENTITIES STATE) at (X, Y) on
LEVEL, or NIL if none is found. Used by GRAB-ITEM to look up whatever
loot the player is currently standing on. Unlike BLOCKING-ENTITY-AT,
this filters on the entity's own class rather than GET-BLOCKS-MOVEMENT
-- GROUND-ITEMs are always BLOCKS-MOVEMENT NIL, so BLOCKING-ENTITY-AT
would never find one anyway -- and unlike STAIRS-ENTITY-AT, this
checks TYPEP rather than NAME, since a GROUND-ITEM's own NAME varies
per payload (\"Kombucha\", \"Scroll of PIP\", \"Reply-All Bomb\", ...)
rather than being one of a small fixed set of strings."
  (find-if (lambda (ent) (and (= (get-level ent) level)
                              (typep ent 'ground-item)))
           (entities-at-cell state x y)))

(defun trap-at (state x y level)
  "Return the TRAP-FIXTURE in (GET-ENTITIES STATE) at (X, Y) on LEVEL,
or NIL if none is found (FUTURE_PLANS.md §8). Mirrors GROUND-ITEM-AT:
filters on TYPEP rather than GET-BLOCKS-MOVEMENT (a TRAP-FIXTURE, like
any other FIXTURE, is always BLOCKS-MOVEMENT NIL, so BLOCKING-ENTITY-AT
would never find one) or NAME (only one archetype -- MAKE-BROKEN-
DEPLOYMENT-TRAP -- exists so far, but future ones would each have their
own NAME). Deliberately returns a still-HIDDEN-P trap exactly the same
as an already-revealed one -- MOVE-PLAYER's own open-floor branch, the
only caller, needs to find (and trigger) a trap regardless of whether
the player has spotted it yet; concealment only ever affects RENDER-
GRID's own drawing, never collision/trigger logic."
  (find-if (lambda (ent) (and (= (get-level ent) level)
                              (typep ent 'trap-fixture)))
           (entities-at-cell state x y)))

(defun auto-pickup-item-at (state x y level)
  "Return the AUTO-PICKUP-ITEM in (GET-ENTITIES STATE) at (X, Y) on
LEVEL, or NIL if none is found (FUTURE_PLANS.md §16). Mirrors
GROUND-ITEM-AT/TRAP-AT: filters on TYPEP rather than GET-BLOCKS-
MOVEMENT (an AUTO-PICKUP-ITEM, like a GROUND-ITEM, is always
BLOCKS-MOVEMENT NIL) or NAME (each of the 23 *RDESCENT-COLLECTIBLE-
CATALOG* entries has its own distinct NAME). Used by
MAYBE-AUTO-PICKUP-COLLECTIBLE to look up whatever collectible the
player's own final position for this turn landed on."
  (find-if (lambda (ent) (and (= (get-level ent) level)
                              (typep ent 'auto-pickup-item)))
           (entities-at-cell state x y)))

(defun maybe-reveal-hidden-entities (state visible-mask)
  "Return a fresh GAME-STATE derived from STATE with every currently
HIDDEN-P TRAP-FIXTURE on the player's own current LEVEL that falls
within VISIBLE-MASK (the FOV bit-vector, indexed via XY-TO-INDEX, that
MOVE-PLAYER already computed for the player's final position this
turn) independently rolled against SENIORITY-DETECTION-CHANCE of the
player's own SENIORITY Corporate RPG Stat (FUTURE_PLANS.md §8's own
Detection Chance formula) -- a successful roll flips that TRAP-
FIXTURE's own HIDDEN-P to NIL (via UPDATE-ENTITY, SUBSTITUTEd into
STATE's own ENTITIES list), discovering -- but not triggering -- the
trap; RENDER-GRID (RDESCENT/SERVER.LISP) draws its CHAR from then on.
An already-revealed trap, one on a different LEVEL, or one outside
VISIBLE-MASK, is left completely untouched, and so is every non-TRAP-
FIXTURE entity. If no roll actually succeeds this call (the common
case -- most turns have no still-hidden trap in sight at all), STATE
itself is returned completely unchanged (EQ), not merely an
UPDATE-GAME-STATE-produced equivalent copy -- preserving MOVE-PLAYER-
INNER's own existing \"no-op branches return the identical STATE\"
contract (see e.g. its unrecognized-DIRECTION/blocked-by-wall/
insufficient-ENERGY branches, and their own regression tests) even
though MOVE-PLAYER (the public wrapper) now always pipes its result
through this function. Called once per MOVE-PLAYER call (see that
function's own wrapper), regardless of which of MOVE-PLAYER-INNER's
branches ran -- a trap you already suspect (having stood still,
attacked a monster, or bonded a Doge instead of moving) is exactly as
detectable as one you just moved towards, since FUTURE_PLANS.md §8
does not gate detection on the specific action taken, only on the
trap being in sight. While the player has an active :MICRODOSING
effect (§17's Microdose Tab), the SENIORITY-DETECTION-CHANCE roll is
bypassed entirely and every qualifying trap in VISIBLE-MASK is
revealed outright, standing in for the plan text's \"hidden doors/
traps glow neon colors\" (see *RDESCENT-MICRODOSING-SECONDS*'s own
docstring)."
  (let* ((player (get-player state))
         (mask-length (length visible-mask))
         (revealed-any nil)
         (microdosing (entity-effect player :microdosing))
         (new-entities
           (mapcar (lambda (ent)
                     (if (and (typep ent 'trap-fixture)
                              (get-hidden-p ent)
                              (= (get-level ent) (get-level player))
                              (let ((index (xy-to-index (get-x ent) (get-y ent))))
                                (and (< index mask-length) (plusp (bit visible-mask index))))
                              (or microdosing
                                  (< (random 100) (seniority-detection-chance (effective-seniority player)))))
                         (progn (setf revealed-any t) (update-entity ent :hidden-p nil))
                         ent))
                   (get-entities state))))
    (if revealed-any
        (update-game-state state :entities new-entities)
        state)))

(defun maybe-auto-pickup-collectible (state)
  "Return a fresh GAME-STATE derived from STATE if an AUTO-PICKUP-ITEM
(FUTURE_PLANS.md §16) occupies the player's own final (X, Y) on their
current LEVEL for this turn, otherwise STATE itself unchanged (EQ) --
preserving the same \"no-op branches return the identical STATE\"
contract MAYBE-REVEAL-HIDDEN-ENTITIES already established, even though
MOVE-PLAYER (the public wrapper) now always pipes its result through
this function too, regardless of which of MOVE-PLAYER-INNER's own cond
branches ran. If a collectible is found, its own ITEM-ID is silently
adjoined onto the player's COLLECTION-LOG (ADJOIN, so re-collecting an
already-owned ITEM-ID -- e.g. once *RDESCENT-COLLECTIBLE-CATALOG*'s
own depth-indexing wraps back around past depth 23 -- never grows the
log or double-counts), the AUTO-PICKUP-ITEM entity is removed from
ENTITIES entirely, and a \"You found ~A! [N/TOTAL]\" message (N/TOTAL
counting only that item's own owning COLLECTIBLE-SET, not the full
23-item catalog) is pushed onto MESSAGE-LOG in the item's own
ENTITY-MESSAGE-COLOR (see MAKE-COLLECTIBLE -- one color per set). If
this pickup is the last item needed to complete that set (i.e. it
was not already complete before this pickup, but is now -- see
COLLECTION-SET-COMPLETE-P), a second \"Set Complete: ~A! You have
gained the ~A bonus, permanently.\" message is also pushed, in that
same color, announcing the newly active passive bonus (e.g.
HARDWARE-EMULATION-ACTIVE-P). Called once per MOVE-PLAYER call,
exactly like MAYBE-REVEAL-HIDDEN-ENTITIES -- a collectible you land on
by bumping into a monster instead of moving is exactly as auto-picked-
up as one you walk onto directly, since FUTURE_PLANS.md §16 does not
gate pickup on the specific action taken, only on the player's final
position."
  (let* ((player (get-player state))
         (item-entity (auto-pickup-item-at state (get-x player) (get-y player) (get-level player))))
    (if (not item-entity)
        state
        (let* ((item-id (get-item-id item-entity))
               (item (find-collectible-item item-id))
               (set-id (collectible-item-set item))
               (already-complete (collection-set-complete-p player set-id))
               (new-player (update-entity player
                                           :collection-log (adjoin item-id (get-collection-log player))))
               (now-complete (and (not already-complete) (collection-set-complete-p new-player set-id)))
               (owned-count (length (intersection (collectible-set-item-ids set-id)
                                                   (get-collection-log new-player))))
               (set-total (length (collectible-set-item-ids set-id)))
               (set (find-collectible-set set-id))
               (messages
                 (append (list (make-log-message
                                (format nil "You found ~A! [~D/~D]" (get-name item-entity) owned-count set-total)
                                (entity-message-color item-entity)))
                         (when now-complete
                           (list (make-log-message
                                  (format nil "Set Complete: ~A! You have gained the ~A bonus, permanently."
                                          (collectible-set-name set) (collectible-set-bonus-name set))
                                  (entity-message-color item-entity)))))))
          (update-game-state
           state
           :player new-player
           :entities (remove item-entity (get-entities state))
           :message-log (append-log-messages (get-message-log state) messages))))))

(defun maybe-reveal-full-map (state)
  "Return a fresh GAME-STATE derived from STATE with its own EXPLORED
bit-vector forced to all-1s (every tile on the current level marked
explored) if the player's own GNU-OMNISCIENCE-ACTIVE-P is true (i.e.
their Pantheon of the Beard collectible set is complete -- FUTURE_
PLANS.md §16), overriding whatever partial EXPLORED update whichever
of MOVE-PLAYER-INNER's own cond branches ran already computed --
otherwise STATE itself is returned unchanged (EQ). Implemented as a
single post-processing pass, exactly like MAYBE-REVEAL-HIDDEN-
ENTITIES/MAYBE-AUTO-PICKUP-COLLECTIBLE, rather than touching each of
MOVE-PLAYER-INNER's own ~10 individual FOV-computing branches, so
\"GNU/Omniscience\" needs no change to any of them: this pass always
runs last (see MOVE-PLAYER) and simply overwrites the final EXPLORED
result outright when active. If EXPLORED is already all-1s (e.g. a
second move after the set was already completed), this still returns
an UPDATE-GAME-STATE-produced copy rather than STATE itself -- unlike
MAYBE-REVEAL-HIDDEN-ENTITIES/MAYBE-AUTO-PICKUP-COLLECTIBLE, there is
no cheap way to detect \"already all 1s\" without scanning the whole
bit-vector first, and forcing an already-all-1s bit-vector is
harmless (idempotent), so this optimization is not worth the extra
complexity."
  (let ((player (get-player state)))
    (if (gnu-omniscience-active-p player)
        (update-game-state state
                            :explored (make-array (* *rdescent-field-width* *rdescent-field-height*)
                                                   :element-type 'bit :initial-element 1))
        state)))

(defun random-safe-player-teleport-destination (state map level)
  "Return two values X Y naming a random walkable, non-blocked tile on
LEVEL within MAP, suitable for §13's YubiKey rescue teleport. If no
such tile exists (pathological test fixture), fall back to the
player's current coordinates rather than signalling."
  (let ((choices nil)
        (player (get-player state)))
    (dotimes (y *rdescent-field-height*)
      (dotimes (x *rdescent-field-width*)
        (let ((tile (map-tile-ref map x y)))
          (when (and tile
                     (get-walkable tile)
                     (not (blocking-entity-at state x y level)))
            (push (list x y) choices)))))
    (if choices
        (destructuring-bind (x y) (nth (random (length choices)) choices)
          (values x y))
        (values (get-x player) (get-y player)))))

(defun golden-parachute-severance-items (state player)
  "Return a list of 2-3 freshly randomized EQUIPPABLE-ITEMs (see
RANDOMIZE-NEWLY-SPAWNED-EQUIPPABLE-ITEM) drawn from
*RDESCENT-MONSTER-DROP-EQUIPPABLE-SPAWN-TABLE* -- The Golden
Parachute's own \"severance package\" payout (FUTURE_PLANS.md §15),
handed to PLAYER's own INVENTORY the moment it triggers. The spawn
table's own DEPTH-CAP is (MAX 8 current depth) so an early-run trigger
still pays out solidly, while a late-run trigger keeps scaling with
how deep PLAYER has actually gotten. Deliberately drawn from the
*ordinary* armory drop table, not from the ten unique §15 items
themselves -- a severance package is common consolation gear, not
another rare/legendary find."
  (let ((depth-cap (max 8 (get-current-depth state))))
    (loop repeat (+ 2 (random 2))
          for factory = (spawn-table-choice *rdescent-monster-drop-equippable-spawn-table* depth-cap)
          when factory
            collect (randomize-newly-spawned-equippable-item
                     (get-payload (funcall factory (get-x player) (get-y player) (get-level player)))))))

(defun maybe-trigger-golden-parachute-save (state player map level)
  "Return four values NEW-STATE NEW-PLAYER SAVED-P SAVE-MESSAGE. If
PLAYER has just been reduced to 0 HP or below, is wearing The Golden
Parachute, and has never triggered one before (a permanent, once-
ever-per-run flag -- unlike the YubiKey's own once-*per-floor*
MARK-YUBIKEY-USED-THIS-FLOOR), the Golden Parachute deploys: PLAYER is
fully healed (to EFFECTIVE-MAX-HP), teleported to a random safe tile
(see this section's own preamble for why not literally next-floor's
stairwell), revived to CHAR #\\@ / IS-ALIVE T, has the Golden Parachute
itself removed from the :BODY slot, and receives a severance package
of 2-3 items (GOLDEN-PARACHUTE-SEVERANCE-ITEMS) appended to
INVENTORY; NEW-STATE additionally records the permanent one-time use
via SET-GAME-STATE-FLAG. Otherwise STATE/PLAYER are returned unchanged,
SAVED-P is NIL, and SAVE-MESSAGE is NIL."
  (if (and (not (is-alive player))
           (golden-parachute-active-p player)
           (not (game-state-flag state :golden-parachute-used)))
      (multiple-value-bind (x y) (random-safe-player-teleport-destination state map level)
        (values
         (set-game-state-flag state :golden-parachute-used t)
         (update-entity player
                        :x x
                        :y y
                        :hp (effective-max-hp player)
                        :char #\@
                        :is-alive t
                        :equipment (equipment-with-slot (get-equipment player) :body nil)
                        :inventory (append (get-inventory player)
                                           (golden-parachute-severance-items state player)))
         t
         "Your Golden Parachute deploys! You float safely to the ground, fully healed, with a severance package of gear!"))
      (values state player nil nil)))

(defun maybe-trigger-yubikey-save (state player map level)
  "Return four values NEW-STATE NEW-PLAYER SAVED-P SAVE-MESSAGE. If
PLAYER has just been reduced to 0 HP or below, is wearing The YubiKey
of Second Factors, and has not already spent it on this floor, the
YubiKey shatters: PLAYER is restored to 1 HP, teleported to a random
safe tile, revived to CHAR #\\@ / IS-ALIVE T, and removed from the
:HEAD slot; NEW-STATE additionally records the once-per-floor use via
MARK-YUBIKEY-USED-THIS-FLOOR. If the YubiKey does not trigger (not
equipped, already used this floor, or PLAYER is still alive), this
falls through to MAYBE-TRIGGER-GOLDEN-PARACHUTE-SAVE (FUTURE_PLANS.md
§15) instead, so a player wearing both death-save items always gets
the YubiKey's cheaper, once-per-floor save first, saving the Golden
Parachute's own once-*ever* trigger for when the YubiKey is unequipped,
already spent, or simply not owned. Otherwise (neither triggers)
STATE/PLAYER are returned unchanged, SAVED-P is NIL, and SAVE-MESSAGE
is NIL."
  (if (and (not (is-alive player))
           (yubikey-of-second-factors-active-p player)
           (not (yubikey-used-this-floor-p state)))
      (multiple-value-bind (x y) (random-safe-player-teleport-destination state map level)
        (values
         (mark-yubikey-used-this-floor state)
         (update-entity player
                        :x x
                        :y y
                        :hp 1
                        :char #\@
                        :is-alive t
                        :equipment (equipment-with-slot (get-equipment player) :head nil))
         t
         "Your YubiKey shatters, preserving you at 1 HP and blinking you to a safe tile!"))
      (maybe-trigger-golden-parachute-save state player map level)))

(defun maybe-trigger-out-of-office (state)
  "Return a fresh GAME-STATE derived from STATE if the player has The
\"Out of Office\" Auto-Responder equipped in :OFF-HAND (see OUT-OF-
OFFICE-AUTO-RESPONDER-ACTIVE-P), their HP has fallen to or below
*RDESCENT-OUT-OF-OFFICE-HP-THRESHOLD-PERCENT* of their own EFFECTIVE-
MAX-HP, they don't already have an active :OUT-OF-OFFICE STATUS-
EFFECT, and they haven't already triggered this on the current floor
(OUT-OF-OFFICE-USED-THIS-FLOOR-P) -- otherwise STATE itself is
returned unchanged (EQ), exactly like MAYBE-REVEAL-HIDDEN-ENTITIES/
MAYBE-AUTO-PICKUP-COLLECTIBLE's own no-op contract. When it does
trigger, the player is given a fresh :OUT-OF-OFFICE STATUS-EFFECT (via
ENTITY-WITH-EFFECT -- a self-triggered passive, not an inflicted
attack, so no APPLY-STATUS-EFFECT/SENIORITY-DEFLECTION-CHANCE roll is
appropriate here, same rationale as CONSUMABLE-ITEM's own APPLY-ITEM
method) lasting *RDESCENT-OUT-OF-OFFICE-TICKS*, every currently
:HOSTILE monster on the player's own LEVEL (see ENTITY-DISPOSITION-
TOWARD, ENTITIES.LISP, the seam this effect is actually consulted
through) instantly drops that aggro, the floor's once-per-floor use is
recorded via MARK-OUT-OF-OFFICE-USED-THIS-FLOOR, and a callout message
is pushed onto MESSAGE-LOG. Called once per MOVE-PLAYER call, right
alongside MAYBE-REVEAL-HIDDEN-ENTITIES/MAYBE-AUTO-PICKUP-COLLECTIBLE/
MAYBE-REVEAL-FULL-MAP."
  (let ((player (get-player state)))
    (if (and (is-alive player)
             (out-of-office-auto-responder-active-p player)
             (not (entity-effect player :out-of-office))
             (not (out-of-office-used-this-floor-p state))
             (let ((max-hp (effective-max-hp player)))
               (and max-hp (<= (* 100 (hp player)) (* *rdescent-out-of-office-hp-threshold-percent* max-hp)))))
        (update-game-state
         (mark-out-of-office-used-this-floor state)
         :player (entity-with-effect player :out-of-office *rdescent-out-of-office-ticks*)
         :message-log (append-log-messages
                       (get-message-log state)
                       (list (make-log-message
                              "Your \"Out of Office\" Auto-Responder kicks in! You slip out of sight, and every hostile nearby suddenly loses interest."
                              *rdescent-status-effect-callout-color*))))
        state)))

(defun maybe-click-clack-stun (state level)
  "Return a fresh GAME-STATE derived from STATE with every :HOSTILE
entity within *RDESCENT-CLICK-CLACK-STUN-RADIUS* Chebyshev tiles of
the player's own (post-move) (X, Y) on LEVEL inflicted :STUNNED for
*RDESCENT-CLICK-CLACK-STUN-TICKS* (via APPLY-STATUS-EFFECT, so
SENIORITY-DEFLECTION-CHANCE/AIRPODS-PRO-ACTIVE-P/:MODAFINIL-IMMUNITY
still apply exactly as they would to any other :STUNNED infliction),
if the player has The Mechanical Keyboard of the Ancients equipped in
:WEAPON (see MECHANICAL-KEYBOARD-OF-THE-ANCIENTS-ACTIVE-P) --
otherwise STATE itself is returned unchanged (EQ). Called once per
MOVE-PLAYER call, right alongside MAYBE-TRIGGER-OUT-OF-OFFICE and
this file's other post-processing passes."
  (let ((player (get-player state)))
    (if (not (mechanical-keyboard-of-the-ancients-active-p player))
        state
        (let* ((radius *rdescent-click-clack-stun-radius*)
               (targets (remove-if-not
                         (lambda (ent)
                           (and (is-alive ent) (= (get-level ent) level)
                                (eq (entity-disposition-toward ent player) :hostile)
                                (<= (max (abs (- (get-x ent) (get-x player))) (abs (- (get-y ent) (get-y player))))
                                    radius)))
                         (get-entities state))))
          (if (null targets)
              state
              (update-game-state
               state
               :entities (loop for ent in (get-entities state)
                               collect (if (member ent targets)
                                           (apply-status-effect ent :stunned *rdescent-click-clack-stun-ticks*)
                                           ent))))))))

(defun move-player (state tier level direction)
  "Return a fresh GAME-STATE derived from STATE with its PLAYER moved
one step in response to DIRECTION, exactly like MOVE-PLAYER-INNER
(see its own docstring for the full move/attack/bump-into-Doge/trap-
trigger cond) -- this is a thin wrapper piping MOVE-PLAYER-INNER's own
STATE result through MAYBE-REVEAL-HIDDEN-ENTITIES (FUTURE_PLANS.md §8),
then MAYBE-AUTO-PICKUP-COLLECTIBLE, then MAYBE-REVEAL-FULL-MAP (both
FUTURE_PLANS.md §16), then MAYBE-TRIGGER-OUT-OF-OFFICE and MAYBE-
CLICK-CLACK-STUN (both FUTURE_PLANS.md §15) before returning it, using
the same VISIBLE-MASK MOVE-PLAYER-INNER already computed, so every
call -- whichever of its cond branches ran -- gets an equal chance to
notice any still-hidden TRAP-FIXTURE now within the player's own field
of view, auto-collect any AUTO-PICKUP-ITEM now under the player's own
feet, have its own EXPLORED bit-vector overridden to fully-revealed if
GNU-OMNISCIENCE-ACTIVE-P, arm The \"Out of Office\" Auto-Responder if
HP has fallen low enough, and stun every nearby hostile if The
Mechanical Keyboard of the Ancients is equipped -- not just the
branches that actually change the player's X/Y."
  (multiple-value-bind (result-state map visible-mask) (move-player-inner state tier level direction)
    (values (maybe-click-clack-stun
             (maybe-trigger-out-of-office
              (maybe-reveal-full-map
               (maybe-auto-pickup-collectible
                (maybe-reveal-hidden-entities result-state visible-mask))))
             level)
            map visible-mask)))

(defun move-player-inner (state tier level direction)
  "Return a fresh GAME-STATE derived from STATE with its PLAYER moved
one step in response to DIRECTION (one of the strings \"up\", \"down\",
\"left\", or \"right\"). The target position is clamped to the playing
field's bounds (0 <= x < *RDESCENT-FIELD-WIDTH*, 0 <= y <
*RDESCENT-FIELD-HEIGHT*). Several outcomes are possible:
  - If the target lands on a locked-door TILE (FUTURE_PLANS.md §9 --
    GET-LOCKED-KEY-ID non-NIL) this player has not already opened (see
    DOOR-OPENED-P), and the player holds its matching key (see
    KEY-HELD-P), the move proceeds like an ordinary floor move (same
    *RDESCENT-MOVE-ENERGY-COST*/FOV refresh) but also spends the key
    (REMOVE-KEY-HELD -- keys are single-use) and permanently marks the
    door opened for this player (ADD-DOOR-OPENED), pushing a \"You use
    your ~A to unlock the door!\" message. If the player lacks the
    matching key, the move is refused (no ENERGY spent) with a \"The
    door is locked. You need a ~A.\" message instead. Either way, this
    is resolved entirely from this player's own GAME-STATE flags, not
    by mutating the shared, cross-player-cached TILE itself (see
    TILE's own docstring for why).
  - If the target lands on a non-walkable TILE in TIER/LEVEL's
    procedurally generated dungeon (see GENERATE-DUNGEON -- the same
    map RENDER-GRID draws, so collision and rendering can never
    disagree) that isn't a locked door this player has already opened,
    the move is refused entirely and STATE is returned unchanged
    rather than clamped to the wall's edge.
  - Else, if a BLOCKING-ENTITY-AT occupies the target cell and it is
    a still-wild (not yet bonded) COMPANION -- the Office Doge,
    FUTURE_PLANS.md §22 -- the player bonds it instead of attacking
    it: for free (no ENERGY cost), its BONDED slot flips to T (see
    COMPANION-BONDED-P) and a flavor message is pushed, but the player
    does not move onto its tile this turn. If it occupies the target
    cell and is already a BONDED-COMPANION-P (the player's own Doge),
    the player swaps places with it instead of attacking -- \"you
    cannot attack it\" -- at the ordinary *RDESCENT-MOVE-ENERGY-COST*
    (100), gated by ENERGY exactly like an ordinary move.
  - Else, if a BLOCKING-ENTITY-AT occupies the target cell (e.g. an
    Orc or Troll), the player attacks it instead of moving onto it,
    provided the player's ENTITY-ENERGY is at least
    *RDESCENT-ATTACK-ENERGY-COST* (150) -- otherwise (mashing input
    faster than the player's ENTITY-SPEED can refill its ENERGY) the
    action is rejected and STATE is returned unchanged, exactly as if
    DIRECTION had been unrecognized. When the attack is affordable,
    that cost is deducted from the player's ENERGY and RESOLVE-ATTACK
    (ARCHITECTURE_PLAN.md §5) resolves the hit: PIVOT-DODGE-CHANCE is
    rolled against the target's own PIVOT first -- every monster today
    has PIVOT 0 (a 0% dodge chance), so this is presently a no-op, but
    if it does trigger, a random dodge/parry/block/evade message (see
    RANDOM-DODGE-PHRASE) is pushed and nothing else about the target
    changes. Otherwise damage is (MAX 0 (- player's EFFECTIVE-POWER
    target's EFFECTIVE-DEFENSE)) -- folding in any EQUIPMENT either
    side has, see §4 -- scaled by the target's own BANDWIDTH-DAMAGE-
    MULTIPLIER; if that damage is > 0, the target's HP is reduced by
    it (via UPDATE-ENTITY, replacing the old target with the new one
    inside STATE's ENTITIES list via SUBSTITUTE) and a random hit-verb
    message (see RANDOM-HIT-VERB, e.g. \"You hit/strike/kick/elbow/
    claw at/punch/smack the ~A for ~D damage!\") is pushed onto
    MESSAGE-LOG. If the
    target's new HP is <= 0, it dies: it is transformed into an inert
    corpse (CHAR #\\%, NAME \"remains of ~A\", BLOCKS-MOVEMENT NIL,
    RENDER-ORDER 0, IS-ALIVE NIL -- see ENTITY's RENDER-ORDER slot
    documentation and RENDER-GRID's corpse-under-actor draw ordering),
    the target's own GET-XP (its fixed kill-reward value, see
    MAKE-ORC/MAKE-TROLL/ENEMY) is added onto the player's own GET-XP,
    and a random Monty-Python-style obituary message (see RANDOM-
    OBITUARY-MESSAGE) is pushed as well. If
    damage is 0 (EFFECTIVE-POWER <= target's EFFECTIVE-DEFENSE), no HP
    change (and no XP transfer) happens but a \"...does no damage!\"
    message is still pushed so the player gets feedback. Either way
    the player itself does not move this turn.
  - Else the target is open floor: provided the player's ENTITY-ENERGY
    is at least *RDESCENT-MOVE-ENERGY-COST* (100) -- otherwise the
    move is likewise rejected and STATE is returned unchanged -- that
    cost is deducted from the player's ENERGY and the player moves
    there, and EXPLORED is refreshed -- COMPUTE-FOV is recomputed from
    the player's new position (at a radius scaled by the player's own
    DOMAIN-KNOWLEDGE, see DOMAIN-KNOWLEDGE-FOV-RADIUS) and
    bitwise-OR'd (via BIT-IOR, itself pure -- it conses a fresh result
    bit-vector rather than mutating either argument) onto the old
    EXPLORED mask, so previously seen tiles stay marked \"explored\"
    even once they fall out of the player's current sight radius. If
    a TRAP-AT lookup finds a TRAP-FIXTURE already occupying that same
    cell (FUTURE_PLANS.md §8 -- BLOCKS-MOVEMENT NIL, so the move above
    still happens as an ordinary step, never a bump), the trap
    triggers instead of being silently walked over: RESOLVE-ATTACK-ON-
    PLAYER (COMMANDS.LISP) resolves it exactly like a monster's own
    attack against the just-moved player (dodge roll against the
    player's own PIVOT first, then damage from the trap's own POWER
    vs. the player's EFFECTIVE-DEFENSE, scaled by BANDWIDTH-DAMAGE-
    MULTIPLIER, with the same CHAR #\\%/IS-ALIVE NIL death
    transformation on a lethal hit), a \"You trigger a ~A!\" family of
    messages is pushed (mirroring PROCESS-ENEMY-TURNS' own BUILD-
    COMBAT-MESSAGES usage), and the trap's own HIDDEN-P unconditionally
    flips to NIL via UPDATE-ENTITY -- win, lose, or dodge, triggering a
    trap always reveals it.
An unrecognized DIRECTION is likewise a no-op. Pure: STATE itself is
never mutated -- either the original STATE is returned (unmoved,
possibly with an updated MESSAGE-LOG) or a fresh GAME-STATE is,
produced via UPDATE-GAME-STATE/UPDATE-ENTITY rather than
MAKE-INSTANCE'd by hand, so callers can always treat the return value
as independent of whatever was passed in.
Returns three values: the resulting STATE, plus the MAP and
VISIBLE-MASK this call already computed for TIER/LEVEL and the
player's (possibly just-moved) position -- callers such as
APPLY-RDESCENT-COMMAND can pass these straight into
PROCESS-ENEMY-TURNS instead of paying for GENERATE-DUNGEON/COMPUTE-FOV
a second time for what is almost always the same tier/level/position
within a single command's processing (see TECHNICAL_DEBT.md item
#31). The second and third values are always fresh/correct for the
player's *final* position in this call, even on the two no-move
branches, since MAP is TIER/LEVEL's (cache-memoized) dungeon
regardless of outcome, and VISIBLE-MASK is computed from the player's
resulting position either way."
  (multiple-value-bind (dx dy) (direction-delta direction)
    (if (and (zerop dx) (zerop dy))
        ;; Unrecognized DIRECTION: no displacement at all, so STATE is
        ;; returned unchanged rather than re-derived.
        (let* ((player (get-player state))
               (map (generate-dungeon tier level)))
          (values state map (compute-fov map (get-x player) (get-y player) (effective-fov-radius player))))
        (let* ((player (get-player state))
               (map (generate-dungeon tier level))
               (target-x (max 0 (min (1- *rdescent-field-width*) (+ (get-x player) dx))))
               (target-y (max 0 (min (1- *rdescent-field-height*) (+ (get-y player) dy))))
               (target-tile (map-tile-ref map target-x target-y))
               (target (blocking-entity-at state target-x target-y level)))
          (cond
            ((and target-tile (get-locked-key-id target-tile)
                  (not (door-opened-p state level target-x target-y))
                  (key-held-p state (get-locked-key-id target-tile)))
             ;; Locked door tile (FUTURE_PLANS.md §9), not yet opened
             ;; by this player, and the player holds its matching key:
             ;; treat this as an ordinary move (same ENERGY cost/FOV
             ;; refresh), but also spend the key (single-use -- see
             ;; REMOVE-KEY-HELD) and permanently record this door as
             ;; opened for this player (ADD-DOOR-OPENED), so it never
             ;; re-locks on a later visit even though the underlying,
             ;; cross-player-shared TILE itself never actually changes
             ;; WALKABLE/CHAR.
             (if (< (entity-energy player) *rdescent-move-energy-cost*)
                 (values state map (compute-fov map (get-x player) (get-y player) (effective-fov-radius player)))
                 (let* ((key-id (get-locked-key-id target-tile))
                        (key-name (get-locked-key-name target-tile))
                        (visible-mask (compute-fov map target-x target-y (effective-fov-radius player)))
                        (moved-player (update-entity player :x target-x :y target-y
                                                            :energy (- (entity-energy player) *rdescent-move-energy-cost*)))
                        (unlocked-state (add-door-opened (remove-key-held state key-id) level target-x target-y)))
                   (values
                    (update-game-state unlocked-state
                                       :player moved-player
                                       :explored (bit-ior (get-explored state) visible-mask)
                                       :message-log (append-log-messages
                                                     (get-message-log unlocked-state)
                                                     (list (make-log-message
                                                            (format nil "You use your ~A to unlock the door!" key-name)
                                                            (entity-message-color player)))))
                    map
                    visible-mask))))
            ((and target-tile (get-locked-key-id target-tile)
                  (not (door-opened-p state level target-x target-y)))
             ;; Locked door, no matching key: blocked, no ENERGY spent,
             ;; but a message tells the player what they need -- the
             ;; same "no-op-but-message" convention GRAB-ITEM already
             ;; uses for "nothing here"/"can't carry any more".
             (values
              (update-game-state state
                                 :message-log (append-log-messages
                                               (get-message-log state)
                                               (list (make-log-message
                                                      (format nil "The door is locked. You need a ~A." (get-locked-key-name target-tile))
                                                      (entity-message-color player)))))
              map
              (compute-fov map (get-x player) (get-y player) (effective-fov-radius player))))
            ((not (and target-tile (or (get-walkable target-tile) (get-locked-key-id target-tile))))
             ;; Ordinary wall (or a locked door already opened by this
             ;; player, handled above -- by the time control reaches
             ;; here a non-NIL LOCKED-KEY-ID always means DOOR-OPENED-P
             ;; was already T, so it's safe to treat as passable
             ;; alongside WALKABLE below).
             (values state map (compute-fov map (get-x player) (get-y player) (effective-fov-radius player))))
            ((and target (companion-p target) (not (bonded-companion-p target)))
             ;; Bumping into a still-wild Office Doge bonds it for
             ;; free, no ENERGY cost -- FUTURE_PLANS.md §22 -- rather
             ;; than resolving as an attack; the player does not move
             ;; onto the Doge's own tile this turn.
             (values
              (update-game-state
               state
               :entities (substitute (update-entity target :bonded t) target (get-entities state))
               :message-log (append-log-messages
                             (get-message-log state)
                             (list (make-log-message
                                    (format nil "The ~A wags its tail and decides to follow you!" (get-name target))
                                    (entity-message-color target)))))
              map
              (compute-fov map (get-x player) (get-y player) (effective-fov-radius player))))
            ((and target (bonded-companion-p target))
             ;; "You cannot attack it": bumping into your own already-
             ;; bonded Doge swaps places with it instead, at the
             ;; ordinary move ENERGY cost.
             (if (< (entity-energy player) *rdescent-move-energy-cost*)
                 (values state map (compute-fov map (get-x player) (get-y player) (effective-fov-radius player)))
                 (let ((visible-mask (compute-fov map target-x target-y (effective-fov-radius player))))
                   (values
                    (update-game-state
                     state
                     :player (update-entity player :x target-x :y target-y
                                                   :energy (- (entity-energy player) *rdescent-move-energy-cost*))
                     :entities (substitute (update-entity target :x (get-x player) :y (get-y player)) target (get-entities state))
                     :explored (bit-ior (get-explored state) visible-mask))
                    map
                    visible-mask))))
            (target
             (if (< (entity-energy player) (effective-attack-energy-cost player))
                 ;; Mashing input faster than ENERGY can refill: reject
                 ;; the attack and leave STATE (including ENERGY)
                 ;; completely unchanged.
                 (values state map (compute-fov map (get-x player) (get-y player) (effective-fov-radius player)))
                 (multiple-value-bind (damage dies new-target broken-item-names) (resolve-attack-volley player target)
                   (if (null damage)
                       (values
                        (update-game-state
                         state
                         :player (update-entity player :energy (- (entity-energy player) (effective-attack-energy-cost player)))
                         :message-log (append-log-messages
                                       (get-message-log state)
                                       (list (make-log-message
                                              (format nil "You attack, but the ~A ~A" (get-name target) (random-dodge-phrase :third))
                                              (entity-message-color player)))))
                        map
                        (compute-fov map (get-x player) (get-y player) (effective-fov-radius player)))
                       (let* ((new-target (if dies
                                              (update-entity new-target :char #\% :name (format nil "remains of ~A" (get-name target))
                                                                        :blocks-movement nil :render-order 0 :is-alive nil)
                                              new-target))
                              (drop (and dies (maybe-drop-monster-loot new-target)))
                              (messages
                                (append
                                 (monster-death-drop-messages drop)
                                 (let ((color (entity-message-color player))
                                       (verb (random-hit-verb :second)))
                                   (build-combat-messages
                                    (get-name target) damage dies color
                                    (concatenate 'string "You " verb " the ~A for ~D damage!")
                                    (concatenate 'string "You " verb " the ~A but it does no damage!")
                                    (random-obituary-message (get-name target) color)))
                                 (item-break-messages broken-item-names))))
                         (values
                          (record-middle-manager-kills
                           (update-game-state
                            state
                            :player (update-entity player
                                                   :energy (- (entity-energy player) (effective-attack-energy-cost player))
                                                   :xp (if dies (+ (get-xp player) (get-xp target)) (get-xp player)))
                            :entities (if drop
                                          (cons drop (substitute new-target target (get-entities state)))
                                          (substitute new-target target (get-entities state)))
                            :message-log (append-log-messages (get-message-log state) messages))
                           (if (and dies (typep target 'middle-manager)) 1 0))
                          map
                          (compute-fov map (get-x player) (get-y player) (effective-fov-radius player))))))))
            ((< (entity-energy player) *rdescent-move-energy-cost*)
             ;; Mashing input faster than ENERGY can refill: reject the
             ;; move and leave STATE (including ENERGY) completely
             ;; unchanged.
             (values state map (compute-fov map (get-x player) (get-y player) (effective-fov-radius player))))
            (t
             (let* ((visible-mask (compute-fov map target-x target-y (effective-fov-radius player)))
                    (moved-player (update-entity player :x target-x :y target-y
                                                        :energy (- (entity-energy player) *rdescent-move-energy-cost*)))
                    (trap (trap-at state target-x target-y level)))
               (if trap
                   ;; The player just stepped onto a TRAP-FIXTURE's own
                   ;; tile: trigger it via RESOLVE-ATTACK-ON-PLAYER,
                   ;; exactly as if it were an ordinary monster's
                   ;; attack (FUTURE_PLANS.md §8), and unconditionally
                   ;; reveal it -- win, lose, or dodge -- since a
                   ;; trigger is always noticed even when it misses.
                   (multiple-value-bind (damage dies resolved-player broken-item-names)
                       (resolve-attack-on-player trap moved-player)
                     (multiple-value-bind (rescued-state new-player yubikey-saved-p save-message)
                         (maybe-trigger-yubikey-save state resolved-player map level)
                       (let* ((trap-color (entity-message-color trap))
                              (revealed-trap (update-entity trap :hidden-p nil))
                              (messages
                                (append
                                 (if (null damage)
                                     (list (make-log-message
                                            (format nil "You trigger a ~A, but narrowly avoid its blow!" (get-name trap))
                                            trap-color))
                                     (build-combat-messages
                                      (get-name trap) damage (and dies (not yubikey-saved-p)) trap-color
                                      "You trigger a ~A! It hits you for ~D damage!"
                                      "You trigger a ~A, but it does no damage!"
                                      "You have died..."))
                                 (item-break-messages broken-item-names)
                                 (when yubikey-saved-p
                                   (list (make-log-message
                                          save-message
                                          *rdescent-status-effect-callout-color*))))))
                         (values
                          (update-game-state rescued-state
                                             :player new-player
                                             :entities (substitute revealed-trap trap (get-entities rescued-state))
                                             :explored (bit-ior (get-explored state) visible-mask)
                                             :message-log (append-log-messages
                                                           (get-message-log rescued-state)
                                                           messages))
                          map
                          visible-mask))))
                   (values
                    (update-game-state state
                                       :player moved-player
                                       :explored (bit-ior (get-explored state) visible-mask))
                    map
                    visible-mask)))))))))

(defun use-stairs (state tier level max-depth)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to use a staircase at its current position on LEVEL, given
TIER (needed to GENERATE-DUNGEON for a not-yet-visited depth) and
MAX-DEPTH (this connection's deepest permitted LEVEL, per
RDESCENT-TIER-MAX-DEPTH) -- a JWT-derived limit that lives on
RDESCENT-CLIENT (RDESCENT/SERVER.LISP), not GAME-STATE itself, hence
its being passed in explicitly here exactly as TIER/LEVEL already are.
Pure; STATE itself is never mutated.

If the player is not currently standing on a staircase entity (see
STAIRS-ENTITY-AT), STATE is returned unchanged. Otherwise:

  - Standing on \"Stairs Down\": if CURRENT-DEPTH already equals
    MAX-DEPTH, the descent is refused -- STATE is returned with an
    unchanged PLAYER/MAP/ENTITIES/CURRENT-DEPTH but a sassy \"Your
    'none' tier trial has expired. Upgrade to CONS to descend
    further.\" message pushed onto MESSAGE-LOG -- a JWT-gated dead end,
    not a bug. Otherwise the current LEVEL's DUNGEON-LEVEL-SNAPSHOT
    (its MAP, ENTITIES, and EXPLORED) is saved into LEVELS under
    CURRENT-DEPTH before anything else changes, and CURRENT-DEPTH is
    incremented. If the incremented depth already has a
    DUNGEON-LEVEL-SNAPSHOT in LEVELS (the player has been there
    before), that snapshot's MAP/ENTITIES/EXPLORED are restored
    verbatim -- corpses, wandering monsters, and dropped loot exactly
    as they were left -- and the player is placed at that snapshot's
    own \"Stairs Up\" entity's (X, Y) (guaranteed to exist: every
    depth > 1 always gets one, per SPAWN-STAIRS-FOR-LEVEL). If the
    incremented depth has never been visited, GENERATE-DUNGEON/
    SPAWN-MONSTERS-FOR-LEVEL/SPAWN-STAIRS-FOR-LEVEL/SPAWN-ITEMS-FOR-LEVEL
    build it fresh (the same MAKE-INITIAL-STATE recipe), EXPLORED
    starts as an empty bit-vector, and the player is placed in the new
    dungeon's Room 0 (its first generated room's center).

  - Standing on \"Stairs Up\": the current LEVEL's DUNGEON-LEVEL-
    SNAPSHOT is likewise saved into LEVELS under CURRENT-DEPTH,
    CURRENT-DEPTH is decremented (always present in LEVELS already,
    since the player necessarily passed through it to get here), and
    its saved MAP/ENTITIES/EXPLORED are restored, with the player
    placed at that snapshot's own \"Stairs Down\" entity's (X, Y).

Either way EXPLORED and CURRENT-DEPTH/LEVELS/MAP/ENTITIES are all
updated together via UPDATE-GAME-STATE, and the player's own LEVEL
slot (ENTITY-LEVEL) is updated to match the new CURRENT-DEPTH so
future MOVE-PLAYER/PROCESS-ENEMY-TURNS calls collide against the
correct dungeon.

If the player currently has an alive, bonded COMPANION (see FIND-
BONDED-COMPANION -- the Office Doge, FUTURE_PLANS.md §22), it is
carried across the depth transition exactly like the player's own
ENTITY: it is extracted from the departing level's ENTITIES before
that level's DUNGEON-LEVEL-SNAPSHOT is captured (so it is never
duplicated or left stranded on the old level), then re-appended --
with an updated LEVEL slot, positioned alongside the player's own new
(X, Y) -- to whichever ENTITIES list the new depth ends up with
(cached snapshot, freshly generated level, or the level being
ascended back to). A freshly generated new depth's own SPAWN-DOGE-
FOR-LEVEL call is passed HAS-COMPANION-P true whenever a COMPANION was
carried forward this way, so a second, wild Doge is never spawned on
a level the player already brought their own Doge to. Conversely, if
no COMPANION was carried forward (the player never adopted one, or
lost theirs to combat), that same call is passed GUARANTEED-P true --
per §22's own \"Replacement on the next level\" framing, a fresh wild
Doge is certain to appear on this newly generated depth, bypassing
the ordinary *RDESCENT-DOGE-SPAWN-CHANCE* roll every other level's
Doge placement is subject to."
  (let* ((player (get-player state))
         (stairs (stairs-entity-at state (get-x player) (get-y player) level))
         (carried-companion (find-if (lambda (ent) (and (bonded-companion-p ent) (is-alive ent)))
                                      (get-entities state))))
    (cond
      ((not stairs) state)
      ((string= (get-name stairs) "Stairs Down")
       (if (>= (get-current-depth state) max-depth)
           (update-game-state
            state
            :message-log (append-log-messages (get-message-log state)
                                               (list "Your 'none' tier trial has expired. Upgrade to CONS to descend further.")))
           (let* ((departing-depth (get-current-depth state))
                  (new-depth (1+ departing-depth))
                  (levels (fset:with (get-levels state) departing-depth
                                     (make-dungeon-level-snapshot
                                      :map (get-map state)
                                      :entities (if carried-companion
                                                    (remove carried-companion (get-entities state))
                                                    (get-entities state))
                                      :explored (get-explored state)))))
             (multiple-value-bind (snapshot found-p) (fset:lookup levels new-depth)
               (if found-p
                   (let* ((entities (dungeon-level-snapshot-entities snapshot))
                          (up (find "Stairs Up" entities :key #'get-name :test #'string=))
                          (entities (if carried-companion
                                        (append entities
                                                (list (update-entity carried-companion
                                                                      :level new-depth
                                                                      :x (get-x up) :y (get-y up))))
                                        entities)))
                     (reset-yubikey-used-this-floor
                      (update-game-state
                       state
                       :player (update-entity player :level new-depth :x (get-x up) :y (get-y up))
                       :entities entities
                       :map (dungeon-level-snapshot-map snapshot)
                       :current-depth new-depth
                       :levels levels
                       :explored (dungeon-level-snapshot-explored snapshot))))
                   (multiple-value-bind (map rooms locked-door) (generate-dungeon tier new-depth)
                     (let* ((spawn-room (first rooms))
                            (player-x (if spawn-room (rect-room-center-x spawn-room) (get-x player)))
                            (player-y (if spawn-room (rect-room-center-y spawn-room) (get-y player)))
                            (spawned-monsters-and-stairs
                              (append (spawn-monsters-for-level tier new-depth rooms (effective-hygiene player) (effective-synergy player))
                                      (spawn-stairs-for-level tier new-depth rooms max-depth)))
                            (spawned-items (filter-out-owned-unique-items
                                            state
                                            (spawn-items-for-level tier new-depth rooms spawned-monsters-and-stairs)))
                            (spawned-fixtures (spawn-fixtures-for-level tier new-depth rooms
                                                                        (append spawned-monsters-and-stairs spawned-items)))
                            (spawned-traps
                              (spawn-traps-for-level tier new-depth rooms
                                                     (append spawned-monsters-and-stairs spawned-items spawned-fixtures)))
                            (spawned-collectibles
                              (spawn-collectibles-for-level tier new-depth rooms
                                                            (append spawned-monsters-and-stairs spawned-items
                                                                    spawned-fixtures spawned-traps)))
                            (spawned-plaque
                              (spawn-plaque-for-level tier new-depth rooms
                                                      (append spawned-monsters-and-stairs spawned-items
                                                              spawned-fixtures spawned-traps spawned-collectibles)))
                            (entities
                              (append spawned-monsters-and-stairs
                                      spawned-items
                                      spawned-fixtures
                                      spawned-traps
                                      spawned-collectibles
                                      spawned-plaque
                                      (spawn-keys-for-level tier new-depth rooms locked-door
                                                            (append spawned-monsters-and-stairs spawned-items
                                                                    spawned-fixtures spawned-traps spawned-collectibles spawned-plaque))
                                      (spawn-doge-for-level tier new-depth rooms
                                                            (append spawned-monsters-and-stairs spawned-items)
                                                            (not (not carried-companion))
                                                            (not carried-companion))
                                      (if carried-companion
                                          (list (update-entity carried-companion
                                                                :level new-depth
                                                                :x player-x :y player-y))
                                          nil)))
                            (explored (make-array (* *rdescent-field-width* *rdescent-field-height*)
                                                  :element-type 'bit :initial-element 0)))
                       (reset-yubikey-used-this-floor
                        (update-game-state
                         state
                         :player (update-entity player :level new-depth :x player-x :y player-y)
                         :entities entities
                         :map map
                         :current-depth new-depth
                         :levels (fset:with levels new-depth
                                            (make-dungeon-level-snapshot
                                             :map map :entities entities :explored explored))
                         :explored explored)))))))))
      (t
       ;; Standing on "Stairs Up".
       (let* ((departing-depth (get-current-depth state))
              (new-depth (1- departing-depth))
              (levels (fset:with (get-levels state) departing-depth
                                 (make-dungeon-level-snapshot
                                  :map (get-map state)
                                  :entities (if carried-companion
                                                (remove carried-companion (get-entities state))
                                                (get-entities state))
                                  :explored (get-explored state))))
              (snapshot (fset:lookup levels new-depth))
              (entities (dungeon-level-snapshot-entities snapshot))
              (down (find "Stairs Down" entities :key #'get-name :test #'string=))
              (entities (if carried-companion
                            (append entities
                                    (list (update-entity carried-companion
                                                          :level new-depth
                                                          :x (get-x down) :y (get-y down))))
                            entities)))
         (reset-yubikey-used-this-floor
          (update-game-state
           state
           :player (update-entity player :level new-depth :x (get-x down) :y (get-y down))
           :entities entities
           :map (dungeon-level-snapshot-map snapshot)
           :current-depth new-depth
           :levels levels
           :explored (dungeon-level-snapshot-explored snapshot))))))))

(defun drink-potion (state)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to drink one Kombucha (a healing potion charge tracked by
ENTITY's KOMBUCHA slot, read via GET-KOMBUCHA). Pure; STATE itself is
never mutated.

Three outcomes are possible:
  - If the player is not IS-ALIVE, STATE is returned completely
    unchanged -- a corpse can't drink, exactly as a corpse can't move
    or attack (see MOVE-PLAYER/USE-STAIRS' analogous dead-player
    guards).
  - Else, if the player's ENTITY-ENERGY is below
    *RDESCENT-MOVE-ENERGY-COST* (drinking, like moving, costs one
    turn's worth of ENERGY), the action is rejected and STATE is
    returned unchanged, exactly as MOVE-PLAYER silently rejects a move
    or attack thrown at it faster than ENERGY can refill.
  - Else, if the player has no Kombucha charges left (GET-KOMBUCHA <=
    0) or is already at full health (HP >= EFFECTIVE-MAX-HP), the
    drink fails:
    STATE's PLAYER/ENTITIES/ENERGY are left unchanged, but a warning
    message (\"You have no Kombuchas left!\" or \"You are already at
    full health!\" respectively) is pushed onto MESSAGE-LOG so the
    player gets feedback on why nothing happened, in the player's own
    ENTITY-MESSAGE-COLOR (white, by default).
  - Else the drink succeeds: HP is increased by the player's own
    KOMBUCHA-HEAL-AMOUNT (5 plus a bonus/penalty scaled by the
    player's own CAFFEINE-TOLERANCE, see that function), capped at
    EFFECTIVE-MAX-HP (folding in any EQUIPMENT :MAX-HP bonus, see §4);
    KOMBUCHA is decremented by 1; *RDESCENT-MOVE-ENERGY-COST*
    is deducted from ENERGY (spending this turn, same cost as an
    ordinary move); and a \"You drink a Kombucha. +~D HP.\" message
    (reporting the actual amount healed, which may differ from the
    base 5) is pushed onto MESSAGE-LOG, also in the player's own
    ENTITY-MESSAGE-COLOR."
  (let ((player (get-player state)))
    (cond
      ((not (is-alive player)) state)
      ((< (entity-energy player) *rdescent-move-energy-cost*) state)
      ((or (<= (get-kombucha player) 0) (>= (hp player) (effective-max-hp player)))
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message
                             (if (<= (get-kombucha player) 0)
                                 "You have no Kombuchas left!"
                                 "You are already at full health!")
                             (entity-message-color player))))))
      (t
       (let* ((heal-amount (kombucha-heal-amount (effective-caffeine-tolerance player)))
              (new-hp (min (effective-max-hp player) (+ (hp player) heal-amount))))
         (update-game-state
          state
          :player (update-entity player
                                 :hp new-hp
                                 :kombucha (1- (get-kombucha player))
                                 :energy (- (entity-energy player) *rdescent-move-energy-cost*))
          :message-log (append-log-messages
                        (get-message-log state)
                        (list (make-log-message (format nil "You drink a Kombucha. +~D HP." heal-amount)
                                                (entity-message-color player))))))))))

(defun fixture-at (state x y level)
  "Return the first FIXTURE in (GET-ENTITIES STATE) at (X, Y) on LEVEL,
or NIL if none is found. Used by INTERACT-FIXTURE to look up whatever
shrine/vendor/NPC/door the player is currently standing on
(ARCHITECTURE_PLAN.md §3/§8) -- mirrors GROUND-ITEM-AT's own TYPEP-
based lookup pattern (a FIXTURE's NAME varies per concrete instance --
\"Espresso Machine\", \"Kombucha Bar\", ... -- rather than being one of
a small fixed set of strings, so this checks TYPEP rather than NAME,
exactly like GROUND-ITEM-AT)."
  (find-if (lambda (ent) (and (= (get-level ent) level)
                              (typep ent 'fixture)))
           (entities-at-cell state x y)))

(defun interact-shrine (state player shrine)
  "Return a fresh GAME-STATE derived from STATE describing the outcome
of PLAYER interacting with SHRINE (a SHRINE-FIXTURE already confirmed
to be at PLAYER's own position, and PLAYER already confirmed IS-ALIVE
with enough ENERGY to act -- see INTERACT-FIXTURE, which performs both
checks before calling this function). Pure; STATE itself is never
mutated.

  - If SHRINE's own GET-USE-COUNT is a number that has reached 0 (a
    finite-use shrine that's run dry -- see *RDESCENT-SHRINE-USE-
    LIMIT*), the interaction fails exactly like DRINK-POTION's own
    \"no charges left\" case: STATE's PLAYER/ENTITIES/ENERGY are left
    unchanged (no turn is spent), but a \"The ~A reads OUT OF ORDER.\"
    message is pushed onto MESSAGE-LOG in SHRINE's own
    ENTITY-MESSAGE-COLOR. A NIL GET-USE-COUNT means unlimited charges,
    so this branch never triggers for such a shrine.
  - Else, SHRINE's GET-SHRINE-KIND selects the effect:
      - :ESPRESSO always succeeds (ENERGY has no upper cap to run
        into): the player's ENERGY becomes (- ENERGY
        *RDESCENT-MOVE-ENERGY-COST*), then
        *RDESCENT-ESPRESSO-ENERGY-RESTORE* (300) is added back -- a
        net +200 ENERGY gain for this turn's trip to the machine -- and
        a \"The Espresso Machine hums to life. +~D Energy.\" message
        (reporting the full 300 restored, not the net gain) is pushed
        in SHRINE's own ENTITY-MESSAGE-COLOR.
      - :KOMBUCHA-BAR heals HP via KOMBUCHA-HEAL-AMOUNT/PLAYER's own
        CAFFEINE-TOLERANCE, the exact formula DRINK-POTION already
        uses, capped at EFFECTIVE-MAX-HP. If PLAYER is already at full
        health,
        the interaction fails exactly like DRINK-POTION's own
        already-full case: STATE unchanged (no turn spent), a \"You
        are already at full health!\" message pushed in PLAYER's own
        ENTITY-MESSAGE-COLOR.
      - :WATER-COOLER heals a flat *RDESCENT-WATER-COOLER-HEAL-AMOUNT*
        (2) HP, capped at EFFECTIVE-MAX-HP, with the same
        already-full-health failure as :KOMBUCHA-BAR above.
  - On any successful (charge-consuming) interaction, SHRINE's own
    GET-USE-COUNT is decremented by 1 (via UPDATE-ENTITY's :USE-COUNT
    keyword) unless it is NIL (unlimited), *RDESCENT-MOVE-ENERGY-COST*
    is deducted from PLAYER's ENERGY (already netted into the :ESPRESSO
    case above, applied directly for the two healing cases), and the
    resulting PLAYER/updated SHRINE (via SUBSTITUTE within ENTITIES)
    are both written into the returned GAME-STATE alongside the
    success message."
  (let ((use-count (get-use-count shrine)))
    (cond
      ((and use-count (<= use-count 0))
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message (format nil "The ~A reads OUT OF ORDER." (get-name shrine))
                                              (entity-message-color shrine))))))
      ((eq (get-shrine-kind shrine) :espresso)
       (update-game-state
        state
        :player (update-entity player :energy (+ (- (entity-energy player) *rdescent-move-energy-cost*)
                                                  *rdescent-espresso-energy-restore*))
        :entities (substitute (update-entity shrine :use-count (and use-count (1- use-count)))
                              shrine (get-entities state))
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message
                             (format nil "The Espresso Machine hums to life. +~D Energy."
                                    *rdescent-espresso-energy-restore*)
                             (entity-message-color shrine))))))
      ((>= (hp player) (effective-max-hp player))
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message "You are already at full health!" (entity-message-color player))))))
      (t
       (let* ((heal-amount (if (eq (get-shrine-kind shrine) :kombucha-bar)
                               (kombucha-heal-amount (effective-caffeine-tolerance player))
                               *rdescent-water-cooler-heal-amount*))
              (new-hp (min (effective-max-hp player) (+ (hp player) heal-amount))))
         (update-game-state
          state
          :player (update-entity player :hp new-hp :energy (- (entity-energy player) *rdescent-move-energy-cost*))
          :entities (substitute (update-entity shrine :use-count (and use-count (1- use-count)))
                                shrine (get-entities state))
          :message-log (append-log-messages
                        (get-message-log state)
                        (list (make-log-message (format nil "The ~A dispenses a little relief. +~D HP."
                                                        (get-name shrine) heal-amount)
                                                (entity-message-color shrine))))))))))

(defun vendor-stock-listing-text (player)
  "Return a single, human-readable string listing every entry of
*RDESCENT-VENDOR-STOCK-TABLE* (FUTURE_PLANS.md §10), 0-indexed to
match PURCHASE-COMMAND's own ITEM-INDEX, each priced via VENDOR-ITEM-
PRICE against PLAYER's own SYNERGY -- e.g. \"[0] Kombucha - 50 RSU |
[1] Scroll of PIP - 300 RSU | ...\" -- pushed onto MESSAGE-LOG by
INTERACT-WITH-FIXTURE's own VENDOR-FIXTURE method. Two different
players (or the same player at two different SYNERGY values) can see
two different price lists for the exact same catalog, since each
listing is computed fresh at interact-time rather than baked into the
VENDOR-FIXTURE itself."
  (format nil "~{~A~^ | ~}"
          (loop for entry in *rdescent-vendor-stock-table*
                for index from 0
                collect (format nil "[~D] ~A - ~D RSU" index (vendor-stock-entry-name entry)
                                (vendor-item-price (vendor-stock-entry-base-price entry) (effective-synergy player)
                                                    (platinum-corporate-amex-active-p player))))))

(defun interact-disgruntled-it-guy (state player npc)
  "Return a fresh GAME-STATE derived from STATE describing the outcome
of PLAYER talking to NPC (an NPC-FIXTURE with NPC-KIND
:DISGRUNTLED-IT-GUY, already confirmed to be at PLAYER's own position
-- see INTERACT-WITH-FIXTURE's own NPC-FIXTURE method, which calls
this). FUTURE_PLANS.md §11's own example quest-giver. Like talking to
a VENDOR-FIXTURE, this never spends ENERGY -- a conversation, not a
walk-up-and-consume interaction. Pure; STATE itself is never mutated.

  - If IT-GUY-QUEST-REWARD-CLAIMED-P (the quest is already fully paid
    out), a dismissive \"We're square. Don't push it.\" message is
    pushed; nothing else changes.
  - Else, if IT-GUY-QUEST-COMPLETE-P (accepted, and
    IT-GUY-QUEST-KILLS has reached *RDESCENT-IT-GUY-QUEST-KILL-
    TARGET*), the reward is paid out: *RDESCENT-IT-GUY-QUEST-REWARD-
    RSU* is added to PLAYER's own RSU, CLAIM-IT-GUY-QUEST-REWARD marks
    the quest's own :IT-GUY-QUEST-REWARD-CLAIMED-P flag T (so this
    branch, and RECORD-MIDDLE-MANAGER-KILLS, never fire again for this
    player), and a \"~A hands you ~D RSU\" message is pushed.
  - Else, if IT-GUY-QUEST-ACCEPTED-P (accepted, but not yet enough
    kills), a progress-report message (\"~D/~D Middle Managers
    down.\") is pushed; nothing else changes.
  - Else (never yet talked to), ACCEPT-IT-GUY-QUEST sets
    :IT-GUY-QUEST-ACCEPTED-P and resets :IT-GUY-QUEST-KILLS to 0, and
    an quest-offer message is pushed."
  (let ((color (entity-message-color npc)))
    (cond
      ((it-guy-quest-reward-claimed-p state)
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message
                             (format nil "The ~A waves you off: \"We're square. Don't push it.\"" (get-name npc))
                             color)))))
      ((it-guy-quest-complete-p state)
       (let ((claimed-state (claim-it-guy-quest-reward state)))
         (update-game-state
          claimed-state
          :player (update-entity player :rsu (+ (get-rsu player) *rdescent-it-guy-quest-reward-rsu*))
          :message-log (append-log-messages
                        (get-message-log claimed-state)
                        (list (make-log-message
                               (format nil "The ~A hands you ~D RSU: \"Knew you had it in you.\""
                                      (get-name npc) *rdescent-it-guy-quest-reward-rsu*)
                               color))))))
      ((it-guy-quest-accepted-p state)
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message
                             (format nil "The ~A: \"~D/~D Middle Managers down. Get back to me when you're done.\""
                                    (get-name npc) (it-guy-quest-kills state) *rdescent-it-guy-quest-kill-target*)
                             color)))))
      (t
       (let ((accepted-state (accept-it-guy-quest state)))
         (update-game-state
          accepted-state
          :message-log (append-log-messages
                        (get-message-log accepted-state)
                        (list (make-log-message
                               (format nil "The ~A grumbles: \"Kill ~D Middle Managers and I'll make it worth your while.\""
                                      (get-name npc) *rdescent-it-guy-quest-kill-target*)
                               color)))))))))

(defun interact-npc (state player npc)
  "Return a fresh GAME-STATE derived from STATE describing the outcome
of PLAYER talking to NPC (an NPC-FIXTURE already confirmed to be at
PLAYER's own position -- see INTERACT-WITH-FIXTURE's own NPC-FIXTURE
method, which calls this). Dispatches on NPC's own GET-NPC-KIND --
currently only :DISGRUNTLED-IT-GUY (see INTERACT-DISGRUNTLED-IT-GUY)
-- mirroring INTERACT-SHRINE's own SHRINE-KIND COND dispatch. An
unrecognized/future NPC-KIND falls through to STATE unchanged."
  (case (get-npc-kind npc)
    (:disgruntled-it-guy (interact-disgruntled-it-guy state player npc))
    (t state)))

(defgeneric interact-with-fixture (fixture state player)
  (:documentation "Dispatch helper for INTERACT-FIXTURE: return a fresh GAME-STATE
derived from STATE describing the outcome of PLAYER interacting with
FIXTURE (already confirmed to be at PLAYER's own position, and PLAYER
already confirmed IS-ALIVE with enough ENERGY to act). Dispatches on
FIXTURE's own class rather than INTERACT-FIXTURE's old TYPEP COND, so
adding a new FIXTURE subclass (NPC-FIXTURE/DOOR-FIXTURE, per
ARCHITECTURE_PLAN.md §3) only requires a new INTERACT-WITH-FIXTURE
method, not an edit to INTERACT-FIXTURE itself. The default FIXTURE
method returns STATE unchanged (no turn spent) -- today's implemented
cases are SHRINE-FIXTURE, via INTERACT-SHRINE above, VENDOR-
FIXTURE, via VENDOR-STOCK-LISTING-TEXT below (FUTURE_PLANS.md §10),
NPC-FIXTURE, via INTERACT-NPC (FUTURE_PLANS.md §11), and PLAQUE-
FIXTURE (the final-level congratulatory plaque): unlike every
other INTERACT-WITH-FIXTURE method, interacting with a VENDOR-FIXTURE,
NPC-FIXTURE, or PLAQUE-FIXTURE never spends ENERGY or mutates PLAYER at
all (barring the Disgruntled IT Guy's own quest-reward payout, itself a
deliberate exception -- see INTERACT-DISGRUNTLED-IT-GUY) -- it purely
pushes VENDOR-STOCK-LISTING-TEXT's own wares/prices, NPC dialogue, or
(for PLAQUE-FIXTURE) a \"You read the ~A.\" message plus the one-shot
:PLAQUE-TEXT GAME-STATE flag (see SET-GAME-STATE-FLAG, MECHANICS.LISP,
and RDESCENT-OUTBOUND-PACKETS/TICK-ALL-CLIENTS, RDESCENT/SERVER.LISP,
for how that flag becomes a client-side modal popup) onto MESSAGE-LOG,
a free \"look\"/conversation; PURCHASE-ITEM (driven by
the separate PURCHASE-COMMAND, not INTERACT-COMMAND) is what actually
spends RSU/ENERGY and grants an item.")
  (:method ((fixture fixture) state player)
    (declare (ignore player))
    state)
  (:method ((fixture shrine-fixture) state player)
    (interact-shrine state player fixture))
  (:method ((fixture vendor-fixture) state player)
    (update-game-state
     state
     :message-log (append-log-messages
                   (get-message-log state)
                   (list (make-log-message (format nil "The ~A offers: ~A" (get-name fixture)
                                                    (vendor-stock-listing-text player))
                                           (entity-message-color fixture))))))
  (:method ((fixture npc-fixture) state player)
    (interact-npc state player fixture))
  (:method ((fixture plaque-fixture) state player)
    (declare (ignore player))
    (set-game-state-flag
     (update-game-state
      state
      :message-log (append-log-messages
                    (get-message-log state)
                    (list (make-log-message (format nil "You read the ~A." (get-name fixture))
                                            (entity-message-color fixture)))))
     :plaque-text (get-plaque-text fixture))))

(defun interact-fixture (state)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to interact with whatever FIXTURE (if any) occupies the
player's own (X, Y) on their current LEVEL (see FIXTURE-AT) --
ARCHITECTURE_PLAN.md §3/§8's INTERACT-COMMAND reducer. Pure; STATE
itself is never mutated.

Validation, in order (mirroring DRINK-POTION/GRAB-ITEM's own
no-op-but-message convention for a logically impossible action):
  - If the player is not IS-ALIVE, STATE is returned unchanged.
  - Else, if the player's ENTITY-ENERGY is below
    *RDESCENT-MOVE-ENERGY-COST* (interacting, like drinking/grabbing,
    costs one ordinary turn), the action is rejected and STATE is
    returned unchanged.
  - Else, if FIXTURE-AT finds nothing at the player's own position,
    the interaction fails: STATE's PLAYER/ENTITIES/ENERGY are left
    unchanged (no turn is spent), but a \"There is nothing here to
    interact with.\" message is pushed onto MESSAGE-LOG in the
    player's own ENTITY-MESSAGE-COLOR.
  - Else, dispatches on the found FIXTURE's own class via
    INTERACT-WITH-FIXTURE (today only SHRINE-FIXTURE has a real
    method; any future FIXTURE subclass adds a new
    INTERACT-WITH-FIXTURE method rather than a new command --
    INTERACT-COMMAND itself never changes)."
  (let ((player (get-player state)))
    (cond
      ((not (is-alive player)) state)
      ((< (entity-energy player) *rdescent-move-energy-cost*) state)
      (t
       (let ((fixture (fixture-at state (get-x player) (get-y player) (get-level player))))
         (if (null fixture)
             (update-game-state
              state
              :message-log (append-log-messages
                            (get-message-log state)
                            (list (make-log-message "There is nothing here to interact with."
                                                    (entity-message-color player)))))
             (interact-with-fixture fixture state player)))))))

(defun purchase-item (state tier item-index)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to buy the *RDESCENT-VENDOR-STOCK-TABLE* entry at ITEM-INDEX
from whatever VENDOR-FIXTURE (if any) occupies the player's own (X, Y)
on their current LEVEL (see FIXTURE-AT) -- FUTURE_PLANS.md §10's
PURCHASE-COMMAND reducer. TIER (the client's subscription tier) is
threaded through explicitly for RDESCENT-TIER-KOMBUCHA-LIMIT/RDESCENT-
TIER-INVENTORY-LIMIT, exactly like GRAB-ITEM. Pure; STATE itself is
never mutated. Every equippable purchase is passed through
RANDOMIZE-EQUIPPABLE-ITEM-MODIFIER before landing in INVENTORY, so a
purchase may randomly come out :CURSED or :BLESSED rather than
:NORMAL (see ITEM-MODIFIER/ENTITIES.LISP) -- a non-equippable purchase
(e.g. a Scroll of PIP or Kombucha) is unaffected.

Validation, in order (mirroring GRAB-ITEM's own no-op-but-message
convention for a logically impossible/unaffordable action):
  - If the player is not IS-ALIVE, STATE is returned unchanged.
  - Else, if the player's ENTITY-ENERGY is below
    *RDESCENT-MOVE-ENERGY-COST* (purchasing, like grabbing/
    interacting, costs one ordinary turn), the action is rejected and
    STATE is returned unchanged.
  - Else, if FIXTURE-AT finds nothing at the player's own position, or
    what it finds is not a VENDOR-FIXTURE, the purchase fails: STATE's
    PLAYER/ENTITIES/ENERGY are left unchanged (no turn is spent), but
    a \"There is nothing here to buy from.\" message is pushed onto
    MESSAGE-LOG in the player's own ENTITY-MESSAGE-COLOR.
  - Else, if ITEM-INDEX is not a valid index into
    *RDESCENT-VENDOR-STOCK-TABLE*, the purchase fails identically
    (no turn spent), with a \"That item isn't for sale here.\"
    message.
  - Else, if the player's own RSU is less than VENDOR-ITEM-PRICE of
    that entry's BASE-PRICE (adjusted by the player's own SYNERGY),
    the purchase fails identically (no turn spent, no RSU/ENERGY
    change), with a \"You can't afford the ~A (~D RSU).\" message.
  - Else, if the entry's own VENDOR-STOCK-ENTRY-PAYLOAD is the keyword
    :KOMBUCHA and the player's KOMBUCHA count is already at its
    tier-specific capacity (RDESCENT-TIER-KOMBUCHA-LIMIT), the
    purchase fails identically (no turn spent, no RSU/ENERGY change --
    unlike a free GRAB-ITEM pickup, an unaffordable-to-carry purchase
    never even reaches the register), with a \"You can't carry any
    more Kombucha!\" message.
  - Else, if the entry's own PAYLOAD is a function-designator (every
    other entry) and the player's INVENTORY is already at its
    tier-specific capacity (RDESCENT-TIER-INVENTORY-LIMIT), the
    purchase fails identically, with a \"Your inventory is full!\"
    message.
  - Else the purchase succeeds: the price is deducted from the
    player's own RSU, *RDESCENT-MOVE-ENERGY-COST* is deducted from
    ENERGY, and either the player's KOMBUCHA count is incremented by 1
    (a :KOMBUCHA entry) or a freshly (FUNCALL PAYLOAD)-constructed
    RDESCENT-ITEM is appended onto INVENTORY (every other entry) --
    the VENDOR-FIXTURE itself is left completely untouched (no
    USE-COUNT to decrement -- see VENDOR-FIXTURE's own docstring) --
    and a \"You buy a ~A for ~D RSU!\" message (the entry's own NAME
    and the price actually charged) is pushed onto MESSAGE-LOG in the
    VENDOR-FIXTURE's own ENTITY-MESSAGE-COLOR."
  (let* ((player (get-player state))
         (fixture (and (is-alive player)
                       (fixture-at state (get-x player) (get-y player) (get-level player))))
         (vendor (and (typep fixture 'vendor-fixture) fixture)))
    (cond
      ((not (is-alive player)) state)
      ((< (entity-energy player) *rdescent-move-energy-cost*) state)
      ((not vendor)
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message "There is nothing here to buy from." (entity-message-color player))))))
      ((or (null item-index) (not (integerp item-index))
           (< item-index 0) (>= item-index (length *rdescent-vendor-stock-table*)))
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message "That item isn't for sale here." (entity-message-color player))))))
      (t
       (let* ((entry (nth item-index *rdescent-vendor-stock-table*))
              (price (vendor-item-price (vendor-stock-entry-base-price entry) (effective-synergy player)
                                         (platinum-corporate-amex-active-p player))))
         (cond
           ((< (get-rsu player) price)
            (update-game-state
             state
             :message-log (append-log-messages
                           (get-message-log state)
                           (list (make-log-message (format nil "You can't afford the ~A (~D RSU)."
                                                           (vendor-stock-entry-name entry) price)
                                                   (entity-message-color player))))))
           ((and (eq (vendor-stock-entry-payload entry) :kombucha)
                 (>= (get-kombucha player) (rdescent-tier-kombucha-limit tier)))
            (update-game-state
             state
             :message-log (append-log-messages
                           (get-message-log state)
                           (list (make-log-message "You can't carry any more Kombucha!" (entity-message-color player))))))
           ((and (not (eq (vendor-stock-entry-payload entry) :kombucha))
                 (>= (length (get-inventory player)) (rdescent-tier-inventory-limit tier)))
            (update-game-state
             state
             :message-log (append-log-messages
                           (get-message-log state)
                           (list (make-log-message "Your inventory is full!" (entity-message-color player))))))
           ((eq (vendor-stock-entry-payload entry) :kombucha)
            (update-game-state
             state
             :player (update-entity player
                                    :kombucha (1+ (get-kombucha player))
                                    :rsu (- (get-rsu player) price)
                                    :energy (- (entity-energy player) *rdescent-move-energy-cost*))
             :message-log (append-log-messages
                           (get-message-log state)
                           (list (make-log-message (format nil "You buy a ~A for ~D RSU!"
                                                           (vendor-stock-entry-name entry) price)
                                                   (entity-message-color vendor))))))
           (t
            (update-game-state
             state
             :player (update-entity player
                                    :inventory (append (get-inventory player)
                                                       (list (randomize-newly-spawned-equippable-item
                                                              (funcall (vendor-stock-entry-payload entry)))))
                                    :rsu (- (get-rsu player) price)
                                    :energy (- (entity-energy player) *rdescent-move-energy-cost*))
             :message-log (append-log-messages
                           (get-message-log state)
                           (list (make-log-message (format nil "You buy a ~A for ~D RSU!"
                                                           (vendor-stock-entry-name entry) price)
                                                   (entity-message-color vendor))))))))))))

(defun remove-item-at (list index)
  "Return a fresh list containing every element of LIST except the one
at position INDEX (0-indexed). Pure helper used by USE-ITEM to remove
a spent RDESCENT-ITEM from a player's INVENTORY without mutating the
original list (SBCL/CL has no O(1) immutable-list removal primitive,
so this is a plain SUBSEQ/APPEND splice)."
  (append (subseq list 0 index) (subseq list (1+ index))))

(defgeneric apply-item (item state player item-index target-x target-y)
  (:documentation "Dispatch helper for USE-ITEM: apply ITEM (already
looked up at ITEM-INDEX in PLAYER's own INVENTORY) against (TARGET-X,
TARGET-Y), returning a fresh GAME-STATE derived from STATE. Dispatches
on ITEM's own class rather than USE-ITEM's old ETYPECASE, so adding a
new RDESCENT-ITEM subclass only requires a new APPLY-ITEM method, not
an edit to USE-ITEM itself. See the TARGETED-ITEM and AREA-EFFECT-ITEM
methods for the exact success/no-op semantics of each kind."))

(defmethod apply-item ((item targeted-item) state player item-index target-x target-y)
  "Apply a TARGETED-ITEM (ITEM) at ITEM-INDEX in PLAYER's own
INVENTORY against whatever BLOCKING-ENTITY-AT occupies (TARGET-X,
TARGET-Y) on PLAYER's current LEVEL. If no such entity is there,
STATE is returned completely unchanged -- a targeted item is only
consumed on a hit, unlike an AREA-EFFECT-ITEM (see that method), which
always detonates. Otherwise the target takes *RDESCENT-PIP-DAMAGE*
plus DOMAIN-KNOWLEDGE-BONUS-DAMAGE (scaled by PLAYER's own
DOMAIN-KNOWLEDGE, clamped at 0 via MAX so a low roll never turns the
hit into a heal) flat damage (bypassing DEFENSE entirely -- a psychic
strike, not a physical one), *RDESCENT-USE-ITEM-ENERGY-COST* is deducted from
PLAYER's ENERGY, ITEM is spliced out of INVENTORY at ITEM-INDEX (via
REMOVE-ITEM-AT), and a \"You issue a ~A to the ~A! It takes ~D psychic
damage!\" message (GET-ITEM-NAME, the target's GET-NAME, and the
damage dealt) is pushed onto MESSAGE-LOG in the player's own
ENTITY-MESSAGE-COLOR. If the damage kills the target (new HP <= 0),
it is transformed into an inert corpse exactly as MOVE-PLAYER's own
combat branch does (CHAR #\\%, NAME \"remains of ~A\", BLOCKS-MOVEMENT
NIL, RENDER-ORDER 0, IS-ALIVE NIL), the target's own GET-XP is added
onto the player's, and a random Monty-Python-style obituary message
(see RANDOM-OBITUARY-MESSAGE) is pushed
ahead of the hit message (newest first, matching
BUILD-COMBAT-MESSAGES' own slain-ahead-of-hit convention). Pure."
  (let ((target (blocking-entity-at state target-x target-y (get-level player))))
    (if target
        (let* ((damage (max 0 (+ *rdescent-pip-damage* (domain-knowledge-bonus-damage (effective-domain-knowledge player)))))
               (new-hp (- (hp target) damage))
               (dies (<= new-hp 0))
               (new-target
                 (if dies
                     (update-entity target :char #\% :name (format nil "remains of ~A" (get-name target))
                                           :blocks-movement nil :render-order 0 :is-alive nil :hp new-hp)
                     (update-entity target :hp new-hp)))
               (color (entity-message-color player))
               (drop (and dies (maybe-drop-monster-loot new-target)))
               (hit-message (make-log-message
                             (format nil "You issue a ~A to the ~A! It takes ~D psychic damage!"
                                     (get-item-name item) (get-name target) damage)
                             color))
               (messages (append
                          (monster-death-drop-messages drop)
                          (if dies
                              (list (random-obituary-message (get-name target) color)
                                    hit-message)
                              (list hit-message)))))
          (record-middle-manager-kills
           (update-game-state
            state
            :player (update-entity player
                                   :energy (- (entity-energy player) *rdescent-use-item-energy-cost*)
                                   :inventory (remove-item-at (get-inventory player) item-index)
                                   :xp (if dies (+ (get-xp player) (get-xp target)) (get-xp player)))
            :entities (if drop
                          (cons drop (substitute new-target target (get-entities state)))
                          (substitute new-target target (get-entities state)))
            :message-log (append-log-messages (get-message-log state) messages))
           (if (and dies (typep target 'middle-manager)) 1 0)))
        state)))

(defun reply-all-chain-reaction (entities level radius damage color survivors)
  "Return four values -- a fresh ENTITIES list, a newest-first list of
LOG-MESSAGEs, total XP gained, and a middle-manager kill count -- for
FUTURE_PLANS.md §18.3's \"Reply-All Chain Reaction\", as detonated
from APPLY-ITEM's AREA-EFFECT-ITEM method after its own first blast
wave. Rolls *RDESCENT-REPLY-ALL-CHAIN-REACTION-CHANCE-PERCENT* once
(not once per SURVIVORS) -- if that roll fails, or SURVIVORS is empty
(nothing to chain from), ENTITIES is returned completely unchanged
alongside no messages and zero XP/kills. Otherwise, each entity in
SURVIVORS (i.e. an entity that was caught in the first blast wave but
did not die from it) is re-looked-up in ENTITIES by its current
position rather than by (possibly now-stale) EQ identity, since an
earlier SURVIVOR's own chained blast may already have killed a later
one; any SURVIVOR that is missing or no longer IS-ALIVE by the time
its turn comes is skipped. A live survivor is first offered
APPLY-STATUS-EFFECT's :CONFUSED effect (respecting SENIORITY's
Deflection Chance, exactly like CAST-REORG-MEMO -- a deflected
survivor gets no \"panics\" message, though it still detonates its own
blast), then detonates its own DAMAGE-dealing, RADIUS-wide blast
centered on its own tile against every other still-IS-ALIVE,
GET-BLOCKS-MOVEMENT entity on LEVEL (note DAMAGE is passed in by the
caller without any DOMAIN-KNOWLEDGE-BONUS-DAMAGE, since this second
wave isn't cast by the player). Entities hit by a chain blast do not
themselves chain further -- this is deliberately a single, non-
recursive wave, matching *RDESCENT-REPLY-ALL-CHAIN-REACTION-CHANCE-
PERCENT*'s own docstring. Messages read, in top-to-bottom (newest-
first) order: each survivor's own blast's hit/obituary messages, then
that survivor's own panic message (if not deflected), with earlier
SURVIVORS' entire message batches ending up *below* (older than) later
ones -- mirroring APPLY-ITEM's own newest-processed-is-newest-message
convention. Pure."
  (if (or (null survivors) (>= (random 100) *rdescent-reply-all-chain-reaction-chance-percent*))
      (values entities nil 0 0)
      (labels ((detonate-secondary-blast (entities source-x source-y exclude)
                 "Return four values (a fresh ENTITIES list, a newest-
first messages list, XP gained, and middle-manager kills) for a
DAMAGE-dealing, RADIUS-wide blast centered on (SOURCE-X, SOURCE-Y),
hitting every other IS-ALIVE, GET-BLOCKS-MOVEMENT entity on LEVEL
except EXCLUDE (the entity that is itself detonating this blast).
Mirrors APPLY-ITEM's own AREA-EFFECT-ITEM blast-wave logic exactly."
                 (let ((targets (remove-if-not
                                 (lambda (ent)
                                   (and (not (eq ent exclude)) (is-alive ent) (get-blocks-movement ent)
                                        (= (get-level ent) level)
                                        (<= (max (abs (- (get-x ent) source-x)) (abs (- (get-y ent) source-y))) radius)))
                                 entities)))
                   (destructuring-bind (new-entities messages xp mm-kills)
                       (fold-left
                        (lambda (acc ent)
                          (destructuring-bind (entities messages xp mm-kills) acc
                            (let* ((new-hp (- (hp ent) damage))
                                   (dies (<= new-hp 0))
                                   (new-ent (if dies
                                                (update-entity ent :char #\% :name (format nil "remains of ~A" (get-name ent))
                                                              :blocks-movement nil :render-order 0 :is-alive nil :hp new-hp)
                                                (update-entity ent :hp new-hp)))
                                   (hit-message (make-log-message (format nil "The ~A takes ~D damage!" (get-name ent) damage) color)))
                              (list (substitute new-ent ent entities)
                                    (if dies
                                        (append (list (random-obituary-message (get-name ent) color) hit-message) messages)
                                        (append (list hit-message) messages))
                                    (+ xp (if dies (get-xp ent) 0))
                                    (+ mm-kills (if (and dies (typep ent 'middle-manager)) 1 0))))))
                        (list entities nil 0 0)
                        targets)
                     (values new-entities messages xp mm-kills)))))
        (destructuring-bind (final-entities messages xp mm-kills)
            (fold-left
             (lambda (acc survivor)
               (destructuring-bind (entities messages xp mm-kills) acc
                 (let ((current (find-if (lambda (e) (and (= (get-x e) (get-x survivor)) (= (get-y e) (get-y survivor))
                                                           (= (get-level e) level) (is-alive e)))
                                          entities)))
                   (if (not current)
                       acc
                       (multiple-value-bind (confused-ent deflected) (apply-status-effect current :confused *rdescent-confusion-ticks*)
                         (let* ((entities-after-confusion (substitute confused-ent current entities))
                                (panic-message (unless deflected
                                                 (make-log-message (format nil "The ~A's inbox explodes with Reply-Alls! They panic!"
                                                                           (get-name confused-ent))
                                                                   color))))
                           (multiple-value-bind (blast-entities blast-messages blast-xp blast-mm-kills)
                               (detonate-secondary-blast entities-after-confusion (get-x confused-ent) (get-y confused-ent) confused-ent)
                             (list blast-entities
                                   (append blast-messages (if panic-message (list panic-message) nil) messages)
                                   (+ xp blast-xp)
                                   (+ mm-kills blast-mm-kills)))))))))
             (list entities nil 0 0)
             survivors)
          (values final-entities messages xp mm-kills)))))


(defmethod apply-item ((item area-effect-item) state player item-index target-x target-y)
  "Detonate an AREA-EFFECT-ITEM (ITEM) at ITEM-INDEX in PLAYER's own
INVENTORY, centered on (TARGET-X, TARGET-Y). Every entity in
(GET-ENTITIES STATE) that is IS-ALIVE, GET-BLOCKS-MOVEMENT, shares
PLAYER's current LEVEL, and lies within *RDESCENT-REPLY-ALL-RADIUS*
Chebyshev distance of the target tile takes *RDESCENT-REPLY-ALL-DAMAGE*
plus DOMAIN-KNOWLEDGE-BONUS-DAMAGE (scaled by PLAYER's own
DOMAIN-KNOWLEDGE, clamped at 0 via MAX) flat damage (bypassing
DEFENSE, like the TARGETED-ITEM method's PIP damage), threaded via
FOLD-LEFT so each entity's own death/XP transfer
is independent of the others (list order, same as
PROCESS-ENEMY-TURNS). Every entity hit by this first blast wave that
survives it is then offered to REPLY-ALL-CHAIN-REACTION
(FUTURE_PLANS.md §18.3, \"The Reply-All Chain Reaction\"), which rolls
*RDESCENT-REPLY-ALL-CHAIN-REACTION-CHANCE-PERCENT* once per
detonation and, on success, Confuses each survivor and sets off its
own secondary blast against other entities -- see that function's own
docstring for the full mechanic, including its two deliberate
simplifications: the chain is a single, non-recursive wave, and (since
GET-ENTITIES never includes the player at all) it can only ever catch
other monsters/fixtures in its crossfire, never PLAYER. Unlike the
TARGETED-ITEM method, this always consumes ITEM and deducts
*RDESCENT-USE-ITEM-ENERGY-COST* even if no entity happens to be caught
in the blast -- an area-effect item detonates unconditionally once
used. A \"You cast Reply-All! The thread explodes!\" announcement is
always pushed (in PLAYER's own ENTITY-MESSAGE-COLOR), followed by each
hit entity's own \"The ~A takes ~D damage!\" message (and a random
Monty-Python-style obituary message, see RANDOM-OBITUARY-MESSAGE,
ahead of it for any entity the blast kills, matching the TARGETED-ITEM
method's slain-ahead-of-hit ordering), followed in turn by any
REPLY-ALL-CHAIN-REACTION messages -- MESSAGES is newest-first
throughout, so the chain reaction's own messages (the *last* thing to
happen) end up closest to the top of the log, with the original
announcement oldest of the batch. Pure."
  (let* ((level (get-level player))
         (radius *rdescent-reply-all-radius*)
         (damage (max 0 (+ *rdescent-reply-all-damage* (domain-knowledge-bonus-damage (effective-domain-knowledge player)))))
         (color (entity-message-color player))
         (targets (remove-if-not
                   (lambda (ent)
                     (and (is-alive ent) (get-blocks-movement ent) (= (get-level ent) level)
                          (<= (max (abs (- (get-x ent) target-x)) (abs (- (get-y ent) target-y))) radius)))
                   (get-entities state))))
    (destructuring-bind (blast-entities blast-messages blast-xp-gained blast-middle-manager-kills survivors)
        (fold-left
         (lambda (acc ent)
           (destructuring-bind (entities messages xp mm-kills survivors) acc
             (let* ((new-hp (- (hp ent) damage))
                    (dies (<= new-hp 0))
                    (new-ent (if dies
                                 (update-entity ent :char #\% :name (format nil "remains of ~A" (get-name ent))
                                               :blocks-movement nil :render-order 0 :is-alive nil :hp new-hp)
                                 (update-entity ent :hp new-hp)))
                    (hit-message (make-log-message (format nil "The ~A takes ~D damage!" (get-name ent) damage) color)))
               (list (substitute new-ent ent entities)
                     (if dies
                         (append (list (random-obituary-message (get-name ent) color)
                                       hit-message)
                                 messages)
                         (append (list hit-message) messages))
                     (+ xp (if dies (get-xp ent) 0))
                     (+ mm-kills (if (and dies (typep ent 'middle-manager)) 1 0))
                     (if dies survivors (cons new-ent survivors))))))
         (list (get-entities state) nil 0 0 nil)
         targets)
      (multiple-value-bind (new-entities chain-messages chain-xp-gained chain-middle-manager-kills)
          (reply-all-chain-reaction blast-entities level radius (max 0 *rdescent-reply-all-damage*) color survivors)
        (record-middle-manager-kills
         (update-game-state
          state
          :player (update-entity player
                                 :energy (- (entity-energy player) *rdescent-use-item-energy-cost*)
                                 :inventory (remove-item-at (get-inventory player) item-index)
                                 :xp (+ (get-xp player) blast-xp-gained chain-xp-gained))
          :entities new-entities
          :message-log (append-log-messages
                        (get-message-log state)
                        (append chain-messages blast-messages (list (make-log-message "You cast Reply-All! The thread explodes!" color)))))
         (+ blast-middle-manager-kills chain-middle-manager-kills))))))

(defun cast-reorg-memo (state target-x target-y level)
  "Return two values: a fresh GAME-STATE derived from STATE, and a
LOG-MESSAGE describing the outcome, of handing a Vague Re-Org Memo to
whatever BLOCKING-ENTITY-AT occupies (TARGET-X, TARGET-Y) on LEVEL.
This is APPLY-ITEM's REORG-MEMO method's own effect logic, factored
out (mirroring how the TARGETED-ITEM/AREA-EFFECT-ITEM methods keep
each item's STATE-changing effect distinct from the player's own
energy/inventory bookkeeping, which APPLY-ITEM's caller handles) so it
can be unit-tested and reasoned about independently of ENERGY/
INVENTORY concerns. Pure; STATE itself is never mutated.
  - If no entity occupies the target tile, STATE is returned
    completely unchanged, alongside a white \"You dropped the memo on
    the floor. Nobody cares.\" message -- the memo is wasted on empty
    air, not consumed on a miss (mirroring TARGETED-ITEM's own
    no-target no-op, though APPLY-ITEM's REORG-MEMO method still
    consumes the item and spends ENERGY regardless, unlike a plain
    TARGETED-ITEM miss -- see that method).
  - Else APPLY-STATUS-EFFECT attempts to attach a :CONFUSED STATUS-
    EFFECT (*RDESCENT-CONFUSION-TICKS*, 10 turns) to the target,
    first rolling its own SENIORITY-DEFLECTION-CHANCE (see
    ARCHITECTURE_PLAN.md §1) -- CAST-REORG-MEMO is the first caller
    to wire SENIORITY's long-planned Deflection Chance up at all. If
    deflected, STATE is returned unchanged (the target's SENIORITY
    let it shrug the memo off) alongside a white \"You hand the
    Re-Org Memo to the ~A, but their SENIORITY lets them shrug it off
    without a second glance.\" message; otherwise the target's own
    HP/DEFENSE/POWER and everything else about it is untouched -- a
    Re-Org Memo deals no damage, only confusion -- and a white \"You
    hand the Re-Org Memo to the ~A. Their eyes glaze over in panic!\"
    message (naming the target via GET-NAME) is returned."
  (let ((target (blocking-entity-at state target-x target-y level)))
    (if (not target)
        (values state (make-log-message "You dropped the memo on the floor. Nobody cares."))
        (multiple-value-bind (new-target deflected) (apply-status-effect target :confused *rdescent-confusion-ticks*)
          (if deflected
              (values state (make-log-message (format nil "You hand the Re-Org Memo to the ~A, but their SENIORITY lets them shrug it off without a second glance."
                                                       (get-name target))))
              (values
               (update-game-state state :entities (substitute new-target target (get-entities state)))
               (make-log-message (format nil "You hand the Re-Org Memo to the ~A. Their eyes glaze over in panic!"
                                         (get-name target)))))))))

(defmethod apply-item ((item reorg-memo) state player item-index target-x target-y)
  "Apply a REORG-MEMO (ITEM) at ITEM-INDEX in PLAYER's own INVENTORY
against whatever BLOCKING-ENTITY-AT occupies (TARGET-X, TARGET-Y) on
PLAYER's current LEVEL, via CAST-REORG-MEMO. Unlike SCROLL-OF-PIP's
own TARGETED-ITEM method, a miss still consumes ITEM and spends
*RDESCENT-USE-ITEM-ENERGY-COST* ENERGY -- the memo, once handed over
to nobody, is still gone; only its effect (finding and confusing a
target) can fail, not its consumption. Pure."
  (multiple-value-bind (new-state message) (cast-reorg-memo state target-x target-y (get-level player))
    (update-game-state
     new-state
     :player (update-entity (get-player new-state)
                            :energy (- (entity-energy player) *rdescent-use-item-energy-cost*)
                            :inventory (remove-item-at (get-inventory player) item-index))
     :message-log (append-log-messages (get-message-log new-state) (list message)))))

(defmethod apply-item ((item equippable-item) state player item-index target-x target-y)
  "Reject USE-ITEM for an EQUIPPABLE-ITEM (ITEM) at ITEM-INDEX --
every EQUIPPABLE-ITEM (a weapon or piece of armor, RDESCENT/
ENTITIES.LISP) is a melee-only stat stick moved between INVENTORY and
EQUIPMENT via EQUIP-ITEM, never consumed/aimed like a TARGETED-ITEM/
AREA-EFFECT-ITEM, so there is nothing for USE-ITEM's own targeting
flow (TARGET-X/TARGET-Y) to do with one. STATE's PLAYER/ENTITIES/
ENERGY are left completely unchanged (no turn is spent), matching
EQUIP-ITEM's own \"You can't equip that.\" no-op-but-message
convention for the opposite mismatch; a \"You can't use that -- equip
it instead.\" message is pushed onto MESSAGE-LOG in the player's own
ENTITY-MESSAGE-COLOR. This exists purely as defense in depth against a
buggy or malicious client sending a use-item command for an
equippable item's own inventory index (the ordinary rdescent.js client
never does -- pressing Enter on an equippable inventory row equips it
directly instead of entering targeting mode, see HANDLEKEYDOWN) --
without this method, ITEM's class matches neither the TARGETED-ITEM
nor AREA-EFFECT-ITEM methods above, and USE-ITEM's dispatch through
APPLY-ITEM would otherwise signal a NO-APPLICABLE-METHOD error."
  (declare (ignore item-index target-x target-y))
  (update-game-state
   state
   :message-log (append-log-messages
                 (get-message-log state)
                 (list (make-log-message "You can't use that -- equip it instead." (entity-message-color player))))))

(defmethod apply-item ((item consumable-item) state player item-index target-x target-y)
  "Self-administer a CONSUMABLE-ITEM (ITEM) at ITEM-INDEX in PLAYER's
own INVENTORY -- see CONSUMABLE-ITEM's own docstring (RDESCENT/
ENTITIES.LISP) for exactly what each of its HEAL-AMOUNT/ENERGY-
RESTORE/EFFECT/CLEANSE-KIND/STAT-OVERRIDES/FLAVOR-TEXT slots does.
TARGET-X/TARGET-Y are ignored entirely -- a CONSUMABLE-ITEM always
acts on PLAYER alone, unlike TARGETED-ITEM/AREA-EFFECT-ITEM. Always
consumes ITEM and deducts *RDESCENT-USE-ITEM-ENERGY-COST* from
PLAYER's own ENERGY first, before ENERGY-RESTORE (if any) adds back
on top -- so a full-Energy-restore item still nets a real gain rather
than the use-cost silently eating into it. HEAL-AMOUNT is applied
next, capped at EFFECTIVE-MAX-HP (a plain integer amount) or set to
EFFECTIVE-MAX-HP outright (the keyword :FULL); then EFFECT (a
(:CHANCE <0-100> :KIND ... :TICKS-REMAINING ... [:MAGNITUDE ...]
[:EXPIRE-INTO ...]) plist, if any) is rolled against its own :CHANCE
(default 100, i.e. unconditional) and, on success, attached via
ENTITY-WITH-EFFECT (never APPLY-STATUS-EFFECT -- a self-administered
effect never rolls its own SENIORITY-DEFLECTION-CHANCE against the
very player choosing to take it); then CLEANSE-KIND (if any) is
stripped via ENTITY-WITH-EFFECT ... 0; then STAT-OVERRIDES (if any) is
applied as permanent slot overrides via UPDATE-ENTITY. Finally ITEM's
own FLAVOR-TEXT is pushed onto MESSAGE-LOG in PLAYER's own ENTITY-
MESSAGE-COLOR. Pure."
  (let* ((heal-amount (get-heal-amount item))
         (energy-restore (get-energy-restore item))
         (effect (get-consumable-effect item))
         (cleanse-kind (get-cleanse-kind item))
         (stat-overrides (get-stat-overrides item))
         (max-hp (effective-max-hp player))
         (new-hp (cond
                   ((null heal-amount) (hp player))
                   ((eq heal-amount :full) (or max-hp (hp player)))
                   (t (let ((healed (+ (hp player) heal-amount)))
                        (if max-hp (min max-hp healed) healed)))))
         (base-energy (- (entity-energy player) *rdescent-use-item-energy-cost*))
         (new-energy (cond
                       ((null energy-restore) base-energy)
                       ((eq energy-restore :full) *rdescent-pharmacy-full-energy-restore*)
                       (t (+ base-energy energy-restore))))
         (chance (or (getf effect :chance) 100))
         (effect-hits (and effect (< (random 100) chance)))
         (player-with-effect
           (if effect-hits
               (entity-with-effect player (getf effect :kind) (getf effect :ticks-remaining)
                                   (getf effect :magnitude) (getf effect :expire-into))
               player))
         (player-with-cleanse
           (if cleanse-kind
               (entity-with-effect player-with-effect cleanse-kind 0)
               player-with-effect))
         (final-player (apply #'update-entity player-with-cleanse
                              :hp new-hp
                              :energy new-energy
                              :inventory (remove-item-at (get-inventory player) item-index)
                              stat-overrides)))
    (update-game-state
     state
     :player final-player
     :message-log (append-log-messages (get-message-log state)
                                       (list (make-log-message (get-flavor-text item) (entity-message-color player)))))))

(defmethod apply-item ((item unmarked-nootropic-stack) state player item-index target-x target-y)
  "Apply the UNMARKED-NOOTROPIC-STACK's own bespoke 50/50 gamble
(RDESCENT/ENTITIES.LISP's own docstring explains why this doesn't fit
DEFINE-CONSUMABLE-ITEM's declarative shape): a flat coin flip either
attaches the same :ADDERALL-FOCUS buff Discarded Adderall grants
(*RDESCENT-ADDERALL-FOCUS-TICKS*, +DOMAIN-KNOWLEDGE via EFFECTIVE-
DOMAIN-KNOWLEDGE), via ENTITY-WITH-EFFECT (no SENIORITY-DEFLECTION-
CHANCE roll, same rationale as the CONSUMABLE-ITEM method above), or
sets PLAYER's own HP down to a bare 1 -- \"a very bad time.\" Always
consumes ITEM and deducts *RDESCENT-USE-ITEM-ENERGY-COST* from
PLAYER's own ENERGY regardless of which branch the coin flip takes.
TARGET-X/TARGET-Y are ignored entirely. Pure."
  (declare (ignore target-x target-y))
  (let* ((base-energy (- (entity-energy player) *rdescent-use-item-energy-cost*))
         (lucky (< (random 100) 50))
         (gambled-player
           (if lucky
               (entity-with-effect player :adderall-focus *rdescent-adderall-focus-ticks*)
               (update-entity player :hp 1)))
         (message (if lucky
                      "You dry-swallow an Unmarked Nootropic Stack. Whatever it was, you suddenly feel sharper."
                      "You dry-swallow an Unmarked Nootropic Stack. Your vision swims and your knees buckle. That was NOT a good idea.")))
    (update-game-state
     state
     :player (update-entity gambled-player
                           :energy base-energy
                           :inventory (remove-item-at (get-inventory player) item-index))
     :message-log (append-log-messages (get-message-log state)
                                       (list (make-log-message message (entity-message-color player)))))))

(defmethod apply-item ((item root-password-post-it-note) state player item-index target-x target-y)
  "Apply The Root Password Post-It Note's own bespoke effect
(FUTURE_PLANS.md §15, RDESCENT/ENTITIES.LISP's own docstring explains
why this doesn't fit DEFINE-CONSUMABLE-ITEM's declarative shape):
permanently sets STATE's own :SKELETON-KEY-ACTIVE GAME-STATE flag via
SET-GAME-STATE-FLAG, which KEY-HELD-P (RDESCENT/MECHANICS.LISP)
consults to let this player unlock *every* locked door for the rest of
the run, regardless of KEY-ID. Always consumes ITEM and deducts
*RDESCENT-USE-ITEM-ENERGY-COST* from PLAYER's own ENERGY.
TARGET-X/TARGET-Y are ignored entirely. Pure."
  (declare (ignore target-x target-y))
  (update-game-state
   (set-game-state-flag state :skeleton-key-active t)
   :player (update-entity player
                         :energy (- (entity-energy player) *rdescent-use-item-energy-cost*)
                         :inventory (remove-item-at (get-inventory player) item-index))
   :message-log (append-log-messages
                 (get-message-log state)
                 (list (make-log-message
                        "You memorize the Root Password. Every locked door in the building suddenly feels a lot less locked."
                        (entity-message-color player))))))

(defun use-item (state item-index target-x target-y)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to use the RDESCENT-ITEM at ITEM-INDEX in (GET-INVENTORY
PLAYER) against (TARGET-X, TARGET-Y). Pure; STATE itself is never
mutated. Validation, in order:
  - If the player is not IS-ALIVE, STATE is returned unchanged -- a
    corpse can't use an item, exactly like MOVE-PLAYER/USE-STAIRS/
    DRINK-POTION's own dead-player guards.
  - Else, if the player's ENTITY-ENERGY is below
    *RDESCENT-USE-ITEM-ENERGY-COST*, the action is rejected and STATE
    is returned unchanged, exactly as MOVE-PLAYER/DRINK-POTION reject
    an action thrown at them faster than ENERGY can refill.
  - Else, if ITEM-INDEX is not a valid index into (GET-INVENTORY
    PLAYER) (out of range, or the inventory is empty), STATE is
    returned unchanged.
  - Else the item at ITEM-INDEX is looked up and dispatched to
    APPLY-ITEM, a generic function specialized on the item's own
    class -- a TARGETED-ITEM (e.g. a Scroll of PIP) or an
    AREA-EFFECT-ITEM (e.g. a Reply-All Bomb) -- see each method's own
    docstring for its exact success/no-op semantics."
  (let* ((player (get-player state))
         (inventory (get-inventory player)))
    (cond
      ((not (is-alive player)) state)
      ((< (entity-energy player) *rdescent-use-item-energy-cost*) state)
      ((or (null item-index) (not (integerp item-index))
           (< item-index 0) (>= item-index (length inventory)))
       state)
      (t
       (let ((item (nth item-index inventory)))
         (apply-item item state player item-index target-x target-y))))))

(defun grab-item (state tier)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to pick up whatever GROUND-ITEM (if any) occupies the
player's own (X, Y) on their current LEVEL. Pure; STATE itself is
never mutated. TIER (the client's subscription tier, e.g. NIL/\"CONS\"/
\"CADR\") is threaded through explicitly rather than read off STATE
(GAME-STATE has no tier slot -- TIER lives on RDESCENT-CLIENT and is
passed as an explicit parameter everywhere else a tier-dependent limit
is needed, e.g. MOVE-PLAYER/USE-STAIRS), so callers must supply
whatever TIER EXECUTE-IMMEDIATE-COMMAND/EXECUTE-QUEUED-COMMAND were
themselves given.

Validation, in order:
  - If the player is not IS-ALIVE, STATE is returned unchanged.
  - Else, if the player's ENTITY-ENERGY is below
    *RDESCENT-MOVE-ENERGY-COST* (grabbing, like drinking, costs one
    ordinary turn), the action is rejected and STATE is returned
    unchanged, exactly as MOVE-PLAYER/DRINK-POTION reject an action
    thrown at them faster than ENERGY can refill.
  - Else, if GROUND-ITEM-AT finds nothing at the player's own
    position but FIXTURE-AT finds a shrine/vendor/other FIXTURE there,
    the grab fails with a more specific message than the generic
    empty-floor case below: STATE's PLAYER/ENTITIES/ENERGY are left
    unchanged, but a \"You can't pick up ~A -- press 't' to use it.\"
    message (~A being the FIXTURE's own GET-NAME, e.g. \"Espresso
    Machine\") is pushed in the player's own ENTITY-MESSAGE-COLOR,
    steering the player toward INTERACT-COMMAND ('t') instead of
    leaving them confused by a generic \"nothing here\" message for a
    tile that plainly has something on it.
  - Else, if GROUND-ITEM-AT finds nothing at the player's own
    position, the grab fails: STATE's PLAYER/ENTITIES/ENERGY are left
    unchanged (no turn is spent grasping at empty floor), but a
    \"There is nothing here to pick up.\" message is pushed onto
    MESSAGE-LOG in the player's own ENTITY-MESSAGE-COLOR, matching
    DRINK-POTION's own no-op-but-message convention for a logically
    impossible action.
  - Else, if the found GROUND-ITEM's GET-PAYLOAD is :KOMBUCHA:
      - If the player's KOMBUCHA count is already at its tier-specific
        capacity (RDESCENT-TIER-KOMBUCHA-LIMIT of TIER -- 5/10/25 for
        none/CONS/CADR-or-higher), the grab fails exactly like the
        empty-floor case above: STATE's PLAYER/ENTITIES/ENERGY are
        left unchanged (the Kombucha stays on the ground), but a
        \"You can't carry any more Kombucha!\" message is pushed in
        the player's own ENTITY-MESSAGE-COLOR.
      - Else the player's KOMBUCHA count is incremented by 1,
        *RDESCENT-MOVE-ENERGY-COST* is deducted from ENERGY, the
        GROUND-ITEM is removed from ENTITIES, and a \"You pick up a
        Kombucha!\" message (in the GROUND-ITEM's own
        ENTITY-MESSAGE-COLOR, i.e. green) is pushed.
  - Else, if the found GROUND-ITEM's GET-PAYLOAD is a cons whose CAR
    is :STOCK-OPTION, the player's RSU count is increased by its CDR
    (the rolled amount, see MAKE-GROUND-STOCK-OPTION), *RDESCENT-
    MOVE-ENERGY-COST* is deducted from ENERGY, the GROUND-ITEM is
    removed from ENTITIES, and a \"You cash in a Stock Option for ~D
    RSU!\" message (in the GROUND-ITEM's own ENTITY-MESSAGE-COLOR,
    i.e. gold) is pushed -- like a Kombucha, a Stock Option is never
    added to INVENTORY and so is never subject to
    RDESCENT-TIER-INVENTORY-LIMIT (or any RSU-specific cap; RSU, this
    game's gold/loot currency, has no carrying limit).
  - Else, if the found GROUND-ITEM's GET-PAYLOAD is a cons whose CAR
    is :KEY (FUTURE_PLANS.md §9, see MAKE-GROUND-KEY), its CDR (a
    key-id keyword) is added to the player's own :KEYS-HELD GAME-STATE
    flag (ADD-KEY-HELD), *RDESCENT-MOVE-ENERGY-COST* is deducted from
    ENERGY, the GROUND-ITEM is removed from ENTITIES, and a \"You pick
    up a ~A!\" message (the GROUND-ITEM's own GET-NAME, e.g.
    \"Corporate Badge\") is pushed in the GROUND-ITEM's own
    ENTITY-MESSAGE-COLOR -- like a Kombucha/Stock Option, a key is
    never added to INVENTORY (avoiding a growing key ring) and so is
    never subject to RDESCENT-TIER-INVENTORY-LIMIT.
  - Else the payload is an RDESCENT-ITEM. If the player's inventory is
    already at its tier-specific capacity (RDESCENT-TIER-INVENTORY-
    LIMIT of TIER -- 5/10/25 for none/CONS/CADR-or-higher), the grab
    fails exactly like the empty-floor case above: STATE's PLAYER/
    ENTITIES/ENERGY are left unchanged (the item stays on the ground
    for a later, roomier trip), but a \"Your inventory is full!\"
    message is pushed in the player's own ENTITY-MESSAGE-COLOR.
  - Else the grab succeeds: the payload item is appended onto
    (GET-INVENTORY PLAYER), *RDESCENT-MOVE-ENERGY-COST* is deducted
    from ENERGY, the GROUND-ITEM is removed from ENTITIES, and a
    \"You pick up a ~A!\" message (GET-ITEM-NAME of the payload) is
    pushed in the GROUND-ITEM's own ENTITY-MESSAGE-COLOR."
  (let* ((player (get-player state))
         (ground-item (and (is-alive player)
                            (ground-item-at state (get-x player) (get-y player) (get-level player)))))
    (cond
      ((not (is-alive player)) state)
      ((< (entity-energy player) *rdescent-move-energy-cost*) state)
      ((and (not ground-item) (fixture-at state (get-x player) (get-y player) (get-level player)))
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message
                             (format nil "You can't pick up ~A -- press 't' to use it."
                                     (get-name (fixture-at state (get-x player) (get-y player) (get-level player))))
                             (entity-message-color player))))))
      ((not ground-item)
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message "There is nothing here to pick up." (entity-message-color player))))))
      ((eq (get-payload ground-item) :kombucha)
       (if (>= (get-kombucha player) (rdescent-tier-kombucha-limit tier))
           (update-game-state
            state
            :message-log (append-log-messages
                          (get-message-log state)
                          (list (make-log-message "You can't carry any more Kombucha!" (entity-message-color player)))))
           (update-game-state
            state
            :player (update-entity player
                                   :kombucha (1+ (get-kombucha player))
                                   :energy (- (entity-energy player) *rdescent-move-energy-cost*))
            :entities (remove ground-item (get-entities state))
            :message-log (append-log-messages
                          (get-message-log state)
                          (list (make-log-message "You pick up a Kombucha!" (entity-message-color ground-item)))))))
      ((and (consp (get-payload ground-item)) (eq (car (get-payload ground-item)) :stock-option))
       (let ((amount (cdr (get-payload ground-item))))
         (update-game-state
          state
          :player (update-entity player
                                 :rsu (+ (get-rsu player) amount)
                                 :energy (- (entity-energy player) *rdescent-move-energy-cost*))
          :entities (remove ground-item (get-entities state))
          :message-log (append-log-messages
                        (get-message-log state)
                        (list (make-log-message (format nil "You cash in a Stock Option for ~D RSU!" amount)
                                                 (entity-message-color ground-item)))))))
      ((and (consp (get-payload ground-item)) (eq (car (get-payload ground-item)) :key))
       (let* ((key-id (cdr (get-payload ground-item)))
              (keyed-state (add-key-held state key-id)))
         (update-game-state
          keyed-state
          :player (update-entity player :energy (- (entity-energy player) *rdescent-move-energy-cost*))
          :entities (remove ground-item (get-entities state))
          :message-log (append-log-messages
                        (get-message-log keyed-state)
                        (list (make-log-message (format nil "You pick up a ~A!" (get-name ground-item))
                                                 (entity-message-color ground-item)))))))
      ((>= (length (get-inventory player)) (rdescent-tier-inventory-limit tier))
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message "Your inventory is full!" (entity-message-color player))))))
      (t
       (let ((payload (get-payload ground-item)))
         (update-game-state
          state
          :player (update-entity player
                                 :inventory (append (get-inventory player) (list payload))
                                 :energy (- (entity-energy player) *rdescent-move-energy-cost*))
          :entities (remove ground-item (get-entities state))
          :message-log (append-log-messages
                        (get-message-log state)
                        (list (make-log-message (format nil "You pick up a ~A!" (get-item-name payload))
                                                 (entity-message-color ground-item))))))))))

(defun drop-item (state item-index)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to drop the RDESCENT-ITEM at ITEM-INDEX in (GET-INVENTORY
PLAYER) onto the floor at the player's own (X, Y). Pure; STATE itself
is never mutated.

Validation, in order:
  - If the player is not IS-ALIVE, STATE is returned unchanged.
  - Else, if the player's ENTITY-ENERGY is below
    *RDESCENT-MOVE-ENERGY-COST* (dropping, like grabbing, costs one
    ordinary turn), the action is rejected and STATE is returned
    unchanged.
  - Else, if ITEM-INDEX is not a valid index into (GET-INVENTORY
    PLAYER) (out of range, or the inventory is empty), STATE is
    returned unchanged, exactly like USE-ITEM's own out-of-range
    guard.
  - Else the drop succeeds: the item at ITEM-INDEX is spliced out of
    INVENTORY (via REMOVE-ITEM-AT), *RDESCENT-MOVE-ENERGY-COST* is
    deducted from ENERGY, a fresh GROUND-ITEM (CHAR #\\?, MESSAGE-
    COLOR taken from the dropped item's own GET-ITEM-NAME via the
    same yellow/orange convention used by MAKE-GROUND-PIP/MAKE-
    GROUND-REPLY-ALL, NAME the item's own GET-ITEM-NAME, PAYLOAD the
    item itself) is created at the player's (X, Y, LEVEL) and appended
    onto ENTITIES, and a \"You drop a ~A.\" message (GET-ITEM-NAME of
    the dropped item) is pushed onto MESSAGE-LOG in the player's own
    ENTITY-MESSAGE-COLOR."
  (let* ((player (get-player state))
         (inventory (get-inventory player)))
    (cond
      ((not (is-alive player)) state)
      ((< (entity-energy player) *rdescent-move-energy-cost*) state)
      ((or (null item-index) (not (integerp item-index))
           (< item-index 0) (>= item-index (length inventory)))
       state)
      (t
       (let* ((item (nth item-index inventory))
              (dropped (make-instance 'ground-item
                                       :x (get-x player) :y (get-y player) :level (get-level player)
                                       :char #\?
                                       :name (get-item-name item)
                                       :message-color (if (typep item 'area-effect-item) "orange" "yellow")
                                       :payload item)))
         (update-game-state
          state
          :player (update-entity player
                                 :inventory (remove-item-at inventory item-index)
                                 :energy (- (entity-energy player) *rdescent-move-energy-cost*))
          :entities (append (get-entities state) (list dropped))
          :message-log (append-log-messages
                        (get-message-log state)
                        (list (make-log-message (format nil "You drop a ~A." (get-item-name item))
                                                 (entity-message-color player))))))))))

(defun equip-item (state item-index)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to equip the RDESCENT-ITEM at ITEM-INDEX in (GET-INVENTORY
PLAYER) into its own EQUIP-SLOT (ARCHITECTURE_PLAN.md §4/§8). Pure;
STATE itself is never mutated.

Validation, in order (mirroring DROP-ITEM's own no-op-but-message
convention where a message is warranted):
  - If the player is not IS-ALIVE, STATE is returned unchanged.
  - Else, if the player's ENTITY-ENERGY is below
    *RDESCENT-MOVE-ENERGY-COST* (equipping, like dropping/drinking,
    costs one ordinary turn), the action is rejected and STATE is
    returned unchanged.
  - Else, if ITEM-INDEX is not a valid index into (GET-INVENTORY
    PLAYER) (out of range, or the inventory is empty), STATE is
    returned unchanged, exactly like DROP-ITEM/USE-ITEM's own
    out-of-range guard.
  - Else, if the item at ITEM-INDEX is not an EQUIPPABLE-ITEM (e.g. a
    Scroll of PIP), the equip fails: STATE's PLAYER/ENTITIES/ENERGY
    are left unchanged (no turn is spent fumbling with an unequippable
    item), but a \"You can't equip that.\" message is pushed onto
    MESSAGE-LOG in the player's own ENTITY-MESSAGE-COLOR.
  - Else, if the item's own EQUIP-SLOT is already occupied by a
    different EQUIPPABLE-ITEM whose ITEM-CURSED-P is true, the equip
    fails exactly like UNEQUIP-ITEM's own cursed rejection (swapping
    gear implicitly unequips whatever was there first, so this closes
    the same loophole -- see ITEM-CURSED-P/EQUIPPABLE-ITEM's
    docstring): STATE's PLAYER/ENTITIES/ENERGY are left unchanged, but
    a \"The ~A is cursed and won't come off!\" message (the currently-
    equipped item's own GET-ITEM-NAME) is pushed.
  - Else the equip succeeds: the item is spliced out of INVENTORY (via
    REMOVE-ITEM-AT), passed through ITEM-UNCLOAKED (permanently
    revealing its own \"Cursed \"/\"Blessed \" MODIFIER prefix in
    GET-ITEM-NAME from now on, even after a later UNEQUIP-ITEM -- see
    EQUIPPABLE-ITEM's own class docstring for why cloaking exists at
    all), and placed into EQUIPMENT under its own EQUIP-SLOT
    (via EQUIPMENT-WITH-SLOT), *RDESCENT-MOVE-ENERGY-COST* is deducted
    from ENERGY, and a \"You equip the ~A.\" message (GET-ITEM-NAME of
    the newly equipped, now-uncloaked item) is pushed. If EQUIP-SLOT was already
    occupied by a different EQUIPPABLE-ITEM, that previously-equipped
    item is swapped back into INVENTORY (appended, same convention as
    GRAB-ITEM) rather than being discarded, and an additional \"You
    unequip the ~A.\" message for it is also pushed, ending up as the
    older (second) of the two per APPEND-LOG-MESSAGES' newest-first
    MESSAGES convention -- the \"You equip...\" line is always the
    more recent of the two."
  (let* ((player (get-player state))
         (inventory (get-inventory player)))
    (cond
      ((not (is-alive player)) state)
      ((< (entity-energy player) *rdescent-move-energy-cost*) state)
      ((or (null item-index) (not (integerp item-index))
           (< item-index 0) (>= item-index (length inventory)))
       state)
      ((not (typep (nth item-index inventory) 'equippable-item))
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message "You can't equip that." (entity-message-color player))))))
      ((let ((previous (equipped-item player (get-equip-slot (nth item-index inventory)))))
         (and previous (item-cursed-p previous)))
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message
                             (format nil "The ~A is cursed and won't come off!"
                                     (get-item-name (equipped-item player (get-equip-slot (nth item-index inventory)))))
                             (entity-message-color player))))))
      (t
       (let* ((item (item-uncloaked (nth item-index inventory)))
              (slot (get-equip-slot item))
              (previous (equipped-item player slot))
              (new-inventory (remove-item-at inventory item-index))
              (new-inventory (if previous (append new-inventory (list previous)) new-inventory))
              (messages (append (list (make-log-message (format nil "You equip the ~A." (get-item-name item))
                                                        (entity-message-color player)))
                                (and previous
                                     (list (make-log-message (format nil "You unequip the ~A." (get-item-name previous))
                                                              (entity-message-color player)))))))
         (update-game-state
          state
          :player (update-entity player
                                 :inventory new-inventory
                                 :equipment (equipment-with-slot (get-equipment player) slot item)
                                 :energy (- (entity-energy player) *rdescent-move-energy-cost*))
          :message-log (append-log-messages (get-message-log state) messages)))))))

(defun unequip-item (state slot)
  "Return a fresh GAME-STATE derived from STATE with the player having
attempted to unequip whatever EQUIPPABLE-ITEM (if any) currently
occupies EQUIPMENT SLOT (one of :WEAPON/:BODY/:HEAD/:OFF-HAND --
ARCHITECTURE_PLAN.md §4/§8). Pure; STATE itself is never mutated.

Validation, in order:
  - If the player is not IS-ALIVE, STATE is returned unchanged.
  - Else, if the player's ENTITY-ENERGY is below
    *RDESCENT-MOVE-ENERGY-COST*, the action is rejected and STATE is
    returned unchanged.
  - Else, if EQUIPPED-ITEM finds nothing in SLOT, the unequip fails:
    STATE's PLAYER/ENTITIES/ENERGY are left unchanged (no turn is
    spent), but a \"Nothing is equipped there.\" message is pushed
    onto MESSAGE-LOG in the player's own ENTITY-MESSAGE-COLOR,
    matching GRAB-ITEM/INTERACT-FIXTURE's own no-op-but-message
    convention for a logically impossible action.
  - Else, if that item's own ITEM-CURSED-P is true, the unequip fails:
    STATE's PLAYER/ENTITIES/ENERGY are left unchanged (no turn is
    spent trying), but a \"The ~A is cursed and won't come off!\"
    message (the cursed item's own GET-ITEM-NAME) is pushed onto
    MESSAGE-LOG -- see EQUIPPABLE-ITEM's own docstring for what marks
    an item CURSED in the first place (a randomly-rolled, per-instance
    ITEM-MODIFIER -- e.g. an unlucky find of the Unwashed Hoodie).
  - Else the unequip succeeds: the item is removed from EQUIPMENT (via
    EQUIPMENT-WITH-SLOT with a NIL ITEM) and appended onto INVENTORY,
    *RDESCENT-MOVE-ENERGY-COST* is deducted from ENERGY, and a \"You
    unequip the ~A.\" message (GET-ITEM-NAME of the removed item) is
    pushed. Unlike EQUIP-ITEM, this never fails for being at capacity
    -- INVENTORY's own RDESCENT-TIER-INVENTORY-LIMIT is only ever
    enforced by GRAB-ITEM picking up new loot, not by moving an item
    the player is already carrying (in one form or another) back into
    INVENTORY from EQUIPMENT."
  (let* ((player (get-player state))
         (item (equipped-item player slot)))
    (cond
      ((not (is-alive player)) state)
      ((< (entity-energy player) *rdescent-move-energy-cost*) state)
      ((not item)
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message "Nothing is equipped there." (entity-message-color player))))))
      ((item-cursed-p item)
       (update-game-state
        state
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message (format nil "The ~A is cursed and won't come off!" (get-item-name item))
                                              (entity-message-color player))))))
      (t
       (update-game-state
        state
        :player (update-entity player
                               :inventory (append (get-inventory player) (list item))
                               :equipment (equipment-with-slot (get-equipment player) slot nil)
                               :energy (- (entity-energy player) *rdescent-move-energy-cost*))
        :message-log (append-log-messages
                      (get-message-log state)
                      (list (make-log-message (format nil "You unequip the ~A." (get-item-name item))
                                              (entity-message-color player)))))))))
