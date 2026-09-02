;;; -*- Lisp -*-

;;; Small pure ENTITY helpers/factories that were originally the tail
;;; of the monolithic RDESCENT/ENTITIES.LISP: the FUTURE_PLANS.md §16
;;; scavenger-hunt AUTO-PICKUP-ITEM class and its MAKE-COLLECTIBLE
;;; factory (see RDESCENT/ENTITIES.LISP's own COLLECTIBLE-ITEM/
;;; COLLECTIBLE-SET structs), FORMAT-XP-FOR-HTML/FORMAT-RSU-FOR-HTML/
;;; HEALTH-PERCENTAGE (HTML/stats-panel formatting helpers), GROUP-
;;; INVENTORY-FOR-DISPLAY (which TYPEP-checks EQUIPPABLE-ITEM, hence
;;; this file's load-order dependency on RDESCENT/ITEMS/* below), and
;;; MAKE-STAIRS-UP/MAKE-STAIRS-DOWN/SPAWN-STAIRS-FOR-LEVEL.
;;;
;;; Deliberately loaded after RDESCENT/ITEMS/* (unlike RDESCENT/
;;; ENTITIES.LISP itself, which loads first) purely because GROUP-
;;; INVENTORY-FOR-DISPLAY's own (TYPEP ITEM 'EQUIPPABLE-ITEM) check
;;; needs the EQUIPPABLE-ITEM class (RDESCENT/ITEMS/BASE.LISP) to
;;; already exist; every other function here is independent of the
;;; item hierarchy. See RDESCENT/ENTITIES.LISP's own header comment for
;;; this engine's full file map.

(in-package "JRM-CODE-PROJECT")

(defclass auto-pickup-item (entity)
  ((item-id :initarg :item-id :reader get-item-id))
  (:default-initargs :blocks-movement nil :render-order 0 :is-alive nil :char #\~)
  (:documentation "An ENTITY representing one FUTURE_PLANS.md §16
scavenger-hunt collectible lying on the dungeon floor -- unlike a
GROUND-ITEM, it is picked up silently and automatically the instant a
player merely steps onto its tile (see MAYBE-AUTO-PICKUP-COLLECTIBLE,
RDESCENT/ACTIONS.LISP), with no explicit GRAB-ITEM command required
and no INVENTORY slot ever occupied. BLOCKS-MOVEMENT defaults to NIL
and RENDER-ORDER to 0 (drawn underneath the player/monsters, mirroring
GROUND-ITEM's own convention) so a collectible never obstructs
movement and never hides a living actor standing on its tile;
IS-ALIVE defaults to NIL so PROCESS-ENEMY-TURNS never offers it a
monster's turn. CHAR defaults to #\\~ -- a glyph no other ENTITY in
this codebase currently uses, so a collectible is always visually
distinct from every ordinary GROUND-ITEM/trap/fixture/monster glyph.
ITEM-ID (read via GET-ITEM-ID) is a keyword uniquely identifying which
*RDESCENT-COLLECTIBLE-CATALOG* entry this is (see FIND-COLLECTIBLE-
ITEM) -- MAYBE-AUTO-PICKUP-COLLECTIBLE adjoins it onto the player's
own COLLECTION-LOG (idempotent, so a duplicate pickup of an
already-owned ITEM-ID -- e.g. after the *RDESCENT-COLLECTIBLE-CATALOG*
index cycles past depth 23 -- is a harmless no-op rather than a crash
or a double-count) and removes this entity from the level entirely.
See MAKE-COLLECTIBLE for the concrete factory, and
SPAWN-COLLECTIBLES-FOR-LEVEL for procedural placement. An
AUTO-PICKUP-ITEM's ITEM-ID survives a pass through UPDATE-ENTITY
(e.g. ACCRUE-ENERGY's per-tick sweep over every entity in
REDUCE-TICK) exactly like GROUND-ITEM's own PAYLOAD -- see
UPDATE-ENTITY's own docstring."))

(defun make-collectible (x y level item-id)
  "Pure factory: return a fresh AUTO-PICKUP-ITEM at (X, Y) on LEVEL
representing the *RDESCENT-COLLECTIBLE-CATALOG* entry named ITEM-ID
(a keyword, e.g. :HOLLERITH-PUNCH-CARDS) lying on the floor -- NAME
and MESSAGE-COLOR looked up from that entry's own COLLECTIBLE-ITEM-
NAME and its owning COLLECTIBLE-SET's own COLLECTIBLE-SET-MESSAGE-
COLOR (so every item in the same set shares one color), ITEM-ID the
keyword itself. Signals an error via FIND-COLLECTIBLE-ITEM's implicit
FIND failure (returning NIL, then choking on COLLECTIBLE-ITEM-NAME of
NIL) if ITEM-ID is not present in *RDESCENT-COLLECTIBLE-CATALOG* --
deliberately not defensively guarded, since every caller (SPAWN-
COLLECTIBLES-FOR-LEVEL) only ever passes an ITEM-ID it just read out
of that same catalog."
  (let* ((item (find-collectible-item item-id))
         (set (find-collectible-set (collectible-item-set item))))
    (make-instance 'auto-pickup-item :x x :y y :level level :item-id item-id
                                      :name (collectible-item-name item)
                                      :message-color (collectible-set-message-color set))))

(defun format-xp-for-html (player)
  "Return the PLAYER entity's GET-XP as a right-justified, 9-character
wide string suitable for embedding in monospace HTML (see
RDESCENT-PLAYER-STATS-PACKET), padded on the left with literal
\"&nbsp;\" entities (six characters of markup per padding
character) rather than plain space characters, since HTML collapses
consecutive literal spaces to one when rendered, which would defeat
the right-justification. If PLAYER's XP already prints at 9 characters
or wider, no padding is added (never truncated)."
  (let* ((xp-str (write-to-string (get-xp player)))
         (pad-len (max 0 (- 9 (length xp-str)))))
    (with-output-to-string (s)
      (loop repeat pad-len do (write-string "&nbsp;" s))
      (write-string xp-str s))))

(defun format-rsu-for-html (player)
  "Return the PLAYER entity's GET-RSU as a right-justified,
8-character wide string suitable for embedding in monospace HTML (see
RDESCENT-PLAYER-STATS-PACKET), padded on the left with literal
\"&nbsp;\" entities exactly like FORMAT-XP-FOR-HTML -- but one
character narrower (8 rather than 9) so the padded digits still line
up under XP's own once each is prefixed with its own label: \"XP: \" is
4 characters while \"RSU: \" is 5, so shaving one character off RSU's
own padding width keeps \"XP: \"+9-wide-value and \"RSU: \"+8-wide-value
the same total length, meaning both values' rightmost (least
significant) digits land in the same column when the two lines are
stacked in the stats panel. If PLAYER's RSU already prints at 8
characters or wider, no padding is added (never truncated)."
  (let* ((rsu-str (write-to-string (get-rsu player)))
         (pad-len (max 0 (- 8 (length rsu-str)))))
    (with-output-to-string (s)
      (loop repeat pad-len do (write-string "&nbsp;" s))
      (write-string rsu-str s))))

(defun health-percentage (player)
  "Return PLAYER's remaining health as an integer percentage in
[0, 100], (FLOOR (* 100 (/ HP MAX-HP))), used by
RDESCENT-PLAYER-STATS-PACKET to size the HP bar's green/red spans.
Safe against division by zero or an unset MAX-HP/HP (either NIL, per
ENTITY's own :INITFORM, or 0) -- either case returns 0 rather than
signaling, and the result is always clamped to [0, 100] even if HP has
somehow been left greater than MAX-HP."
  (let ((hp (hp player)) (max (max-hp player)))
    (if (or (null hp) (null max) (<= max 0))
        0
        (max 0 (min 100 (floor (* 100 (/ hp max))))))))

(defun group-inventory-for-display (inventory)
  "Return a list of (NAME COUNT INDEX EQUIPPABLE-P) lists summarizing
INVENTORY (a list of RDESCENT-ITEMs, in the order carried) for display
purposes: one entry per distinct GET-ITEM-NAME, in first-appearance
order, where COUNT is how many items in INVENTORY share that NAME and
INDEX is the position (0-based) of the *first* such item within
INVENTORY itself -- the index a client should reference (e.g. in a
subsequent \"use-item\"/\"drop\"/\"equip\" command) to act on one
instance of that item -- and EQUIPPABLE-P is whether that first
instance is an EQUIPPABLE-ITEM (via TYPEP), letting /js/rdescent.js's
inventory modal offer an \"e\" (equip) shortcut only for rows where
it would actually succeed. Used by RDESCENT-INVENTORY-PACKET
(RDESCENT/SERVER.LISP) so /js/rdescent.js's inventory modal lists
\"Scroll of PIP (x2)\" instead of two separate \"Scroll of PIP\" rows,
without changing how USE-ITEM/DROP-ITEM/EQUIP-ITEM address items --
INDEX still refers straight into the player's own flat INVENTORY list,
so using, dropping, or equipping \"one\" of a grouped entry simply
consumes the first (lowest-index) instance, leaving the rest still
carried. Pure."
  (let ((counts (make-hash-table :test 'equal))
        (order nil))
    (loop for item in inventory
          for idx from 0
          for name = (get-item-name item)
          do (let ((entry (gethash name counts)))
               (if entry
                   (incf (second entry))
                   (let ((new-entry (list name 1 idx (typep item 'equippable-item))))
                     (setf (gethash name counts) new-entry)
                     (push new-entry order)))))
    (nreverse order)))

(defun make-stairs-up (x y level)
  "Pure factory: return a fresh ENTITY representing a staircase leading
back up to LEVEL - 1, placed at (X, Y) on LEVEL -- char #\\<, NAME
\"Stairs Up\", BLOCKS-MOVEMENT NIL (purely a marker tile the player can
freely stand on/walk through -- see SPAWN-STAIRS-FOR-LEVEL), RENDER-
ORDER 0 (drawn underneath the player/monsters when a living actor
shares its tile, mirroring the corpse convention -- see MOVE-PLAYER),
and IS-ALIVE NIL (it's a fixture, not a combatant; never targeted by
attacks or AI)."
  (make-instance 'entity :x x :y y :char #\< :name "Stairs Up" :blocks-movement nil :level level
                        :render-order 0 :is-alive nil))

(defun make-stairs-down (x y level)
  "Pure factory: return a fresh ENTITY representing a staircase leading
down to LEVEL + 1, placed at (X, Y) on LEVEL -- char #\\>, NAME
\"Stairs Down\", BLOCKS-MOVEMENT NIL, RENDER-ORDER 0, IS-ALIVE NIL. See
MAKE-STAIRS-UP for the rationale behind these values."
  (make-instance 'entity :x x :y y :char #\> :name "Stairs Down" :blocks-movement nil :level level
                        :render-order 0 :is-alive nil))

(defun spawn-stairs-for-level (tier level rooms max-depth)
  "Return a fresh list of staircase ENTITYs (via MAKE-STAIRS-UP/MAKE-
STAIRS-DOWN) for TIER/LEVEL, given ROOMS (the list of RECT-ROOMs
returned by GENERATE-DUNGEON) and MAX-DEPTH (this connection's
deepest permitted LEVEL, per RDESCENT-TIER-MAX-DEPTH). Room 0 (the
first room in ROOMS, where the player spawns -- see MAKE-INITIAL-
STATE) gets a Stairs-Up entity at its center whenever LEVEL > 1, since
there's nowhere to ascend to from LEVEL 1. A single room chosen at
random from the remaining rooms (i.e. never room 0) gets a Stairs-Down
entity at its center whenever LEVEL < MAX-DEPTH, since a client
already at its tier's depth ceiling has nowhere further to descend to.
Deterministic and pure: *RANDOM-STATE* is rebound (dynamically) to
MAKE-DETERMINISTIC-RANDOM-STATE's result for the duration of this
call -- a fresh binding independent of the one SPAWN-MONSTERS-FOR-LEVEL
uses, so adding/removing stairs never perturbs monster placement (or
vice versa) for the same TIER/LEVEL -- so the same (TIER, LEVEL, ROOMS,
MAX-DEPTH) always yields an EQUAL list of stairs entities. Returns NIL
if ROOMS is empty (no rooms were successfully carved) or if there is
only one room and LEVEL <= 1 and LEVEL >= MAX-DEPTH (no stairs of
either kind apply)."
  (let ((*random-state* (make-deterministic-random-state tier level))
        (entities nil))
    (when rooms
      (let ((spawn-room (first rooms)))
        (when (> level 1)
          (push (make-stairs-up (rect-room-center-x spawn-room)
                                 (rect-room-center-y spawn-room)
                                 level)
                entities))
        (let ((other-rooms (rest rooms)))
          (when (and other-rooms (< level max-depth))
            (let ((exit-room (nth (random (length other-rooms)) other-rooms)))
              (push (make-stairs-down (rect-room-center-x exit-room)
                                      (rect-room-center-y exit-room)
                                      level)
                    entities))))))
    (nreverse entities)))
