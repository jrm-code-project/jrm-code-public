;;; -*- Lisp -*-

;;; Procedural dungeon generation (RECT-ROOM/DIG-ROOM/DIG-TUNNEL,
;;; the per-(TIER,LEVEL) *DUNGEON-CACHE*, GENERATE-DUNGEON,
;;; CHOOSE-ROOM-KIND's depth-parameterized room-type tagging,
;;; SPAWN-TABLE-ENTRY/SPAWN-TABLE-CHOICE's depth-gated weighted-choice
;;; spawning, SPAWN-MONSTERS-FOR-LEVEL/SPAWN-ITEMS-FOR-LEVEL) and
;;; field-of-view/fog-of-war math (FOV-BRESENHAM-LINE/
;;; FOV-PERIMETER-CELLS/COMPUTE-FOV).
;;;
;;; Third of several files this engine was split across (originally a
;;; single ENGINE.LISP) -- see RDESCENT/ENTITIES.LISP's own header
;;; comment for the full file map and the value types this file builds
;;; on top of (ENTITY/GAME-MAP/TILE/etc.).

(in-package "JRM-CODE-PROJECT")

(defstruct rect-room
  "A candidate rectangular room during dungeon generation, from corner
(X1, Y1) to corner (X2, Y2) inclusive-exclusive (X1 <= X < X2, Y1 <= Y
< Y2). Purely a generation-time scratch value -- never stored in a
GAME-MAP or GAME-STATE, which only ever see the finished TILE array.
All slots are :READ-ONLY: no code ever SETFs a RECT-ROOM's fields
after construction.

KIND (see ARCHITECTURE_PLAN.md §6) is the room-type tag -- :CUBICLE,
:OPEN-OFFICE, :SERVER-ROOM, or NIL -- rolled once by CHOOSE-ROOM-KIND
when the room is first placed in GENERATE-DUNGEON, and stamped onto
every one of the room's floor TILEs by DIG-ROOM (see TILE's own
ROOM-KIND slot). Defaults to NIL so existing callers that construct a
RECT-ROOM directly (e.g. test fixtures) without a :KIND continue to
work unchanged."
  (x1 nil :read-only t)
  (y1 nil :read-only t)
  (x2 nil :read-only t)
  (y2 nil :read-only t)
  (kind nil :read-only t))

(defun rect-room-center-x (room)
  "Return the X coordinate of ROOM's center, rounded down."
  (floor (+ (rect-room-x1 room) (rect-room-x2 room)) 2))

(defun rect-room-center-y (room)
  "Return the Y coordinate of ROOM's center, rounded down."
  (floor (+ (rect-room-y1 room) (rect-room-y2 room)) 2))

(defun rect-room-intersects-p (a b)
  "Return true if rectangles A and B (both RECT-ROOMs) overlap,
including touching edges (so generated rooms always have at least a
1-tile wall gap between them)."
  (and (<= (rect-room-x1 a) (rect-room-x2 b))
       (>= (rect-room-x2 a) (rect-room-x1 b))
       (<= (rect-room-y1 a) (rect-room-y2 b))
       (>= (rect-room-y2 a) (rect-room-y1 b))))

(defun room-kind-weights-for-level (level)
  "Return a plist of (ROOM-KIND WEIGHT ...) for LEVEL, the weighted
distribution CHOOSE-ROOM-KIND draws from -- ARCHITECTURE_PLAN.md §6's
depth-parameterized room-type mix. Shallow LEVELs favor :CUBICLE (a
dense, partitioned Cubicle Maze); as LEVEL increases, :OPEN-OFFICE
grows at :CUBICLE's expense, up to a 60% cap, so the deepest floors
trend toward wide-open, exposed \"Agile Workspaces\" (see the earlier
FUTURE_PLANS.md discussion this section was designed against).
:SERVER-ROOM holds a constant 20% share at every depth -- it's a
distinct flavor of muffled room (see ROOM-ACOUSTICS), not a depth-
gated one. All three weights always sum to 100."
  (let* ((open-office (min 60 (max 5 (* level 3))))
         (server-room 20)
         (cubicle (max 10 (- 100 open-office server-room))))
    (list :cubicle cubicle :open-office open-office :server-room server-room)))

(defun choose-room-kind (level)
  "Return a single room-kind keyword (:CUBICLE, :OPEN-OFFICE, or
:SERVER-ROOM) for a newly-placed room on LEVEL, chosen via a weighted
random draw over ROOM-KIND-WEIGHTS-FOR-LEVEL's distribution. Assumes
*RANDOM-STATE* is already dynamically bound by the caller (see
GENERATE-DUNGEON), so this call's randomness participates in that same
deterministic, per-(TIER, LEVEL) sequence."
  (let* ((weights (room-kind-weights-for-level level))
         (total (loop for (nil weight) on weights by #'cddr sum weight))
         (roll (random total)))
    (loop for (kind weight) on weights by #'cddr
          do (decf roll weight)
          when (< roll 0) return kind
          finally (return (first weights)))))

(defun room-kind-display-name (kind)
  "Return a short, human-readable string naming room-kind KIND (a
TILE's GET-ROOM-KIND value -- :CUBICLE, :OPEN-OFFICE, :SERVER-ROOM, or
NIL) for display in the client's sidebar (see RDESCENT-PLAYER-STATS-
PACKET in rdescent/server.lisp and #stats-room in views.lisp):
:CUBICLE is \"Cubicle Farm\", :OPEN-OFFICE is \"Open Office\", and
:SERVER-ROOM is \"Server Room\", matching this file's own flavor names
for each archetype (see ROOM-SIZE-MULTIPLIER-FOR-KIND's docstring);
NIL (a corridor tile, outside any room) is \"Corridor\". Pure."
  (case kind
    (:cubicle "Cubicle Farm")
    (:open-office "Open Office")
    (:server-room "Server Room")
    (t "Corridor")))

(defun room-size-multiplier-for-kind (kind)
  "Return a multiplier applied to a newly-placed room's MIN-SIZE/
MAX-SIZE for the given room KIND (FUTURE_PLANS.md §4's physical
room-generation-parameter scaling by depth): :CUBICLE rooms run
smaller than the baseline (a dense, partitioned \"Cubicle Maze\"/
\"Cubicle Farm\" layout, per §4's levels-1-5 description), :OPEN-OFFICE
rooms run larger (the wide-open \"Agile Workspace\" floor plans with
long sightlines §4 describes for the deepest levels), and
:SERVER-ROOM stays at the plain MIN-SIZE/MAX-SIZE baseline -- it's a
constant-weight muffled-room flavor (see ROOM-ACOUSTICS), not itself a
depth-scaled archetype like the other two (see ROOM-KIND-WEIGHTS-FOR-
LEVEL's own docstring). Since ROOM-KIND-WEIGHTS-FOR-LEVEL already
shifts the CUBICLE/OPEN-OFFICE mix toward OPEN-OFFICE as LEVEL
increases, this indirectly makes rooms trend larger (and, since a
fixed WIDTH x HEIGHT grid holds proportionally fewer/thinner walls
between bigger rooms, less wall-dense) with depth without needing a
second, independent LEVEL-keyed table that could drift out of sync
with the room-kind distribution."
  (case kind
    (:cubicle 0.7)
    (:open-office 1.4)
    (t 1.0)))

(defun dig-room (grid room)
  "Destructively carve ROOM (a RECT-ROOM) into GRID (a private, local
2D array of TILEs, as allocated by GENERATE-DUNGEON), replacing every
cell strictly inside ROOM's bounds with a fresh walkable floor TILE
(GET-WALKABLE = T, GET-CHAR = #\\., GET-ROOM-KIND = ROOM's own KIND --
see ARCHITECTURE_PLAN.md §6). GRID is mutated in place -- this
is the one deliberately imperative step of dungeon generation; see
this section's banner comment for why that's safe."
  (let ((room-kind (rect-room-kind room)))
    (dorange (y (1+ (rect-room-y1 room)) (rect-room-y2 room))
      (dorange (x (1+ (rect-room-x1 room)) (rect-room-x2 room))
        (setf (aref grid y x) (make-instance 'tile :walkable t :char #\. :room-kind room-kind)))))
  grid)

(defun dig-tunnel (grid x1 y1 x2 y2)
  "Destructively carve an L-shaped, single-tile-wide floor tunnel in
GRID connecting (X1, Y1) to (X2, Y2), taking a random one of the two
possible orthogonal routes (horizontal-then-vertical or
vertical-then-horizontal) each time -- no TCOD-style pathfinding, just
a coin flip via (RANDOM 2). Since X1/Y1 and X2/Y2 are themselves room
centers (see GENERATE-DUNGEON), part of the L-shape's path runs
through each endpoint room's own already-room-kind-stamped interior:
CARVE preserves any pre-existing tile's own ROOM-KIND rather than
blanking it back to NIL, so a tunnel never re-labels a room's own
floor as a corridor (which would otherwise make that room's name
\"bleed\" corridor tiles into its interior/border -- only genuinely new
corridor cells, which had no room-kind to begin with, end up NIL. GRID
is mutated in place. Returns (VALUES
GRID ELBOW-X ELBOW-Y): ELBOW-X/ELBOW-Y is the tunnel's own \"corner\"
tile -- (X2, Y1) for the horizontal-then-vertical route, (X1, Y2) for
vertical-then-horizontal -- guaranteed to have just been carved by
this call regardless of which route was taken, and guaranteed (since
RECT-ROOM-INTERSECTS-P already forbids two rooms from ever touching)
to sit somewhere along the corridor rather than inside either room's
own interior as long as the two rooms aren't pathologically close.
GENERATE-DUNGEON uses this as the one deterministic candidate tile per
tunnel a locked door (FUTURE_PLANS.md §9) could occupy, without
needing to duplicate this function's own routing logic to rediscover
which cells it carved."
  (flet ((carve (x y)
           (let ((existing (aref grid y x)))
             (setf (aref grid y x)
                   (make-instance 'tile :walkable t :char #\.
                                  :room-kind (and existing (get-room-kind existing)))))))
    (if (= (random 2) 0)
        (progn
          (dorange (x (min x1 x2) (1+ (max x1 x2))) (carve x y1))
          (dorange (y (min y1 y2) (1+ (max y1 y2))) (carve x2 y))
          (values grid x2 y1))
        (progn
          (dorange (y (min y1 y2) (1+ (max y1 y2))) (carve x1 y))
          (dorange (x (min x1 x2) (1+ (max x1 x2))) (carve x y2))
          (values grid x1 y2)))))

(defparameter *dungeon-cache-max-entries* 200
  "Maximum number of (TIER . LEVEL) dungeon layouts *DUNGEON-CACHE* will
retain at once. When a cache miss would push the table over this
limit, the oldest surviving entries (by insertion order, tracked in
*DUNGEON-CACHE-INSERTION-ORDER*) are evicted first. See
TECHNICAL_DEBT.md item #33: without a bound, this table would retain
every dungeon ever generated for the server's entire lifetime, growing
without limit as clients explore ever-deeper (\"LAMBDA\"-tier depths go
up to 65536). 200 entries is generous headroom above what a single
play session realistically visits, while still bounding total memory
to a fixed multiple of one dungeon's size (roughly 100x33 TILE
instances plus a ROOMS list) rather than an unbounded one.")

(defvar *dungeon-cache-insertion-order* nil
  "A list of *DUNGEON-CACHE* keys ((TIER . LEVEL) conses), most-
recently-inserted first, used only to decide which entries to evict
once *DUNGEON-CACHE-MAX-ENTRIES* is exceeded. This is a simple FIFO
eviction policy (oldest inserted, not least-recently-used/accessed) --
deliberately simpler than true LRU, which would require updating this
list on every cache *hit* (i.e. on the hot path of every GENERATE-
DUNGEON call, not just the rare cache-miss path), for a bound whose
only real job is to keep memory finite, not to guarantee the single
theoretically-best set of entries is retained.")

(defvar *dungeon-cache* (make-hash-table :test 'equal)
  "Memoizes GENERATE-DUNGEON's result by (CONS TIER LEVEL), each entry
itself a (LIST GAME-MAP ROOMS LOCKED-DOOR) -- ROOMS a list of
RECT-ROOMs in generation order, LOCKED-DOOR a single LOCKED-DOOR
struct or NIL (FUTURE_PLANS.md §9) -- so repeated visits to the same
(tier, level) reuse the identical GAME-MAP/ROOMS/LOCKED-DOOR instances
instead of re-running generation (which would be wasted CPU, even
though it's deterministic and would produce an EQUAL layout). Bounded
to at most *DUNGEON-CACHE-MAX-ENTRIES* entries; see *DUNGEON-CACHE-
INSERTION-ORDER* for the eviction policy.")

(defun evict-oldest-dungeon-cache-entries ()
  "Evict entries from *DUNGEON-CACHE* (oldest-inserted first, per
*DUNGEON-CACHE-INSERTION-ORDER*) until its size is at most
*DUNGEON-CACHE-MAX-ENTRIES*. Called after every cache-miss insertion in
GENERATE-DUNGEON; a no-op whenever the cache is already within bounds."
  (loop while (> (hash-table-count *dungeon-cache*) *dungeon-cache-max-entries*)
        do (let ((oldest-key (car (last *dungeon-cache-insertion-order*))))
             (setf *dungeon-cache-insertion-order*
                   (nbutlast *dungeon-cache-insertion-order*))
             (remhash oldest-key *dungeon-cache*))))

(defstruct locked-door
  "A single locked door placed by GENERATE-DUNGEON (FUTURE_PLANS.md
§9), gating access to every room from SAFE-ROOM-COUNT onward in that
call's own ROOMS list. X/Y is the corridor tile GENERATE-DUNGEON
converted from an ordinary DIG-TUNNEL elbow into a locked door TILE
(GET-WALKABLE NIL, GET-LOCKED-KEY-ID/GET-LOCKED-KEY-NAME set) --
shared, immutable dungeon geometry exactly like the rest of the TILE
array, safe to memoize in *DUNGEON-CACHE*. KEY-ID is the keyword
MOVE-PLAYER/GRAB-ITEM/KEY-HELD-P match against the player's own
KEYS-HELD flag (ARCHITECTURE_PLAN.md §9) to decide whether they may
open it; KEY-NAME is its human-readable display name (e.g. \"Corporate
Badge\"), used both by MAKE-GROUND-KEY (for the matching pickup's own
NAME) and by MOVE-PLAYER's own \"you need a ~A\" message.
SAFE-ROOM-COUNT is how many of ROOMS (counting from the front, i.e.
the player's own spawn room first) are reachable *without* passing
through this door -- ARCHITECTURE_PLAN.md §7's own generation-time
reachability constraint -- so SPAWN-KEYS-FOR-LEVEL knows which rooms
are eligible to hold this door's own matching key. This works because
GENERATE-DUNGEON's own ROOMS form a strict linear chain (each new room
is tunneled only to the immediately previous one -- see DIG-TUNNEL's
own call site below), so gating the tunnel between ROOMS[SAFE-ROOM-
COUNT - 1] and ROOMS[SAFE-ROOM-COUNT] necessarily blocks every room
from SAFE-ROOM-COUNT onward and only those rooms -- no BFS/flood-fill
over a room-connectivity graph is needed, unlike ARCHITECTURE_PLAN.md
§7's own more general framing of this problem, because this dungeon
generator's own topology happens to already be that simple."
  (x nil :read-only t)
  (y nil :read-only t)
  (key-id nil :read-only t)
  (key-name nil :read-only t)
  (safe-room-count nil :read-only t))

(defparameter *rdescent-locked-door-chance* 0.25
  "Probability (out of 1.0) that GENERATE-DUNGEON places a LOCKED-DOOR
at all for a given (TIER, LEVEL) -- see that function. Chosen to sit
in the same \"rare landmark\" ballpark as *RDESCENT-FIXTURE-SPAWN-
CHANCE*/*RDESCENT-TRAP-SPAWN-CHANCE* (both 0.2): a locked door is a
deliberate detour/reward loop, not an every-floor gate.")

(defparameter *rdescent-locked-door-max-attempts* 10
  "Maximum number of candidate tunnel elbows GENERATE-DUNGEON will try
before giving up on placing a LOCKED-DOOR for a level entirely (rather
than looping forever): an elbow is rejected if it happens to fall
inside either of the two rooms its own tunnel connects (a rare
placement accident -- see DIG-TUNNEL's own docstring), mirroring the
same \"try N times, then just skip this rare landmark for this level\"
tolerance already established by SPAWN-FIXTURES-FOR-LEVEL/SPAWN-TRAPS-
FOR-LEVEL's own 10-attempt collision retries.")

(defun generate-dungeon (tier level &key (width *rdescent-field-width*)
                                      (height *rdescent-field-height*)
                                      (max-rooms 30) (min-size 6) (max-size 10))
  "Procedurally generate (or return the memoized) dungeon for TIER (a
string) and LEVEL (a positive integer), returned as (VALUES GAME-MAP
ROOMS LOCKED-DOOR): a WIDTH x HEIGHT grid, starting entirely as wall
TILEs (GET-WALKABLE = NIL, GET-CHAR = #\\#), into which up to MAX-ROOMS
random rectangular rooms (each MIN-SIZE to MAX-SIZE tiles per side)
are carved via DIG-ROOM, connected in generation order to the previous
room's center via DIG-TUNNEL. ROOMS is the list of successfully placed
RECT-ROOMs, in the order they were carved (first room -- the player's
spawn room -- first), exposed so callers (see SPAWN-MONSTERS-FOR-LEVEL)
can place monsters inside real rooms rather than re-deriving them from
the raw TILE grid. LOCKED-DOOR (FUTURE_PLANS.md §9) is a single
LOCKED-DOOR struct or NIL: with probability *RDESCENT-LOCKED-DOOR-
CHANCE* (and only when at least two rooms exist), one of the tunnels'
own DIG-TUNNEL elbow tiles is converted into a locked door TILE
instead of ordinary corridor floor, gating every room from its own
SAFE-ROOM-COUNT onward -- see LOCKED-DOOR's own docstring for why no
BFS/flood-fill is needed to guarantee this. Deterministic:
*RANDOM-STATE* is rebound (dynamically, so no other thread's
randomness is disturbed) to MAKE-DETERMINISTIC-RANDOM-STATE's result
for the duration of generation, so the same (TIER, LEVEL) always
yields an EQUAL layout -- and, thanks to the *DUNGEON-CACHE* memo
table, the exact same GAME-MAP/ROOMS/LOCKED-DOOR instances on every
call after the first, as long as that entry hasn't since been evicted
(see *DUNGEON-CACHE-MAX-ENTRIES*): a cache miss for a previously-
visited (TIER, LEVEL) simply regenerates the identical (EQUAL, not EQ)
layout from scratch, so eviction only costs CPU on a rare re-visit,
never correctness.

Each room's TILEs are also stamped with a room-kind (:CUBICLE,
:OPEN-OFFICE, or :SERVER-ROOM -- see TILE's own ROOM-KIND slot),
chosen per room by CHOOSE-ROOM-KIND from a LEVEL-derived distribution
(ARCHITECTURE_PLAN.md §6): this is deterministic, immutable dungeon
geometry exactly like the tile layout itself, so it participates in
the same *RANDOM-STATE* sequence and is safe to memoize alongside it.

Each room's own physical MIN-SIZE/MAX-SIZE range is also scaled by its
own KIND via ROOM-SIZE-MULTIPLIER-FOR-KIND (FUTURE_PLANS.md §4's
physical room-generation-parameter scaling by depth, layered on top of
CHOOSE-ROOM-KIND's already-depth-biased kind selection): a :CUBICLE
room rolls smaller than the plain MIN-SIZE/MAX-SIZE keyword args, an
:OPEN-OFFICE room rolls larger, and the scaled range is always clamped
to stay comfortably inside WIDTH/HEIGHT (at most (MIN WIDTH HEIGHT) -
2 per side) so a caller passing a small WIDTH/HEIGHT (as several test
fixtures do) never risks an oversized room straddling the grid edge."
  (let* ((cache-key (cons tier level))
         (cached (or (gethash cache-key *dungeon-cache*)
                     (let ((generated
                             (let ((*random-state* (make-deterministic-random-state tier level))
                                   (grid (make-array (list height width))))
                               (dotimes (y height)
                                 (dotimes (x width)
                                   (setf (aref grid y x) (make-instance 'tile :walkable nil :char #\#))))
                               (let ((rooms nil)
                                     (tunnel-elbows nil))
                                 (dotimes (_ max-rooms)
                                   (let* ((kind (choose-room-kind level))
                                          (multiplier (room-size-multiplier-for-kind kind))
                                          (size-cap (max 3 (- (min width height) 2)))
                                          (scaled-min (max 3 (min size-cap (round (* min-size multiplier)))))
                                          (scaled-max (max scaled-min (min size-cap (round (* max-size multiplier)))))
                                          (room-width (+ scaled-min (random (1+ (- scaled-max scaled-min)))))
                                          (room-height (+ scaled-min (random (1+ (- scaled-max scaled-min)))))
                                          (x1 (random (max 1 (- width room-width 1))))
                                          (y1 (random (max 1 (- height room-height 1))))
                                          (room (make-rect-room :x1 x1 :y1 y1
                                                                 :x2 (+ x1 room-width)
                                                                 :y2 (+ y1 room-height)
                                                                 :kind kind)))
                                     (unless (some (lambda (other) (rect-room-intersects-p room other)) rooms)
                                       (dig-room grid room)
                                       (when rooms
                                         (multiple-value-bind (g elbow-x elbow-y)
                                             (dig-tunnel grid (rect-room-center-x (first rooms))
                                                         (rect-room-center-y (first rooms))
                                                         (rect-room-center-x room)
                                                         (rect-room-center-y room))
                                           (declare (ignore g))
                                           (push (cons elbow-x elbow-y) tunnel-elbows)))
                                       (push room rooms))))
                                 (setf rooms (nreverse rooms))
                                 (setf tunnel-elbows (nreverse tunnel-elbows))
                                 (let ((locked-door (place-locked-door grid rooms tunnel-elbows)))
                                   (list (make-instance 'game-map :tiles grid) rooms locked-door))))))
                       (setf (gethash cache-key *dungeon-cache*) generated)
                       (push cache-key *dungeon-cache-insertion-order*)
                       (evict-oldest-dungeon-cache-entries)
                       generated))))
    (values (first cached) (second cached) (third cached))))

(defun point-strictly-inside-room-p (x y room)
  "T if (X, Y) falls strictly inside ROOM's own walkable interior --
i.e. the same open range DIG-ROOM itself carves (excluding ROOM's own
wall border) -- NIL otherwise. Used by PLACE-LOCKED-DOOR to reject a
candidate tunnel elbow that happens to coincide with one of the two
rooms its own tunnel connects (see DIG-TUNNEL's own docstring for when
this can happen)."
  (and (< (rect-room-x1 room) x (rect-room-x2 room))
       (< (rect-room-y1 room) y (rect-room-y2 room))))

(defun place-locked-door (grid rooms tunnel-elbows)
  "Return a LOCKED-DOOR (or NIL), mutating GRID in place to carve it.
ROOMS and TUNNEL-ELBOWS are GENERATE-DUNGEON's own already-reversed
(generation-order) lists: TUNNEL-ELBOWS[J] is the DIG-TUNNEL elbow
tile of the tunnel connecting ROOMS[J] to ROOMS[J+1], so there are
always exactly (1- (LENGTH ROOMS)) elbows. Returns NIL immediately
(no door this level -- the common case) unless ROOMS has at least
three entries and a (RANDOM 1.0) roll beats *RDESCENT-LOCKED-DOOR-
CHANCE*. The candidate elbow index J is always drawn from [1, (LENGTH
TUNNEL-ELBOWS) - 1] -- i.e. never the very first tunnel (connecting
ROOMS[0], the player's own spawn room, to ROOMS[1]) -- so a door's own
SAFE-ROOM-COUNT (see below) is always >= 2, guaranteeing at least one
*non*-spawn-room safe room always exists for SPAWN-KEYS-FOR-LEVEL to
place the matching key in: every other spawn function in this file
(SPAWN-MONSTERS-FOR-LEVEL/SPAWN-ITEMS-FOR-LEVEL/SPAWN-FIXTURES-FOR-
LEVEL/SPAWN-TRAPS-FOR-LEVEL/SPAWN-DOGE-FOR-LEVEL) already never places
anything in ROOMS[0], and SPAWN-KEYS-FOR-LEVEL preserves that same
invariant by only ever considering ROOMS[1..SAFE-ROOM-COUNT - 1].
Otherwise tries up to *RDESCENT-LOCKED-DOOR-MAX-ATTEMPTS* random
elbows, skipping any that falls inside either of its own tunnel's two
rooms (POINT-STRICTLY-INSIDE-ROOM-P) -- a rare placement accident, not
a real gate, since players could just walk around it -- and gives up
(returns NIL) if every attempt collides, mirroring SPAWN-FIXTURES-FOR-
LEVEL/SPAWN-TRAPS-FOR-LEVEL's own \"try N times, then skip this rare
landmark\" tolerance. On success, overwrites GRID's chosen elbow cell
with a locked-door TILE (GET-WALKABLE NIL, GET-CHAR *RDESCENT-LOCKED-
DOOR-CHAR*, GET-LOCKED-KEY-ID/GET-LOCKED-KEY-NAME set to the one
concrete archetype presently implemented, *RDESCENT-CORPORATE-BADGE-
KEY-ID*/*RDESCENT-CORPORATE-BADGE-KEY-NAME* -- FUTURE_PLANS.md §9's
own single-archetype precedent, mirroring §8's single trap archetype),
and returns a LOCKED-DOOR whose SAFE-ROOM-COUNT is (1+ J): ROOMS[0..J]
inclusive are reachable without passing through the new door, so
SPAWN-KEYS-FOR-LEVEL may place the matching key in any of ROOMS[1..J].
Must be called (as GENERATE-DUNGEON does) inside the same dynamic
extent as that call's own *RANDOM-STATE* binding, so the door's
placement participates in the same deterministic per-(TIER, LEVEL)
sequence as the rest of generation."
  (when (and (>= (length rooms) 3) (< (random 1.0) *rdescent-locked-door-chance*))
    (loop repeat *rdescent-locked-door-max-attempts*
          thereis (let* ((j (+ 1 (random (1- (length tunnel-elbows)))))
                         (elbow (nth j tunnel-elbows))
                         (x (car elbow))
                         (y (cdr elbow)))
                    (unless (or (point-strictly-inside-room-p x y (nth j rooms))
                                (point-strictly-inside-room-p x y (nth (1+ j) rooms)))
                      (setf (aref grid y x)
                            (make-instance 'tile :walkable nil :char *rdescent-locked-door-char*
                                                  :locked-key-id *rdescent-corporate-badge-key-id*
                                                  :locked-key-name *rdescent-corporate-badge-key-name*))
                      (make-locked-door :x x :y y
                                         :key-id *rdescent-corporate-badge-key-id*
                                         :key-name *rdescent-corporate-badge-key-name*
                                         :safe-room-count (1+ j)))))))

;;; Depth-aware spawn tables
;;;
;;; SPAWN-TABLE-ENTRY/SPAWN-TABLE-CHOICE (ARCHITECTURE_PLAN.md §7)
;;; generalize what used to be separate, hardcoded weighted-choice
;;; logic duplicated (with different literal probabilities) in
;;; SPAWN-MONSTERS-FOR-LEVEL and SPAWN-ITEMS-FOR-LEVEL, into one
;;; declarative table shape and one depth-filtered weighted-choice
;;; helper both functions call. Each entry's MIN-DEPTH/MAX-DEPTH gate
;;; when it's eligible at all (the seam future bestiary tiers/rare-
;;; item depth gates/fixture types all plug into, without a bespoke
;;; COND ladder per feature); its WEIGHT is only compared against
;;; other *currently eligible* entries' weights, so adding a new deep-
;;; only entry never perturbs the relative odds among shallow-only
;;; entries. *RDESCENT-MONSTER-SPAWN-TABLE*'s two entries reproduce
;;; the exact pre-existing 80/20 Orc/Troll split at every depth --
;;; this refactor is behavior-preserving on its own; only a future
;;; table edit (adding new bestiary entries with real MIN-DEPTH/
;;; MAX-DEPTH gates) will actually change spawn composition by depth.

(defstruct spawn-table-entry
  "One row of a depth-gated weighted spawn table (ARCHITECTURE_PLAN.md
§7): KIND is a human-readable tag for logging/debugging (never
inspected by SPAWN-TABLE-CHOICE itself); MIN-DEPTH/MAX-DEPTH (inclusive)
bound the LEVELs at which this entry is eligible at all; WEIGHT is this
entry's share of the weighted draw among only the *other currently
eligible* entries at a given LEVEL (entries outside their depth range
don't affect the odds among the ones that are in range); FACTORY is a
function of (X Y LEVEL) returning a freshly constructed ENTITY. All
slots :READ-ONLY -- entries are never mutated after construction."
  (kind nil :read-only t)
  (min-depth 1 :read-only t)
  (max-depth most-positive-fixnum :read-only t)
  (weight 1 :read-only t)
  (factory nil :read-only t))

(defun spawn-table-choice (table level)
  "Return the FACTORY function of one SPAWN-TABLE-ENTRY drawn at random
from TABLE (a list of SPAWN-TABLE-ENTRYs), weighted by WEIGHT, restricted
to entries whose MIN-DEPTH <= LEVEL <= MAX-DEPTH. Assumes *RANDOM-STATE*
is already dynamically bound by the caller, so this call's randomness
participates in that same deterministic sequence. Returns NIL if no
entry in TABLE is eligible at LEVEL (callers should design tables so
this never happens for any LEVEL they actually spawn at, but NIL is
returned rather than erroring so a misconfigured table fails soft)."
  (let* ((eligible (remove-if-not (lambda (entry) (<= (spawn-table-entry-min-depth entry)
                                                       level
                                                       (spawn-table-entry-max-depth entry)))
                                   table))
         (total-weight (reduce #'+ eligible :key #'spawn-table-entry-weight :initial-value 0)))
    (when (plusp total-weight)
      (let ((roll (random total-weight)))
        (dolist (entry eligible)
          (decf roll (spawn-table-entry-weight entry))
          (when (minusp roll) (return (spawn-table-entry-factory entry))))))))

(defparameter *rdescent-monster-spawn-table*
  (list (make-spawn-table-entry :kind :code-monkey :weight 80 :factory #'make-orc)
        (make-spawn-table-entry :kind :internet-troll :weight 20 :factory #'make-troll)
        (make-spawn-table-entry :kind :middle-manager :min-depth 3 :weight 15 :factory #'make-middle-manager))
  "The monster half of ARCHITECTURE_PLAN.md §7's depth-aware spawn
table, consulted by SPAWN-MONSTERS-FOR-LEVEL via SPAWN-TABLE-CHOICE.
The original Orc/Troll entries default to MIN-DEPTH 1/MAX-DEPTH
MOST-POSITIVE-FIXNUM (eligible at every depth) with an 80/20 WEIGHT
split, exactly reproducing the pre-existing hardcoded Orc/Troll ratio;
the Middle Manager (FUTURE_PLANS.md §1's first :MANAGEMENT-faction
bestiary entry, added so HYGIENE's Suit/feral swing is actually
visible in play) is gated to MIN-DEPTH 3 so shallow levels -- and every
pre-§1 test fixed at LEVEL 1 -- keep spawning only Orcs/Trolls. This
is the seam future bestiary tiers (an Executive Washroom boss only
near a tier's MAX-DEPTH) plug into by adding new entries here, not by
editing SPAWN-MONSTERS-FOR-LEVEL's body.")

(defun spawn-count-for-level (level)
  "Return how many spawn attempts (monsters or items) a single room
should get on LEVEL, per ARCHITECTURE_PLAN.md §3/§7's graduated-
difficulty progression: 0-2 (RANDOM 3) for shallow LEVELs (1-5,
matching the original, pre-§7 hardcoded density exactly -- a Cubicle
Maze with a few easy enemies), 1-3 for mid LEVELs (6-14), and 2-4 for
the deepest LEVELs (15+, wide-open, highly-dangerous Agile
Workspaces) -- the exact 0-2 -> 1-3 -> 2-4 progression FUTURE_PLANS.md
itself proposed for graduated enemy difficulty by depth."
  (cond ((<= level 5) (random 3))
        ((<= level 14) (+ 1 (random 3)))
        (t (+ 2 (random 3)))))

(defparameter *rdescent-monster-depth-scaling-per-level* 0.04
  "Fractional MAX-HP/POWER growth SCALE-MONSTER-STATS-FOR-LEVEL applies
per LEVEL beyond 1 (FUTURE_PLANS.md §3's \"scale enemy strength with
depth\" bullet): a monster spawned on LEVEL 11 (10 levels past LEVEL
1) rolls (1 + (* 10 0.04)) = 1.4x its factory's own base MAX-HP/POWER,
before *RDESCENT-MONSTER-DEPTH-SCALING-CAP* clamps how far this can
grow. Deliberately a small, linear-in-LEVEL factor rather than an
exponential curve or per-monster-type tuning table: it nudges every
bestiary entry's baseline stats upward together as LEVEL increases
(so grinding an early floor doesn't trivially outpace the difficulty
curve, per §3's own rationale) without needing a bespoke scaling
constant for each individual monster class.")

(defparameter *rdescent-monster-depth-scaling-cap* 3.0
  "Ceiling on SCALE-MONSTER-STATS-FOR-LEVEL's own multiplier -- a
monster's MAX-HP/POWER never grows past 3x its factory's own base
values, no matter how deep LEVEL is (this TIER's LAMBDA depths run to
65536 -- see *DUNGEON-CACHE-MAX-ENTRIES*'s own docstring -- so an
uncapped linear factor would eventually produce absurd, unkillable
monsters).")

(defun monster-depth-scale-factor (level)
  "Return the multiplier SCALE-MONSTER-STATS-FOR-LEVEL applies to a
freshly spawned monster's own base MAX-HP/POWER for LEVEL: 1.0 at
LEVEL 1 (unchanged from a monster factory's own literal stats),
growing linearly by *RDESCENT-MONSTER-DEPTH-SCALING-PER-LEVEL* per
LEVEL beyond 1, clamped to never exceed *RDESCENT-MONSTER-DEPTH-
SCALING-CAP*."
  (min *rdescent-monster-depth-scaling-cap*
       (+ 1.0 (* (max 0 (1- level)) *rdescent-monster-depth-scaling-per-level*))))

(defun scale-monster-stats-for-level (monster level)
  "Return MONSTER (a freshly constructed ENEMY returned by a spawn-
table FACTORY), possibly UPDATE-ENTITY'd so its own MAX-HP/HP and
POWER are multiplied by MONSTER-DEPTH-SCALE-FACTOR for LEVEL, rounded
to the nearest integer and never allowed to fall below 1 even if a
future factory ever supplied a 0-POWER monster (FUTURE_PLANS.md §3's
\"scale enemy strength with depth\" bullet: even within one bestiary
tier, a later-LEVEL spawn of the same monster type rolls modestly
higher HP/damage than an earlier-LEVEL one). MONSTER's own HP is set
equal to its scaled MAX-HP -- every spawn-table FACTORY constructs a
monster at full health, so scaling MAX-HP without also scaling HP
would otherwise spawn a monster already missing HP it never lost. A
LEVEL 1 MONSTER-DEPTH-SCALE-FACTOR of exactly 1.0 short-circuits to
returning MONSTER unchanged (EQ), avoiding a needless UPDATE-ENTITY
copy on the common shallow-LEVEL case. XP reward is deliberately left
untouched -- see MAKE-ORC's own docstring for why XP is a fixed,
factory-literal reward, not a derived stat. Called once per monster by
SPAWN-MONSTERS-FOR-LEVEL, alongside SPAWN-TIME-DISPOSITION."
  (let ((factor (monster-depth-scale-factor level)))
    (if (= factor 1.0)
        monster
        (let ((scaled-max-hp (max 1 (round (* (max-hp monster) factor))))
              (scaled-power (max 1 (round (* (power monster) factor)))))
          (update-entity monster :max-hp scaled-max-hp :hp scaled-max-hp :power scaled-power)))))

(defun spawn-time-disposition (monster hygiene synergy)
  "Return MONSTER (a freshly constructed ENEMY returned by a spawn-
table FACTORY, still bearing whatever DISPOSITION its factory/ENEMY's
own INITIALIZE-INSTANCE :AFTER default gave it), possibly UPDATE-
ENTITY'd to a different DISPOSITION per FUTURE_PLANS.md §1's two
spawn-time rules, applied in order: first, HYGIENE-BANDED-DISPOSITION
(RDESCENT/ENTITIES.LISP) -- the player's own HYGIENE Corporate RPG
Stat swings a :HOSTILE :MANAGEMENT monster to :NEUTRAL above HYGIENE
14, or a :HOSTILE :DISGRUNTLED-DEV monster to :NEUTRAL below HYGIENE
8; then, only if MONSTER is still :HOSTILE after that (a monster
already pacified by the HYGIENE band has nothing left to pacify),
SYNERGY-PACIFY-CHANCE's independent (RANDOM 100) roll, which can
additionally turn any still-:HOSTILE monster :NEUTRAL regardless of
FACTION. Called once per monster by SPAWN-MONSTERS-FOR-LEVEL, with
HYGIENE/SYNERGY the *player's* own stats at level-generation time (a
monster's own HYGIENE/SYNERGY, always the ENTITY default, is never
consulted -- see ENTITY's docstring)."
  (let ((banded-disposition (hygiene-banded-disposition (get-faction monster) hygiene (get-disposition monster))))
    (cond ((not (eq banded-disposition (get-disposition monster)))
           (update-entity monster :disposition banded-disposition))
          ((and (eq banded-disposition :hostile)
                (< (random 100) (synergy-pacify-chance synergy)))
           (update-entity monster :disposition :neutral))
          (t monster))))

;;; Monster spawning
;;;
;;; SPAWN-MONSTERS-FOR-LEVEL is a pure function: given TIER, LEVEL, and
;;; the ROOMS list returned by GENERATE-DUNGEON, it deterministically
;;; (via MAKE-DETERMINISTIC-RANDOM-STATE, exactly like dungeon carving
;;; itself) produces a list of monster ENTITYs to seed a fresh
;;; GAME-STATE's ENTITIES slot with. The first room is always skipped,
;;; since that's where the player spawns (see MAKE-INITIAL-STATE).

(defun spawn-monsters-for-level (tier level rooms &optional (hygiene 10) (synergy 0))
  "Return a fresh list of monster ENTITYs deterministically placed
inside ROOMS (a list of RECT-ROOMs, as returned by GENERATE-DUNGEON)
for TIER/LEVEL, drawn from *RDESCENT-MONSTER-SPAWN-TABLE* via
SPAWN-TABLE-CHOICE (ARCHITECTURE_PLAN.md §7). The first room in ROOMS
is always skipped (it's where the player spawns). For each remaining
room, SPAWN-COUNT-FOR-LEVEL monsters are placed at random coordinates
strictly inside that room's bounds; a candidate cell already occupied
by a previously placed monster in this same call is skipped rather
than stacking two monsters on one tile. Every monster that does place
is passed through SPAWN-TIME-DISPOSITION along with HYGIENE/SYNERGY --
the player's own Corporate RPG Stats at the moment this level is
generated (defaulting to 10/0, the values that leave every monster's
DISPOSITION exactly as its factory set it -- see HYGIENE-BANDED-
DISPOSITION/SYNERGY-PACIFY-CHANCE) -- possibly softening an otherwise-
:HOSTILE monster to :NEUTRAL per FUTURE_PLANS.md §1 -- and through
SCALE-MONSTER-STATS-FOR-LEVEL, which multiplies its own MAX-HP/HP/
POWER by MONSTER-DEPTH-SCALE-FACTOR for LEVEL (FUTURE_PLANS.md §3's
\"scale enemy strength with depth\" bullet). Deterministic and
pure: *RANDOM-STATE* is rebound (dynamically) to MAKE-DETERMINISTIC-
RANDOM-STATE's result for the duration of this call, so the same
(TIER, LEVEL, ROOMS, HYGIENE, SYNERGY) always yields an EQUAL list of
monsters, and no external state (including *RANDOM-STATE* outside this
call's dynamic extent) is ever touched."
  (let ((*random-state* (make-deterministic-random-state tier level))
        (entities nil)
        (skipped-count 0))
    (dolist (room (rest rooms))
      (dotimes (_ (spawn-count-for-level level))
        (let* ((x (+ 1 (rect-room-x1 room) (random (max 1 (- (rect-room-x2 room) (rect-room-x1 room) 1)))))
               (y (+ 1 (rect-room-y1 room) (random (max 1 (- (rect-room-y2 room) (rect-room-y1 room) 1))))))
          (if (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y))) entities)
              (incf skipped-count)
              (push (scale-monster-stats-for-level
                     (spawn-time-disposition
                      (funcall (spawn-table-choice *rdescent-monster-spawn-table* level) x y level)
                      hygiene synergy)
                     level)
                    entities)))))
    ;; TECHNICAL_DEBT.md item #32: a coordinate collision silently
    ;; drops the roll rather than retrying with a different cell, so
    ;; the effective spawn rate can fall below the nominal 0-2-per-room
    ;; design without anyone noticing. This is a non-issue at current
    ;; density (0-2 monsters, up to 30 rooms), so we don't change the
    ;; behavior -- but log it if it ever does start happening, so a
    ;; future density increase (bigger packs, smaller rooms) doesn't
    ;; quietly under-populate levels with no visible signal.
    (when (> skipped-count 0)
      (rdescent-safe-log-warning
       "spawn-monsters-for-level: skipped ~D monster spawn(s) for tier ~A level ~D due to coordinate collisions"
       skipped-count tier level))
    (nreverse entities)))

(defparameter *rdescent-item-spawn-table*
  (list (make-spawn-table-entry :kind :stock-option :weight 5 :factory #'make-ground-stock-option)
        (make-spawn-table-entry :kind :kombucha :weight 45 :factory #'make-ground-kombucha)
        (make-spawn-table-entry :kind :pip :weight 15 :factory #'make-ground-pip)
        (make-spawn-table-entry :kind :reply-all :weight 10 :factory #'make-ground-reply-all)
        (make-spawn-table-entry :kind :reorg-memo :weight 10 :factory #'make-ground-reorg-memo)
        (make-spawn-table-entry :kind :stack-of-unread-memos :weight 4 :factory #'make-ground-stack-of-unread-memos)
        (make-spawn-table-entry :kind :red-swingline-stapler :weight 4 :factory #'make-ground-red-swingline-stapler)
        (make-spawn-table-entry :kind :three-foot-ethernet-cable :weight 3 :factory #'make-ground-three-foot-ethernet-cable)
        (make-spawn-table-entry :kind :startup-green-t-shirt :weight 3 :factory #'make-ground-startup-green-t-shirt)
        (make-spawn-table-entry :kind :blue-light-blocking-glasses :weight 2 :factory #'make-ground-blue-light-blocking-glasses)
        (make-spawn-table-entry :kind :telescoping-pointer :min-depth 2 :weight 3 :factory #'make-ground-telescoping-pointer)
        (make-spawn-table-entry :kind :patagonia-fleece-vest :min-depth 2 :weight 3 :factory #'make-ground-patagonia-fleece-vest)
        (make-spawn-table-entry :kind :aws-certified-solutions-architect-plaque :min-depth 2 :weight 2 :factory #'make-ground-aws-certified-solutions-architect-plaque)
        (make-spawn-table-entry :kind :headphones-of-noise-canceling :min-depth 2 :weight 2 :factory #'make-ground-headphones-of-noise-canceling)
        (make-spawn-table-entry :kind :whiteboard-marker-of-dominance :min-depth 3 :weight 2 :factory #'make-ground-whiteboard-marker-of-dominance)
        (make-spawn-table-entry :kind :unwashed-hoodie :min-depth 3 :weight 2 :factory #'make-ground-unwashed-hoodie)
        (make-spawn-table-entry :kind :branded-corporate-yeti-mug :min-depth 3 :weight 2 :factory #'make-ground-branded-corporate-yeti-mug)
        (make-spawn-table-entry :kind :razor-sharp-aluminum-mousepad :min-depth 3 :weight 2 :factory #'make-ground-razor-sharp-aluminum-mousepad)
        (make-spawn-table-entry :kind :mechanical-keyboard :min-depth 4 :weight 2 :factory #'make-ground-mechanical-keyboard)
        (make-spawn-table-entry :kind :rubber-band-gatling-gun :min-depth 4 :weight 2 :factory #'make-ground-rubber-band-gatling-gun)
        (make-spawn-table-entry :kind :laser-pointer-of-redirection :min-depth 4 :weight 2 :factory #'make-ground-laser-pointer-of-redirection)
        (make-spawn-table-entry :kind :nerf-retaliator :min-depth 4 :weight 2 :factory #'make-ground-nerf-retaliator)
        (make-spawn-table-entry :kind :ironed-button-down :min-depth 4 :weight 2 :factory #'make-ground-ironed-button-down)
        (make-spawn-table-entry :kind :yubikey-of-second-factors :min-depth 4 :weight 1 :factory #'make-ground-yubikey-of-second-factors)
        (make-spawn-table-entry :kind :agile-scrum-master-certificate :min-depth 4 :weight 2 :factory #'make-ground-agile-scrum-master-certificate)
        (make-spawn-table-entry :kind :keyboard-of-kinesis :min-depth 5 :weight 1 :factory #'make-ground-keyboard-of-kinesis)
        (make-spawn-table-entry :kind :megaphone-of-lets-take-this-offline :min-depth 5 :weight 1 :factory #'make-ground-megaphone-of-lets-take-this-offline)
        (make-spawn-table-entry :kind :can-of-compressed-air :min-depth 5 :weight 1 :factory #'make-ground-can-of-compressed-air)
        (make-spawn-table-entry :kind :usb-drive-shuriken :min-depth 5 :weight 1 :factory #'make-ground-usb-drive-shuriken)
        (make-spawn-table-entry :kind :lanyard-of-the-vip :min-depth 5 :weight 1 :factory #'make-ground-lanyard-of-the-vip)
        (make-spawn-table-entry :kind :stack-overflow-plagiarized-script :min-depth 5 :weight 1 :factory #'make-ground-stack-overflow-plagiarized-script)
        (make-spawn-table-entry :kind :severed-server-rack-rail :min-depth 6 :weight 1 :factory #'make-ground-severed-server-rack-rail)
        (make-spawn-table-entry :kind :reply-all-blunderbuss :min-depth 6 :weight 1 :factory #'make-ground-reply-all-blunderbuss)
        (make-spawn-table-entry :kind :hr-whistleblower :min-depth 7 :weight 1 :factory #'make-ground-hr-whistleblower)
        ;; §17 The Corporate Pharmacy -- Tier 1 (everyday grub, depth 1+)
        (make-spawn-table-entry :kind :stale-croissant :weight 8 :factory #'make-ground-stale-croissant)
        (make-spawn-table-entry :kind :day-old-breakroom-pizza :weight 6 :factory #'make-ground-day-old-breakroom-pizza)
        (make-spawn-table-entry :kind :someone-elses-tupperware-lunch :weight 5 :factory #'make-ground-someone-elses-tupperware-lunch)
        (make-spawn-table-entry :kind :happy-birthday-sheet-cake :weight 2 :factory #'make-ground-happy-birthday-sheet-cake)
        (make-spawn-table-entry :kind :handful-of-free-office-almonds :weight 10 :factory #'make-ground-handful-of-free-office-almonds)
        ;; §17 Tier 2 -- the caffeine aisle (depth 2+)
        (make-spawn-table-entry :kind :tgif-leftover-beer :min-depth 2 :weight 5 :factory #'make-ground-tgif-leftover-beer)
        (make-spawn-table-entry :kind :breakroom-coffee-burnt :min-depth 2 :weight 6 :factory #'make-ground-breakroom-coffee-burnt)
        (make-spawn-table-entry :kind :artisan-latte :min-depth 2 :weight 4 :factory #'make-ground-artisan-latte)
        (make-spawn-table-entry :kind :quadruple-shot-espresso :min-depth 2 :weight 3 :factory #'make-ground-quadruple-shot-espresso)
        (make-spawn-table-entry :kind :warm-monster-energy-drink :min-depth 2 :weight 2 :factory #'make-ground-warm-monster-energy-drink)
        (make-spawn-table-entry :kind :the-smart-water :min-depth 2 :weight 2 :factory #'make-ground-the-smart-water)
        ;; §17 Tier 3 -- the hard stuff (depth 3+, deliberately rare)
        (make-spawn-table-entry :kind :discarded-adderall :min-depth 3 :weight 2 :factory #'make-ground-discarded-adderall)
        (make-spawn-table-entry :kind :modafinil :min-depth 3 :weight 1 :factory #'make-ground-modafinil)
        (make-spawn-table-entry :kind :dexedrine-spansule :min-depth 3 :weight 1 :factory #'make-ground-dexedrine-spansule)
        (make-spawn-table-entry :kind :baggie-of-blow-executive-grade :min-depth 4 :weight 1 :factory #'make-ground-baggie-of-blow-executive-grade)
        (make-spawn-table-entry :kind :microdose-tab-lsd :min-depth 3 :weight 1 :factory #'make-ground-microdose-tab-lsd)
        (make-spawn-table-entry :kind :unmarked-nootropic-stack :min-depth 3 :weight 2 :factory #'make-ground-unmarked-nootropic-stack)
        ;; §15 Rare & Legendary Loot -- Rare tier (depth 6+, weight 1:
        ;; the deepest-and-rarest ground finds, matching the HR
        ;; Whistleblower's own precedent one row above)
        (make-spawn-table-entry :kind :out-of-office-auto-responder :min-depth 6 :weight 1 :factory #'make-ground-out-of-office-auto-responder)
        (make-spawn-table-entry :kind :root-password-post-it-note :min-depth 6 :weight 1 :factory #'make-ground-root-password-post-it-note)
        (make-spawn-table-entry :kind :airpods-pro-noise-canceling :min-depth 6 :weight 1 :factory #'make-ground-airpods-pro-noise-canceling)
        (make-spawn-table-entry :kind :platinum-corporate-amex :min-depth 6 :weight 1 :factory #'make-ground-platinum-corporate-amex)
        (make-spawn-table-entry :kind :pager-of-dread :min-depth 6 :weight 1 :factory #'make-ground-pager-of-dread)
        ;; §15 Legendary tier (depth 9+, weight 1: rarer still)
        (make-spawn-table-entry :kind :b0fhs-lart :min-depth 9 :weight 1 :factory #'make-ground-b0fhs-lart)
        (make-spawn-table-entry :kind :source-code-of-the-universe :min-depth 9 :weight 1 :factory #'make-ground-source-code-of-the-universe)
        (make-spawn-table-entry :kind :c-suite-keycard :min-depth 9 :weight 1 :factory #'make-ground-c-suite-keycard)
        (make-spawn-table-entry :kind :golden-parachute :min-depth 9 :weight 1 :factory #'make-ground-golden-parachute)
        (make-spawn-table-entry :kind :mechanical-keyboard-of-the-ancients :min-depth 9 :weight 1 :factory #'make-ground-mechanical-keyboard-of-the-ancients))
  "The ground-item half of ARCHITECTURE_PLAN.md §7's depth-aware spawn
table, consulted by SPAWN-ITEMS-FOR-LEVEL via SPAWN-TABLE-CHOICE. All
five consumable/RSU entries remain available from depth 1 onward,
while §13's concrete armory content plugs into the same table as a
second wave of low-weight, depth-gated GROUND-ITEM factories rather
than a separate loot subsystem. The weights deliberately keep
consumables common and make the stronger artifact-tier weapons (e.g.
The HR Whistleblower) much rarer finds than early-slot filler gear.
§17's Corporate Pharmacy catalog plugs into this same table as a third
wave: Tier 1 (everyday grub) is available from depth 1 like Kombucha,
Tier 2 (the caffeine aisle) is gated to depth 2+, and Tier 3 (the hard
stuff) is gated to depth 3+ (Baggie of Blow, the single strongest
buff+heal+energy combo of the three, to depth 4+) and given
deliberately low weights, matching the plan text's own framing of
Tier 3 as rare, high-risk finds rather than everyday consumables.
§15's ten unique Rare/Legendary items (FUTURE_PLANS.md §15) plug into
this same table as a fourth and final wave, at :MIN-DEPTH 6/:WEIGHT 1
(Rare) and :MIN-DEPTH 9/:WEIGHT 1 (Legendary) -- the deepest, rarest
entries in the whole table -- with duplicate-prevention handled
entirely outside this table (see FILTER-OUT-OWNED-UNIQUE-ITEMS,
consulted by USE-STAIRS's own fresh-floor-generation call to this
function) rather than by this table's own WEIGHT/depth gating.")

(defun spawn-items-for-level (tier level rooms existing-entities)
  "Return a fresh list of loot ENTITYs deterministically placed inside
ROOMS (a list of RECT-ROOMs, as returned by GENERATE-DUNGEON) for
TIER/LEVEL, drawn from *RDESCENT-ITEM-SPAWN-TABLE* via
SPAWN-TABLE-CHOICE (ARCHITECTURE_PLAN.md §7), given EXISTING-ENTITIES
(the monsters and staircases already spawned for this same level, via
SPAWN-MONSTERS-FOR-LEVEL/SPAWN-STAIRS-FOR-LEVEL -- see MAKE-INITIAL-
STATE/USE-STAIRS, which build this level's full ENTITIES list by
appending this function's own result after both). The first room in
ROOMS is always skipped (it's where the player spawns -- see
MAKE-INITIAL-STATE). For each remaining room, 0 to 2 items are placed
at random coordinates strictly inside that room's bounds; a candidate
cell already occupied by a previously placed item in this same call,
or by any entity in EXISTING-ENTITIES, is skipped rather than stacking
loot on a monster or staircase. Deterministic and pure: *RANDOM-STATE*
is rebound (dynamically) to MAKE-DETERMINISTIC-RANDOM-STATE's result
for the duration of this call -- the same seeding SPAWN-MONSTERS-FOR-
LEVEL/SPAWN-STAIRS-FOR-LEVEL each use, in their own independent
dynamic extent -- so the same (TIER, LEVEL, ROOMS, EXISTING-ENTITIES)
always yields an EQUAL list of items, and no external state (including
*RANDOM-STATE* outside this call's dynamic extent) is ever touched.
Every spawned EQUIPPABLE-ITEM ground find additionally has its own
ITEM-MODIFIER re-rolled via RANDOMIZE-EQUIPPABLE-ITEM-MODIFIER (see
ENTITIES.LISP), consuming further draws from this same *RANDOM-STATE*
extent -- so a Stack of Unread Memos found on the ground may turn out
:CURSED or :BLESSED rather than :NORMAL, deterministically, exactly
like everything else this function produces."
  (let ((*random-state* (make-deterministic-random-state tier level))
        (entities nil))
    (dolist (room (rest rooms))
      (dotimes (_ (random 3))
        (let* ((x (+ 1 (rect-room-x1 room) (random (max 1 (- (rect-room-x2 room) (rect-room-x1 room) 1)))))
               (y (+ 1 (rect-room-y1 room) (random (max 1 (- (rect-room-y2 room) (rect-room-y1 room) 1))))))
          (unless (or (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y))) entities)
                      (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y))) existing-entities))
            (let ((ground (funcall (spawn-table-choice *rdescent-item-spawn-table* level) x y level)))
              (push (if (typep (get-payload ground) 'equippable-item)
                        (update-entity ground :payload (randomize-newly-spawned-equippable-item (get-payload ground)))
                        ground)
                    entities))))))
    (nreverse entities)))

(defparameter *rdescent-unique-item-classes*
  '(out-of-office-auto-responder root-password-post-it-note airpods-pro-noise-canceling
    platinum-corporate-amex pager-of-dread b0fhs-lart source-code-of-the-universe
    c-suite-keycard golden-parachute mechanical-keyboard-of-the-ancients)
  "The ten unique FUTURE_PLANS.md §15 item classes FILTER-OUT-OWNED-
UNIQUE-ITEMS enforces at most one live copy of per GAME-STATE (across
INVENTORY, EQUIPMENT, and every still-on-the-ground GROUND-ITEM on any
level, current or not).")

(defun unique-item-p (payload)
  "T if PAYLOAD (an RDESCENT-ITEM) is an instance of one of the ten
§15 *RDESCENT-UNIQUE-ITEM-CLASSES*, NIL otherwise."
  (some (lambda (class) (typep payload class)) *rdescent-unique-item-classes*))

(defun state-owned-unique-item-classes (state)
  "Return a list (possibly with duplicates -- callers only ever use
MEMBER against it) of the CLASS-OF every §15 unique item already
present anywhere in STATE: the player's own INVENTORY and EQUIPMENT
slots, every GROUND-ITEM currently in (GET-ENTITIES STATE) (the
current level), and every GROUND-ITEM stashed inside any other level's
own DUNGEON-LEVEL-SNAPSHOT-ENTITIES (GET-LEVELS) -- so a unique item
left behind on an earlier, revisited-later floor still counts as
\"owned\" and is never re-rolled a second time on some other floor."
  (let ((player (get-player state))
        (classes nil))
    (dolist (item (get-inventory player))
      (when (unique-item-p item) (push (class-of item) classes)))
    (loop for (nil item) on (get-equipment player) by #'cddr
          when (and item (unique-item-p item))
            do (push (class-of item) classes))
    (dolist (ent (get-entities state))
      (when (and (typep ent 'ground-item) (unique-item-p (get-payload ent)))
        (push (class-of (get-payload ent)) classes)))
    (fset:do-map (depth snapshot (get-levels state))
      (declare (ignore depth))
      (dolist (ent (dungeon-level-snapshot-entities snapshot))
        (when (and (typep ent 'ground-item) (unique-item-p (get-payload ent)))
          (push (class-of (get-payload ent)) classes))))
    classes))

(defun filter-out-owned-unique-items (state entities)
  "Return a fresh list containing every element of ENTITIES (as
returned by SPAWN-ITEMS-FOR-LEVEL) except any GROUND-ITEM whose own
PAYLOAD is a §15 unique item (UNIQUE-ITEM-P) whose CLASS-OF is already
present anywhere in STATE (see STATE-OWNED-UNIQUE-ITEM-CLASSES) --
enforcing \"never two copies of the same unique item in the same
GAME-STATE\" purely at ground-spawn time, without touching monster-
kill drops at all (see this file's own §15 preamble comment,
ENTITIES.LISP). Called by USE-STAIRS's (RDESCENT/ACTIONS.LISP)
fresh-floor-generation branch, immediately after SPAWN-ITEMS-FOR-LEVEL
returns -- MAKE-INITIAL-STATE's own call to SPAWN-ITEMS-FOR-LEVEL
(RDESCENT/MECHANICS.LISP) needs no equivalent filtering, since a
freshly created player/GAME-STATE can never already own any item at
all. A freshly spawned unique item is *never* filtered out by an
identical sibling spawned in this exact same SPAWN-ITEMS-FOR-LEVEL
call (STATE itself, not ENTITIES, is what's checked against) -- in the
vanishingly rare case two entries of the *same* unique class both roll
in one call, both would still spawn; this is deemed acceptable since
each entry's own WEIGHT 1 make that a near-impossible coincidence in
practice, and guarding against it would require single-item
class-uniqueness bookkeeping *within* SPAWN-ITEMS-FOR-LEVEL itself,
not just across calls."
  (let ((owned (state-owned-unique-item-classes state)))
    (remove-if (lambda (ent)
                 (and (typep ent 'ground-item)
                      (unique-item-p (get-payload ent))
                      (member (class-of (get-payload ent)) owned)))
               entities)))

(defparameter *rdescent-monster-drop-equippable-spawn-table*
  (remove-if-not (lambda (entry) (typep (get-payload (funcall (spawn-table-entry-factory entry) 0 0 1)) 'equippable-item))
                 *rdescent-item-spawn-table*)
  "The subset of *RDESCENT-ITEM-SPAWN-TABLE* whose FACTORY produces an
EQUIPPABLE-ITEM payload (i.e. every armory weapon/armor/accessory
entry, excluding the plain consumables/RSU rows like Kombucha or Stock
Option) -- classified once at load time by probing each entry's own
FACTORY with dummy (0, 0, 1) coordinates. Consulted by MAYBE-DROP-
MONSTER-ITEM (via SPAWN-TABLE-CHOICE) so a monster's rare item drop
only ever produces genuine gear, never a redundant second consumable.")

(defparameter *rdescent-monster-item-drop-chance-percent* 20
  "Percent chance (out of a further (RANDOM 100) roll, only consulted
once MAYBE-DROP-MONSTER-LOOT's own outer *RDESCENT-MONSTER-RSU-DROP-
CHANCE-PERCENT* roll already succeeded) that a slain monster's loot
drop is an actual equippable item (MAYBE-DROP-MONSTER-ITEM) rather
than the more common Severance Package RSU windfall (MAKE-GROUND-
SEVERANCE-PACKAGE) -- kept small so finding real gear off a monster
corpse remains a rarer, more memorable event than the ordinary RSU
drop it's layered on top of.")

(defparameter *rdescent-monster-item-drop-xp-per-depth* 3
  "Divisor MAYBE-DROP-MONSTER-ITEM applies to a slain monster's own
GET-XP (this engine's existing per-monster difficulty proxy) to derive
the maximum spawn-table MIN-DEPTH its item drop's SPAWN-TABLE-CHOICE
draw is capped at -- so a tougher monster (higher XP) unlocks
progressively pricier/rarer gear tiers in the pool its drop is drawn
from, exactly mirroring MONSTER-RSU-DROP-AMOUNT's own XP-scaling for
the ordinary RSU case, just expressed as pool access rather than a
raw multiplier.")

(defun maybe-drop-monster-item (target)
  "Return a fresh GROUND-ITEM entity at TARGET's own X/Y/LEVEL, drawn
from *RDESCENT-MONSTER-DROP-EQUIPPABLE-SPAWN-TABLE* via SPAWN-TABLE-
CHOICE restricted to entries whose MIN-DEPTH is at most (FLOOR (GET-XP
TARGET) *RDESCENT-MONSTER-ITEM-DROP-XP-PER-DEPTH*) -- so a tougher
TARGET's item drop draws from a pool that includes progressively
better gear -- with its own EQUIPPABLE-ITEM payload's MODIFIER/
DURABILITY freshly rolled via RANDOMIZE-NEWLY-SPAWNED-EQUIPPABLE-ITEM,
exactly as SPAWN-ITEMS-FOR-LEVEL rolls an ordinary floor find. Returns
NIL if no entry in the table is eligible yet at that depth cap (e.g. a
monster too weak, by XP, to unlock even the cheapest armory entry)."
  (let* ((depth-cap (max 1 (floor (get-xp target) *rdescent-monster-item-drop-xp-per-depth*)))
         (factory (spawn-table-choice *rdescent-monster-drop-equippable-spawn-table* depth-cap)))
    (when factory
      (let ((ground (funcall factory (get-x target) (get-y target) (get-level target))))
        (update-entity ground :payload (randomize-newly-spawned-equippable-item (get-payload ground)))))))

(defun maybe-drop-monster-loot (target)
  "Top-level monster-death loot roll -- the function every real
\"a monster just died\" call site (MOVE-PLAYER's melee kill, APPLY-
ITEM's Scroll of PIP kill, CONFUSED-ENTITY-TURN's stumble-kill,
COMPANION-AI-TURN's own attack) should call. Return a fresh GROUND-ITEM
entity dropped at TARGET's own X/Y/LEVEL, or NIL (the common case,
since the outer drop chance is deliberately small). First rolls
against *RDESCENT-MONSTER-RSU-DROP-CHANCE-PERCENT*; if that succeeds,
a further roll against *RDESCENT-MONSTER-ITEM-DROP-CHANCE-PERCENT*
decides whether the drop is an actual equippable item (MAYBE-DROP-
MONSTER-ITEM -- itself capable of returning NIL if no armory entry is
eligible yet at TARGET's own XP, in which case this falls through to
the RSU case below rather than dropping nothing) or the more common
Severance Package RSU windfall (MAKE-GROUND-SEVERANCE-PACKAGE, sized
via MONSTER-RSU-DROP-AMOUNT) -- both proportional to TARGET's own
GET-XP, this engine's existing per-monster difficulty proxy."
  (when (< (random 100) *rdescent-monster-rsu-drop-chance-percent*)
    (or (and (< (random 100) *rdescent-monster-item-drop-chance-percent*)
             (maybe-drop-monster-item target))
        (make-ground-severance-package (get-x target) (get-y target) (get-level target)
                                       (monster-rsu-drop-amount (get-xp target))))))

;;; Fixture spawning
;;;
;;; SPAWN-FIXTURES-FOR-LEVEL is a pure function, parallel in shape to
;;; SPAWN-MONSTERS-FOR-LEVEL/SPAWN-ITEMS-FOR-LEVEL above but placing at
;;; most a single FIXTURE for the whole level (rather than 0-2 per
;;; room) -- ARCHITECTURE_PLAN.md §3/§7: fixtures are meant to be a
;;; rare, memorable landmark (FUTURE_PLANS.md §12's own "free-standing"
;;; framing), not something that clutters every room the way loot/
;;; monsters do.

(defparameter *rdescent-fixture-spawn-chance* 0.2
  "Probability (out of 1.0) that SPAWN-FIXTURES-FOR-LEVEL places any
FIXTURE at all for a given (TIER, LEVEL) -- see that function. Kept
low and depth-independent for now (no ARCHITECTURE_PLAN.md/FUTURE_
PLANS.md text calls for depth-scaling shrine/vendor frequency the way
monster density scales -- see SPAWN-COUNT-FOR-LEVEL) so at most one
shrine or vendor exists on roughly 1 in 5 floors, a rare enough
landmark not to trivialize resource management (FUTURE_PLANS.md §12's
own stated concern).")

(defparameter *rdescent-fixture-spawn-table*
  (list (make-spawn-table-entry :kind :espresso :weight 1 :factory #'make-espresso-machine)
        (make-spawn-table-entry :kind :kombucha-bar :weight 1 :factory #'make-kombucha-bar)
        (make-spawn-table-entry :kind :water-cooler :weight 1 :factory #'make-water-cooler)
        (make-spawn-table-entry :kind :vending-machine :weight 1 :factory #'make-vending-machine)
        (make-spawn-table-entry :kind :disgruntled-it-guy :weight 1 :factory #'make-disgruntled-it-guy))
  "The fixture half of ARCHITECTURE_PLAN.md §7's depth-aware spawn
table, consulted by SPAWN-FIXTURES-FOR-LEVEL via SPAWN-TABLE-CHOICE.
All five fixture kinds (three free SHRINE-FIXTURE kinds, FUTURE_
PLANS.md §12; one paid VENDOR-FIXTURE kind, FUTURE_PLANS.md §10; and
one NPC-FIXTURE kind, the Disgruntled IT Guy, FUTURE_PLANS.md §11) are
equally likely (weight 1 apiece) and eligible at every depth -- sharing
one table/spawn-chance with the shrines rather than a second
independent one (unlike SPAWN-TRAPS-FOR-LEVEL/SPAWN-KEYS-FOR-LEVEL's
own deliberately separate tables) is intentional here: an NPC-FIXTURE
is exactly as rare a landmark as a shrine or vendor, so folding it into
the same \"at most one FIXTURE per level\" draw keeps that existing
invariant (and its own regression tests) true without any change to
SPAWN-FIXTURES-FOR-LEVEL itself -- only this table grew, exactly as
VENDOR-FIXTURE's own addition (§10) already did.")

(defun spawn-fixtures-for-level (tier level rooms existing-entities)
  "Return a fresh list of at most one FIXTURE ENTITY, deterministically
placed inside ROOMS (a list of RECT-ROOMs, as returned by GENERATE-
DUNGEON) for TIER/LEVEL, given EXISTING-ENTITIES (every monster/item/
staircase already spawned for this same level -- see MAKE-INITIAL-
STATE/USE-STAIRS, which build this level's full ENTITIES list by
appending this function's own result last). With probability
(1 - *RDESCENT-FIXTURE-SPAWN-CHANCE*), or if ROOMS has fewer than 2
entries (no non-spawn room to place one in), this returns NIL: most
floors have no fixture at all. Otherwise, one of ROOMS (excluding the
first, the player's spawn room) is picked at random, and up to 10
random coordinates strictly inside that room's bounds are tried until
one doesn't collide with any entity in EXISTING-ENTITIES; if all 10
attempts collide, this returns NIL for this level rather than looping
forever or falling back to an occupied cell (a vanishingly rare
outcome at this density, unlike SPAWN-MONSTERS-FOR-LEVEL's own
per-room collision skip, so no counter/logging is warranted here). The
fixture's own kind (:ESPRESSO/:KOMBUCHA-BAR/:WATER-COOLER/:VENDING-
MACHINE/:DISGRUNTLED-IT-GUY) is drawn from *RDESCENT-FIXTURE-SPAWN-TABLE* via
SPAWN-TABLE-CHOICE.
Deterministic and pure: *RANDOM-STATE* is rebound (dynamically) to
MAKE-DETERMINISTIC-RANDOM-STATE's result for the duration of this
call -- the same seeding SPAWN-MONSTERS-FOR-LEVEL/SPAWN-ITEMS-FOR-LEVEL
each use, in their own independent dynamic extent -- so the same
(TIER, LEVEL, ROOMS, EXISTING-ENTITIES) always yields an EQUAL result,
and no external state (including *RANDOM-STATE* outside this call's
dynamic extent) is ever touched."
  (let ((*random-state* (make-deterministic-random-state tier level)))
    (if (or (< (length rooms) 2) (>= (random 1.0) *rdescent-fixture-spawn-chance*))
        nil
        (let ((room (nth (1+ (random (1- (length rooms)))) rooms)))
          (loop repeat 10
                thereis (let* ((x (+ 1 (rect-room-x1 room)
                                     (random (max 1 (- (rect-room-x2 room) (rect-room-x1 room) 1)))))
                               (y (+ 1 (rect-room-y1 room)
                                     (random (max 1 (- (rect-room-y2 room) (rect-room-y1 room) 1))))))
                          (unless (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y)))
                                            existing-entities)
                            (list (funcall (spawn-table-choice *rdescent-fixture-spawn-table* level) x y level)))))))))

;;; Plaque spawning
;;;
;;; SPAWN-PLAQUE-FOR-LEVEL places a single commemorative PLAQUE-FIXTURE
;;; (see its own docstring, RDESCENT/ENTITIES.LISP) on a dungeon TIER's
;;; own ultimate, final LEVEL -- and only that level, never any other
;;; depth. Unlike SPAWN-FIXTURES-FOR-LEVEL/SPAWN-TRAPS-FOR-LEVEL, this
;;; is not a rare, probabilistic landmark: every playthrough that
;;; actually reaches the bottom floor of its tier is guaranteed to find
;;; exactly one plaque there.

(defun spawn-plaque-for-level (tier level rooms existing-entities)
  "Return a fresh list containing at most one PLAQUE-FIXTURE ENTITY for
TIER/LEVEL, given ROOMS/EXISTING-ENTITIES exactly as SPAWN-FIXTURES-
FOR-LEVEL expects them. If LEVEL is not TIER's own ultimate, final
level (RDESCENT-TIER-MAX-DEPTH), or ROOMS has fewer than 2 entries (no
non-spawn room to place one in), this returns NIL. Otherwise, one of
ROOMS (excluding the first, the player's spawn room) is picked at
random, and up to 10 random coordinates strictly inside that room's
bounds are tried until one doesn't collide with any entity in
EXISTING-ENTITIES; if all 10 attempts collide, this returns NIL for
this level rather than looping forever or falling back to an occupied
cell, exactly like SPAWN-FIXTURES-FOR-LEVEL's own analogous fallback.
Deterministic and pure: *RANDOM-STATE* is rebound (dynamically) to
MAKE-DETERMINISTIC-RANDOM-STATE's result for the duration of this
call, in its own independent dynamic extent from SPAWN-FIXTURES-FOR-
LEVEL's -- so the same (TIER, LEVEL, ROOMS, EXISTING-ENTITIES) always
yields an EQUAL result, and no external state is ever touched."
  (let ((*random-state* (make-deterministic-random-state tier level)))
    (if (or (/= level (rdescent-tier-max-depth tier)) (< (length rooms) 2))
        nil
        (let ((room (nth (1+ (random (1- (length rooms)))) rooms)))
          (loop repeat 10
                thereis (let* ((x (+ 1 (rect-room-x1 room)
                                     (random (max 1 (- (rect-room-x2 room) (rect-room-x1 room) 1)))))
                               (y (+ 1 (rect-room-y1 room)
                                     (random (max 1 (- (rect-room-y2 room) (rect-room-y1 room) 1))))))
                          (unless (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y)))
                                            existing-entities)
                            (list (make-plaque x y level tier)))))))))

;;; Trap spawning
;;;
;;; SPAWN-TRAPS-FOR-LEVEL is a pure function, parallel in shape to
;;; SPAWN-FIXTURES-FOR-LEVEL immediately above (at most one landmark
;;; for the whole level, not per-room), but deliberately kept as its
;;; own independent spawn table/chance/function rather than folded
;;; into *RDESCENT-FIXTURE-SPAWN-TABLE* -- existing tests (and
;;; SPAWN-FIXTURES-FOR-LEVEL's own docstring) already assert a level
;;; has "at most one" SHRINE-FIXTURE; merging traps into that same
;;; table would either break that invariant or silently make shrines
;;; rarer than *RDESCENT-FIXTURE-SPAWN-CHANCE* documents. FUTURE_
;;; PLANS.md §8 ("Traps & Hidden Enemies").

(defparameter *rdescent-trap-spawn-chance* 0.2
  "Probability (out of 1.0) that SPAWN-TRAPS-FOR-LEVEL places any
TRAP-FIXTURE at all for a given (TIER, LEVEL) -- see that function.
Chosen to match *RDESCENT-FIXTURE-SPAWN-CHANCE* (also 0.2): traps are
meant to be exactly as rare a landmark as a shrine, not an every-floor
hazard, per FUTURE_PLANS.md §8's own \"rare\" framing.")

(defparameter *rdescent-trap-spawn-table*
  (list (make-spawn-table-entry :kind :broken-deployment :weight 1 :factory #'make-broken-deployment-trap))
  "The trap half of ARCHITECTURE_PLAN.md §7's depth-aware spawn table,
consulted by SPAWN-TRAPS-FOR-LEVEL via SPAWN-TABLE-CHOICE. Presently a
single entry (Broken Deployment, FUTURE_PLANS.md §8's own named
archetype) eligible at every depth -- the seam future trap archetypes
plug into by adding new entries here, exactly like *RDESCENT-FIXTURE-
SPAWN-TABLE*.")

(defun spawn-traps-for-level (tier level rooms existing-entities)
  "Return a fresh list of at most one TRAP-FIXTURE ENTITY, deterministically
placed inside ROOMS (a list of RECT-ROOMs, as returned by GENERATE-
DUNGEON) for TIER/LEVEL, given EXISTING-ENTITIES (every monster/item/
fixture/staircase already spawned for this same level -- see MAKE-
INITIAL-STATE/USE-STAIRS, which build this level's full ENTITIES list
by appending this function's own result last). With probability
(1 - *RDESCENT-TRAP-SPAWN-CHANCE*), or if ROOMS has fewer than 2
entries (no non-spawn room to place one in), this returns NIL: most
floors have no trap at all. Otherwise, one of ROOMS (excluding the
first, the player's spawn room) is picked at random, and up to 10
random coordinates strictly inside that room's bounds are tried until
one doesn't collide with any entity in EXISTING-ENTITIES; if all 10
attempts collide, this returns NIL for this level rather than looping
forever or falling back to an occupied cell -- exactly the same
collision-retry policy as SPAWN-FIXTURES-FOR-LEVEL. The trap's own
kind (only :BROKEN-DEPLOYMENT for now) is drawn from *RDESCENT-TRAP-
SPAWN-TABLE* via SPAWN-TABLE-CHOICE.
Deterministic and pure: *RANDOM-STATE* is rebound (dynamically) to
MAKE-DETERMINISTIC-RANDOM-STATE's result for the duration of this
call, in its own independent dynamic extent from SPAWN-FIXTURES-FOR-
LEVEL's own identically-seeded call, so the same (TIER, LEVEL, ROOMS,
EXISTING-ENTITIES) always yields an EQUAL result, and no external
state is ever touched."
  (let ((*random-state* (make-deterministic-random-state tier level)))
    (if (or (< (length rooms) 2) (>= (random 1.0) *rdescent-trap-spawn-chance*))
        nil
        (let ((room (nth (1+ (random (1- (length rooms)))) rooms)))
          (loop repeat 10
                thereis (let* ((x (+ 1 (rect-room-x1 room)
                                     (random (max 1 (- (rect-room-x2 room) (rect-room-x1 room) 1)))))
                               (y (+ 1 (rect-room-y1 room)
                                     (random (max 1 (- (rect-room-y2 room) (rect-room-y1 room) 1))))))
                          (unless (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y)))
                                            existing-entities)
                            (list (funcall (spawn-table-choice *rdescent-trap-spawn-table* level) x y level)))))))))

;;; Collectible spawning (FUTURE_PLANS.md §16, "Scavenger Hunt
;;; Collectibles")
;;;
;;; SPAWN-COLLECTIBLES-FOR-LEVEL is a pure function, parallel in shape
;;; to SPAWN-TRAPS-FOR-LEVEL above, but -- unlike the weighted random
;;; SPAWN-TABLE-CHOICE every other spawn-for-level function uses to
;;; pick a kind -- selects its item *deterministically*, indexing
;;; straight into *RDESCENT-COLLECTIBLE-CATALOG* by LEVEL. Since every
;;; dungeon level is generated (and re-generated on revisit, from its
;;; own cached DUNGEON-LEVEL-SNAPSHOT) independently, with no
;;; cross-level "already spawned" bookkeeping passed between calls, a
;;; random choice could hand out the same collectible on two different
;;; levels simultaneously; a 1:1 depth-to-catalog-index mapping instead
;;; guarantees no two levels ever offer the same item (until the
;;; modulo wraps back around past depth *RDESCENT-COLLECTIBLE-CATALOG*
;;; length -- MAYBE-AUTO-PICKUP-COLLECTIBLE, RDESCENT/ACTIONS.LISP,
;;; handles a duplicate/already-owned pickup as a harmless no-op rather
;;; than a crash or a double-count, so this eventual wraparound is
;;; never actually a problem).

(defparameter *rdescent-collectible-spawn-chance* 0.3
  "Probability (out of 1.0) that SPAWN-COLLECTIBLES-FOR-LEVEL places a
collectible at all for a given (TIER, LEVEL) -- see that function.
Deliberately its own independent constant, slightly higher than
*RDESCENT-TRAP-SPAWN-CHANCE*/*RDESCENT-FIXTURE-SPAWN-CHANCE* (both
0.2), rather than reusing either of them, mirroring how traps already
got their own independent spawn-chance parameter distinct from
fixtures/keys (FUTURE_PLANS.md §16's own \"a reason to explore the map\"
framing calls for collectibles to turn up a bit more often than a
shrine or a trap, without being on every single floor).")

(defun spawn-collectibles-for-level (tier level rooms existing-entities)
  "Return a fresh list of at most one AUTO-PICKUP-ITEM collectible,
deterministically placed inside ROOMS (a list of RECT-ROOMs, as
returned by GENERATE-DUNGEON) for TIER/LEVEL, given EXISTING-ENTITIES
(every monster/item/fixture/trap/key/staircase already spawned for
this same level -- see MAKE-INITIAL-STATE/USE-STAIRS, which build this
level's full ENTITIES list by appending this function's own result
last). With probability (1 - *RDESCENT-COLLECTIBLE-SPAWN-CHANCE*), or
if ROOMS has fewer than 2 entries (no non-spawn room to place one in),
this returns NIL: most floors have no collectible at all. Otherwise,
one of ROOMS (excluding the first, the player's spawn room) is picked
at random, and up to 10 random coordinates strictly inside that room's
bounds are tried until one doesn't collide with any entity in
EXISTING-ENTITIES; if all 10 attempts collide, this returns NIL for
this level rather than looping forever or falling back to an occupied
cell -- exactly the same collision-retry policy as SPAWN-TRAPS-FOR-
LEVEL/SPAWN-FIXTURES-FOR-LEVEL. Unlike those two, the collectible's
own ITEM-ID is not drawn from a weighted SPAWN-TABLE-CHOICE, but
selected deterministically via (NTH (MOD (1- LEVEL) (LENGTH
*RDESCENT-COLLECTIBLE-CATALOG*)) *RDESCENT-COLLECTIBLE-CATALOG*) --
see this function's own top-of-section commentary for why.
Deterministic and pure: *RANDOM-STATE* is rebound (dynamically) to
MAKE-DETERMINISTIC-RANDOM-STATE's result for the duration of this
call, in its own independent dynamic extent from every other spawn-
for-level function's own identically-seeded call, so the same (TIER,
LEVEL, ROOMS, EXISTING-ENTITIES) always yields an EQUAL result, and no
external state is ever touched."
  (let ((*random-state* (make-deterministic-random-state tier level)))
    (if (or (< (length rooms) 2) (>= (random 1.0) *rdescent-collectible-spawn-chance*))
        nil
        (let ((room (nth (1+ (random (1- (length rooms)))) rooms))
              (item-id (collectible-item-id
                        (nth (mod (1- level) (length *rdescent-collectible-catalog*))
                             *rdescent-collectible-catalog*))))
          (loop repeat 10
                thereis (let* ((x (+ 1 (rect-room-x1 room)
                                     (random (max 1 (- (rect-room-x2 room) (rect-room-x1 room) 1)))))
                               (y (+ 1 (rect-room-y1 room)
                                     (random (max 1 (- (rect-room-y2 room) (rect-room-y1 room) 1))))))
                          (unless (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y)))
                                            existing-entities)
                            (list (make-collectible x y level item-id))))))))) 

;;; Key spawning
;;;
;;; SPAWN-KEYS-FOR-LEVEL is a pure function, parallel in shape to
;;; SPAWN-FIXTURES-FOR-LEVEL/SPAWN-TRAPS-FOR-LEVEL above, but -- unlike
;;; those two -- never rolls its own rarity chance: whether a key is
;;; placed at all is entirely decided already by GENERATE-DUNGEON's own
;;; LOCKED-DOOR return value (via *RDESCENT-LOCKED-DOOR-CHANCE*), since
;;; a key only ever makes sense paired 1:1 with the door it opens.
;;; FUTURE_PLANS.md §9 ("Keys & Locked Doors").

(defun spawn-keys-for-level (tier level rooms locked-door existing-entities)
  "Return a fresh list of at most one GROUND-ITEM key, deterministically
placed inside ROOMS (a list of RECT-ROOMs, as returned by GENERATE-
DUNGEON) for TIER/LEVEL, given EXISTING-ENTITIES (every monster/item/
fixture/trap/staircase already spawned for this same level -- see
MAKE-INITIAL-STATE/USE-STAIRS, which build this level's full ENTITIES
list by appending this function's own result last). Returns NIL
immediately if LOCKED-DOOR is NIL (the common case -- most levels have
no locked door and therefore no key). Otherwise picks one of ROOMS[1..
LOCKED-DOOR's own SAFE-ROOM-COUNT - 1] at random -- excluding ROOMS[0]
(the player's own spawn room), exactly like SPAWN-FIXTURES-FOR-LEVEL/
SPAWN-TRAPS-FOR-LEVEL/SPAWN-MONSTERS-FOR-LEVEL/SPAWN-ITEMS-FOR-LEVEL/
SPAWN-DOGE-FOR-LEVEL all already do -- PLACE-LOCKED-DOOR's own
SAFE-ROOM-COUNT is always >= 2 precisely so this range is never empty
(see its own docstring). This is ARCHITECTURE_PLAN.md §7's own
generation-time reachability constraint, guaranteeing the key is
always reachable without ever passing through its own matching door --
and tries up to 10 random coordinates strictly inside that room's
bounds until one doesn't collide with any entity in EXISTING-ENTITIES;
if all 10 attempts collide, this returns NIL for this level rather
than looping forever or falling back to an occupied cell -- exactly
the same collision-retry policy as SPAWN-FIXTURES-FOR-LEVEL/SPAWN-
TRAPS-FOR-LEVEL (a level's own door simply goes un-keyed in this
vanishingly rare case; still gated, just harder). The key's own
KEY-ID/KEY-NAME are copied straight from LOCKED-DOOR, via MAKE-GROUND-
KEY, so it always matches the one door it was placed for.
Deterministic and pure: *RANDOM-STATE* is rebound (dynamically) to
MAKE-DETERMINISTIC-RANDOM-STATE's result for the duration of this
call, in its own independent dynamic extent from SPAWN-FIXTURES-FOR-
LEVEL/SPAWN-TRAPS-FOR-LEVEL's own identically-seeded calls, so the
same (TIER, LEVEL, ROOMS, LOCKED-DOOR, EXISTING-ENTITIES) always
yields an EQUAL result, and no external state is ever touched."
  (when locked-door
    (let ((*random-state* (make-deterministic-random-state tier level)))
      (let ((room (nth (1+ (random (1- (locked-door-safe-room-count locked-door)))) rooms)))
        (loop repeat 10
              thereis (let* ((x (+ 1 (rect-room-x1 room)
                                   (random (max 1 (- (rect-room-x2 room) (rect-room-x1 room) 1)))))
                             (y (+ 1 (rect-room-y1 room)
                                   (random (max 1 (- (rect-room-y2 room) (rect-room-y1 room) 1))))))
                        (unless (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y)))
                                          existing-entities)
                          (list (make-ground-key x y level
                                                  (locked-door-key-id locked-door)
                                                  (locked-door-key-name locked-door))))))))))


;;;
;;; SPAWN-DOGE-FOR-LEVEL is a pure function, parallel in shape to
;;; SPAWN-FIXTURES-FOR-LEVEL above -- FUTURE_PLANS.md §22, "Companion
;;; Pet: The Office Doge": a wild, not-yet-bonded COMPANION is an even
;;; rarer landmark than a FIXTURE, and (per that section's own "you
;;; will find a new one on the next level" framing) must never be
;;; spawned on a level where the player already has a bonded COMPANION
;;; following them -- see this function's own HAS-COMPANION-P
;;; parameter, threaded through by MAKE-INITIAL-STATE/USE-STAIRS.

(defparameter *rdescent-doge-spawn-chance* 0.15
  "Probability (out of 1.0) that SPAWN-DOGE-FOR-LEVEL places a wild
Office Doge COMPANION at all for a given (TIER, LEVEL), when the
player does not already have one bonded -- see that function. Kept
low, like *RDESCENT-FIXTURE-SPAWN-CHANCE*, so discovering Doge feels
like a rare, memorable event rather than a routine one.")

(defun spawn-doge-for-level (tier level rooms existing-entities has-companion-p &optional guaranteed-p)
  "Return a fresh list of at most one wild (not-yet-bonded) Office
Doge COMPANION ENTITY (see MAKE-DOGE), deterministically placed inside
ROOMS (a list of RECT-ROOMs, as returned by GENERATE-DUNGEON) for
TIER/LEVEL, given EXISTING-ENTITIES (every monster/item/fixture/
staircase already spawned for this same level) -- mirroring SPAWN-
FIXTURES-FOR-LEVEL's own shape exactly, including its \"at most 10
random-cell attempts inside a random non-spawn room, skip on
collision\" placement strategy and its own MAKE-DETERMINISTIC-RANDOM-
STATE seeding (so the same (TIER, LEVEL, ROOMS, EXISTING-ENTITIES,
HAS-COMPANION-P, GUARANTEED-P) always yields an EQUAL result). If
HAS-COMPANION-P is true (the player already has a bonded Doge
following them -- see FIND-BONDED-COMPANION), this always returns NIL
immediately, before even touching *RANDOM-STATE*: FUTURE_PLANS.md §22
is explicit that a wild Doge only ever appears \"on the next level\"
after the player's own Doge has been killed, never while they already
have one. Otherwise, if ROOMS has fewer than 2 entries, this also
returns NIL (there is nowhere sensible to place it).

GUARANTEED-P (default NIL) skips the *RDESCENT-DOGE-SPAWN-CHANCE* roll
entirely -- USE-STAIRS passes it true whenever the player is
transitioning to a freshly generated depth without an already-bonded
Doge, per §22's own \"Replacement on the next level\" framing: \"the
next use-stairs transition guarantees a fresh Doge spawn on the new
level regardless of the normal per-level spawn-chance roll.\" A brand
new game's own MAKE-INITIAL-STATE call leaves GUARANTEED-P NIL, so the
player's very first encounter with Doge remains an ordinary, rare
discovery rather than a certainty."
  (if has-companion-p
      nil
      (let ((*random-state* (make-deterministic-random-state tier level)))
        (if (or (< (length rooms) 2) (and (not guaranteed-p) (>= (random 1.0) *rdescent-doge-spawn-chance*)))
            nil
            (let ((room (nth (1+ (random (1- (length rooms)))) rooms)))
              (loop repeat 10
                    thereis (let* ((x (+ 1 (rect-room-x1 room)
                                         (random (max 1 (- (rect-room-x2 room) (rect-room-x1 room) 1)))))
                                   (y (+ 1 (rect-room-y1 room)
                                         (random (max 1 (- (rect-room-y2 room) (rect-room-y1 room) 1))))))
                              (unless (find-if (lambda (ent) (and (= (get-x ent) x) (= (get-y ent) y)))
                                                existing-entities)
                                (list (make-doge x y level)))))))))) 

;;; Field of view / fog of war
;;;
;;; COMPUTE-FOV is a pure function: MAP, PLAYER-X/Y, and RADIUS go in,
;;; a fresh WIDTH*HEIGHT bit-vector (indexed via XY-TO-INDEX) comes
;;; out, with 1 meaning "currently visible" and 0 "currently hidden".
;;; No TCOD or other external FOV library is used -- this is a
;;; self-contained implementation of Albert Ford's "symmetric
;;; shadowcasting" algorithm (https://www.albertford.com/shadowcasting/),
;;; which is mathematically exact on a square grid: unlike perimeter
;;; raycasting (which casts one line per boundary cell and can leave
;;; gaps or "pillar dancing" artifacts around corners, since a
;;; discrete line doesn't perfectly cover every cell it visually
;;; passes near), shadowcasting sweeps each of the 4 quadrants around
;;; the player row by row, splitting the row's visible column range
;;; at every wall it discovers, so every tile within RADIUS is
;;; classified as lit or shadowed with no seams and full front/back
;;; symmetry (if A can see B, B can see A).

(defstruct (fov-row (:constructor make-fov-row (depth start-slope end-slope)))
  "One row of a single quadrant's shadowcasting sweep: DEPTH is how
many tiles out (in the quadrant's own forward direction) this row is
from the origin, and START-SLOPE/END-SLOPE bound the (still narrowing)
wedge of columns in this row that remain possibly visible."
  depth start-slope end-slope)

(defun fov-round-ties-up (n)
  "Round N to the nearest integer, breaking exact .5 ties upward --
used by FOV-SCAN to compute a row's first visible column from its
START-SLOPE."
  (floor (+ n 0.5d0)))

(defun fov-round-ties-down (n)
  "Round N to the nearest integer, breaking exact .5 ties downward --
used by FOV-SCAN to compute a row's last visible column from its
END-SLOPE."
  (ceiling (- n 0.5d0)))

(defun fov-slope (depth col)
  "Return the slope of the line from the origin through the tile at
(DEPTH, COL) in a quadrant's own (depth, column) coordinates -- used
by FOV-SCAN to narrow a row's START-SLOPE/END-SLOPE at wall/floor
transitions."
  (/ (float (- (* 2 col) 1) 1.0d0) (* 2.0d0 depth)))

(defun fov-symmetric-p (depth col start-slope end-slope)
  "T if the tile at (DEPTH, COL) falls within [START-SLOPE, END-SLOPE]
at this DEPTH -- the exact test (from Ford's algorithm) that makes
shadowcasting symmetric: a floor tile only counts as visible if it is
truly within the current wedge, not merely because some ray was cast
near it."
  (and (>= col (* depth start-slope)) (<= col (* depth end-slope))))

(defun fov-quadrant-transform (cardinal origin-x origin-y depth col)
  "Return, as two values, the real map (X, Y) coordinates for the tile
at (DEPTH, COL) in the quadrant facing CARDINAL (one of :NORTH,
:SOUTH, :EAST, :WEST) rooted at (ORIGIN-X, ORIGIN-Y). Folding all 8
octants into 4 quadrants (each quadrant's COL ranges symmetrically
across both octants on either side of its cardinal axis) is what lets
FOV-SCAN cover the full 360 degrees around the player in just 4
sweeps."
  (ecase cardinal
    (:north (values (+ origin-x col) (- origin-y depth)))
    (:south (values (+ origin-x col) (+ origin-y depth)))
    (:east  (values (+ origin-x depth) (+ origin-y col)))
    (:west  (values (- origin-x depth) (+ origin-y col)))))

(defun fov-blocked-p (map x y)
  "T if (X, Y) is outside MAP's bounds or is a non-walkable tile --
FOV-SCAN's notion of an opaque tile that blocks and splits sight
lines. Bounds-checking here (rather than requiring callers to clamp
first) means a quadrant sweep can run off the edge of the map and
simply be treated as shadowed, without ever indexing outside MAP's
own TILES array."
  (let ((tile (map-tile-ref map x y)))
    (not (and tile (get-walkable tile)))))

(defun fov-scan (map origin-x origin-y cardinal radius mark-visible row)
  "Recursively scan ROW (and, as needed, every row beyond it out to
RADIUS) of the quadrant facing CARDINAL from (ORIGIN-X, ORIGIN-Y),
calling MARK-VISIBLE on every tile determined to be lit.

This is the heart of Ford's symmetric shadowcasting algorithm: ROW's
columns (bounded by its own START-SLOPE/END-SLOPE, converted to
integer bounds via FOV-ROUND-TIES-UP/DOWN) are walked left to right;
each tile is revealed if it is a wall (walls themselves are always
visible, so players can see what's blocking them) or if it passes
FOV-SYMMETRIC-P. Every wall-to-floor transition narrows this row's own
START-SLOPE going forward; every floor-to-wall transition spins off a
narrower recursive scan of the next row out (bounded by that
transition's slope), so light correctly splits around corners instead
of leaking past them. If the row ends on open floor, the sweep
continues outward at the same (unsplit) slope range. Recursion (and
this row's own tile-marking) stops once DEPTH exceeds RADIUS, or once
a candidate tile's squared Euclidean distance from the origin exceeds
RADIUS^2 -- this is what makes the lit area circular rather than a
diamond or square."
  (when (<= (fov-row-depth row) radius)
    (let* ((depth (fov-row-depth row))
           (min-col (fov-round-ties-up (* depth (fov-row-start-slope row))))
           (max-col (fov-round-ties-down (* depth (fov-row-end-slope row))))
           (prev-blocked nil)
           (prev-known nil))
      (loop for col from min-col to max-col
            do (multiple-value-bind (x y) (fov-quadrant-transform cardinal origin-x origin-y depth col)
                 (let ((blocked (fov-blocked-p map x y))
                       (dx (- x origin-x)) (dy (- y origin-y)))
                   (when (and (<= (+ (* dx dx) (* dy dy)) (* radius radius))
                              (or blocked (fov-symmetric-p depth col (fov-row-start-slope row) (fov-row-end-slope row))))
                     (funcall mark-visible x y))
                   (when prev-known
                     (cond
                       ((and prev-blocked (not blocked))
                        (setf (fov-row-start-slope row) (fov-slope depth col)))
                       ((and (not prev-blocked) blocked)
                        (fov-scan map origin-x origin-y cardinal radius mark-visible
                                  (make-fov-row (1+ depth) (fov-row-start-slope row) (fov-slope depth col))))))
                   (setf prev-blocked blocked prev-known t))))
      (when (and prev-known (not prev-blocked))
        (fov-scan map origin-x origin-y cardinal radius mark-visible
                  (make-fov-row (1+ depth) (fov-row-start-slope row) (fov-row-end-slope row)))))))

(defun compute-fov (map player-x player-y radius)
  "Return a fresh bit-vector, WIDTH*HEIGHT bits (WIDTH/HEIGHT taken
from MAP's own TILES array dimensions, indexed via XY-TO-INDEX), with
bit 1 at every cell currently visible from (PLAYER-X, PLAYER-Y) within
RADIUS tiles, 0 elsewhere.

This is exact symmetric shadowcasting (see FOV-SCAN): the player's own
tile is always marked visible, then each of the 4 quadrants around it
is swept outward row by row, splitting into narrower recursive scans
at every corner so that light never leaks past a wall and every tile
within the circular RADIUS is classified correctly, with no perimeter-
raycasting seams or missed off-axis cells.

Pure: MAP is only ever read via MAP-TILE-REF, never written to; a
brand new bit-vector is allocated and returned every call."
  (destructuring-bind (height width) (array-dimensions (get-tiles map))
    (let ((visible (make-array (* width height) :element-type 'bit :initial-element 0)))
      (flet ((mark-visible (x y)
               (when (and (<= 0 x (1- width)) (<= 0 y (1- height)))
                 (setf (bit visible (xy-to-index x y width)) 1))))
        (mark-visible player-x player-y)
        (dolist (cardinal '(:north :south :east :west))
          (fov-scan map player-x player-y cardinal radius #'mark-visible
                    (make-fov-row 1 -1.0d0 1.0d0))))
      visible)))
