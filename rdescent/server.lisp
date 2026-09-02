;;; -*- Lisp -*-

;;; Hunchensocket WebSocket backend for "Recursive Descent" (/ws/rdescent.ws). The
;;; DB-LOGGING-ACCEPTOR class (REQUEST-LOG.LISP) mixes in
;;; HUNCHENSOCKET:WEBSOCKET-ACCEPTOR so this same acceptor handles both
;;; ordinary HTTP requests and WebSocket upgrades; this file just
;;; defines the resource/client classes, registers /ws/rdescent.ws in
;;; HUNCHENSOCKET:*WEBSOCKET-DISPATCH-TABLE*, and sends the initial
;;; welcome packets that RESOURCES/WWW/HTML/JS/RDESCENT.JS expects
;;; (JSON objects shaped {"target": ..., "html": ...}), plus the
;;; server-authoritative game loop that broadcasts the rendered playing
;;; field to every connected client's own independent GAME-STATE at a
;;; fixed tick rate, with TEXT-MESSAGE-RECEIVED reduced to a thin
;;; imperative dispatcher that only parses each incoming {"action":
;;; "move", "direction": ...} packet into an RDESCENT-COMMAND via
;;; PARSE-RDESCENT-COMMAND and ENQUEUEs it onto that client's own
;;; INPUT-QUEUE, decoupled entirely from GAME-STATE mutation, which
;;; instead happens once per client per heartbeat, on the single
;;; game-loop thread, via TICK-GAME-STATE draining that queue and
;;; folding it (plus one world tick) over GAME-STATE through the pure
;;; ADVANCE-GAME-STATE reducer (RDESCENT/COMMANDS.LISP), and
;;; RDESCENT-OUTBOUND-PACKETS as the pure state->packets seam consumed
;;; by TICK-ALL-CLIENTS (the I/O shell that actually performs the
;;; per-tick GAME-STATE advancement and socket sends), and the pure
;;; ADD-RDESCENT-CLIENT / REMOVE-RDESCENT-CLIENT reducers applied by a
;;; dedicated registry-actor thread (see RDESCENT-CLIENTS-REGISTRY-LOOP)
;;; that privately owns *RDESCENT-CLIENTS*; CLIENT-CONNECTED/
;;; CLIENT-DISCONNECTED and TICK-ALL-CLIENTS only ever send it immutable
;;; messages (RDESCENT-REGISTER-CLIENT / RDESCENT-UNREGISTER-CLIENT /
;;; RDESCENT-CLIENTS-SNAPSHOT) via *RDESCENT-CLIENTS-MAILBOX* rather
;;; than touching the list directly -- the "move mutation to the
;;; edges" idea taken to its conclusion, eliminating the last lock in
;;; this file. The game loop itself (START-GAME-LOOP) is organized as
;;; an explicit "event source -> pure transform -> I/O sink" pipeline:
;;; RDESCENT-TICK-EVENTS is a SERIES event source of tick events,
;;; consumed by ITERATE, with TICK-ALL-CLIENTS (pure transform + I/O
;;; sink, as above) run once per tick.
;;;
;;; This file is the imperative I/O shell built on top of the pure
;;; functional core defined across RDESCENT/ENTITIES.LISP,
;;; RDESCENT/MECHANICS.LISP, RDESCENT/DUNGEON.LISP,
;;; RDESCENT/ACTIONS.LISP, and RDESCENT/COMMANDS.LISP (immutable value
;;; types, dungeon generation, FOV, and the state reducers); see those
;;; files' banner comments for why RDESCENT-SAFE-LOG-WARNING,
;;; *RDESCENT-DEFAULT-MEMBERSHIP-TIER*, and *RDESCENT-FOV-RADIUS* are
;;; defined there instead of here despite this file being their most
;;; natural, I/O-flavored home.

(in-package "JRM-CODE-PROJECT")

(defparameter *rdescent-welcome-message* "*** WELCOME TO RECURSIVE DESCENT ***"
  "Message centered in the playing field when a client first connects.")

(defun rdescent-center-line (text width)
  "Return TEXT centered within a WIDTH-character line, padded with
spaces on both sides. TEXT is truncated if it is longer than WIDTH."
  (let* ((text (if (> (length text) width) (subseq text 0 width) text))
         (total-padding (- width (length text)))
         (left-padding (floor total-padding 2))
         (right-padding (- total-padding left-padding)))
    (concatenate 'string
                 (make-string left-padding :initial-element #\Space)
                 text
                 (make-string right-padding :initial-element #\Space))))

(defun render-rdescent-playing-field (&key (width *rdescent-field-width*)
                                        (height *rdescent-field-height*)
                                        (message *rdescent-welcome-message*)
                                        (message-line (floor height 2)))
  "Build the initial playing-field grid: HEIGHT lines of WIDTH space
characters each, joined by newlines, with MESSAGE centered on
MESSAGE-LINE (0-indexed). Pure: no I/O, safe to unit test directly."
  (with-output-to-string (out)
    (dotimes (line height)
      (write-string (if (= line message-line)
                        (rdescent-center-line message width)
                        (make-string width :initial-element #\Space))
                    out)
      (unless (= line (1- height))
        (write-char #\Newline out)))))

(defun rdescent-message-log-packet ()
  "The JSON packet sent to a freshly-connected client to populate the
message-log div."
  (json-string `(("target" . "message-log")
                 ("html" . "Welcome to Recursive Descent"))))

(defun rdescent-playing-field-packet ()
  "The JSON packet sent to a freshly-connected client to populate the
playing-field div.
NOTE: not called anywhere in production code -- CLIENT-CONNECTED now
sends the real FOV-rendered RDESCENT-GRID-PACKET instead of this blank
placeholder (see its docstring) -- only exercised by the test suite
(tests/tests.lisp's RDESCENT-PLAYING-FIELD-PACKET-PURE)."
  (json-string `(("target" . "playing-field")
                 ("html" . ,(render-rdescent-playing-field)))))

(defun rdescent-client-raw-tier-from-request (request)
  "Return the membership tier claimed by REQUEST's JWT's :TIER claim,
or NIL if no JWT is present at all, or it is malformed/lacks a :TIER
claim -- checked in the same order as RDESCENT-PRESENTED-JWT
(RDESCENT-ROUTES.LISP): an Authorization: ****** header first, falling
back to the membership_jwt cookie. Unlike
RDESCENT-CLIENT-MEMBERSHIP-TIER-FROM-REQUEST, this deliberately does
NOT fall back to *RDESCENT-DEFAULT-MEMBERSHIP-TIER* -- it distinguishes
\"no JWT at all\" (NIL) from an explicit \"CONS\" tier claim, since
RDESCENT-TIER-MAX-DEPTH (RDESCENT/ENTITIES.LISP) treats those as the
same 8-level depth cap today but they are conceptually different
inputs, and this raw value is what MAX-DEPTH is actually derived from."
  (let* ((token (or (bearer-token-from-header-value (hunchentoot:header-in :authorization request))
                    (hunchentoot:cookie-in *jwt-cookie-name* request)))
         (claims (and token (decode-jwt token))))
    (and claims (cdr (assoc :tier claims)))))

(defun rdescent-client-membership-tier-from-request (request)
  "Return the membership tier claimed by REQUEST's JWT -- checked in
the same order as RDESCENT-PRESENTED-JWT (RDESCENT-ROUTES.LISP): an
Authorization: ****** header first, falling back to the
membership_jwt cookie -- or *RDESCENT-DEFAULT-MEMBERSHIP-TIER* if
neither is present, is malformed, or lacks a :TIER claim. Used once,
at CLIENT-CONNECTED time, to permanently associate a connecting
RDESCENT-CLIENT with a tier for the lifetime of its connection (see
RDESCENT-CLIENT's MEMBERSHIP-TIER slot)."
  (or (rdescent-client-raw-tier-from-request request)
      *rdescent-default-membership-tier*))

(defclass rdescent-client (hunchensocket:websocket-client)
  ((membership-tier :initarg :membership-tier :reader get-membership-tier
                    :initform *rdescent-default-membership-tier*
                    :documentation "This client's membership tier
(a string), extracted once from its connecting request's JWT (cookie
or Authorization header) by CLIENT-CONNECTED and never changed
afterward -- see RDESCENT-CLIENT-MEMBERSHIP-TIER-FROM-REQUEST. Used by
RENDER-GRID/GENERATE-DUNGEON to pick which tier's dungeon layout this
client's playing field is drawn from.")
   (max-depth :initarg :max-depth :reader get-max-depth
              :initform (rdescent-tier-max-depth nil)
              :documentation "The deepest dungeon LEVEL this client is
permitted to descend to, stamped once from the connecting request's
raw JWT :TIER claim (see RDESCENT-CLIENT-RAW-TIER-FROM-REQUEST and
RDESCENT-TIER-MAX-DEPTH, RDESCENT/ENTITIES.LISP) by CLIENT-CONNECTED and
never changed afterward: 8 with no JWT presented at all, 128 for
\"CONS\", 1024 for \"CADR\", 65536 for \"LAMBDA\". Deliberately kept
separate from MEMBERSHIP-TIER (which always defaults to
*RDESCENT-DEFAULT-MEMBERSHIP-TIER* for dungeon-layout purposes even
with no JWT) since MAX-DEPTH's \"no JWT\" case (8) is stricter than an
explicit \"CONS\" claim's case (also 8, coincidentally the same today,
but the two are computed from different inputs).")
   (session-id :initarg :session-id :reader get-session-id
               :initform nil
               :documentation "The *RDESCENT-SESSION-COOKIE-NAME* cookie
value presented by this client's connecting WebSocket upgrade request
(see RDESCENT-CLIENT-SESSION-ID-FROM-REQUEST), or NIL if none was
presented (e.g. cookies disabled). Stamped once by CLIENT-CONNECTED and
never changed afterward. Used purely to correlate a disconnect with a
later reconnect from the same browser, so CLIENT-DISCONNECTED can stash
GAME-STATE under it (RDESCENT-SUSPEND-GAME-STATE) and a subsequent
CLIENT-CONNECTED for a NEW RDESCENT-CLIENT instance presenting the same
cookie can resume it (RDESCENT-RESUME-GAME-STATE) instead of starting a
fresh dungeon run from MAKE-INITIAL-STATE -- see TECHNICAL_DEBT: a
dropped WebSocket connection (idle timeout, network blip, backgrounded
tab, etc.) used to always silently reset an in-progress game because
GAME-STATE previously lived only on the (now-discarded) old CLIENT
instance.")
   (game-state :initform (make-initial-state)
               :accessor get-game-state
               :documentation "This client's own, independent GAME-STATE
-- each connected player gets a separate instance so one player's
position is never shared with or overwritten by another's. Its PLAYER
slot is this client's own ENTITY, always kept in sync with its current
dungeon LEVEL by MOVE-PLAYER/APPLY-RDESCENT-COMMAND/ADVANCE-GAME-STATE.
Only ever read or written from the game-loop thread, inside
TICK-GAME-STATE -- see INPUT-QUEUE below for how commands from a
client's own WebSocket read thread reach it instead of racing with the
game loop directly.")
   (input-queue :initform (sb-concurrency:make-queue :name "rdescent-input-queue")
               :reader get-input-queue
               :documentation "A thread-safe FIFO (SB-CONCURRENCY:QUEUE)
of this client's own pending RDESCENT-COMMANDs, oldest first. Every
WebSocket message this client sends is parsed and pushed onto this
queue by TEXT-MESSAGE-RECEIVED -- which is the only method that ever
ENQUEUEs onto it, from that client's own Hunchensocket read thread --
and later drained, in order, by RDESCENT-DRAIN-INPUT-QUEUE/
TICK-GAME-STATE on the single shared game-loop thread. Decoupling
input from state mutation this way means a player mashing keys faster
than *RDESCENT-TICK-SECONDS* never contends for or races GAME-STATE:
every queued command is still applied, in order, the next time this
client's tick runs (see ADVANCE-GAME-STATE), rather than being lost,
rejected outright, or requiring GAME-STATE itself to be locked.")
   (last-sent-packets :initform nil
                       :accessor get-last-sent-packets
                       :documentation "An alist of (KEY . PACKET-STRING)
remembering the most recent JSON packet string actually sent to this
client for each of RDESCENT-OUTBOUND-PACKETS' per-tick packet kinds
(:GRID, :STATS, :INVENTORY, :MESSAGE-LOG), so RDESCENT-PACKET-IF-
CHANGED can skip resending a packet that is byte-for-byte identical to
what this client already has on screen -- e.g. an idle or merely-
standing-still player's playing-field/stats/inventory content rarely
changes tick to tick, yet TICK-ALL-CLIENTS runs 1/*RDESCENT-TICK-
SECONDS* (20) times a second, so without this cache every connected
client would otherwise receive ~20 duplicate copies of its own unread
screen every second. Like GAME-STATE, this is only ever read or
written from the single game-loop thread (inside TICK-ALL-CLIENTS via
RDESCENT-OUTBOUND-PACKETS/RDESCENT-PACKET-IF-CHANGED), so no lock is
needed around this read-then-write."))
  (:documentation "A single connected Recursive Descent player."))

(defclass rdescent-resource (hunchensocket:websocket-resource)
  ()
  (:default-initargs :client-class 'rdescent-client)
  (:documentation "The /ws/rdescent.ws WebSocket resource for Recursive Descent."))

(defvar *rdescent-resource* (make-instance 'rdescent-resource)
  "The single, shared Recursive Descent WebSocket resource.")

(defvar *rdescent-clients* nil
  "The list of currently connected RDESCENT-CLIENT instances. This is
private, single-writer state: only RDESCENT-CLIENTS-REGISTRY-LOOP (the
registry actor thread) ever reads or writes it. Every other thread
communicates with it exclusively via immutable messages sent through
*RDESCENT-CLIENTS-MAILBOX* -- see RDESCENT-REGISTER-CLIENT,
RDESCENT-UNREGISTER-CLIENT, and RDESCENT-CLIENTS-SNAPSHOT. This is the
Phase 5 stretch goal: \"move mutation to the edges\" taken to its
natural conclusion -- exactly one thread ever touches this list, so no
lock is needed at all (contrast with Phases 1-4's single-writer,
lock-free GAME-STATE, which relies on struct-slot read/write atomicity
instead of a dedicated owning thread).")

(defvar *rdescent-clients-mailbox* (sb-concurrency:make-mailbox
                                    :name "rdescent-clients-registry")
  "The registry actor's inbox. Messages are plain lists so no CLOS
class is needed: (:CONNECT client), (:DISCONNECT client), or
(:SNAPSHOT reply-mailbox). See RDESCENT-CLIENTS-REGISTRY-LOOP.")

(defvar *rdescent-clients-registry-thread* nil
  "The background thread running RDESCENT-CLIENTS-REGISTRY-LOOP, or NIL
if START-RDESCENT-CLIENTS-REGISTRY hasn't been called yet (or the
thread has since died).")

(defun add-rdescent-client (clients client)
  "Return a fresh list with CLIENT added to CLIENTS. Pure: CLIENTS
itself is never mutated. CLIENT is added at most once -- if it is
already present (by EQ), CLIENTS is returned unchanged rather than
duplicated."
  (if (member client clients)
      clients
      (cons client clients)))

(defun remove-rdescent-client (clients client)
  "Return a fresh list with CLIENT removed from CLIENTS. Pure: CLIENTS
itself is never mutated. Removing a CLIENT not present in CLIENTS is a
no-op that returns an equivalent (but freshly-consed) list."
  (remove client clients))

(defun rdescent-clients-registry-loop ()
  "Body of the registry actor thread: receive messages from
*RDESCENT-CLIENTS-MAILBOX* forever, applying the pure
ADD-RDESCENT-CLIENT / REMOVE-RDESCENT-CLIENT reducers to the privately-
owned *RDESCENT-CLIENTS* for :CONNECT/:DISCONNECT messages, or replying
with the current client list for :SNAPSHOT messages. This is the only
place in the file that reads or writes *RDESCENT-CLIENTS* directly."
  (loop
    (let ((message (sb-concurrency:receive-message *rdescent-clients-mailbox*)))
      (ecase (first message)
        (:connect (setf *rdescent-clients*
                        (add-rdescent-client *rdescent-clients* (second message))))
        (:disconnect (setf *rdescent-clients*
                           (remove-rdescent-client *rdescent-clients* (second message))))
        (:snapshot (sb-concurrency:send-message (second message) *rdescent-clients*))))))

(defun start-rdescent-clients-registry ()
  "Spawn (if not already running) the registry actor thread. Safe to
call more than once: a second call is a no-op while the actor thread
from a previous call is still alive."
  (unless (and *rdescent-clients-registry-thread*
              (bordeaux-threads:thread-alive-p *rdescent-clients-registry-thread*))
    (setf *rdescent-clients-registry-thread*
          (bordeaux-threads:make-thread #'rdescent-clients-registry-loop
                                        :name "rdescent-clients-registry")))
  *rdescent-clients-registry-thread*)

(defun rdescent-register-client (client)
  "Ask the registry actor to add CLIENT to the connected-clients list.
Fire-and-forget: does not wait for the actor to process the message."
  (sb-concurrency:send-message *rdescent-clients-mailbox* (list :connect client)))

(defun rdescent-unregister-client (client)
  "Ask the registry actor to remove CLIENT from the connected-clients
list. Fire-and-forget: does not wait for the actor to process the
message."
  (sb-concurrency:send-message *rdescent-clients-mailbox* (list :disconnect client)))

(defparameter *rdescent-clients-snapshot-timeout-seconds* 1
  "Maximum time, in seconds, RDESCENT-CLIENTS-SNAPSHOT will block
waiting for the registry actor thread to reply before giving up and
falling back to *RDESCENT-LAST-CLIENTS-SNAPSHOT* instead of hanging
forever. See TECHNICAL_DEBT.md item #35: without this, a wedged or
dead registry actor thread would freeze TICK-ALL-CLIENTS (and so every
connected client's updates) for the rest of the process's lifetime,
since nothing else in the call chain would ever notice or time out.")

(defvar *rdescent-last-clients-snapshot* nil
  "The most recent client list successfully returned by
RDESCENT-CLIENTS-SNAPSHOT. Used as the fallback return value if a
future call times out waiting for the registry actor to reply, so a
transient stall (or the registry actor thread being restarted by
RDESCENT-GAME-LOOP-WATCHDOG-LOOP) degrades to \"serve the same client
list as last tick\" rather than to no clients / no ticks at all.")

(defun rdescent-clients-snapshot ()
  "Synchronously ask the registry actor for the current list of
connected clients and return it. Used by TICK-ALL-CLIENTS in place of
the old with-lock-held + copy-list critical section. Blocks for at
most *RDESCENT-CLIENTS-SNAPSHOT-TIMEOUT-SECONDS*: if the registry
actor thread is wedged or dead and no reply arrives in time, logs a
warning and returns *RDESCENT-LAST-CLIENTS-SNAPSHOT* (the last
successfully-obtained list) instead of blocking forever, so a stuck
registry actor degrades to stale-but-live ticking rather than freezing
the whole game loop -- see TECHNICAL_DEBT.md item #35."
  (let ((reply-mailbox (sb-concurrency:make-mailbox :name "rdescent-clients-snapshot-reply")))
    (sb-concurrency:send-message *rdescent-clients-mailbox* (list :snapshot reply-mailbox))
    (multiple-value-bind (snapshot ok)
        (sb-concurrency:receive-message reply-mailbox
                                        :timeout *rdescent-clients-snapshot-timeout-seconds*)
      (if ok
          (setf *rdescent-last-clients-snapshot* snapshot)
          (progn
            (rdescent-safe-log-warning
             "rdescent-clients-snapshot timed out after ~D second(s) waiting for the registry actor; falling back to the last known client list"
             *rdescent-clients-snapshot-timeout-seconds*)
            *rdescent-last-clients-snapshot*)))))

(defun rdescent-websocket-dispatcher (request)
  "Route WebSocket upgrade requests for /ws/rdescent.ws to *RDESCENT-RESOURCE*."
  (when (string= (hunchentoot:script-name request) "/ws/rdescent.ws")
    *rdescent-resource*))

(pushnew 'rdescent-websocket-dispatcher hunchensocket:*websocket-dispatch-table*)

(defparameter *rdescent-session-cookie-name* "rdescent_session_id"
  "Name of the cookie RDESCENT-ROUTES.LISP's /rdescent.html handler
stamps on a visitor's first page load (creating it via
RDESCENT-GENERATE-SESSION-ID if absent) and that CLIENT-CONNECTED below
reads back off the WebSocket upgrade request to correlate a fresh
connection with a possibly-still-suspended GAME-STATE from a recent
prior connection by the same browser -- see RDESCENT-SUSPEND-GAME-
STATE/RDESCENT-RESUME-GAME-STATE. Not security-sensitive (unlike
*JWT-COOKIE-NAME*): it never grants any privilege, only lets a
reconnecting client resume its own dungeon run instead of one always
starting over from scratch.")

(defun rdescent-generate-session-id ()
  "Return a fresh, opaque session identifier: 32 lowercase hex digits
assembled from four (RANDOM (EXPT 2 32)) fixnums. Not cryptographically
sensitive -- see *RDESCENT-SESSION-COOKIE-NAME*'s docstring -- so
ordinary RANDOM is fine here, unlike JWT.LISP's signing key material."
  (format nil "~(~8,'0x~8,'0x~8,'0x~8,'0x~)"
          (random (expt 2 32)) (random (expt 2 32))
          (random (expt 2 32)) (random (expt 2 32))))

(defun rdescent-client-session-id-from-request (request)
  "Return the *RDESCENT-SESSION-COOKIE-NAME* cookie value from REQUEST
(the connecting WebSocket upgrade request, as captured by HUNCHENSOCKET:
CLIENT-REQUEST), or NIL if none was presented -- e.g. a client with
cookies disabled, or one that reached /ws/rdescent.ws without ever
having loaded /rdescent.html first (which is what normally stamps this
cookie; see RDESCENT-ROUTES.LISP). NIL simply disables state
suspension/resumption for that connection: CLIENT-CONNECTED falls back
to an ordinary MAKE-INITIAL-STATE, exactly as it did before this cookie
existed. REQUEST itself may also be NIL (e.g. a test double with no
real HTTP request behind it at all), in which case this simply returns
NIL rather than erroring."
  (and request (hunchentoot:cookie-in *rdescent-session-cookie-name* request)))

(defvar *rdescent-suspended-states-lock*
  (bordeaux-threads:make-lock "rdescent-suspended-states")
  "Guards all reads/writes of *RDESCENT-SUSPENDED-STATES*. Unlike
*RDESCENT-CLIENTS* (moved to a lock-free single-owner-thread actor
because TICK-ALL-CLIENTS reads it every tick), suspended-state
lookups/inserts only happen at CLIENT-CONNECTED/CLIENT-DISCONNECTED
time -- rare relative to the tick rate -- so a plain lock is simpler
and sufficient here.")

(defvar *rdescent-suspended-states* (make-hash-table :test 'equal)
  "Session-id (string, see *RDESCENT-SESSION-COOKIE-NAME*) -> (CONS
game-state disconnect-universal-time), for clients that disconnected
recently enough that RDESCENT-RESUME-GAME-STATE might still hand their
GAME-STATE back to a reconnecting browser. Entries are removed as soon
as they are resumed (at most once each) or once
RDESCENT-EVICT-EXPIRED-SUSPENDED-STATES (called periodically by
RDESCENT-GAME-LOOP-WATCHDOG-LOOP) notices they are older than
*RDESCENT-SUSPEND-GRACE-SECONDS* -- so, like *DUNGEON-CACHE*
(TECHNICAL_DEBT.md item #33), this table can never grow unboundedly
from abandoned sessions that never reconnect.")

(defparameter *rdescent-suspend-grace-seconds* 300
  "How long, in seconds, a disconnected client's GAME-STATE remains in
*RDESCENT-SUSPENDED-STATES* and eligible for RDESCENT-RESUME-GAME-STATE
to hand back to a reconnecting browser sharing the same
*RDESCENT-SESSION-COOKIE-NAME* cookie. A reconnect within this window
resumes exactly where the player left off; a reconnect after it (or a
browser with no session cookie at all) instead gets a brand-new
MAKE-INITIAL-STATE run, same as before this feature existed.")

(defun rdescent-suspend-game-state (session-id game-state)
  "Stash GAME-STATE in *RDESCENT-SUSPENDED-STATES* under SESSION-ID,
timestamped now, so a reconnecting browser presenting the same
*RDESCENT-SESSION-COOKIE-NAME* cookie within *RDESCENT-SUSPEND-GRACE-
SECONDS* can resume it via RDESCENT-RESUME-GAME-STATE. A no-op if
SESSION-ID is NIL (e.g. a client with cookies disabled)."
  (when session-id
    (bordeaux-threads:with-lock-held (*rdescent-suspended-states-lock*)
      (setf (gethash session-id *rdescent-suspended-states*)
            (cons game-state (get-universal-time))))))

(defun rdescent-resume-game-state (session-id)
  "If SESSION-ID (possibly NIL) has a still-fresh suspended GAME-STATE
in *RDESCENT-SUSPENDED-STATES* (stashed at most *RDESCENT-SUSPEND-
GRACE-SECONDS* ago), remove and return it; otherwise return NIL. Always
removes any entry found (fresh or stale) so a given suspended state is
only ever resumed once, never handed to two different reconnecting
clients."
  (when session-id
    (bordeaux-threads:with-lock-held (*rdescent-suspended-states-lock*)
      (let ((entry (gethash session-id *rdescent-suspended-states*)))
        (when entry
          (remhash session-id *rdescent-suspended-states*)
          (when (<= (- (get-universal-time) (cdr entry))
                    *rdescent-suspend-grace-seconds*)
            (car entry)))))))

(defun rdescent-evict-expired-suspended-states ()
  "Remove every entry from *RDESCENT-SUSPENDED-STATES* whose
disconnect-universal-time is older than *RDESCENT-SUSPEND-GRACE-
SECONDS*, so a session that disconnects and never reconnects doesn't
linger in the table forever. Called periodically by RDESCENT-GAME-LOOP-
WATCHDOG-LOOP, alongside its existing dead-thread checks. Safe to call
concurrently with RDESCENT-SUSPEND-GAME-STATE/RDESCENT-RESUME-GAME-
STATE: REMHASH-ing the current key mid-MAPHASH is explicitly permitted
by the CLHS."
  (bordeaux-threads:with-lock-held (*rdescent-suspended-states-lock*)
    (let ((cutoff (- (get-universal-time) *rdescent-suspend-grace-seconds*)))
      (maphash (lambda (session-id entry)
                 (when (< (cdr entry) cutoff)
                   (remhash session-id *rdescent-suspended-states*)))
               *rdescent-suspended-states*))))

(defmethod hunchensocket:client-connected ((resource rdescent-resource)
                                           (client rdescent-client))
  "Register CLIENT with the registry actor and send it the initial
message-log and playing-field packets. Registration is now a message
send (RDESCENT-REGISTER-CLIENT) rather than a locked read-modify-write
-- the ADD-RDESCENT-CLIENT reducer it invokes is unchanged, only the
actor owns applying it.

Also stamps CLIENT's MEMBERSHIP-TIER and MAX-DEPTH slots from the
connecting request's JWT (see RDESCENT-CLIENT-MEMBERSHIP-TIER-FROM-
REQUEST/RDESCENT-CLIENT-RAW-TIER-FROM-REQUEST/RDESCENT-TIER-MAX-DEPTH),
using HUNCHENSOCKET:CLIENT-REQUEST -- the original HTTP upgrade
request, permanently retained by Hunchensocket -- rather than
HUNCHENTOOT:*REQUEST*, which is not guaranteed to still be dynamically
bound here."
  (setf (slot-value client 'membership-tier)
        (rdescent-client-membership-tier-from-request
         (hunchensocket:client-request client)))
  (setf (slot-value client 'max-depth)
        (rdescent-tier-max-depth
         (rdescent-client-raw-tier-from-request (hunchensocket:client-request client))))
  (setf (slot-value client 'session-id)
        (rdescent-client-session-id-from-request (hunchensocket:client-request client)))
  ;; GAME-STATE's :INITFORM ran before MEMBERSHIP-TIER was known above
  ;; (defaulting to *RDESCENT-DEFAULT-MEMBERSHIP-TIER*'s dungeon/
  ;; monsters), so rebuild it now for CLIENT's actual tier -- unless a
  ;; still-fresh GAME-STATE from a recent disconnect by this same
  ;; browser (same SESSION-ID) is waiting in *RDESCENT-SUSPENDED-
  ;; STATES*, in which case resume that instead so a dropped/reconnected
  ;; WebSocket connection no longer resets an in-progress run.
  (setf (get-game-state client)
        (or (rdescent-resume-game-state (get-session-id client))
            (make-initial-state (get-membership-tier client))))
  (rdescent-register-client client)
  (hunchensocket:send-text-message client (rdescent-message-log-packet))
  ;; Send the real FOV-rendered grid for CLIENT's freshly-built
  ;; GAME-STATE (RDESCENT-GRID-PACKET) rather than the blank
  ;; RDESCENT-PLAYING-FIELD-PACKET placeholder, so the very first
  ;; frame a client sees already shows its starting field of view
  ;; lit up around the player instead of an empty field that only
  ;; fills in once the game loop's next tick fires.
  (hunchensocket:send-text-message client (rdescent-grid-packet client))
  (hunchensocket:send-text-message client (rdescent-player-stats-packet client)))

(defmethod hunchensocket:client-disconnected ((resource rdescent-resource)
                                              (client rdescent-client))
  "Unregister CLIENT from the registry actor once it disconnects, via
the pure REMOVE-RDESCENT-CLIENT reducer applied by the actor thread.
Also stashes CLIENT's current GAME-STATE in *RDESCENT-SUSPENDED-STATES*
(keyed on its SESSION-ID, if any) via RDESCENT-SUSPEND-GAME-STATE, so
that if the same browser reconnects (a new WebSocket connection means a
new RDESCENT-CLIENT instance, but Hunchensocket resends the same
cookies, including *RDESCENT-SESSION-COOKIE-NAME*) within
*RDESCENT-SUSPEND-GRACE-SECONDS*, CLIENT-CONNECTED resumes this exact
GAME-STATE instead of starting a fresh dungeon run."
  (rdescent-suspend-game-state (get-session-id client) (get-game-state client))
  (rdescent-unregister-client client))

(defmethod hunchensocket:text-message-received ((resource rdescent-resource)
                                                (client rdescent-client)
                                                message)
  "Parse MESSAGE into an RDESCENT-COMMAND via PARSE-RDESCENT-COMMAND
and, if recognized, ENQUEUE it onto CLIENT's own INPUT-QUEUE. This
method no longer touches GAME-STATE at all -- it runs on CLIENT's own
Hunchensocket read thread, which can be woken by a fast typist far more
often than the shared game-loop thread ticks, so its only job now is
parsing and handing the command off; TICK-GAME-STATE (run once per
client per *RDESCENT-TICK-SECONDS* heartbeat, on the single game-loop
thread) is what actually drains this queue and applies every command
in order via ADVANCE-GAME-STATE. This keeps GAME-STATE mutation
single-threaded (only ever touched from the game-loop thread) without
requiring a lock: SB-CONCURRENCY:QUEUE is itself thread-safe for
concurrent ENQUEUE/DEQUEUE. Malformed or unrecognized messages simply
parse to NIL and are silently ignored -- never enqueued -- so garbage
input from a client can never crash this thread (which would take down
that client's whole WebSocket read loop) nor pollute its queue.
The 'save' action is intercepted directly here at the I/O edge: since
GAME-STATE is an immutable value, we can safely grab a reference to it
and pack it immediately without any locks, sending the payload back
directly."
  (declare (ignore resource))
  (let ((packet (ignore-errors (cl-json:decode-json-from-string message))))
    (if (and packet (equal (cdr (assoc :action packet)) "save"))
        (hunchensocket:send-text-message
         client
         (cl-json:encode-json-alist-to-string 
          `(("target" . "save-payload")
            ("payload" . ,(pack-save-state (get-game-state client))))))
        (let ((command (parse-rdescent-command message)))
          (when command
            (sb-concurrency:enqueue command (get-input-queue client)))))))

;;; Server-authoritative game loop
;;;
(defun rdescent-drain-input-queue (queue)
  "Return an ordinary list, oldest first, of every RDESCENT-COMMAND
currently pending on QUEUE (a client's own INPUT-QUEUE), removing them
all from QUEUE as a side effect. Uses SB-CONCURRENCY:DEQUEUE in a loop
rather than a single bulk primitive: DEQUEUE returns two values (the
element, and a generalized boolean that's NIL once QUEUE is empty), so
looping until that second value is NIL is the standard SB-CONCURRENCY
idiom for draining a queue completely. Safe to call from any thread,
but in production only TICK-GAME-STATE ever calls this, always from
the single shared game-loop thread, so the commands returned here are
never interleaved with a concurrent drain of the same QUEUE."
  (loop for (command foundp) = (multiple-value-list (sb-concurrency:dequeue queue))
        while foundp
        collect command))

(defun tick-game-state (client)
  "Advance CLIENT's own GAME-STATE by exactly one game-loop heartbeat:
drain every RDESCENT-COMMAND currently pending on CLIENT's INPUT-QUEUE
(via RDESCENT-DRAIN-INPUT-QUEUE, oldest first) and fold them, plus one
world tick (ENERGY accrual and enemy AI), over CLIENT's current
GAME-STATE via the pure ADVANCE-GAME-STATE reducer, storing the result
back into CLIENT's GAME-STATE. This is the only place GAME-STATE is
ever mutated now that TEXT-MESSAGE-RECEIVED merely enqueues -- called
once per client, per *RDESCENT-TICK-SECONDS* heartbeat, by
TICK-ALL-CLIENTS on the single shared game-loop thread, so no locking
is needed around this read-compute-write even though CLIENT's own
WebSocket read thread may be concurrently ENQUEUEing new commands onto
the same INPUT-QUEUE at any moment (SB-CONCURRENCY:QUEUE itself is
safe for that concurrent access)."
  (let ((commands (rdescent-drain-input-queue (get-input-queue client)))
        (state (get-game-state client)))
    (setf (get-game-state client)
          (advance-game-state state (get-membership-tier client) commands (get-max-depth client)))))

(defun tile-render-char (state level x y tile)
  "Return the CHAR RENDER-GRID should draw for TILE at (X, Y) on
LEVEL, from this specific STATE's own player's point of view. Almost
always just (GET-CHAR TILE) -- the one exception is a locked-door TILE
(FUTURE_PLANS.md §9, GET-LOCKED-KEY-ID non-NIL) this player has
already opened (DOOR-OPENED-P): TILE's own CHAR is shared, cross-
player-cached dungeon geometry and so can never be mutated to
permanently show \"open\" for just one player (see TILE's own
docstring) -- instead, this returns an ordinary floor character
(#\\.) whenever DOOR-OPENED-P is T for this STATE, so an opened door
renders correctly for the player who opened it while every other
player sharing this same cached dungeon still sees it drawn closed."
  (if (and (get-locked-key-id tile) (door-opened-p state level x y))
      #\.
      (get-char tile)))

(defun render-grid (state tier level &key (width *rdescent-field-width*)
                                       (height *rdescent-field-height*))
  "Return an HTML string representing the WIDTH x HEIGHT playing
field for TIER's dungeon at LEVEL, one <span> per contiguous run of
same-visibility-class cells (rather than one per character) so the
DOM/CSS work stays cheap even at 80x30: each cell is classified as
:VISIBLE (currently lit by COMPUTE-FOV, CSS class \"v\"), :EXPLORED
(previously seen -- STATE's EXPLORED bit is set, but the cell isn't
currently visible -- CSS class \"e\", dimmed), or :SHROUD (never seen --
CSS class \"s\", rendered as a blank space regardless of what's really
there). Only :VISIBLE cells ever show an entity or the player -- STATE's
ENTITIES/PLAYER are invisible in fog or shroud, so descending into an
unlit room never reveals monsters lurking in it ahead of time. A still
HIDDEN-P TRAP-FIXTURE (FUTURE_PLANS.md §8) is likewise skipped when
populating ENTITY-CHARS regardless of visibility class, so the tile
underneath renders exactly as if nothing were there -- MAYBE-REVEAL-
HIDDEN-ENTITIES/MOVE-PLAYER's own trigger branch are the only ways a
trap's HIDDEN-P ever flips to NIL and its CHAR starts being drawn.
Rows are
separated by a literal #\\Newline inside the run of <span>s (not a
fresh span each line) so word-wrapping/copy-paste behaves like a
normal terminal transcript. Pure: no I/O, safe to unit test directly.
No locking is needed here: STATE (and the ENTITY/GAME-MAP values
reachable from it) are immutable, so no other thread can be mutating
them concurrently."
  (let* ((map (generate-dungeon tier level :width width :height height))
         (visible-mask (compute-fov map (get-x (get-player state)) (get-y (get-player state))
                                    (effective-fov-radius (get-player state))))
         (explored-mask (get-explored state))
         ;; Foreground: entities on this LEVEL only, sorted by
         ;; RENDER-ORDER ascending so corpses (order 0) are drawn
         ;; first and living actors -- including the player -- (order
         ;; 1) are drawn afterwards, overwriting a corpse's character
         ;; in the array if they share a cell (see ENTITY's
         ;; RENDER-ORDER slot documentation). Indexed by XY-TO-INDEX
         ;; into a flat vector for O(1) per-cell lookup during the
         ;; render loop below, mirroring VISIBLE-MASK/EXPLORED-MASK's
         ;; own flat layout.
         (entity-chars (make-array (* width height) :initial-element nil)))
    (dolist (ent (stable-sort (remove-if-not (lambda (ent) (= (get-level ent) level))
                                             (append (get-entities state) (list (get-player state))))
                              #'< :key #'render-order))
      (let ((x (get-x ent)) (y (get-y ent)))
        (when (and (<= 0 x (1- width)) (<= 0 y (1- height))
                   (not (and (typep ent 'trap-fixture) (get-hidden-p ent))))
          (setf (aref entity-chars (xy-to-index x y width)) (get-char ent)))))
    (with-output-to-string (out)
      (let ((current-state nil))
        (flet ((switch-to (tile-state)
                 (unless (eq tile-state current-state)
                   (when current-state (write-string "</span>" out))
                   (write-string (ecase tile-state
                                   (:visible "<span class=\"v\">")
                                   (:explored "<span class=\"e\">")
                                   (:shroud "<span class=\"s\">"))
                                 out)
                   (setf current-state tile-state))))
          (dotimes (y height)
            (dotimes (x width)
              (let* ((index (xy-to-index x y width))
                     (tile-state (cond ((= 1 (bit visible-mask index)) :visible)
                                      ((= 1 (bit explored-mask index)) :explored)
                                      (t :shroud))))
                (switch-to tile-state)
                (write-char (if (eq tile-state :shroud)
                               #\Space
                               (or (and (eq tile-state :visible) (aref entity-chars index))
                                   (let ((tile (map-tile-ref map x y)))
                                     (if tile (tile-render-char state level x y tile) #\Space))))
                            out)))
            (unless (= y (1- height))
              (write-char #\Newline out)))
          (when current-state (write-string "</span>" out)))))))

(defun rdescent-message-log-html (state &key (limit 5))
  "Return an HTML string (messages joined by \"<br>\") of the last
LIMIT entries in (GET-MESSAGE-LOG STATE), most-recent first (matching
MESSAGE-LOG's own newest-first ordering), or NIL if MESSAGE-LOG is
empty. Each entry's text (LOG-ENTRY-TEXT) is ESCAPE-HTML'd before being
wrapped in a <span style=\"color:...\"> set to that entry's own display
color (LOG-ENTRY-COLOR, \"white\" unless the entry specified otherwise
via MAKE-LOG-MESSAGE) -- entry text can embed entity NAMEs and is
otherwise untrusted-looking free text being dropped straight into the
DOM. Pure: STATE's MESSAGE-LOG is only ever read, never cleared here --
see RDESCENT-OUTBOUND-PACKETS' docstring for why the reducer
intentionally never truncates it."
  (let ((messages (subseq (get-message-log state) 0 (min limit (length (get-message-log state))))))
    (when messages
      (format nil "~{~A~^<br>~}"
              (mapcar (lambda (entry)
                        (format nil "<span style=\"color:~A\">~A</span>"
                                (log-entry-color entry)
                                (escape-html (log-entry-text entry))))
                      messages)))))

(defun rdescent-grid-packet (client)
  "The JSON packet sent to CLIENT each game tick, carrying the freshly
rendered playing field for CLIENT's own GAME-STATE, MEMBERSHIP-TIER,
and its player's current dungeon GET-LEVEL, plus \"width\"/\"height\"
(*RDESCENT-FIELD-WIDTH*/*RDESCENT-FIELD-HEIGHT*, the fixed grid
dimensions RENDER-GRID rendered HTML at) so /js/rdescent.js's targeting
cursor can clamp its own movement to the field's true bounds without
needing a second, independently-maintained copy of these constants
that could silently drift out of sync with the server's own values."
  (let ((state (get-game-state client)))
    (json-string
     `(("target" . "playing-field")
       ("html" . ,(render-grid state (get-membership-tier client)
                               (get-level (get-player state))))
       ("width" . ,*rdescent-field-width*)
       ("height" . ,*rdescent-field-height*)))))

(defun rdescent-player-stats-packet (client)
  "The JSON packet sent to CLIENT each game tick carrying its player's
current dungeon depth, HP-bar data, XP, RSU, Kombucha count, its seven
Corporate RPG Stats, and grid
position, targeting \"player-stats\"
with separate fields -- rather than one opaque HTML blob -- so
/js/rdescent.js can update the side panel's *persistent* DOM nodes in
place (mutating style.width/textContent) instead of destroying and
recreating them via innerHTML every tick, which is what let the CSS
width transition on the HP bar's spans actually animate (see
render-rdescent-page's docstring in views.lisp): \"depth-html\" is
\"Level: <current-depth>/<max-depth>\", \"room-html\" is
ROOM-KIND-DISPLAY-NAME (rdescent/dungeon.lisp) applied to the
GET-ROOM-KIND of the TILE (via MAP-TILE-REF, rdescent/mechanics.lisp)
at the player's own current (X, Y) -- e.g. \"Cubicle Farm\",
\"Open Office\", \"Server Room\", or \"Corridor\" while standing in a
hallway outside any room -- for #stats-room (centered via CSS,
unlike its label-prefixed siblings), displayed directly below
\"depth-html\" and above the HP bar in the side panel (see
RENDER-RDESCENT-PAGE's markup in views.lisp), \"hp-pct\"/\"dmg-pct\" are
HEALTH-PERCENTAGE and its complement (for #stats-hp-bar/#stats-dmg-bar's
style.width), \"hp-text\" is the plain \"<current>/<max> HP\" string (for
#stats-hp-text's textContent), \"xp-html\" is the player's XP
right-justified to 9 characters via FORMAT-XP-FOR-HTML (using
\"&nbsp;\" padding, since that's only meaningful when assigned via
innerHTML, not textContent), \"rsu-html\" is the player's RSU (this
game's gold/loot currency, see ENTITY's RSU slot/GET-RSU and GRAB-ITEM's
Stock Option handling) right-justified to 8 characters via
FORMAT-RSU-FOR-HTML -- one character narrower than XP's own 9, so the
two values' digits still line up visually once each is prefixed with
its own \"XP: \"/\"RSU: \" label (see FORMAT-RSU-FOR-HTML's own
docstring) -- displayed directly below/after XP in the stats panel,
\"kombucha-html\" is \"Kombuchas: <count>\" (for #stats-kombucha's
innerHTML, see GET-KOMBUCHA/DRINK-POTION in rdescent/entities.lisp and
rdescent/actions.lisp respectively),
\"equipment\" is a JSON array of exactly four objects, one per
equipment slot in the fixed order :WEAPON/:BODY/:HEAD/:OFF-HAND (see
ENTITY's EQUIPMENT slot/EQUIPPED-ITEM in rdescent/entities.lisp), each
with \"slot\" (the slot's PRINC-TO-STRING'd keyword name, e.g.
\"WEAPON\"), \"name\" (the occupying item's GET-ITEM-NAME, or NIL if
that slot is empty), and \"durability\"/\"max-durability\" (the
occupying item's own GET-DURABILITY/GET-MAX-DURABILITY, or NIL for an
empty slot -- see EQUIPPABLE-ITEM's own class docstring/APPLY-
EQUIPMENT-WEAR, rdescent/entities.lisp, for how these wear down in
combat) -- so /js/rdescent.js can render the currently
equipped items (name and remaining durability) into the stats side
panel (see #stats-equipment in
views.lisp) and also drive its equipment-slot picker modal used to
choose which slot to send an {\"action\": \"unequip\"} command for,
without a second dedicated packet,
\"val-bandwidth\"/\"val-pivot\"/\"val-caffeine-tolerance\"/
\"val-domain-knowledge\"/\"val-seniority\"/\"val-synergy\"/\"val-hygiene\"
are the player's seven Corporate RPG Stats (see ENTITY's docstring and
ROLL-STAT, rdescent/mechanics.lisp) as plain decimal strings via
PRINC-TO-STRING -- built by iterating
*RDESCENT-CORPORATE-STAT-ACCESSORS* (rdescent/mechanics.lisp) once rather
than one hand-written CONS per stat, so adding/renaming a stat only
requires updating that one list (see TECHNICAL_DEBT.md item #41) --
unlike XP/RSU these are just raw numbers with no
padding/label baked in, since /js/rdescent.js assigns them via
textContent (not innerHTML) onto #val-... spans that already carry
their own static label in a sibling span (see RENDER-RDESCENT-PAGE's
#player-corporate-stats markup in views.lisp) -- and
\"x\"/\"y\" are the player's own current grid coordinates (integers,
ENTITY-X/ENTITY-Y) -- sent every tick so /js/rdescent.js always knows
where the player currently stands, letting it seed the item-targeting
cursor at the player's own position rather than an arbitrary
field-center guess (see ENTERTARGETINGMODE in rdescent.js).
CURRENT-DEPTH is read from CLIENT's own GAME-STATE (GET-CURRENT-DEPTH)
and MAX-DEPTH from CLIENT itself (GET-MAX-DEPTH, this connection's
JWT-derived depth ceiling -- see RDESCENT-TIER-MAX-DEPTH). Pure aside
from reading CLIENT's own GAME-STATE/MAX-DEPTH."
  (let* ((state (get-game-state client))
         (player (get-player state))
         (hp-pct (health-percentage player))
         (tile (map-tile-ref (get-map state) (get-x player) (get-y player))))
    (json-string (append
                  `(("target" . "player-stats")
                    ("depth-html" . ,(format nil "Level: ~D/~D"
                                             (get-current-depth state) (get-max-depth client)))
                    ("room-html" . ,(room-kind-display-name (and tile (get-room-kind tile))))
                    ("hp-pct" . ,hp-pct)
                    ("dmg-pct" . ,(- 100 hp-pct))
                    ("hp-text" . ,(format nil "~D/~D HP" (hp player) (max-hp player)))
                    ("xp-html" . ,(format nil "XP: ~A" (format-xp-for-html player)))
                    ("rsu-html" . ,(format nil "RSU: ~A" (format-rsu-for-html player)))
                    ("kombucha-html" . ,(format nil "Kombuchas: ~D" (get-kombucha player)))
                    ("equipment" . ,(mapcar (lambda (slot)
                                              (let ((item (equipped-item player slot)))
                                                `(("slot" . ,(princ-to-string slot))
                                                  ("name" . ,(if item (get-item-name item) *json-null*))
                                                  ("durability" . ,(if item (get-durability item) *json-null*))
                                                  ("max-durability" . ,(if item (get-max-durability item) *json-null*)))))
                                            '(:weapon :body :head :off-hand))))
                  (mapcar (lambda (stat-accessor)
                            (cons (format nil "val-~A" (car stat-accessor))
                                  (princ-to-string (funcall (cdr stat-accessor) player))))
                          *rdescent-corporate-stat-accessors*)
                  `(("x" . ,(get-x player))
                    ("y" . ,(get-y player)))))))

(defun rdescent-inventory-packet (client)
  "The JSON packet sent to CLIENT each game tick carrying its player's
current INVENTORY contents, targeting \"inventory\" with an \"items\"
field: a JSON array of objects, one per distinct item name (see
GROUP-INVENTORY-FOR-DISPLAY in rdescent/entities.lisp), each with
\"name\" (GET-ITEM-NAME), \"count\" (how many the player carries),
\"index\" (the position of the first such item within the player's raw
INVENTORY list), and \"equippable\" (whether that first instance is an
EQUIPPABLE-ITEM, letting /js/rdescent.js's inventory modal offer an
\"e\" (equip) shortcut only where {\"action\": \"equip\"} would
actually succeed) -- e.g. two Scrolls of PIP and one Reply-All Bomb
becomes [{\"name\":\"Scroll of PIP\",\"count\":2,\"index\":0,
\"equippable\":false},{\"name\":\"Reply-All Bomb\",\"count\":1,
\"index\":2,\"equippable\":false}], so /js/rdescent.js's inventory
modal renders a single \"Scroll of PIP (x2)\" row instead of two
separate rows, while \"index\" is still exactly the raw INVENTORY
position a subsequent {\"action\": \"use-item\", \"item-index\": N,
...}/{\"action\": \"drop\", \"item-index\": N}/{\"action\": \"equip\",
\"item-index\": N} command must reference (see PARSE-RDESCENT-COMMAND
(rdescent/commands.lisp) / USE-ITEM/DROP-ITEM/EQUIP-ITEM
(rdescent/actions.lisp) to
act on one instance of that grouped entry. Pure aside from reading
CLIENT's own GAME-STATE."
  (json-string
   `(("target" . "inventory")
     ("items" . ,(mapcar (lambda (entry)
                          `(("name" . ,(first entry))
                            ("count" . ,(second entry))
                            ("index" . ,(third entry))
                            ("equippable" . ,(if (fourth entry) *json-true* *json-false*))))
                        (group-inventory-for-display (get-inventory (get-player (get-game-state client)))))))))

(defun rdescent-packet-if-changed (client key candidate)
  "Return CANDIDATE (a JSON packet string) unchanged if it differs from
the last packet CLIENT was actually sent under KEY (a keyword such as
:GRID, :STATS, :INVENTORY, or :MESSAGE-LOG), updating CLIENT's own
LAST-SENT-PACKETS cache to CANDIDATE either way it differed; otherwise
(CANDIDATE is STRING= to what is already cached under KEY) return NIL,
so RDESCENT-OUTBOUND-PACKETS omits this tick's packet for KEY entirely
-- CANDIDATE is always a fresh, idempotent snapshot of CLIENT's
current derived state (never a delta), so skipping an identical resend
is always safe: the client's own DOM/JS state already reflects exactly
that content from the last time it changed. See LAST-SENT-PACKETS'
own docstring (the RDESCENT-CLIENT slot definition, above) for why
most ticks most players are connected for, this returns NIL for every
one of :GRID/:STATS/:INVENTORY -- nothing on their own screen changed
since the previous tick."
  (let* ((cache (get-last-sent-packets client))
         (previous (cdr (assoc key cache :test #'eq))))
    (unless (and previous (string= candidate previous))
      (setf (get-last-sent-packets client)
            (acons key candidate (remove key cache :key #'car :test #'eq)))
      candidate)))

(defun rdescent-outbound-packets (client)
  "Return the list of JSON packet strings to actually send CLIENT this
tick for its current GAME-STATE: a playing-field packet
(RDESCENT-GRID-PACKET), a player-stats packet
(RDESCENT-PLAYER-STATS-PACKET), and an inventory packet
(RDESCENT-INVENTORY-PACKET) -- but each of those three only when it
differs from what CLIENT was already sent last tick (see RDESCENT-
PACKET-IF-CHANGED/LAST-SENT-PACKETS: an idle or merely-not-moving
player's own screen usually hasn't changed at all between one 50ms
tick and the next, so resending byte-identical HTML/JSON 20 times a
second would be pure waste) -- plus, when GET-MESSAGE-LOG is non-empty
and its rendering differs from last tick's, a message-log packet
carrying the HTML for its most recent 5 entries (RDESCENT-MESSAGE-LOG-
HTML) under \"html\", plus its most recent 50 entries under
\"history-html\" (for the scrollable full-log modal toggled by the 'v'
key -- see /js/rdescent.js's MESSAGE-LOG-MODAL handling), plus, when
STATE's own :PLAQUE-TEXT GAME-STATE flag (see GAME-STATE-FLAG/SET-
GAME-STATE-FLAG, RDESCENT/MECHANICS.LISP) is non-NIL, a one-shot
\"plaque\" packet carrying that text under \"text\" (never deduped
against LAST-SENT-PACKETS -- it is already inherently one-shot, since
TICK-ALL-CLIENTS clears the underlying flag immediately after this
packet is sent) -- see INTERACT-WITH-FIXTURE's own PLAQUE-FIXTURE
method (RDESCENT/ACTIONS.LISP), which sets that flag when a player
reads a final-level Commemorative Plaque, and TICK-ALL-CLIENTS' own
step (below) that clears the flag immediately after this packet is
sent, so the client's modal only ever pops open once per read rather
than every tick thereafter.
Not pure -- despite computing the packet contents purely, it also
mutates CLIENT's own LAST-SENT-PACKETS cache as a side effect of
deciding what to omit, so calling this twice in a row for the same
GAME-STATE returns a shorter (or empty) list the second time. Still
the seam between the game's core and the I/O shell (TICK-ALL-CLIENTS)
that actually sends them: callers just send whatever this list
contains, in order, without needing to know how many packets that is,
what they represent, or why some ticks it's fewer than others.

Note MESSAGE-LOG is never truncated or *cleared* by
RDESCENT-OUTBOUND-PACKETS itself (or anywhere else that only reads
it) -- it grows exactly as MOVE-PLAYER/PROCESS-ENEMY-TURNS push onto
it, and this function simply formats and sends only the last 5 (and,
for history, last 50) entries every tick, which is simple and correct
(the client always sees the most recent messages). Unbounded growth of
the underlying list is bounded elsewhere, not here: UPDATE-GAME-STATE
(the single choke point every writer goes through) caps MESSAGE-LOG to
its newest *RDESCENT-MESSAGE-LOG-MAX-LENGTH* entries, so even a client
that stays connected indefinitely can't accumulate an ever-growing
linked list -- see UPDATE-GAME-STATE's docstring."
  (let* ((state (get-game-state client))
         (log-html (rdescent-message-log-html state))
         (history-html (rdescent-message-log-html state :limit 50))
         (plaque-text (game-state-flag state :plaque-text)))
    (remove nil
            (append (list (rdescent-packet-if-changed client :grid (rdescent-grid-packet client))
                          (rdescent-packet-if-changed client :stats (rdescent-player-stats-packet client))
                          (rdescent-packet-if-changed client :inventory (rdescent-inventory-packet client)))
                    (when log-html
                      (list (rdescent-packet-if-changed
                             client :message-log
                             (json-string `(("target" . ,"message-log")
                                            ("html" . ,log-html)
                                            ("history-html" . ,(or history-html "")))))))
                    (when plaque-text
                      (list (json-string
                             `(("target" . "plaque")
                               ("text" . ,plaque-text)))))))))

(defun tick-all-clients ()
  "Advance every connected client's own GAME-STATE by one game-loop
heartbeat (via TICK-GAME-STATE, which drains that client's INPUT-QUEUE
and folds it plus one world tick over GAME-STATE) and then compute and
send its outbound packets (via RDESCENT-OUTBOUND-PACKETS, pure), so
every player sees their own '@' position, on their own tier's dungeon
level, rather than a single shared one. This function is the I/O
shell: it owns the only side effects in the per-tick pipeline --
reading the client list, mutating each client's GAME-STATE, and
performing the socket sends -- while the state transition and the
packets themselves are computed by pure functions (ADVANCE-GAME-STATE,
RDESCENT-OUTBOUND-PACKETS). The client list itself is obtained via
RDESCENT-CLIENTS-SNAPSHOT, a synchronous request/reply message to the
registry actor thread (see *RDESCENT-CLIENTS-MAILBOX*), rather than a
locked read of a shared list. Any client whose tick/packets fail to
compute (e.g. a bug in ADVANCE-GAME-STATE/RENDER-GRID/COMPUTE-FOV/
RDESCENT-MESSAGE-LOG-HTML triggered only by that client's own
particular GAME-STATE or queued commands) or send (e.g. a half-closed
socket not yet reaped by CLIENT-DISCONNECTED) is skipped rather than
aborting every other client's tick -- the HANDLER-CASE below wraps
TICK-GAME-STATE and the packet computation (RDESCENT-OUTBOUND-PACKETS)
as well as the send loop, per TECHNICAL_DEBT.md item #28: an argument
to MAPC is evaluated before MAPC's lambda body runs, so previously only
errors raised while *sending* packets were caught -- any error raised
while *ticking the state* or *computing* the packets escaped this
function (and, uncaught by START-GAME-LOOP in turn, killed the whole
game-loop thread, silently stopping ticks for every connected client,
not just the one whose state triggered the bug).

After a successful send, if CLIENT's own GAME-STATE carries a non-NIL
:PLAQUE-TEXT flag (see GAME-STATE-FLAG/SET-GAME-STATE-FLAG,
RDESCENT/MECHANICS.LISP, and RDESCENT-OUTBOUND-PACKETS' own \"plaque\"
packet above), that flag is cleared back to NIL immediately -- this is
the one-shot half of that trigger: the packet was already included in
this very tick's send, so leaving the flag set would resend the same
\"plaque\" packet (and re-pop the client's modal) on every subsequent
tick forever."
  (mapc (lambda (client)
          (handler-case
              (progn
                (tick-game-state client)
                (mapc (lambda (packet)
                        (hunchensocket:send-text-message client packet))
                      (rdescent-outbound-packets client))
                (when (game-state-flag (get-game-state client) :plaque-text)
                  (setf (get-game-state client)
                        (set-game-state-flag (get-game-state client) :plaque-text nil))))
            (error (e)
              (rdescent-safe-log-warning "failed to tick/compute/send packets for client -- ~A" e))))
        (rdescent-clients-snapshot)))

(defvar *rdescent-game-loop-thread* nil
  "The background thread running the game loop's TICK-ALL-CLIENTS
heartbeat, or NIL if START-GAME-LOOP hasn't been called yet (or the
thread has since died). See START-GAME-LOOP.")

(defun rdescent-tick-events ()
  "Expands to an infinite SERIES of tick events, one produced every
*RDESCENT-TICK-SECONDS* seconds. This is the explicit reactive **event
source** consumed by START-GAME-LOOP: it isolates the game loop's only
remaining side effect that isn't already a socket send -- the timer's
(SLEEP ...) -- inside a SCAN-FN generator, so that the game loop's body
(in START-GAME-LOOP) reads as \"for each tick, run the pure-transform
-> I/O-sink pipeline\" rather than a flat imperative loop mixing timing
and broadcasting together. Each element is the keyword :TICK; its
value is irrelevant, only its arrival matters. Uses SERIES (already a
project dependency per PACKAGE.LISP, used elsewhere for lazy sequence
fusion, e.g. DB-AUTH.LISP's code generation) rather than introducing a
new reactive-streams dependency."
  (declare (optimizable-series-function))
  (scan-fn 'symbol
           (lambda () (sleep *rdescent-tick-seconds*) :tick)
           (lambda (tick) (declare (ignore tick)) (sleep *rdescent-tick-seconds*) :tick)))

(defun rdescent-collect-ticks (n)
  "Collect the first N elements of RDESCENT-TICK-EVENTS into an
ordinary list. Exists so callers (in particular, tests) can consume a
bounded prefix of the otherwise-infinite tick series without needing
to write their own SERIES pipeline; production code (START-GAME-LOOP)
consumes the series directly via ITERATE instead.
NOTE: not called anywhere in production code -- only exercised by the
test suite (tests/tests.lisp's RDESCENT-TICK-EVENTS-PURE)."
  (collect 'list (subseries (rdescent-tick-events) 0 n)))

(defun rdescent-tick-once ()
  "Call TICK-ALL-CLIENTS once for a single game-loop tick, catching and
logging (via RDESCENT-SAFE-LOG-WARNING) any error TICK-ALL-CLIENTS
itself doesn't already anticipate, so a single bad tick never kills
START-GAME-LOOP's own background thread -- see that function's own
docstring for why this belt-and-suspenders HANDLER-CASE exists.
Factored out into its own ordinary DEFUN, rather than written inline
in START-GAME-LOOP's own ITERATE body, because HANDLER-CASE expands
(at least on SBCL, via SB-INT:DX-FLET) into an FLET -- and SERIES'
own ITERATE macro (this file's PACKAGE.LISP-wide SERIES dependency,
used here to drive the tick loop) refuses to have an FLET inlined
directly inside one of its own scan bodies (\"Restriction violation
... The form FLET not allowed in SERIES expressions\", a hard
COMPILE-FILE error, not a mere style-warning). Calling this as a
single, ordinary FUNCALL from within ITERATE's scan body sidesteps
that restriction entirely, since no FLET-expanding macro is left
lexically inside the scan expression itself."
  (handler-case (tick-all-clients)
    (error (e)
      (rdescent-safe-log-warning "tick-all-clients failed for this tick -- ~A" e))))

(defun start-game-loop ()
  "Spawn (if not already running) a background thread that, for every
tick produced by RDESCENT-TICK-EVENTS (the event source), calls
TICK-ALL-CLIENTS (the pure-transform-then-I/O-sink pipeline described
in TICK-ALL-CLIENTS' docstring) -- structurally \"event source -> pure
transform -> I/O sink\" rather than one flat imperative loop body, per
this file's reactive-programming convention. Safe to call more than
once: a second call is a no-op while the loop thread from a previous
call is still alive.

Each tick's TICK-ALL-CLIENTS call is individually wrapped in its own
HANDLER-CASE (belt-and-suspenders alongside TICK-ALL-CLIENTS' own
per-client HANDLER-CASE, per TECHNICAL_DEBT.md item #28): even a bug
TICK-ALL-CLIENTS itself doesn't anticipate (e.g. in
RDESCENT-CLIENTS-SNAPSHOT, or anywhere outside the per-client loop) is
logged and the loop continues to the next tick rather than silently
killing this thread -- once this thread dies, nothing currently
restarts it, so every connected client would otherwise stop receiving
updates for the rest of the process's lifetime with no visible error.
See RDESCENT-TICK-ONCE, just above, for why that HANDLER-CASE is its
own top-level DEFUN rather than written inline here."
  (unless (and *rdescent-game-loop-thread*
               (bordeaux-threads:thread-alive-p *rdescent-game-loop-thread*))
    (setf *rdescent-game-loop-thread*
          (bordeaux-threads:make-thread
           (lambda ()
             (iterate ((tick (rdescent-tick-events)))
              (declare (ignore tick))
              (rdescent-tick-once)))
           :name "rdescent-game-loop")))
  *rdescent-game-loop-thread*)

(defvar *rdescent-game-loop-watchdog-thread* nil
  "The background thread running RDESCENT-GAME-LOOP-WATCHDOG-LOOP, or
NIL if START-GAME-LOOP-WATCHDOG hasn't been called yet. See
START-GAME-LOOP-WATCHDOG.")

(defparameter *rdescent-watchdog-check-seconds* 5
  "How often, in seconds, the watchdog thread checks whether
*RDESCENT-GAME-LOOP-THREAD* (and, per TECHNICAL_DEBT.md item #35,
*RDESCENT-CLIENTS-REGISTRY-THREAD*) is still alive.")

(defun rdescent-game-loop-watchdog-loop ()
  "Body of the watchdog thread: forever, sleep
*RDESCENT-WATCHDOG-CHECK-SECONDS* seconds, then call START-GAME-LOOP
and START-RDESCENT-CLIENTS-REGISTRY again, and call
RDESCENT-EVICT-EXPIRED-SUSPENDED-STATES. The two START- calls are
idempotent (a no-op whenever their respective thread is already alive),
so this is simply a periodic \"is it still alive? if not, restart it\"
check rather than ever spawning a duplicate -- a defense-in-depth
backstop for TECHNICAL_DEBT.md item #28 (the game loop) and item #35
(the clients registry actor): even if some future bug manages to kill
the game-loop thread despite TICK-ALL-CLIENTS/START-GAME-LOOP's own
per-tick and per-client error handling, ticks resume for every
connected client within *RDESCENT-WATCHDOG-CHECK-SECONDS* instead of
staying dead for the rest of the process's lifetime. The eviction call
bounds *RDESCENT-SUSPENDED-STATES*'s size the same way this loop
already keeps the two threads alive -- a periodic sweep rather than a
one-off cleanup. Logs a warning the first time it notices and repairs a
dead game-loop thread, via RDESCENT-SAFE-LOG-WARNING (this loop runs on
its own background thread, same as the game loop itself, so the same
non-request-thread logging caveat from item #29 applies here too)."
  (loop
    (sleep *rdescent-watchdog-check-seconds*)
    (let ((game-loop-was-dead (not (and *rdescent-game-loop-thread*
                                        (bordeaux-threads:thread-alive-p *rdescent-game-loop-thread*))))
          (registry-was-dead (not (and *rdescent-clients-registry-thread*
                                       (bordeaux-threads:thread-alive-p *rdescent-clients-registry-thread*)))))
      (start-game-loop)
      (start-rdescent-clients-registry)
      (rdescent-evict-expired-suspended-states)
      (when game-loop-was-dead
        (rdescent-safe-log-warning "game-loop thread was dead; restarted it"))
      (when registry-was-dead
        (rdescent-safe-log-warning "clients-registry thread was dead; restarted it")))))

(defun start-game-loop-watchdog ()
  "Spawn (if not already running) the background watchdog thread that
periodically restarts *RDESCENT-GAME-LOOP-THREAD* if it has died. Safe
to call more than once: a second call is a no-op while the watchdog
thread from a previous call is still alive."
  (unless (and *rdescent-game-loop-watchdog-thread*
               (bordeaux-threads:thread-alive-p *rdescent-game-loop-watchdog-thread*))
    (setf *rdescent-game-loop-watchdog-thread*
          (bordeaux-threads:make-thread #'rdescent-game-loop-watchdog-loop
                                        :name "rdescent-game-loop-watchdog")))
  *rdescent-game-loop-watchdog-thread*)
