;;; -*- Lisp -*-

;;; The core engine: server lifecycle (start/stop), bootstrap wheel-user
;;; provisioning, and the top-level MAIN entrypoint.

(in-package "JRM-CODE-PROJECT")

(defvar *acceptor* nil
  "The Hunchentoot server acceptor instance.")

(defvar *acceptor-lock* (sb-thread:make-mutex :name "acceptor-lifecycle")
  "Guards every read-then-write transition of *ACCEPTOR* (checking whether
one is already running, then starting/stopping/replacing it) in
START-SERVER and STOP-SERVER. Without this, two concurrent callers (e.g.
an admin \"restart\" action racing a monitoring/health-check script that
also calls START-SERVER) could both observe *ACCEPTOR* as NIL, each start
their own Hunchentoot instance, and leak one of them -- the same class of
unsynchronized-global-state risk already fixed for *TARPIT-STRIKES* (see
TARPIT.LISP) and *RATE-LIMITS* (see RATE-LIMIT.LISP), now applied
consistently here too.")

(defmacro run-startup-step ((label) &body body)
  "Run BODY as one named startup step during START-SERVER. Unlike a bare
IGNORE-ERRORS, this always prints the LABEL and, on failure, the actual
condition report (not just silence) via FORMAT, and records LABEL in
*STARTUP-FAILURES* so START-SERVER can print an explicit summary of what
did/didn't come up cleanly once the acceptor is listening. Startup still
proceeds best-effort past a failed step (e.g. Stripe being unreachable
shouldn't prevent the site from serving static pages), but a failure is
never silent the way a plain IGNORE-ERRORS would make it."
  `(handler-case
       (progn ,@body (format t ";; ~A: OK~%" ,label))
     (error (e)
       (format t ";; ~A: FAILED -- ~A~%" ,label e)
       (push ,label *startup-failures*))))

(defvar *startup-failures* nil
  "Labels (strings) of any RUN-STARTUP-STEP that failed during the most
recent START-SERVER call, in the order they failed. Empty/NIL means every
startup step completed without signaling an error. Inspect this after
calling START-SERVER (e.g. in a REPL, monitoring script, or health check)
to detect a partially-initialized server that is nonetheless listening.")

(defun stop-server ()
  "Stop the running Hunchentoot server. Thread-safe: the check-then-clear
of *ACCEPTOR* is serialized via *ACCEPTOR-LOCK* so a concurrent
START-SERVER/STOP-SERVER call can never race this one (see
*ACCEPTOR-LOCK*)."
  (sb-thread:with-mutex (*acceptor-lock*)
    (%stop-acceptor-locked)))

(defun %stop-acceptor-locked ()
  "The actual stop-the-acceptor logic, assuming *ACCEPTOR-LOCK* is already
held by the caller. Factored out so START-SERVER can stop a stale
acceptor and start a fresh one as a single atomic, lock-held operation
(rather than releasing and re-acquiring the lock between STOP-SERVER and
the rest of START-SERVER, which would let another thread's START-SERVER
or STOP-SERVER interleave in between)."
  (when *acceptor*
    (format t ";; Stopping Hunchentoot server...~%")
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    (format t ";; Server stopped.~%"))
  *acceptor*)

(defun parse-bootstrap-wheel-usernames (env-value)
  "Parse ENV-VALUE (the raw BOOTSTRAP_WHEEL_USERNAMES environment
variable string, or NIL if unset) into a list of usernames: split on
commas, trim surrounding whitespace from each piece, and drop any
resulting empty strings (so a trailing comma or accidental double comma
doesn't produce a bogus blank username). Returns NIL for a NIL,
empty, or all-whitespace/comma ENV-VALUE."
  (if (or (null env-value) (zerop (length (string-trim '(#\Space #\Tab) env-value))))
      nil
      (remove "" (mapcar (lambda (piece) (string-trim '(#\Space #\Tab) piece))
                          (uiop:split-string env-value :separator ","))
              :test #'string=)))

(defparameter *bootstrap-wheel-usernames*
  (parse-bootstrap-wheel-usernames (uiop:getenv "BOOTSTRAP_WHEEL_USERNAMES"))
  "Usernames that must always have the wheel (super user) bit set,
enforced idempotently every time the server starts (e.g. in case the
database was restored from an older backup predating the wheel column).
Note: an email-shaped string is still just a valid username here -- this
list is not restricted to email addresses. Populated from the
BOOTSTRAP_WHEEL_USERNAMES environment variable (a comma-separated list,
see PARSE-BOOTSTRAP-WHEEL-USERNAMES), defaulting to NIL (no bootstrap
wheel users at all) if the variable is unset -- a personal identity is
no longer granted admin rights unconditionally for every deployment
built from this source.")

(defun ensure-bootstrap-wheels ()
  "Ensure every username in *BOOTSTRAP-WHEEL-USERNAMES* has the wheel bit
set, if the corresponding user account exists."
  (mapc (lambda (username)
          (if (jrm-auth:get-user username)
              (progn
                (jrm-auth:set-user-wheel username t)
                (format t ";; Ensured wheel bit set for ~A~%" username))
              (format t ";; Warning: bootstrap wheel user ~A does not exist yet; skipping.~%" username)))
        *bootstrap-wheel-usernames*))

(defun generate-random-session-secret ()
  "Generate a fresh, cryptographically random session secret (a 64-
character hex string, matching the shape of the prior hardcoded literal)
using IRONCLAD's CSPRNG. Used as the fallback when HUNCHENTOOT_SESSION_SECRET
is unset, so a deployment that forgets to set the environment variable
still gets a unique, unguessable secret per process rather than silently
reusing a value checked into source control -- the tradeoff is that
sessions won't survive a server restart if the env var isn't set
(every restart mints a new random secret, invalidating old session
cookies), which is an acceptable, safe failure mode for a missing
config value."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 32)))

(defun start-server ()
  "Start the Hunchentoot server on port 4242 serving resources/www/html/.
Thread-safe: the whole check-existing/stop-stale/init/start sequence runs
under *ACCEPTOR-LOCK*, so a concurrent call to START-SERVER or STOP-SERVER
from another thread can't interleave and leak or double-start an
acceptor (see *ACCEPTOR-LOCK*)."
  (sb-thread:with-mutex (*acceptor-lock*)
    (when *acceptor*
      (format t ";; Server acceptor already exists. Stopping it first...~%")
      (%stop-acceptor-locked))
    (setf *startup-failures* nil)
    (format t ";; Initializing authentication database...~%")
    (run-startup-step ("auth database") (jrm-auth:init-db))
    (format t ";; Initializing pastebin database...~%")
    (run-startup-step ("pastebin database") (jrm-auth:init-pastebin-db))
    (format t ";; Initializing API keys database...~%")
    (run-startup-step ("API keys database") (jrm-auth:init-api-keys-db))
    (format t ";; Ensuring bootstrap wheel users...~%")
    (run-startup-step ("bootstrap wheel users") (ensure-bootstrap-wheels))
    (format t ";; Initializing Stripe product...~%")
    (run-startup-step ("Stripe product initialization") (init-stripe-product))
    (format t ";; Registering interviews paywall dispatcher...~%")
    (register-interviews-dispatcher)
    (format t ";; Registering heresies paywall dispatchers...~%")
    (register-heresies-dispatchers)
    (format t ";; Registering direct paste URL dispatcher...~%")
    (register-paste-direct-dispatcher)
    (setf hunchentoot:*session-secret*
          (or (uiop:getenv "HUNCHENTOOT_SESSION_SECRET") (generate-random-session-secret)))
    (setf *acceptor*
          (make-instance 'hunchentoot:easy-acceptor
                         :port 4242
                         :address "::"
                         :document-root (asdf:system-relative-pathname :jrm-code-project "resources/www/html/")))
    (hunchentoot:start *acceptor*)
    (if *startup-failures*
        (format t ";; Server is now running with ~D failed startup step(s): ~{~A~^, ~}. Check the log above for details -- these subsystems may not work correctly until fixed.~%"
                (length *startup-failures*) (reverse *startup-failures*))
        (format t ";; Server is now running. Visit http://localhost:4242/~%"))
    *acceptor*))

;; 404 tarpit: punish IPs that repeatedly hit 404 (see TARPIT.LISP) by
;; holding their connection open, and once tarpitted, replacing the
;; response with a 429 Too Many Requests plus a taunting message body
;; instead of the ordinary 404 (mildly delayed but otherwise untouched).
(defmethod hunchentoot::acceptor-status-message :around ((acceptor hunchentoot::easy-acceptor) status-code &key &allow-other-keys)
  (if (= status-code hunchentoot::+http-not-found+)
      (or (tarpit-handle-404 (hunchentoot:real-remote-addr))
          (call-next-method))
      (call-next-method)))

(defun main ()
  "Main entrypoint of the system."
  (start-server))
