;;; -*- Lisp -*-

;;; The core engine: server lifecycle (start/stop), bootstrap wheel-user
;;; provisioning, and the top-level MAIN entrypoint.

(in-package "JRM-CODE-PROJECT")

(defvar *acceptor* nil
  "The Hunchentoot server acceptor instance.")

(defun get-session-secret ()
  "Return the Hunchentoot session-encryption secret from the SESSION_SECRET
environment variable. If unset, generate a fresh, cryptographically random
64-character hex string at startup instead of falling back to any
hardcoded value -- sessions simply won't survive a restart with no
SESSION_SECRET configured, which is the safe failure mode (as opposed to
every deployment sharing one baked-in secret)."
  (or (uiop:getenv "SESSION_SECRET")
      (ironclad:byte-array-to-hex-string (ironclad:random-data 32))))

(defun stop-server ()
  "Stop the running Hunchentoot server."
  (when *acceptor*
    (format t ";; Stopping Hunchentoot server...~%")
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    (format t ";; Server stopped.~%"))
  *acceptor*)

(defun parse-bootstrap-wheel-usernames ()
  "Parse a comma-separated list of usernames/emails from the
BOOTSTRAP_WHEEL_USERNAMES environment variable, trimming whitespace
around each entry and dropping empty entries. Returns NIL if the
environment variable is unset or empty, so no account is granted wheel
access by default in a fresh, unconfigured deployment."
  (let ((raw (uiop:getenv "BOOTSTRAP_WHEEL_USERNAMES")))
    (if (and raw (plusp (length raw)))
        (remove-if (lambda (s) (zerop (length s)))
                   (mapcar (lambda (s) (string-trim '(#\Space #\Tab #\Newline #\Return) s))
                           (cl-ppcre:split "," raw)))
        nil)))

(defparameter *bootstrap-wheel-usernames* (parse-bootstrap-wheel-usernames)
  "Usernames that must always have the wheel (super user) bit set,
enforced idempotently every time the server starts (e.g. in case the
database was restored from an older backup predating the wheel column).
Sourced from the BOOTSTRAP_WHEEL_USERNAMES environment variable (a
comma-separated list); defaults to NIL (no bootstrap wheel users) if
unset. Note: an email-shaped string is still just a valid username here
-- this list is not restricted to email addresses.")

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

(defun start-server ()
  "Start the Hunchentoot server on port 4242 serving resources/www/html/."
  (when *acceptor*
    (format t ";; Server acceptor already exists. Stopping it first...~%")
    (stop-server))
  (format t ";; Initializing authentication database...~%")
  (ignore-errors (jrm-auth:init-db))
  (format t ";; Initializing pastebin database...~%")
  (ignore-errors (jrm-auth:init-pastebin-db))
  (format t ";; Initializing API keys database...~%")
  (ignore-errors (jrm-auth:init-api-keys-db))
  (format t ";; Ensuring bootstrap wheel users...~%")
  (ignore-errors (ensure-bootstrap-wheels))
  (format t ";; Initializing Stripe product...~%")
  (ignore-errors (init-stripe-product))
  (format t ";; Registering interviews paywall dispatcher...~%")
  (register-interviews-dispatcher)
  (format t ";; Registering heresies paywall dispatchers...~%")
  (register-heresies-dispatchers)
  (format t ";; Registering direct paste URL dispatcher...~%")
  (register-paste-direct-dispatcher)
  (setf hunchentoot:*session-secret* (get-session-secret))
  (setf *acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port 4242
                       :address "::"
                       :document-root (asdf:system-relative-pathname :jrm-code-project "resources/www/html/")))
  (hunchentoot:start *acceptor*)
  (format t ";; Server is now running. Visit http://localhost:4242/~%")
  *acceptor*)

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
