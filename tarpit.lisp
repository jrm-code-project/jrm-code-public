;;; -*- Lisp -*-

;;; -- 404 TARPIT --
;;;
;;; A dynamic, in-memory rate limiter specifically for 404 Not Found
;;; responses, meant to punish bots that brute-force the server looking
;;; for exploitable paths (WordPress admin panels, .env files, etc.).
;;; Distinct from RATE-LIMIT.LISP's token-bucket limiter (which throttles
;;; the programmatic /api/v1/... routes): this one tracks 404s per
;;; source IP address in a rolling 60-second window, and once an IP
;;; racks up more than *TARPIT-STRIKE-THRESHOLD* 404s within that
;;; window, every subsequent 404 response from that IP is held open for
;;; *TARPIT-PENALTY-SECONDS* (rather than the ordinary
;;; *TARPIT-NORMAL-DELAY-SECONDS*) before finally responding with 429
;;; Too Many Requests instead of 404 -- tying up one of the bot's
;;; concurrent worker threads/connections for as long as possible.
;;;
;;; Entirely in-process and in-memory (a single EQUAL hash table guarded
;;; by an SB-THREAD:MUTEX, since Hunchentoot dispatches requests to a
;;; pool of worker threads): this resets on restart and does not
;;; coordinate across multiple server processes, which is fine for this
;;; single-process deployment.

(in-package "JRM-CODE-PROJECT")

(defparameter *tarpit-window-seconds* 60
  "The rolling window, in seconds, within which consecutive 404s from
the same IP accumulate strikes. An IP whose most recent 404 is older
than this many seconds gets its strike count reset to 1 on its next
404, effectively starting a fresh window.")

(defparameter *tarpit-strike-threshold* 10
  "An IP is considered \"in the tarpit\" once its strike count *exceeds*
this many 404s within *TARPIT-WINDOW-SECONDS* (i.e. the 11th strike and
beyond within the window trigger the penalty).")

(defparameter *tarpit-penalty-seconds* 15
  "How long (via SLEEP) to hold the connection open for an IP that is in
the tarpit, before finally responding with 429 Too Many Requests.")

(defparameter *tarpit-normal-delay-seconds* 1
  "How long (via SLEEP) to hold the connection open for an ordinary
(not-yet-tarpitted) 404, to mildly slow down casual scanning without
meaningfully impacting legitimate users who hit a genuine dead link.")

(defstruct tarpit-entry
  "STRIKES is how many 404s this IP has racked up in the current
window; LAST-SEEN is the universal time of its most recent 404."
  (strikes 1 :type integer)
  (last-seen (get-universal-time) :type integer))

(defparameter *tarpit-strikes* (make-hash-table :test 'equal)
  "Maps an IP address (a string, per HUNCHENTOOT:REAL-REMOTE-ADDR) to a
TARPIT-ENTRY tracking its 404 strikes within the current
*TARPIT-WINDOW-SECONDS* window. Guarded by *TARPIT-LOCK* -- never read
or write this table without holding that lock.")

(defparameter *tarpit-lock* (sb-thread:make-mutex :name "tarpit-strikes")
  "Mutex serializing all access to *TARPIT-STRIKES*, since Hunchentoot
serves requests from a pool of worker threads and 404s from many
different IPs (or the same IP, concurrently) can arrive at once.")

(defun tarpit-register-strike (ip)
  "Record a 404 for IP and return T if IP is now in the tarpit (its
strike count exceeds *TARPIT-STRIKE-THRESHOLD* within the current
*TARPIT-WINDOW-SECONDS* window), or NIL otherwise. Thread-safe: all
reads and writes of *TARPIT-STRIKES* happen while holding *TARPIT-LOCK*.

Strike logic: if IP has no prior entry, or its last 404 was more than
*TARPIT-WINDOW-SECONDS* seconds ago, its strike count resets to 1 and
its timestamp updates to now (a fresh window). Otherwise (last 404 was
within the window), its strike count increments and its timestamp
updates to now."
  (sb-thread:with-mutex (*tarpit-lock*)
    (let* ((now (get-universal-time))
           (entry (gethash ip *tarpit-strikes*)))
      (if (and entry (<= (- now (tarpit-entry-last-seen entry)) *tarpit-window-seconds*))
          (progn
            (incf (tarpit-entry-strikes entry))
            (setf (tarpit-entry-last-seen entry) now))
          (setf (gethash ip *tarpit-strikes*)
                (make-tarpit-entry :strikes 1 :last-seen now)))
      (> (tarpit-entry-strikes (gethash ip *tarpit-strikes*)) *tarpit-strike-threshold*))))

(defparameter *tarpit-taunts*
  #("Nice try. Did you really think brute-forcing /wp-admin.php on a Lisp server was going to work?"
    "Congratulations, you've discovered the tarpit. Enjoy the wait, script kiddie."
    "404s all the way down, and now so is your connection pool. Have fun."
    "This isn't WordPress. This isn't PHP. This is Common Lisp, and you just got rate-limited by a CONS cell."
    "Scanning for .env files? Bold strategy. Anyway, you're in the tarpit now."
    "Your bot's thread pool called. It wants to know why it's stuck here for 15 seconds."
    "Keep hammering 404s if you want, but every one just extends your sentence in the tarpit."
    "A wise script kiddie would give up by now. You are not that script kiddie.")
  "A rotating set of taunts served in the response body once an IP is
tarpitted (see TARPIT-HANDLE-404), so bots grinding through 404s at
least get mocked for their trouble.")

(defun tarpit-random-taunt ()
  "Pick one of *TARPIT-TAUNTS* pseudo-randomly."
  (aref *tarpit-taunts* (random (length *tarpit-taunts*))))

(defun tarpit-handle-404 (ip)
  "Apply the 404 tarpit penalty for a 404 response from IP: register a
strike (see TARPIT-REGISTER-STRIKE), then either hold the connection for
*TARPIT-PENALTY-SECONDS*, set the response code to 429 Too Many
Requests, and return an HTML taunt message body (if IP is now in the
tarpit), or hold it for the milder *TARPIT-NORMAL-DELAY-SECONDS* and
return NIL (leaving the response as an ordinary 404 Not Found)
otherwise. Intended to be called from the acceptor's status-message
handling (see SERVER.LISP), which should use a non-NIL return value as
the literal response body instead of the default 404 message."
  (if (tarpit-register-strike ip)
      (progn
        (sleep *tarpit-penalty-seconds*)
        (setf (hunchentoot:return-code*) hunchentoot:+http-too-many-requests+)
        (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
        (format nil "<html><head><title>429 Too Many Requests</title></head><body><h1>429 Too Many Requests</h1><p>~A</p></body></html>"
                (tarpit-random-taunt)))
      (progn
        (sleep *tarpit-normal-delay-seconds*)
        nil)))
