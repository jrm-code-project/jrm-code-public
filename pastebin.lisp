;;; -*- mode: Lisp; coding: utf-8; -*-

(in-package :jrm-auth)

;;; -- PASTEBIN DATA ACCESS LAYER --
;;;
;;; Backs the pastebin feature with a `pastes` table keyed by a random,
;;; unguessable 16-character Base62 id. Each paste belongs to a member
;;; (owner_username) and carries a per-owner UNIQUE `user_sequence_num` (the
;;; owner's Nth paste ever), which is what makes the "rolling window"
;;; retention queries below possible: ordering by that column descending
;;; and skipping the first N rows identifies exactly the stale pastes to
;;; reap. Free members keep their 3 most recent pastes (each expiring 90
;;; days after creation); paid members keep their 128 most recent pastes
;;; (which never expire on their own).
;;;
;;; NOTE: the spec for this table names its foreign key target
;;; `members(username)`, but this codebase's user table is `users` (see
;;; DB-AUTH.LISP) -- there is no separate `members` table, so the
;;; constraint below references `users(username)` instead.
;;;
;;; TESTABILITY: POSTMODERN:QUERY/EXECUTE/WITH-TRANSACTION are macros that
;;; compile straight down to CL-POSTGRES wire-protocol calls, so there is
;;; no runtime seam to intercept without a live Postgres socket. To allow
;;; the public functions below to be exercised against a mock database
;;; (see tests/tests.lisp), every actual database touch is funneled
;;; through one of the *PASTEBIN-...* special variables defined just
;;; below. Each defaults to a small function that does the real
;;; POSTMODERN/WITH-DB call; tests rebind them (via LET) to an in-memory
;;; fake, with no change to the public API or default production
;;; behavior.

(defun %pastebin-init-schema ()
  (rename-column-if-exists "pastes" "owner_email" "owner_username")
  (with-db
      (postmodern:execute "CREATE TABLE IF NOT EXISTS pastes (
                                       id VARCHAR(16) PRIMARY KEY,
                                       owner_username VARCHAR(255) NOT NULL,
                                       user_sequence_num INTEGER NOT NULL,
                                       content TEXT NOT NULL,
                                       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                                       expires_at TIMESTAMP,

                                       FOREIGN KEY (owner_username) REFERENCES users(username) ON DELETE CASCADE,
                                       UNIQUE (owner_username, user_sequence_num))")
    (postmodern:execute "CREATE INDEX IF NOT EXISTS idx_pastes_owner ON pastes(owner_username)")
    (postmodern:execute "CREATE INDEX IF NOT EXISTS idx_pastes_expires ON pastes(expires_at)")))

(defun %pastebin-max-sequence (owner-username)
  (with-db
      (sql-null-to-nil
       (postmodern:query
        "SELECT MAX(user_sequence_num) FROM pastes WHERE owner_username = $1"
        owner-username :single))))

(defun %pastebin-insert-free (id owner-username seq content expires-at)
  (with-db
      (postmodern:execute
       "INSERT INTO pastes (id, owner_username, user_sequence_num, content, expires_at)
        VALUES ($1, $2, $3, $4, $5)"
       id owner-username seq content expires-at)))

(defun %pastebin-insert-paid (id owner-username seq content)
  (with-db
      (postmodern:execute
       "INSERT INTO pastes (id, owner_username, user_sequence_num, content, expires_at)
        VALUES ($1, $2, $3, $4, NULL)"
       id owner-username seq content)))

(defun %pastebin-rolling-delete (owner-username offset)
  (with-db
      (postmodern:execute
       "DELETE FROM pastes WHERE id IN
          (SELECT id FROM pastes WHERE owner_username = $1 ORDER BY user_sequence_num DESC OFFSET $2)"
       owner-username offset)))

(defun %pastebin-get-content (paste-id)
  (with-db
      (sql-null-to-nil
       (postmodern:query
        "SELECT content FROM pastes WHERE id = $1 AND (expires_at IS NULL OR expires_at > CURRENT_TIMESTAMP)"
        paste-id :single))))

(defun %pastebin-get-user-pastes (owner-username)
  (with-db
      (mapcar #'normalize-row
              (postmodern:query
               "SELECT id,
                       TO_CHAR(created_at, 'YYYY-MM-DD HH24:MI:SS') AS created_at,
                       TO_CHAR(expires_at, 'YYYY-MM-DD HH24:MI:SS') AS expires_at,
                       LEFT(content, 50) AS content_preview
                  FROM pastes
                 WHERE owner_username = $1 AND (expires_at IS NULL OR expires_at > CURRENT_TIMESTAMP)
                 ORDER BY user_sequence_num DESC"
               owner-username :alists))))

(defun %pastebin-delete-manual (paste-id owner-username)
  (with-db
      (postmodern:execute "DELETE FROM pastes WHERE id = $1 AND owner_username = $2" paste-id owner-username)))

(defun %pastebin-downgrade-update (owner-username expires-at)
  (with-db
      (postmodern:execute
       "UPDATE pastes SET expires_at = $2 WHERE owner_username = $1 AND expires_at IS NULL"
       owner-username expires-at)))

(defun %pastebin-reap ()
  (with-db
      (postmodern:execute "DELETE FROM pastes WHERE expires_at < CURRENT_TIMESTAMP")))

(defun %pastebin-call-with-transaction (thunk)
  (with-db (postmodern:with-transaction () (funcall thunk))))

(defun universal-time-to-sql-timestamp (universal-time)
  "Format UNIVERSAL-TIME (as returned by GET-UNIVERSAL-TIME) as a
\"YYYY-MM-DD HH:MM:SS\" string in UTC, suitable for a Postgres TIMESTAMP
column literal. Used so that all paste expiration timestamps are
computed once, in Lisp, rather than by the database server via NOW() --
the Lisp application is the single source of truth for time, which is
what makes ADD-PASTE and DOWNGRADE-USER-TO-FREE deterministically unit
testable against a mock database."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))

(defparameter *pastebin-init-schema* #'%pastebin-init-schema
  "Function of no arguments that creates the pastes table/indexes.")
(defparameter *pastebin-max-sequence* #'%pastebin-max-sequence
  "Function of (OWNER-USERNAME) returning that owner's current max
USER-SEQUENCE-NUM, or NIL if they have no pastes yet.")
(defparameter *pastebin-insert-free* #'%pastebin-insert-free
  "Function of (ID OWNER-USERNAME SEQ CONTENT EXPIRES-AT) inserting a
free-tier paste. EXPIRES-AT must be a SQL timestamp string (see
UNIVERSAL-TIME-TO-SQL-TIMESTAMP), computed by the caller -- the Lisp
application, not the database, is the source of truth for time.")
(defparameter *pastebin-insert-paid* #'%pastebin-insert-paid
  "Function of (ID OWNER-USERNAME SEQ CONTENT) inserting a paid-tier paste
that never expires on its own.")
(defparameter *pastebin-rolling-delete* #'%pastebin-rolling-delete
  "Function of (OWNER-USERNAME OFFSET) deleting all but OWNER-USERNAME's OFFSET
most recent pastes.")
(defparameter *pastebin-get-content* #'%pastebin-get-content
  "Function of (PASTE-ID) returning its content, or NIL if missing/expired.")
(defparameter *pastebin-get-user-pastes* #'%pastebin-get-user-pastes
  "Function of (OWNER-USERNAME) returning a list of alists (one per
non-expired paste owned by OWNER-USERNAME, most recent first), each with
keys :ID, :CREATED-AT, :EXPIRES-AT, and :CONTENT-PREVIEW (the paste's
first 50 characters).")
(defparameter *pastebin-delete-manual* #'%pastebin-delete-manual
  "Function of (PASTE-ID OWNER-USERNAME) deleting that paste if owned by them.")
(defparameter *pastebin-downgrade-update* #'%pastebin-downgrade-update
  "Function of (OWNER-USERNAME EXPIRES-AT) starting the expiry clock on
OWNER-USERNAME's remaining previously-permanent pastes. EXPIRES-AT must be a
SQL timestamp string (see UNIVERSAL-TIME-TO-SQL-TIMESTAMP), computed by
the caller.")
(defparameter *pastebin-reap* #'%pastebin-reap
  "Function of no arguments deleting every expired paste.")
(defparameter *pastebin-call-with-transaction* #'%pastebin-call-with-transaction
  "Function of (THUNK) that invokes THUNK inside a database transaction.")
(defparameter *pastebin-clock* #'get-universal-time
  "Function of no arguments returning the current universal time. ADD-PASTE
and DOWNGRADE-USER-TO-FREE call this (rather than GET-UNIVERSAL-TIME
directly) to compute expiration timestamps, so tests can rebind it to a
controllable fake clock and exercise real expiration behavior (e.g. \"91
days later this paste is gone\") deterministically, without depending on
wall-clock time or a live database.")

(defun init-pastebin-db ()
  (funcall *pastebin-init-schema*))

(defparameter *base62-alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "The 62-character alphabet used to render random paste ids.")

(defun generate-paste-id (&optional (length 16))
  "Return a fresh cryptographically random LENGTH-character Base62 string,
suitable for use as an unguessable public paste id. Random bytes come from
IRONCLAD's secure PRNG (the same source DB-AUTH.LISP uses for password
salts). Each byte is accepted only if it falls below the largest multiple
of 62 that fits in a byte (248); bytes >= 248 are rejected and redrawn, so
every character of *BASE62-ALPHABET* remains exactly equiprobable instead
of a naive MOD 62 subtly favoring the first 256 mod 62 = 8 characters."
  (let ((alphabet-size (length *base62-alphabet*))
        (reject-above (* 62 (floor 256 62))) ; = 248
        (result (make-string length)))
    (dotimes (i length result)
      (loop for byte = (aref (ironclad:random-data 1) 0)
            when (< byte reject-above)
              do (setf (char result i) (char *base62-alphabet* (mod byte alphabet-size)))
                 (return)))))

(defparameter *paste-tier-retention-limits* '(:free 3 :paid 128)
  "Number of most-recent pastes retained per owner, keyed by membership
TIER. Anything beyond this count (ordered by USER-SEQUENCE-NUM) is
dropped by the rolling-window cleanup in ADD-PASTE and
DOWNGRADE-USER-TO-FREE.")

(defun paste-tier-retention-limit (tier)
  (or (getf *paste-tier-retention-limits* tier)
      (error "Unknown paste tier: ~S (expected :FREE or :PAID)" tier)))

(defun add-paste (owner-username content tier)
  "Insert a new paste of CONTENT owned by OWNER-USERNAME, tagged with the
owner's next per-owner sequence number, then enforce the rolling-window
retention policy for TIER. TIER must be :FREE (paste expires 90 days from
now; owner retains at most 3 pastes) or :PAID (paste never expires on its
own; owner retains at most 128 pastes). All three steps run inside a
single transaction. Returns the freshly generated paste id."
  (let ((owner-username (normalize-username owner-username))
        (paste-id (generate-paste-id))
        (retention-limit (paste-tier-retention-limit tier)))
    (funcall *pastebin-call-with-transaction*
             (lambda ()
               (let ((next-seq (1+ (or (funcall *pastebin-max-sequence* owner-username) 0))))
                 (ecase tier
                   (:free (funcall *pastebin-insert-free* paste-id owner-username next-seq content
                                   (universal-time-to-sql-timestamp
                                    (+ (funcall *pastebin-clock*) (* 90 24 60 60)))))
                   (:paid (funcall *pastebin-insert-paid* paste-id owner-username next-seq content)))
                 (funcall *pastebin-rolling-delete* owner-username retention-limit))))
    paste-id))

(defun get-paste (paste-id)
  "Return the CONTENT of PASTE-ID, or NIL if no such paste exists or it has
expired (EXPIRES_AT is non-NULL and no later than now)."
  (funcall *pastebin-get-content* paste-id))

(defun get-user-pastes (owner-username)
  "Return OWNER-USERNAME's non-expired pastes as a list of alists (keys
:ID, :CREATED-AT, :EXPIRES-AT, :CONTENT-PREVIEW), most recently created
first (per USER-SEQUENCE-NUM descending). CONTENT-PREVIEW is truncated
to the paste's first 50 characters, suitable for a dashboard listing
without shipping full paste bodies the caller likely won't display."
  (funcall *pastebin-get-user-pastes* (normalize-username owner-username)))

(defun delete-paste-manual (paste-id owner-username)
  "Delete the paste PASTE-ID, but only if it is owned by OWNER-USERNAME, so a
member can't delete another member's paste by guessing its id."
  (funcall *pastebin-delete-manual* paste-id (normalize-username owner-username)))

(defun downgrade-user-to-free (owner-username)
  "Apply the free-tier retention policy to OWNER-USERNAME after their paid
subscription lapses. Runs in a single transaction: first drop all but
OWNER-USERNAME's 3 most recent pastes, then start the 90-day expiration clock
on whichever remaining pastes were previously permanent (paid, i.e.
EXPIRES_AT IS NULL)."
  (let ((owner-username (normalize-username owner-username)))
    (funcall *pastebin-call-with-transaction*
             (lambda ()
               (funcall *pastebin-rolling-delete* owner-username (paste-tier-retention-limit :free))
               (funcall *pastebin-downgrade-update* owner-username
                        (universal-time-to-sql-timestamp
                         (+ (funcall *pastebin-clock*) (* 90 24 60 60))))))))

(defun reap-expired-pastes ()
  "Permanently delete every paste whose EXPIRES_AT has passed. Intended to
be invoked periodically (e.g. from a cron job or scheduled task) to keep
the table pruned of pastes past their free-tier expiration."
  (funcall *pastebin-reap*))
