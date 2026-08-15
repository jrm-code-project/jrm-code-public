;;; -*- mode: Lisp; coding: utf-8; -*-

(in-package :jrm-auth)

(defvar *db-params* (list (or (uiop:getenv "DB_NAME") "jrm_db") (or (uiop:getenv "DB_USER") "postgres") (or (uiop:getenv "DB_PASSWORD") "password") (or (uiop:getenv "DB_HOST") "localhost") :pooled-p t)
  "PostgreSQL connection parameters (DB-NAME USER PASS HOST). DB-NAME is
read from the DB_NAME environment variable at load time, falling back
to \"jrm_db\" (the prior hardcoded default) if unset; USER is likewise
read from DB_USER, falling back to \"postgres\" if unset; PASS is read
from DB_PASSWORD, falling back to \"password\" if unset; HOST is read
from DB_HOST, falling back to \"localhost\" if unset -- so every
connection parameter can be overridden for a real deployment without a
code change.")

(defmacro with-db (&body body)
  `(postmodern:with-connection *db-params*
     ,@body))

;; -- CRYPTO (Pure Lisp) --
(defun hash-password (password &optional salt-hex)
  (let* ((salt (if salt-hex
                   (ironclad:hex-string-to-byte-array salt-hex)
                   (ironclad:random-data 16)))
         (salt-hex-str (ironclad:byte-array-to-hex-string salt))
         (pass-bytes (ironclad:ascii-string-to-byte-array password))
         (digest (ironclad:make-digest :sha256)))
    (ironclad:update-digest digest salt)
    (ironclad:update-digest digest pass-bytes)
    (let ((hash-hex (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))
      (format nil "~A$~A" salt-hex-str hash-hex))))

(defun check-password (password stored-hash)
  (let* ((pos (position #\$ stored-hash))
         (salt-hex (subseq stored-hash 0 pos))
         (computed (hash-password password salt-hex)))
    (string= stored-hash computed)))

;; -- USERNAME NORMALIZATION --
;;
;; Usernames are compared case-insensitively: "Prunesquallor" and
;; "prunesquallor" are the same account. Rather than relying on a Postgres
;; extension (e.g. CITEXT), normalization happens entirely on the Lisp
;; side of the boundary -- every function below that accepts a username
;; from a caller downcases it (via NORMALIZE-USERNAME) before it ever
;; touches a SQL query, so the column can remain a plain VARCHAR and the
;; database never needs to know about case-folding.
(defun normalize-username (username)
  "Canonicalize USERNAME for storage/lookup: downcase it. All DB-AUTH
functions that take a username from a caller must funnel it through this
before using it in a query, so lookups and inserts agree on identity
regardless of the case the caller supplied."
  (and username (string-downcase username)))

;; -- SCHEMA MIGRATION HELPER --
;;
;; Renames COLUMN-NAME to NEW-COLUMN-NAME on TABLE-NAME, but only if
;; COLUMN-NAME still exists and NEW-COLUMN-NAME does not -- i.e. it is
;; safe to run unconditionally on every server start:
;;   - a brand-new database (table doesn't exist yet) does nothing here;
;;     the CREATE TABLE IF NOT EXISTS statements that follow create the
;;     table with the new column name directly.
;;   - a database already migrated to the new column name does nothing
;;     (second EXISTS check fails).
;;   - a database still on the old schema (e.g. a production database
;;     from before the email->username rename) gets migrated in place,
;;     preserving all existing rows and foreign-key relationships
;;     (Postgres re-points existing FK constraints at the renamed column
;;     automatically; only the column's name changes, not its identity).
(defun rename-column-if-exists (table-name column-name new-column-name)
  (with-db
      (postmodern:execute
       (format nil
               "DO $$
                BEGIN
                  IF EXISTS (SELECT 1 FROM information_schema.columns
                             WHERE table_name = '~A' AND column_name = '~A')
                     AND NOT EXISTS (SELECT 1 FROM information_schema.columns
                                     WHERE table_name = '~A' AND column_name = '~A')
                  THEN
                    EXECUTE 'ALTER TABLE ~A RENAME COLUMN ~A TO ~A';
                  END IF;
                END $$;"
               table-name column-name table-name new-column-name
               table-name column-name new-column-name))))

;; -- DATABASE SCHEMA (Postgres) --
(defun init-db ()
  (rename-column-if-exists "users" "email" "username")
  (rename-column-if-exists "recovery_codes" "user_email" "username")
  (with-db
      (postmodern:execute "CREATE TABLE IF NOT EXISTS users (
                                       username VARCHAR(255) PRIMARY KEY,
                                       password_hash VARCHAR(255) NOT NULL,
                                       totp_secret VARCHAR(64),
                                       auth_state VARCHAR(32) DEFAULT 'pending_2fa',
                                       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")
    (postmodern:execute "ALTER TABLE users ADD COLUMN IF NOT EXISTS stripe_customer_id VARCHAR(255)")
    (postmodern:execute "ALTER TABLE users ADD COLUMN IF NOT EXISTS stripe_subscription_id VARCHAR(255)")
    (postmodern:execute "ALTER TABLE users ADD COLUMN IF NOT EXISTS subscription_status VARCHAR(50) DEFAULT 'inactive'")
    (postmodern:execute "ALTER TABLE users ADD COLUMN IF NOT EXISTS membership_tier VARCHAR(50) DEFAULT 'CONS'")
    (postmodern:execute "ALTER TABLE users ADD COLUMN IF NOT EXISTS wheel BOOLEAN DEFAULT FALSE")
    (postmodern:execute "CREATE TABLE IF NOT EXISTS recovery_codes (
                                       id SERIAL PRIMARY KEY,
                                       username VARCHAR(255) REFERENCES users(username) ON DELETE CASCADE,
                                       code_hash VARCHAR(255) NOT NULL,
                                       is_used BOOLEAN DEFAULT FALSE)")))

;; -- SIGNUP PROTOCOL ENGINE --
(defun create-user (username plain-password)
  "Returns T if the user was successfully created, NIL if they already exist."
  (let ((username (normalize-username username)))
    (unless (get-user username)
      (let ((hash (hash-password plain-password)))
        (with-db
          (postmodern:execute "INSERT INTO users (username, password_hash) VALUES ($1, $2) ON CONFLICT (username) DO NOTHING"
                       username hash)
          t)))))

(defun sql-null-to-nil (value)
  "Postmodern represents a SQL NULL as the keyword :NULL rather than NIL.
Normalize it to NIL so callers can use ordinary truthiness checks."
  (if (eq value :null) nil value))

(defun normalize-row (row)
  "Normalize every value in an alist ROW (as returned by POSTMODERN:QUERY with
:ALISTS) so that SQL NULLs read back as NIL instead of :NULL."
  (mapcar (lambda (pair) (cons (car pair) (sql-null-to-nil (cdr pair)))) row))

(defstruct (user (:conc-name user-) (:copier nil))
  "An immutable snapshot of one row of the users table. Constructed only by
ROW->USER at the database boundary -- callers should never build one of
these by hand, and should always go through the named accessors
(USER-USERNAME, USER-MEMBERSHIP-TIER, etc.) rather than re-deriving fields
from a raw query row."
  username
  password-hash
  totp-secret
  auth-state
  stripe-customer-id
  stripe-subscription-id
  subscription-status
  membership-tier
  wheel-p)

(defun row->user (row)
  "Convert a normalized users-table alist ROW (see NORMALIZE-ROW) into an
immutable USER struct."
  (make-user :username (cdr (assoc :username row))
             :password-hash (cdr (assoc :password-hash row))
             :totp-secret (cdr (assoc :totp-secret row))
             :auth-state (cdr (assoc :auth-state row))
             :stripe-customer-id (cdr (assoc :stripe-customer-id row))
             :stripe-subscription-id (cdr (assoc :stripe-subscription-id row))
             :subscription-status (cdr (assoc :subscription-status row))
             :membership-tier (cdr (assoc :membership-tier row))
             :wheel-p (cdr (assoc :wheel row))))

(defparameter *user-columns*
  "username, password_hash, totp_secret, auth_state, stripe_customer_id, stripe_subscription_id, subscription_status, membership_tier, wheel"
  "The standard column list selected whenever a users-table row is read into
a USER struct.")

(defmacro query-users (query &rest args)
  "Run QUERY (a SELECT against the users table using *USER-COLUMNS*) with
ARGS as its parameters, returning a list of immutable USER structs rather
than raw alist rows. A macro (not a function) because POSTMODERN:QUERY
itself is a macro that inspects its query argument at macroexpansion
time."
  `(mapcar (lambda (row) (row->user (normalize-row row)))
           (postmodern:query ,query ,@args :alists)))

(defun get-user (username)
  (let ((username (normalize-username username)))
    (with-db
        (query-users (format nil "SELECT ~A FROM users WHERE username = $1" *user-columns*) username))))

(defun activate-user-2fa (username secret)
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "UPDATE users SET totp_secret = $1, auth_state = 'active' WHERE username = $2"
                            secret username))))

(defun random-string (length)
  "Return a random LENGTH-character string drawn from an unambiguous
uppercase-alphanumeric alphabet, built as a SERIES scan/collect pipeline
rather than a character-by-character SETF loop (FUNCTIONAL_REFACTOR.md
Phase 7)."
  (let ((chars "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"))
    (series:collect 'string
      (series:mapping ((i (series:scan-range :below length)))
        (declare (ignore i))
        (char chars (random (length chars)))))))

(defun generate-recovery-codes (username)
  (let ((username (normalize-username username))
        (codes (series:collect
                (series:mapping ((i (series:scan-range :below 10)))
                  (declare (ignore i))
                  (format nil "~A-~A" (random-string 4) (random-string 4))))))
    (with-db
        (dolist (code codes)
          (postmodern:execute "INSERT INTO recovery_codes (username, code_hash) VALUES ($1, $2)"
                              username (hash-password code))))
    codes))

(defun verify-login (username plain-password)
  (let ((user (car (get-user username))))
    (if user
        (let ((hash (user-password-hash user)))
          (if (check-password plain-password hash)
              user
              nil))
        nil)))

(defun delete-user (username)
  "Completely obliterate the user from the database. Recovery codes cascade automatically."
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "DELETE FROM users WHERE username = $1" username))))

(defun update-user-password (username new-plain-password)
  "Replace USERNAME's stored password hash with a freshly-salted hash of
NEW-PLAIN-PASSWORD."
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "UPDATE users SET password_hash = $1 WHERE username = $2"
                            (hash-password new-plain-password) username))))

(defun verify-recovery-code (username code)
  "Verifies and consumes a recovery code for a user. Returns T if valid and unused, NIL otherwise."
  (let* ((username (normalize-username username))
         (db-codes (with-db
                      (postmodern:query "SELECT id, code_hash FROM recovery_codes WHERE username = $1 AND is_used = FALSE" username :alists))))
    (dolist (db-code db-codes)
      (let ((id (cdr (assoc :id db-code)))
            (stored-hash (cdr (assoc :code-hash db-code))))
        (when (check-password code stored-hash)
          (with-db
              (postmodern:execute "UPDATE recovery_codes SET is_used = TRUE WHERE id = $1" id))
          (return-from verify-recovery-code t))))
    nil))

(defun delete-recovery-codes (username)
  "Deletes all recovery codes for a user from the database."
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "DELETE FROM recovery_codes WHERE username = $1" username))))

(defun update-user-stripe-customer (username customer-id)
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "UPDATE users SET stripe_customer_id = $1 WHERE username = $2" customer-id username))))

(defun update-user-subscription (username customer-id subscription-id status tier)
  "Record a (new or changed) Stripe subscription for USERNAME, including the
membership TIER (\"CADR\" or \"LAMBDA\") the customer's current price maps to."
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "UPDATE users SET stripe_customer_id = $1, stripe_subscription_id = $2, subscription_status = $3, membership_tier = $4 WHERE username = $5"
                            customer-id subscription-id status tier username))))

(defun update-user-tier-status (username tier status)
  "Update the membership TIER and subscription STATUS for USERNAME without
touching the stored Stripe customer/subscription IDs. Used when an existing
subscription changes (e.g. a Billing Portal upgrade/downgrade or renewal)."
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "UPDATE users SET membership_tier = $1, subscription_status = $2 WHERE username = $3"
                            tier status username))))

(defun cancel-user-subscription (username)
  "Downgrade USERNAME back to the free CONS tier after their Stripe subscription
has ended (cancellation, with any pro-rated refund handled entirely by Stripe)."
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "UPDATE users SET membership_tier = 'CONS', subscription_status = 'canceled', stripe_subscription_id = NULL WHERE username = $1"
                            username))))

(defun get-user-by-customer (customer-id)
  (with-db
      (query-users (format nil "SELECT ~A FROM users WHERE stripe_customer_id = $1" *user-columns*) customer-id)))

(defun set-user-wheel (username wheel-p)
  "Turn the WHEEL bit on (T) or off (NIL) for USERNAME. Wheels are super users
with access to special pages, regardless of their membership tier."
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "UPDATE users SET wheel = $1 WHERE username = $2" (and wheel-p t) username))))

(defun wheel-p (username)
  "Return T if USERNAME currently has the wheel bit set, NIL otherwise."
  (let ((user (car (get-user username))))
    (and user (user-wheel-p user) t)))

(defun list-users (&key (search "") (limit 20) (offset 0))
  "Return up to LIMIT users (alphabetically by username) starting at OFFSET,
optionally filtered to usernames containing SEARCH (case-insensitive
substring match). Used to paginate/search the wheel-only membership admin
page."
  (with-db
      (query-users
       (format nil "SELECT ~A FROM users WHERE username ILIKE $1 ORDER BY username ASC LIMIT $2 OFFSET $3" *user-columns*)
       (format nil "%~A%" search) limit offset)))

(defun count-users (&key (search ""))
  "Return the total number of users matching SEARCH (see LIST-USERS), for
pagination."
  (with-db
      (cdr (assoc :count
                  (car (postmodern:query
                        "SELECT COUNT(*) AS count FROM users WHERE username ILIKE $1"
                        (format nil "%~A%" search) :alists))))))

(defun admin-set-tier (username tier)
  "Directly set USERNAME's membership TIER without touching subscription_status
or any Stripe identifiers -- used by the wheel-only admin page to grant or
adjust membership independent of payment (e.g. comping a tier, or fixing
up membership data after a manual Stripe-side correction)."
  (let ((username (normalize-username username)))
    (with-db
        (postmodern:execute "UPDATE users SET membership_tier = $1 WHERE username = $2" tier username))))

(defun admin-create-user (username plain-password tier)
  "Create a new user account (or, if USERNAME already exists, just apply TIER
to it) with membership TIER granted directly, no payment required. Returns T
if a new account was created, NIL if the username already existed (its tier
is still updated in that case). New accounts still go through the normal
2FA setup flow on first login, same as self-service signups."
  (let ((username (normalize-username username)))
    (if (get-user username)
        (progn (admin-set-tier username tier) nil)
        (let ((hash (hash-password plain-password)))
          (with-db
              (postmodern:execute "INSERT INTO users (username, password_hash, membership_tier) VALUES ($1, $2, $3) ON CONFLICT (username) DO NOTHING"
                                  username hash tier))
          t))))
