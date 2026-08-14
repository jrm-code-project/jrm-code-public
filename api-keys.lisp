;;; -*- mode: Lisp; coding: utf-8; -*-

(in-package :jrm-auth)

;;; -- API KEY DATA ACCESS LAYER --
;;;
;;; Backs a programmatic API with a single-active-key-per-member
;;; `api_keys` table. OWNER_USERNAME is the PRIMARY KEY, so a member has at
;;; most one live key: generating a new one overwrites (upserts) the old
;;; row rather than accumulating rows, which is exactly the semantics we
;;; want ("issuing a new key revokes the old one").
;;;
;;; SECURITY: the raw API key is never stored. CREATE-API-KEY hashes it
;;; with the same salted-SHA-256 HASH-PASSWORD used for member passwords
;;; (see DB-AUTH.LISP) before writing it to KEY_HASH, and returns the raw
;;; value to the caller exactly once -- there is no way to recover it from
;;; the database afterward. VERIFY-API-KEY checks a raw key using
;;; CHECK-PASSWORD, the same constant-structure comparison used for login.
;;;
;;; TESTABILITY: as in PASTEBIN.LISP, every actual database touch is
;;; funneled through one of the *API-KEYS-...* special variables defined
;;; below, so tests can rebind them to an in-memory mock without a live
;;; Postgres connection.

(defun %api-keys-init-schema ()
  (rename-column-if-exists "api_keys" "owner_email" "owner_username")
  (with-db
      (postmodern:execute "CREATE TABLE IF NOT EXISTS api_keys (
                                       owner_username VARCHAR(255) PRIMARY KEY,
                                       key_hash VARCHAR(255) NOT NULL,
                                       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

                                       FOREIGN KEY (owner_username) REFERENCES users(username) ON DELETE CASCADE)")))

(defun %api-keys-upsert (owner-username key-hash)
  (with-db
      (postmodern:execute
       "INSERT INTO api_keys (owner_username, key_hash) VALUES ($1, $2)
        ON CONFLICT (owner_username) DO UPDATE SET key_hash = EXCLUDED.key_hash, created_at = CURRENT_TIMESTAMP"
       owner-username key-hash)))

(defun %api-keys-get-hash (owner-username)
  (with-db
      (sql-null-to-nil
       (postmodern:query "SELECT key_hash FROM api_keys WHERE owner_username = $1" owner-username :single))))

(defun %api-keys-revoke (owner-username)
  (with-db
      (postmodern:execute "DELETE FROM api_keys WHERE owner_username = $1" owner-username)))

(defparameter *api-keys-init-schema* #'%api-keys-init-schema
  "Function of no arguments that creates the api_keys table.")
(defparameter *api-keys-upsert* #'%api-keys-upsert
  "Function of (OWNER-USERNAME KEY-HASH) storing KEY-HASH as OWNER-USERNAME's
sole active API key, replacing any previous one.")
(defparameter *api-keys-get-hash* #'%api-keys-get-hash
  "Function of (OWNER-USERNAME) returning that owner's stored KEY-HASH, or
NIL if they have no active API key.")
(defparameter *api-keys-revoke* #'%api-keys-revoke
  "Function of (OWNER-USERNAME) deleting that owner's active API key, if any.")

(defun init-api-keys-db ()
  (funcall *api-keys-init-schema*))

(defconstant +api-key-prefix+
  (if (boundp '+api-key-prefix+) (symbol-value '+api-key-prefix+) "jrm_live_")
  "Prefix prepended to every raw API key, so keys are recognizable at a
glance (and greppable/rotatable en masse) without decoding them. Guarded
so reloading this file doesn't trigger a DEFCONSTANT redefinition error
-- comparing the old and new string values with DEFCONSTANT's default
EQL test always fails even when the strings are EQUAL, since string
literals aren't guaranteed to be coalesced across compilations.")

(defun generate-raw-api-key ()
  "Return a fresh raw API key: +API-KEY-PREFIX+ followed by 32
cryptographically random bytes (from IRONCLAD's secure PRNG -- the same
source DB-AUTH.LISP uses for password salts) rendered as lowercase hex.
This raw value is only ever available to the caller of CREATE-API-KEY;
the database only ever stores its hash."
  (concatenate 'string +api-key-prefix+
               (ironclad:byte-array-to-hex-string (ironclad:random-data 32))))

(defun create-api-key (owner-username)
  "Generate a fresh raw API key for OWNER-USERNAME, store only its
CHECK-PASSWORD-compatible hash (replacing any previous key for
OWNER-USERNAME, since OWNER_USERNAME is the table's primary key), and return
the RAW key. This is the only time the raw key is ever available --
losing it means OWNER-USERNAME must generate a new one."
  (let* ((owner-username (normalize-username owner-username))
         (raw-key (generate-raw-api-key))
         (key-hash (hash-password raw-key)))
    (funcall *api-keys-upsert* owner-username key-hash)
    raw-key))

(defun verify-api-key (owner-username raw-key)
  "Return T if RAW-KEY is OWNER-USERNAME's current active API key, NIL if it
does not match or OWNER-USERNAME has no active key."
  (let* ((owner-username (normalize-username owner-username))
         (key-hash (funcall *api-keys-get-hash* owner-username)))
    (and key-hash (check-password raw-key key-hash) t)))

(defun revoke-api-key (owner-username)
  "Delete OWNER-USERNAME's active API key, if any, so VERIFY-API-KEY will no
longer accept it."
  (funcall *api-keys-revoke* (normalize-username owner-username)))
