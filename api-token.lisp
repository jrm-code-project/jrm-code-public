;;; -*- mode: Lisp; coding: utf-8; -*-

(in-package :jrm-auth)

;;; -- PROGRAMMATIC API: TOKEN EXCHANGE --
;;;
;;; Backs POST /api/v1/auth/token (see API-TOKEN-ROUTES.LISP in the
;;; JRM-CODE-PROJECT package): a caller presents a raw API key (minted by
;;; CREATE-API-KEY, see API-KEYS.LISP) and, if VERIFY-API-KEY accepts it,
;;; receives a short-lived signed JWT it can use to authenticate
;;; subsequent API calls without re-sending the raw key on every request.
;;;
;;; TESTABILITY: following the same DI pattern used throughout this
;;; codebase (see PASTEBIN.LISP's *PASTEBIN-CLOCK* and API-KEYS.LISP's
;;; *API-KEYS-...* specials), the wall clock and the signing secret are
;;; both special variables so tests can rebind them (a fixed clock makes
;;; :IAT/:EXP assertions exact and deterministic; a fixed secret makes
;;; signatures reproducible) without touching real time or real
;;; deployment secrets. GET-USER-TIER's actual database query is likewise
;;; routed through *GET-USER-TIER* so it can be mocked without a live
;;; Postgres connection.

(defparameter *jwt-secret* "[REDACTED_JWT_SECRET]"
  "HMAC-SHA256 signing secret for API-issued JWTs (see GENERATE-API-JWT).
This is a placeholder for local development only -- a real deployment
must rebind this (e.g. from a JWT_SECRET environment variable) before
issuing any token that will be trusted.")

(defparameter *jwt-clock* #'get-universal-time
  "Function of no arguments returning the current time (as a Common Lisp
universal time), used by GENERATE-API-JWT to compute :IAT/:EXP. Defaults
to GET-UNIVERSAL-TIME; tests rebind this to a fixed or fast-forwardable
clock, mirroring PASTEBIN.LISP's *PASTEBIN-CLOCK*.")

(defparameter *api-jwt-lifetime-seconds* 3600
  "How long (in seconds) a token minted by GENERATE-API-JWT remains valid
after issuance.")

(defun unix-time-from-universal-time (universal-time)
  "Convert UNIVERSAL-TIME (as returned by GET-UNIVERSAL-TIME) to Unix time
(seconds since 1970-01-01 UTC), the epoch JWT's numeric date claims
(:IAT/:EXP) are defined in terms of."
  (- universal-time (encode-universal-time 0 0 0 1 1 1970 0)))

(defun generate-api-jwt (owner-username tier)
  "Mint a signed HS256 JWT for OWNER-USERNAME asserting membership TIER
(e.g. \"free\" or \"paid\"), valid for *API-JWT-LIFETIME-SECONDS* (1 hour
by default) from the current time (per *JWT-CLOCK*). Returns the encoded
JWT as a string. Claims: :SUB (OWNER-USERNAME), :TIER (TIER), :IAT (issued-at,
Unix time), :EXP (expiry, Unix time, exactly *API-JWT-LIFETIME-SECONDS*
after :IAT)."
  (let* ((now (unix-time-from-universal-time (funcall *jwt-clock*)))
         (claims (list (cons "sub" owner-username)
                       (cons "tier" tier)
                       (cons "iat" now)
                       (cons "exp" (+ now *api-jwt-lifetime-seconds*)))))
    (jose:encode :hs256 (ironclad:ascii-string-to-byte-array *jwt-secret*) claims)))

(defun %get-user-tier (owner-username)
  "Query the users table directly for OWNER-USERNAME's MEMBERSHIP_TIER
column, returning :FREE if it is NULL, unset, or the base \"CONS\" tier,
and :PAID for any higher tier (\"CADR\", \"LAMBDA\", etc.). Returns NIL if
no such user exists."
  (with-db
      (let ((raw-tier (sql-null-to-nil
                       (postmodern:query "SELECT membership_tier FROM users WHERE username = $1" (normalize-username owner-username) :single))))
        (cond ((null raw-tier) nil)
              ((string-equal raw-tier "CONS") :free)
              (t :paid)))))

(defparameter *get-user-tier* #'%get-user-tier
  "Function of (OWNER-USERNAME) returning that user's tier as :FREE or :PAID
(or NIL if the user does not exist). Rebind in tests to avoid a live
database dependency.")

(defun get-user-tier (owner-username)
  "Return OWNER-USERNAME's membership tier as :FREE or :PAID, or NIL if no
such user exists."
  (funcall *get-user-tier* owner-username))
