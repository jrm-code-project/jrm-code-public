;;; -*- mode: Lisp; coding: utf-8; -*-

(in-package :jrm-auth)

;;; -- IN-MEMORY TOKEN BUCKET RATE LIMITER --
;;;
;;; A simple in-process rate limiter shared by every Hunchentoot worker
;;; thread. Each distinct KEY (an IP address for anonymous callers, or an
;;; authenticated user's username -- see WITH-API-AUTH-AND-RATE-LIMIT in
;;; API-MIDDLEWARE.LISP) gets its own TOKEN-BUCKET that refills at a
;;; constant rate and is drained by one token per allowed request. Being
;;; in-memory, this resets on restart and does not coordinate across
;;; multiple server processes -- adequate for a single-process deployment,
;;; but would need a shared store (e.g. Redis) to scale horizontally.

(defstruct token-bucket
  "TOKENS is the number of requests currently available for KEY (a float
or rational, since fractional tokens accumulate between refills).
LAST-UPDATE is the universal time TOKENS was last topped up."
  (tokens 0.0d0 :type real)
  (last-update (get-universal-time) :type integer))

(defparameter *rate-limits* (make-hash-table :test 'equal)
  "Maps a rate-limit KEY (a string: an IP address or a username) to its
TOKEN-BUCKET. Guarded by *RATE-LIMITS-LOCK* -- never read or write this
table without holding that lock.")

(defparameter *rate-limits-lock* (sb-thread:make-mutex :name "rate-limits")
  "Mutex serializing all access to *RATE-LIMITS*, since Hunchentoot
serves requests from a pool of worker threads.")

(defun check-rate-limit (key max-tokens refill-rate-per-second)
  "Consume one token from KEY's bucket, refilling it first at
REFILL-RATE-PER-SECOND tokens/second for however long has elapsed since
its last update (creating a fresh, full bucket of MAX-TOKENS if KEY has
never been seen before), capped at MAX-TOKENS. Returns T and deducts one
token if at least 1.0 token was available; returns NIL (and leaves the
bucket untouched below 1.0) if the caller should be rate limited."
  (sb-thread:with-mutex (*rate-limits-lock*)
    (let* ((now (get-universal-time))
           (bucket (or (gethash key *rate-limits*)
                       (setf (gethash key *rate-limits*)
                             (make-token-bucket :tokens max-tokens :last-update now))))
           (elapsed (max 0 (- now (token-bucket-last-update bucket))))
           (refilled (min max-tokens
                          (+ (token-bucket-tokens bucket) (* elapsed refill-rate-per-second)))))
      (setf (token-bucket-last-update bucket) now)
      (cond ((>= refilled 1.0d0)
             (setf (token-bucket-tokens bucket) (- refilled 1.0d0))
             t)
            (t
             (setf (token-bucket-tokens bucket) refilled)
             nil)))))

;;; -- JWT VERIFICATION (API MIDDLEWARE) --
;;;
;;; Verifies tokens minted by GENERATE-API-JWT (see API-TOKEN.LISP),
;;; reusing that file's *JWT-SECRET* (the HMAC signing key) and
;;; *JWT-CLOCK* (a mockable "now" function) so tests can exercise
;;; expiry/signature checks deterministically.

(defun verify-and-extract-jwt (token-string)
  "Verify TOKEN-STRING as an HS256 JWT signed with *JWT-SECRET*, decoding
it with the `jose' library. Returns the decoded claims alist if the
signature checks out AND the :EXP claim is strictly greater than the
current time (per *JWT-CLOCK*, converted to Unix time to match
GENERATE-API-JWT's numeric date claims). Returns NIL for any failure --
a bad signature, malformed token, missing/unparseable :EXP claim, or an
expired token -- rather than letting `jose' signal a condition out to
the caller."
  (handler-case
      (let* ((claims (jose:decode *jwt-algorithm*
                                  (ironclad:ascii-string-to-byte-array *jwt-secret*)
                                  token-string))
             (exp (cdr (assoc "exp" claims :test #'string=)))
             (now (unix-time-from-universal-time (funcall *jwt-clock*))))
        (if (and (realp exp) (> exp now))
            claims
            nil))
    (error () nil)))
