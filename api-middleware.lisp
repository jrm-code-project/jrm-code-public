;;; -*- Lisp -*-

;;; Rate limiting + JWT authentication middleware for programmatic API
;;; routes (e.g. the future /api/v1/... namespace). Wraps a Hunchentoot
;;; handler body so that:
;;;   - anonymous callers (no Authorization header, or a header that
;;;     isn't a Bearer token) are rate-limited by IP address at a modest
;;;     10 requests/minute, and
;;;   - authenticated callers (a valid Bearer JWT, see
;;;     JRM-AUTH:VERIFY-AND-EXTRACT-JWT in RATE-LIMIT.LISP) are
;;;     rate-limited by their username at a more generous 100
;;;     requests/minute, with their username and tier bound to *API-USER*
;;;     and *API-TIER* for BODY to use.

(in-package "JRM-CODE-PROJECT")

(defvar *api-user* nil
  "Bound by WITH-API-AUTH-AND-RATE-LIMIT to the authenticated caller's
username (the JWT's :SUB claim) for the duration of BODY, or NIL for an
anonymous (IP-rate-limited) request.")

(defvar *api-tier* nil
  "Bound by WITH-API-AUTH-AND-RATE-LIMIT to the authenticated caller's
membership tier (the JWT's :TIER claim, as a string) for the duration of
BODY, or NIL for an anonymous request.")

(defparameter *anonymous-rate-limit-max-tokens* 10
  "Token bucket capacity (and effectively the burst size) for callers
with no valid Bearer token, keyed by IP address.")

(defparameter *anonymous-rate-limit-refill-per-second* (/ 10.0d0 60.0d0)
  "Refill rate, in tokens/second, for anonymous callers -- 10 requests
per minute.")

(defparameter *authenticated-rate-limit-max-tokens* 100
  "Token bucket capacity for callers presenting a valid Bearer JWT, keyed
by their authenticated username.")

(defparameter *authenticated-rate-limit-refill-per-second* (/ 100.0d0 60.0d0)
  "Refill rate, in tokens/second, for authenticated callers -- 100
requests per minute.")

(defun too-many-requests-response ()
  "The standard 429 JSON body returned when CHECK-RATE-LIMIT denies a
request, for either the anonymous or authenticated path."
  (setf (hunchentoot:return-code*) hunchentoot:+http-too-many-requests+)
  (setf (hunchentoot:content-type*) "application/json")
  (json-string '(("error" . "Too Many Requests"))))

(defun unauthorized-or-expired-token-response ()
  "The standard 401 JSON body returned when a Bearer token is present but
JRM-AUTH:VERIFY-AND-EXTRACT-JWT rejects it (bad signature, malformed
token, or expired)."
  (setf (hunchentoot:return-code*) hunchentoot:+http-authorization-required+)
  (setf (hunchentoot:content-type*) "application/json")
  (json-string '(("error" . "Unauthorized or expired token"))))

(defun bearer-token-from-header-value (header)
  "Pure helper: return the token substring of HEADER (an Authorization
header value, or NIL) if it is a `Bearer <token>' header, or NIL if
HEADER is NIL or uses a different scheme. Factored out of
BEARER-TOKEN-FROM-AUTHORIZATION-HEADER so it can be unit-tested without
a live Hunchentoot request context."
  (when (and header (>= (length header) 7) (string-equal header "Bearer " :end1 7))
    (string-trim " " (subseq header 7))))

(defun bearer-token-from-authorization-header ()
  "Return the raw token substring of the current request's Authorization
header if it is present and is a `Bearer <token>' header, or NIL if the
header is missing or uses a different scheme."
  (bearer-token-from-header-value (hunchentoot:header-in* :authorization)))

(defmacro with-api-auth-and-rate-limit (&body body)
  "Wrap BODY (a Hunchentoot handler's response-producing forms) with rate
limiting and, when a Bearer token is presented, JWT authentication.

With no Bearer token (or a non-Bearer Authorization header), BODY runs
under a 10-requests/minute limit keyed by the caller's IP address
\(HUNCHENTOOT:REAL-REMOTE-ADDR\); a 429 JSON error is returned instead of
running BODY once that budget is exhausted.

With a Bearer token, it is verified via JRM-AUTH:VERIFY-AND-EXTRACT-JWT;
an invalid or expired token short-circuits with a 401 JSON error without
running BODY. A valid token is rate-limited at a more generous 100
requests/minute keyed by the token's \:SUB (username) claim; once verified
and within budget, BODY runs with *API-USER* and *API-TIER* bound to the
token's :SUB and :TIER claims."
  `(let ((%bearer-token (bearer-token-from-authorization-header)))
     (if %bearer-token
         (let ((%claims (jrm-auth:verify-and-extract-jwt %bearer-token)))
           (if (null %claims)
               (unauthorized-or-expired-token-response)
               (let ((*api-user* (cdr (assoc "sub" %claims :test #'string=)))
                     (*api-tier* (cdr (assoc "tier" %claims :test #'string=))))
                 (if (jrm-auth:check-rate-limit *api-user*
                                                 *authenticated-rate-limit-max-tokens*
                                                 *authenticated-rate-limit-refill-per-second*)
                     (progn ,@body)
                     (too-many-requests-response)))))
         (if (jrm-auth:check-rate-limit (hunchentoot:real-remote-addr)
                                         *anonymous-rate-limit-max-tokens*
                                         *anonymous-rate-limit-refill-per-second*)
             (progn ,@body)
             (too-many-requests-response)))))
