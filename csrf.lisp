;;; -*- Lisp -*-

(in-package "JRM-CODE-PROJECT")

;; --- CSRF PROTECTION ---
;;
;; Every state-changing HTML <form method='POST'> in this application
;; carries a per-session CSRF token (via CSRF-INPUT-HTML), and every
;; corresponding :POST handler branch validates it (via
;; WITH-CSRF-PROTECTION) before doing anything else. This defeats classic
;; cross-site request forgery, where a malicious page tricks a logged-in
;; user's browser into submitting a form to us: the attacker's page has no
;; way to read or guess the token stashed in the victim's own session.
;;
;; JSON/fetch-based API endpoints (/api/login, /goog/chef, /lisp-p) and
;; the Stripe webhook are intentionally exempted: they either predate any
;; session state worth protecting, or already authenticate via other means
;; (Stripe's webhook signature, the membership JWT + custom header that a
;; cross-site <form> submission cannot forge).

(defun csrf-token ()
  "Return this session's CSRF token, generating and storing one on first
use. Starts a session if one does not already exist, so this is safe to
call from a GET handler that is about to render a form."
  (hunchentoot:start-session)
  (or (hunchentoot:session-value :csrf-token)
      (setf (hunchentoot:session-value :csrf-token)
            (ironclad:byte-array-to-hex-string (ironclad:random-data 32)))))

(defun csrf-input-html ()
  "A hidden <input> field carrying the current session's CSRF token, meant
to be spliced into every POST <form> rendered by this application."
  (format nil "<input type='hidden' name='csrf-token' value='~A'>" (csrf-token)))

(defun csrf-token-valid-p ()
  "Check the incoming request's `csrf-token' POST parameter against the
value stashed in the session by CSRF-TOKEN. Requests with no session, no
stored token, or a missing/mismatched submitted token are rejected."
  (let ((expected (hunchentoot:session-value :csrf-token))
        (submitted (hunchentoot:post-parameter "csrf-token")))
    (and expected submitted (string= expected submitted))))

(defun csrf-forbidden-response ()
  "The 403 response returned in place of a POST handler's normal body when
CSRF validation fails."
  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
  "<html><head><style>body { font-family: sans-serif; background: #111; color: #f00; padding: 2rem; }</style></head><body><h2>403 Forbidden</h2><p>Invalid or missing CSRF token. Please reload the page and try again.</p></body></html>")

(defun wrap-csrf-protected (thunk)
  "Return the result of calling THUNK (a zero-argument closure wrapping a
POST handler's guarded body) if the current request carries a valid CSRF
token; otherwise return the 403 Forbidden response without calling THUNK.
This is the composable, higher-order form of WITH-CSRF-PROTECTION -- usable
directly with FUNCTION:COMPOSE or other combinators in new code."
  (if (csrf-token-valid-p)
      (funcall thunk)
      (csrf-forbidden-response)))

(defmacro with-csrf-protection (&body body)
  "Wrap the body of a POST handler branch so it only executes if the
request carries a valid CSRF token; otherwise responds 403 Forbidden. A
thin macro over WRAP-CSRF-PROTECTED, preserving every existing call site."
  `(wrap-csrf-protected (lambda () ,@body)))

;; --- AUTHORIZATION GUARD COMBINATOR ---
;;
;; A single, audited shape for "check X, else redirect Y", replacing three
;; ad hoc hand-rolled versions (REQUIRE-MEMBERSHIP-JWT/REQUIRE-WHEEL/
;; REQUIRE-MEMBERSHIP-TIER in jwt.lisp, and REQUIRE-SESSION-WHEEL in
;; admin.lisp). See FUNCTIONAL_REFACTOR.md Phase 3.

(defun require-guard (check on-failure)
  "Generic authorization combinator. CHECK is a zero-argument thunk that
returns a non-NIL success value (e.g. JWT claims, or a wheel's username) or
NIL to indicate failure. ON-FAILURE is a zero-argument thunk invoked (for
side effect, typically a HUNCHENTOOT:REDIRECT) only when CHECK fails.
Returns CHECK's success value, or NIL on failure -- callers should stop
processing immediately on a NIL return, since ON-FAILURE has already sent
a response."
  (or (funcall check)
      (progn (funcall on-failure) nil)))
