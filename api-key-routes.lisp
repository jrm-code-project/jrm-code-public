;;; -*- Lisp -*-

;;; JSON/fetch-based routes backing the dashboard's "API Keys" panel.
;;; Like /api/login (see CSRF.LISP's header comment), these are session-
;;; authenticated JSON endpoints rather than <form>-submitted pages, so
;;; they are intentionally exempt from the CSRF-token dance used by the
;;; HTML forms elsewhere in the app: a cross-site page cannot read the
;;; session cookie's value, and every mutation here still requires an
;;; existing authenticated session.

;;; --- ERROR-HANDLING POLICY (see TECHNICAL_DEBT.md item 7) ---
;;;
;;; This codebase uses three deliberate, documented error-handling
;;; strategies rather than one uniform rule, because the right behavior
;;; genuinely differs by call site. Every HANDLER-CASE/IGNORE-ERRORS in
;;; the app should map to one of these:
;;;
;;; 1. BEST-EFFORT STARTUP STEP (SERVER.LISP's RUN-STARTUP-STEP): a
;;;    subsystem that isn't strictly required for the site to serve any
;;;    traffic at all (e.g. the Stripe catalog sync). Startup proceeds
;;;    past a failure, but the failure is ALWAYS logged (via FORMAT T)
;;;    and recorded in *STARTUP-FAILURES* so it's visible in the final
;;;    "Server is now running" summary line -- never silently swallowed.
;;;
;;; 2. REQUEST-SCOPED RECOVERY TO AN HTTP ERROR RESPONSE (this file's
;;;    WITH-JSON-ERROR-RESPONSE; BILLING.LISP's STRIPE-WEBHOOK-HANDLER;
;;;    STRIPE.LISP's outbound HTTP calls, which convert a failure into an
;;;    (ERR reason) outcome value instead of a raised condition -- see
;;;    OUTCOME.LISP): an unhandled error during one request/one outbound
;;;    call must not crash the worker thread or leak Hunchentoot's HTML
;;;    error page to a JSON-consuming client. The condition is always
;;;    logged (FORMAT T) AND converted to a specific status code + JSON
;;;    body the caller can parse -- logging and a well-formed response
;;;    are both required, not an either/or.
;;;
;;; 3. PURE VALIDATION, NO LOGGING NEEDED (ADMIN.LISP's
;;;    COMPUTE-ADMIN-PAGINATION using IGNORE-ERRORS around PARSE-INTEGER;
;;;    API-PASTES-ROUTES.LISP's header parsing): a malformed *user input*
;;;    value (not a subsystem failure) that has an obvious, harmless
;;;    default (e.g. an unparseable `page' query parameter just means
;;;    "page 0"). These are expected, routine inputs, not operational
;;;    anomalies -- logging every bad query parameter would be noise, not
;;;    signal, so IGNORE-ERRORS with a sensible default is correct here
;;;    and deliberately does not log.
;;;
;;; The rule of thumb: if the failure indicates something an operator
;;; would want to know about (a subsystem is down, an unexpected
;;; exception occurred), it must be logged. If it's just a malformed
;;; piece of routine user input with an obvious fallback, a silent
;;; IGNORE-ERRORS is fine. Category 2 is the most common shape for
;;; request handlers and is implemented once here as
;;; WITH-JSON-ERROR-RESPONSE so every JSON API route shares the same
;;; behavior rather than each hand-rolling its own HANDLER-CASE.

(in-package "JRM-CODE-PROJECT")

(defun json-string (value)
  "Encode VALUE (an alist, string, number, etc.) as a JSON string using
CL-JSON, the JSON library already used elsewhere in this application
(see BILLING.LISP's Stripe webhook handling)."
  (cl-json:encode-json-to-string value))

(defun require-json-session-user ()
  "Return the authenticated user's username from the current Hunchentoot
session, or NIL if there isn't one. Callers must check for NIL and
respond with 401 + a JSON error body before doing anything else -- this
function only inspects state, it never writes a response itself."
  (hunchentoot:session-value :authenticated-user))

(defun json-unauthorized-response ()
  "The standard 401 JSON body returned by the API-key routes (and other
JSON endpoints) when there is no authenticated session."
  (setf (hunchentoot:return-code*) hunchentoot:+http-authorization-required+)
  (json-string '(("status" . "error") ("message" . "Not authenticated"))))

(defmacro with-json-error-response (&body body)
  "Run BODY (already inside a JSON-content-typed handler) and, if any
unhandled Lisp error escapes it (e.g. a database error because the
api_keys table hasn't been initialized yet), catch it and respond with
a 500 + JSON error body instead of letting Hunchentoot's default HTML
error page leak through -- fetch()'s response.json() would otherwise
fail to parse that HTML and surface a misleading generic network error
in the browser.

Also logs the condition report via FORMAT T, matching every other
unhandled-error site in the codebase (STRIPE.LISP, BILLING.LISP's
WEBHOOK-ERROR-RESPONSE, SERVER.LISP's RUN-STARTUP-STEP): an unexpected
500 here should never be silent -- see the error-handling policy
documented at the top of this file."
  `(handler-case (progn ,@body)
     (error (e)
       (format t ";; Warning: Unhandled error in JSON API handler: ~A~%" e)
       (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
       (json-string (list (cons "status" "error") (cons "message" (format nil "~A" e)))))))

(hunchentoot:define-easy-handler (generate-api-key-action :uri "/api/internal/generate-api-key") ()
  (setf (hunchentoot:content-type*) "application/json")
  (case (hunchentoot:request-method*)
    (:post
     (with-json-error-response
       (let ((owner-username (require-json-session-user)))
         (if owner-username
             (let ((raw-key (jrm-auth:create-api-key owner-username)))
               (json-string (list (cons "status" "success") (cons "raw_key" raw-key))))
             (json-unauthorized-response)))))
    (t
     (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
     (json-string '(("status" . "error") ("message" . "Method not allowed"))))))

(hunchentoot:define-easy-handler (revoke-api-key-action :uri "/api/internal/revoke-api-key") ()
  (setf (hunchentoot:content-type*) "application/json")
  (case (hunchentoot:request-method*)
    (:post
     (with-json-error-response
       (let ((owner-username (require-json-session-user)))
         (if owner-username
             (progn
               (jrm-auth:revoke-api-key owner-username)
               (json-string '(("status" . "success"))))
             (json-unauthorized-response)))))
    (t
     (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
     (json-string '(("status" . "error") ("message" . "Method not allowed"))))))
