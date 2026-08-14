;;; -*- Lisp -*-

;;; JSON/fetch-based routes backing the dashboard's "API Keys" panel.
;;; Like /api/login (see CSRF.LISP's header comment), these are session-
;;; authenticated JSON endpoints rather than <form>-submitted pages, so
;;; they are intentionally exempt from the CSRF-token dance used by the
;;; HTML forms elsewhere in the app: a cross-site page cannot read the
;;; session cookie's value, and every mutation here still requires an
;;; existing authenticated session.

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
in the browser."
  `(handler-case (progn ,@body)
     (error (e)
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
