;;; -*- Lisp -*-

;;; POST /api/v1/auth/token -- exchanges a raw API key for a short-lived
;;; JWT (see API-TOKEN.LISP's JRM-AUTH:GENERATE-API-JWT). This is the
;;; entry point programmatic API clients use instead of a browser
;;; session: they authenticate once with their long-lived raw API key
;;; (see API-KEY-ROUTES.LISP for how that key is minted/revoked from the
;;; dashboard) and then use the returned Bearer token for up to an hour
;;; before needing to exchange again.

(in-package "JRM-CODE-PROJECT")

(defun json-bad-request-response (message)
  "The standard 400 JSON body returned when the request body is missing,
malformed, or not valid JSON."
  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
  (json-string (list (cons "status" "error") (cons "message" message))))

(defun json-invalid-credentials-response ()
  "The standard 401 JSON body returned when the submitted username/API-key
pair does not verify."
  (setf (hunchentoot:return-code*) hunchentoot:+http-authorization-required+)
  (json-string '(("error" . "Invalid credentials"))))

(defun parse-json-request-body ()
  "Parse the current request's raw POST body as JSON, returning the
decoded alist, or NIL if the body is empty/absent or is not valid JSON.
Callers should treat a NIL return as a 400 Bad Request."
  (let ((raw-body (hunchentoot:raw-post-data :force-text t)))
    (and raw-body
         (plusp (length raw-body))
         (handler-case (cl-json:decode-json-from-string raw-body)
           (error () nil)))))

(defun api-jwt-tier-string (tier)
  "Render the :FREE/:PAID keyword returned by JRM-AUTH:GET-USER-TIER as the
lowercase string GENERATE-API-JWT's :TIER claim expects."
  (ecase tier
    (:free "free")
    (:paid "paid")))

(hunchentoot:define-easy-handler (api-token-exchange-action :uri "/api/v1/auth/token") ()
  (setf (hunchentoot:content-type*) "application/json")
  (case (hunchentoot:request-method*)
    (:post
     (with-json-error-response
       (let ((body (parse-json-request-body)))
         (if (null body)
             (json-bad-request-response "Malformed or missing JSON request body")
             (let ((username (cdr (assoc :username body)))
                   (api-key (cdr (assoc :api--key body))))
               (if (and username api-key (jrm-auth:verify-api-key username api-key))
                   (let* ((tier (api-jwt-tier-string (or (jrm-auth:get-user-tier username) :free)))
                          (token (jrm-auth:generate-api-jwt username tier)))
                     (json-string (list (cons "access_token" token)
                                        (cons "expires_in" 3600)
                                        (cons "token_type" "Bearer"))))
                   (json-invalid-credentials-response))))))) 
    (t
     (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
     (json-string '(("status" . "error") ("message" . "Method not allowed"))))))
