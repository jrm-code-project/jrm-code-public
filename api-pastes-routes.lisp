;;; -*- Lisp -*-

;;; RESTful JSON routes exposing the Pastebin business logic (see
;;; PASTEBIN.LISP's ADD-PASTE/GET-PASTE/DELETE-PASTE-MANUAL) over the
;;; programmatic API, all multiplexed through a single Hunchentoot
;;; handler on /api/v1/pastes (since DEFINE-EASY-HANDLER dispatches by
;;; URI, not by HTTP method) that dispatches on REQUEST-METHOD*:
;;;
;;;   POST   -- create a paste; requires a Bearer JWT (see
;;;             API-MIDDLEWARE.LISP's WITH-API-AUTH-AND-RATE-LIMIT),
;;;             since it's created on behalf of *API-USER*.
;;;   GET    -- read a paste by ?id=; left public/unauthenticated, since
;;;             paste content is meant to be shared/read by anyone who
;;;             has the id, the same way a traditional pastebin link
;;;             works.
;;;   DELETE -- delete a paste by ?id=; requires a Bearer JWT, since only
;;;             the owning *API-USER* may delete their own paste.

(in-package "JRM-CODE-PROJECT")

(defun api-tier-keyword (tier-string)
  "Convert the TIER-STRING bound to *API-TIER* by WITH-API-AUTH-AND-RATE-LIMIT
(the JWT's \"tier\" claim, e.g. \"free\" or \"paid\") back into the :FREE/
:PAID keyword ADD-PASTE expects. Defaults to :FREE for an unrecognized or
missing tier, so a paste is never accidentally minted with unlimited
retention."
  (cond ((string-equal tier-string "paid") :paid)
        (t :free)))

(defun paste-not-found-response ()
  "The standard 404 JSON body returned when GET /api/v1/pastes is asked
for an id that doesn't exist (or has expired)."
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (json-string '(("error" . "Paste not found or expired"))))

(defparameter *max-paste-size-bytes* 65536
  "Maximum accepted size, in UTF-8 encoded octets, of a paste's content.
POST /api/v1/pastes rejects anything larger with a 413.")

(defun paste-too-large-response ()
  "The standard 413 JSON body returned when POST /api/v1/pastes's
\"content\" exceeds *MAX-PASTE-SIZE-BYTES* once UTF-8 encoded."
  (setf (hunchentoot:return-code*) hunchentoot:+http-request-entity-too-large+)
  (json-string '(("error" . "Payload exceeds 64KB limit."))))

(defun paste-not-valid-lisp-response ()
  "The standard 422 JSON body returned when POST /api/v1/pastes's
\"content\" does not appear to be valid Common Lisp (per LISP-P:LISP-P)."
  (setf (hunchentoot:return-code*) hunchentoot:+http-unprocessable-entity+)
  (json-string '(("error" . "Content does not appear to be valid Common Lisp."))))

(defun content-byte-size (content)
  "Return CONTENT's length in UTF-8 encoded octets, via
BABEL:STRING-SIZE-IN-OCTETS (accounts for multi-byte characters, unlike
plain CL:LENGTH)."
  (babel:string-size-in-octets content :encoding :utf-8))

(defun valid-lisp-content-p (content)
  "T if CONTENT parses as Common Lisp per LISP-P:LISP-P, NIL otherwise.
LISP-P:LISP-P reads from a stream, so CONTENT is wrapped in a
MAKE-STRING-INPUT-STREAM."
  (and (lisp-p:lisp-p (make-string-input-stream content)) t))

(defun declared-content-length ()
  "Parse the request's incoming Content-Length header as an integer, or
NIL if absent/unparseable (e.g. a chunked-transfer request with no
declared length)."
  (let ((header (hunchentoot:header-in* :content-length)))
    (and header (ignore-errors (parse-integer header)))))

(defun request-body-too-large-p ()
  "T if the client's declared Content-Length already exceeds
*MAX-PASTE-SIZE-BYTES*, checked purely from the request header --
before RAW-POST-DATA/PARSE-JSON-REQUEST-BODY ever reads a single byte
of the body off the socket. This lets POST /api/v1/pastes reject an
oversized upload immediately, without first buffering the whole
request into a string just to measure it."
  (let ((declared (declared-content-length)))
    (and declared (> declared *max-paste-size-bytes*))))

(defun api-pastes-create ()
  "POST /api/v1/pastes: create a new paste owned by *API-USER* (see
WITH-API-AUTH-AND-RATE-LIMIT in API-PASTES-ACTION) from the \"content\"
field of the JSON request body, tagged with *API-TIER*'s retention
policy. Returns 201 + {\"status\":\"success\",\"id\":...} on success;
400 if the body is missing/malformed or \"content\" is absent/empty;
413 if the request exceeds *MAX-PASTE-SIZE-BYTES* (checked immediately
from the Content-Length header via REQUEST-BODY-TOO-LARGE-P -- before
the body is ever read -- and, as a fallback for requests with no
declared length, re-checked on the decoded \"content\" field's UTF-8
size); or 422 if \"content\" does not appear to be valid Common Lisp
(see VALID-LISP-CONTENT-P). All of these checks run after
authentication/rate-limiting (WITH-API-AUTH-AND-RATE-LIMIT, above) but
before ADD-PASTE ever touches the database."
  (with-api-auth-and-rate-limit
    (if (request-body-too-large-p)
        (paste-too-large-response)
        (let ((body (parse-json-request-body)))
          (if (null body)
              (json-bad-request-response "Malformed or missing JSON request body")
              (let ((content (cdr (assoc :content body))))
                (cond
                  ((not (and content (stringp content) (plusp (length content))))
                   (json-bad-request-response "Missing or empty \"content\" field"))
                  ;; Fallback for the (rare) request with no declared
                  ;; Content-Length: REQUEST-BODY-TOO-LARGE-P couldn't
                  ;; check up front, so check the decoded field now.
                  ((> (content-byte-size content) *max-paste-size-bytes*)
                   (paste-too-large-response))
                  ((not (valid-lisp-content-p content))
                   (paste-not-valid-lisp-response))
                  (t
                   (let ((paste-id (jrm-auth:add-paste *api-user* content (api-tier-keyword *api-tier*))))
                     (setf (hunchentoot:return-code*) hunchentoot:+http-created+)
                     (json-string (list (cons "status" "success") (cons "id" paste-id)))))))))))) 

(defun api-pastes-get (id)
  "GET /api/v1/pastes?id=...: return the paste's content. Deliberately
NOT wrapped in WITH-API-AUTH-AND-RATE-LIMIT -- pastes are meant to be
readable by anyone holding the id, the same as a traditional pastebin
link, with no Bearer token required. Returns 200 +
{\"id\":...,\"content\":...} if found, 404 if the id doesn't exist or
has expired, or 400 if \"id\" is missing."
  (if (and id (plusp (length id)))
      (let ((content (jrm-auth:get-paste id)))
        (if (null content)
            (paste-not-found-response)
            (json-string (list (cons "id" id) (cons "content" content)))))
      (json-bad-request-response "Missing required \"id\" query parameter")))

(defun api-pastes-delete (id)
  "DELETE /api/v1/pastes?id=...: delete the paste, but only if it is
owned by *API-USER* (see WITH-API-AUTH-AND-RATE-LIMIT in
API-PASTES-ACTION and DELETE-PASTE-MANUAL's ownership check). Returns
200 + {\"status\":\"success\"} (even if no matching owned paste
existed, mirroring DELETE-PASTE-MANUAL's plain DELETE semantics), or 400
if \"id\" is missing."
  (with-api-auth-and-rate-limit
    (if (and id (plusp (length id)))
        (progn
          (jrm-auth:delete-paste-manual id *api-user*)
          (json-string '(("status" . "success"))))
        (json-bad-request-response "Missing required \"id\" query parameter"))))

(hunchentoot:define-easy-handler (api-pastes-action :uri "/api/v1/pastes") (id)
  (setf (hunchentoot:content-type*) "application/json")
  (with-json-error-response
    (case (hunchentoot:request-method*)
      (:post (api-pastes-create))
      (:get (api-pastes-get id))
      (:delete (api-pastes-delete id))
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
       (json-string '(("status" . "error") ("message" . "Method not allowed")))))))

(defun api-user-pastes-list ()
  "GET /api/v1/user/pastes: return the authenticated *API-USER*'s
non-expired pastes (see GET-USER-PASTES) as a JSON array of
{\"id\",\"created_at\",\"expires_at\",\"content_preview\"} objects, most
recently created first."
  (with-api-auth-and-rate-limit
    (json-string (mapcar (lambda (row)
                            (list (cons "id" (cdr (assoc :id row)))
                                  (cons "created_at" (cdr (assoc :created-at row)))
                                  (cons "expires_at" (cdr (assoc :expires-at row)))
                                  (cons "content_preview" (cdr (assoc :content-preview row)))))
                          (jrm-auth:get-user-pastes *api-user*)))))

(hunchentoot:define-easy-handler (api-user-pastes-action :uri "/api/v1/user/pastes") ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-json-error-response
    (case (hunchentoot:request-method*)
      (:get (api-user-pastes-list))
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
       (json-string '(("status" . "error") ("message" . "Method not allowed")))))))
