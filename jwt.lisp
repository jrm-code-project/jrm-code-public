;;; -*- Lisp -*-

;;; Minimal JWT (JSON Web Token) support -- HS256 signing/verification only.
;;; Used to hand the browser a long-lived, tamper-evident token that encodes
;;; the user's membership tier, so that static feature-preview pages can
;;; check access without hitting the database or session store on every
;;; request. See https://datatracker.ietf.org/doc/html/rfc7519.

(in-package "JRM-CODE-PROJECT")

(defun get-jwt-secret ()
  "Retrieve the JWT signing secret from the environment. Falls back to the
Stripe secret key (already a private, per-deployment secret) if
JWT_SECRET is not explicitly configured, so a working secret is always
available without extra setup."
  (or (uiop:getenv "JWT_SECRET")
      (get-stripe-secret-key)
      ""))

(defun base64url-encode-string (string)
  "Encode STRING (UTF-8) as unpadded base64url, per RFC 7515 Appendix C.
Note: cl-base64's :URI mode uses '.' rather than '=' as its padding
character, which would collide with the JWT's '.' segment separator, so
we strip both possible padding characters explicitly."
  (string-right-trim
   ".="
   (cl-base64:usb8-array-to-base64-string
    (flexi-streams:string-to-octets string :external-format :utf-8)
    :uri t)))

(defun base64url-decode-string (b64url)
  "Decode an unpadded base64url STRING back to a UTF-8 string. cl-base64's
:URI decoder expects '.' (not '=') as its padding character, so pad with
'.' to match."
  (let* ((padded (let ((rem (mod (length b64url) 4)))
                   (if (zerop rem)
                       b64url
                       (concatenate 'string b64url (make-string (- 4 rem) :initial-element #\.))))))
    (flexi-streams:octets-to-string
     (cl-base64:base64-string-to-usb8-array padded :uri t)
     :external-format :utf-8)))

(defun hmac-sha256-bytes (secret message)
  "Compute the raw HMAC-SHA256 digest of MESSAGE (a string) using SECRET (a string)."
  (let ((hmac (ironclad:make-mac :hmac (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-mac hmac (flexi-streams:string-to-octets message :external-format :utf-8))
    (ironclad:produce-mac hmac)))

(defun encode-jwt (claims &key (secret (get-jwt-secret)))
  "Encode CLAIMS (an alist of JSON-serializable key/value pairs) as a signed
HS256 JWT string."
  (let* ((header-json (cl-json:encode-json-to-string '(("alg" . "HS256") ("typ" . "JWT"))))
         (payload-json (cl-json:encode-json-to-string claims))
         (header-b64 (base64url-encode-string header-json))
         (payload-b64 (base64url-encode-string payload-json))
         (signing-input (format nil "~A.~A" header-b64 payload-b64))
         (signature-b64 (string-right-trim
                         ".="
                         (cl-base64:usb8-array-to-base64-string (hmac-sha256-bytes secret signing-input) :uri t))))
    (format nil "~A.~A" signing-input signature-b64)))

(defun decode-jwt (token &key (secret (get-jwt-secret)))
  "Verify TOKEN's HS256 signature and expiry, returning its claims alist on
success, or NIL if the token is malformed, has an invalid signature, or has
expired."
  (handler-case
      (let* ((parts (uiop:split-string token :separator "."))
             (header-b64 (first parts))
             (payload-b64 (second parts))
             (signature-b64 (third parts)))
        (when (and header-b64 payload-b64 signature-b64)
          (let* ((signing-input (format nil "~A.~A" header-b64 payload-b64))
                 (expected-signature-b64 (string-right-trim
                                          ".="
                                          (cl-base64:usb8-array-to-base64-string
                                           (hmac-sha256-bytes secret signing-input) :uri t))))
            (when (string= signature-b64 expected-signature-b64)
              (let* ((payload-json (base64url-decode-string payload-b64))
                     (claims (cl-json:decode-json-from-string payload-json))
                     (exp (cdr (assoc :exp claims))))
                (when (or (null exp) (>= exp (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0))))
                  claims))))))
    (error () nil)))

(defparameter *jwt-cookie-name* "membership_jwt")
(defparameter *jwt-lifetime-seconds* (* 14 24 60 60)
  "How long the membership JWT remains valid after issuance (14 days), so
that access to feature previews persists for a while even after a
subscription is cancelled.")

(defun unix-time ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun issue-membership-jwt (username tier &optional wheel-p)
  "Create and set the membership JWT cookie for USERNAME/TIER on the current
Hunchentoot response. The cookie is long-lived (see *JWT-LIFETIME-SECONDS*)
so that feature-preview access persists for a while even after the
underlying subscription is cancelled. WHEEL-P, if true, encodes the wheel
(super user) bit in the token, granting access to special wheel-only
pages regardless of membership tier."
  (let* ((now (unix-time))
         (token (encode-jwt (list (cons "sub" username)
                                  (cons "tier" tier)
                                  (cons "wheel" (and wheel-p t))
                                  (cons "iat" now)
                                  (cons "exp" (+ now *jwt-lifetime-seconds*))))))
    (hunchentoot:set-cookie *jwt-cookie-name*
                            :value token
                            :path "/"
                            :max-age *jwt-lifetime-seconds*
                            :http-only t)
    token))

(defun current-membership-tier-from-jwt ()
  "Return the membership tier encoded in the request's JWT cookie, or NIL if
absent, malformed, or expired."
  (let ((token (hunchentoot:cookie-in *jwt-cookie-name*)))
    (and token (cdr (assoc :tier (decode-jwt token))))))

(defun wheel-jwt-p ()
  "Return T if the request's JWT cookie is present, valid, and has the wheel
(super user) bit set."
  (let ((token (hunchentoot:cookie-in *jwt-cookie-name*)))
    (and token (eq (cdr (assoc :wheel (decode-jwt token))) t))))

(defun redirect-to-login-with-breadcrumb (&optional (return-path (hunchentoot:request-uri*)))
  "Redirect to the login splash page, passing RETURN-PATH along as a `next`
breadcrumb so that a successful login can send the user back to the page
they originally requested."
  (hunchentoot:redirect (format nil "/?next=~A" (hunchentoot:url-encode return-path))))

(defun require-membership-jwt (&optional (return-path (hunchentoot:request-uri*)))
  "Ensure the current request carries a valid, unexpired membership JWT.
Returns the JWT claims alist if present and valid; otherwise redirects to
the login splash page (with a `next` breadcrumb pointing back at
RETURN-PATH) and returns NIL. Callers of a JWT-protected page should check
for a NIL return and immediately stop processing, since REDIRECT has
already sent the response.
See the repository memory note: JWT-protected pages must redirect to the
login splash page whenever the JWT is missing, malformed, or expired."
  (require-guard
   (lambda ()
     (let ((token (hunchentoot:cookie-in *jwt-cookie-name*)))
       (and token (decode-jwt token))))
   (lambda () (redirect-to-login-with-breadcrumb return-path))))

(defun redirect-to-upgrade-required (minimum-tier &optional (return-path (hunchentoot:request-uri*)))
  "Redirect to the upgrade-required page, indicating MINIMUM-TIER is needed to
access RETURN-PATH. The upgrade-required page carries RETURN-PATH along as
a `next` breadcrumb, so that once the user purchases the appropriate plan,
they are sent back to RETURN-PATH (getting in this time) rather than to
the dashboard."
  (hunchentoot:redirect (format nil "/upgrade-required?tier=~A&next=~A"
                                (hunchentoot:url-encode minimum-tier)
                                (hunchentoot:url-encode return-path))))

(defun require-wheel (&optional (return-path (hunchentoot:request-uri*)))
  "Ensure the current request's JWT has the wheel (super user) bit set.
Returns the JWT claims alist on success; otherwise redirects (to login if
the JWT is missing/expired, or to the upgrade-required page -- reusing it
to indicate wheel access is needed -- if the JWT is valid but lacks the
wheel bit) and returns NIL. Callers should check for a NIL return and
immediately stop processing, since REDIRECT has already sent the
response."
  (let ((claims (require-membership-jwt return-path)))
    (and claims
         (require-guard
          (lambda () (and (eq (cdr (assoc :wheel claims)) t) claims))
          (lambda () (redirect-to-upgrade-required "Wheel" return-path))))))

(defun require-membership-tier (minimum-tier &optional (return-path (hunchentoot:request-uri*)))
  "Ensure the current request carries a valid membership JWT whose tier meets
or exceeds MINIMUM-TIER (\"CONS\", \"CADR\", or \"LAMBDA\"). Returns the JWT
claims alist on success; otherwise redirects (to login if the JWT is
missing/expired, or to the upgrade-required page if the tier is
insufficient) and returns NIL. Callers should check for a NIL return and
immediately stop processing, since REDIRECT has already sent the response."
  (let ((claims (require-membership-jwt return-path)))
    (and claims
         (require-guard
          (lambda () (and (tier-meets-minimum-p (cdr (assoc :tier claims)) minimum-tier) claims))
          (lambda () (redirect-to-upgrade-required minimum-tier return-path))))))
