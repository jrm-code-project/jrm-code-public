;;; -*- Lisp -*-

;;; Application logic: the /lisp-p syntax-validation endpoint, and the
;;; Michelin chef Gemini-powered Lisp roast feature (/goog/chef, /chef.html).
;;; If more AI features are added later, they can live here or get their
;;; own feature files, untangled from the auth/billing/admin system.

(in-package "JRM-CODE-PROJECT")

(defconstant +lisp-p-minimum-delay-ms+ 300
  "Minimum time, in milliseconds, that the /lisp-p handler takes to
respond, regardless of how quickly LISP-P:LISP-P finishes.  This makes
the endpoint's timing independent of the size/shape of its input.")

(hunchentoot:define-easy-handler (lisp-p-page :uri "/lisp-p") ()
  (case (hunchentoot:request-method*)
    (:post
     (let ((start-time (get-internal-real-time)))
       (unwind-protect
            (handler-case
                (if (lisp-p:lisp-p (hunchentoot:raw-post-data :want-stream t))
                    (progn
                      (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
                      "OK")
                    (progn
                      (setf (hunchentoot:return-code*) hunchentoot:+http-unprocessable-entity+)
                      "Unprocessable Entity"))
              (error ()
                (setf (hunchentoot:return-code*) hunchentoot:+http-unprocessable-entity+)
                "Unprocessable Entity"))
         (let ((elapsed-ms (/ (* 1000 (- (get-internal-real-time) start-time))
                              internal-time-units-per-second)))
           (when (< elapsed-ms +lisp-p-minimum-delay-ms+)
             (sleep (/ (- +lisp-p-minimum-delay-ms+ elapsed-ms) 1000)))))))
    (t
     (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
     "Method Not Allowed")))

(defparameter *chef-system-prompt*
  "<redacted>"
  "System persona instruction sent to Gemini for the /goog/chef endpoint.")

(defparameter *gemini-read-timeout-seconds* 60
  "Read timeout for the Gemini generateContent call. Dexador's default
of 10 seconds (Dexador.util:*default-read-timeout*) is too short for
generateContent responses, which can legitimately take longer than that,
and was causing spurious \"ERROR 12002: Timeout\" failures on Windows.")

(defun roast-code-with-gemini (code api-key)
  "Send CODE to Gemini, dressed up with the Michelin-chef persona, using
API-KEY for authentication. Returns an outcome value: (OK roast-text) on
success, or (ERR reason) if Gemini rejects the request or the call fails
-- see FUNCTIONAL_REFACTOR.md Phase 6."
  (handler-case
      (let* ((url "https://generativelanguage.googleapis.com/v1beta/models/gemini-flash-latest:generateContent")
             (headers (list (cons "x-goog-api-key" api-key)
                            (cons "Content-Type" "application/json")))
             (payload (list (cons "systemInstruction"
                                  (list (cons "parts" (vector (list (cons "text" *chef-system-prompt*))))))
                            (cons "contents"
                                  (vector (list (cons "parts" (vector (list (cons "text" code)))))))))
             (json-payload (cl-json:encode-json-to-string payload))
             (response (dex:post url :headers headers :content json-payload
                                     :read-timeout *gemini-read-timeout-seconds*
                                     :connect-timeout *gemini-read-timeout-seconds*))
             (json (cl-json:decode-json-from-string response))
             (candidates (cdr (assoc :candidates json)))
             (content (cdr (assoc :content (first candidates))))
             (parts (cdr (assoc :parts content)))
             (text (cdr (assoc :text (first parts)))))
        (if text
            (ok text)
            (err "Gemini response did not contain any roast text.")))
    (error (e)
      (err (format nil "~A" e)))))

(defparameter *chef-minimum-tier* "CONS"
  "Minimum membership tier required to use the Chef roast feature (page and
API endpoint alike).")

(hunchentoot:define-easy-handler (chef-handler :uri "/goog/chef") ()
  "Validate that the POST body is Lisp via LISP-P, then hand it to Gemini
to be roasted by a foul-mouthed virtual Michelin chef. Caddy is expected
to reject requests missing the x-goog-api-key header before they ever
reach here; we re-check it as a failsafe in case this port is hit directly.
Gated behind +CHEF-MINIMUM-TIER+ membership: requests without a
sufficient membership JWT cookie are rejected with 403, rather than
redirected, since this is an API endpoint (called via fetch) rather than
a page navigation."
  (setf (hunchentoot:content-type*) "text/plain")
  (case (hunchentoot:request-method*)
    (:post
     (let ((membership-tier (current-membership-tier-from-jwt)))
       (if (not (and membership-tier (tier-meets-minimum-p membership-tier *chef-minimum-tier*)))
           (progn
             (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
             (format nil "~A membership or higher required. Get out of my kitchen!" *chef-minimum-tier*))
           (let ((api-key (hunchentoot:header-in* "x-goog-api-key"))
                 (raw-code (hunchentoot:raw-post-data :force-text t)))
             (cond
               ((not api-key)
                (setf (hunchentoot:return-code*) hunchentoot:+http-authorization-required+)
                "Missing x-goog-api-key header.")
               ((not (and raw-code (plusp (length raw-code))))
                (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
                "Where is the code? The plate is empty!")
               ((not (lisp-p:lisp-p (make-string-input-stream raw-code)))
                (setf (hunchentoot:return-code*) hunchentoot:+http-unprocessable-entity+)
                "This isn't Lisp! It's raw, unstructured garbage! Get out of my kitchen!")
               (t
                (let ((outcome (roast-code-with-gemini raw-code api-key)))
                  (if (outcome-ok-p outcome)
                      (outcome-value outcome)
                      (progn
                        (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
                        (format nil "The kitchen caught fire during execution: ~A" (outcome-reason outcome)))))))))))
    (t
     (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
     "Only POST requests are allowed, you donkey!")))

(hunchentoot:define-easy-handler (chef-page :uri "/chef.html") ()
  "Serve the Chef front-end (resources/www/html/chef.html), gated behind
+CHEF-MINIMUM-TIER+ membership. Defining this easy handler at the same URI
as the static file takes priority over the acceptor's plain static-file
fallback, so unauthenticated/insufficient-tier requests never reach the
raw HTML on disk -- they get redirected to login or the upgrade-required
page instead, exactly like any other JWT-protected page."
  (let ((claims (require-membership-tier *chef-minimum-tier*)))
    (when claims
      (setf (hunchentoot:content-type*) "text/html")
      (uiop:read-file-string
       (asdf:system-relative-pathname :jrm-code-project "resources/www/html/chef.html")))))
