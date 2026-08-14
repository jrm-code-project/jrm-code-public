;;; -*- Lisp -*-

;;; The Stripe integration: the upgrade-required interstitial, checkout
;;; session creation, the billing-portal redirect, and the Stripe webhook
;;; handler. Kept separate from auth.lisp so the money-handling code (and
;;; especially the webhook, which must be re-audited whenever Stripe
;;; changes their API payload) can be reviewed in isolation.

(in-package "JRM-CODE-PROJECT")

;; --- WEBHOOK EVENT DECISION LOGIC (pure) ---

(defun webhook-event->db-commands (json)
  "Given the decoded Stripe webhook JSON alist, return a list of pure command
descriptions (plists) describing what should happen -- with no I/O
performed. The caller (STRIPE-WEBHOOK-HANDLER) is a thin imperative loop
that executes each command against JRM-AUTH:*. See FUNCTIONAL_REFACTOR.md
Phase 2: this makes webhook dispatch logic directly unit-testable with
hand-built fixtures, with no live Stripe payload or database required.

Recognized commands:
  (:update-subscription :username U :customer-id C :subscription-id S :status ST :tier T)
  (:update-tier-status :username U :tier T :status ST)
  (:update-tier-status-by-customer :customer-id C :tier T :status ST)
  (:cancel-subscription :username U)
  (:cancel-subscription-by-customer :customer-id C)
  (:mark-past-due-by-customer :customer-id C)
  (:log :message M) -- informational, no DB effect; used when required
    fields are missing so the caller can still log the anomaly."
  (let* ((type (cdr (assoc :type json)))
         (data (cdr (assoc :data json)))
         (obj (cdr (assoc :object data))))
    (cond
      ;; A customer completed Managed Payments checkout for a new subscription.
      ((string= type "checkout.session.completed")
       (let* ((client-ref-id (cdr (assoc :client--reference--id obj)))
              (customer-id (cdr (assoc :customer obj)))
              (sub-id (cdr (assoc :subscription obj))))
         (if (and client-ref-id customer-id sub-id)
             (list (list :op :update-subscription
                         :username client-ref-id
                         :customer-id customer-id
                         :subscription-id sub-id
                         :tier :lookup-from-subscription
                         :subscription-id-for-tier-lookup sub-id))
             (list (list :op :log
                         :message "Missing required fields in checkout.session.completed webhook object.")))))
      ;; The customer's subscription changed: renewal, or an upgrade/downgrade
      ;; made through the Billing Portal (Stripe handles proration automatically).
      ((or (string= type "customer.subscription.updated")
           (string= type "customer.subscription.created"))
       (let* ((customer-id (cdr (assoc :customer obj)))
              (status (cdr (assoc :status obj)))
              (items (cdr (assoc :data (cdr (assoc :items obj)))))
              (price-id (cdr (assoc :id (cdr (assoc :price (first items)))))))
         (list (list :op :update-tier-status-by-customer
                     :customer-id customer-id
                     :tier :lookup-from-price-id
                     :price-id-for-tier-lookup price-id
                     :status status))))
      ;; The subscription ended (cancellation, with any pro-rated refund
      ;; already handled by Stripe) -- downgrade the user back to CONS.
      ((string= type "customer.subscription.deleted")
       (let ((customer-id (cdr (assoc :customer obj))))
         (list (list :op :cancel-subscription-by-customer :customer-id customer-id))))
      ;; A renewal invoice failed to collect payment -- flag the account.
      ((string= type "invoice.payment_failed")
       (let ((customer-id (cdr (assoc :customer obj))))
         (list (list :op :mark-past-due-by-customer :customer-id customer-id))))
      (t nil))))

(hunchentoot:define-easy-handler (upgrade-required-page :uri "/upgrade-required") (tier next)
  "Shown when a JWT-protected page requires a higher membership tier than the
user currently holds. Offers a `Return to Dashboard' link (so the user can
browse/purchase an upgraded plan) and, if the tier being offered is
purchasable, a direct purchase link that carries the NEXT breadcrumb
through checkout so the user lands back on the originally-requested page
(with a freshly-issued JWT) after paying -- instead of just the dashboard."
  (let* ((user (hunchentoot:session-value :authenticated-user))
         (required-tier (or tier "CADR"))
         (purchasable-p (member required-tier '("CADR" "LAMBDA") :test #'string-equal))
         (purchase-link (if (and user purchasable-p)
                            (format nil "/create-checkout-session?tier=~A~@[&next=~A~]"
                                    (hunchentoot:url-encode required-tier)
                                    (and next (hunchentoot:url-encode next)))
                            nil)))
    (html-page
     (format nil "~A Membership Required" required-tier)
     (format nil "<div class='box'>
                    <h2>~A Membership Required</h2>
                    <p>This page requires an active <b>~A</b> membership or higher.</p>
                    ~A
                    <p><a href='/dashboard' class='btn btn-secondary'>Return to Dashboard</a></p>
                  </div>"
             (html-escape required-tier)
             (html-escape required-tier)
             (if purchase-link
                 (format nil "<p><a href='~A' class='btn'>Upgrade to ~A</a></p>"
                         purchase-link (html-escape required-tier))
                 ""))
     :extra-style ".box { max-width: 480px; margin: 4rem auto; border: 2px solid #88c0d0; border-radius: 8px; padding: 2rem; text-align: center; }")))

(hunchentoot:define-easy-handler (create-checkout-session-action :uri "/create-checkout-session") (tier next)
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (if user
        (if (member tier '("CADR" "LAMBDA") :test #'string-equal)
            (let ((outcome (create-stripe-checkout-session user (string-upcase tier) next)))
              (if (outcome-ok-p outcome)
                  (hunchentoot:redirect (outcome-value outcome))
                  (html-page "Checkout Error"
                             (format nil "<h2>Error</h2><p>Could not create checkout session: ~A</p><p><a href='/dashboard'>Return to Dashboard</a></p>"
                                     (html-escape (outcome-reason outcome))))))
            (hunchentoot:redirect "/dashboard"))
        (hunchentoot:redirect "/"))))

(hunchentoot:define-easy-handler (manage-subscription-action :uri "/manage-subscription") ()
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (if user
        (let* ((user-data (car (jrm-auth:get-user user)))
               (customer-id (jrm-auth:user-stripe-customer-id user-data))
               (portal-url (and customer-id (create-billing-portal-session customer-id))))
          (if portal-url
              (hunchentoot:redirect portal-url)
              (progn
                (setf (hunchentoot:return-code*) 502)
                "<html><head><style>body { background: #111; color: #f00; font-family: sans-serif; padding: 2rem; }</style></head><body><h2>Error</h2><p>Could not open the subscription management portal.</p><p><a href='/dashboard' style='color:#fff;'>Return to Dashboard</a></p></body></html>")))
        (hunchentoot:redirect "/"))))

(defun execute-webhook-db-command (command)
  "Thin imperative interpreter for a single command produced by
WEBHOOK-EVENT->DB-COMMANDS (a pure function). Performs the actual DB
mutation (or log message) the command describes and returns an outcome
value: (OK description) on success, or (ERR reason) if the command could
not be applied (e.g. the customer/user could not be resolved). See
FUNCTIONAL_REFACTOR.md Phase 6 -- the caller (STRIPE-WEBHOOK-HANDLER)
collects these outcomes and reports failures once, at the shell boundary,
instead of each branch independently deciding how to report a problem."
  (destructuring-bind (&key op username customer-id subscription-id status tier
                            price-id-for-tier-lookup subscription-id-for-tier-lookup
                            message)
      command
    (ecase op
      (:update-subscription
       (let ((resolved-tier (or (and (eq tier :lookup-from-subscription)
                                     (get-stripe-subscription-tier subscription-id-for-tier-lookup))
                                "CADR")))
         (format t ";; Processing checkout.session.completed for user ~A, customer ~A, subscription ~A~%"
                 username customer-id subscription-id)
         (jrm-auth:update-user-subscription username customer-id subscription-id "active" resolved-tier)
         (ok (format nil "Updated subscription for ~A." username))))
      (:update-tier-status-by-customer
       (let* ((resolved-tier (if (eq tier :lookup-from-price-id)
                                  (tier-from-price-id price-id-for-tier-lookup)
                                  tier))
              (user (car (jrm-auth:get-user-by-customer customer-id))))
         (if (and user resolved-tier)
             (progn
               (jrm-auth:update-user-tier-status (jrm-auth:user-username user) resolved-tier status)
               (ok (format nil "Updated tier/status for ~A." (jrm-auth:user-username user))))
             (err (format nil "Could not resolve user/tier for subscription update (customer=~A price=~A)"
                          customer-id price-id-for-tier-lookup)))))
      (:cancel-subscription-by-customer
       (let ((user (car (jrm-auth:get-user-by-customer customer-id))))
         (if user
             (progn
               (jrm-auth:cancel-user-subscription (jrm-auth:user-username user))
               (ok (format nil "Cancelled subscription for ~A." (jrm-auth:user-username user))))
             (err (format nil "Could not resolve user for canceled subscription (customer=~A)" customer-id)))))
      (:mark-past-due-by-customer
       (let ((user (car (jrm-auth:get-user-by-customer customer-id))))
         (if user
             (progn
               (jrm-auth:update-user-tier-status (jrm-auth:user-username user)
                                                 (jrm-auth:user-membership-tier user)
                                                 "past_due")
               (ok (format nil "Marked ~A past due." (jrm-auth:user-username user))))
             (err (format nil "Could not resolve user for past-due invoice (customer=~A)" customer-id)))))
      (:log
       (err message)))))

(hunchentoot:define-easy-handler (stripe-webhook-handler :uri "/stripe-webhook") ()
  (case (hunchentoot:request-method*)
    (:post
     (handler-case
         (let* ((raw-body (hunchentoot:raw-post-data :force-text t))
                (signature (hunchentoot:header-in* "Stripe-Signature")))
           (if (not (verify-stripe-webhook-signature raw-body signature))
               (progn
                 (format t ";; Warning: Rejected Stripe webhook with invalid signature.~%")
                 (setf (hunchentoot:return-code*) 400)
                 "{\"error\": \"invalid signature\"}")
               (let* ((json (cl-json:decode-json-from-string raw-body))
                      (type (cdr (assoc :type json)))
                      (commands (webhook-event->db-commands json))
                      ;; Pure/impure split (Phase 6): the commands are already
                      ;; computed; here we just execute each and collect
                      ;; whatever outcomes come back, reporting any failures
                      ;; once at the end instead of each branch separately
                      ;; deciding how to report a problem.
                      (outcomes (mapcar #'execute-webhook-db-command commands))
                      (failures (remove-if #'outcome-ok-p outcomes)))
                 (format t ";; Stripe Webhook Received: type=~A~%" type)
                 (dolist (failure failures)
                   (format t ";; Warning: ~A~%" (outcome-reason failure)))
                 (if failures
                     "{\"status\": \"partial-failure\"}"
                     "{\"status\": \"success\"}"))))
       (error (e)
         (setf (hunchentoot:return-code*) 500)
         (format nil "{\"error\": \"~A\"}" e))))
    (t
     (setf (hunchentoot:return-code*) 405)
     "Method Not Allowed")))
