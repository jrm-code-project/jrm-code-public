;;; -*- Lisp -*-

(in-package "JRM-CODE-PROJECT")

;; API version that supports Managed Payments (Stripe's merchant-of-record
;; solution -- see https://docs.stripe.com/payments/managed-payments).
(defparameter *stripe-api-version* "2026-02-25.preview")

;; Product tax code eligible for Managed Payments: "Software as a service
;; (SaaS) - electronic download - personal use". See
;; https://docs.stripe.com/payments/managed-payments/eligibility#product-eligibility
(defparameter *stripe-tax-code* "txcd_10103100")

;; Paid membership tiers backed by Stripe. CONS is the free tier and has no
;; corresponding Stripe product/price. K-Machine is bespoke consulting
;; ("Contact for pricing") and is intentionally not sold through Managed
;; Payments, which does not support professional/human-delivered services.
(defparameter *stripe-tier-plans*
  '(("CADR"   "CADR Membership"   995)
    ("LAMBDA" "LAMBDA Membership" 4995))
  "List of (tier-name product-name unit-amount-in-cents) for paid tiers.")

(defstruct (stripe-catalog (:copier nil))
  "Immutable snapshot of everything we know about our Stripe product/price
catalog and Billing Portal configuration. Replaces four separately-mutated
globals (*STRIPE-TIER-PRICE-IDS*, *STRIPE-TIER-PRODUCT-IDS*,
*STRIPE-PRICE-ID-TIERS*, *STRIPE-BILLING-PORTAL-CONFIGURATION-ID*) with one
value that can never be observed half-updated. See FUNCTIONAL_REFACTOR.md
Phase 4."
  (tier-price-ids nil)              ; alist: tier name -> Stripe price ID
  (tier-product-ids nil)            ; alist: tier name -> Stripe product ID
  (price-id-tiers nil)              ; alist: Stripe price ID -> tier name (inverse of TIER-PRICE-IDS)
  (billing-portal-configuration-id nil))

(defvar *stripe-catalog* (make-stripe-catalog)
  "The current, live Stripe catalog snapshot, set once (as a whole new
value) by INIT-STRIPE-PRODUCT on server startup, and read via the small
accessor functions TIER-PRICE-ID/TIER-FROM-PRICE-ID that close over it.")

(defun catalog-with-tier (catalog tier price-id product-id)
  "Return a new STRIPE-CATALOG, derived from CATALOG, with TIER's PRICE-ID and
PRODUCT-ID recorded (added to all three of the tier/price/product alists in
one step). Pure: does not mutate CATALOG."
  (make-stripe-catalog
   :tier-price-ids (acons tier price-id (stripe-catalog-tier-price-ids catalog))
   :tier-product-ids (acons tier product-id (stripe-catalog-tier-product-ids catalog))
   :price-id-tiers (acons price-id tier (stripe-catalog-price-id-tiers catalog))
   :billing-portal-configuration-id (stripe-catalog-billing-portal-configuration-id catalog)))

(defun catalog-with-billing-portal-configuration-id (catalog config-id)
  "Return a new STRIPE-CATALOG, derived from CATALOG, with its billing portal
configuration ID set to CONFIG-ID. Pure: does not mutate CATALOG."
  (make-stripe-catalog
   :tier-price-ids (stripe-catalog-tier-price-ids catalog)
   :tier-product-ids (stripe-catalog-tier-product-ids catalog)
   :price-id-tiers (stripe-catalog-price-id-tiers catalog)
   :billing-portal-configuration-id config-id))

(defun get-stripe-secret-key ()
  "Retrieve the Stripe secret key from environment variables."
  (or (uiop:getenv "STRIPE_SECRET_KEY") ""))

(defun get-stripe-publishable-key ()
  "Retrieve the Stripe publishable key from environment variables."
  (or (uiop:getenv "STRIPE_PUBLISHABLE_KEY") ""))

(defun get-stripe-webhook-secret ()
  "Retrieve the Stripe webhook signing secret from environment variables."
  (or (uiop:getenv "STRIPE_WEBHOOK_SECRET") ""))

(defun get-base-url ()
  "Dynamically determine the host and protocol of the running server."
  (format nil "~A://~A"
          (if (hunchentoot:ssl-p) "https" "http")
          (or (hunchentoot:host) "localhost:4242")))

(defun stripe-auth-headers (secret-key)
  "Standard headers for an authenticated Stripe API request."
  (list (cons "Authorization" (format nil "Bearer ~A" secret-key))
        (cons "stripe-version" *stripe-api-version*)))

(defun stripe-secret-key-configured-p (secret-key)
  "Return T if SECRET-KEY looks like a real, configured Stripe secret key
(non-NIL and non-empty) rather than the unset-environment-variable default."
  (and secret-key (not (string= secret-key ""))))

(defmacro with-stripe-credentials ((headers-var &key (secret-key-var (gensym "SECRET-KEY"))) &body body)
  "Evaluate BODY with HEADERS-VAR bound to the authenticated Stripe request
headers, only if STRIPE_SECRET_KEY is configured; otherwise return NIL
without evaluating BODY. Replaces the eight duplicated
`(let ((secret-key ...)) (when (and secret-key (not (string= secret-key
\"\"))) ...))' guard clauses called out in FUNCTIONAL_REFACTOR.md Sec 1.6
with a single audited combinator. SECRET-KEY-VAR, if supplied, is also
bound to the raw secret key for callers (like CREATE-STRIPE-CHECKOUT-SESSION)
that need it directly in addition to the headers."
  `(let ((,secret-key-var (get-stripe-secret-key)))
     (when (stripe-secret-key-configured-p ,secret-key-var)
       (let ((,headers-var (stripe-auth-headers ,secret-key-var)))
         ,@body))))

(defun tier-price-id (tier)
  "Look up the Stripe price ID for a paid TIER name (\"CADR\" or \"LAMBDA\")."
  (cdr (assoc tier (stripe-catalog-tier-price-ids *stripe-catalog*) :test #'string-equal)))

(defun tier-from-price-id (price-id)
  "Look up the membership tier name for a Stripe price ID, or NIL if unknown."
  (cdr (assoc price-id (stripe-catalog-price-id-tiers *stripe-catalog*) :test #'string=)))

(defun find-existing-tier-product (product-name)
  "Query Stripe to see if a product named PRODUCT-NAME already exists.
Returns (VALUES product-id price-id) or NIL if not found."
  (with-stripe-credentials (headers)
    (handler-case
        (let* ((url "https://api.stripe.com/v1/products")
               (response (dex:get url :headers headers))
               (json (cl-json:decode-json-from-string response))
               (data (cdr (assoc :data json))))
          (dolist (prod data)
            (let ((name (cdr (assoc :name prod)))
                  (prod-id (cdr (assoc :id prod)))
                  (price-id (cdr (assoc :default--price prod))))
              (when (and name (string= name product-name))
                (return-from find-existing-tier-product (values prod-id price-id))))))
      (error (e)
        (format t ";; Warning: Failed to query existing Stripe products: ~A~%" e)
        nil))))

(defun create-tier-product (product-name unit-amount)
  "Create a Stripe product/price for PRODUCT-NAME billed monthly at UNIT-AMOUNT cents."
  (or (with-stripe-credentials (headers)
        (handler-case
            (let* ((url "https://api.stripe.com/v1/products")
                   (content (list (cons "name" product-name)
                                  (cons "description" (format nil "~A subscription" product-name))
                                  (cons "tax_code" *stripe-tax-code*)
                                  (cons "default_price_data[unit_amount]" (format nil "~D" unit-amount))
                                  (cons "default_price_data[currency]" "usd")
                                  (cons "default_price_data[recurring][interval]" "month")))
                   (response (dex:post url :headers headers :content content))
                   (json (cl-json:decode-json-from-string response))
                   (prod-id (cdr (assoc :id json)))
                   (price-id (cdr (assoc :default--price json))))
              (format t ";; Created Stripe product ~A (~A) with price ~A~%" prod-id product-name price-id)
              (values prod-id price-id))
          (error (e)
            (format t ";; Error: Failed to create Stripe product ~A: ~A~%" product-name e)
            (values nil nil))))
      (progn
        (format t ";; Warning: STRIPE_SECRET_KEY is not set. Cannot create Stripe product.~%")
        (values nil nil))))

(defun ensure-tier-product (catalog tier product-name unit-amount)
  "Find or create the Stripe product/price for TIER, performing the actual
Stripe HTTP calls (a side effect), and return a new STRIPE-CATALOG derived
from CATALOG with TIER's price/product IDs recorded via
CATALOG-WITH-TIER -- CATALOG itself is never mutated. Also returns
(VALUES new-catalog product-id price-id)."
  (multiple-value-bind (prod-id price-id) (find-existing-tier-product product-name)
    (unless prod-id
      (setf (values prod-id price-id) (create-tier-product product-name unit-amount)))
    (values (if (and prod-id price-id)
                (catalog-with-tier catalog tier price-id prod-id)
                catalog)
            prod-id price-id)))

(defun ensure-billing-portal-configuration (catalog)
  "Create (or reuse) a Billing Portal configuration that lets customers switch
between the paid membership tiers, with proration, and cancel with a
pro-rated refund. See https://docs.stripe.com/api/customer_portal/configurations.
Performs the actual Stripe HTTP call (a side effect) and returns a new
STRIPE-CATALOG derived from CATALOG with the resulting configuration ID
recorded -- CATALOG itself is never mutated."
  (or (and (stripe-catalog-tier-price-ids catalog)
           (with-stripe-credentials (headers)
             (handler-case
                 (let* ((url "https://api.stripe.com/v1/billing_portal/configurations")
                        (content (append
                                  (list (cons "business_profile[headline]" "Manage your JRM-Code membership")
                                        (cons "features[subscription_cancel][enabled]" "true")
                                        (cons "features[subscription_cancel][mode]" "immediately")
                                        (cons "features[subscription_cancel][proration_behavior]" "create_prorations")
                                        (cons "features[subscription_update][enabled]" "true")
                                        (cons "features[subscription_update][default_allowed_updates][0]" "price")
                                        (cons "features[subscription_update][proration_behavior]" "create_prorations")
                                        (cons "features[payment_method_update][enabled]" "true"))
                                  (loop for (tier . price-id) in (stripe-catalog-tier-price-ids catalog)
                                        for i from 0
                                        collect (cons (format nil "features[subscription_update][products][~D][product]" i)
                                                      (cdr (assoc tier (stripe-catalog-tier-product-ids catalog) :test #'string-equal)))
                                        collect (cons (format nil "features[subscription_update][products][~D][prices][0]" i)
                                                      price-id))))
                        (response (dex:post url :headers headers :content content))
                        (json (cl-json:decode-json-from-string response))
                        (config-id (cdr (assoc :id json))))
                   (format t ";; Created Stripe billing portal configuration ~A~%" config-id)
                   (catalog-with-billing-portal-configuration-id catalog config-id))
               (error (e)
                 (format t ";; Warning: Failed to create Stripe billing portal configuration: ~A~%" e)
                 nil))))
      catalog))

(defun init-stripe-product ()
  "Initialize the Stripe subscription products (CADR, LAMBDA) and billing
portal configuration on server startup. Builds the new catalog up as a
sequence of pure `(catalog, ...) -> new-catalog' transformations (via
ENSURE-TIER-PRODUCT/ENSURE-BILLING-PORTAL-CONFIGURATION) and only then
installs the finished result as *STRIPE-CATALOG* in one atomic SETF --
the catalog can never be observed half-updated partway through this dance."
  (let ((secret-key (get-stripe-secret-key)))
    (if (or (null secret-key) (string= secret-key ""))
        (format t ";; Warning: STRIPE_SECRET_KEY is not set. Skipping Stripe product initialization.~%")
        (setf *stripe-catalog*
              (ensure-billing-portal-configuration
               (fold:fold-left (lambda (catalog plan)
                                  (destructuring-bind (tier product-name unit-amount) plan
                                    (ensure-tier-product catalog tier product-name unit-amount)))
                                (make-stripe-catalog)
                                *stripe-tier-plans*))))))

(defun create-stripe-checkout-session (username tier &optional next)
  "Create a Managed Payments Checkout Session for USERNAME to subscribe to TIER
(\"CADR\" or \"LAMBDA\") and return an outcome value: (OK checkout-url) on
success, or (ERR reason) on failure -- see FUNCTIONAL_REFACTOR.md Phase 6.
USERNAME is passed through as Stripe's `client_reference_id' only (an
opaque string we choose); Stripe's hosted Checkout page collects the
customer's actual billing email itself, so this application never stores
or transmits an email address to Stripe. If NEXT is given, the success
URL carries it along as a breadcrumb so the dashboard can redirect the
user back to the originally-requested JWT-protected page (re-issuing
their JWT first) instead of just landing on the dashboard."
  (let ((price-id (tier-price-id tier)))
    (cond
      ((not (stripe-secret-key-configured-p (get-stripe-secret-key)))
       (format t ";; Warning: STRIPE_SECRET_KEY is not set.~%")
       (err "Stripe is not configured (missing STRIPE_SECRET_KEY)."))
      ((null price-id)
       (format t ";; Error: No Stripe price configured for tier ~A. Initializing now...~%" tier)
       (init-stripe-product)
       (if (tier-price-id tier)
           (create-stripe-checkout-session username tier next)
           (progn
             (format t ";; Error: Stripe price for tier ~A still not available after retry.~%" tier)
             (err (format nil "No Stripe price configured for tier ~A." tier)))))
      (t
       (or (with-stripe-credentials (headers)
             (handler-case
                 (let* ((url "https://api.stripe.com/v1/checkout/sessions")
                        (base-url (get-base-url))
                        (success-url (format nil "~A/dashboard?checkout_status=success~@[&next=~A~]"
                                             base-url (and next (hunchentoot:url-encode next))))
                        (cancel-url (format nil "~A/dashboard?checkout_status=cancel" base-url))
                        (content (list (cons "mode" "subscription")
                                       (cons "success_url" success-url)
                                       (cons "cancel_url" cancel-url)
                                       (cons "line_items[0][price]" price-id)
                                       (cons "line_items[0][quantity]" "1")
                                       (cons "client_reference_id" username)
                                       (cons "subscription_data[metadata][tier]" tier)
                                       (cons "managed_payments[enabled]" "true")))
                        (response (dex:post url :headers headers :content content))
                        (json (cl-json:decode-json-from-string response))
                        (checkout-url (cdr (assoc :url json))))
                   (if checkout-url
                       (ok checkout-url)
                       (err "Stripe did not return a checkout URL.")))
               (error (e)
                 (format t ";; Error: Failed to create Stripe checkout session: ~A~%" e)
                 (err (format nil "Failed to create Stripe checkout session: ~A" e)))))
           (err "Stripe is not configured (missing STRIPE_SECRET_KEY)."))))))


(defun create-billing-portal-session (customer-id)
  "Create a Billing Portal session for CUSTOMER-ID (self-service upgrade,
downgrade, or cancellation with pro-rated refund) and return its URL."
  (and customer-id
       (with-stripe-credentials (headers)
         (handler-case
             (let* ((url "https://api.stripe.com/v1/billing_portal/sessions")
                    (return-url (format nil "~A/dashboard" (get-base-url)))
                    (config-id (stripe-catalog-billing-portal-configuration-id *stripe-catalog*))
                    (content (append (list (cons "customer" customer-id)
                                           (cons "return_url" return-url))
                                     (when config-id
                                       (list (cons "configuration" config-id))))))
               (let* ((response (dex:post url :headers headers :content content))
                      (json (cl-json:decode-json-from-string response)))
                 (cdr (assoc :url json))))
           (error (e)
             (format t ";; Error: Failed to create Stripe billing portal session: ~A~%" e)
             nil)))))

(defun get-stripe-subscription-tier (subscription-id)
  "Fetch SUBSCRIPTION-ID from Stripe and return the membership tier
corresponding to its current price, or NIL if it can't be determined."
  (and subscription-id
       (with-stripe-credentials (headers)
         (handler-case
             (let* ((url (format nil "https://api.stripe.com/v1/subscriptions/~A" subscription-id))
                    (response (dex:get url :headers headers))
                    (json (cl-json:decode-json-from-string response))
                    (items (cdr (assoc :data (cdr (assoc :items json)))))
                    (first-item (first items))
                    (price (cdr (assoc :price first-item)))
                    (price-id (cdr (assoc :id price))))
               (tier-from-price-id price-id))
           (error (e)
             (format t ";; Error: Failed to fetch Stripe subscription ~A: ~A~%" subscription-id e)
             nil)))))

(defun stripe-get-json (url headers)
  "GET URL from Stripe and return the decoded JSON body."
  (cl-json:decode-json-from-string (dex:get url :headers headers)))

(defun refund-latest-invoice-charge (headers invoice-id amount)
  "Refund AMOUNT (in cents) from the charge behind INVOICE-ID. Returns the
refund's ID, or NIL if there was nothing to refund."
  (when (and invoice-id (plusp amount))
    (let* ((invoice (stripe-get-json (format nil "https://api.stripe.com/v1/invoices/~A" invoice-id) headers))
           (charge-id (or (cdr (assoc :charge invoice))
                          (let ((payment-intent-id (cdr (assoc :payment--intent invoice))))
                            (and payment-intent-id
                                 (cdr (assoc :latest--charge
                                             (stripe-get-json (format nil "https://api.stripe.com/v1/payment_intents/~A" payment-intent-id) headers))))))))
      (when charge-id
        (let* ((url "https://api.stripe.com/v1/refunds")
               (content (list (cons "charge" charge-id)
                              (cons "amount" (princ-to-string amount))
                              (cons "reason" "requested_by_customer")))
               (response (dex:post url :headers headers :content content))
               (json (cl-json:decode-json-from-string response)))
          (cdr (assoc :id json)))))))

(defun cancel-stripe-subscription-with-prorated-refund (subscription-id)
  "Immediately cancel SUBSCRIPTION-ID and refund the customer a pro-rated
amount for the unused remainder of the current billing period. Returns the
refunded amount in cents (0 if nothing was due), or NIL on failure."
  (and subscription-id
       (with-stripe-credentials (headers)
         (handler-case
             (let* ((subscription (stripe-get-json (format nil "https://api.stripe.com/v1/subscriptions/~A" subscription-id) headers))
                    (period-start (cdr (assoc :current--period--start subscription)))
                    (period-end (cdr (assoc :current--period--end subscription)))
                    (latest-invoice-id (cdr (assoc :latest--invoice subscription)))
                    (items (cdr (assoc :data (cdr (assoc :items subscription)))))
                    (unit-amount (cdr (assoc :unit--amount (cdr (assoc :price (first items))))))
                    (now (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))
                    (total-seconds (and period-start period-end (- period-end period-start)))
                    (remaining-seconds (and period-end (max 0 (- period-end now))))
                    (refund-amount (if (and unit-amount total-seconds (plusp total-seconds))
                                       (round (* unit-amount (/ remaining-seconds total-seconds)))
                                       0)))
               (when (plusp refund-amount)
                 (refund-latest-invoice-charge headers latest-invoice-id refund-amount))
               ;; Cancel the subscription immediately; the pro-rated refund above
               ;; already returned the unused balance, so no further Stripe-side
               ;; proration/credit is needed.
               (dex:delete (format nil "https://api.stripe.com/v1/subscriptions/~A" subscription-id) :headers headers)
               refund-amount)
           (error (e)
             (format t ";; Error: Failed to cancel/refund Stripe subscription ~A: ~A~%" subscription-id e)
             nil)))))

;; -- WEBHOOK SIGNATURE VERIFICATION --
;; See https://docs.stripe.com/webhooks#verify-events

(defun parse-stripe-signature-header (header)
  "Parse a Stripe-Signature header into an alist, e.g. ((\"t\" . \"1614556800\")
(\"v1\" . \"abc...\") (\"v1\" . \"def...\"))."
  (loop for part in (cl-ppcre:split "," header)
        for eq-pos = (position #\= part)
        when eq-pos
          collect (cons (subseq part 0 eq-pos) (subseq part (1+ eq-pos)))))

(defun hmac-sha256-hex (secret message)
  "Compute the hex-encoded HMAC-SHA256 of MESSAGE using SECRET."
  (let ((hmac (ironclad:make-mac :hmac (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-mac hmac (ironclad:ascii-string-to-byte-array message))
    (ironclad:byte-array-to-hex-string (ironclad:produce-mac hmac))))

(defun verify-stripe-webhook-signature (payload signature-header &key (tolerance 300))
  "Verify that PAYLOAD (the raw request body) was signed by Stripe according
to SIGNATURE-HEADER (the value of the Stripe-Signature header). Returns T if
valid, NIL otherwise."
  (let ((secret (get-stripe-webhook-secret)))
    (and secret
         (not (string= secret ""))
         signature-header
         (let* ((parts (parse-stripe-signature-header signature-header))
                (timestamp (cdr (assoc "t" parts :test #'string=)))
                (candidates (mapcar #'cdr (remove "v1" parts :key #'car :test-not #'string=))))
           (and timestamp
                candidates
                (<= (abs (- (get-universal-time)
                            (+ (parse-integer timestamp) 2208988800)))
                    tolerance)
                (let ((expected (hmac-sha256-hex secret (format nil "~A.~A" timestamp payload))))
                  (some (lambda (candidate) (string= expected candidate)) candidates)))))))
