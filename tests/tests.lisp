;;; -*- Lisp -*-

(in-package "JRM-CODE-PROJECT/TESTS")

(def-suite jrm-suite
  :description "Main test suite for jrm-code-project")

(in-suite jrm-suite)

(test basic-sanity
  "Verify basic lisp system arithmetic sanity."
  (is (= 2 (+ 1 1)))
  (is-true t))

(test package-exports
  "Verify that package symbols are exported correctly and bound."
  (is (fboundp 'jrm-code-project:main))
  (is (fboundp 'jrm-code-project::start-server))
  (is (fboundp 'jrm-code-project::stop-server))
  (is (fboundp 'jrm-code-project::signup-page))
  (is (fboundp 'jrm-code-project::setup-2fa-page))
  (is (fboundp 'jrm-code-project::regenerate-recovery-page))
  (is (fboundp 'jrm-code-project::regenerate-2fa-page)))

(test static-resources-exist
  "Ensure that critical static HTML resources are located correctly under the ASDF system."
  (let* ((html-root (asdf:system-relative-pathname :jrm-code-project "resources/www/html/"))
         (index-file (merge-pathnames "index.html" html-root))
         (css-file (merge-pathnames "css/style.css" html-root)))
    (is (probe-file html-root))
    (is (probe-file index-file))
    (is (probe-file css-file))))

(test row->user-conversion
  "Verify JRM-AUTH::ROW->USER converts a hand-built users-table alist row
into an immutable USER struct with the expected accessors, without
requiring a live database connection."
  (let ((user (jrm-auth::row->user
               '((:username . "row-fixture@domain.com")
                 (:password-hash . "deadbeef$c0ffee")
                 (:totp-secret . nil)
                 (:auth-state . "active")
                 (:stripe-customer-id . "cus_fixture")
                 (:stripe-subscription-id . nil)
                 (:subscription-status . "inactive")
                 (:membership-tier . "CADR")
                 (:wheel . t)))))
    (is (typep user 'jrm-auth:user))
    (is (string= "row-fixture@domain.com" (jrm-auth:user-username user)))
    (is (string= "deadbeef$c0ffee" (jrm-auth:user-password-hash user)))
    (is (null (jrm-auth:user-totp-secret user)))
    (is (string= "active" (jrm-auth:user-auth-state user)))
    (is (string= "cus_fixture" (jrm-auth:user-stripe-customer-id user)))
    (is (null (jrm-auth:user-stripe-subscription-id user)))
    (is (string= "inactive" (jrm-auth:user-subscription-status user)))
    (is (string= "CADR" (jrm-auth:user-membership-tier user)))
    (is-true (jrm-auth:user-wheel-p user))))

(test password-hashing-pure
  "Verify JRM-AUTH::HASH-PASSWORD/CHECK-PASSWORD (db-auth.lisp) are a pure,
salted-hash pair requiring no database: same password + same salt always
hashes identically, different salts still verify correctly, and the wrong
password is rejected."
  (let ((hash1 (jrm-auth::hash-password "correct horse battery staple"))
        (hash2 (jrm-auth::hash-password "correct horse battery staple")))
    ;; Two hashes of the same password (fresh random salts) should differ...
    (is (not (string= hash1 hash2)))
    ;; ...but each should still verify correctly against its own hash.
    (is-true (jrm-auth::check-password "correct horse battery staple" hash1))
    (is-true (jrm-auth::check-password "correct horse battery staple" hash2))
    ;; The wrong password must fail.
    (is-false (jrm-auth::check-password "wrong password" hash1)))
  ;; Hashing with an explicit salt is deterministic.
  (let* ((salt-hex (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))
         (hash-a (jrm-auth::hash-password "same-password" salt-hex))
         (hash-b (jrm-auth::hash-password "same-password" salt-hex)))
    (is (string= hash-a hash-b))))

(test jwt-encode-decode-round-trip-pure
  "Verify ENCODE-JWT/DECODE-JWT (jwt.lisp) round-trip claims correctly, that
DECODE-JWT rejects a tampered signature, and that it rejects an expired
token -- with no database or Hunchentoot request required (an explicit
SECRET is passed so no environment variable is needed)."
  (let* ((secret "test-jwt-secret")
         (now (jrm-code-project::unix-time))
         (claims (list (cons "sub" "user@domain.com") (cons "tier" "CADR")
                       (cons "wheel" nil) (cons "iat" now) (cons "exp" (+ now 3600))))
         (token (jrm-code-project::encode-jwt claims :secret secret)))
    (let ((decoded (jrm-code-project::decode-jwt token :secret secret)))
      (is (string= "user@domain.com" (cdr (assoc :sub decoded))))
      (is (string= "CADR" (cdr (assoc :tier decoded)))))
    ;; Wrong secret -> signature mismatch -> NIL.
    (is (null (jrm-code-project::decode-jwt token :secret "wrong-secret")))
    ;; Tampered payload segment (flip one character) -> signature mismatch -> NIL.
    (let* ((parts (uiop:split-string token :separator "."))
           (payload-b64 (second parts))
           (tampered-payload (concatenate 'string
                                          (subseq payload-b64 0 (1- (length payload-b64)))
                                          (if (char= (char payload-b64 (1- (length payload-b64))) #\A) "B" "A")))
           (tampered (format nil "~A.~A.~A" (first parts) tampered-payload (third parts))))
      (is (null (jrm-code-project::decode-jwt tampered :secret secret))))
    ;; Expired token -> NIL.
    (let* ((expired-claims (list (cons "sub" "user@domain.com") (cons "exp" (- now 10))))
           (expired-token (jrm-code-project::encode-jwt expired-claims :secret secret)))
      (is (null (jrm-code-project::decode-jwt expired-token :secret secret))))))

(test tier-rank-and-minimum
  "Verify the pure tier comparison logic in tier.lisp."
  (is (= 0 (jrm-code-project::tier-rank "CONS")))
  (is (= 1 (jrm-code-project::tier-rank "CADR")))
  (is (= 2 (jrm-code-project::tier-rank "LAMBDA")))
  ;; Unrecognized tiers rank as the lowest/free tier.
  (is (= 0 (jrm-code-project::tier-rank "BOGUS")))
  (is (= 0 (jrm-code-project::tier-rank nil)))
  ;; Case-insensitive comparison.
  (is (= 1 (jrm-code-project::tier-rank "cadr")))
  (is-true (jrm-code-project::tier-meets-minimum-p "LAMBDA" "CADR"))
  (is-true (jrm-code-project::tier-meets-minimum-p "CADR" "CADR"))
  (is-false (jrm-code-project::tier-meets-minimum-p "CONS" "CADR")))

(test random-string-series-pure
  "Verify JRM-AUTH::RANDOM-STRING (rewritten as a SERIES scan/collect
pipeline in FUNCTIONAL_REFACTOR.md Phase 7) returns strings of the
requested length drawn only from its unambiguous uppercase-alphanumeric
alphabet, with no database required."
  (dotimes (i 20)
    (let ((s (jrm-auth::random-string 8)))
      (is (= 8 (length s)))
      (is (every (lambda (c) (find c "ABCDEFGHJKLMNPQRSTUVWXYZ23456789")) s))))
  (is (string= "" (jrm-auth::random-string 0))))

(test admin-pagination-pure
  "Verify COMPUTE-ADMIN-PAGINATION (admin.lisp) derives correct pure
pagination data from a raw `page' query-parameter string, page size, and
total count -- no database or Hunchentoot request required."
  ;; Middle page: both prev and next available.
  (let ((p (jrm-code-project::compute-admin-pagination "2" 20 55)))
    (is (= 2 (jrm-code-project::admin-pagination-page-num p)))
    (is (= 40 (jrm-code-project::admin-pagination-offset p)))
    (is (= 3 (jrm-code-project::admin-pagination-total-pages p)))
    (is-true (jrm-code-project::admin-pagination-has-prev p))
    (is-false (jrm-code-project::admin-pagination-has-next p)))
  ;; First page: no prev, has next.
  (let ((p (jrm-code-project::compute-admin-pagination "0" 20 55)))
    (is-false (jrm-code-project::admin-pagination-has-prev p))
    (is-true (jrm-code-project::admin-pagination-has-next p)))
  ;; Missing/garbage page param defaults to page 0.
  (let ((p (jrm-code-project::compute-admin-pagination nil 20 5)))
    (is (= 0 (jrm-code-project::admin-pagination-page-num p))))
  (let ((p (jrm-code-project::compute-admin-pagination "not-a-number" 20 5)))
    (is (= 0 (jrm-code-project::admin-pagination-page-num p))))
  ;; Zero total still yields at least one (empty) page.
  (let ((p (jrm-code-project::compute-admin-pagination "0" 20 0)))
    (is (= 1 (jrm-code-project::admin-pagination-total-pages p)))
    (is-false (jrm-code-project::admin-pagination-has-next p))))

(test stripe-catalog-with-tier-pure
  "Verify CATALOG-WITH-TIER and CATALOG-WITH-BILLING-PORTAL-CONFIGURATION-ID
build up an immutable STRIPE-CATALOG purely from hand-built fixtures, with
no live Stripe API calls or global mutation required."
  (let* ((empty (jrm-code-project::make-stripe-catalog))
         (with-cadr (jrm-code-project::catalog-with-tier empty "CADR" "price_cadr" "prod_cadr")))
    ;; The empty catalog starts with nothing recorded.
    (is (null (jrm-code-project::stripe-catalog-tier-price-ids empty)))
    (is (null (jrm-code-project::stripe-catalog-tier-product-ids empty)))
    (is (null (jrm-code-project::stripe-catalog-price-id-tiers empty)))
    (is (null (jrm-code-project::stripe-catalog-billing-portal-configuration-id empty)))
    ;; CATALOG-WITH-TIER returns a *new* catalog; EMPTY itself is untouched.
    (is (null (jrm-code-project::stripe-catalog-tier-price-ids empty)))
    (is (string= "price_cadr" (cdr (assoc "CADR" (jrm-code-project::stripe-catalog-tier-price-ids with-cadr) :test #'string-equal))))
    (is (string= "prod_cadr" (cdr (assoc "CADR" (jrm-code-project::stripe-catalog-tier-product-ids with-cadr) :test #'string-equal))))
    (is (string= "CADR" (cdr (assoc "price_cadr" (jrm-code-project::stripe-catalog-price-id-tiers with-cadr) :test #'string=))))
    ;; Adding a second tier preserves the first.
    (let ((with-both (jrm-code-project::catalog-with-tier with-cadr "LAMBDA" "price_lambda" "prod_lambda")))
      (is (string= "price_cadr" (cdr (assoc "CADR" (jrm-code-project::stripe-catalog-tier-price-ids with-both) :test #'string-equal))))
      (is (string= "price_lambda" (cdr (assoc "LAMBDA" (jrm-code-project::stripe-catalog-tier-price-ids with-both) :test #'string-equal))))
      ;; WITH-CADR itself remains unaffected by building WITH-BOTH from it.
      (is (null (cdr (assoc "LAMBDA" (jrm-code-project::stripe-catalog-tier-price-ids with-cadr) :test #'string-equal))))
      ;; Configuring the billing portal ID preserves the tier data.
      (let ((configured (jrm-code-project::catalog-with-billing-portal-configuration-id with-both "bpc_123")))
        (is (string= "bpc_123" (jrm-code-project::stripe-catalog-billing-portal-configuration-id configured)))
        (is (string= "price_cadr" (cdr (assoc "CADR" (jrm-code-project::stripe-catalog-tier-price-ids configured) :test #'string-equal))))
        (is (null (jrm-code-project::stripe-catalog-billing-portal-configuration-id with-both)))))))

(test stripe-tier-price-id-lookups-use-current-catalog
  "Verify TIER-PRICE-ID/TIER-FROM-PRICE-ID read through the small accessor
functions that close over *STRIPE-CATALOG*, so swapping in a hand-built
catalog changes their answers without any live Stripe call."
  (let ((jrm-code-project::*stripe-catalog*
          (jrm-code-project::catalog-with-tier
           (jrm-code-project::make-stripe-catalog) "CADR" "price_test_cadr" "prod_test_cadr")))
    (is (string= "price_test_cadr" (jrm-code-project::tier-price-id "CADR")))
    (is (string= "price_test_cadr" (jrm-code-project::tier-price-id "cadr")))
    (is (null (jrm-code-project::tier-price-id "LAMBDA")))
    (is (string= "CADR" (jrm-code-project::tier-from-price-id "price_test_cadr")))
    (is (null (jrm-code-project::tier-from-price-id "price_unknown")))))

(test with-stripe-credentials-combinator
  "Verify WITH-STRIPE-CREDENTIALS evaluates its body (with HEADERS-VAR bound)
only when STRIPE_SECRET_KEY is configured, otherwise returning NIL without
evaluating the body -- exercised by stubbing GET-STRIPE-SECRET-KEY rather
than mutating real environment variables."
  (let ((original (fdefinition 'jrm-code-project::get-stripe-secret-key)))
    (let ((body-called-p nil))
      (unwind-protect
           (progn
             (setf (fdefinition 'jrm-code-project::get-stripe-secret-key) (lambda () "sk_test_123"))
             (let ((result (jrm-code-project::with-stripe-credentials (headers)
                             (setf body-called-p t)
                             (is (assoc "Authorization" headers :test #'string=))
                             :body-ran)))
               (is-true body-called-p)
               (is (eq :body-ran result))))
        (setf (fdefinition 'jrm-code-project::get-stripe-secret-key) original)))
    (let ((body-called-p nil))
      (unwind-protect
           (progn
             (setf (fdefinition 'jrm-code-project::get-stripe-secret-key) (lambda () ""))
             (let ((result (jrm-code-project::with-stripe-credentials (headers)
                             (declare (ignore headers))
                             (setf body-called-p t)
                             :body-ran)))
               (is-false body-called-p)
               (is (null result))))
        (setf (fdefinition 'jrm-code-project::get-stripe-secret-key) original)))))

(test dashboard-view-model-pure
  "Verify DASHBOARD-VIEW-MODEL derives correct pure view data from hand-built
USER fixtures, with no database or Hunchentoot request required."
  ;; A CADR-tier user with no Stripe customer yet, no checkout-status/next.
  (let* ((user (jrm-auth::row->user
                '((:username . "vm-fixture@domain.com")
                  (:password-hash . "x")
                  (:totp-secret . nil)
                  (:auth-state . "active")
                  (:stripe-customer-id . nil)
                  (:stripe-subscription-id . nil)
                  (:subscription-status . "active")
                  (:membership-tier . "CADR")
                  (:wheel . nil))))
         (vm (jrm-code-project::dashboard-view-model user nil nil)))
    (is (string= "vm-fixture@domain.com" (jrm-code-project::dashboard-view-model-username vm)))
    (is (string= "CADR" (jrm-code-project::dashboard-view-model-tier vm)))
    (is-false (jrm-code-project::dashboard-view-model-wheel-p vm))
    (is (null (jrm-code-project::dashboard-view-model-notification vm)))
    (is (null (jrm-code-project::dashboard-view-model-redirect-to vm)))
    (is-true (jrm-code-project::tier-card-view-active-p (jrm-code-project::dashboard-view-model-cadr-card vm)))
    (is (eq :active (jrm-code-project::tier-card-view-class (jrm-code-project::dashboard-view-model-cadr-card vm))))
    (is (eq :current (jrm-code-project::tier-card-view-badge (jrm-code-project::dashboard-view-model-cadr-card vm))))
    (is (eq :active (jrm-code-project::tier-card-view-button (jrm-code-project::dashboard-view-model-cadr-card vm))))
    (is-false (jrm-code-project::tier-card-view-active-p (jrm-code-project::dashboard-view-model-cons-card vm)))
    ;; No Stripe customer yet, so CONS's button is plain :disabled, not :manage-subscription.
    (is (eq :disabled (jrm-code-project::tier-card-view-button (jrm-code-project::dashboard-view-model-cons-card vm)))))

  ;; A CONS-tier user WITH a Stripe customer id gets a manage-subscription button.
  (let* ((user (jrm-auth::row->user
                '((:username . "vm-fixture2@domain.com")
                  (:password-hash . "x")
                  (:totp-secret . nil)
                  (:auth-state . "active")
                  (:stripe-customer-id . "cus_abc")
                  (:stripe-subscription-id . nil)
                  (:subscription-status . "canceled")
                  (:membership-tier . "CONS")
                  (:wheel . t))))
         (vm (jrm-code-project::dashboard-view-model user "success" "/some/next")))
    (is-true (jrm-code-project::dashboard-view-model-wheel-p vm))
    (is (eq :success (jrm-code-project::dashboard-view-model-notification vm)))
    ;; success + next -> should redirect straight to next.
    (is (string= "/some/next" (jrm-code-project::dashboard-view-model-redirect-to vm)))
    (is (eq :active (jrm-code-project::tier-card-view-button (jrm-code-project::dashboard-view-model-cons-card vm))))

    ;; cancel notification, no next -> no redirect.
    (let ((vm2 (jrm-code-project::dashboard-view-model user "cancel" nil)))
      (is (eq :cancel (jrm-code-project::dashboard-view-model-notification vm2)))
      (is (null (jrm-code-project::dashboard-view-model-redirect-to vm2))))))

(test webhook-event->db-commands-pure
  "Verify WEBHOOK-EVENT->DB-COMMANDS derives correct pure command
descriptions from hand-built Stripe webhook JSON fixtures, with no live
Stripe payload or database required."
  ;; checkout.session.completed with all required fields present.
  (let* ((json '((:type . "checkout.session.completed")
                 (:data . ((:object . ((:client--reference--id . "user@domain.com")
                                        (:customer . "cus_123")
                                        (:subscription . "sub_456"))))))))
         (let ((commands (jrm-code-project::webhook-event->db-commands json)))
           (is (= 1 (length commands)))
           (let ((cmd (first commands)))
             (is (eq :update-subscription (getf cmd :op)))
             (is (string= "user@domain.com" (getf cmd :username)))
             (is (string= "cus_123" (getf cmd :customer-id)))
             (is (string= "sub_456" (getf cmd :subscription-id))))))

  ;; checkout.session.completed missing required fields -> a :log command, no DB command.
  (let* ((json '((:type . "checkout.session.completed")
                 (:data . ((:object . ((:customer . "cus_123"))))))))
    (let ((commands (jrm-code-project::webhook-event->db-commands json)))
      (is (= 1 (length commands)))
      (is (eq :log (getf (first commands) :op)))))

  ;; customer.subscription.updated.
  (let* ((json '((:type . "customer.subscription.updated")
                 (:data . ((:object . ((:customer . "cus_789")
                                        (:status . "active")
                                        (:items . ((:data . (((:price . ((:id . "price_cadr")))))))))))))))
    (let ((commands (jrm-code-project::webhook-event->db-commands json)))
      (is (= 1 (length commands)))
      (let ((cmd (first commands)))
        (is (eq :update-tier-status-by-customer (getf cmd :op)))
        (is (string= "cus_789" (getf cmd :customer-id)))
        (is (string= "active" (getf cmd :status)))
        (is (eq :lookup-from-price-id (getf cmd :tier)))
        (is (string= "price_cadr" (getf cmd :price-id-for-tier-lookup))))))

  ;; customer.subscription.deleted.
  (let* ((json '((:type . "customer.subscription.deleted")
                 (:data . ((:object . ((:customer . "cus_999"))))))))
    (let ((commands (jrm-code-project::webhook-event->db-commands json)))
      (is (= 1 (length commands)))
      (let ((cmd (first commands)))
        (is (eq :cancel-subscription-by-customer (getf cmd :op)))
        (is (string= "cus_999" (getf cmd :customer-id))))))

  ;; invoice.payment_failed.
  (let* ((json '((:type . "invoice.payment_failed")
                 (:data . ((:object . ((:customer . "cus_111"))))))))
    (let ((commands (jrm-code-project::webhook-event->db-commands json)))
      (is (= 1 (length commands)))
      (let ((cmd (first commands)))
        (is (eq :mark-past-due-by-customer (getf cmd :op)))
        (is (string= "cus_111" (getf cmd :customer-id))))))

  ;; Unrecognized event type -> no commands.
  (let* ((json '((:type . "some.other.event") (:data . ((:object . nil))))))
    (is (null (jrm-code-project::webhook-event->db-commands json)))))

(test outcome-result-failure-pure
  "Verify the RESULT/FAILURE outcome-value combinators in outcome.lisp: OK,
ERR, OUTCOME-OK-P, OUTCOME-VALUE, OUTCOME-REASON. No I/O required."
  (let ((success (jrm-code-project::ok 42))
        (failure (jrm-code-project::err "went wrong")))
    (is-true (jrm-code-project::outcome-p success))
    (is-true (jrm-code-project::outcome-p failure))
    (is-true (jrm-code-project::outcome-ok-p success))
    (is-false (jrm-code-project::outcome-ok-p failure))
    (is (= 42 (jrm-code-project::outcome-value success)))
    (is (eq :default (jrm-code-project::outcome-value failure :default)))
    (is (string= "went wrong" (jrm-code-project::outcome-reason failure)))
    (is (string= "Unknown error" (jrm-code-project::outcome-reason success)))
    (is (string= "custom" (jrm-code-project::outcome-reason success "custom")))))

(test execute-webhook-db-command-outcomes
  "Verify EXECUTE-WEBHOOK-DB-COMMAND (billing.lisp) returns explicit
RESULT/FAILURE outcome values (FUNCTIONAL_REFACTOR.md Phase 6) rather than
silently logging. The :LOG case (produced when WEBHOOK-EVENT->DB-COMMANDS
could not find required fields) never touches the database and is always a
FAILURE. The customer-lookup cases require a live Postgres connection (like
the other JRM-AUTH:*-backed tests in this suite) to confirm a nonexistent
Stripe customer id resolves to a clean FAILURE outcome rather than an error."
  (let ((outcome (jrm-code-project::execute-webhook-db-command
                  (list :op :log :message "Missing required fields."))))
    (is-false (jrm-code-project::outcome-ok-p outcome))
    (is (string= "Missing required fields." (jrm-code-project::outcome-reason outcome))))
  (let ((outcome (jrm-code-project::execute-webhook-db-command
                  (list :op :cancel-subscription-by-customer :customer-id "cus_does_not_exist"))))
    (is-false (jrm-code-project::outcome-ok-p outcome))
    (is (search "cus_does_not_exist" (jrm-code-project::outcome-reason outcome))))
  (let ((outcome (jrm-code-project::execute-webhook-db-command
                  (list :op :mark-past-due-by-customer :customer-id "cus_does_not_exist"))))
    (is-false (jrm-code-project::outcome-ok-p outcome))))

(test require-guard-combinator
  "Verify JRM-CODE-PROJECT::REQUIRE-GUARD calls CHECK, returns its success
value without touching ON-FAILURE when CHECK succeeds, and calls
ON-FAILURE (returning NIL) when CHECK fails -- with no Hunchentoot request
or redirect required."
  (let ((on-failure-called-p nil))
    (let ((result (jrm-code-project::require-guard
                   (lambda () :some-success-value)
                   (lambda () (setf on-failure-called-p t)))))
      (is (eq :some-success-value result))
      (is-false on-failure-called-p)))
  (let ((on-failure-called-p nil))
    (let ((result (jrm-code-project::require-guard
                   (lambda () nil)
                   (lambda () (setf on-failure-called-p t)))))
      (is (null result))
      (is-true on-failure-called-p))))

(test wrap-csrf-protected-combinator
  "Verify JRM-CODE-PROJECT::WRAP-CSRF-PROTECTED calls THUNK only when
CSRF-TOKEN-VALID-P is true, otherwise returning the 403 response without
calling THUNK. Stubs CSRF-TOKEN-VALID-P rather than exercising real
Hunchentoot session/POST-parameter state."
  ;; CSRF-TOKEN-VALID-P depends on Hunchentoot's dynamic request/session
  ;; context, so temporarily replace its function definition rather than
  ;; standing up a real request; always restored via UNWIND-PROTECT.
  (let ((original (fdefinition 'jrm-code-project::csrf-token-valid-p)))
    (let ((thunk-called-p nil))
      (unwind-protect
           (progn
             (setf (fdefinition 'jrm-code-project::csrf-token-valid-p) (lambda () t))
             (let ((result (jrm-code-project::wrap-csrf-protected
                            (lambda () (setf thunk-called-p t) :ok))))
               (is-true thunk-called-p)
               (is (eq :ok result))))
        (setf (fdefinition 'jrm-code-project::csrf-token-valid-p) original)))
    (let ((thunk-called-p nil))
      (unwind-protect
           (progn
             (setf (fdefinition 'jrm-code-project::csrf-token-valid-p) (lambda () nil))
             (let* ((hunchentoot:*reply* (make-instance 'hunchentoot:reply))
                    (result (jrm-code-project::wrap-csrf-protected
                             (lambda () (setf thunk-called-p t) :ok))))
               (is-false thunk-called-p)
               (is (stringp result))
               (is (search "403" result))))
        (setf (fdefinition 'jrm-code-project::csrf-token-valid-p) original)))))

(test html-rendering-combinators-pure
  "Verify the pure HTML rendering combinators in views.lisp: HTML-ESCAPE,
HTML-PAGE, HTML-NOTIFICATION, and HTML-FORM. No I/O required."
  ;; HTML-ESCAPE.
  (is (string= "" (jrm-code-project::html-escape nil)))
  (is (string= "&lt;script&gt;" (jrm-code-project::html-escape "<script>")))
  (is (string= "42" (jrm-code-project::html-escape 42)))

  ;; HTML-PAGE splices title (escaped) and body into standard chrome.
  (let ((page (jrm-code-project::html-page "My <Title>" "<p>body</p>")))
    (is (search "My &lt;Title&gt;" page))
    (is (search "<p>body</p>" page))
    (is (search "<html>" page)))
  (let ((page (jrm-code-project::html-page "T" "<p>x</p>" :extra-style ".foo { color: red; }")))
    (is (search ".foo { color: red; }" page)))

  ;; HTML-NOTIFICATION.
  (is (string= "" (jrm-code-project::html-notification nil "unused")))
  (let ((n (jrm-code-project::html-notification :success "Yay <b>done</b>")))
    (is (search "notification-success" n))
    (is (search "Yay &lt;b&gt;done&lt;/b&gt;" n)))
  (let ((n (jrm-code-project::html-notification :cancel "Nope")))
    (is (search "notification-cancel" n)))

  ;; HTML-FORM: default GET-less POST form, with CSRF token spliced in first.
  (let ((form (jrm-code-project::html-form "/do-thing" "<button>Go</button>" :csrf-token "<input type='hidden' name='csrf'>")))
    (is (search "action='/do-thing'" form))
    (is (search "method='POST'" form))
    (is (< (search "csrf" form) (search "Go" form)))))

(test render-member-row-pure
  "Verify RENDER-MEMBER-ROW (admin.lisp) is a pure (USER -> HTML) function:
given a hand-built USER fixture, it renders the expected tier selection,
subscription status, and wheel/refund controls with values HTML-escaped,
requiring no database or Hunchentoot session."
  (let ((original (fdefinition 'jrm-code-project::csrf-input-html)))
    (unwind-protect
        (progn
          (setf (fdefinition 'jrm-code-project::csrf-input-html)
                (lambda () "<input type='hidden' name='csrf-token' value='stub'>"))
          (let ((member (jrm-auth::row->user
                         '((:username . "member@domain.com")
                           (:password-hash . "x")
                           (:totp-secret . nil)
                           (:auth-state . "active")
                           (:stripe-customer-id . "cus_1")
                           (:stripe-subscription-id . "sub_1")
                           (:subscription-status . "active")
                           (:membership-tier . "CADR")
                           (:wheel . nil)))))
            (let ((row (jrm-code-project::render-member-row member)))
              (is (search "member@domain.com" row))
              (is (search "option value='CADR' selected" row))
              (is (search "active" row))
              (is (search "(paid)" row))
              (is (search "Make Wheel" row))
              (is (search "Refund/Delete" row))))
          ;; A wheel member with no subscription: "Revoke Wheel" and plain "Refund".
          (let ((member (jrm-auth::row->user
                         '((:username . "wheel@domain.com")
                           (:password-hash . "x")
                           (:totp-secret . nil)
                           (:auth-state . "active")
                           (:stripe-customer-id . nil)
                           (:stripe-subscription-id . nil)
                           (:subscription-status . "inactive")
                           (:membership-tier . "CONS")
                           (:wheel . t)))))
            (let ((row (jrm-code-project::render-member-row member)))
              (is (search "Revoke Wheel" row))
              (is (not (search "(paid)" row)))
              (is (search ">Refund<" row)))))
      (setf (fdefinition 'jrm-code-project::csrf-input-html) original))))

(test recovery-code-verification
  "Verify generating, checking, and consuming recovery codes in the database."
  (let ((username "test_recovery@domain.com")
        (password "SomePassword123!"))
    ;; Ensure DB is initialized
    (ignore-errors (jrm-auth:init-db))
    ;; Clean up any residual user
    (jrm-auth:delete-user username)
    (is-true (jrm-auth:create-user username password))
    (let ((codes (jrm-auth:generate-recovery-codes username)))
      (is (= 10 (length codes)))
      ;; Verify that an invalid code fails
      (is-false (jrm-auth:verify-recovery-code username "INVALID-CODE"))
      ;; Verify that a valid code succeeds
      (let ((test-code (car codes)))
        (is-true (jrm-auth:verify-recovery-code username test-code))
        ;; Verify that using the same code again fails (since it's been consumed)
        (is-false (jrm-auth:verify-recovery-code username test-code))))
    ;; Test deletion of recovery codes and regeneration
    (let ((more-codes (jrm-auth:generate-recovery-codes username)))
      (is (= 10 (length more-codes)))
      (let ((code-to-delete (car more-codes)))
        (jrm-auth:delete-recovery-codes username)
        ;; Now the generated code should fail because it was deleted
        (is-false (jrm-auth:verify-recovery-code username code-to-delete))))
    ;; Clean up
    (jrm-auth:delete-user username)))

(test stripe-database-and-routes
  "Verify Stripe metadata storage, updates, and existence of web handlers."
  (let ((username "test_stripe@domain.com")
        (password "StripePassword123!")
        (customer-id "cus_test123")
        (sub-id "sub_test123"))
    ;; Ensure DB is initialized and clean
    (ignore-errors (jrm-auth:init-db))
    (jrm-auth:delete-user username)
    
    ;; Create test user
    (is-true (jrm-auth:create-user username password))
    
    ;; 1. Check default subscription status is 'inactive'
    (let* ((user (car (jrm-auth:get-user username)))
           (status (jrm-auth:user-subscription-status user)))
      (is (string= "inactive" status)))
      
    ;; 2. Update stripe customer ID
    (jrm-auth:update-user-stripe-customer username customer-id)
    (let* ((user (car (jrm-auth:get-user username)))
           (cust-id (jrm-auth:user-stripe-customer-id user)))
      (is (string= customer-id cust-id)))
      
    ;; 3. Update subscription status to 'active' and tier to CADR
    (jrm-auth:update-user-subscription username customer-id sub-id "active" "CADR")
    (let* ((user (car (jrm-auth:get-user username)))
           (status (jrm-auth:user-subscription-status user))
           (s-id (jrm-auth:user-stripe-subscription-id user))
           (tier (jrm-auth:user-membership-tier user)))
      (is (string= "active" status))
      (is (string= sub-id s-id))
      (is (string= "CADR" tier)))
      
    ;; 4. Verify lookup by customer ID
    (let* ((user (car (jrm-auth:get-user-by-customer customer-id)))
           (found-username (jrm-auth:user-username user)))
      (is (string= username found-username)))

    ;; 5. Simulate a Billing Portal upgrade (customer.subscription.updated) to LAMBDA
    (jrm-auth:update-user-tier-status username "LAMBDA" "active")
    (let* ((user (car (jrm-auth:get-user username)))
           (tier (jrm-auth:user-membership-tier user)))
      (is (string= "LAMBDA" tier)))

    ;; 6. Simulate a cancellation (customer.subscription.deleted): back to CONS
    (jrm-auth:cancel-user-subscription username)
    (let* ((user (car (jrm-auth:get-user username)))
           (tier (jrm-auth:user-membership-tier user))
           (status (jrm-auth:user-subscription-status user))
           (s-id (jrm-auth:user-stripe-subscription-id user)))
      (is (string= "CONS" tier))
      (is (string= "canceled" status))
      (is (null s-id)))

    ;; 7. Verify routes/handlers exist in Hunchentoot easy-acceptor
    (is (fboundp 'jrm-code-project::create-checkout-session-action))
    (is (fboundp 'jrm-code-project::manage-subscription-action))
    (is (fboundp 'jrm-code-project::stripe-webhook-handler))
    
    ;; Clean up
    (jrm-auth:delete-user username)))

(test signup-username-validation
  "Verify VALID-USERNAME-P accepts any non-empty, whitespace-free string
under 255 characters, and rejects empty strings, over-length strings, and
strings containing whitespace."
  ;; 1. Check a variety of non-email-shaped and email-shaped strings all
  ;; pass -- usernames need not be email addresses.
  (is-true (jrm-code-project::valid-username-p "prunesquallor"))
  (is-true (jrm-code-project::valid-username-p "user@domain.com"))
  (is-true (jrm-code-project::valid-username-p "user_123"))
  (is-true (jrm-code-project::valid-username-p "plainaddress"))

  ;; 2. Check invalid usernames return NIL.
  (is-false (jrm-code-project::valid-username-p ""))
  (is-false (jrm-code-project::valid-username-p "has a space"))
  (is-false (jrm-code-project::valid-username-p "tab	separated"))
  (is-false (jrm-code-project::valid-username-p (make-string 255 :initial-element #\a))))

(test user-membership-tiers
  "Verify that newly created users default to the CONS membership tier."
  (let ((username "tier_test@domain.com")
        (password "TierPassword123!"))
    ;; Initialize & clean DB
    (ignore-errors (jrm-auth:init-db))
    (jrm-auth:delete-user username)
    
    ;; Create test user
    (is-true (jrm-auth:create-user username password))
    
    ;; Verify membership_tier defaults to "CONS"
    (let* ((user (car (jrm-auth:get-user username)))
           (tier (jrm-auth:user-membership-tier user)))
      (is (string= "CONS" tier)))
      
    ;; Clean up
    (jrm-auth:delete-user username)))

;;; -- PASTEBIN: MOCK DATABASE FIXTURE --
;;;
;;; PASTEBIN.LISP funnels every actual database touch through the
;;; JRM-AUTH::*PASTEBIN-...* special variables (see the "TESTABILITY" note
;;; at the top of that file), each of which defaults to a small function
;;; that performs a real POSTMODERN/WITH-DB call. WITH-MOCK-PASTEBIN-DB
;;; below rebinds all of them to an in-memory alternative backed by
;;; *MOCK-PASTES*, so ADD-PASTE, GET-PASTE, DELETE-PASTE-MANUAL,
;;; DOWNGRADE-USER-TO-FREE, REAP-EXPIRED-PASTES, and INIT-PASTEBIN-DB can
;;; all be exercised end to end -- including ADD-PASTE's transaction --
;;; without a live PostgreSQL connection.

(defstruct mock-paste id owner-username seq content expires-at)

(defvar *mock-pastes* nil
  "The in-memory pastes table used by WITH-MOCK-PASTEBIN-DB.")

(defvar *mock-schema-initialized-p* nil
  "Set by the mock INIT-PASTEBIN-DB backend so tests can assert it ran.")

(defvar *mock-now* 0
  "The fake \"current time\" (a universal time) WITH-MOCK-PASTEBIN-DB
installs as JRM-AUTH::*PASTEBIN-CLOCK*. ADD-PASTE/DOWNGRADE-USER-TO-FREE
read it when computing expiration, and the mock's own
GET-CONTENT/REAP primitives read it when deciding what has expired, so
tests can deterministically simulate time passing (e.g. advance it 91
days to make a fresh :FREE paste expire) with no wall-clock dependency.")

(defun sql-timestamp-to-universal-time (sql-timestamp)
  "Parse a \"YYYY-MM-DD HH:MM:SS\" string (as produced by
JRM-AUTH::UNIVERSAL-TIME-TO-SQL-TIMESTAMP) back into a Lisp universal
time. Test-only inverse of that conversion, used so the mock pastes
table can keep storing/comparing expiration as plain integers while
still exercising the real string-producing code path in PASTEBIN.LISP."
  (encode-universal-time (parse-integer sql-timestamp :start 17 :end 19)
                          (parse-integer sql-timestamp :start 14 :end 16)
                          (parse-integer sql-timestamp :start 11 :end 13)
                          (parse-integer sql-timestamp :start 8 :end 10)
                          (parse-integer sql-timestamp :start 5 :end 7)
                          (parse-integer sql-timestamp :start 0 :end 4)
                          0))

(defmacro with-mock-pastebin-db (&body body)
  "Run BODY with all pastebin database primitives (see PASTEBIN.LISP)
rebound to an in-memory mock store, completely isolated from any real
database, and with JRM-AUTH::*PASTEBIN-CLOCK* rebound to the
freely-adjustable *MOCK-NOW* (initialized to the real current time)."
  `(let* ((*mock-pastes* nil)
          (*mock-schema-initialized-p* nil)
          (*mock-now* (get-universal-time))
          (jrm-auth::*pastebin-clock* (lambda () *mock-now*))
          (jrm-auth::*pastebin-init-schema*
            (lambda () (setf *mock-schema-initialized-p* t)))
          (jrm-auth::*pastebin-max-sequence*
            (lambda (owner-username)
              (let ((mine (remove owner-username *mock-pastes*
                                   :key #'mock-paste-owner-username :test-not #'string=)))
                (when mine (reduce #'max mine :key #'mock-paste-seq)))))
          (jrm-auth::*pastebin-insert-free*
            (lambda (id owner-username seq content expires-at)
              (push (make-mock-paste :id id :owner-username owner-username :seq seq
                                      :content content
                                      :expires-at (sql-timestamp-to-universal-time expires-at))
                    *mock-pastes*)))
          (jrm-auth::*pastebin-insert-paid*
            (lambda (id owner-username seq content)
              (push (make-mock-paste :id id :owner-username owner-username :seq seq
                                      :content content :expires-at nil)
                    *mock-pastes*)))
          (jrm-auth::*pastebin-rolling-delete*
            (lambda (owner-username offset)
              (let* ((mine (sort (remove owner-username *mock-pastes*
                                         :key #'mock-paste-owner-username :test-not #'string=)
                                  #'> :key #'mock-paste-seq))
                     (others (remove owner-username *mock-pastes*
                                      :key #'mock-paste-owner-username :test #'string=))
                     (keep-mine (subseq mine 0 (min offset (length mine)))))
                (setf *mock-pastes* (append others keep-mine)))))
          (jrm-auth::*pastebin-get-content*
            (lambda (paste-id)
              (let ((row (find paste-id *mock-pastes* :key #'mock-paste-id :test #'string=)))
                (when (and row (or (null (mock-paste-expires-at row))
                                    (> (mock-paste-expires-at row) (funcall jrm-auth::*pastebin-clock*))))
                  (mock-paste-content row)))))
          (jrm-auth::*pastebin-get-user-pastes*
            (lambda (owner-username)
              (let ((mine (remove owner-username *mock-pastes*
                                   :key #'mock-paste-owner-username :test-not #'string=)))
                (mapcar (lambda (p)
                          `((:id . ,(mock-paste-id p))
                            (:created-at . "2024-01-01 00:00:00")
                            (:expires-at . ,(mock-paste-expires-at p))
                            (:content-preview . ,(subseq (mock-paste-content p)
                                                          0 (min 50 (length (mock-paste-content p)))))))
                        (remove-if (lambda (p) (and (mock-paste-expires-at p)
                                                     (<= (mock-paste-expires-at p)
                                                         (funcall jrm-auth::*pastebin-clock*))))
                                   (sort (copy-list mine) #'> :key #'mock-paste-seq))))))
          (jrm-auth::*pastebin-delete-manual*
            (lambda (paste-id owner-username)
              (setf *mock-pastes*
                    (remove-if (lambda (p) (and (string= (mock-paste-id p) paste-id)
                                                 (string= (mock-paste-owner-username p) owner-username)))
                               *mock-pastes*))))
          (jrm-auth::*pastebin-downgrade-update*
            (lambda (owner-username expires-at)
              (dolist (p *mock-pastes*)
                (when (and (string= (mock-paste-owner-username p) owner-username)
                           (null (mock-paste-expires-at p)))
                  (setf (mock-paste-expires-at p) (sql-timestamp-to-universal-time expires-at))))))
          (jrm-auth::*pastebin-reap*
            (lambda ()
              (setf *mock-pastes*
                    (remove-if (lambda (p) (and (mock-paste-expires-at p)
                                                 (< (mock-paste-expires-at p) (funcall jrm-auth::*pastebin-clock*))))
                               *mock-pastes*))))
          (jrm-auth::*pastebin-call-with-transaction*
            (lambda (thunk) (funcall thunk))))
     ,@body))

(test paste-id-generation
  "Verify GENERATE-PASTE-ID produces unguessable Base62 ids of the right
shape without touching any database."
  (let ((id (jrm-auth:generate-paste-id)))
    (is (= 16 (length id)))
    (is (every (lambda (c) (find c jrm-auth::*base62-alphabet*)) id)))
  ;; A custom length is honored.
  (is (= 32 (length (jrm-auth:generate-paste-id 32))))
  ;; Two consecutive ids are (overwhelmingly likely to be) distinct.
  (is (string/= (jrm-auth:generate-paste-id) (jrm-auth:generate-paste-id))))

(test paste-tier-retention-limit-pure
  "Verify JRM-AUTH::PASTE-TIER-RETENTION-LIMIT maps :FREE/:PAID to the
spec'd rolling-window sizes and rejects unknown tiers, with no database
involved."
  (is (= 3 (jrm-auth::paste-tier-retention-limit :free)))
  (is (= 128 (jrm-auth::paste-tier-retention-limit :paid)))
  (signals error (jrm-auth::paste-tier-retention-limit :bogus)))

(test universal-time-to-sql-timestamp-pure
  "Verify JRM-AUTH::UNIVERSAL-TIME-TO-SQL-TIMESTAMP formats a known
universal time as the expected UTC \"YYYY-MM-DD HH:MM:SS\" string, with no
database involved -- the Lisp application computes this value itself
rather than delegating to Postgres's NOW()."
  (is (string= "2024-01-15 12:30:45"
               (jrm-auth::universal-time-to-sql-timestamp
                (encode-universal-time 45 30 12 15 1 2024 0))))
  ;; Round-trips through the test-only inverse helper.
  (is (= (encode-universal-time 0 0 0 1 6 2026 0)
         (sql-timestamp-to-universal-time
          (jrm-auth::universal-time-to-sql-timestamp
           (encode-universal-time 0 0 0 1 6 2026 0))))))

(test pastebin-init-db-mock
  "Verify INIT-PASTEBIN-DB invokes the schema-initialization primitive."
  (with-mock-pastebin-db
    (is-false *mock-schema-initialized-p*)
    (jrm-auth:init-pastebin-db)
    (is-true *mock-schema-initialized-p*)))

(test pastebin-add-and-get-paste
  "Verify ADD-PASTE stores retrievable content and GET-PASTE returns NIL
for an unknown id."
  (with-mock-pastebin-db
    (let* ((username "paster@domain.com")
           (id (jrm-auth:add-paste username "(print \"hello\")" :free)))
      (is (= 16 (length id)))
      (is (string= "(print \"hello\")" (jrm-auth:get-paste id)))
      (is (null (jrm-auth:get-paste "no-such-paste-id"))))))

(test pastebin-sequence-numbering
  "Verify each new paste for the same owner gets the next
USER-SEQUENCE-NUM, while a different owner starts back at 1."
  (with-mock-pastebin-db
    (let ((username "seq-owner@domain.com")
          (other "other-owner@domain.com"))
      (jrm-auth:add-paste username "one" :free)
      (jrm-auth:add-paste username "two" :free)
      (jrm-auth:add-paste username "three" :free)
      (jrm-auth:add-paste other "first" :free)
      (is (equal '(3 2 1)
                 (mapcar #'mock-paste-seq
                         (sort (remove username *mock-pastes*
                                       :key #'mock-paste-owner-username :test-not #'string=)
                               #'> :key #'mock-paste-seq))))
      (is (= 1 (mock-paste-seq
                (find other *mock-pastes* :key #'mock-paste-owner-username :test #'string=)))))))

(test pastebin-free-tier-rolling-window
  "Verify a :FREE owner's pastes are trimmed down to their 3 most recent,
and that the older, evicted pastes are no longer retrievable."
  (with-mock-pastebin-db
    (let ((username "free-owner@domain.com") (ids nil))
      (dotimes (i 5)
        (push (jrm-auth:add-paste username (format nil "paste-~D" i) :free) ids))
      (setf ids (nreverse ids)) ; ids[0] is the oldest, ids[4] the newest
      (is (= 3 (length (remove username *mock-pastes*
                                :key #'mock-paste-owner-username :test-not #'string=))))
      ;; The two oldest pastes were reaped by the rolling window...
      (is (null (jrm-auth:get-paste (nth 0 ids))))
      (is (null (jrm-auth:get-paste (nth 1 ids))))
      ;; ...but the 3 most recent remain.
      (is (string= "paste-2" (jrm-auth:get-paste (nth 2 ids))))
      (is (string= "paste-3" (jrm-auth:get-paste (nth 3 ids))))
      (is (string= "paste-4" (jrm-auth:get-paste (nth 4 ids)))))))

(test pastebin-paid-tier-retains-more
  "Verify a :PAID owner's rolling window (128) does not trim a handful of
pastes the way the :FREE window (3) would."
  (with-mock-pastebin-db
    (let ((username "paid-owner@domain.com") (ids nil))
      (dotimes (i 5)
        (push (jrm-auth:add-paste username (format nil "paste-~D" i) :paid) ids))
      (setf ids (nreverse ids))
      (is (= 5 (length (remove username *mock-pastes*
                                :key #'mock-paste-owner-username :test-not #'string=))))
      (dotimes (i 5)
        (is (string= (format nil "paste-~D" i) (jrm-auth:get-paste (nth i ids))))))))

(test pastebin-paid-paste-never-expires
  "Verify a :PAID paste is inserted with a NIL EXPIRES-AT."
  (with-mock-pastebin-db
    (let ((id (jrm-auth:add-paste "payer@domain.com" "permanent" :paid)))
      (is (null (mock-paste-expires-at
                 (find id *mock-pastes* :key #'mock-paste-id :test #'string=)))))))

(test pastebin-expired-paste-not-returned
  "Verify GET-PASTE treats a :FREE paste as gone once real time (simulated
via the injected *PASTEBIN-CLOCK*) has advanced past its 90-day
EXPIRES-AT -- exercising ADD-PASTE's actual computed expiration rather
than a hand-poked field."
  (with-mock-pastebin-db
    (let* ((username "expiring@domain.com")
           (id (jrm-auth:add-paste username "stale content" :free)))
      ;; Just before expiry: still there.
      (incf *mock-now* (- (* 90 24 60 60) 60))
      (is (string= "stale content" (jrm-auth:get-paste id)))
      ;; Past the 90-day mark: gone.
      (incf *mock-now* 120)
      (is (null (jrm-auth:get-paste id))))))

(test pastebin-delete-paste-manual-ownership
  "Verify DELETE-PASTE-MANUAL only deletes the paste when the supplied
owner-username actually matches, protecting against deleting someone else's
paste by guessing its id."
  (with-mock-pastebin-db
    (let* ((owner "owner@domain.com")
           (id (jrm-auth:add-paste owner "mine" :free)))
      ;; Wrong owner: no-op, paste survives.
      (jrm-auth:delete-paste-manual id "attacker@domain.com")
      (is (string= "mine" (jrm-auth:get-paste id)))
      ;; Correct owner: paste is gone.
      (jrm-auth:delete-paste-manual id owner)
      (is (null (jrm-auth:get-paste id))))))

(test pastebin-get-user-pastes
  "Verify GET-USER-PASTES returns only owner-username's own non-expired
pastes, most-recently-created first, each summarized as an alist with
:ID, :CREATED-AT, :EXPIRES-AT, and a 50-character :CONTENT-PREVIEW."
  (with-mock-pastebin-db
    (let* ((owner "owner@domain.com")
           (other "other@domain.com")
           (id1 (jrm-auth:add-paste owner "first paste" :paid))
           (id2 (jrm-auth:add-paste owner (make-string 60 :initial-element #\x) :paid))
           (expired-id (jrm-auth:add-paste owner "will expire" :free)))
      ;; A paste belonging to a different owner must never show up.
      (jrm-auth:add-paste other "not mine" :free)
      ;; Advance the clock 91 days so EXPIRED-ID's :FREE paste has expired.
      (incf *mock-now* (* 91 24 60 60))
      (let ((rows (jrm-auth:get-user-pastes owner)))
        (is (= 2 (length rows)))
        ;; Most recent (highest sequence number) first: ID2 before ID1.
        (is (string= id2 (cdr (assoc :id (first rows)))))
        (is (string= id1 (cdr (assoc :id (second rows)))))
        ;; The expired paste is excluded entirely.
        (is (null (find expired-id rows :key (lambda (r) (cdr (assoc :id r))) :test #'string=)))
        ;; Content preview is truncated to (at most) 50 characters.
        (is (= 50 (length (cdr (assoc :content-preview (first rows))))))
        ;; A :PAID paste has no expiration.
        (is (null (cdr (assoc :expires-at (first rows)))))))))

(test pastebin-downgrade-user-to-free
  "Verify DOWNGRADE-USER-TO-FREE trims a former paid user down to their 3
most recent pastes and starts the 90-day expiration clock on the
survivors that were previously permanent, using the actual computed
EXPIRES-AT (via the injected *PASTEBIN-CLOCK*) rather than a hand-poked
field."
  (with-mock-pastebin-db
    (let ((username "downgraded@domain.com") (ids nil))
      (dotimes (i 5)
        (push (jrm-auth:add-paste username (format nil "paste-~D" i) :paid) ids))
      (setf ids (nreverse ids))
      (jrm-auth:downgrade-user-to-free username)
      (let ((remaining (remove username *mock-pastes*
                                :key #'mock-paste-owner-username :test-not #'string=)))
        (is (= 3 (length remaining)))
        ;; Every surviving paste now has a future EXPIRES-AT.
        (dolist (p remaining)
          (is (not (null (mock-paste-expires-at p))))
          (is (> (mock-paste-expires-at p) *mock-now*))))
      ;; The two oldest were dropped entirely.
      (is (null (jrm-auth:get-paste (nth 0 ids))))
      (is (null (jrm-auth:get-paste (nth 1 ids))))
      ;; The 3 survivors are readable now, but 91 simulated days later
      ;; their fresh 90-day clock (started by the downgrade) has run out.
      (is (string= "paste-4" (jrm-auth:get-paste (nth 4 ids))))
      (incf *mock-now* (* 91 24 60 60))
      (is (null (jrm-auth:get-paste (nth 4 ids)))))))

(test pastebin-reap-expired-pastes
  "Verify REAP-EXPIRED-PASTES removes only pastes whose EXPIRES-AT has
passed (per the injected *PASTEBIN-CLOCK*), leaving unexpired and
never-expiring pastes untouched."
  (with-mock-pastebin-db
    (let ((expired-id (jrm-auth:add-paste "reap-me@domain.com" "old" :free)))
      ;; 91 simulated days pass before the other two pastes are written.
      (incf *mock-now* (* 91 24 60 60))
      (let ((fresh-id (jrm-auth:add-paste "keep-me@domain.com" "fresh" :free))
            (permanent-id (jrm-auth:add-paste "forever@domain.com" "permanent" :paid)))
        (jrm-auth:reap-expired-pastes)
        (is (null (find expired-id *mock-pastes* :key #'mock-paste-id :test #'string=)))
        (is (string= "fresh" (jrm-auth:get-paste fresh-id)))
        (is (string= "permanent" (jrm-auth:get-paste permanent-id)))))))

;;; -- API KEYS: MOCK DATABASE FIXTURE --
;;;
;;; API-KEYS.LISP funnels every actual database touch through the
;;; JRM-AUTH::*API-KEYS-...* special variables (see the "TESTABILITY"
;;; note at the top of that file), each defaulting to a real
;;; POSTMODERN/WITH-DB call. WITH-MOCK-API-KEYS-DB rebinds all of them to
;;; an in-memory alist keyed by owner-username, so CREATE-API-KEY,
;;; VERIFY-API-KEY, REVOKE-API-KEY, and INIT-API-KEYS-DB can all be
;;; exercised without a live PostgreSQL connection.

(defvar *mock-api-keys* nil
  "Alist of (owner-username . KEY-HASH) used by WITH-MOCK-API-KEYS-DB.")

(defvar *mock-api-keys-schema-initialized-p* nil
  "Set by the mock INIT-API-KEYS-DB backend so tests can assert it ran.")

(defmacro with-mock-api-keys-db (&body body)
  "Run BODY with all api-keys database primitives (see API-KEYS.LISP)
rebound to an in-memory mock store, completely isolated from any real
database."
  `(let* ((*mock-api-keys* nil)
          (*mock-api-keys-schema-initialized-p* nil)
          (jrm-auth::*api-keys-init-schema*
            (lambda () (setf *mock-api-keys-schema-initialized-p* t)))
          (jrm-auth::*api-keys-upsert*
            (lambda (owner-username key-hash)
              (let ((entry (assoc owner-username *mock-api-keys* :test #'string=)))
                (if entry
                    (setf (cdr entry) key-hash)
                    (push (cons owner-username key-hash) *mock-api-keys*)))))
          (jrm-auth::*api-keys-get-hash*
            (lambda (owner-username)
              (cdr (assoc owner-username *mock-api-keys* :test #'string=))))
          (jrm-auth::*api-keys-revoke*
            (lambda (owner-username)
              (setf *mock-api-keys*
                    (remove owner-username *mock-api-keys* :key #'car :test #'string=)))))
     ,@body))

(test generate-raw-api-key-format
  "Verify GENERATE-RAW-API-KEY produces a jrm_live_-prefixed key backed by
32 bytes (64 hex characters) of randomness, and that consecutive keys
differ, with no database involved."
  (let ((key (jrm-auth:generate-raw-api-key)))
    (is (= (+ (length "jrm_live_") 64) (length key)))
    (is (string= "jrm_live_" (subseq key 0 9)))
    (is (every (lambda (c) (find c "0123456789abcdef")) (subseq key 9))))
  (is (string/= (jrm-auth:generate-raw-api-key) (jrm-auth:generate-raw-api-key))))

(test api-keys-init-db-mock
  "Verify INIT-API-KEYS-DB invokes the schema-initialization primitive."
  (with-mock-api-keys-db
    (is-false *mock-api-keys-schema-initialized-p*)
    (jrm-auth:init-api-keys-db)
    (is-true *mock-api-keys-schema-initialized-p*)))

(test api-keys-create-and-verify
  "Verify CREATE-API-KEY returns a raw key that VERIFY-API-KEY accepts,
while a wrong key or a different owner's key is rejected, and the raw
value is never stored (only its hash appears in the mock table)."
  (with-mock-api-keys-db
    (let* ((username "api-user@domain.com")
           (raw-key (jrm-auth:create-api-key username)))
      (is (string= "jrm_live_" (subseq raw-key 0 9)))
      (is-true (jrm-auth:verify-api-key username raw-key))
      (is-false (jrm-auth:verify-api-key username "jrm_live_deadbeef"))
      (is-false (jrm-auth:verify-api-key "someone-else@domain.com" raw-key))
      ;; Only the hash is stored, never the raw key.
      (is (string/= raw-key (cdr (assoc username *mock-api-keys* :test #'string=)))))))

(test api-keys-create-overwrites-previous-key
  "Verify a second CREATE-API-KEY for the same owner invalidates the
first key (one active key per owner, enforced by OWNER_USERNAME being the
primary key)."
  (with-mock-api-keys-db
    (let* ((username "rotating-user@domain.com")
           (first-key (jrm-auth:create-api-key username))
           (second-key (jrm-auth:create-api-key username)))
      (is (string/= first-key second-key))
      (is-false (jrm-auth:verify-api-key username first-key))
      (is-true (jrm-auth:verify-api-key username second-key))
      (is (= 1 (length *mock-api-keys*))))))

(test api-keys-verify-unknown-owner
  "Verify VERIFY-API-KEY returns NIL for an owner with no active key,
rather than erroring."
  (with-mock-api-keys-db
    (is-false (jrm-auth:verify-api-key "no-such-owner@domain.com" "jrm_live_anything"))))

(test api-keys-revoke
  "Verify REVOKE-API-KEY deletes the owner's active key, so a previously
valid raw key is subsequently rejected."
  (with-mock-api-keys-db
    (let* ((username "revoke-me@domain.com")
           (raw-key (jrm-auth:create-api-key username)))
      (is-true (jrm-auth:verify-api-key username raw-key))
      (jrm-auth:revoke-api-key username)
      (is-false (jrm-auth:verify-api-key username raw-key))
      (is (null (assoc username *mock-api-keys* :test #'string=))))))

;;; -- API TOKEN EXCHANGE: MOCK CLOCK/SECRET/DB FIXTURE --
;;;
;;; API-TOKEN.LISP funnels GENERATE-API-JWT's timekeeping through
;;; JRM-AUTH::*JWT-CLOCK* and its signing key through
;;; JRM-AUTH::*JWT-SECRET* (mirroring PASTEBIN.LISP's *PASTEBIN-CLOCK*),
;;; and GET-USER-TIER's database lookup through JRM-AUTH::*GET-USER-TIER*
;;; (mirroring API-KEYS.LISP's *API-KEYS-...* specials), so all of this
;;; can be tested deterministically and without a live database.

(defvar *mock-user-tiers* nil
  "Alist of (owner-username . TIER) used by WITH-MOCK-API-TOKEN-DB to mock
JRM-AUTH:GET-USER-TIER without a live database.")

(defmacro with-mock-api-token-db (&body body)
  "Run BODY with JRM-AUTH::*JWT-CLOCK* rebound to a fixed, fast-forwardable
mock time (see *MOCK-NOW*, reused from the pastebin fixture above),
JRM-AUTH::*JWT-SECRET* rebound to a fixed test secret, and
JRM-AUTH::*GET-USER-TIER* rebound to an in-memory alist store."
  `(let* ((*mock-now* (get-universal-time))
          (*mock-user-tiers* nil)
          (jrm-auth::*jwt-clock* (lambda () *mock-now*))
          (jrm-auth::*jwt-secret* "test-fixture-jwt-secret")
          (jrm-auth::*get-user-tier*
            (lambda (owner-username) (cdr (assoc owner-username *mock-user-tiers* :test #'string=)))))
     ,@body))

(test generate-api-jwt-claims
  "Verify GENERATE-API-JWT produces a token whose :SUB, :TIER, :IAT, and
:EXP claims match what was requested and the injected *JWT-CLOCK*, with
:EXP exactly *API-JWT-LIFETIME-SECONDS* (3600) after :IAT."
  (with-mock-api-token-db
    (let* ((token (jrm-auth:generate-api-jwt "jwt-user@domain.com" "paid"))
           (claims (jose:decode :hs256
                                (ironclad:ascii-string-to-byte-array jrm-auth::*jwt-secret*)
                                token))
           (expected-iat (- *mock-now* (encode-universal-time 0 0 0 1 1 1970 0))))
      (is (string= "jwt-user@domain.com" (cdr (assoc "sub" claims :test #'string=))))
      (is (string= "paid" (cdr (assoc "tier" claims :test #'string=))))
      (is (= expected-iat (cdr (assoc "iat" claims :test #'string=))))
      (is (= (+ expected-iat 3600) (cdr (assoc "exp" claims :test #'string=)))))))

(test generate-api-jwt-rejects-wrong-secret
  "Verify a token minted with one secret fails to decode/verify against a
different secret, i.e. the signature genuinely depends on *JWT-SECRET*."
  (with-mock-api-token-db
    (let ((token (jrm-auth:generate-api-jwt "jwt-user@domain.com" "free")))
      (signals error
        (jose:decode :hs256 (ironclad:ascii-string-to-byte-array "wrong-secret") token)))))

(test get-user-tier-mocked
  "Verify GET-USER-TIER returns the mocked tier for a known owner and NIL
for an unknown one, without touching a live database."
  (with-mock-api-token-db
    (push (cons "free-user@domain.com" :free) *mock-user-tiers*)
    (push (cons "paid-user@domain.com" :paid) *mock-user-tiers*)
    (is (eq :free (jrm-auth:get-user-tier "free-user@domain.com")))
    (is (eq :paid (jrm-auth:get-user-tier "paid-user@domain.com")))
    (is (null (jrm-auth:get-user-tier "no-such-user@domain.com")))))

(test api-jwt-tier-string-pure
  "Verify JRM-CODE-PROJECT::API-JWT-TIER-STRING (a pure keyword -> string
mapping with no I/O) renders :FREE/:PAID as the lowercase strings
GENERATE-API-JWT's :TIER claim expects."
  (is (string= "free" (jrm-code-project::api-jwt-tier-string :free)))
  (is (string= "paid" (jrm-code-project::api-jwt-tier-string :paid))))

;;; -- TOKEN BUCKET RATE LIMITER --
;;;
;;; JRM-AUTH:CHECK-RATE-LIMIT reads/writes the global JRM-AUTH::*RATE-LIMITS*
;;; hash table, so WITH-CLEAN-RATE-LIMITS gives each test its own empty
;;; table -- avoiding cross-test interference from bucket state a
;;; previous test left behind (or a shared KEY string colliding).

(defmacro with-clean-rate-limits (&body body)
  "Run BODY with JRM-AUTH::*RATE-LIMITS* rebound to a fresh, empty hash
table, so token-bucket state from other tests (or prior runs) cannot
leak in."
  `(let ((jrm-auth::*rate-limits* (make-hash-table :test 'equal)))
     ,@body))

(test check-rate-limit-allows-up-to-capacity
  "Verify a freshly-seen KEY starts with a full MAX-TOKENS bucket, so
exactly MAX-TOKENS consecutive calls are allowed and the next one (with
no time having elapsed to refill) is denied."
  (with-clean-rate-limits
    (dotimes (i 5)
      (is-true (jrm-auth:check-rate-limit "1.2.3.4" 5 (/ 10.0d0 60.0d0))))
    (is-false (jrm-auth:check-rate-limit "1.2.3.4" 5 (/ 10.0d0 60.0d0)))))

(test check-rate-limit-refills-over-time
  "Verify tokens are replenished at REFILL-RATE-PER-SECOND as time
passes, by manipulating the bucket's LAST-UPDATE to simulate elapsed
time between calls."
  (with-clean-rate-limits
    ;; Drain the bucket completely.
    (dotimes (i 3)
      (is-true (jrm-auth:check-rate-limit "5.6.7.8" 3 1.0d0)))
    (is-false (jrm-auth:check-rate-limit "5.6.7.8" 3 1.0d0))
    ;; Simulate 5 seconds passing at a 1 token/second refill rate.
    (let ((bucket (gethash "5.6.7.8" jrm-auth::*rate-limits*)))
      (setf (jrm-auth::token-bucket-last-update bucket)
            (- (jrm-auth::token-bucket-last-update bucket) 5)))
    (is-true (jrm-auth:check-rate-limit "5.6.7.8" 3 1.0d0))))

(test check-rate-limit-caps-at-max-tokens
  "Verify refilling never accumulates more than MAX-TOKENS worth of
tokens, even after a very long idle period."
  (with-clean-rate-limits
    (is-true (jrm-auth:check-rate-limit "9.9.9.9" 2 1.0d0))
    (let ((bucket (gethash "9.9.9.9" jrm-auth::*rate-limits*)))
      (setf (jrm-auth::token-bucket-last-update bucket)
            (- (jrm-auth::token-bucket-last-update bucket) 100000)))
    ;; Capacity is only 2, so only 2 calls should succeed even though a
    ;; huge amount of idle time has passed.
    (is-true (jrm-auth:check-rate-limit "9.9.9.9" 2 1.0d0))
    (is-true (jrm-auth:check-rate-limit "9.9.9.9" 2 1.0d0))
    (is-false (jrm-auth:check-rate-limit "9.9.9.9" 2 1.0d0))))

(test check-rate-limit-independent-keys
  "Verify separate KEYs have independent buckets, so draining one does
not affect another."
  (with-clean-rate-limits
    (is-true (jrm-auth:check-rate-limit "user-a@domain.com" 1 (/ 10.0d0 60.0d0)))
    (is-false (jrm-auth:check-rate-limit "user-a@domain.com" 1 (/ 10.0d0 60.0d0)))
    (is-true (jrm-auth:check-rate-limit "user-b@domain.com" 1 (/ 10.0d0 60.0d0)))))

;;; -- 404 TARPIT (TARPIT-REGISTER-STRIKE) --
;;;
;;; TARPIT-REGISTER-STRIKE is exercised directly (rather than through
;;; TARPIT-HANDLE-404) so these tests never actually SLEEP: they only
;;; verify the strike-accounting/threshold logic, manipulating
;;; JRM-CODE-PROJECT::*TARPIT-STRIKES* entries' timestamps directly to
;;; simulate time passing without a real wall-clock dependency.

(defmacro with-clean-tarpit-strikes (&body body)
  "Run BODY with JRM-CODE-PROJECT::*TARPIT-STRIKES* rebound to a fresh,
empty hash table, isolating each test from state left behind by others."
  `(let ((jrm-code-project::*tarpit-strikes* (make-hash-table :test 'equal)))
     ,@body))

(test tarpit-register-strike-first-hit-not-tarpitted
  "Verify a single 404 from a fresh IP is not enough to trigger the
tarpit."
  (with-clean-tarpit-strikes
    (is-false (jrm-code-project::tarpit-register-strike "1.2.3.4"))
    (is (= 1 (jrm-code-project::tarpit-entry-strikes
              (gethash "1.2.3.4" jrm-code-project::*tarpit-strikes*))))))

(test tarpit-register-strike-exceeds-threshold
  "Verify an IP that racks up more than *TARPIT-STRIKE-THRESHOLD* 404s
within *TARPIT-WINDOW-SECONDS* becomes tarpitted."
  (with-clean-tarpit-strikes
    (dotimes (i jrm-code-project::*tarpit-strike-threshold*)
      (is-false (jrm-code-project::tarpit-register-strike "5.6.7.8")))
    ;; That was exactly *TARPIT-STRIKE-THRESHOLD* strikes -- not yet over
    ;; the threshold. One more strike should tip it into the tarpit.
    (is-true (jrm-code-project::tarpit-register-strike "5.6.7.8"))))

(test tarpit-register-strike-window-expiry-resets-count
  "Verify an IP's strike count resets to 1 (rather than continuing to
accumulate) once its last strike falls outside *TARPIT-WINDOW-SECONDS*."
  (with-clean-tarpit-strikes
    (dotimes (i jrm-code-project::*tarpit-strike-threshold*)
      (jrm-code-project::tarpit-register-strike "9.9.9.9"))
    ;; Rewind the recorded timestamp so it looks like the window expired.
    (let ((entry (gethash "9.9.9.9" jrm-code-project::*tarpit-strikes*)))
      (setf (jrm-code-project::tarpit-entry-last-seen entry)
            (- (jrm-code-project::tarpit-entry-last-seen entry)
               (1+ jrm-code-project::*tarpit-window-seconds*))))
    ;; A fresh strike after the window expired should reset the count to
    ;; 1, not push it over the threshold.
    (is-false (jrm-code-project::tarpit-register-strike "9.9.9.9"))
    (is (= 1 (jrm-code-project::tarpit-entry-strikes
              (gethash "9.9.9.9" jrm-code-project::*tarpit-strikes*))))))

(test tarpit-register-strike-independent-ips
  "Verify separate IPs have independent strike counts, so one IP's 404s
do not tarpit an unrelated IP."
  (with-clean-tarpit-strikes
    (dotimes (i (1+ jrm-code-project::*tarpit-strike-threshold*))
      (jrm-code-project::tarpit-register-strike "10.0.0.1"))
    (is-false (jrm-code-project::tarpit-register-strike "10.0.0.2"))))

;;; -- JWT VERIFICATION (VERIFY-AND-EXTRACT-JWT) --

(test verify-and-extract-jwt-valid-token
  "Verify a token minted by GENERATE-API-JWT, under the same mocked
clock/secret it was minted with, decodes successfully and returns claims
matching what was requested."
  (with-mock-api-token-db
    (let* ((token (jrm-auth:generate-api-jwt "verify-me@domain.com" "paid"))
           (claims (jrm-auth:verify-and-extract-jwt token)))
      (is-true claims)
      (is (string= "verify-me@domain.com" (cdr (assoc "sub" claims :test #'string=))))
      (is (string= "paid" (cdr (assoc "tier" claims :test #'string=)))))))

(test verify-and-extract-jwt-rejects-expired-token
  "Verify a token whose :EXP has already passed (per *JWT-CLOCK*) is
rejected, returning NIL rather than signaling an error."
  (with-mock-api-token-db
    (let ((token (jrm-auth:generate-api-jwt "expired-user@domain.com" "free")))
      ;; Fast-forward the mock clock well past the token's 1-hour lifetime.
      (incf *mock-now* (* 2 3600))
      (is (null (jrm-auth:verify-and-extract-jwt token))))))

(test verify-and-extract-jwt-rejects-bad-signature
  "Verify a token signed with a different secret than *JWT-SECRET* is
rejected, returning NIL rather than signaling an error out to the
caller."
  (with-mock-api-token-db
    (let ((token (let ((jrm-auth::*jwt-secret* "some-other-secret"))
                   (jrm-auth:generate-api-jwt "forged-user@domain.com" "free"))))
      ;; TOKEN was signed with "some-other-secret"; verifying it happens
      ;; under the fixture's real *JWT-SECRET* binding, which has already
      ;; been restored by the time we get here.
      (is (null (jrm-auth:verify-and-extract-jwt token))))))

(test verify-and-extract-jwt-rejects-garbage
  "Verify a string that isn't a well-formed JWT at all is rejected
gracefully, returning NIL instead of signaling an error."
  (with-mock-api-token-db
    (is (null (jrm-auth:verify-and-extract-jwt "not.a.jwt")))
    (is (null (jrm-auth:verify-and-extract-jwt "")))))

;;; -- HUNCHENTOOT MIDDLEWARE (WITH-API-AUTH-AND-RATE-LIMIT) --
;;;
;;; BEARER-TOKEN-FROM-AUTHORIZATION-HEADER is a pure string-parsing
;;; helper (no Hunchentoot request context required), so it can be
;;; tested directly without standing up a real HTTP request.

(test bearer-token-from-authorization-header-pure
  "Verify the Bearer-token parsing helper extracts the token from a
well-formed header, tolerates a case-insensitive scheme name and extra
whitespace, and returns NIL for a missing or non-Bearer header."
  (is (string= "abc.def.ghi" (jrm-code-project::bearer-token-from-header-value "Bearer abc.def.ghi")))
  (is (string= "abc.def.ghi" (jrm-code-project::bearer-token-from-header-value "bearer  abc.def.ghi")))
  (is (null (jrm-code-project::bearer-token-from-header-value "Basic dXNlcjpwYXNz")))
  (is (null (jrm-code-project::bearer-token-from-header-value nil))))


;;; -- PASTES API HELPERS (API-PASTES-ROUTES.LISP) --
;;;
;;; API-TIER-KEYWORD is a pure keyword<->string mapping (no I/O), so it
;;; is tested directly, mirroring API-JWT-TIER-STRING-PURE above.

(test api-tier-keyword-pure
  "Verify API-TIER-KEYWORD converts *API-TIER*'s \"free\"/\"paid\" JWT
claim strings (case-insensitively) back to the :FREE/:PAID keywords
ADD-PASTE expects, and defaults to :FREE for anything unrecognized or
NIL, so a paste is never accidentally minted with unlimited retention."
  (is (eq :paid (jrm-code-project::api-tier-keyword "paid")))
  (is (eq :paid (jrm-code-project::api-tier-keyword "PAID")))
  (is (eq :free (jrm-code-project::api-tier-keyword "free")))
  (is (eq :free (jrm-code-project::api-tier-keyword "unknown-tier")))
  (is (eq :free (jrm-code-project::api-tier-keyword nil))))

(test content-byte-size-pure
  "Verify CONTENT-BYTE-SIZE counts UTF-8 encoded octets rather than
character count, so multi-byte characters (e.g. emoji) are correctly
weighed against *MAX-PASTE-SIZE-BYTES* in POST /api/v1/pastes."
  (is (= 5 (jrm-code-project::content-byte-size "hello")))
  ;; U+1F600 GRINNING FACE encodes as 4 UTF-8 octets, not 1 character.
  (is (= 4 (jrm-code-project::content-byte-size (string (code-char #x1F600))))))

(test valid-lisp-content-p-pure
  "Verify VALID-LISP-CONTENT-P wraps LISP-P:LISP-P around a
MAKE-STRING-INPUT-STREAM, accepting well-formed Lisp forms and
rejecting unparseable garbage, so POST /api/v1/pastes can reject
non-Lisp content with a 422 before ever calling ADD-PASTE."
  (is (jrm-code-project::valid-lisp-content-p "(defun foo (x) (+ x 1))"))
  (is (not (jrm-code-project::valid-lisp-content-p "not lisp (("))))

;;; -- DIRECT PASTE URL HELPERS (PASTE-DIRECT-ROUTES.LISP) --

(test escape-html-pure
  "Verify ESCAPE-HTML replaces &, <, >, and both quote characters with
their entity references, leaving ordinary text untouched, so untrusted
paste content can be safely embedded inside <pre><code>...</code></pre>."
  (is (string= "&lt;script&gt;alert(1)&lt;/script&gt;"
               (jrm-code-project::escape-html "<script>alert(1)</script>")))
  (is (string= "&amp;&#39;&quot;" (jrm-code-project::escape-html "&'\"")))
  (is (string= "plain text 123" (jrm-code-project::escape-html "plain text 123")))
  (is (string= "" (jrm-code-project::escape-html ""))))

(test paste-direct-url-regex-matches-expected-forms
  "Verify *PASTE-DIRECT-URL-REGEX* accepts 12-16 character alphanumeric
ids with a .txt or .html extension, and rejects malformed ids/extensions."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings jrm-code-project::*paste-direct-url-regex* "/p/AbCdEf123456.txt")
    (is-true match)
    (is (string= "AbCdEf123456" (aref groups 0)))
    (is (string= "txt" (aref groups 1))))
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings jrm-code-project::*paste-direct-url-regex* "/p/AbCdEf1234567890.html")
    (is-true match)
    (is (string= "AbCdEf1234567890" (aref groups 0)))
    (is (string= "html" (aref groups 1))))
  (is-false (cl-ppcre:scan jrm-code-project::*paste-direct-url-regex* "/p/short.txt"))
  (is-false (cl-ppcre:scan jrm-code-project::*paste-direct-url-regex* "/p/AbCdEf123456.pdf"))
  (is-false (cl-ppcre:scan jrm-code-project::*paste-direct-url-regex* "/p/has-dash-123.txt")))
