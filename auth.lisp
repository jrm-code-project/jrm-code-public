;;; -*- Lisp -*-

;;; The gatekeeper: signup, 2FA setup/regeneration, recovery codes, the
;;; login protocol (password + TOTP challenge), the member dashboard,
;;; password changes, and account deletion.

(in-package "JRM-CODE-PROJECT")

;; --- DASHBOARD VIEW MODEL (pure decision logic) ---

(defstruct (tier-card-view (:copier nil))
  "Immutable view-model for a single tier card on the dashboard: purely data
-- CSS class name, badge kind, and button kind -- with no HTML baked in.
Rendering (turning these into markup) is deferred to the view layer."
  active-p
  (class :inactive :type (member :active :inactive :inactive-soon))
  (badge :none :type (member :current :soon :none))
  (button :disabled :type (member :active :manage-subscription :disabled :contact)))

(defstruct (dashboard-view-model (:copier nil))
  "Immutable, pure snapshot of everything the dashboard page needs to render,
derived solely from a USER, an optional Stripe CHECKOUT-STATUS query param,
and an optional NEXT breadcrumb. Contains no HTML -- see
FUNCTIONAL_REFACTOR.md Phase 2/5."
  username
  tier
  wheel-p
  notification            ; one of :success, :cancel, or NIL
  redirect-to              ; non-NIL if the caller should immediately redirect here instead of rendering
  cons-card
  cadr-card
  lambda-card
  kmachine-card)

(defun dashboard-view-model (user checkout-status next)
  "Given a USER struct, the `checkout-status' and `next' query parameters from
a /dashboard request, compute the pure DASHBOARD-VIEW-MODEL describing what
should be shown/done. Performs no I/O and has no side effects."
  (let* ((tier (or (jrm-auth:user-membership-tier user) "CONS"))
         (has-stripe-customer-p (not (null (jrm-auth:user-stripe-customer-id user))))
         (success-p (and checkout-status (string= checkout-status "success")))
         (has-next-p (and next (plusp (length next)))))
    (make-dashboard-view-model
     :username (jrm-auth:user-username user)
     :tier tier
     :wheel-p (jrm-auth:user-wheel-p user)
     :notification (cond (success-p :success)
                          ((and checkout-status (string= checkout-status "cancel")) :cancel)
                          (t nil))
     ;; If we just came back from a successful upgrade purchase and a `next`
     ;; breadcrumb is present, the caller should redirect straight back to
     ;; the originally-requested JWT-protected page instead of rendering.
     :redirect-to (and success-p has-next-p next)
     :cons-card (make-tier-card-view
                 :active-p (string-equal tier "CONS")
                 :class (if (string-equal tier "CONS") :active :inactive)
                 :badge (if (string-equal tier "CONS") :current :none)
                 :button (cond ((string-equal tier "CONS") :active)
                               (has-stripe-customer-p :manage-subscription)
                               (t :disabled)))
     :cadr-card (make-tier-card-view
                 :active-p (string-equal tier "CADR")
                 :class (if (string-equal tier "CADR") :active :inactive-soon)
                 :badge (if (string-equal tier "CADR") :current :soon)
                 :button (if (string-equal tier "CADR") :active :disabled))
     :lambda-card (make-tier-card-view
                   :active-p (string-equal tier "LAMBDA")
                   :class (if (string-equal tier "LAMBDA") :active :inactive-soon)
                   :badge (if (string-equal tier "LAMBDA") :current :soon)
                   :button (if (string-equal tier "LAMBDA") :active :disabled))
     :kmachine-card (make-tier-card-view
                     :active-p (string-equal tier "K-Machine")
                     :class (if (string-equal tier "K-Machine") :active :inactive)
                     :badge (if (string-equal tier "K-Machine") :current :none)
                     :button (if (string-equal tier "K-Machine") :active :contact)))))

;; --- AUTHENTICATION HANDLERS ---

(defun valid-username-p (username)
  "T if USERNAME is a non-empty string, under 255 characters, and contains
no whitespace; NIL otherwise. Usernames need not be email addresses --
any unique string satisfying this shape is acceptable. Uniqueness and
case-insensitive comparison are enforced at the database boundary (see
JRM-AUTH::NORMALIZE-USERNAME)."
  (and (stringp username)
       (plusp (length username))
       (< (length username) 255)
       (notany (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page))) username)
       t))

(defun get-signup-username ()
  (hunchentoot:session-value :signup-username))

(hunchentoot:define-easy-handler (signup-page :uri "/signup") (username password)
  (hunchentoot:start-session)
  (case (hunchentoot:request-method*)
    (:get
     (setf (hunchentoot:session-value :recovery-viewed) nil)
     (format nil "<html>
                    <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; } input { padding: 0.5rem; margin-bottom: 1rem; }</style></head>
                    <body><h2>Secure Signup</h2>
                      <form method='POST'>
                        ~A
                        <label>Username (any unique string):</label><br>
                        <input type='text' name='username' value='~A' required><br>
                        <label>Password:</label><br>
                        <input type='password' name='password' required><br>
                        <input type='submit' value='Sign Up'>
                      </form>
                    </body>
                  </html>"
             (csrf-input-html) (or username "")))
    (:post
     (with-csrf-protection
         (if (and username password)
             (if (valid-username-p username)
                 (if (jrm-auth:create-user username password)
                     (progn
                       (setf (hunchentoot:session-value :signup-username) (jrm-auth:normalize-username username))
                       (hunchentoot:redirect "/setup-2fa"))
                     (format nil "<html>
                                <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; }</style></head>
                                <body><h2>Signup Failed</h2>
                                  <p style='color: #bf616a;'>An account with that username already exists.</p>
                                  <a href='/' style='color: #88c0d0;'>Return to Login</a>
                                </body>
                              </html>"))
                 (format nil "<html>
                            <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; }</style></head>
                            <body><h2>Signup Failed</h2>
                              <p style='color: #bf616a;'>Invalid username formatting.</p>
                              <a href='/signup' style='color: #88c0d0;'>Try Again</a>
                            </body>
                          </html>"))
             (format nil "<html><body><h2>Error</h2><p>Username and password are required.</p></body></html>"))))))

(hunchentoot:define-easy-handler (setup-2fa-page :uri "/setup-2fa") (totp-code)
  (let ((username (get-signup-username)))
    (unless username (hunchentoot:redirect "/signup"))
    (case (hunchentoot:request-method*)
      (:get
       (let* ((secret (totp:generate-secret))
              (uri (totp:generate-qr-uri secret username :issuer "JRM-Code")))
         (setf (hunchentoot:session-value :temp-secret) secret)
         (let ((qr-url (format nil "https://api.qrserver.com/v1/create-qr-code/?size=250x250&data=~A" (hunchentoot:url-encode uri))))
           (format nil "<html>
                          <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; }</style></head>
                          <body><h2>Setup 2FA</h2>
                            <p>Scan this QR code with Google Authenticator or Authy:</p>
                            <img src='~A' style='border: 10px solid white;'><br><br>
                            <p>Or manually enter this secret: <b style='color: #0f0; font-family: monospace;'>~A</b></p>
                            <form method='POST'>
                              ~A
                              Enter 6-digit code: <input type='text' name='totp-code' required><br>
                              <input type='submit' value='Verify & Lock'>
                            </form>
                          </body>
                        </html>" qr-url (string-upcase secret) (csrf-input-html)))))
      (:post
       (with-csrf-protection
           (let ((secret (hunchentoot:session-value :temp-secret)))
             (if (and secret totp-code (totp:verify-totp secret totp-code))
                 (progn
                   (jrm-auth:activate-user-2fa username secret)
                   (hunchentoot:redirect "/recovery-codes"))
                 (format nil "<html><head><style>body { background: #111; color: #f00; }</style></head><body><h2>Error</h2><p>Invalid code. <a href='/setup-2fa' style='color:#fff;'>Try again</a>.</p></body></html>"))))))))

(hunchentoot:define-easy-handler (recovery-codes-page :uri "/recovery-codes") ()
  (let ((username (get-signup-username)))
    (unless username (hunchentoot:redirect "/signup"))
    (let ((codes (or (hunchentoot:session-value :temp-recovery-codes)
                     (let ((new-codes (jrm-auth:generate-recovery-codes username)))
                       (setf (hunchentoot:session-value :temp-recovery-codes) new-codes)
                       new-codes))))
      (format nil "<html>
                     <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; } li { font-family: monospace; font-size: 1.2rem; color: #0f0; }</style></head>
                     <body><h2>Your Recovery Codes</h2>
                       <p>Print these or save them offline. If you lose your phone, these are your only way back in.</p>
                       <p><b>You will only see this screen once.</b></p>
                       <ul>~{<li>~A</li>~}</ul>
                       <br><a href='/' style='color: #fff;'>Proceed to Login</a>
                     </body>
                   </html>" codes))))
;; --- LOGIN PROTOCOL ---

(hunchentoot:define-easy-handler (api-login :uri "/api/login") (username password next)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((user (jrm-auth:verify-login username password)))
    (if user
        (progn
          (hunchentoot:start-session)
          (setf (hunchentoot:session-value :limbo-username) (jrm-auth:user-username user))
          ;; Stash the post-login breadcrumb (if any) in the session so it
          ;; survives the GET/POST round trip through /challenge-2fa.
          (when (and next (plusp (length next)))
            (setf (hunchentoot:session-value :post-login-redirect) next))
          "{\"success\": true, \"success_url\": \"/challenge-2fa\"}")
        "{\"success\": false, \"error\": \"Invalid username or password\"}")))

(hunchentoot:define-easy-handler (challenge-2fa-page :uri "/challenge-2fa") (totp-code)
  (let ((username (hunchentoot:session-value :limbo-username)))
    (unless username (hunchentoot:redirect "/"))
    (case (hunchentoot:request-method*)
      (:get
       (format nil "<html>
                      <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; } input { padding: 0.5rem; margin-bottom: 1rem; }</style></head>
                      <body><h2>Two-Factor Authentication</h2>
                        <p>Enter the 6-digit code from your authenticator app, or enter an 8-character recovery code (e.g., ABCD-1234).</p>
                        <form method='POST'>
                          ~A
                          <input type='text' name='totp-code' required autocomplete='off' autofocus><br>
                          <input type='submit' value='Authenticate'>
                        </form>
                      </body>
                    </html>" (csrf-input-html)))
      (:post
       (with-csrf-protection
           (let* ((user (car (jrm-auth:get-user username)))
                  (secret (jrm-auth:user-totp-secret user)))
             (if (or (and secret totp-code (totp:verify-totp secret totp-code))
                     (and totp-code (jrm-auth:verify-recovery-code username totp-code)))
                 (progn
                   (setf (hunchentoot:session-value :authenticated-user) (jrm-auth:user-username user))
                   (setf (hunchentoot:session-value :limbo-username) nil)
                   (issue-membership-jwt (jrm-auth:user-username user) (or (jrm-auth:user-membership-tier user) "CONS") (jrm-auth:user-wheel-p user))
                   (let ((redirect-to (or (hunchentoot:session-value :post-login-redirect) "/dashboard")))
                     (setf (hunchentoot:session-value :post-login-redirect) nil)
                     (hunchentoot:redirect redirect-to)))
                 (format nil "<html><head><style>body { background: #111; color: #f00; }</style></head><body><h2>Error</h2><p>Invalid code. <a href='/challenge-2fa' style='color:#fff;'>Try again</a>.</p></body></html>"))))))))

;; --- DASHBOARD HTML RENDERING (translates pure view-model data to markup
;; using the pure combinators in views.lisp; see FUNCTIONAL_REFACTOR.md
;; Phase 5) ---

(defun tier-card-class-html (card)
  (ecase (tier-card-view-class card)
    (:active "tier-card active")
    (:inactive "tier-card")
    (:inactive-soon "tier-card tier-card-disabled")))

(defun tier-card-badge-html (card)
  (ecase (tier-card-view-badge card)
    (:current "<span class='tier-badge'>Current Tier</span>")
    (:soon "<span class='tier-badge tier-badge-soon'>Coming Soon</span>")
    (:none "")))

(defun tier-card-button-html (card &key (select-label "Select"))
  (ecase (tier-card-view-button card)
    (:active "<button class='tier-btn tier-btn-active' disabled>Active</button>")
    (:manage-subscription "<a class='tier-btn tier-btn-active' style='display:block;text-align:center;text-decoration:none;box-sizing:border-box;' href='/manage-subscription'>Manage Subscription</a>")
    (:disabled (format nil "<button class='tier-btn tier-btn-disabled' disabled>~A</button>"
                        (if (eq (tier-card-view-badge card) :soon) "Coming Soon" select-label)))
    (:contact "<button class='tier-btn tier-btn-disabled' disabled>Contact Us</button>")))

(defun tier-card-html (card name price benefits-html &key (select-label "Select"))
  "Pure (TIER-CARD-VIEW -> HTML) rendering function for one tier card in the
dashboard's tier grid."
  (format nil "<div class='~A'>
                 ~A
                 <div class='tier-name'>~A</div>
                 <div class='tier-price'>~A</div>
                 <ul class='tier-benefits'>~A</ul>
                 ~A
               </div>"
          (tier-card-class-html card)
          (tier-card-badge-html card)
          (html-escape name)
          (html-escape price)
          benefits-html
          (tier-card-button-html card :select-label select-label)))

(defun render-dashboard (vm &key wheel-link-html csrf-token api-jwt)
  "Pure (DASHBOARD-VIEW-MODEL -> HTML) rendering function for the whole
/dashboard page body. Takes no I/O-derived data beyond what is already
captured in VM plus the small pieces (the wheel admin link, a freshly-
minted CSRF token, and a freshly-minted short-lived programmatic-API
JWT for the pastebin panel's fetch() calls) that are necessarily
generated per-request."
  (html-page
   "Dashboard"
   (format nil "~A
                <div class='vault'>
                  <h2>Welcome to the Vault, ~A</h2>
                  <p>You are fully authenticated.</p>
                  <p><span class='tier-badge'>Membership: ~A</span></p>
                </div>
                <div class='subpage-menu'>
                  <a href='/chef.html' class='subpage-link'>The Chef</a>
                  <a href='/heresies/index.html' class='subpage-link'>Heresies</a>
                </div>
                <div class='actions'>
                  <a href='/logout' class='btn'>Logout</a>
                  <a href='/change-password' class='btn' style='background: #ebcb8b;'>Change Password</a>
                  <a href='/regenerate-recovery' class='btn' style='background: #a3be8c;'>Regenerate Recovery Codes</a>
                  <a href='/regenerate-2fa' class='btn' style='background: #b48ead;'>Regenerate 2FA</a>
                  ~A
                  ~A
                </div>
                <div class='vault'>
                  <h3 style='margin-top: 0; border-bottom: 1px solid #333; padding-bottom: 0.5rem;'>Programmatic API Key</h3>
                  <p>Generate a key to authenticate scripted requests against this account. Only one key can be active at a time -- generating a new one immediately revokes the old one.</p>
                  <div class='actions'>
                    <button id='generate-api-key-btn' class='btn'>Generate API Key</button>
                    <button id='revoke-api-key-btn' class='btn btn-danger'>Revoke API Key</button>
                  </div>
                </div>
                <div class='vault'>
                  <h3 style='margin-top: 0; border-bottom: 1px solid #333; padding-bottom: 0.5rem;'>Pastebin</h3>
                  <p>Curate and share code snippets via a plain link.</p>
                  <a href='/pastebin' class='btn'>Manage My Pastes (CONS Tier)</a>
                  <p class='tier-subtext'>As a CONS tier user, you can curate up to 3 pastes. Older pastes will be automatically recycled.</p>
                </div>
                <div class='vault'>
                  <h3 style='margin-top: 0; border-bottom: 1px solid #333; padding-bottom: 0.5rem;'>Membership Tiers</h3>
                  <div class='tier-grid'>
                    ~A
                    ~A
                    ~A
                    ~A
                  </div>
                </div>
                <div id='api-key-modal-overlay' class='modal-overlay'>
                  <div class='modal'>
                    <h3>Your New API Key</h3>
                    <p class='modal-warning'>Copy this key now. For security reasons, it will never be displayed again.</p>
                    <code id='api-key-value' class='modal-key'></code>
                    <div class='modal-actions'>
                      <button id='api-key-copy-btn' class='btn'>Copy to Clipboard</button>
                      <button id='api-key-close-btn' class='btn btn-secondary'>Close</button>
                    </div>
                  </div>
                </div>
                <script src='/js/api-keys.js'></script>"
           (html-notification (dashboard-view-model-notification vm)
                               (ecase (dashboard-view-model-notification vm)
                                 (:success "Your payment has been processed successfully!")
                                 (:cancel "Checkout was cancelled.")
                                 ((nil) "")))
           (html-escape (dashboard-view-model-username vm))
           (html-escape (dashboard-view-model-tier vm))
           (or wheel-link-html "")
           (html-form "/delete-account"
                      "<button type='submit' class='btn btn-danger' onclick=\"return confirm('WARNING: This will permanently obliterate your account and all recovery codes from the database. Are you absolutely sure?');\">Delete Account</button>"
                      :style "margin: 0;"
                      :csrf-token csrf-token)
           (tier-card-html (dashboard-view-model-cons-card vm) "CONS" "Free"
                            "<li>Access to site</li>" :select-label "Select CONS")
           (tier-card-html (dashboard-view-model-cadr-card vm) "CADR" "$9.95/mo"
                            "<li>Access to site.</li><li>Previews of features.</li>")
           (tier-card-html (dashboard-view-model-lambda-card vm) "LAMBDA" "$49.95/mo"
                            "<li>Access to site.</li><li>Access to features.</li>")
           (tier-card-html (dashboard-view-model-kmachine-card vm) "K-Machine" "Contact for pricing"
                            "<li>Consulting.</li>"))
   :extra-style
   ".vault { border: 2px solid #88c0d0; padding: 2rem; border-radius: 8px; margin-bottom: 2rem; }
    .actions { display: flex; gap: 1rem; align-items: center; flex-wrap: wrap; }
    .tier-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 1.5rem; margin-top: 1.5rem; margin-bottom: 2rem; }
    .tier-card { background: #1e1e1e; border: 2px solid #333; border-radius: 6px; padding: 1.5rem; text-align: center; transition: border-color 0.2s ease; display: flex; flex-direction: column; justify-content: space-between; }
    .tier-card.active { border-color: #88c0d0; background: rgba(136, 192, 208, 0.05); }
    .tier-card-disabled { opacity: 0.55; }
    .tier-badge-soon { background: #ebcb8b; }
    .tier-name { font-family: monospace; font-size: 1.5rem; font-weight: bold; color: #88c0d0; margin-bottom: 0.5rem; }
    .tier-price { font-size: 1.25rem; font-weight: bold; color: #ebcb8b; margin-bottom: 1rem; }
    .tier-benefits { list-style-type: none; padding: 0; margin: 0 0 1.5rem 0; text-align: left; flex-grow: 1; }
    .tier-benefits li { font-size: 0.9rem; color: #ccc; margin-bottom: 0.5rem; position: relative; padding-left: 1.25rem; line-height: 1.4; }
    .tier-benefits li::before { content: '\\2022'; color: #88c0d0; position: absolute; left: 0; font-weight: bold; }
    .tier-badge { background: #88c0d0; color: #111; font-size: 0.8rem; font-weight: bold; padding: 0.25rem 0.5rem; border-radius: 4px; display: inline-block; margin-bottom: 1rem; text-transform: uppercase; align-self: center; }
    .tier-btn { width: 100%; padding: 0.5rem; border-radius: 4px; font-weight: bold; font-size: 0.9rem; border: none; font-family: inherit; }
    .tier-btn-active { background: #88c0d0; color: #111; cursor: default; }
    .tier-btn-disabled { background: #333; color: #666; cursor: not-allowed; }
    .subpage-menu { display: flex; gap: 1rem; flex-wrap: wrap; margin-bottom: 2rem; }
    .subpage-link { padding: 0.5rem 1rem; text-decoration: none; color: #88c0d0; background: #1e1e1e; border: 2px solid #88c0d0; border-radius: 4px; font-weight: bold; transition: all 0.2s ease; }
    .subpage-link:hover { background: rgba(136, 192, 208, 0.15); }
    .modal-overlay { display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0, 0, 0, 0.7); align-items: center; justify-content: center; z-index: 1000; }
    .modal-overlay.open { display: flex; }
    .modal { background: #1e1e1e; border: 2px solid #88c0d0; border-radius: 8px; padding: 2rem; max-width: 480px; width: 90%; box-sizing: border-box; }
    .modal h3 { margin-top: 0; }
    .modal-warning { color: #ebcb8b; font-weight: bold; }
    .modal-key { display: block; word-break: break-all; background: #111; border: 1px solid #333; border-radius: 4px; padding: 1rem; margin: 1rem 0; font-size: 0.95rem; color: #a3be8c; }
    .modal-actions { display: flex; gap: 1rem; justify-content: flex-end; }
    .paste-input { width: 100%; box-sizing: border-box; background: #111; color: #eee; border: 1px solid #333; border-radius: 4px; padding: 0.75rem; font-family: monospace; font-size: 0.9rem; margin-bottom: 1rem; resize: vertical; }
    .pastes-list { margin-top: 1.5rem; display: flex; flex-direction: column; gap: 0.5rem; }
    .paste-row { display: flex; align-items: center; gap: 1rem; background: #1e1e1e; border: 1px solid #333; border-radius: 4px; padding: 0.75rem 1rem; flex-wrap: wrap; }
    .paste-row a { font-family: monospace; }
    .paste-row .paste-meta { color: #888; font-size: 0.85rem; margin-right: auto; }
    .tier-subtext { color: #888; font-size: 0.85rem; margin-top: 0.5rem; }"
   :extra-head (format nil "<meta name='jwt-token' content='~A'>" (html-escape api-jwt))))

(defun render-pastebin-page (api-jwt)
  "Pure (API-JWT -> HTML) rendering function for the standalone /pastebin
page: just the create/list/delete pastebin panel (see PASTEBIN.JS),
with API-JWT embedded via a <meta name='jwt-token'> tag so the page's
fetch() calls can authenticate against the Bearer-secured
/api/v1/pastes endpoints without the user re-entering their API key."
  (html-page
   "Pastebin"
   "<div class='vault'>
      <h2>Manage My Pastes</h2>
      <p>Create and manage your pastes. Shared via a plain link -- anyone with the id can view it.</p>
      <textarea id='paste-input' class='paste-input' rows='10' placeholder='Paste your code here...'></textarea>
      <div class='actions'>
        <button id='btn-create-paste' class='btn'>Create Paste</button>
      </div>
      <div id='my-pastes-list' class='pastes-list'></div>
    </div>
    <div class='actions'>
      <a href='/dashboard' class='btn btn-secondary'>Back to Dashboard</a>
    </div>
    <script src='/js/pastebin.js'></script>"
   :extra-style
   ".vault { border: 2px solid #88c0d0; padding: 2rem; border-radius: 8px; margin-bottom: 2rem; }
    .actions { display: flex; gap: 1rem; align-items: center; flex-wrap: wrap; }
    .paste-input { width: 100%; box-sizing: border-box; background: #111; color: #eee; border: 1px solid #333; border-radius: 4px; padding: 0.75rem; font-family: monospace; font-size: 0.9rem; margin-bottom: 1rem; resize: vertical; }
    .pastes-list { margin-top: 1.5rem; display: flex; flex-direction: column; gap: 0.5rem; }
    .paste-row { display: flex; align-items: center; gap: 1rem; background: #1e1e1e; border: 1px solid #333; border-radius: 4px; padding: 0.75rem 1rem; flex-wrap: wrap; }
    .paste-row a { font-family: monospace; }
    .paste-row .paste-meta { color: #888; font-size: 0.85rem; margin-right: auto; }"
   :extra-head (format nil "<meta name='jwt-token' content='~A'>" (html-escape api-jwt))))

(hunchentoot:define-easy-handler (pastebin-page :uri "/pastebin") ()
  "GET /pastebin: the CONS-tier pastebin management page. Requires an
authenticated session (same check as /dashboard); unauthenticated
visitors are bounced to / (this codebase has no separate /login route --
/ itself hosts the signup/login form), mirroring every other
session-gated handler in this file. On success, mints a fresh 1-hour
programmatic-API JWT (scoped to the user's current tier) and renders
the pastebin panel with it embedded for PASTEBIN.JS to use."
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (if user
        (render-pastebin-page
         (jrm-auth:generate-api-jwt
          user (api-jwt-tier-string (or (jrm-auth:get-user-tier user) :free))))
        (hunchentoot:redirect "/"))))

(hunchentoot:define-easy-handler (dashboard-page :uri "/dashboard") (checkout-status next)
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (if user
        (let* ((user-data (car (jrm-auth:get-user user)))
               (vm (dashboard-view-model user-data checkout-status next)))
          ;; Refresh the membership JWT on every dashboard visit so tier
          ;; changes (upgrades/downgrades) propagate promptly, while still
          ;; letting the cookie persist independently of session/DB state.
          (issue-membership-jwt user (dashboard-view-model-tier vm) (dashboard-view-model-wheel-p vm))
          ;; If we just came back from a successful upgrade purchase and a
          ;; `next` breadcrumb is present (set when the user was bounced
          ;; here from an upgrade-required page), send them straight back
          ;; to the originally-requested JWT-protected page now that their
          ;; freshly-issued JWT reflects the new tier -- instead of showing
          ;; the dashboard.
          (when (dashboard-view-model-redirect-to vm)
            (hunchentoot:redirect (dashboard-view-model-redirect-to vm)))
          (render-dashboard vm
                             :wheel-link-html (if (dashboard-view-model-wheel-p vm)
                                                   "<a href='/admin/members' class='btn' style='background: #d08770;'>Manage Members</a>"
                                                   "")
                             ;; A short-lived programmatic-API JWT (distinct
                             ;; from the long-lived membership cookie JWT
                             ;; above), embedded into the page so the
                             ;; dashboard's pastebin panel (see
                             ;; pastebin.js) can call the Bearer-secured
                             ;; /api/v1/pastes endpoints without the user
                             ;; ever re-entering their API key.
                             :api-jwt (jrm-auth:generate-api-jwt
                                       user (api-jwt-tier-string (or (jrm-auth:get-user-tier user) :free)))
                             :csrf-token (csrf-input-html)))
        (hunchentoot:redirect "/"))))

(hunchentoot:define-easy-handler (regenerate-recovery-page :uri "/regenerate-recovery") ()
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (if user
        (progn
          (jrm-auth:delete-recovery-codes user)
          (let ((codes (jrm-auth:generate-recovery-codes user)))
            (format nil "<html>
                           <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; } li { font-family: monospace; font-size: 1.2rem; color: #0f0; }</style></head>
                           <body><h2>New Recovery Codes</h2>
                             <p>Save these offline. Your old recovery codes have been invalidated and replaced.</p>
                             <p><b>You will only see this screen once.</b></p>
                             <ul>~{<li>~A</li>~}</ul>
                             <br><a href='/dashboard' style='color: #fff; font-weight: bold;'>Return to Dashboard</a>
                           </body>
                         </html>" codes)))
        (hunchentoot:redirect "/"))))

(hunchentoot:define-easy-handler (regenerate-2fa-page :uri "/regenerate-2fa") (totp-code)
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (unless user (hunchentoot:redirect "/"))
    (case (hunchentoot:request-method*)
      (:get
       (let* ((secret (totp:generate-secret))
              (uri (totp:generate-qr-uri secret user :issuer "JRM-Code")))
         (setf (hunchentoot:session-value :temp-regen-secret) secret)
         (let ((qr-url (format nil "https://api.qrserver.com/v1/create-qr-code/?size=250x250&data=~A" (hunchentoot:url-encode uri))))
           (format nil "<html>
                          <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; }</style></head>
                          <body><h2>Regenerate 2FA Setup</h2>
                            <p>Scan this QR code with Google Authenticator or Authy to configure your new 2FA key:</p>
                            <img src='~A' style='border: 10px solid white;'><br><br>
                            <p>Or manually enter this secret: <b style='color: #0f0; font-family: monospace;'>~A</b></p>
                            <form method='POST'>
                              ~A
                              Enter 6-digit code: <input type='text' name='totp-code' required><br>
                              <input type='submit' value='Verify & Save'>
                            </form>
                          </body>
                        </html>" qr-url (string-upcase secret) (csrf-input-html)))))
      (:post
       (with-csrf-protection
           (let ((secret (hunchentoot:session-value :temp-regen-secret)))
             (if (and secret totp-code (totp:verify-totp secret totp-code))
                 (progn
                   (jrm-auth:activate-user-2fa user secret)
                   (setf (hunchentoot:session-value :temp-regen-secret) nil)
                   (hunchentoot:redirect "/dashboard"))
                 (format nil "<html><head><style>body { background: #111; color: #f00; }</style></head><body><h2>Error</h2><p>Invalid code. <a href='/regenerate-2fa' style='color:#fff;'>Try again</a>.</p></body></html>"))))))))

(hunchentoot:define-easy-handler (change-password-page :uri "/change-password") (current-password new-password confirm-password)
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (unless user (hunchentoot:redirect "/"))
    (flet ((render (&optional message message-class)
             (format nil "<html>
                            <head><style>
                              body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; }
                              form { max-width: 320px; }
                              label { display: block; margin-bottom: 0.25rem; }
                              input { width: 100%; box-sizing: border-box; padding: 0.5rem; margin-bottom: 1rem; border-radius: 4px; border: 1px solid #333; background: #1e1e1e; color: #eee; }
                              input[type=submit] { width: auto; background: #88c0d0; color: #111; font-weight: bold; border: none; cursor: pointer; }
                              .notification { padding: 1rem; border-radius: 4px; margin-bottom: 1.5rem; border: 1px solid; }
                              .notification-error { background: rgba(191, 97, 106, 0.2); border-color: #bf616a; color: #bf616a; }
                              .notification-success { background: rgba(163, 190, 140, 0.2); border-color: #a3be8c; color: #a3be8c; }
                            </style></head>
                            <body>
                              <h2>Change Password</h2>
                              ~A
                              <form method='POST'>
                                ~A
                                <label for='current-password'>Current Password</label>
                                <input type='password' name='current-password' id='current-password' required>
                                <label for='new-password'>New Password</label>
                                <input type='password' name='new-password' id='new-password' required minlength='8'>
                                <label for='confirm-password'>Confirm New Password</label>
                                <input type='password' name='confirm-password' id='confirm-password' required minlength='8'>
                                <input type='submit' value='Update Password'>
                              </form>
                              <p><a href='/dashboard' style='color: #fff;'>&larr; Return to Dashboard</a></p>
                            </body>
                          </html>"
                     (if message
                         (format nil "<div class='notification notification-~A'>~A</div>" message-class (hunchentoot:escape-for-html message))
                         "")
                     (csrf-input-html))))
      (case (hunchentoot:request-method*)
        (:get (render))
        (:post
         (with-csrf-protection
             (let ((user-data (car (jrm-auth:get-user user))))
               (cond
                 ((not (jrm-auth:check-password current-password (jrm-auth:user-password-hash user-data)))
                  (render "Current password is incorrect." "error"))
                 ((not (string= new-password confirm-password))
                  (render "New password and confirmation do not match." "error"))
                 ((< (length new-password) 8)
                  (render "New password must be at least 8 characters long." "error"))
                 (t
                  (jrm-auth:update-user-password user new-password)
                  (render "Your password has been updated." "success"))))))))))

(hunchentoot:define-easy-handler (logout-page :uri "/logout") ()
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value :authenticated-user) nil)
  (hunchentoot:remove-session hunchentoot:*session*)
  ;; Intentionally leave the membership JWT cookie in place: it lets the
  ;; user retain access to feature-preview pages for its remaining
  ;; lifetime even after logging out.
  (hunchentoot:redirect "/"))

(hunchentoot:define-easy-handler (delete-account-action :uri "/delete-account") ()
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (if user
        (case (hunchentoot:request-method*)
          (:post
           (with-csrf-protection
               ;; If they have an active paid subscription, cancel it and refund
               ;; the pro-rated unused portion before removing their account.
               (let* ((user-data (car (jrm-auth:get-user user)))
                      (subscription-id (jrm-auth:user-stripe-subscription-id user-data)))
                 (when subscription-id
                   (cancel-stripe-subscription-with-prorated-refund subscription-id)))
             ;; Nuke the user from the database
             (jrm-auth:delete-user user)
             ;; Obliterate the session
             (setf (hunchentoot:session-value :authenticated-user) nil)
             (hunchentoot:remove-session hunchentoot:*session*)
             ;; Kick them out to the cold
             (hunchentoot:redirect "/")))
          (t (hunchentoot:redirect "/dashboard")))
        (hunchentoot:redirect "/"))))
