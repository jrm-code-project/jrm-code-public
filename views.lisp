;;; -*- Lisp -*-

;;; Pure, composable HTML rendering combinators. Each is a plain
;;; string(s) -> string function with no I/O, so it is directly unit
;;; testable with hand-built fixtures. See FUNCTIONAL_REFACTOR.md Phase 5.
;;;
;;; Every combinator that interpolates a *user-controlled* value routes it
;;; through HTML-ESCAPE internally, so escaping is structurally guaranteed
;;; rather than convention-dependent (closes the gap called out in
;;; FUNCTIONAL_REFACTOR.md Sec 1.3) -- callers no longer need to remember
;;; to wrap every interpolated value in HUNCHENTOOT:ESCAPE-FOR-HTML by hand.

(in-package "JRM-CODE-PROJECT")

(defun html-escape (value)
  "Escape VALUE (coerced to a string; NIL becomes \"\") for safe interpolation
into HTML markup. The single combinator through which every
user-controlled value should flow before being spliced into a rendering
combinator's output."
  (hunchentoot:escape-for-html (if value (princ-to-string value) "")))

(defun html-page (title body-html &key (extra-style "") (extra-head ""))
  "Wrap BODY-HTML in the standard page chrome: an HTML5 doctype, a <head>
with TITLE (escaped), the shared dark-theme base stylesheet (plus any
EXTRA-STYLE rules a specific page needs on top of it), any EXTRA-HEAD
markup (e.g. a <meta> tag a page-specific script needs to read), and a
<body>. Pure: TITLE and BODY-HTML are simply spliced in (BODY-HTML is
trusted markup already built by other rendering combinators, not raw
user input)."
  (format nil "<html>
                 <head>
                   <title>~A</title>
                   ~A
                   <style>
                     body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; }
                     a { color: #88c0d0; }
                     .btn { display: inline-block; padding: 0.5rem 1rem; text-decoration: none; color: #111; background: #88c0d0; border-radius: 4px; font-weight: bold; font-family: inherit; font-size: 1rem; border: none; cursor: pointer; }
                     .btn-secondary { background: transparent; color: #eee; border: 2px solid #666; }
                     .btn-danger { background: transparent; color: #bf616a; border: 2px solid #bf616a; cursor: pointer; transition: all 0.2s ease; }
                     .btn-danger:hover { background: rgba(191, 97, 106, 0.2); }
                     .notification { padding: 1rem; border-radius: 4px; margin-bottom: 1.5rem; border: 1px solid; }
                     .notification-success { background: rgba(163, 190, 140, 0.2); border-color: #a3be8c; color: #a3be8c; }
                     .notification-cancel { background: rgba(191, 97, 106, 0.2); border-color: #bf616a; color: #bf616a; }
                     .notification-error { background: rgba(191, 97, 106, 0.2); border-color: #bf616a; color: #bf616a; }
                     ~A
                   </style>
                 </head>
                 <body>
                   ~A
                 </body>
               </html>"
          (html-escape title)
          extra-head
          extra-style
          body-html))

(defun html-notification (kind text)
  "Return a standard notification banner <div> of KIND (:success, :cancel, or
:error) containing TEXT (escaped). Returns \"\" if KIND is NIL (no
notification to show)."
  (if (null kind)
      ""
      (format nil "<div class='notification notification-~(~A~)'>~A</div>"
              (string-downcase (symbol-name kind))
              (html-escape text))))

(defun html-form (action inner-html &key (method "POST") csrf-token (style "display:inline;") onsubmit)
  "Wrap INNER-HTML (trusted markup for the form's fields/buttons, already
built by the caller) in a <form METHOD ACTION> tag, splicing in a hidden
CSRF-TOKEN input first if one is supplied (from CSRF-INPUT-HTML), so every
POST form built with this combinator carries its CSRF token structurally
rather than by each caller remembering to include it. STYLE is spliced
into the form tag's inline style attribute; ONSUBMIT, if supplied, becomes
the form's onsubmit handler (assumed to be trusted, statically-authored
JavaScript, not user input)."
  (format nil "<form method='~A' action='~A' style='~A'~@[ onsubmit=\"~A\"~]>~A~A</form>"
          (html-escape method) (html-escape action) style onsubmit
          (or csrf-token "") inner-html))

;; --- SIGNUP PAGE RENDERING (pure) ---
;;
;; Extracted from AUTH.LISP's SIGNUP-PAGE handler so the actual markup
;; construction is a set of small, directly unit-testable pure functions
;; (see TESTS.LISP's SIGNUP-PAGE-RENDERING-* tests) rather than being
;; inlined in the route handler alongside session/DB logic. Every one of
;; these routes any caller-supplied value through HTML-ESCAPE, which is
;; what closes the reflected-XSS gap previously found in the GET form's
;; prefilled username field (a raw ~A splice with no escaping).

(defun render-signup-form-page (&key (prefill-username ""))
  "The GET /signup page: a bare signup form, with PREFILL-USERNAME (escaped)
pre-populating the username field -- e.g. carried over from an agent-
friendly `?username=' query parameter, or empty for a fresh visit."
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
          (csrf-input-html) (html-escape prefill-username)))

(defun render-signup-error-page (message-html &key retry-href retry-label)
  "A generic 'Signup Failed' error page displaying MESSAGE-HTML (trusted
markup, already escaped/built by the caller -- see HTML-ESCAPE) with an
optional single link back (RETRY-HREF/RETRY-LABEL, e.g. back to /signup
to try again or to / to log in)."
  (format nil "<html>
                <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; }</style></head>
                <body><h2>Signup Failed</h2>
                  <p style='color: #bf616a;'>~A</p>
                  ~@[<a href='~A' style='color: #88c0d0;'>~A</a>~]
                </body>
              </html>"
          message-html
          retry-href (html-escape retry-label)))

(defun render-username-taken-page ()
  "The specific 'Signup Failed' page shown when JRM-AUTH:CREATE-USER
reports the username is already in use."
  (render-signup-error-page "An account with that username already exists."
                             :retry-href "/" :retry-label "Return to Login"))

(defun render-invalid-username-page ()
  "The specific 'Signup Failed' page shown when VALID-USERNAME-P rejects
the submitted username."
  (render-signup-error-page "Invalid username formatting."
                             :retry-href "/signup" :retry-label "Try Again"))

(defun render-signup-missing-fields-page ()
  "The plain error page shown when the signup POST is missing a username
and/or password entirely."
  "<html><body><h2>Error</h2><p>Username and password are required.</p></body></html>")

;; --- ADMIN MEMBER-ADMINISTRATION VIEW (see TECHNICAL_DEBT.md item 5) ---
;;
;; Moved out of ADMIN-MEMBERS-PAGE (admin.lisp) so the handler itself is a
;; thin dispatcher (fetch data -> compute pagination -> render), mirroring
;; the SIGNUP-PAGE decomposition above. Every caller-supplied value
;; (search term, usernames, tier, subscription status) routes through
;; HTML-ESCAPE here rather than at the call site, so escaping is
;; structurally guaranteed the same way it is for the rest of this file.

(defun render-admin-member-row (username tier is-wheel-p has-subscription-p subscription-status)
  "Pure rendering function for one <tr> in the /admin/members table:
USERNAME/TIER/IS-WHEEL-P/HAS-SUBSCRIPTION-P/SUBSCRIPTION-STATUS are plain
data pulled from a USER struct by the caller (ADMIN.LISP), not the
struct itself, so this function has no dependency on JRM-AUTH:USER's
shape. Builds the three per-row admin action forms (set-tier/
toggle-wheel/refund-and-delete) via HTML-FORM so each form's CSRF token
is carried structurally."
  (let* ((set-tier-form
          (html-form "/admin/members/set-tier"
                     (format nil "<input type='hidden' name='username' value='~A'>
                                  <select name='tier'>
                                    <option value='CONS' ~A>CONS</option>
                                    <option value='CADR' ~A>CADR</option>
                                    <option value='LAMBDA' ~A>LAMBDA</option>
                                  </select>
                                  <button type='submit' class='btn-sm'>Set</button>"
                             (html-escape username)
                             (if (string-equal tier "CONS") "selected" "")
                             (if (string-equal tier "CADR") "selected" "")
                             (if (string-equal tier "LAMBDA") "selected" ""))
                     :style "display:inline-flex;gap:0.4rem;"
                     :csrf-token (csrf-input-html)))
         (toggle-wheel-form
          (html-form "/admin/members/toggle-wheel"
                     (format nil "<input type='hidden' name='username' value='~A'>
                                  <button type='submit' class='btn-sm'>~A</button>"
                             (html-escape username)
                             (if is-wheel-p "Revoke Wheel" "Make Wheel"))
                     :csrf-token (csrf-input-html)))
         (refund-and-delete-form
          (html-form "/admin/members/refund-and-delete"
                     (format nil "<input type='hidden' name='username' value='~A'>
                                  <label><input type='checkbox' name='delete-too' value='yes'> also delete</label>
                                  <button type='submit' class='btn-sm btn-sm-danger'>Refund~A</button>"
                             (html-escape username)
                             (if has-subscription-p "/Delete" ""))
                     :csrf-token (csrf-input-html)
                     :onsubmit (format nil "return confirm('This will cancel/refund any active subscription and permanently delete ~A. Are you sure?');"
                                        (html-escape username)))))
    (format nil "<tr>
                   <td>~A</td>
                   <td>~A</td>
                   <td>~A~A</td>
                   <td>~A</td>
                   <td>~A</td>
                 </tr>"
            (html-escape username)
            set-tier-form
            (html-escape subscription-status)
            (if has-subscription-p " (paid)" "")
            toggle-wheel-form
            refund-and-delete-form)))

(defun render-admin-members-page (&key search member-rows-html page-num total-pages total has-prev has-next)
  "Pure rendering function for the whole /admin/members page body:
SEARCH is the raw (un-escaped -- escaped internally) search-box value,
MEMBER-ROWS-HTML is a pre-rendered list of RENDER-ADMIN-MEMBER-ROW
results (already-trusted markup), and the remaining keys describe the
current page/pager state exactly as ADMIN-PAGINATION captures it."
  (html-page
   "Member Administration"
   (format nil "<h2>Member Administration</h2>
                <p><a href='/dashboard' style='color:#88c0d0;'>&larr; Return to Dashboard</a></p>
                <form method='GET' action='/admin/members'>
                  <input type='text' name='q' placeholder='Search by username' value='~A'>
                  <button type='submit' class='btn'>Search</button>
                </form>
                <table>
                  <thead><tr><th>Username</th><th>Tier</th><th>Subscription</th><th>Wheel</th><th>Refund / Delete</th></tr></thead>
                  <tbody>~{~A~}</tbody>
                </table>
                <div class='pager'>
                  ~A
                  <span>Page ~A of ~A (~A members)</span>
                  ~A
                </div>
                <div class='add-member'>
                  <h3>Add Member (no payment required)</h3>
                  ~A
                </div>"
           (html-escape search)
           member-rows-html
           (if has-prev
               (format nil "<a class='btn' href='/admin/members?q=~A&page=~A'>&laquo; Prev</a>"
                       (hunchentoot:url-encode search) (1- page-num))
               "")
           (1+ page-num) total-pages total
           (if has-next
               (format nil "<a class='btn' href='/admin/members?q=~A&page=~A'>Next &raquo;</a>"
                       (hunchentoot:url-encode search) (1+ page-num))
               "")
           (html-form "/admin/members/create"
                      "<input type='text' name='username' placeholder='username' required>
                       <input type='password' name='password' placeholder='temporary password' required>
                       <select name='tier'>
                         <option value='CONS'>CONS</option>
                         <option value='CADR'>CADR</option>
                         <option value='LAMBDA'>LAMBDA</option>
                       </select>
                       <button type='submit' class='btn'>Create</button>"
                      :csrf-token (csrf-input-html)))
   :extra-style "table { width: 100%; border-collapse: collapse; margin-top: 1rem; }
                 th, td { text-align: left; padding: 0.5rem; border-bottom: 1px solid #333; vertical-align: middle; }
                 th { color: #88c0d0; }
                 .btn-sm { padding: 0.25rem 0.6rem; border-radius: 4px; border: none; background: #4c566a; color: #eee; cursor: pointer; }
                 .btn-sm-danger { background: #bf616a; }
                 select { background: #1a1a1a; color: #eee; border: 1px solid #444; border-radius: 4px; padding: 0.2rem; }
                 input[type=text] { background: #1a1a1a; color: #eee; border: 1px solid #444; border-radius: 4px; padding: 0.4rem; }
                 .pager { margin-top: 1rem; display: flex; gap: 1rem; align-items: center; }
                 .add-member { margin-top: 2rem; border: 1px solid #333; border-radius: 8px; padding: 1rem; }"))

;; --- 2FA / RECOVERY-CODE VIEW RENDERING (see TECHNICAL_DEBT.md items 5/6) ---
;;
;; Extracted from AUTH.LISP's SETUP-2FA-PAGE, RECOVERY-CODES-PAGE, and
;; CHALLENGE-2FA-PAGE handlers, mirroring the SIGNUP-PAGE/ADMIN-MEMBERS-PAGE
;; decomposition above: every caller-supplied value (QR image URL, TOTP
;; secret, recovery codes) routes through HTML-ESCAPE here so escaping is
;; structurally guaranteed rather than convention-dependent.

(defun render-setup-2fa-page (qr-url secret csrf-token-html)
  "Pure rendering function for the GET /setup-2fa page: QR-URL is the
already-built QR-code image URL, SECRET is the raw base32 TOTP secret (will
be upcased and escaped here), and CSRF-TOKEN-HTML is the pre-built hidden
CSRF input (from CSRF-INPUT-HTML)."
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
               </html>"
          (html-escape qr-url) (html-escape (string-upcase secret)) csrf-token-html))

(defun render-2fa-invalid-code-page (retry-href)
  "Pure rendering function for the 'invalid TOTP code' error page shared by
SETUP-2FA-PAGE and CHALLENGE-2FA-PAGE's POST handlers. RETRY-HREF is the
statically-authored (not user-controlled) link back to the form."
  (format nil "<html><head><style>body { background: #111; color: #f00; }</style></head><body><h2>Error</h2><p>Invalid code. <a href='~A' style='color:#fff;'>Try again</a>.</p></body></html>"
          (html-escape retry-href)))

(defun render-recovery-codes-page (codes)
  "Pure rendering function for the one-time /recovery-codes display: CODES is
a list of raw recovery-code strings, each escaped before being spliced into
its own <li>."
  (format nil "<html>
                 <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; } li { font-family: monospace; font-size: 1.2rem; color: #0f0; }</style></head>
                 <body><h2>Your Recovery Codes</h2>
                   <p>Print these or save them offline. If you lose your phone, these are your only way back in.</p>
                   <p><b>You will only see this screen once.</b></p>
                   <ul>~{<li>~A</li>~}</ul>
                   <br><a href='/' style='color: #fff;'>Proceed to Login</a>
                 </body>
               </html>" (mapcar #'html-escape codes)))

(defun render-challenge-2fa-page (csrf-token-html)
  "Pure rendering function for the GET /challenge-2fa page. CSRF-TOKEN-HTML
is the pre-built hidden CSRF input (from CSRF-INPUT-HTML); there is no other
caller-supplied data on this page."
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
               </html>" csrf-token-html))

;; --- ACCOUNT-SETTINGS VIEW RENDERING (see TECHNICAL_DEBT.md items 5/6/8) ---
;;
;; Extracted from AUTH.LISP's REGENERATE-RECOVERY-PAGE, REGENERATE-2FA-PAGE,
;; and CHANGE-PASSWORD-PAGE handlers -- the last remaining handlers that
;; inlined raw FORMAT NIL HTML alongside session/DB logic (finishing off
;; items 5/6). Every caller-supplied value routes through HTML-ESCAPE here.

(defun render-regenerate-recovery-page (codes)
  "Pure rendering function for the /regenerate-recovery display: CODES is a
list of raw recovery-code strings, each escaped before being spliced into
its own <li>. Distinct from RENDER-RECOVERY-CODES-PAGE only in copy (this
is the 'regenerated' variant, reachable from the dashboard rather than the
signup flow) and its return link."
  (format nil "<html>
                 <head><style>body { font-family: sans-serif; background: #111; color: #eee; padding: 2rem; } li { font-family: monospace; font-size: 1.2rem; color: #0f0; }</style></head>
                 <body><h2>New Recovery Codes</h2>
                   <p>Save these offline. Your old recovery codes have been invalidated and replaced.</p>
                   <p><b>You will only see this screen once.</b></p>
                   <ul>~{<li>~A</li>~}</ul>
                   <br><a href='/dashboard' style='color: #fff; font-weight: bold;'>Return to Dashboard</a>
                 </body>
               </html>" (mapcar #'html-escape codes)))

(defun render-regenerate-2fa-page (qr-url secret csrf-token-html)
  "Pure rendering function for the GET /regenerate-2fa page: QR-URL is the
already-built QR-code image URL, SECRET is the raw base32 TOTP secret
(escaped and upcased here), and CSRF-TOKEN-HTML is the pre-built hidden
CSRF input."
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
               </html>"
          (html-escape qr-url) (html-escape (string-upcase secret)) csrf-token-html))

(defun render-change-password-page (csrf-token-html &optional message message-class)
  "Pure rendering function for the /change-password page (both the initial
GET and every POST outcome, success or error, re-render the same form with
an optional inline notification banner). MESSAGE, if supplied, is escaped;
MESSAGE-CLASS is a statically-authored CSS class name (\"error\" or
\"success\"), not user input."
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
              (format nil "<div class='notification notification-~A'>~A</div>" message-class (html-escape message))
              "")
          csrf-token-html))


