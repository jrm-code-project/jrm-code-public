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
  "The plain error page shown when the signup POST is missing a username entirely."
  "<html><body><h2>Error</h2><p>Username is required.</p></body></html>")
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
                       <select name='tier'>
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

;; --- ACCOUNT-SETTINGS VIEW RENDERING (see TECHNICAL_DEBT.md items 5/6/8) ---
;;
;; Extracted from AUTH.LISP's REGENERATE-RECOVERY-PAGE, REGENERATE-2FA-PAGE,
;; and CHANGE-PASSWORD-PAGE handlers -- the last remaining handlers that
;; inlined raw FORMAT NIL HTML alongside session/DB logic (finishing off
;; items 5/6). Every caller-supplied value routes through HTML-ESCAPE here.

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

(defun render-webauthn-register-page (csrf-token-html)
  "Pure rendering function for GET /setup-webauthn: displays the button
that registers a new Passkey and posts it to Lisp. The registration
logic lives in an external script (js/webauthn-register.js) rather than
an inline <script> block, because the site's Content-Security-Policy is
`script-src 'self' ...` with no 'unsafe-inline' -- an inline <script> (or
an inline onclick='...' handler) is silently blocked by the browser."
  (html-page 
   "Setup Passkey"
   (format nil "<h2>Setup Passkey</h2>
                <p>Register a hardware key, TouchID, or Windows Hello.</p>
                <form id='webauthn-form'>
                  ~A
                  <button type='button' id='webauthn-register-btn' class='btn'>Register Passkey</button>
                </form>
                <div id='webauthn-error' class='notification notification-error' style='display:none;'></div>"
           csrf-token-html)
   :extra-head "<script src='/js/webauthn-common.js'></script><script src='/js/webauthn-register.js'></script>"))

(defun render-onboard-link-invalid-page ()
  "Pure rendering function for the GET /onboard 'Link Expired' error page,
shown when the bootstrap admin onboarding token is missing, malformed,
expired, or doesn't match *BOOTSTRAP-ADMIN-EMAIL* (see
SEND-BOOTSTRAP-ADMIN-ONBOARDING-EMAIL/ONBOARD-PAGE in server.lisp/auth.lisp).
Contains no interpolated user-controlled data, so nothing needs escaping."
  (html-page
   "Link Expired"
   "<h2>Link Expired</h2>
    <p>This onboarding link is invalid or has expired. Onboarding links are
    only valid for a short time after being emailed.</p>
    <p><a href='/' class='btn'>Return to Login</a></p>"))

;; --- HERESIES ARTICLE CHROME (moved here from heresies.lisp -- see
;; TECHNICAL_DEBT.md items #5/#6: pure HTML-rendering functions belong
;; alongside the rest of the escaping-safe view layer, not scattered
;; across individual feature files) ---

(defun heresy-index-items-html (uri-prefix slugs title-fn)
  "Build the <li> list shared by both the full and teaser heresies
indexes: for each slug in SLUGS, a link under URI-PREFIX labeled by
calling TITLE-FN on the slug (escaped)."
  (format nil "~{~A~%~}"
          (mapcar (lambda (slug)
                    (format nil "<li><a href=\"~A~A.html\">~A</a></li>"
                            uri-prefix slug (html-escape (funcall title-fn slug))))
                  slugs)))

(defun render-rdescent-page (jwt-or-nil)
  "Render the public /rdescent.html page (\"Recursive Descent\"): a
hidden data div carrying JWT-OR-NIL (the caller's presented JWT,
HTML-escaped, or an empty string if none was presented) for
/js/rdescent.js to scrape out of the DOM via its data-jwt attribute,
a static \"Recursive Descent\" <h2> title (CSS class \"game-title\") atop
the side panel, plus a message-log div and a playing-field div for the
game engine to render into. #playing-field is the scrollable outer
box; it contains #playing-field-inner, a position:relative wrapper
sized to the grid's own intrinsic content width (via CSS
width:fit-content) and horizontally centered within #playing-field
(via margin:0 auto) -- because #playing-field-inner's own top-left
corner therefore always coincides with the grid's column-0/row-0
character regardless of how much empty space the centering pushes it
right by on a given viewport, its two children -- #playing-field-grid
(the actual grid HTML, replaced wholesale every tick by the grid
packet) and a sibling #targeting-cursor overlay div -- can both use
simple, viewport-independent ch/em pixel math to line the cursor up
with the grid (see /js/rdescent.js's UPDATETARGETINGCURSOR and
style.css's #targeting-cursor rule). #targeting-cursor is kept as a
SIBLING rather than a child of #playing-field-grid specifically
because #playing-field-grid's innerHTML is fully replaced every tick,
which would otherwise destroy a cursor element living inside it.
#playing-field-grid and #targeting-cursor are deliberately written on
a single source line with no whitespace between their tags: since
#playing-field-grid's own CSS is WHITE-SPACE:PRE (needed to render the
grid packet's row-separating newlines verbatim), any literal
newline/indentation between these two divs in this format string
would itself be preserved as a real blank line if it were inherited
onto -- or otherwise present as a text node within -- a WHITE-SPACE:PRE
ancestor, pushing the rendered grid down and desyncing it from
#targeting-cursor's fixed-offset math. Also
rendered: a hidden #message-log-modal overlay
(toggled by the 'v' key -- see /js/rdescent.js) that displays the
last 50 message-log entries in a scrollable box, a hidden
#inventory-modal overlay (toggled by the 'i' key) listing the
player's current inventory items for selection before entering
targeting mode, a hidden #equipment-modal overlay (toggled by the 'u'
key) listing the player's four equipment slots and their occupants for
selection before sending an \"unequip\" command (see /js/rdescent.js's
RENDEREQUIPMENTMODAL), a hidden #plaque-modal overlay (server-triggered
by a one-shot \"plaque\" packet -- see RDESCENT-OUTBOUND-PACKETS/
TICK-ALL-CLIENTS, RDESCENT/SERVER.LISP, and /js/rdescent.js's
SHOWPLAQUEMODAL -- when a player reads a final-level Commemorative
Plaque, dismissed by Escape) displaying that plaque's own congratulatory
text, and a hidden #legend-modal overlay (toggled by the
'?' key, dismissed by Escape) with static markup documenting every
keybinding -- unlike the other two modals, its content never changes
at runtime, so it needs no JS-populated body element, just a plain
[hidden]-attribute toggle (see /js/rdescent.js's TOGGLELEGENDMODAL).
The side panel's PLAYER-STATS div contains
static, persistent child nodes -- #stats-depth (Level: X/Y text),
#stats-room (just the room-kind name, e.g. \"Cubicle Farm\" --
centered via CSS, unlike its label-prefixed siblings -- directly below
#stats-depth --
see ROOM-KIND-DISPLAY-NAME, rdescent/dungeon.lisp, and
RDESCENT-PLAYER-STATS-PACKET's \"room-html\" field, rdescent/server.lisp),
a .health-bar-container with permanent #stats-hp-bar/#stats-dmg-bar
spans and a #stats-hp-text overlay, #stats-xp, #stats-rsu (RDESCENT's
gold/loot currency, displayed directly after XP -- see
FORMAT-RSU-FOR-HTML in rdescent/entities.lisp for why its padding is one
character narrower than XP's own), #stats-kombucha, a static
#player-corporate-stats grid of seven .stat-row divs (Bandwidth/Pivot/
Caffeine Tol/Domain Know/Seniority/Synergy/Hygiene -- ENTITY's seven
flavor-only \"Corporate RPG Stats\", each rolled once via ROLL-STAT --
see rdescent/mechanics.lisp), each a flex row pairing a static label span
with a #val-... span (e.g. #val-bandwidth) -- so that /js/rdescent.js's
tick handler (see APPLYSERVERPACKET's 'player-stats
case) can update their CSS width/textContent in place every tick
rather than destroying and recreating them via innerHTML: only
mutating existing DOM nodes' style.width lets the CSS transition on
.health-bar-hp/.health-bar-dmg actually animate, and never emitting
literal whitespace/<br> around the bar avoids the stray blank line a
naive innerHTML replacement produced, and finally #stats-equipment (a
plain textContent list of currently-equipped item names by slot,
rebuilt each tick from RDESCENT-PLAYER-STATS-PACKET's \"equipment\"
field -- see /js/rdescent.js's APPLYSERVERPACKET 'player-stats case),
placed below the Corporate RPG Stats grid so equipped gear reads as
its own section beneath the core stat block. Uses the site's standard
stylesheet (/css/style.css) and loads /js/rdescent.js. Pure:
JWT-OR-NIL is simply escaped and spliced in -- no validation is
performed here, since /rdescent.html is public and does not require a
JWT to load."
  (format nil "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>Recursive Descent</title>
  <link rel=\"stylesheet\" href=\"/css/style.css?v=12\">
</head>
<body>
  <div id=\"rdescent-data\" data-jwt=\"~A\" hidden style=\"display:none;position:absolute;width:0;height:0;overflow:hidden;margin:0;padding:0;\"></div>
  <div id=\"rdescent-game\" tabindex=\"0\">
    <div id=\"game-container\">
      <div id=\"top-row\">
        <div id=\"side-panel\">
          <h2 class=\"game-title\">Recursive Descent</h2>
          <div id=\"player-stats\">
            <div id=\"stats-depth\"></div>
            <div id=\"stats-room\"></div>
            <div class=\"health-bar-container\">
              <span id=\"stats-hp-bar\" class=\"health-bar-hp\"></span><span id=\"stats-dmg-bar\" class=\"health-bar-dmg\"></span><span id=\"stats-hp-text\" class=\"health-bar-text\"></span>
            </div>
            <div id=\"stats-xp\"></div>
            <div id=\"stats-rsu\"></div>
            <div id=\"stats-kombucha\"></div>
            <div id=\"player-corporate-stats\">
              <div class=\"stat-row\"><span>Bandwidth:</span> <span id=\"val-bandwidth\"></span></div>
              <div class=\"stat-row\"><span>Pivot:</span> <span id=\"val-pivot\"></span></div>
              <div class=\"stat-row\"><span>Caffeine Tol:</span> <span id=\"val-caffeine-tolerance\"></span></div>
              <div class=\"stat-row\"><span>Domain Know:</span> <span id=\"val-domain-knowledge\"></span></div>
              <div class=\"stat-row\"><span>Seniority:</span> <span id=\"val-seniority\"></span></div>
              <div class=\"stat-row\"><span>Synergy:</span> <span id=\"val-synergy\"></span></div>
              <div class=\"stat-row\"><span>Hygiene:</span> <span id=\"val-hygiene\"></span></div>
            </div>
            <div id=\"stats-equipment\"></div>
          </div>
        </div>
        <div id=\"playing-field\">
          <div id=\"playing-field-inner\"><div id=\"playing-field-grid\"></div><div id=\"targeting-cursor\" hidden></div></div>
        </div>
      </div>
      <div id=\"message-log\"></div>
    </div>
  </div>
  <div id=\"message-log-modal\" hidden>
    <div id=\"message-log-modal-content\">
      <h3>Message History</h3>
      <div id=\"message-log-modal-body\" tabindex=\"0\"></div>
    </div>
  </div>
  <div id=\"inventory-modal\" hidden>
    <div id=\"inventory-modal-content\">
      <h3>Inventory</h3>
      <div id=\"inventory-modal-body\" tabindex=\"0\"></div>
    </div>
  </div>
  <div id=\"equipment-modal\" hidden>
    <div id=\"equipment-modal-content\">
      <h3>Equipment</h3>
      <div id=\"equipment-modal-body\" tabindex=\"0\"></div>
    </div>
  </div>
  <div id=\"plaque-modal\" hidden>
    <div id=\"plaque-modal-content\">
      <h3>Commemorative Plaque</h3>
      <div id=\"plaque-modal-body\" tabindex=\"0\"></div>
    </div>
  </div>
  <div id=\"legend-modal\" hidden>
    <div id=\"legend-modal-content\">
      <h3>Controls</h3>
      <div id=\"legend-modal-body\" tabindex=\"0\">
        <div class=\"legend-row\"><span class=\"legend-key\">Arrow Keys</span><span class=\"legend-desc\">Move / attack an adjacent enemy</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">g</span><span class=\"legend-desc\">Grab whatever item is on the floor underfoot</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">t</span><span class=\"legend-desc\">Interact with a shrine, vendor, or other fixture underfoot</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">s</span><span class=\"legend-desc\">Save game state to browser</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">i</span><span class=\"legend-desc\">Open inventory</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">Arrow Keys, Enter</span><span class=\"legend-desc\">(in Inventory) Select an item to use, then aim and confirm</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">e</span><span class=\"legend-desc\">(in Inventory) Equip the selected item, if equippable</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">d</span><span class=\"legend-desc\">(in Inventory) Drop the selected item</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">u</span><span class=\"legend-desc\">Open equipment / unequip an item (cursed items can't be unequipped)</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">k</span><span class=\"legend-desc\">Drink a Kombucha to heal</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">&lt; / &gt;</span><span class=\"legend-desc\">Use stairs up / down</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">v</span><span class=\"legend-desc\">Toggle message history</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">?</span><span class=\"legend-desc\">Toggle this legend</span></div>
        <div class=\"legend-row\"><span class=\"legend-key\">Escape</span><span class=\"legend-desc\">Cancel targeting / close inventory / close this legend</span></div>
      </div>
    </div>
  </div>
  <script src=\"/js/rdescent.js\"></script>
</body>
</html>"
          (html-escape (or jwt-or-nil ""))))

(defun render-heresy-page (title body-html)
  "Wrap BODY-HTML (trusted markup, an essay's body fragment already read
from disk) in the site's article chrome for a full, paywalled essay
page."
  (format nil "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>~A - JRM Code Project Heresies</title>
  <link rel=\"stylesheet\" href=\"/css/article.css\">
</head>
<body>
  <article>
    <header>
      <h1>~A</h1>
      <p class=\"byline\">Heresies &mdash; JRM Code Project</p>
    </header>
    ~A
    <footer>
      <a href=\"/heresies/index.html\">&larr; All Heresies</a>
      <a href=\"/dashboard\">Dashboard</a>
    </footer>
  </article>
</body>
</html>"
          (html-escape title) (html-escape title) body-html))

(defun render-heresy-teaser-page (next-url title teaser-body-html)
  "Wrap TEASER-BODY-HTML (the first paragraph(s) of the essay named by
TITLE) in the site's article chrome for a public teaser page, ending in
a call-to-action linking to NEXT-URL (already a fully-formed,
URL-encoded \"/?next=...\" redirect back to the full, protected essay --
built by the caller, since it depends on the essay's slug, which this
combinator doesn't otherwise need)."
  (format nil "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>~A (Preview) - JRM Code Project Heresies</title>
  <link rel=\"stylesheet\" href=\"/css/article.css\">
</head>
<body>
  <article>
    <header>
      <h1>~A</h1>
      <p class=\"byline\">Heresies &mdash; JRM Code Project (Preview)</p>
    </header>
    ~A
    <div class=\"paywall-banner\">
      <h3>Read the Full Heresy</h3>
      <p><em>This is a preview. Sign in &mdash; it's free &mdash; to keep reading.</em></p>
      <p><a href=\"~A\" class=\"button\">Sign In to Read the Full Essay &rarr;</a></p>
    </div>
    <footer>
      <a href=\"/heresies-teasers/index.html\">&larr; All Heresies</a>
    </footer>
  </article>
</body>
</html>"
          (html-escape title)
          (html-escape title)
          teaser-body-html
          next-url))

(defun render-heresies-index (index-items-html)
  "The paywalled /heresies/index.html page: lists every essay (already
rendered as <li> markup by INDEX-ITEMS-HTML, see HERESY-INDEX-ITEMS-HTML),
linking to its full, protected page."
  (format nil "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>JRM Code Project - Heresies</title>
  <link rel=\"stylesheet\" href=\"/css/article.css\">
</head>
<body>
  <article>
    <header>
      <h1>Heresies</h1>
      <p class=\"byline\">Short, opinionated essays on Lisp and software design.</p>
    </header>
    <ul>
      ~A
    </ul>
    <footer>
      <a href=\"/dashboard\">&larr; Dashboard</a>
    </footer>
  </article>
</body>
</html>"
          index-items-html))

(defun render-heresies-teasers-index (index-items-html)
  "The public /heresies-teasers/index.html page: lists every essay
(already rendered as <li> markup by INDEX-ITEMS-HTML, see
HERESY-INDEX-ITEMS-HTML), linking to its public teaser page, with a
further link on to the (paywalled) full index."
  (format nil "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>JRM Code Project - Heresies (Previews)</title>
  <link rel=\"stylesheet\" href=\"/css/article.css\">
</head>
<body>
  <article>
    <header>
      <h1>Heresies (Preview)</h1>
      <p class=\"byline\">Short, opinionated essays on Lisp and software design.</p>
    </header>
    <ul>
      ~A
    </ul>
    <footer>
      <a href=\"/heresies/index.html\">Read the full heresies &rarr; (membership required)</a>
    </footer>
  </article>
</body>
</html>"
          index-items-html))





