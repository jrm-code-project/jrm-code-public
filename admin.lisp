;;; -*- Lisp -*-

;;; The wheelhouse: wheel-only membership administration (search/paginate
;;; members, toggle wheel bit, set tier, refund+delete, comp new members).
;;; Isolated so god-mode controls stay far away from standard user routing.

(in-package "JRM-CODE-PROJECT")

;; --- WHEEL-ONLY MEMBERSHIP ADMINISTRATION ---

(defparameter *admin-page-size* 20
  "Number of members shown per page on the wheel-only membership admin page.")

(defun require-session-wheel ()
  "Ensure the current session's authenticated user has the wheel bit set in
the database (the live, authoritative value -- not the cached JWT claim,
since this page performs sensitive membership/billing mutations). Returns
the wheel's username on success; otherwise redirects (to login if not
authenticated at all, or to the dashboard if authenticated but not a
wheel) and returns NIL."
  (let ((user (hunchentoot:session-value :authenticated-user)))
    (if (null user)
        (require-guard (lambda () nil) (lambda () (hunchentoot:redirect "/")))
        (require-guard
         (lambda () (and (jrm-auth:wheel-p user) user))
         (lambda () (hunchentoot:redirect "/dashboard"))))))

(defun admin-escape (value)
  (html-escape value))

(defun render-member-row (member)
  "Pure (USER -> HTML) rendering function: a table row summarizing MEMBER's
tier, subscription status, and wheel bit, plus the three admin action
forms (set-tier/toggle-wheel/refund-and-delete) for that member. Each form
is built with HTML-FORM so its CSRF token is carried structurally."
  (let* ((username (jrm-auth:user-username member))
         (tier (or (jrm-auth:user-membership-tier member) "CONS"))
         (is-wheel (jrm-auth:user-wheel-p member))
         (has-subscription (not (null (jrm-auth:user-stripe-subscription-id member))))
         (subscription-status (or (jrm-auth:user-subscription-status member) "inactive"))
         (set-tier-form
          (html-form "/admin/members/set-tier"
                     (format nil "<input type='hidden' name='username' value='~A'>
                                  <select name='tier'>
                                    <option value='CONS' ~A>CONS</option>
                                    <option value='CADR' ~A>CADR</option>
                                    <option value='LAMBDA' ~A>LAMBDA</option>
                                  </select>
                                  <button type='submit' class='btn-sm'>Set</button>"
                             (admin-escape username)
                             (if (string-equal tier "CONS") "selected" "")
                             (if (string-equal tier "CADR") "selected" "")
                             (if (string-equal tier "LAMBDA") "selected" ""))
                     :style "display:inline-flex;gap:0.4rem;"
                     :csrf-token (csrf-input-html)))
         (toggle-wheel-form
          (html-form "/admin/members/toggle-wheel"
                     (format nil "<input type='hidden' name='username' value='~A'>
                                  <button type='submit' class='btn-sm'>~A</button>"
                             (admin-escape username)
                             (if is-wheel "Revoke Wheel" "Make Wheel"))
                     :csrf-token (csrf-input-html)))
         (refund-and-delete-form
          (html-form "/admin/members/refund-and-delete"
                     (format nil "<input type='hidden' name='username' value='~A'>
                                  <label><input type='checkbox' name='delete-too' value='yes'> also delete</label>
                                  <button type='submit' class='btn-sm btn-sm-danger'>Refund~A</button>"
                             (admin-escape username)
                             (if has-subscription "/Delete" ""))
                     :csrf-token (csrf-input-html)
                     :onsubmit (format nil "return confirm('This will cancel/refund any active subscription and permanently delete ~A. Are you sure?');"
                                        (admin-escape username)))))
    (format nil "<tr>
                   <td>~A</td>
                   <td>~A</td>
                   <td>~A~A</td>
                   <td>~A</td>
                   <td>~A</td>
                 </tr>"
            (admin-escape username)
            set-tier-form
            (admin-escape subscription-status)
            (if has-subscription " (paid)" "")
            toggle-wheel-form
            refund-and-delete-form)))

(defstruct (admin-pagination (:copier nil))
  "Immutable, pure snapshot of the pagination math for one /admin/members
request: derived solely from the requested PAGE-NUM, PAGE-SIZE, and TOTAL
member count. See FUNCTIONAL_REFACTOR.md Phase 7 -- SERIES doesn't help
here since this is scalar arithmetic, not a sequence-shaped
transformation, so it's kept as a small pure function instead."
  page-num offset total-pages has-prev has-next)

(defun compute-admin-pagination (page-num-param page-size total)
  "Given the raw `page' query parameter string PAGE-NUM-PARAM, PAGE-SIZE, and
the TOTAL member count, compute the pure ADMIN-PAGINATION describing the
current page's offset and prev/next state. Performs no I/O."
  (let* ((page-num (max 0 (or (ignore-errors (parse-integer page-num-param)) 0)))
         (total-pages (max 1 (ceiling total page-size))))
    (make-admin-pagination
     :page-num page-num
     :offset (* page-num page-size)
     :total-pages total-pages
     :has-prev (> page-num 0)
     :has-next (< (1+ page-num) total-pages))))

(hunchentoot:define-easy-handler (admin-members-page :uri "/admin/members") (q page)
  (let ((wheel-username (require-session-wheel)))
    (when wheel-username
      (let* ((search (or q ""))
             (total (jrm-auth:count-users :search search))
             (pagination (compute-admin-pagination page *admin-page-size* total))
             (page-num (admin-pagination-page-num pagination))
             (offset (admin-pagination-offset pagination))
             (members (jrm-auth:list-users :search search :limit *admin-page-size* :offset offset))
             (total-pages (admin-pagination-total-pages pagination))
             (has-prev (admin-pagination-has-prev pagination))
             (has-next (admin-pagination-has-next pagination)))
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
                 (admin-escape search)
                 (mapcar #'render-member-row members)
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
                       .add-member { margin-top: 2rem; border: 1px solid #333; border-radius: 8px; padding: 1rem; }")))))

(hunchentoot:define-easy-handler (admin-toggle-wheel-action :uri "/admin/members/toggle-wheel") (username)
  (let ((wheel-username (require-session-wheel)))
    (when wheel-username
      (with-csrf-protection
          (when (and username (plusp (length username)))
            (jrm-auth:set-user-wheel username (not (jrm-auth:wheel-p username))))
        (hunchentoot:redirect "/admin/members")))))

(hunchentoot:define-easy-handler (admin-set-tier-action :uri "/admin/members/set-tier") (username tier)
  (let ((wheel-username (require-session-wheel)))
    (when wheel-username
      (with-csrf-protection
          ;; Directly comp/adjust the tier -- no payment required. This does not
          ;; touch any existing Stripe subscription; if the member has a paid
          ;; subscription and the wheel is downgrading them, cancel-and-refund
          ;; via /admin/members/refund-and-delete (uncheck "also delete") first
          ;; so the subscription and database stay consistent.
          (when (and username (plusp (length username)) (member tier '("CONS" "CADR" "LAMBDA") :test #'string-equal))
            (let* ((user-data (car (jrm-auth:get-user username)))
                   (subscription-id (and user-data (jrm-auth:user-stripe-subscription-id user-data)))
                   (current-tier (and user-data (jrm-auth:user-membership-tier user-data))))
              (if (and subscription-id (not (string-equal tier current-tier)))
                  ;; Refuse to silently desync a paid subscription from the
                  ;; membership tier -- cancel/refund it first so Stripe and the
                  ;; database agree, then apply the wheel's requested tier.
                  (progn
                    (cancel-stripe-subscription-with-prorated-refund subscription-id)
                    (jrm-auth:admin-set-tier username tier))
                  (jrm-auth:admin-set-tier username tier))))
        (hunchentoot:redirect "/admin/members")))))

(hunchentoot:define-easy-handler (admin-refund-and-delete-action :uri "/admin/members/refund-and-delete") (username delete-too)
  (let ((wheel-username (require-session-wheel)))
    (when wheel-username
      (with-csrf-protection
          (when (and username (plusp (length username)))
            (let* ((user-data (car (jrm-auth:get-user username)))
                   (subscription-id (and user-data (jrm-auth:user-stripe-subscription-id user-data))))
              ;; Ensure any active paid subscription is cancelled and pro-rated
              ;; refunded before touching membership/account state, so a member
              ;; is never left with a live Stripe subscription that the local
              ;; database no longer reflects.
              (when subscription-id
                (cancel-stripe-subscription-with-prorated-refund subscription-id))
              (if (and delete-too (string-equal delete-too "yes"))
                  (jrm-auth:delete-user username)
                  (jrm-auth:cancel-user-subscription username))))
        (hunchentoot:redirect "/admin/members")))))

(hunchentoot:define-easy-handler (admin-create-member-action :uri "/admin/members/create") (username password tier)
  (let ((wheel-username (require-session-wheel)))
    (when wheel-username
      (with-csrf-protection
          (when (and username (plusp (length username)) password (plusp (length password)))
            (jrm-auth:admin-create-user username password (if (member tier '("CONS" "CADR" "LAMBDA") :test #'string-equal) tier "CONS")))
        (hunchentoot:redirect "/admin/members")))))
