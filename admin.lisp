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

(defmacro with-admin-members-action (&body body)
  "Wrap the body of an /admin/members/* POST action handler with the
three concerns every one of them repeated identically: (1) require the
current session to belong to a wheel user (via REQUIRE-SESSION-WHEEL),
aborting (via its own redirect) if not; (2) require a valid CSRF token
(via WITH-CSRF-PROTECTION), aborting with a 403 if not; and (3), once
BODY has run, redirect back to /admin/members. Replaces four separate,
near-identical copies of this
require-session-wheel/with-csrf-protection/redirect shape (previously in
ADMIN-TOGGLE-WHEEL-ACTION, ADMIN-SET-TIER-ACTION,
ADMIN-REFUND-AND-DELETE-ACTION, and ADMIN-CREATE-MEMBER-ACTION) with one
audited, shared macro -- so a future admin action only needs to supply
its own validation/mutation logic, not re-derive the wrapping shape (and
risk forgetting the CSRF check or the wheel-authorization check, as any
hand-copied boilerplate risks over time)."
  `(when (require-session-wheel)
     (with-csrf-protection
         (progn ,@body)
       (hunchentoot:redirect "/admin/members"))))

(defun render-member-row (member)
  "Thin adapter: pulls the plain data RENDER-ADMIN-MEMBER-ROW (views.lisp)
needs out of a JRM-AUTH:USER struct MEMBER. The actual HTML construction
(and HTML-ESCAPE routing) lives in views.lisp alongside the rest of the
pure rendering combinators -- see TECHNICAL_DEBT.md item 5."
  (render-admin-member-row
   (jrm-auth:user-username member)
   (or (jrm-auth:user-membership-tier member) "CONS")
   (jrm-auth:user-wheel-p member)
   (not (null (jrm-auth:user-stripe-subscription-id member)))
   (or (jrm-auth:user-subscription-status member) "inactive")))

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
             (members (jrm-auth:list-users :search search :limit *admin-page-size*
                                            :offset (admin-pagination-offset pagination))))
        (render-admin-members-page
         :search search
         :member-rows-html (mapcar #'render-member-row members)
         :page-num (admin-pagination-page-num pagination)
         :total-pages (admin-pagination-total-pages pagination)
         :total total
         :has-prev (admin-pagination-has-prev pagination)
         :has-next (admin-pagination-has-next pagination))))))

(hunchentoot:define-easy-handler (admin-toggle-wheel-action :uri "/admin/members/toggle-wheel") (username)
  (with-admin-members-action
    (when (and username (plusp (length username)))
      (jrm-auth:set-user-wheel username (not (jrm-auth:wheel-p username))))))

(hunchentoot:define-easy-handler (admin-set-tier-action :uri "/admin/members/set-tier") (username tier)
  (with-admin-members-action
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
            (jrm-auth:admin-set-tier username tier))))))

(hunchentoot:define-easy-handler (admin-refund-and-delete-action :uri "/admin/members/refund-and-delete") (username delete-too)
  (with-admin-members-action
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
            (jrm-auth:cancel-user-subscription username))))))

(hunchentoot:define-easy-handler (admin-create-member-action :uri "/admin/members/create") (username password tier)
  (with-admin-members-action
    (when (and username (plusp (length username)) password (plusp (length password)))
      (jrm-auth:admin-create-user username password (if (member tier '("CONS" "CADR" "LAMBDA") :test #'string-equal) tier "CONS")))))
