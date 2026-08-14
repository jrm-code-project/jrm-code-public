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
