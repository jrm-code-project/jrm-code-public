;;; -*- Lisp -*-

;;; The heresies: short essays whose bodies live as bare HTML fragments
;;; (just <p>/<h2>/<pre> markup, no <head>/<body> wrapper) under
;;; resources/www/html/heresies/. Unlike the interviews (whose files are
;;; already complete, styled pages served as-is), each heresy fragment
;;; is wrapped at request time in the site's article chrome, so there is
;;; a single canonical source file per essay rather than separate
;;; full/teaser copies to keep in sync.
;;;
;;; Two paywall tiers of access:
;;;   /heresies/<slug>.html          -- full essay, gated at the free
;;;                                     (CONS) membership tier.
;;;   /heresies-teasers/<slug>.html  -- public preview built from the
;;;                                     first couple of paragraphs of
;;;                                     the same fragment, with a call
;;;                                     to action redirecting to "/"
;;;                                     carrying a `next` breadcrumb
;;;                                     that leads on to the full essay
;;;                                     once the reader signs in.
;;; Both directories also expose a dynamically-rendered index.html.

(in-package "JRM-CODE-PROJECT")

(defparameter *heresies-directory*
  (asdf:system-relative-pathname :jrm-code-project "resources/www/html/heresies/")
  "The directory of essay fragment files. Both index pages and both
per-essay handlers are driven by whatever .html fragments actually
exist here (see LIST-HERESY-SLUGS) rather than a hand-maintained list,
so deleting a fragment file immediately un-publishes it and removes it
from both indexes -- no separate list to remember to update, and no
stale/broken links left behind.")

(defparameter *heresies-titles*
  '(("01-tictactoe-complete" . "Tic-Tac-Toe, Complete")
    ("02-obvious-representation" . "The Obvious Representation")
    ("03-coding-vs-engineering" . "Coding vs. Engineering")
    ("04-mutable-data" . "Mutable Data")
    ("05-immutable-state" . "Immutable State"))
  "Curated (SLUG . TITLE) overrides, used when a nicer title than the
auto-humanized fallback (see HUMANIZE-HERESY-SLUG) is wanted. This
alist is purely cosmetic -- it does NOT determine which essays are
served; that's decided solely by which fragment files exist under
*HERESIES-DIRECTORY* (see LIST-HERESY-SLUGS). An entry here for a
since-deleted file is simply ignored.")

(defparameter *heresies-minimum-tier* "CONS"
  "Minimum membership tier required to read a full essay under
/heresies/ -- currently the free (CONS) tier, i.e. any signed-in
member.")

(defun humanize-heresy-slug (slug)
  "Fallback title for a SLUG with no entry in *HERESIES-TITLES*: strip a
leading NN- numeric prefix (if any), replace hyphens with spaces, and
capitalize each word."
  (let* ((dash (position #\- slug))
         (stripped (if (and dash (every #'digit-char-p (subseq slug 0 dash)))
                       (subseq slug (1+ dash))
                       slug)))
    (format nil "~{~A~^ ~}"
            (mapcar #'string-capitalize (cl-ppcre:split "-" stripped)))))

(defun heresy-title (slug)
  "Return the display title for SLUG: a curated title from
*HERESIES-TITLES* if one exists, otherwise a humanized fallback derived
from SLUG itself."
  (or (cdr (assoc slug *heresies-titles* :test #'string=))
      (humanize-heresy-slug slug)))

(defun valid-heresy-slug-p (slug)
  "T if SLUG is a safe, well-formed heresy identifier (one or more
letters/digits/hyphens, nothing else). Guards HERESY-FRAGMENT-PATHNAME
against path traversal before SLUG is ever used to build a filesystem
path."
  (and (plusp (length slug))
       (cl-ppcre:scan "^[a-zA-Z0-9-]+$" slug)))

(defun heresy-fragment-pathname (slug)
  (merge-pathnames (format nil "~A.html" slug) *heresies-directory*))

(defun heresy-exists-p (slug)
  "T if SLUG is well-formed and names a fragment file that actually
exists under *HERESIES-DIRECTORY*."
  (and (valid-heresy-slug-p slug)
       (probe-file (heresy-fragment-pathname slug))
       t))

(defun list-heresy-slugs ()
  "Return the slugs of every essay fragment file actually present under
*HERESIES-DIRECTORY*, sorted alphabetically (the NN- numeric filename
prefixes keep them in the intended reading order). Scanning the
directory -- rather than consulting a hand-maintained list -- means
deleting a fragment file takes effect immediately, with no broken
links left in either index."
  (sort (mapcar #'pathname-name
                (directory (merge-pathnames "*.html" *heresies-directory*)))
        #'string<))

(defun read-heresy-fragment (slug)
  "Read the raw body-fragment HTML for SLUG from disk. Callers must have
already validated SLUG (see HERESY-EXISTS-P) before calling this, so
that no arbitrary path is ever read."
  (uiop:read-file-string (heresy-fragment-pathname slug)))

(defun extract-teaser-paragraphs (fragment-html &optional (n 2))
  "Return the first N <p>...</p> blocks found in FRAGMENT-HTML (in source
order), joined back into a single HTML string. Used to build a teaser
preview straight from the full essay fragment, so there is no separate
teaser file to keep in sync by hand."
  (let ((paragraphs (cl-ppcre:all-matches-as-strings "(?s)<p>.*?</p>" fragment-html)))
    (format nil "~{~A~^~%~}" (subseq paragraphs 0 (min n (length paragraphs))))))

(defun heresies-slug-from-path (path prefix)
  "Strip PREFIX and a trailing \".html\" (if present) from PATH, returning
the bare slug/index name."
  (let* ((tail (subseq path (length prefix)))
         (dot (search ".html" tail :from-end t)))
    (if dot (subseq tail 0 dot) tail)))

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

(defun render-heresy-teaser-page (slug title teaser-body-html)
  "Wrap TEASER-BODY-HTML (the first paragraph(s) of the essay named by
SLUG/TITLE) in the site's article chrome for a public teaser page,
ending in a call-to-action that redirects to \"/\" carrying a `next`
breadcrumb pointing at the full, protected essay."
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
          (format nil "/?next=~A"
                  (hunchentoot:url-encode (format nil "/heresies/~A.html" slug)))))

(defun heresy-index-items-html (uri-prefix)
  (format nil "~{~A~%~}"
          (mapcar (lambda (slug)
                    (format nil "<li><a href=\"~A~A.html\">~A</a></li>"
                            uri-prefix slug (html-escape (heresy-title slug))))
                  (list-heresy-slugs))))

(defun render-heresies-index ()
  "The paywalled /heresies/index.html page: lists every essay, linking to
its full, protected page."
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
          (heresy-index-items-html "/heresies/")))

(defun render-heresies-teasers-index ()
  "The public /heresies-teasers/index.html page: lists every essay,
linking to its public teaser page, with a further link on to the
(paywalled) full index."
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
          (heresy-index-items-html "/heresies-teasers/")))

(defun heresies-full-handler ()
  "Hunchentoot handler (a zero-argument thunk, per CREATE-PREFIX-DISPATCHER)
for every request under /heresies/. Dispatches to the paywalled index
or a single full essay page, enforcing *HERESIES-MINIMUM-TIER* in
either case."
  (let ((slug (heresies-slug-from-path (hunchentoot:script-name*) "/heresies/")))
    (cond
      ((string= slug "index")
       (when (require-membership-tier *heresies-minimum-tier*)
         (setf (hunchentoot:content-type*) "text/html")
         (render-heresies-index)))
      ((heresy-exists-p slug)
       (when (require-membership-tier *heresies-minimum-tier*)
         (setf (hunchentoot:content-type*) "text/html")
         (render-heresy-page (heresy-title slug) (read-heresy-fragment slug))))
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
       "Not Found"))))

(defun heresies-teaser-handler ()
  "Hunchentoot handler (a zero-argument thunk) for every request under
/heresies-teasers/. Publicly accessible: no membership check. Dispatches
to the public index or a single teaser page."
  (let ((slug (heresies-slug-from-path (hunchentoot:script-name*) "/heresies-teasers/")))
    (cond
      ((string= slug "index")
       (setf (hunchentoot:content-type*) "text/html")
       (render-heresies-teasers-index))
      ((heresy-exists-p slug)
       (setf (hunchentoot:content-type*) "text/html")
       (render-heresy-teaser-page slug (heresy-title slug)
                                  (extract-teaser-paragraphs (read-heresy-fragment slug))))
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
       "Not Found"))))

(defvar *heresies-dispatcher* nil
  "The currently-registered /heresies/ prefix dispatcher, if any -- see
*INTERVIEWS-DISPATCHER* for why this is tracked (avoids leaking
duplicate dispatch-table entries across repeated START-SERVER calls).")

(defvar *heresies-teasers-dispatcher* nil
  "The currently-registered /heresies-teasers/ prefix dispatcher, if any.")

(defun register-heresies-dispatchers ()
  "(Re-)install the /heresies/ and /heresies-teasers/ prefix dispatchers at
the front of HUNCHENTOOT:*DISPATCH-TABLE*."
  (setf hunchentoot:*dispatch-table*
        (remove *heresies-teasers-dispatcher*
                (remove *heresies-dispatcher* hunchentoot:*dispatch-table*)))
  (setf *heresies-teasers-dispatcher*
        (hunchentoot:create-prefix-dispatcher "/heresies-teasers/" 'heresies-teaser-handler))
  (setf *heresies-dispatcher*
        (hunchentoot:create-prefix-dispatcher "/heresies/" 'heresies-full-handler))
  (push *heresies-teasers-dispatcher* hunchentoot:*dispatch-table*)
  (push *heresies-dispatcher* hunchentoot:*dispatch-table*))
