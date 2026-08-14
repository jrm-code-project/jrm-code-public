;;; -*- Lisp -*-

;;; Dynamically-generated /sitemap.xml. Static top-level pages are
;;; hand-listed, but the interview-teasers and heresies-teasers
;;; sections are built by scanning the actual files/fragments on disk
;;; (the same way the heresies index pages are -- see heresies.lisp),
;;; so deleting or adding a teaser page keeps the sitemap in sync
;;; automatically instead of leaving stale/missing <url> entries behind.

(in-package "JRM-CODE-PROJECT")

(defparameter *sitemap-hostname* "https://jrm-code-project.com"
  "Origin used to build absolute <loc> URLs in the generated sitemap.")

(defparameter *interview-teasers-directory*
  (asdf:system-relative-pathname :jrm-code-project "resources/www/html/interview-teasers/")
  "Directory of public interview-teaser pages, scanned to build the
interview-teasers section of the sitemap.")

(defparameter *sitemap-static-urls*
  '(("/" "weekly" "1.0")
    ("/signup" "monthly" "0.8"))
  "Hand-maintained (PATH CHANGEFREQ PRIORITY) triples for top-level pages
that aren't part of a scanned teaser directory.")

(defun list-directory-html-slugs (directory-pathname)
  "Return the slugs (bare filenames, sans \".html\") of every .html file
directly under DIRECTORY-PATHNAME except \"index\", sorted
alphabetically."
  (sort (remove "index"
                (mapcar #'pathname-name
                        (directory (merge-pathnames "*.html" directory-pathname)))
                :test #'string=)
        #'string<))

(defun sitemap-url-entry (loc-path changefreq priority)
  (format nil "    <url>
        <loc>~A~A</loc>
        <changefreq>~A</changefreq>
        <priority>~A</priority>
    </url>"
          *sitemap-hostname* loc-path changefreq priority))

(defun sitemap-teaser-section-entries (uri-prefix slugs)
  "Build sitemap <url> entries for a teaser section: the section's
index.html (weekly, 0.7) followed by one entry per slug in SLUGS
(monthly, 0.6), all under URI-PREFIX."
  (cons (sitemap-url-entry (format nil "~Aindex.html" uri-prefix) "weekly" "0.7")
        (mapcar (lambda (slug)
                  (sitemap-url-entry (format nil "~A~A.html" uri-prefix slug) "monthly" "0.6"))
                slugs)))

(defun render-sitemap ()
  "Render the full /sitemap.xml body: hand-listed static pages, then the
interview-teasers section (scanned from disk), then the
heresies-teasers section (scanned via LIST-HERESY-SLUGS, from
heresies.lisp)."
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
~{~A~%~}</urlset>
"
          (append
           (mapcar (lambda (entry) (apply #'sitemap-url-entry entry)) *sitemap-static-urls*)
           (sitemap-teaser-section-entries
            "/interview-teasers/" (list-directory-html-slugs *interview-teasers-directory*))
           (sitemap-teaser-section-entries
            "/heresies-teasers/" (list-heresy-slugs)))))

(hunchentoot:define-easy-handler (sitemap-page :uri "/sitemap.xml") ()
  "Serve /sitemap.xml dynamically. Defining this easy handler at the same
URI as the static file takes priority over the acceptor's plain
static-file fallback (same trick used by /chef.html), so the
hand-authored static/sitemap.xml on disk is never actually served."
  (setf (hunchentoot:content-type*) "application/xml")
  (render-sitemap))
