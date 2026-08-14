;;; -*- Lisp -*-

;;; Public, direct-URL routes for pastes -- /p/<id>.txt and
;;; /p/<id>.html -- distinct from the JSON API in
;;; API-PASTES-ROUTES.LISP's GET /api/v1/pastes. These exist so a paste
;;; id can be shared as a plain, human-clickable link (e.g.
;;; https://jrm-code-project.com/p/AbCdEf123456.html) that renders a
;;; readable page in a browser, or fetched as raw text by tools/scripts
;;; via the .txt extension, without any JSON envelope.

(in-package "JRM-CODE-PROJECT")

(defparameter *paste-direct-url-regex*
  "^/p/([a-zA-Z0-9]{12,16})\\.(txt|html)$"
  "Regex matched by *PASTE-DIRECT-DISPATCHER* against the request's
script name. Group 1 is the paste id (12-16 alphanumeric characters,
matching GENERATE-PASTE-ID's 16-character Base62 output with some
slack); group 2 is the requested extension, either \"txt\" or \"html\".")

(defun escape-html (string)
  "Escape STRING for safe inclusion in HTML text content: &, <, >, and
both quote characters are replaced with their entity references. Used
to safely embed paste content (arbitrary, untrusted user text) inside
the <pre><code>...</code></pre> block rendered by
PASTE-DIRECT-HTML-RESPONSE."
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#39;" out))
               (t (write-char ch out))))))

(defun paste-direct-html-response (paste-id content)
  "Render CONTENT (the raw paste text for PASTE-ID) as a complete,
dark-themed HTML5 page: a minimal top nav (link back to / and a \"View
Raw\" link to this same paste's .txt URL), PrismJS (core + Common Lisp
component + Tomorrow Night theme, all via CDN) for syntax highlighting,
and the HTML-escaped paste content inside
<pre><code class=\"language-lisp\">...</code></pre>."
  (format nil
          "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>Paste ~A</title>
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism-tomorrow.min.css\">
<style>
  * { box-sizing: border-box; }
  html, body {
    margin: 0;
    padding: 0;
    background: #1d1f21;
    color: #c5c8c6;
    font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif;
  }
  nav {
    display: flex;
    align-items: center;
    gap: 1.5rem;
    padding: 0.75rem 1.25rem;
    background: #141414;
    border-bottom: 1px solid #2a2a2a;
  }
  nav a {
    color: #8ab4f8;
    text-decoration: none;
    font-size: 0.95rem;
  }
  nav a:hover { text-decoration: underline; }
  nav .paste-id {
    margin-left: auto;
    color: #6b6f76;
    font-family: monospace;
    font-size: 0.85rem;
  }
  main { padding: 1.5rem; }
  pre {
    margin: 0;
    border-radius: 6px;
    overflow-x: auto;
  }
  pre[class*=\"language-\"] {
    background: #1d1f21 !important;
  }
</style>
</head>
<body>
<nav>
  <a href=\"/dashboard\">&larr; Home</a>
  <a href=\"/p/~A.txt\">View Raw</a>
  <span class=\"paste-id\">~A</span>
</nav>
<main>
<pre><code class=\"language-lisp\">~A</code></pre>
</main>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-core.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-lisp.min.js\"></script>
</body>
</html>"
          paste-id paste-id paste-id (escape-html content)))

(defun serve-paste-direct ()
  "Hunchentoot handler (a zero-argument thunk, per CREATE-REGEX-DISPATCHER)
for GET /p/<paste-id>.<txt|html>. Looks up PASTE-ID via GET-PASTE and,
if found, responds with either the raw text (.txt) or a syntax-
highlighted HTML page (.html); returns a 404 if the paste doesn't exist
or has expired."
  (cl-ppcre:register-groups-bind (paste-id extension)
      (*paste-direct-url-regex* (hunchentoot:script-name*))
    (let ((content (jrm-auth:get-paste paste-id)))
      (if (null content)
          (progn
            (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
            (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
            "404 Paste Not Found")
          (cond
            ((string= extension "txt")
             (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
             content)
            ((string= extension "html")
             (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
             (paste-direct-html-response paste-id content)))))))

(defvar *paste-direct-dispatcher* nil
  "The currently-registered /p/<id>.<ext> regex dispatcher, if any -- see
*HERESIES-DISPATCHER* for why this is tracked (avoids leaking duplicate
dispatch-table entries across repeated START-SERVER calls).")

(defun register-paste-direct-dispatcher ()
  "(Re-)install the /p/<paste-id>.<txt|html> regex dispatcher at the
front of HUNCHENTOOT:*DISPATCH-TABLE*."
  (setf hunchentoot:*dispatch-table*
        (remove *paste-direct-dispatcher* hunchentoot:*dispatch-table*))
  (setf *paste-direct-dispatcher*
        (hunchentoot:create-regex-dispatcher *paste-direct-url-regex* 'serve-paste-direct))
  (push *paste-direct-dispatcher* hunchentoot:*dispatch-table*))
