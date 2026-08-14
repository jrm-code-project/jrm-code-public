;;; -*- Lisp -*-

;;; Gates the whole resources/www/html/interviews/ directory behind a
;;; membership-tier paywall. Unlike chef.lisp's single-page
;;; DEFINE-EASY-HANDLER trick, this directory can grow to hold many
;;; interview files, so instead of one handler per file we register a
;;; folder dispatcher (HUNCHENTOOT:CREATE-FOLDER-DISPATCHER-AND-HANDLER)
;;; whose CALLBACK enforces REQUIRE-MEMBERSHIP-TIER before any file
;;; under the directory is served. Pushing it onto *DISPATCH-TABLE*
;;; gives it priority over the acceptor's plain static-file fallback,
;;; exactly like the easy-handler-at-the-same-URI trick used elsewhere.

(in-package "JRM-CODE-PROJECT")

(defparameter *interviews-minimum-tier* "CONS"
  "Minimum membership tier required to view any file under
resources/www/html/interviews/ -- currently the free (CONS) tier, i.e.
any signed-in member.")

(defun require-interviews-access (pathname content-type)
  "HANDLE-STATIC-FILE callback for the /interviews/ folder dispatcher.
Ignores PATHNAME/CONTENT-TYPE and simply enforces
*INTERVIEWS-MINIMUM-TIER*; REQUIRE-MEMBERSHIP-TIER redirects (to login
or the upgrade-required page) and aborts the request via
HUNCHENTOOT:REDIRECT when access is not granted."
  (declare (ignore pathname content-type))
  (require-membership-tier *interviews-minimum-tier*))

(defvar *interviews-dispatcher* nil
  "The currently-registered /interviews/ folder dispatcher, if any, so
that REGISTER-INTERVIEWS-DISPATCHER can remove a stale entry before
installing a fresh one (e.g. across repeated START-SERVER calls in a
running REPL) without leaking duplicate entries onto *DISPATCH-TABLE*.")

(defun register-interviews-dispatcher ()
  "(Re-)install the /interviews/ folder dispatcher at the front of
HUNCHENTOOT:*DISPATCH-TABLE*, so paywall-protected interview pages are
never served as plain static files."
  (setf hunchentoot:*dispatch-table*
        (remove *interviews-dispatcher* hunchentoot:*dispatch-table*))
  (setf *interviews-dispatcher*
        (hunchentoot:create-folder-dispatcher-and-handler
         "/interviews/"
         (asdf:system-relative-pathname :jrm-code-project "resources/www/html/interviews/")
         nil
         #'require-interviews-access))
  (push *interviews-dispatcher* hunchentoot:*dispatch-table*))
