;;; -*- Lisp -*-

;;; A minimal explicit-outcome-value pair, used to let pure/impure code
;;; report success or failure as a plain data value instead of via
;;; conditions or a lone NIL-means-failure convention. See
;;; FUNCTIONAL_REFACTOR.md Phase 6.
;;;
;;; Only the imperative shell (a Hunchentoot handler, or a thin
;;; interpreter like EXECUTE-WEBHOOK-DB-COMMAND) should ever pattern-match
;;; on a RESULT/FAILURE to decide what HTTP-visible side effect to
;;; perform; the functions that *produce* them (ROAST-CODE-WITH-GEMINI,
;;; CREATE-STRIPE-CHECKOUT-SESSION, etc.) remain otherwise ordinary
;;; functions -- the only thing that changes is that their "did this
;;; work?" signal is now a first-class, inspectable value instead of a
;;; raised condition or a bare NIL.
;;;
;;; Deliberately dependency-free (no Hunchentoot, no database, no Stripe)
;;; so it can be loaded early and unit tested directly.

(in-package "JRM-CODE-PROJECT")

(defstruct (result (:constructor ok (value)))
  "A successful outcome, wrapping the VALUE produced."
  value)

(defstruct (failure (:constructor err (reason)))
  "A failed outcome, wrapping a human-readable REASON (a string)."
  reason)

(defun outcome-p (x)
  "Return T if X is either a RESULT or a FAILURE outcome value."
  (or (result-p x) (failure-p x)))

(defun outcome-ok-p (x)
  "Return T if X is a successful RESULT outcome."
  (result-p x))

(defun outcome-value (outcome &optional default)
  "If OUTCOME is a RESULT, return its wrapped value; if it is a FAILURE (or
anything else), return DEFAULT."
  (if (result-p outcome) (result-value outcome) default))

(defun outcome-reason (outcome &optional (default "Unknown error"))
  "If OUTCOME is a FAILURE, return its wrapped reason; otherwise return
DEFAULT."
  (if (failure-p outcome) (failure-reason outcome) default))
