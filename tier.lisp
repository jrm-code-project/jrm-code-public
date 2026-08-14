;;; -*- Lisp -*-

;;; Pure membership-tier comparison logic. Deliberately dependency-free (no
;;; Hunchentoot, no database, no Stripe) so it can be unit tested directly
;;; with hand-built fixtures. See FUNCTIONAL_REFACTOR.md, Phase 2.

(in-package "JRM-CODE-PROJECT")

(defparameter *tier-ranks* '(("CONS" . 0) ("CADR" . 1) ("LAMBDA" . 2))
  "Relative ordering of membership tiers, lowest to highest, used to decide
whether a tier meets a page's minimum required tier.")

(defun tier-rank (tier)
  "Return the relative rank of TIER (higher is more privileged), or 0 if TIER
is unrecognized (treated as the lowest/free tier)."
  (or (cdr (assoc tier *tier-ranks* :test #'string-equal)) 0))

(defun tier-meets-minimum-p (tier minimum-tier)
  "Return T if TIER's rank is at least MINIMUM-TIER's rank."
  (>= (tier-rank tier) (tier-rank minimum-tier)))
