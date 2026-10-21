;;; grid-table-calc-symbolic.el --- Symbolic math through Emacs Calc -*- lexical-binding: t -*-

;;; Commentary:

;; CALC hands its text to Emacs Calc's algebraic entry and gives back
;; what Calc prints: =CALC("deriv(x^2, x)") is "2 x".  Calc's language
;; reaches only Calc's own math functions, those named calcFunc-..., so
;; it needs no leave to run Lisp, as =elisp: does.  A function the user
;; defines with defmath joins them.
;;
;; Calc runs in radians, at 15 digits, with its working messages off and
;; its plain language, whatever the user's Calc is set to.  The answer
;; is a number when Calc prints one a double holds exactly, and text
;; otherwise.  It is not part of Excel, which shows #NAME? for it.

;;; Code:

(require 'grid-table-calc)

(declare-function calc-eval "calc" (str &optional separator &rest args))

(defconst grid-calc--calc-modes
  '(calc-angle-mode rad calc-internal-prec 15 calc-language nil
    calc-display-working-message nil calc-symbolic-mode nil)
  "The Calc modes CALC computes under, as `calc-eval' takes them.")

(defvar grid-calc--calc-answers (make-hash-table :test 'equal)
  "What CALC gave for each text it was handed, so far.")

(defconst grid-calc--calc-answers-limit 500
  "How many answers `grid-calc--calc-answers' keeps before it starts over.")

(defun grid-calc--calc-value (printed)
  "The value of what Calc PRINTED: a number when it is one, else text."
  (let ((number (and (string-match-p
                      "\\`-?[0-9]+\\.?[0-9]*\\(?:e-?[0-9]+\\)?\\'" printed)
                     (string-to-number printed))))
    (cond ((null number) printed)
          ((integerp number)
           (if (<= (abs number) (expt 2 53)) number printed))
          ((or (isnan number) (= (abs number) 1.0e+INF)) printed)
          (t number))))

(defun grid-calc--calc (text)
  "What Emacs Calc makes of TEXT, as CALC gives it."
  (let ((answer (gethash text grid-calc--calc-answers 'none)))
    (when (eq answer 'none)
      (setq answer
            (condition-case nil
                ;; Calc says what rules it is compiling as it goes.
                (let* ((inhibit-message t) (message-log-max nil)
                       (printed (calc-eval (cons text grid-calc--calc-modes))))
                  (if (stringp printed)
                      (grid-calc--calc-value printed)
                    ;; (POSITION MESSAGE): TEXT does not parse.
                    (grid-calc-err "#VALUE!")))
              (error (grid-calc-err "#NUM!"))))
      (when (>= (hash-table-count grid-calc--calc-answers)
                grid-calc--calc-answers-limit)
        (clrhash grid-calc--calc-answers))
      (puthash text answer grid-calc--calc-answers))
    answer))

(grid-calc-define calc ((expression text))
  "What Emacs Calc makes of EXPRESSION, written in Calc's algebraic
notation: deriv(x^2, x) is \"2 x\", integ(sin(x), x) is \"-cos(x)\",
roots(x^2 - 5 x + 6, x) is \"[3., 2.]\" and 2^0.5 is 1.4142135623731.
A number comes back as a number, anything else as text.  EXPRESSION that
Calc cannot read is #VALUE!, one it fails on #NUM!."
  (grid-calc--calc expression))

(provide 'grid-table-calc-symbolic)

;;; grid-table-calc-symbolic.el ends here
