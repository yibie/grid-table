;;; grid-table-calc-logic.el --- Logical and information functions -*- lexical-binding: t -*-

;;; Commentary:

;; IF and the functions that choose among their arguments, AND, OR and
;; the rest of the logical functions, and the functions that say what a
;; value is: ISBLANK, ISERROR, TYPE and the like.

;;; Code:

(require 'grid-table-calc)

;;;----------------------------------------------------------------------
;;; Choosing
;;;----------------------------------------------------------------------

(defun grid-calc--branch (ast)
  "Return the value of the lazy argument AST as a branch gives it.
A reference stays one, for a function around the choice to read whole;
an argument left out is FALSE, one left empty blank."
  (if ast (grid-calc-arg ast 'values) :false))

(grid-calc-define if ((condition lazy) (then lazy) &optional (else lazy))
  "THEN when CONDITION is true, else ELSE; FALSE without ELSE.
A CONDITION of several values chooses for each of them."
  (let ((test (grid-calc-arg condition 'any)))
    (if (not (grid-calc-array-p test))
        (grid-calc--branch (if (grid-calc-truth test) then else))
      (let ((yes (grid-calc--deref (grid-calc--branch then)))
            (no (grid-calc--deref (grid-calc--branch else))))
        (grid-calc--broadcast
         (lambda (test yes no)
           (grid-calc--catching
             (if (grid-calc-truth test) yes no)))
         (list test yes no))))))

(grid-calc-define ifs (&rest (pairs lazy))
  "The value after the first condition that is true; #N/A when none is."
  (when (or (null pairs) (cl-oddp (length pairs)))
    (grid-calc-raise "#VALUE!"))
  (catch 'found
    (while pairs
      (let ((test (pop pairs)) (value (pop pairs)))
        (when (grid-calc-truth (grid-calc-arg test 'any))
          (throw 'found (grid-calc-arg value 'values)))))
    (grid-calc-err "#N/A")))

(grid-calc-define iferror ((value any) (fallback lazy))
  "VALUE, or FALLBACK when VALUE is an error value."
  :errors t
  (if (grid-calc-err-p value) (grid-calc--first (grid-calc--branch fallback))
    value))

(grid-calc-define ifna ((value any) (fallback lazy))
  "VALUE, or FALLBACK when VALUE is #N/A."
  :errors t
  (if (eq value (grid-calc-err "#N/A"))
      (grid-calc--first (grid-calc--branch fallback))
    value))

(grid-calc-define switch ((value any) &rest (cases lazy))
  "The result after the first case equal to VALUE, or the last argument.
Cases and results alternate; an argument left over is the default, and
without one no match is #N/A."
  (when (< (length cases) 2) (grid-calc-raise "#VALUE!"))
  (catch 'found
    (while (cdr cases)
      (let ((case (grid-calc-arg (pop cases) 'any))
            (result (pop cases)))
        (when (grid-calc-equal-p value case)
          (throw 'found (grid-calc-arg result 'values)))))
    (if cases (grid-calc-arg (car cases) 'values) (grid-calc-err "#N/A"))))

(grid-calc-define choose ((index integer) &rest (choices lazy))
  "The INDEXth of CHOICES, counting from 1."
  (unless (<= 1 index (length choices)) (grid-calc-raise "#VALUE!"))
  (grid-calc--branch (nth (1- index) choices)))

(grid-calc-define let (&rest (parts lazy))
  "The last argument, computed with each name before it given the value
after it, as LET(x,2,y,x*3,x+y) is 8."
  (when (or (< (length parts) 3) (cl-evenp (length parts)))
    (grid-calc-raise "#VALUE!"))
  (let ((grid-calc--locals grid-calc--locals))
    (while (cdr parts)
      (let ((name (pop parts)) (value (pop parts)))
        (unless (eq (car-safe name) 'NAME) (grid-calc-raise "#NAME?"))
        (push (cons (upcase (cadr name)) (grid-calc-arg value 'values))
              grid-calc--locals)))
    (grid-calc-arg (car parts) 'values)))

;;;----------------------------------------------------------------------
;;; Logical functions
;;;----------------------------------------------------------------------

(defun grid-calc--truths (arguments)
  "Return the truth values among ARGUMENTS, `values' arguments, in order.
In a range or array only numbers and TRUE and FALSE count; text written
as an argument must be TRUE or FALSE.  None at all is #VALUE!."
  (let (truths)
    (dolist (argument arguments)
      (grid-calc-each-value
       argument
       (lambda (value direct)
         (cond
          ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
          ((numberp value) (push (/= value 0) truths))
          ((eq value t) (push t truths))
          ((eq value :false) (push nil truths))
          ((and direct (stringp value)) (push (grid-calc-truth value) truths))
          ((and direct (eq value :blank)) (push nil truths))))))
    (unless truths (grid-calc-raise "#VALUE!"))
    (nreverse truths)))

(grid-calc-define and (&rest (conditions values))
  "TRUE when every one of CONDITIONS is true."
  (grid-calc-bool (cl-every #'identity (grid-calc--truths conditions))))

(grid-calc-define or (&rest (conditions values))
  "TRUE when any one of CONDITIONS is true."
  (grid-calc-bool (cl-some #'identity (grid-calc--truths conditions))))

(grid-calc-define xor (&rest (conditions values))
  "TRUE when an odd number of CONDITIONS are true."
  (grid-calc-bool (cl-oddp (cl-count t (grid-calc--truths conditions)))))

(grid-calc-define not ((condition bool))
  "TRUE when CONDITION is false."
  (grid-calc-bool (not condition)))

(grid-calc-define true ()
  "TRUE."
  t)

(grid-calc-define false ()
  "FALSE."
  :false)

;;;----------------------------------------------------------------------
;;; What a value is
;;;----------------------------------------------------------------------

(defmacro grid-calc--define-is (name doc test)
  "Define NAME, TRUE when TEST, a function of the value, holds; DOC says when."
  `(grid-calc-define ,name ((value any))
     ,doc
     :errors t
     (grid-calc-bool (funcall ,test value))))

(grid-calc--define-is isblank "TRUE when VALUE is an empty cell."
                      #'grid-calc-blank-p)
(grid-calc--define-is isnumber "TRUE when VALUE is a number." #'numberp)
(grid-calc--define-is istext "TRUE when VALUE is text." #'stringp)
(grid-calc--define-is isnontext "TRUE when VALUE is not text."
                      (lambda (value) (not (stringp value))))
(grid-calc--define-is islogical "TRUE when VALUE is TRUE or FALSE."
                      #'grid-calc-boolean-p)
(grid-calc--define-is iserror "TRUE when VALUE is an error value."
                      #'grid-calc-err-p)
(grid-calc--define-is iserr "TRUE when VALUE is an error value but #N/A."
                      (lambda (value)
                        (and (grid-calc-err-p value)
                             (not (eq value (grid-calc-err "#N/A"))))))
(grid-calc--define-is isna "TRUE when VALUE is #N/A."
                      (lambda (value) (eq value (grid-calc-err "#N/A"))))

(grid-calc-define iseven ((number number))
  "TRUE when NUMBER, its fraction cut off, is even."
  (grid-calc-bool (cl-evenp (truncate number))))

(grid-calc-define isodd ((number number))
  "TRUE when NUMBER, its fraction cut off, is odd."
  (grid-calc-bool (cl-oddp (truncate number))))

(grid-calc-define isref ((value values))
  "TRUE when VALUE is a reference."
  :errors t
  (grid-calc-bool (or (grid-calc-ref-p value) (eq (car-safe value) 'union))))

(defun grid-calc--raw-at (ref)
  "Return the raw input of the cell in the corner of REF, or nil."
  (let ((model (grid-calc-ref-model ref))
        (row (grid-calc-ref-top ref)) (col (grid-calc-ref-left ref)))
    (and (< row (grid-data-model-rows model)) (< col (grid-data-model-cols model))
         (grid-cell-raw-input
          (aref (grid-data-model-cells model)
                (+ (* row (grid-data-model-cols model)) col))))))

(grid-calc-define isformula ((reference ref))
  "TRUE when the cell REFERENCE names holds a formula."
  (grid-calc-bool (grid-calc--formula-p (grid-calc--raw-at reference))))

(defun grid-calc-closing-paren (text open)
  "Return where the parenthesis at OPEN in TEXT closes, or nil.
Parentheses in strings and quoted sheet names don't count."
  (let ((depth 0) (pos open) (end (length text)) (found nil))
    (while (and (not found) (< pos end))
      (let ((char (aref text pos)))
        (cond ((memq char '(?\" ?'))
               ;; Up to the closing quote; a doubled one stays inside.
               (setq pos (1+ pos))
               (while (and (< pos end)
                           (or (/= (aref text pos) char)
                               (and (< (1+ pos) end) (= (aref text (1+ pos)) char)
                                    (setq pos (1+ pos)))))
                 (setq pos (1+ pos))))
              ((eq char ?\() (setq depth (1+ depth)))
              ((eq char ?\))
               (setq depth (1- depth))
               (when (= depth 0) (setq found pos)))))
      (setq pos (1+ pos)))
    found))

(defun grid-calc-excel-formula-text (formula)
  "Return FORMULA as Excel shows it: SINGLE(X) as @X, ANCHORARRAY(A1) as A1#.
Workbooks store the operators as these functions."
  (let ((case-fold-search t) (start 0))
    (while (string-match "\\(?:\\`\\|[^A-Za-z0-9_.]\\)\\(SINGLE\\|ANCHORARRAY\\)("
                         formula start)
      (let* ((from (match-beginning 1))
             (open (1- (match-end 0)))
             (anchor (string-equal (upcase (match-string 1 formula)) "ANCHORARRAY"))
             (close (grid-calc-closing-paren formula open)))
        (if (or (null close)
                ;; Not inside a string: an even number of quotes before.
                (cl-oddp (cl-count ?\" formula :end from)))
            (setq start (match-end 0))
          (let ((inner (grid-calc-excel-formula-text
                        (substring formula (1+ open) close))))
            (setq formula (concat (substring formula 0 from)
                                  (if anchor (concat inner "#") (concat "@" inner))
                                  (substring formula (1+ close)))
                  start from)))))
    formula))

(grid-calc-define formulatext ((reference ref))
  "The formula of the cell REFERENCE names, as text, as Excel shows it;
#N/A when there is none."
  (let ((raw (grid-calc--raw-at reference)))
    (if (grid-calc--formula-p raw)
        (grid-calc-excel-formula-text (substring-no-properties raw))
      (grid-calc-err "#N/A"))))

(defconst grid-calc--error-numbers
  '(("#NULL!" . 1) ("#DIV/0!" . 2) ("#VALUE!" . 3) ("#REF!" . 4)
    ("#NAME?" . 5) ("#NUM!" . 6) ("#N/A" . 7) ("#GETTING_DATA" . 8)
    ("#SPILL!" . 9) ("#CALC!" . 14))
  "The number ERROR.TYPE gives for each error value.")

(grid-calc-define error.type ((value any))
  "The number of the error value VALUE, as #DIV/0! is 2; #N/A for no error."
  :errors t
  (or (and (grid-calc-err-p value)
           (cdr (assoc (grid-calc-err-code value) grid-calc--error-numbers)))
      (grid-calc-err "#N/A")))

(grid-calc-define na ()
  "The error value #N/A."
  (grid-calc-err "#N/A"))

(grid-calc-define n ((value values))
  "VALUE as a number: a number itself, TRUE 1, anything else 0.
A range gives what its first cell holds."
  :errors t
  (let ((value (grid-calc--first-of value)))
    (cond ((numberp value) value)
          ((eq value t) 1)
          ((grid-calc-err-p value) value)
          (t 0))))

(grid-calc-define t ((value values))
  "VALUE when it is text, else empty text.
A range gives what its first cell holds."
  :errors t
  (let ((value (grid-calc--first-of value)))
    (cond ((stringp value) value)
          ((grid-calc-err-p value) value)
          (t ""))))

(defun grid-calc--first-of (value)
  "Return the first of VALUE, a `values' argument: a range's corner cell."
  (cond ((grid-calc-ref-p value) (grid-calc-ref-cell value 0 0))
        ((grid-calc-array-p value) (grid-calc-aref value 0 0))
        (t value)))

(grid-calc-define type ((value values))
  "What VALUE is: 1 a number or blank, 2 text, 4 TRUE or FALSE, 16 an
error value and 64 several values."
  :errors t
  (let ((value (grid-calc--deref value)))
    (cond ((grid-calc-array-p value) 64)
          ((stringp value) 2)
          ((grid-calc-boolean-p value) 4)
          ((grid-calc-err-p value) 16)
          (t 1))))

(grid-calc-define single ((value values))
  "The one value VALUE stands for, as @ makes it."
  :errors t
  (cond ((grid-calc-ref-p value)
         (let ((grid-calc-legacy-arrays t) (grid-calc--array-context nil))
           (grid-calc--intersect value)))
        ((grid-calc-array-p value) (grid-calc-aref value 0 0))
        (t value)))

(provide 'grid-table-calc-logic)

;;; grid-table-calc-logic.el ends here
