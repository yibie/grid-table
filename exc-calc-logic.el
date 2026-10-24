;;; exc-calc-logic.el --- Logical and information functions -*- lexical-binding: t -*-

;;; Commentary:

;; IF and the functions that choose among their arguments, AND, OR and
;; the rest of the logical functions, and the functions that say what a
;; value is: ISBLANK, ISERROR, TYPE and the like.

;;; Code:

(require 'exc-calc)

;; The model is in exc-data-model.el, which loads exc-calc, which loads
;; this file: requiring it here would be a circular load.
(declare-function exc-data-model-rows "exc-data-model")
(declare-function exc-data-model-cols "exc-data-model")
(declare-function exc-data-model-cells "exc-data-model")
(declare-function exc-cell-raw-input "exc-data-model")
(declare-function exc-model-array-at "exc-data-model")
(declare-function exc-model-legacy-at "exc-data-model")

;;;----------------------------------------------------------------------
;;; Choosing
;;;----------------------------------------------------------------------

(defun exc-calc--branch (ast)
  "Return the value of the lazy argument AST as a branch gives it.
A reference stays one, for a function around the choice to read whole;
an argument left out is FALSE, one left empty blank."
  (if ast (exc-calc-arg ast 'values) :false))

(exc-calc-define if ((condition lazy) (then lazy) &optional (else lazy))
  "THEN when CONDITION is true, else ELSE; FALSE without ELSE.
A CONDITION of several values chooses for each of them."
  (let ((test (exc-calc-arg condition 'any)))
    (if (not (exc-calc-array-p test))
        (exc-calc--branch (if (exc-calc-truth test) then else))
      (let ((yes (exc-calc--deref (exc-calc--branch then)))
            (no (exc-calc--deref (exc-calc--branch else))))
        (exc-calc--broadcast
         (lambda (test yes no)
           (exc-calc--catching
             (if (exc-calc-truth test) yes no)))
         (list test yes no))))))

(exc-calc-define ifs (&rest (pairs lazy))
  "The value after the first condition that is true; #N/A when none is."
  (when (or (null pairs) (cl-oddp (length pairs)))
    (exc-calc-raise "#VALUE!"))
  (catch 'found
    (while pairs
      (let ((test (pop pairs)) (value (pop pairs)))
        (when (exc-calc-truth (exc-calc-arg test 'any))
          (throw 'found (exc-calc-arg value 'values)))))
    (exc-calc-err "#N/A")))

(defun exc-calc--unless-error (value fallback errorp)
  "The lazy VALUE, or the lazy FALLBACK where it is an error ERRORP takes.
For several values, each error among them is swapped for the value of
FALLBACK at its place."
  (let ((value (exc-calc--scalar-or-array (exc-calc-arg value 'values))))
    (cond
     ((exc-calc-array-p value)
      (if (not (cl-loop for row below (exc-calc-array-rows value)
                        thereis (cl-loop for col below (exc-calc-array-cols value)
                                         thereis (funcall errorp
                                                          (exc-calc-aref value row col)))))
          value
        (exc-calc--broadcast
         (lambda (one other) (if (funcall errorp one) other one))
         (list value (exc-calc--deref (exc-calc--branch fallback))))))
     ((funcall errorp value) (exc-calc--first (exc-calc--branch fallback)))
     (t value))))

(exc-calc-define iferror ((value lazy) (fallback lazy))
  "VALUE, or FALLBACK when VALUE is an error value."
  (exc-calc--unless-error value fallback #'exc-calc-err-p))

(exc-calc-define ifna ((value lazy) (fallback lazy))
  "VALUE, or FALLBACK when VALUE is #N/A."
  (exc-calc--unless-error value fallback
                           (lambda (one) (eq one (exc-calc-err "#N/A")))))

(defun exc-calc--switch-equal (value case)
  "Return non-nil when VALUE matches CASE, as SWITCH takes a match.
Blank is the number 0 there, and matches 0 and nothing else: no text,
no TRUE and no FALSE.  Text matches text without regard to case, and
TRUE and FALSE match only themselves, as pycel's logical workbook has
it, where SWITCH(0,<empty>,\"match\") matches and SWITCH(<empty>,\"\")
does not."
  (let ((value (if (eq value :blank) 0 value))
        (case (if (eq case :blank) 0 case)))
    (cond ((and (numberp value) (numberp case))
           (exc-calc--numbers-equal-p value case))
          ((and (stringp value) (stringp case))
           (string= (downcase value) (downcase case)))
          (t (and (exc-calc-boolean-p value) (eq value case))))))

(exc-calc-define switch ((value any) &rest (cases lazy))
  "The result after the first case equal to VALUE, or the last argument.
Cases and results alternate; an argument left over is the default, and
without one no match is #N/A."
  (when (< (length cases) 2) (exc-calc-raise "#VALUE!"))
  (catch 'found
    (while (cdr cases)
      (let ((case (exc-calc-arg (pop cases) 'any))
            (result (pop cases)))
        (when (exc-calc--switch-equal value case)
          (throw 'found (exc-calc-arg result 'values)))))
    (if cases (exc-calc-arg (car cases) 'values) (exc-calc-err "#N/A"))))

(exc-calc-define choose ((index integer) &rest (choices lazy))
  "The INDEXth of CHOICES, counting from 1."
  (unless (<= 1 index (length choices)) (exc-calc-raise "#VALUE!"))
  (exc-calc--branch (nth (1- index) choices)))

(exc-calc-define let (&rest (parts lazy))
  "The last argument, computed with each name before it given the value
after it, as LET(x,2,y,x*3,x+y) is 8."
  (when (or (< (length parts) 3) (cl-evenp (length parts)))
    (exc-calc-raise "#VALUE!"))
  (let ((exc-calc--locals exc-calc--locals))
    (while (cdr parts)
      (let ((name (pop parts)) (value (pop parts)))
        (unless (eq (car-safe name) 'NAME) (exc-calc-raise "#NAME?"))
        (push (cons (upcase (cadr name)) (exc-calc-arg value 'values))
              exc-calc--locals)))
    (exc-calc-arg (car parts) 'values)))

(exc-calc-define lambda (&rest (parts lazy))
  "A function of the names before the last argument, which it computes:
LAMBDA(x,x+1)(5) is 6."
  (unless parts (exc-calc-raise "#VALUE!"))
  (let ((names (mapcar (lambda (part)
                         (if (eq (car-safe part) 'NAME)
                             (upcase (cadr part))
                           (exc-calc-raise "#VALUE!")))
                       (butlast parts))))
    (unless (= (length names) (length (delete-dups (copy-sequence names))))
      (exc-calc-raise "#VALUE!"))
    (exc-calc-lambda--make :parameters names :body (car (last parts))
                            :locals exc-calc--locals)))

(exc-calc-define \(apply\) ((callee values) &rest (arguments lazy))
  "What CALLEE, a LAMBDA, gives for ARGUMENTS, as LAMBDA(x,x+1)(5) asks."
  :errors t
  (cond ((exc-calc-err-p callee) callee)
        ((exc-calc-lambda-p callee)
         (exc-calc-apply-lambda
          callee (mapcar #'exc-calc--lambda-argument arguments)))
        (t (exc-calc-err "#VALUE!"))))

(exc-calc-define isomitted ((name lazy))
  "TRUE when NAME is an argument its LAMBDA was not given."
  (exc-calc-bool
   (and (eq (car-safe name) 'NAME)
        (eq (cdr (assoc (upcase (cadr name)) exc-calc--locals))
            'exc-calc--omitted))))

(defun exc-calc--function-or-value (ast)
  "Return what the lazy argument AST gives, as a `values' argument.
A built-in function's name alone is that function, a `exc-calc-lambda',
as SUM is in BYROW(A1:B2,SUM)."
  (pcase ast
    ((and `(NAME ,name)
          (guard (not (assoc (upcase name) exc-calc--locals)))
          (guard (not (exc-calc--name-ast name)))
          (guard (gethash (upcase name) exc-calc-functions)))
     (exc-calc-lambda--make :function (upcase name)))
    (_ (exc-calc-arg ast 'values))))

(defun exc-calc--function-argument (ast)
  "Return the function the lazy argument AST gives, a `exc-calc-lambda'.
A built-in function's name alone is that function, as SUM is in
BYROW(A1:B2,SUM).  Anything else is #VALUE!."
  (let ((value (exc-calc--function-or-value ast)))
    (cond ((exc-calc-lambda-p value) value)
          ((exc-calc-err-p value) (exc-calc-raise (exc-calc-err-code value)))
          (t (exc-calc-raise "#VALUE!")))))

(defun exc-calc--one-result (value)
  "Return VALUE, what a LAMBDA gave, as one value of an array.
Several values in one place are #CALC!."
  (let ((value (exc-calc--deref value)))
    (cond ((not (exc-calc-array-p value)) value)
          ((and (= (exc-calc-array-rows value) 1)
                (= (exc-calc-array-cols value) 1))
           (exc-calc-aref value 0 0))
          (t (exc-calc-err "#CALC!")))))

(defun exc-calc--call-each (function &rest values)
  "Return one value of what FUNCTION gives for VALUES, or the error."
  (exc-calc--one-result
   (exc-calc--catching (exc-calc-apply-lambda function values))))

(defun exc-calc--by-lines (array function across)
  "Return what FUNCTION gives for each row of ARRAY, or with ACROSS each
column, as BYROW and BYCOL do."
  (let* ((rows (exc-calc-array-rows array))
         (cols (exc-calc-array-cols array))
         (count (if across cols rows))
         (results (vconcat
                   (cl-loop for index below count collect
                            (exc-calc--call-each
                             function
                             (if across
                                 (exc-calc-array rows 1 (lambda (row _)
                                                           (exc-calc-aref array row index)))
                               (exc-calc-array 1 cols (lambda (_ col)
                                                         (exc-calc-aref array index col)))))))))
    (if across
        (exc-calc-array 1 count (lambda (_ col) (aref results col)))
      (exc-calc-array count 1 (lambda (row _) (aref results row))))))

(exc-calc-define byrow ((array array) (function lazy))
  "What FUNCTION gives for each row of ARRAY, one value a row."
  (exc-calc--by-lines array (exc-calc--function-argument function) nil))

(exc-calc-define bycol ((array array) (function lazy))
  "What FUNCTION gives for each column of ARRAY, one value a column."
  (exc-calc--by-lines array (exc-calc--function-argument function) t))

(exc-calc-define map (&rest (parts lazy))
  "What the last of PARTS, a function, gives for the values in the same
place of each array before it."
  (when (< (length parts) 2) (exc-calc-raise "#VALUE!"))
  (let* ((function (exc-calc--function-argument (car (last parts))))
         (arrays (mapcar (lambda (part)
                           (let ((value (exc-calc--deref (exc-calc-arg part 'values))))
                             (cond ((exc-calc-lambda-p value) (exc-calc-raise "#VALUE!"))
                                   ((exc-calc-array-p value) value)
                                   (t (exc-calc-array 1 1 (lambda (_ _) value))))))
                         (butlast parts))))
    (exc-calc-array (apply #'max (mapcar #'exc-calc-array-rows arrays))
                     (apply #'max (mapcar #'exc-calc-array-cols arrays))
                     (lambda (row col)
                       (apply #'exc-calc--call-each function
                              (mapcar (lambda (array) (exc-calc-aref array row col))
                                      arrays))))))

(defun exc-calc--accumulate (initial array function each)
  "Return the values of folding FUNCTION over ARRAY's values from INITIAL,
row after row, calling EACH with each new one."
  (let ((value (if (eq initial 'exc-calc--omitted) 0 initial))
        (cols (exc-calc-array-cols array)))
    (dotimes (row (exc-calc-array-rows array))
      (dotimes (col cols)
        (setq value (exc-calc--catching
                      (exc-calc-apply-lambda
                       function (list value (exc-calc-aref array row col)))))
        (funcall each row col value)))
    value))

(exc-calc-define reduce ((initial lazy) (array array) (function lazy))
  "INITIAL with FUNCTION applied to it and each value of ARRAY in turn,
the result each time taking its place: REDUCE(0,A1:A3,LAMBDA(a,b,a+b))."
  (exc-calc--accumulate (exc-calc--lambda-argument initial) array
                         (exc-calc--function-argument function) #'ignore))

(exc-calc-define scan ((initial lazy) (array array) (function lazy))
  "Each value REDUCE comes to on its way through ARRAY, in its place."
  (let ((results (exc-calc-array (exc-calc-array-rows array)
                                  (exc-calc-array-cols array))))
    (exc-calc--accumulate
     (exc-calc--lambda-argument initial) array
     (exc-calc--function-argument function)
     (lambda (row col value)
       (aset (exc-calc-array-data results)
             (+ (* row (exc-calc-array-cols array)) col)
             (exc-calc--one-result value))))
    results))

(exc-calc-define makearray ((rows any) (cols any) (function lazy))
  "A ROWS by COLS array of what FUNCTION gives for each row and column,
counting from 1."
  (let ((function (exc-calc--function-argument function))
        ;; TRUE is 1, but text is no count.
        (counts (mapcar (lambda (count)
                          (let ((count (if (eq count t) 1 count)))
                            (if (and (numberp count) (>= count 1))
                                (truncate count)
                              (exc-calc-raise "#VALUE!"))))
                        (list rows cols))))
    (exc-calc-array (car counts) (cadr counts)
                     (lambda (row col)
                       (exc-calc--call-each function (1+ row) (1+ col))))))

;;;----------------------------------------------------------------------
;;; Logical functions
;;;----------------------------------------------------------------------

(defun exc-calc--truths (arguments)
  "Return the truth values among ARGUMENTS, `values' arguments, in order.
In a range or array only numbers and TRUE and FALSE count; text written
as an argument must be TRUE or FALSE.  None at all is #VALUE!."
  (let (truths)
    (dolist (argument arguments)
      (exc-calc-each-value
       argument
       (lambda (value direct)
         (cond
          ((exc-calc-err-p value) (exc-calc-raise (exc-calc-err-code value)))
          ((numberp value) (push (/= value 0) truths))
          ((eq value t) (push t truths))
          ((eq value :false) (push nil truths))
          ((and direct (stringp value)) (push (exc-calc-truth value) truths))
          ((and direct (eq value :blank)) (push nil truths))))))
    (unless truths (exc-calc-raise "#VALUE!"))
    (nreverse truths)))

(exc-calc-define and (&rest (conditions values))
  "TRUE when every one of CONDITIONS is true."
  (exc-calc-bool (cl-every #'identity (exc-calc--truths conditions))))

(exc-calc-define or (&rest (conditions values))
  "TRUE when any one of CONDITIONS is true."
  (exc-calc-bool (cl-some #'identity (exc-calc--truths conditions))))

(exc-calc-define xor (&rest (conditions values))
  "TRUE when an odd number of CONDITIONS are true."
  (exc-calc-bool (cl-oddp (cl-count t (exc-calc--truths conditions)))))

(exc-calc-define not ((condition bool))
  "TRUE when CONDITION is false."
  (exc-calc-bool (not condition)))

(exc-calc-define true ()
  "TRUE."
  t)

(exc-calc-define false ()
  "FALSE."
  :false)

;;;----------------------------------------------------------------------
;;; What a value is
;;;----------------------------------------------------------------------

(defmacro exc-calc--define-is (name doc test)
  "Define NAME, TRUE when TEST, a function of the value, holds; DOC says when."
  `(exc-calc-define ,name ((value any))
     ,doc
     :errors t
     (exc-calc-bool (funcall ,test value))))

(exc-calc--define-is isblank "TRUE when VALUE is an empty cell."
                      #'exc-calc-blank-p)
(exc-calc--define-is isnumber "TRUE when VALUE is a number." #'numberp)
(exc-calc--define-is istext "TRUE when VALUE is text." #'stringp)
(exc-calc--define-is isnontext "TRUE when VALUE is not text."
                      (lambda (value) (not (stringp value))))
(exc-calc--define-is islogical "TRUE when VALUE is TRUE or FALSE."
                      #'exc-calc-boolean-p)
(exc-calc--define-is iserror "TRUE when VALUE is an error value."
                      #'exc-calc-err-p)
(exc-calc--define-is iserr "TRUE when VALUE is an error value but #N/A."
                      (lambda (value)
                        (and (exc-calc-err-p value)
                             (not (eq value (exc-calc-err "#N/A"))))))
(exc-calc--define-is isna "TRUE when VALUE is #N/A."
                      (lambda (value) (eq value (exc-calc-err "#N/A"))))

(exc-calc-define iseven ((number number))
  "TRUE when NUMBER, its fraction cut off, is even."
  :toolpak t
  (exc-calc-bool (cl-evenp (truncate number))))

(exc-calc-define isodd ((number number))
  "TRUE when NUMBER, its fraction cut off, is odd."
  :toolpak t
  (exc-calc-bool (cl-oddp (truncate number))))

(exc-calc-define isref ((value values))
  "TRUE when VALUE is a reference."
  :errors t
  (exc-calc-bool (or (exc-calc-ref-p value) (eq (car-safe value) 'union))))

(defun exc-calc--raw-at (ref)
  "Return the raw input of the cell in the corner of REF, or nil."
  (let ((model (exc-calc-ref-model ref))
        (row (exc-calc-ref-top ref)) (col (exc-calc-ref-left ref)))
    (and (< row (exc-data-model-rows model)) (< col (exc-data-model-cols model))
         (exc-cell-raw-input
          (aref (exc-data-model-cells model)
                (+ (* row (exc-data-model-cols model)) col))))))

(exc-calc-define isformula ((reference ref))
  "TRUE when the cell REFERENCE names holds a formula; for a range, as
many values, one for each cell."
  (if (and (= (exc-calc-ref-rows reference) 1)
           (= (exc-calc-ref-cols reference) 1))
      (exc-calc-bool (exc-calc--formula-p (exc-calc--raw-at reference)))
    (let ((model (exc-calc-ref-model reference))
          (top (exc-calc-ref-top reference))
          (left (exc-calc-ref-left reference)))
      (exc-calc-array (exc-calc-ref-rows reference) (exc-calc-ref-cols reference)
                       (lambda (row col)
                         (exc-calc-bool
                          (exc-calc--formula-p
                           (exc-calc--raw-at
                            (exc-calc-ref--make model (+ top row) (+ left col)
                                                 (+ top row) (+ left col))))))))))

(defun exc-calc-closing-paren (text open)
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

(defun exc-calc-excel-formula-text (formula)
  "Return FORMULA as Excel shows it: SINGLE(X) as @X, ANCHORARRAY(A1) as A1#.
Workbooks store the operators as these functions."
  (let ((case-fold-search t) (start 0))
    (while (string-match "\\(?:\\`\\|[^A-Za-z0-9_.]\\)\\(SINGLE\\|ANCHORARRAY\\)("
                         formula start)
      (let* ((from (match-beginning 1))
             (open (1- (match-end 0)))
             (anchor (string-equal (upcase (match-string 1 formula)) "ANCHORARRAY"))
             (close (exc-calc-closing-paren formula open)))
        (if (or (null close)
                ;; Not inside a string: an even number of quotes before.
                (cl-oddp (cl-count ?\" formula :end from)))
            (setq start (match-end 0))
          (let ((inner (exc-calc-excel-formula-text
                        (substring formula (1+ open) close))))
            (setq formula (concat (substring formula 0 from)
                                  (if anchor (concat inner "#") (concat "@" inner))
                                  (substring formula (1+ close)))
                  start from)))))
    formula))

(defconst exc-calc--many-value-functions
  '("ANCHORARRAY" "UNIQUE" "SORT" "SORTBY" "FILTER" "SEQUENCE" "RANDARRAY"
    "TEXTSPLIT" "VSTACK" "HSTACK" "TOCOL" "TOROW" "WRAPROWS" "WRAPCOLS"
    "TAKE" "DROP" "CHOOSEROWS" "CHOOSECOLS" "EXPAND")
  "Functions Excel shows an @ before where a formula takes one value of
them, as several values are all they are for.")

(defun exc-calc--many-values-node-p (node)
  "Return non-nil when the formula part NODE may stand for several cells
or values, as a range does and A1:A1 does not."
  (pcase node
    (`(RANGE ,first ,last) (not (equal first last)))
    (`(RANGE-OP . ,_) t)
    (`(FUNCTION-CALL ,name . ,_)
     (member (upcase name) exc-calc--many-value-functions))))

(defun exc-calc--explicit-singles (ast)
  "Return the parts of AST an @ of their own already takes one value of."
  (let (singles)
    (named-let walk ((node ast))
      (when (consp node)
        (pcase node
          (`(UNARY-OP @ ,operand) (push operand singles))
          (`(FUNCTION-CALL ,(and name (pred stringp)) (,operand))
           (when (string-equal (upcase name) "SINGLE")
             (push operand singles))))
        (mapc #'walk node)))
    singles))

(defun exc-calc--legacy-formula-text (formula)
  "Return FORMULA, read as a workbook file holds it, with @ shown before
each part that it takes one value of, as Excel shows it: A1:A3+1 as
@A1:A3+1.  See `exc-calc--token-classes'."
  (pcase-let ((`(,ast . ,starts)
               (ignore-errors (exc-parser-parse-starts (substring formula 1)))))
    (if (or (null ast) (eq (car-safe ast) 'ELISP-CODE))
        formula
      (let ((singles (exc-calc--explicit-singles ast))
            positions)
        (maphash (lambda (node class)
                   (let ((start (gethash node starts)))
                     (when (and start (eq class 'VAL) (not (memq node singles))
                                (exc-calc--many-values-node-p node))
                       (push (1+ start) positions))))
                 (exc-calc--token-classes ast))
        (dolist (position (sort (delete-dups positions) #'>) formula)
          (setq formula (concat (substring formula 0 position) "@"
                                (substring formula position))))))))

(exc-calc-define formulatext ((reference ref))
  "The formula of the cell REFERENCE names, as text, as Excel shows it;
#N/A when there is none."
  (let* ((raw (exc-calc--raw-at reference))
         (model (exc-calc-ref-model reference))
         (row (exc-calc-ref-top reference))
         (col (exc-calc-ref-left reference)))
    (if (exc-calc--formula-p raw)
        (let ((raw (substring-no-properties raw)))
          (exc-calc-excel-formula-text
           (if (and (not (exc-model-array-at model row col))
                    (or (exc-model-legacy-at model row col)
                        (if (eq exc-calc--legacy-outside 'unset)
                            exc-calc-legacy-arrays
                          exc-calc--legacy-outside)))
               (exc-calc--legacy-formula-text raw)
             raw)))
      (exc-calc-err "#N/A"))))

(defconst exc-calc--error-numbers
  '(("#NULL!" . 1) ("#DIV/0!" . 2) ("#VALUE!" . 3) ("#REF!" . 4)
    ("#NAME?" . 5) ("#NUM!" . 6) ("#N/A" . 7) ("#GETTING_DATA" . 8)
    ("#SPILL!" . 9) ("#CALC!" . 14))
  "The number ERROR.TYPE gives for each error value.")

(exc-calc-define error.type ((value any))
  "The number of the error value VALUE, as #DIV/0! is 2; #N/A for no error."
  :errors t
  (or (and (exc-calc-err-p value)
           (cdr (assoc (exc-calc-err-code value) exc-calc--error-numbers)))
      (exc-calc-err "#N/A")))

(exc-calc-define na ()
  "The error value #N/A."
  (exc-calc-err "#N/A"))

(exc-calc-define n ((value values))
  "VALUE as a number: a number itself, TRUE 1, anything else 0.
A range gives what its first cell holds."
  :errors t
  (let ((value (exc-calc--first-of value)))
    (cond ((numberp value) value)
          ((eq value t) 1)
          ((exc-calc-err-p value) value)
          (t 0))))

(exc-calc-define t ((value values))
  "VALUE when it is text, else empty text.
A range gives what its first cell holds."
  :errors t
  (let ((value (exc-calc--first-of value)))
    (cond ((stringp value) value)
          ((exc-calc-err-p value) value)
          (t ""))))

(defun exc-calc--first-of (value)
  "Return the first of VALUE, a `values' argument: a range's corner cell."
  (cond ((exc-calc-ref-p value) (exc-calc-ref-cell value 0 0))
        ((exc-calc-array-p value) (exc-calc-aref value 0 0))
        (t value)))

(exc-calc-define type ((value lazy))
  "What VALUE is: 1 a number or blank, 2 text, 4 TRUE or FALSE, 16 an
error value, 64 several values and 128 a function, as LAMBDA makes or
a built-in function's name is."
  :errors t
  (let ((value (exc-calc--deref (exc-calc--function-or-value value))))
    (cond ((exc-calc-array-p value) 64)
          ((exc-calc-lambda-p value) 128)
          ((stringp value) 2)
          ((exc-calc-boolean-p value) 4)
          ((exc-calc-err-p value) 16)
          (t 1))))

(exc-calc-define single ((value values))
  "The one value VALUE stands for, as @ makes it."
  :errors t
  (cond ((exc-calc-ref-p value)
         (let ((exc-calc-legacy-arrays t) (exc-calc--array-context nil))
           (exc-calc--intersect value)))
        ((exc-calc-array-p value) (exc-calc-aref value 0 0))
        (t value)))

(provide 'exc-calc-logic)

;;; exc-calc-logic.el ends here
