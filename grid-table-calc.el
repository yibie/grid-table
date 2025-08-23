;;; grid-table-calc.el --- AST evaluator and calculation engine -*- lexical-binding: t -*-

(require 'grid-table-parser)

;;;----------------------------------------------------------------------
;;; Main Calculation Entry Point
;;;----------------------------------------------------------------------

(defun grid-table-calc-evaluate (model)
  "Main entry point for evaluating all formulas in the model.
This function will iterate through all cells, parse their formulas,
and evaluate them. It's a full recalculation."
  (let* ((cells-vec (grid-data-model-cells model))
         (total-cells (length cells-vec)))
    (dotimes (i total-cells)
      (let* ((cell (aref cells-vec i))
             (raw-input (grid-cell-raw-input cell)))
        (when (string-prefix-p "=" raw-input) ; Only process if it's a formula
          (let* ((formula-str (substring raw-input 1)) ; Remove leading "="
                 (ast (grid-parser-parse formula-str)))
            (set-grid-cell-computed-value cell (grid-calc-eval-ast model ast))))
        (unless (string-prefix-p "=" raw-input) ; If it's not a formula, computed value is raw input
          (set-grid-cell-computed-value cell raw-input))))))

(defvar grid-calc-functions (make-hash-table :test 'equal))

(defmacro grid-calc-defun (name args &rest body)
  "Defines a built-in calculation function and registers it."
  `(puthash ,(symbol-name name)
            (lambda (model . ,args) ; All functions receive model as first arg
              ,@body)
            grid-calc-functions))

;;;----------------------------------------------------------------------
;;; Cell Formula Management
;;;----------------------------------------------------------------------

(defun grid-table-calc-set-cell-formula (model cell-coords formula)
  "Set a formula for a cell and trigger recalculation.
MODEL: The grid data model.
CELL-COORDS: A cons cell (row . col) representing the cell coordinates.
FORMULA: The formula string to set."
  (let* ((row (car cell-coords))
         (col (cdr cell-coords))
         (cell (grid-model-get-cell model row col)))
    (when cell
      (set-grid-cell-raw-input cell formula)
      ;; Trigger a full recalculation
      (grid-table-calc-evaluate model))))

;;;----------------------------------------------------------------------
;;; AST Evaluator
;;;----------------------------------------------------------------------

(defun grid-calc-eval-ast (model ast)
  "Evaluates an AST node in the context of a grid model.
MODEL: The grid data model, used for cell references.
AST: The Abstract Syntax Tree to evaluate."
  (pcase ast
    ;; Special function handlers
    (`(FUNCTION-CALL "INDEX" ,args)
     (grid-calc-eval-index-function model args))
    (`(FUNCTION-CALL "IFERROR" ,args)
     (grid-calc-eval-iferror-function model args))
    (`(FUNCTION-CALL "IFS" ,args)
     (grid-calc-eval-ifs-function model args))
    (`(FUNCTION-CALL "VLOOKUP" ,args)
     (grid-calc-eval-vlookup-function model args))

    ;; Elisp code execution
    (`(ELISP-CODE ,code-str)
     ;; only if user opted-in to elisp evaluation
     (if grid-table-allow-elisp
         (let ((cell (lambda (ref) (grid-calc-get-cell-value model ref))))
           (condition-case err
               (eval (read code-str))
             (error (format "Elisp Error: %s" (error-message-string err)))))
       (error "Elisp disabled: see `grid-table-allow-elisp'")))

    ;; Number literal
    (`(NUMBER ,value) value)
    
    ;; String literal
    (`(STRING ,value) value)
    
    ;; Cell reference
    (`(CELL-REF ,ref-str)
     (grid-calc-get-cell-value model ref-str))
    
    ;; Range reference
    (`(RANGE ,start-cell-ref-ast ,end-cell-ref-ast)
     (grid-calc-get-range-values model start-cell-ref-ast end-cell-ref-ast))
    
    ;; Binary operation
    (`(BINARY-OP ,op ,left-ast ,right-ast)
     (let ((left-val (grid-calc-eval-ast model left-ast))
           (right-val (grid-calc-eval-ast model right-ast)))
       (grid-calc-apply-binary-op op left-val right-val)))
    
    ;; Unary operation
    (`(UNARY-OP ,op ,operand-ast)
     (let ((operand-val (grid-calc-eval-ast model operand-ast)))
       (grid-calc-apply-unary-op op operand-val)))
    
    ;; Function call
    (`(FUNCTION-CALL ,func-name ,args-ast-list)
     (let ((arg-values (mapcar (lambda (arg-ast)
                                (grid-calc-eval-ast model arg-ast))
                             args-ast-list)))
       (grid-calc-call-function model func-name arg-values)))
    
    ;; Unknown node type
    (_ (error "Unknown AST node type: %s" ast))))

;;;----------------------------------------------------------------------
;;; Operator Implementations
;;;----------------------------------------------------------------------

(defun grid-calc-apply-binary-op (op left right)
  "Applies a binary operator."
  (pcase op
    ;; Arithmetic operations
    ('+ (+ (grid-calc-to-number left) (grid-calc-to-number right)))
    ('- (- (grid-calc-to-number left) (grid-calc-to-number right)))
    ('* (* (grid-calc-to-number left) (grid-calc-to-number right)))
    ('/ (let ((r (grid-calc-to-number right)))
          (if (= r 0)
              (error "Division by zero")
            (/ (grid-calc-to-number left) r))))
    ('% (% (grid-calc-to-number left) (grid-calc-to-number right)))
    ('^ (expt (grid-calc-to-number left) (grid-calc-to-number right)))
    
    ;; Comparison operations
    ('= (equal left right))
    ('<> (not (equal left right)))
    ('< (< (grid-calc-to-number left) (grid-calc-to-number right)))
    ('> (> (grid-calc-to-number left) (grid-calc-to-number right)))
    ('<= (<= (grid-calc-to-number left) (grid-calc-to-number right)))
    ('>= (>= (grid-calc-to-number left) (grid-calc-to-number right)))
    
    ;; Logical operations
    ('AND (and left right))
    ('OR (or left right))
    
    ;; String concatenation
    ('& (concat (format "%s" left) (format "%s" right)))
    
    (_ (error "Unknown binary operator: %s" op))))

(defun grid-calc-apply-unary-op (op operand)
  "Applies a unary operator."
  (pcase op
    ('+ (grid-calc-to-number operand))
    ('- (- (grid-calc-to-number operand)))
    ('NOT (not operand))
    (_ (error "Unknown unary operator: %s" op))))

;;;----------------------------------------------------------------------
;;; Type Conversion and Utility Functions
;;;----------------------------------------------------------------------

(defun grid-calc-to-number (value)
  "Converts a value to a number.
Returns 0 for non-numeric values instead of throwing an error."
  (cond
   ((numberp value) value)
   ((stringp value)
    (if (string-empty-p value)
        0
      (if (string-match "^-?[0-9]+\\.?[0-9]*$" value)
          (string-to-number value)
        0))) ; Return 0 for non-numeric strings instead of throwing error
   ((null value) 0)
   (t 0))) ; Return 0 for any other type instead of throwing error

(defun grid-calc--to-time (value)
  "Convert a value to an Emacs time value if possible.
Handles time-values and strings like \"YYYY-MM-DD\"."
  (cond
   ((consp value) value) ; Assume it's already a time value
   ((stringp value)
    (condition-case nil
        (parse-time-string value)
      (error (error "Invalid date/time string: %s" value))))
   (t (error "Value cannot be converted to a date/time: %s" value))))

(defun grid-calc--parse-criteria (criteria)
  "Parse a criteria string (e.g., \">10\", \"<>0\", \"Apple\") into a predicate lambda.
The criteria can be a number, a string, or a string with a comparison operator."
  (if (numberp criteria)
      ;; Criteria is a number, e.g., 10
      (lambda (cell-val) (= (grid-calc-to-number cell-val) criteria))
    (let ((crit-str (grid-calc-to-string criteria)))
      (if (string-match "^\\([<>=]+\\)\\(.*\\)$" crit-str)
          ;; Criteria is a comparison, e.g., ">=10"
          (let* ((op-str (match-string 1 crit-str))
                 (val-str (match-string 2 crit-str))
                 (op (pcase op-str
                       ("="  '=)
                       (">"  '>)
                       ("<"  '<)
                       (">=" '>=)
                       ("<=" '<=)
                       ("<>"  (lambda (a b) (not (= a b))))
                       (_    'equal)))
                 (val-num (string-to-number val-str)))
            (lambda (cell-val)
              (funcall op (grid-calc-to-number cell-val) val-num)))
        ;; Criteria is a simple string match, e.g., "Apple"
        (lambda (cell-val) (equal (grid-calc-to-string cell-val) crit-str))))))

(defun grid-calc-to-string (value)
  "Converts a value to a string."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((null value) "")
   (t (format "%s" value))))

(defun grid-calc-to-boolean (value)
  "Converts a value to a boolean."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((eq value nil) nil)
   ((numberp value) (not (= value 0)))
   ((stringp value) (not (string-empty-p value)))
   (t t)))

;;;----------------------------------------------------------------------
;;; Cell and Range Value Retrieval
;;;----------------------------------------------------------------------

(defun grid-calc-parse-cell-ref (ref-str)
  "Parses a cell reference string (e.g., A1, $A$1) into a plist of coordinates.
This function is duplicated from grid-table-parser.el for now, will be refactored."
  (message "CALLING grid-calc-parse-cell-ref with: %s" ref-str)
  (cond
        ;; Absolute reference $A$1
     ((string-match "^\\$\\([A-Z]+\\)\\$\\([0-9]+\\)$" ref-str) ; Excel row 1 is user-defined header (data row 0)
      (list :col (grid-calc-col-letter-to-number (match-string 1 ref-str))
            :row (- (string-to-number (match-string 2 ref-str)) 1) ; Adjust for 0-based data model
            :absolute-col t :absolute-row t))
     ;; Mixed reference $A1 or A$1
     ((string-match "^\\(\\$?\\)\\([A-Z]+\\)\\(\\$?\\)\\([0-9]+\\)$" ref-str)
      (list :col (grid-calc-col-letter-to-number (match-string 2 ref-str))
            :row (- (string-to-number (match-string 4 ref-str)) 1) ; Adjust for 0-based data model
            :absolute-col (not (string-empty-p (match-string 1 ref-str)))
            :absolute-row (not (string-empty-p (match-string 3 ref-str)))))
   ;; Single cell reference (e.g., A1)
   ;; Excel row 1 is the first data row (user-defined headers).
   ;; So, Excel row N maps to our data model index (N-1).
   ((string-match "^\\([A-Z]+\\)\\([0-9]+\\)$" ref-str)
    (let* ((col-str (match-string 1 ref-str))
           (row-str (match-string 2 ref-str))
           (row-num (string-to-number row-str))
           (model-row (- row-num 1)))
      (message "DEBUG: %s -> col=%s, row=%s, model-row=%s" ref-str col-str row-str model-row)
      (list :col (grid-calc-col-letter-to-number col-str)
            :row model-row
            :absolute-col nil :absolute-row nil)))
   ;; Fallback for debugging
   (t (message "NO MATCH for: %s" ref-str) nil)))

(defun grid-calc-col-letter-to-number (letters)
  "Converts column letters (e.g., A, B, AA) to a 0-based number."
  (let ((result 0))
    (dolist (char (string-to-list (upcase letters)))
      (setq result (+ (* result 26) (- char ?A))))
    result))

(defun grid-calc-get-cell-value (model ref-str)
  "Retrieves the computed value of a cell from the model."
  (let* ((ref (grid-calc-parse-cell-ref ref-str))
         (row (plist-get ref :row))
         (col (plist-get ref :col)))
    (when (and ref (>= row 0) (>= col 0))
      (when (and (< row (grid-data-model-rows model))
                 (< col (grid-data-model-cols model)))
        (grid-model-get-computed-value model row col)))))

(defun grid-calc-get-range-values (model start-cell-ref-ast end-cell-ref-ast)
  "Retrieves a flat list of computed values from a range."
  (let* ((start-ref-str (cadr start-cell-ref-ast)) ; Extract "A1" from (CELL-REF "A1")
         (end-ref-str (cadr end-cell-ref-ast))
         (start-ref (grid-calc-parse-cell-ref start-ref-str))
         (end-ref (grid-calc-parse-cell-ref end-ref-str))
         (values '()))
    
    (when (and start-ref end-ref (plist-get start-ref :row) (plist-get end-ref :row))
      (let* ((row-start (min (plist-get start-ref :row) (plist-get end-ref :row)))
             (row-end   (max (plist-get start-ref :row) (plist-get end-ref :row)))
             (col-start (min (plist-get start-ref :col) (plist-get end-ref :col)))
             (col-end   (max (plist-get start-ref :col) (plist-get end-ref :col))))
        (cl-loop for r from row-start to row-end do
                 (cl-loop for c from col-start to col-end do
                          (when (and (>= r 0) (>= c 0)
                                     (< r (grid-data-model-rows model))
                                     (< c (grid-data-model-cols model)))
                            (push (grid-model-get-computed-value model r c) values))))))
    (nreverse values)))

(defun grid-calc-eval-index-function (model args-ast-list)
  "Special evaluator for the INDEX function.
MODEL: The grid data model.
ARGS-AST-LIST: List of arguments in AST form.

Returns the value at the specified row and column in the range."
  (when (< (length args-ast-list) 2)
    (error "INDEX requires at least 2 arguments"))
  (let* ((range-ast (car args-ast-list))
         (row-num-ast (cadr args-ast-list))
         (col-num-ast (caddr args-ast-list)))
    (unless (eq (car range-ast) 'RANGE)
      (error "First argument to INDEX must be a range"))
    (let* ((start-ref-str (cadr (cadr range-ast)))
           (start-coords (grid-calc-parse-cell-ref start-ref-str))
           (start-row (plist-get start-coords :row))
           (start-col (plist-get start-coords :col))
           (row-num (grid-calc-eval-ast model row-num-ast))
           (col-num (if col-num-ast
                        (grid-calc-eval-ast model col-num-ast)
                      1))
           ;; Calculate absolute coordinates from relative 1-based indices
           (target-row (+ start-row row-num -1))
           (target-col (+ start-col col-num -1)))
      (grid-model-get-computed-value model target-row target-col))))

(defun grid-calc-eval-iferror-function (model args-ast-list)
  "Special evaluator for the IFERROR function.

MODEL: The grid data model.
ARGS-AST-LIST: List of arguments in AST form.

Tries to evaluate the first argument. If it succeeds, returns its value.
If an error occurs during evaluation, evaluates and returns the second argument."
  (when (/= (length args-ast-list) 2)
    (error "IFERROR requires exactly 2 arguments"))
  (let ((value-ast (car args-ast-list))
        (value-if-error-ast (cadr args-ast-list)))
    (condition-case err
        ;; Try to evaluate the first argument
        (grid-calc-eval-ast model value-ast)
      (error
       ;; If an error occurs, evaluate and return the second argument
       (grid-calc-eval-ast model value-if-error-ast)))))

(defun grid-calc-eval-ifs-function (model args-ast-list)
  "Special evaluator for the IFS function.

MODEL: The grid data model.
ARGS-AST-LIST: List of arguments in AST form (pairs of condition and value).

Evaluates each condition in turn. Returns the value corresponding to the first
condition that evaluates to true. If no conditions are met, raises an error."
  (when (oddp (length args-ast-list))
    (error "IFS requires an even number of arguments"))
  (let ((remaining-args args-ast-list))
    (while remaining-args
      (let* ((test-ast (car remaining-args))
             (value-ast (cadr remaining-args))
             (test-result (grid-calc-eval-ast model test-ast)))
        (if (grid-calc-to-boolean test-result)
            (cl-return-from grid-calc-eval-ifs-function
              (grid-calc-eval-ast model value-ast)))
        (setq remaining-args (cddr remaining-args)))))
  (error "No condition in IFS was met")) ; Return #N/A error

(defun grid-calc-eval-vlookup-function (model args-ast-list)
  "Special evaluator for the VLOOKUP function."
  (when (< (length args-ast-list) 3)
    (error "VLOOKUP requires at least 3 arguments"))
  (let* ((lookup-value (grid-calc-eval-ast model (nth 0 args-ast-list)))
         (table-array-ast (nth 1 args-ast-list))
         (col-index-num (grid-calc-eval-ast model (nth 2 args-ast-list)))
         (exact-match (if (nth 3 args-ast-list)
                          (grid-calc-to-boolean (grid-calc-eval-ast model (nth 3 args-ast-list)))
                        t))) ; Default to approximate match if omitted
    (unless (eq (car table-array-ast) 'RANGE)
      (error "Second argument to VLOOKUP must be a range"))
    (when exact-match
      (error "VLOOKUP only supports exact match (FALSE) at this time."))

    (let* ((start-ref-str (cadr (cadr table-array-ast)))
           (end-ref-str (cadr (caddr table-array-ast)))
           (start-coords (grid-calc-parse-cell-ref start-ref-str))
           (end-coords (grid-calc-parse-cell-ref end-ref-str))
           (start-row (plist-get start-coords :row))
           (end-row (plist-get end-coords :row))
           (start-col (plist-get start-coords :col))
           (target-col (+ start-col col-index-num -1)))

      (cl-loop for r from start-row to end-row
               do (let ((cell-val (grid-model-get-computed-value model r start-col)))
                    (when (equal cell-val lookup-value)
                      (cl-return-from grid-calc-eval-vlookup-function
                        (grid-model-get-computed-value model r target-col)))))

      ;; If loop finishes, no match was found
      (error "No match found in VLOOKUP"))))

;;;----------------------------------------------------------------------
;;; Built-in Function Library
;;;----------------------------------------------------------------------

;;; Logical functions
(grid-calc-defun COUNTA (range-values)
  "Counts the number of cells that are not empty in a range."
  (cl-count-if (lambda (v) (not (or (null v) (equal v ""))))
               range-values))

(grid-calc-defun ISBLANK (value)
  "Returns TRUE if the value is blank."
  (or (null value) (equal value "")))

(grid-calc-defun ISNUMBER (value)
  "Returns TRUE if the value is a number."
  (numberp value))

(grid-calc-defun ISTEXT (value)
  "Returns TRUE if the value is text."
  (stringp value))

;;; Math functions
(defun grid-calc--round-by (round-fn number num_digits)
  "Helper for rounding functions."
  (let* ((num (grid-calc-to-number number))
         (digits (grid-calc-to-number num_digits))
         (p (expt 10.0 digits)))
    (/ (funcall round-fn (* num p)) p)))

(grid-calc-defun ROUND (number num_digits)
  "Rounds a number to a specified number of digits."
  (grid-calc--round-by #'round number num_digits))

(grid-calc-defun ROUNDUP (number num_digits)
  "Rounds a number up, away from zero."
  (let ((num (grid-calc-to-number number)))
    (if (>= num 0)
        (grid-calc--round-by #'ceiling number num_digits)
      (grid-calc--round-by #'floor number num_digits))))

(grid-calc-defun ROUNDDOWN (number num_digits)
  "Rounds a number down, toward zero."
  (let ((num (grid-calc-to-number number)))
    (if (>= num 0)
        (grid-calc--round-by #'floor number num_digits)
      (grid-calc--round-by #'ceiling number num_digits))))

(grid-calc-defun INT (number)
  "Rounds a number down to the nearest integer."
  (truncate (grid-calc-to-number number)))

(grid-calc-defun MOD (number divisor)
  "Returns the remainder after a number is divided by a divisor."
  (mod (grid-calc-to-number number) (grid-calc-to-number divisor)))

(grid-calc-defun ABS (number)
  "Returns the absolute value of a number."
  (abs (grid-calc-to-number number)))

(grid-calc-defun SQRT (number)
  "Returns a positive square root."
  (let ((num (grid-calc-to-number number)))
    (if (< num 0)
        (error "SQRT of negative number: %s" num)
      (sqrt num))))

(grid-calc-defun POWER (number power)
  "Returns the result of a number raised to a power."
  (expt (grid-calc-to-number number) (grid-calc-to-number power)))

(defun grid-calc--get-numeric-values (values)
  "From a list of VALUES, return a new list containing only numbers.
Strings are converted to numbers if possible. Non-numeric values are ignored.
Uses grid-calc-to-number for consistent conversion."
  (let ((numbers '()))
    (dolist (v values)
      (let ((num (grid-calc-to-number v)))
        (when (and num (not (= num 0)) (or (numberp v) (and (stringp v) (string-match "^-?[0-9]+\\.?[0-9]*$" v))))
          (push num numbers))))
    (nreverse numbers)))



(defun grid-calc-call-function (model func-name args)
  "Calls a built-in function."
  (let ((func (gethash (upcase func-name) grid-calc-functions)))
    (if func
        (apply func model args) ; Pass model as first arg
      (error "Unknown function: %s" func-name))))

;; Math functions
(grid-calc-defun SUM (range-values)
  "Sums numbers in a range. Ignores non-numeric values."
  (apply #'+ (grid-calc--get-numeric-values range-values)))

(grid-calc-defun AVERAGE (range-values)
  "Averages numbers in a range. Ignores non-numeric values."
  (let ((numbers (grid-calc--get-numeric-values range-values)))
    (if (null numbers) 0
      (/ (apply #'+ numbers) (float (length numbers))))))

(grid-calc-defun COUNT (range-values)
  "Counts numbers in a range. Ignores non-numeric values."
  (length (grid-calc--get-numeric-values range-values)))

(grid-calc-defun MAX (range-values)
  "Returns the maximum value in a range. Ignores non-numeric values."
  (let ((numbers (grid-calc--get-numeric-values range-values)))
    (if (null numbers) 0
      (apply #'max numbers))))

(grid-calc-defun MIN (range-values)
  "Returns the minimum value in a range. Ignores non-numeric values."
  (let ((numbers (grid-calc--get-numeric-values range-values)))
    (if (null numbers) 0
      (apply #'min numbers))))

(grid-calc-defun COUNTIF (range criteria)
  "Counts the number of cells within a range that meet the given criteria.

CRITERIA can be a number, a string, or a string with a comparison operator
like \">10\" or \"<>0\"."
  (let ((predicate (grid-calc--parse-criteria criteria)))
    (cl-count-if predicate range)))

(grid-calc-defun SUMIF (criteria-range criteria &optional sum-range)
  "Adds the cells specified by a given criteria.

CRITERIA-RANGE is the range of cells to evaluate against the CRITERIA.
SUM-RANGE (optional) is the range of cells to sum. If omitted, CRITERIA-RANGE is used.

CRITERIA can be a number, a string, or a string with a comparison operator
like \">10\" or \"<>0\"."
  (let* ((predicate (grid-calc--parse-criteria criteria))
         (range-to-sum (or sum-range criteria-range))
         (total 0))
    (when (/= (length criteria-range) (length range-to-sum))
      (error "SUMIF ranges must have the same size"))
    (dotimes (i (length criteria-range))
      (when (funcall predicate (nth i criteria-range))
        (setq total (+ total (grid-calc-to-number (nth i range-to-sum))))))
    total))

(defun grid-calc--process-ifs-args (args)
  "Helper to parse arguments for SUMIFS and COUNTIFS.
Returns a list of (list-of-values . predicate-lambda)."
  (let ((processed-args '())
        (remaining-args args))
    (while remaining-args
      (let* ((range (car remaining-args))
             (criteria (cadr remaining-args))
             (predicate (grid-calc--parse-criteria criteria)))
        (unless (listp range) (error "Criteria range must be a valid range"))
        (push (cons range predicate) processed-args)
        (setq remaining-args (cddr remaining-args))))
    (nreverse processed-args)))

(grid-calc-defun COUNTIFS (&rest args)
  "Counts the number of cells specified by a given set of conditions or criteria."
  (when (oddp (length args))
    (error "COUNTIFS requires an even number of arguments."))
  (let* ((parsed-pairs (grid-calc--process-ifs-args args))
         (num-rows (length (car (car parsed-pairs)))) ; Length of the first range
         (count 0))
    (dotimes (i num-rows)
      (let ((all-match t))
        (dolist (pair parsed-pairs)
          (let* ((range (car pair))
                 (predicate (cdr pair)))
            (unless (funcall predicate (nth i range))
              (setq all-match nil)
              (cl-return)))) ; Break inner loop
        (when all-match
          (setq count (1+ count)))))
    count))

(grid-calc-defun SUMIFS (sum_range &rest args)
  "Adds the cells in a range that meet multiple criteria."
  (when (oddp (length args))
    (error "SUMIFS requires an odd number of arguments after sum_range."))
  (let* ((parsed-pairs (grid-calc--process-ifs-args args))
         (num-rows (length sum_range))
         (total 0))
    ;; Check that all ranges have the same length
    (dolist (pair parsed-pairs)
      (when (/= (length (car pair)) num-rows)
        (error "All ranges in SUMIFS must have the same size.")))
    
    (dotimes (i num-rows)
      (let ((all-match t))
        (dolist (pair parsed-pairs)
          (let* ((range (car pair))
                 (predicate (cdr pair)))
            (unless (funcall predicate (nth i range))
              (setq all-match nil)
              (cl-return)))) ; Break inner loop
        (when all-match
          (setq total (+ total (grid-calc-to-number (nth i sum_range)))))))
    total))

(grid-calc-defun MATCH (lookup-value lookup-array &optional (match-type 0))
  "Finds the position of a value in a single-row or single-column range.
NOTE: Only exact match (match-type = 0) is currently implemented."
  (when (/= match-type 0)
    (error "MATCH only supports exact match (match-type 0) at this time"))
  (let ((pos (cl-position lookup-value lookup-array :test #'equal)))
    (if pos
        (1+ pos) ; Return 1-based index
      (error "Value not available in MATCH"))))

;; Logical functions
(grid-calc-defun IF (condition true-value false-value)
  "Conditional function."
  (if (grid-calc-to-boolean condition) true-value false-value))

(defun grid-calc--every (pred list)
  "Returns non-nil if PRED is true for all elements in LIST."
  (let ((result t))
    (catch 'found-false
      (dolist (item list)
        (unless (funcall pred item)
          (setq result nil)
          (throw 'found-false nil)))
      result)))

(defun grid-calc--some (pred list)
  "Returns non-nil if PRED is true for any element in LIST."
  (let ((result nil))
    (catch 'found-true
      (dolist (item list)
        (when (funcall pred item)
          (setq result t)
          (throw 'found-true t)))
      result)))

(grid-calc-defun AND (&rest conditions) "Logical AND." (grid-calc--every #'grid-calc-to-boolean conditions))
(grid-calc-defun OR (&rest conditions) "Logical OR." (grid-calc--some #'grid-calc-to-boolean conditions))

;; Text functions
;; Text functions
(grid-calc-defun CONCATENATE (&rest strings)
  "Concatenates strings."
  (mapconcat #'grid-calc-to-string strings ""))

(grid-calc-defun LEN (string)
  "Returns the length of a string."
  (length (grid-calc-to-string string)))

(grid-calc-defun TRIM (text)
  "Removes leading and trailing whitespace from a text string."
  (string-trim (grid-calc-to-string text)))

(grid-calc-defun LEFT (text &optional num_chars)
  "Returns a specified number of characters from the start of a text string."
  (let* ((str (grid-calc-to-string text))
         (len (length str))
         (num (if num_chars (grid-calc-to-number num_chars) 1)))
    (when (> num len) (setq num len))
    (substring str 0 num)))

(grid-calc-defun RIGHT (text &optional num_chars)
  "Returns a specified number of characters from the end of a text string."
  (let* ((str (grid-calc-to-string text))
         (len (length str))
         (num (if num_chars (grid-calc-to-number num_chars) 1)))
    (when (> num len) (setq num len))
    (substring str (- len num))))

(grid-calc-defun MID (text start_num num_chars)
  "Returns a specific number of characters from a text string,
starting at the position you specify."
  (let* ((str (grid-calc-to-string text))
         (start (1- (grid-calc-to-number start_num))) ; 1-based to 0-based
         (num (grid-calc-to-number num_chars)))
    (substring str start (+ start num))))

(grid-calc-defun FIND (find_text within_text &optional start_num)
  "Finds one text string within a second text string, and returns the
number of the starting position of the first text string. Case-sensitive."
  (let* ((find-str (grid-calc-to-string find_text))
         (within-str (grid-calc-to-string within_text))
         (start (if start_num (1- (grid-calc-to-number start_num)) 0)))
    (let ((pos (string-match (regexp-quote find-str) within-str start)))
      (if pos
          (1+ pos) ; Return 1-based position
        (error "Substring not found in FIND")))))

(grid-calc-defun SEARCH (find_text within_text &optional start_num)
  "Finds one text string within a second text string, and returns the
number of the starting position of the first text string. Not case-sensitive."
  (let* ((find-str (grid-calc-to-string find_text))
         (within-str (grid-calc-to-string within_text))
         (start (if start_num (1- (grid-calc-to-number start_num)) 0)))
    (let ((case-fold-search t) ; The only difference from FIND
          (pos (string-match (regexp-quote find-str) within-str start)))
      (if pos
          (1+ pos) ; Return 1-based position
        (error "Substring not found in SEARCH")))))

(grid-calc-defun SUBSTITUTE (text old_text new_text &optional instance_num)
  "Substitutes new_text for old_text in a text string.
NOTE: instance_num is not yet supported; all instances are replaced."
  (when instance_num
    (message "SUBSTITUTE's instance_num argument is not yet supported."))
  (let ((str (grid-calc-to-string text))
        (old (grid-calc-to-string old_text))
        (new (grid-calc-to-string new_text)))
    (replace-regexp-in-string (regexp-quote old) new str t t)))

(grid-calc-defun LEN (string)
  "Returns the length of a string."
  (length (grid-calc-to-string string)))

;; Date/Time functions
(grid-calc-defun NOW ()
  "Returns the current time."
  (current-time))

(grid-calc-defun TODAY ()
  "Returns today's date as a string."
  (format-time-string "%Y-%m-%d"))

(grid-calc-defun DATE (year month day)
  "Creates a date value from year, month, and day."
  (encode-time 0 0 0 (grid-calc-to-number day) (grid-calc-to-number month) (grid-calc-to-number year)))

(grid-calc-defun YEAR (date)
  "Returns the year from a date value."
  (nth 5 (decode-time (grid-calc--to-time date))))

(grid-calc-defun MONTH (date)
  "Returns the month from a date value."
  (nth 4 (decode-time (grid-calc--to-time date))))

(grid-calc-defun DAY (date)
  "Returns the day of the month from a date value."
  (nth 3 (decode-time (grid-calc--to-time date))))

(grid-calc-defun WEEKDAY (date &optional return_type)
  "Returns the day of the week corresponding to a date.
return_type 1 (default): Sunday=1 through Saturday=7.
return_type 2: Monday=1 through Sunday=7."
  (let* ((time (grid-calc--to-time date))
         (dow (nth 6 (decode-time time))) ; Emacs DOW: Sun=0, Mon=1, .. Sat=6
         (type (if return_type (grid-calc-to-number return_type) 1)))
    (pcase type
      (1 (1+ dow))
      (2 (if (= dow 0) 7 dow))
      (_ (error "Unsupported WEEKDAY return_type: %s" type)))))

(grid-calc-defun EOMONTH (start_date months)
  "Returns the serial number of the last day of the month before or
after a specified number of months."
  (let* ((time (grid-calc--to-time start_date))
         (months-offset (grid-calc-to-number months))
         (decoded (decode-time time))
         (year (nth 5 decoded))
         (month (nth 4 decoded))
         ;; Calculate the target month and year
         (total-month (+ month months-offset 1)) ; Go to the start of the *next* month
         (target-year (+ year (floor (1- total-month) 12)))
         (target-month (1+ (mod (1- total-month) 12)))
         ;; Get the first day of the month *after* our target month
         (first-of-next-month (encode-time 0 0 0 1 target-month target-year)))
    ;; Subtract a few seconds to get the last moment of the previous day
    (time-subtract first-of-next-month (seconds-to-time 1))))

;; Lookup functions (Placeholder)
(grid-calc-defun VLOOKUP (lookup-value table-range col-index exact-match)
  "Vertical lookup function (Placeholder)."
  (error "VLOOKUP not yet implemented."))

(provide 'grid-table-calc)
