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
    ;; Elisp code execution
    (`(ELISP-CODE ,code-str)
     ;; only if user opted-in to elisp evaluation
     (if grid-table-allow-elisp
         (let ((cell (lambda (ref) (grid-calc-get-cell-value model ref))))
           (condition-case err
               (eval (read code-str))
             (error (format "Elisp Error: %s" (error-message-string err)))))
       (error "Elisp disabled: see `grid-calc-allow-elisp'")))

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
     ((string-match "^\\$\\([A-Z]+\\)\\$\\([0-9]+\\)$" ref-str) ; Excel row 1 is A,B,C header, Excel row 2 is user-defined header (data row 0)
      (list :col (grid-calc-col-letter-to-number (match-string 1 ref-str))
            :row (- (string-to-number (match-string 2 ref-str)) 2) ; Adjust for A,B,C header row and 0-based data model
            :absolute-col t :absolute-row t))
     ;; Mixed reference $A1 or A$1
     ((string-match "^\\(\\$?\\)\\([A-Z]+\\)\\(\\$?\\)\\([0-9]+\\)$" ref-str)
      (list :col (grid-calc-col-letter-to-number (match-string 2 ref-str))
            :row (- (string-to-number (match-string 4 ref-str)) 2) ; Adjust for A,B,C header row and 0-based data model
            :absolute-col (not (string-empty-p (match-string 1 ref-str)))
            :absolute-row (not (string-empty-p (match-string 3 ref-str)))))
   ;; Single cell reference (e.g., A1)
   ;; Excel row 1 is A,B,C header. Excel row 2 is the first data row (user-defined headers).
   ;; So, Excel row N maps to our data model index (N-2).
   ((string-match "^\\([A-Z]+\\)\\([0-9]+\\)$" ref-str)
    (let* ((col-str (match-string 1 ref-str))
           (row-str (match-string 2 ref-str))
           (row-num (string-to-number row-str))
           (model-row (- row-num 2)))
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
      (let ((start-row (plist-get start-ref :row)) (start-col (plist-get start-ref :col))
            (end-row (plist-get end-ref :row)) (end-col (plist-get end-ref :col)))
        (dotimes (r (1+ (- end-row start-row)))
          (let ((current-row (+ start-row r)))
            (dotimes (c (1+ (- end-col start-col)))
              (let ((current-col (+ start-col c)))
                (when (and (>= current-row 0) (>= current-col 0)
                           (< current-row (grid-data-model-rows model))
                           (< current-col (grid-data-model-cols model)))
                  (push (grid-model-get-computed-value model current-row current-col) values)))))))
    (nreverse values))))

;;;----------------------------------------------------------------------
;;; Built-in Function Library
;;;----------------------------------------------------------------------

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

(defvar grid-calc-functions (make-hash-table :test 'equal))

(defmacro grid-calc-defun (name args &rest body)
  "Defines a built-in calculation function and registers it."
  `(puthash ,(symbol-name name)
            (lambda (model . ,args) ; All functions receive model as first arg
              ,@body)
            grid-calc-functions))

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
(grid-calc-defun CONCATENATE (&rest strings)
  "Concatenates strings."
  (mapconcat #'grid-calc-to-string strings ""))

(grid-calc-defun LEN (string)
  "Returns the length of a string."
  (length (grid-calc-to-string string)))

;; Date/Time functions
(grid-calc-defun NOW ()
  "Returns the current time."
  (current-time))

(grid-calc-defun TODAY ()
  "Returns today's date."
  (format-time-string "%Y-%m-%d"))

;; Lookup functions (Placeholder)
(grid-calc-defun VLOOKUP (lookup-value table-range col-index exact-match)
  "Vertical lookup function (Placeholder)."
  (error "VLOOKUP not yet implemented."))

(provide 'grid-table-calc)
