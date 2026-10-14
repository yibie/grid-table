;;; grid-table-calc.el --- AST evaluator and calculation engine -*- lexical-binding: t -*-

(require 'grid-table-parser)
(require 'grid-table-format)

(declare-function grid-data-model-styles "grid-data-model")
(declare-function grid-data-model-cols "grid-data-model")

;;;----------------------------------------------------------------------
;;; Main Calculation Entry Point
;;;----------------------------------------------------------------------

(define-error 'grid-calc-error "Formula error")
(define-error 'grid-calc-cycle "Circular reference" 'grid-calc-error)

(defconst grid-calc-error-values
  '("#DIV/0!" "#NAME?" "#N/A" "#REF!" "#VALUE!" "#NUM!" "#CYCLE!" "#ERROR!")
  "Values a cell shows when its formula cannot be computed.")

(defvar grid-calc--pass nil
  "State of the recalculation in progress, or nil.
A hash table from each model the recalculation has reached to a vector
with one entry per cell: nil when the cell has not been computed yet,
`visiting' while its formula is being evaluated and `done' afterwards.
One table serves all the sheets of a workbook, so that a formula reading
another sheet has that cell computed first and a cycle through several
sheets is found.")

(defvar grid-calc-sheets nil
  "Function that returns the model of the sheet with a given name, or nil.
Bound by whoever keeps several sheets together; without it a reference
into another sheet is #REF!.")

(defvar grid-calc-current-sheet nil
  "Name of the sheet whose model is being changed, when it has one.")

(defvar grid-calc-defer nil
  "Non-nil makes `grid-table-calc-evaluate' do nothing.
Whoever binds it recalculates afterwards, all sheets together.")

(defun grid-calc--pass-of (model)
  "Return the state vector of MODEL in the recalculation in progress."
  (or (gethash model grid-calc--pass)
      (puthash model (make-vector (length (grid-data-model-cells model)) nil)
               grid-calc--pass)))

(defun grid-calc--sheet-model (model sheet)
  "Return the model a reference into SHEET reads; MODEL when SHEET is nil.
A sheet that does not exist makes the reference #REF!."
  (cond ((null sheet) model)
        ((and grid-calc-sheets (funcall grid-calc-sheets sheet)))
        (t (signal 'grid-calc-error '("#REF!")))))

(defvar grid-calc--depth 0
  "How many formulas are being evaluated on top of each other.")

(defun grid-calc--error-value (err)
  "Return the value shown for a formula that signaled ERR."
  (let ((message (error-message-string err)))
    (cond ((eq (car err) 'grid-calc-cycle) "#CYCLE!")
          ((and (eq (car err) 'grid-calc-error)
                (member (cadr err) grid-calc-error-values))
           (cadr err))
          ((or (eq (car err) 'arith-error)
               (string-match-p "Division by zero" message))
           "#DIV/0!")
          ((string-match-p "Unknown function" message) "#NAME?")
          ((string-match-p "No match\\|not available\\|No condition" message)
           "#N/A")
          (t "#ERROR!"))))

(defun grid-calc--typed-value (model index raw)
  "Return the value of the cell at INDEX of MODEL that holds typed RAW.
Text typed after an apostrophe, and anything in a cell with the text
format, is text even when it reads as a number."
  (cond ((string-empty-p raw) raw)
        ((eq (aref raw 0) ?')
         (propertize (substring raw 1) 'grid-text t))
        ((and (let ((styles (grid-data-model-styles model)))
                (and styles (> (hash-table-count styles) 0)
                     (let ((cols (grid-data-model-cols model)))
                       (eq (car-safe
                            (plist-get (gethash (cons (/ index cols)
                                                      (% index cols))
                                                styles)
                                       :format))
                           'text)))))
         (propertize raw 'grid-text t))
        (t raw)))

(defun grid-calc--compute-cell (model index)
  "Compute the cell at INDEX of MODEL unless this pass already did.
A formula that fails gives its cell one of `grid-calc-error-values'
instead of stopping the recalculation."
  (let ((cell (aref (grid-data-model-cells model) index))
        (pass (grid-calc--pass-of model)))
    (pcase (aref pass index)
      ('done nil)
      ('visiting (signal 'grid-calc-cycle nil))
      (_
       (aset pass index 'visiting)
       (let ((raw (grid-cell-raw-input cell))
             cycle)
         (set-grid-cell-computed-value
          cell
          (if (and (> (length raw) 0) (eq (aref raw 0) ?=))
              (condition-case err
                  (let ((grid-calc--depth (1+ grid-calc--depth)))
                    (grid-calc-eval-ast
                     model (grid-parser-parse (substring raw 1))))
                (grid-calc-cycle (setq cycle t) "#CYCLE!")
                (error (grid-calc--error-value err)))
            (grid-calc--typed-value model index raw)))
         (aset pass index 'done)
         ;; Every formula on a cycle is part of it, not only the one that
         ;; closed it.
         (when (and cycle (> grid-calc--depth 0))
           (signal 'grid-calc-cycle nil)))))))

(defun grid-calc--value (model row col)
  "Return the computed value at ROW and COL of MODEL for a formula.
During a recalculation the cell is computed first, so a formula never
reads a stale value.  An error value is signaled on to the reader."
  (when grid-calc--pass
    (grid-calc--compute-cell model (+ (* row (grid-data-model-cols model)) col)))
  (let ((value (grid-model-get-computed-value model row col)))
    (if (and (stringp value) (member value grid-calc-error-values))
        (signal 'grid-calc-error (list value))
      value)))

(defun grid-table-calc-evaluate-models (models)
  "Evaluate all formulas of MODELS, the sheets of one workbook.
Every cell is computed once; a formula computes the cells it reads
before itself, whatever their position and whichever sheet they are in."
  (let ((grid-calc--pass (make-hash-table :test 'eq))
        (grid-calc--depth 0))
    (dolist (model models)
      (dotimes (i (length (grid-data-model-cells model)))
        (grid-calc--compute-cell model i)))))

(defun grid-table-calc-evaluate (model)
  "Main entry point for evaluating all formulas in the model.
Every cell is computed once; a formula computes the cells it reads
before itself, whatever their position in the sheet."
  (unless grid-calc-defer
    (grid-table-calc-evaluate-models (list model))))

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
    (`(FUNCTION-CALL "XLOOKUP" ,args)
     (grid-calc-eval-xlookup-function model args))
    (`(FUNCTION-CALL "IF" ,args)
     (grid-calc-eval-if-function model args))

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

    ;; TRUE and FALSE
    (`(LITERAL ,value) value)
    
    ;; String literal
    (`(STRING ,value) value)
    
    ;; Cell reference
    (`(CELL-REF ,ref-str)
     (grid-calc-get-cell-value model ref-str))
    (`(CELL-REF ,ref-str ,sheet)
     (grid-calc-get-cell-value (grid-calc--sheet-model model sheet) ref-str))
    
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
    ('+ (grid-calc--tidy-number
         (+ (grid-calc--operand left) (grid-calc--operand right))))
    ('- (grid-calc--tidy-number
         (- (grid-calc--operand left) (grid-calc--operand right))))
    ('* (grid-calc--tidy-number
         (* (grid-calc--operand left) (grid-calc--operand right))))
    ('/ (let ((r (grid-calc--operand right)))
          (if (= r 0)
              (error "Division by zero")
            (grid-calc--tidy-number (/ (float (grid-calc--operand left)) r)))))
    ('^ (expt (grid-calc--operand left) (grid-calc--operand right)))
    
    ;; Comparison operations
    ('= (grid-calc--equal left right))
    ('<> (not (grid-calc--equal left right)))
    ('< (< (grid-calc-to-number left) (grid-calc-to-number right)))
    ('> (> (grid-calc-to-number left) (grid-calc-to-number right)))
    ('<= (<= (grid-calc-to-number left) (grid-calc-to-number right)))
    ('>= (>= (grid-calc-to-number left) (grid-calc-to-number right)))
    
    ;; Logical operations
    ('AND (and left right))
    ('OR (or left right))
    
    ;; String concatenation
    ('& (concat (grid-calc-to-string left) (grid-calc-to-string right)))
    
    (_ (error "Unknown binary operator: %s" op))))

(defun grid-calc-apply-unary-op (op operand)
  "Applies a unary operator."
  (pcase op
    ('+ (grid-calc--operand operand))
    ('- (- (grid-calc--operand operand)))
    ('NOT (not operand))
    ('% (grid-calc--tidy-number (/ (grid-calc--operand operand) 100.0)))
    (_ (error "Unknown unary operator: %s" op))))

;;;----------------------------------------------------------------------
;;; Type Conversion and Utility Functions
;;;----------------------------------------------------------------------

(defun grid-calc--tidy-number (number)
  "Return NUMBER as an integer when it has no fraction."
  (if (and (floatp number) (= number (ftruncate number))
           (< (abs number) 1e15))
      (truncate number)
    number))

(defun grid-calc--numeric (value)
  "Return VALUE as a number when it is one or reads as one, else nil.
Text marked as text is not a number, whatever it reads as."
  (grid-format-number value))

(defun grid-calc--equal (left right)
  "Return non-nil when LEFT and RIGHT are the same spreadsheet value.
Numbers compare by value whether typed or computed; text ignores case."
  (let ((left-number (grid-calc--numeric left))
        (right-number (grid-calc--numeric right)))
    (cond ((and left-number right-number) (= left-number right-number))
          ((and (stringp left) (stringp right))
           (string= (downcase left) (downcase right)))
          (t (equal left right)))))

(defun grid-calc-to-number (value)
  "Converts a value to a number.
Returns 0 for non-numeric values instead of throwing an error."
  (cond
   ((numberp value) value)
   ((stringp value)
    (cond ((string-empty-p value) 0)
          ;; Arithmetic reads a number out of text; only aggregates skip it.
          ((grid-format-text-p value)
           (or (grid-format-number (substring-no-properties value)) 0))
          (t (or (grid-format-number value) 0))))
   ((null value) 0)
   (t 0))) ; Return 0 for any other type instead of throwing error

(defun grid-calc--operand (value)
  "Return VALUE as the number that arithmetic works on.
An empty cell counts as 0 and TRUE as 1.  Text that reads as a number,
or as a date written year first, is that number.  Any other text is no
number: the formula is #VALUE! rather than quietly taking it for 0."
  (cond
   ((numberp value) value)
   ((null value) 0)
   ((eq value t) 1)
   ((stringp value)
    (let ((text (substring-no-properties value)))
      (cond ((string-empty-p text) 0)
            ((grid-format-number text))
            ((grid-format-parse-date text))
            (t (signal 'grid-calc-error '("#VALUE!"))))))
   (t (signal 'grid-calc-error '("#VALUE!")))))

(defun grid-calc--to-serial (value)
  "Return the date VALUE as a serial number.
VALUE is a serial number, or a date written year first as old sheets
hold them."
  (cond
   ((numberp value) value)
   ((and (stringp value) (grid-calc--numeric value)))
   ((and (stringp value) (grid-format-parse-date value)))
   ((and (stringp value)
         (let ((time (ignore-errors (parse-time-string value))))
           (and (nth 5 time) (nth 4 time) (nth 3 time)
                (grid-format-date-to-serial (nth 5 time) (nth 4 time)
                                            (nth 3 time))))))
   (t (error "Value cannot be read as a date: %s" value))))

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
  "Converts a value to a string, a number as the general format shows it."
  (if (stringp value) value (grid-format-general value)))

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
      (list :col (grid-calc-col-letter-to-number col-str)
            :row model-row
            :absolute-col nil :absolute-row nil)))
   ;; Fallback for debugging
   (t nil)))

(defun grid-calc-col-letter-to-number (letters)
  "Converts column letters (e.g., A, B, AA) to a 0-based number."
  (let ((result 0))
    (dolist (char (string-to-list (upcase letters)))
      (setq result (+ (* result 26) (1+ (- char ?A)))))
    (1- result)))

(defun grid-calc-col-number-to-letter (col)
  "Convert 0-based COL to column letters (0 is A, 26 is AA)."
  (let ((result ""))
    (while (>= col 0)
      (setq result (concat (char-to-string (+ ?A (mod col 26))) result)
            col (1- (/ col 26))))
    result))

(defconst grid-calc--reference-regexp
  "\\(\\$?\\)\\([A-Za-z]+\\)\\(\\$?\\)\\([0-9]+\\)"
  "Matches a cell reference: column anchor, letters, row anchor, number.")

(defun grid-calc--reference-at (formula pos &optional _continued)
  "Return the reference that starts at POS in FORMULA, or nil.
The value is (END COL-ANCHOR COL ROW-ANCHOR ROW), with COL zero-based and
ROW the number written.  The tail of a name and the name of a function
are not references."
  (let ((len (length formula)))
    (when (and (eq (string-match grid-calc--reference-regexp formula pos) pos)
               (not (and (> pos 0)
                         (string-match-p "[A-Za-z0-9_.$]"
                                         (substring formula (1- pos) pos))))
               (not (and (< (match-end 0) len)
                         (string-match-p "[A-Za-z0-9_.(!]"
                                         (substring formula (match-end 0)
                                                    (1+ (match-end 0)))))))
      (list (match-end 0)
            (match-string 1 formula)
            (grid-calc-col-letter-to-number (match-string 2 formula))
            (match-string 3 formula)
            (string-to-number (match-string 4 formula))))))

(defun grid-calc--whole-range-at (formula pos)
  "Return the whole columns or whole rows that start at POS in FORMULA.
The value is (END START-REFERENCE END-REFERENCE) for text such as A:C or
$2:5, each reference a (COL-ANCHOR COL ROW-ANCHOR ROW) list whose ROW is
nil for a column and whose COL is nil for a row.  Return nil when no
such range starts there."
  (let ((len (length formula))
        (case-fold-search t))
    (when (and (not (and (> pos 0)
                         (string-match-p "[A-Za-z0-9_.$:]"
                                         (substring formula (1- pos) pos))))
               (or (eq (string-match
                        "\\(\\$?\\)\\([A-Za-z]+\\):\\(\\$?\\)\\([A-Za-z]+\\)"
                        formula pos)
                       pos)
                   (eq (string-match
                        "\\(\\$?\\)\\([0-9]+\\):\\(\\$?\\)\\([0-9]+\\)"
                        formula pos)
                       pos))
               (not (and (< (match-end 0) len)
                         (string-match-p "[A-Za-z0-9_.(!:$]"
                                         (substring formula (match-end 0)
                                                    (1+ (match-end 0)))))))
      (let ((end (match-end 0))
            (make (lambda (anchor text)
                    (if (string-match-p "\\`[0-9]" text)
                        (list "" nil anchor (string-to-number text))
                      (list anchor (grid-calc-col-letter-to-number text)
                            "" nil)))))
        (list end
              (funcall make (match-string 1 formula) (match-string 2 formula))
              (funcall make (match-string 3 formula)
                       (match-string 4 formula)))))))

(defun grid-calc--reference-text (reference)
  "Return the text of REFERENCE, a (COL-ANCHOR COL ROW-ANCHOR ROW) list.
ROW is nil for a whole column and COL is nil for a whole row.  A
reference off the sheet's top or left edge is #REF!."
  (pcase-let ((`(,col-anchor ,col ,row-anchor ,row) reference))
    (if (or (and col (< col 0)) (and row (< row 1)))
        "#REF!"
      (concat (and col col-anchor) (and col (grid-calc-col-number-to-letter col))
              (and row row-anchor) (and row (number-to-string row))))))

(defun grid-calc-sheet-prefix (name)
  "Return NAME as the sheet prefix of a reference, exclamation mark included.
A name that is not plain letters, digits and underscores is quoted."
  (if (string-match-p "\\`[[:alpha:]_][[:alnum:]_.]*\\'" name)
      (concat name "!")
    (concat "'" (string-replace "'" "''" name) "'!")))

(defun grid-calc--same-sheet-p (left right)
  "Return non-nil when LEFT and RIGHT name the same sheet."
  (and left right (string= (downcase left) (downcase right))))

(defun grid-calc--map-references (formula function &optional rename)
  "Return FORMULA with each reference replaced by what FUNCTION returns.
FUNCTION is called with the two ends of a range, each a (COL-ANCHOR COL
ROW-ANCHOR ROW) list, or with one such list and nil for a lone
reference, and with the name of the sheet the reference is written
with, or nil.  It returns the replacement text, without the sheet.
RENAME, when given, is called with that sheet name and returns the name
to write instead, or nil to keep it.  Text inside double quotes and
anything that is not a formula are returned unchanged."
  (if (or (not (stringp formula))
          (not (string-prefix-p "=" formula))
          (string-prefix-p "=elisp:" formula))
      formula
    (let ((pos 0) (len (length formula)) (parts nil) (case-fold-search t))
      (while (< pos len)
        (let* ((prefix (and (not (eq (aref formula pos) ?\"))
                            (grid-parser-sheet-prefix-at formula pos)))
               (start (if prefix (car prefix) pos))
               (whole (and (< start len)
                           (not (eq (aref formula pos) ?\"))
                           (grid-calc--whole-range-at formula start)))
               (reference (and (< start len) (not whole)
                               (grid-calc--reference-at formula start prefix))))
          (cond
           ;; Quoted text
           ((eq (aref formula pos) ?\")
            (let ((end (or (string-search "\"" formula (1+ pos)) (1- len))))
              (push (substring formula pos (1+ end)) parts)
              (setq pos (1+ end))))
           ((or whole reference)
            (let* ((end (car (or whole reference)))
                   (other (and reference (< end len) (eq (aref formula end) ?:)
                               (grid-calc--reference-at formula (1+ end) t)))
                   (sheet (cdr prefix))
                   (renamed (and sheet rename (funcall rename sheet))))
              (when prefix
                (push (if renamed
                          (grid-calc-sheet-prefix renamed)
                        (substring formula pos start))
                      parts))
              (push (if whole
                        (funcall function (nth 1 whole) (nth 2 whole) sheet)
                      (funcall function (cdr reference) (cdr other) sheet))
                    parts)
              (setq pos (cond (whole end) (other (car other)) (t end)))))
           ;; A sheet name in front of something that is no reference
           (prefix
            (push (substring formula pos start) parts)
            (setq pos start))
           ;; A name or number: skip it whole so its tail is not read as a
           ;; reference
           ((eq (string-match "[A-Za-z0-9_.$]+" formula pos) pos)
            (push (match-string 0 formula) parts)
            (setq pos (match-end 0)))
           (t
            (push (substring formula pos (1+ pos)) parts)
            (setq pos (1+ pos))))))
      (apply #'concat (nreverse parts)))))

(defun grid-calc--range-text (start end function)
  "Return the text of a reference with each end passed through FUNCTION.
START and END are as `grid-calc--map-references' passes them."
  (if end
      (concat (funcall function start) ":" (funcall function end))
    (funcall function start)))

(defun grid-calc-shift-formula (formula rows cols &optional local-only)
  "Return FORMULA with its relative references moved ROWS down and COLS right.
Parts of a reference anchored with $ stay.  A reference pushed off the
sheet's top or left edge becomes #REF!.  Text inside double quotes and
anything that is not a formula are returned unchanged.  With LOCAL-ONLY,
references that name a sheet stay as they are; when LOCAL-ONLY is the
name of the formula's own sheet, references that name that sheet move
like the ones that name none."
  (let ((shift (lambda (reference)
                 (pcase-let ((`(,col-anchor ,col ,row-anchor ,row) reference))
                   (grid-calc--reference-text
                    (list col-anchor
                          (if (and col (string-empty-p col-anchor))
                              (+ col cols)
                            col)
                          row-anchor
                          (if (and row (string-empty-p row-anchor))
                              (+ row rows)
                            row)))))))
    (grid-calc--map-references
     formula
     (lambda (start end sheet)
       (grid-calc--range-text
        start end
        (if (and sheet local-only
                 (not (and (stringp local-only)
                           (grid-calc--same-sheet-p sheet local-only))))
            #'grid-calc--reference-text
          shift))))))

(defun grid-calc-rename-sheet-in-formula (formula old new)
  "Return FORMULA with its references into sheet OLD naming NEW instead."
  (grid-calc--map-references
   formula
   (lambda (start end _sheet)
     (grid-calc--range-text start end #'grid-calc--reference-text))
   (lambda (sheet)
     (and (grid-calc--same-sheet-p sheet old) new))))

(defun grid-calc-restructure-formula (formula axis at delta
                                              &optional home target)
  "Return FORMULA rewritten for a row or column inserted or deleted.
AXIS is `row' or `col' and AT the zero-based index of the line inserted
\(DELTA 1) or deleted (DELTA -1).  A reference keeps pointing at the cell
it pointed at, anchored or not: one at or past an inserted line moves
on, one past a deleted line moves back.  A reference to a deleted cell
becomes #REF!; a range that loses a line shrinks, and becomes #REF! only
when nothing of it is left.

HOME names the sheet FORMULA is in and TARGET the sheet that changed;
TARGET defaults to HOME.  Only references into the changed sheet are
rewritten: those written without a sheet when the formula lives there,
and those that name it."
  (let* ((target (or target home))
         (local (or (null target) (grid-calc--same-sheet-p home target)))
         ;; nil for whole columns when rows change, and the other way round
         (index (lambda (reference)
                  (if (eq axis 'row)
                      (and (nth 3 reference) (1- (nth 3 reference)))
                    (nth 1 reference))))
         (moved (lambda (reference to)
                  (grid-calc--reference-text
                   (if (eq axis 'row)
                       (list (nth 0 reference) (nth 1 reference)
                             (nth 2 reference) (1+ to))
                     (list (nth 0 reference) to
                           (nth 2 reference) (nth 3 reference)))))))
    (grid-calc--map-references
     formula
     (lambda (start end sheet)
       (let ((from (funcall index start))
             (to (and end (funcall index end))))
         (cond
          ((or (null from)
               (not (if sheet (grid-calc--same-sheet-p sheet target) local)))
           (grid-calc--range-text start end #'grid-calc--reference-text))
          ((> delta 0)
           (let ((new-from (if (>= from at) (1+ from) from)))
             (if end
                 (concat (funcall moved start new-from) ":"
                         (funcall moved end (if (>= to at) (1+ to) to)))
               (funcall moved start new-from))))
          ((not end)
           (cond ((= from at) "#REF!")
                 (t (funcall moved start (if (> from at) (1- from) from)))))
          (t
           ;; Keep the ends in the order they were written.
           (let* ((low (min from to)) (high (max from to))
                  (new-low (if (> low at) (1- low) low))
                  (new-high (if (>= high at) (1- high) high)))
             (if (< new-high new-low)
                 "#REF!"
               (concat (funcall moved start (if (<= from to) new-low new-high))
                       ":"
                       (funcall moved end (if (<= from to) new-high new-low))))))))))))

(defun grid-calc-get-cell-value (model ref-str)
  "Retrieves the computed value of a cell from the model."
  (let* ((ref (grid-calc-parse-cell-ref ref-str))
         (row (plist-get ref :row))
         (col (plist-get ref :col)))
    (when (and ref (>= row 0) (>= col 0))
      (when (and (< row (grid-data-model-rows model))
                 (< col (grid-data-model-cols model)))
        (grid-calc--value model row col)))))

(defun grid-calc--reference-coords (ref-str)
  "Return (ROW . COL) for REF-STR, zero-based.
ROW is nil for a whole column such as A, COL is nil for a whole row such
as 2.  Return nil when REF-STR is no reference."
  (when (and (string-match "\\`\\$?\\([A-Za-z]+\\)?\\$?\\([0-9]+\\)?\\'" ref-str)
             (or (match-beginning 1) (match-beginning 2)))
    (cons (and (match-beginning 2)
               (1- (string-to-number (match-string 2 ref-str))))
          (and (match-beginning 1)
               (grid-calc-col-letter-to-number (match-string 1 ref-str))))))

(defun grid-calc--range-bounds (model start-cell-ref-ast end-cell-ref-ast)
  "Return (MODEL ROW-START ROW-END COL-START COL-END) for a range.
MODEL is the sheet the range reads.  Whole columns and whole rows reach
the edges of the sheet, and the bounds are kept inside it; an end before
its start means nothing of the range is on the sheet."
  (let* ((model (grid-calc--sheet-model model (nth 2 start-cell-ref-ast)))
         (start (grid-calc--reference-coords (cadr start-cell-ref-ast)))
         (end (grid-calc--reference-coords (cadr end-cell-ref-ast)))
         (last-row (1- (grid-data-model-rows model)))
         (last-col (1- (grid-data-model-cols model))))
    (unless (and start end)
      (error "Invalid range"))
    (let ((row-a (or (car start) 0)) (row-b (or (car end) last-row))
          (col-a (or (cdr start) 0)) (col-b (or (cdr end) last-col)))
      (list model
            (max 0 (min row-a row-b)) (min last-row (max row-a row-b))
            (max 0 (min col-a col-b)) (min last-col (max col-a col-b))))))

(defun grid-calc-get-range-values (model start-cell-ref-ast end-cell-ref-ast)
  "Retrieves a flat list of computed values from a range."
  (pcase-let ((`(,model ,row-start ,row-end ,col-start ,col-end)
               (grid-calc--range-bounds model start-cell-ref-ast
                                        end-cell-ref-ast))
              (values '()))
    (cl-loop for r from row-start to row-end do
             (cl-loop for c from col-start to col-end do
                      (push (grid-calc--value model r c) values)))
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
    (let* ((bounds (grid-calc--range-bounds model (cadr range-ast)
                                            (caddr range-ast)))
           (start-row (nth 1 bounds))
           (start-col (nth 3 bounds))
           (height (1+ (- (nth 2 bounds) start-row)))
           (width (1+ (- (nth 4 bounds) start-col)))
           (first (grid-calc-to-number
                   (grid-calc-eval-ast model row-num-ast)))
           ;; One number alone counts along a range that is a single row.
           (along-row (and (null col-num-ast) (= height 1) (> width 1)))
           (row-num (if along-row 1 first))
           (col-num (cond (along-row first)
                          (col-num-ast
                           (grid-calc-to-number
                            (grid-calc-eval-ast model col-num-ast)))
                          (t 1))))
      ;; A position outside the range is no cell of it.
      (unless (and (integerp row-num) (integerp col-num)
                   (<= 1 row-num height) (<= 1 col-num width))
        (signal 'grid-calc-error '("#REF!")))
      (grid-calc--value (car bounds)
                        (+ start-row row-num -1) (+ start-col col-num -1)))))

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
  (cl-block grid-calc-eval-ifs-function
   (let ((remaining-args args-ast-list))
    (while remaining-args
      (let* ((test-ast (car remaining-args))
             (value-ast (cadr remaining-args))
             (test-result (grid-calc-eval-ast model test-ast)))
        (if (grid-calc-to-boolean test-result)
            (cl-return-from grid-calc-eval-ifs-function
              (grid-calc-eval-ast model value-ast)))
        (setq remaining-args (cddr remaining-args)))))
   (error "No condition in IFS was met"))) ; Return #N/A error

(defun grid-calc-eval-vlookup-function (model args-ast-list)
  "Special evaluator for the VLOOKUP function."
  (when (< (length args-ast-list) 3)
    (error "VLOOKUP requires at least 3 arguments"))
  (cl-block grid-calc-eval-vlookup-function
   (let* ((lookup-value (grid-calc-eval-ast model (nth 0 args-ast-list)))
         (table-array-ast (nth 1 args-ast-list))
         (col-index-num (grid-calc-eval-ast model (nth 2 args-ast-list)))
         ;; Without a fourth argument, or with TRUE, the first column is
         ;; taken as sorted and the last row not above the value is found.
         (approximate (or (null (nth 3 args-ast-list))
                          (grid-calc-to-boolean
                           (grid-calc-eval-ast model (nth 3 args-ast-list))))))
    (unless (eq (car table-array-ast) 'RANGE)
      (error "Second argument to VLOOKUP must be a range"))

    (let* ((bounds (grid-calc--range-bounds model (cadr table-array-ast)
                                            (caddr table-array-ast)))
           (start-row (nth 1 bounds))
           (end-row (nth 2 bounds))
           (start-col (nth 3 bounds))
           (target-col (+ start-col col-index-num -1))
           (model (car bounds)))

      (unless (and (integerp col-index-num)
                   (<= start-col target-col (nth 4 bounds)))
        (signal 'grid-calc-error '("#REF!")))
      (if approximate
          (let ((wanted (grid-calc--numeric lookup-value))
                found)
            (cl-loop
             for r from start-row to end-row
             do (let* ((cell-val (grid-calc--value model r start-col))
                       (number (grid-calc--numeric cell-val)))
                  (cond
                   ;; Numbers are looked up among numbers, text among text.
                   ((and wanted number)
                    (if (<= number wanted) (setq found r) (cl-return)))
                   ((and (not wanted) (not number)
                         (stringp lookup-value) (stringp cell-val)
                         (not (string-empty-p cell-val)))
                    (if (string-greaterp (downcase cell-val)
                                         (downcase lookup-value))
                        (cl-return)
                      (setq found r))))))
            (when found
              (cl-return-from grid-calc-eval-vlookup-function
                (grid-calc--value model found target-col))))
        (cl-loop for r from start-row to end-row
                 do (let ((cell-val (grid-calc--value model r start-col)))
                      (when (grid-calc--equal cell-val lookup-value)
                        (cl-return-from grid-calc-eval-vlookup-function
                          (grid-calc--value model r target-col))))))

      ;; If loop finishes, no match was found
      (error "No match found in VLOOKUP")))))

(defun grid-calc-eval-if-function (model args-ast-list)
  "Special evaluator for the IF function.
Only the branch the condition picks is evaluated, so an error in the
other one does not matter.  Without a third argument a false condition
gives FALSE."
  (unless (memq (length args-ast-list) '(2 3))
    (error "IF requires 2 or 3 arguments"))
  (if (grid-calc-to-boolean (grid-calc-eval-ast model (nth 0 args-ast-list)))
      (grid-calc-eval-ast model (nth 1 args-ast-list))
    (and (nth 2 args-ast-list)
         (grid-calc-eval-ast model (nth 2 args-ast-list)))))

(defun grid-calc-eval-xlookup-function (model args-ast-list)
  "Special evaluator for the XLOOKUP function.
The arguments are the value to look up, the range to look in, the range
to return from and, optionally, what to give when nothing matches, which
is only evaluated then.  Only exact matches are found."
  (unless (<= 3 (length args-ast-list) 6)
    (error "XLOOKUP requires 3 to 6 arguments"))
  (let* ((lookup-value (grid-calc-eval-ast model (nth 0 args-ast-list)))
         (lookup-array (grid-calc-eval-ast model (nth 1 args-ast-list)))
         (return-array (grid-calc-eval-ast model (nth 2 args-ast-list)))
         (match-mode (and (nth 4 args-ast-list)
                          (grid-calc-eval-ast model (nth 4 args-ast-list))))
         (search-mode (and (nth 5 args-ast-list)
                           (grid-calc-eval-ast model (nth 5 args-ast-list)))))
    (unless (listp lookup-array) (setq lookup-array (list lookup-array)))
    (unless (listp return-array) (setq return-array (list return-array)))
    (unless (member match-mode '(nil 0))
      (error "XLOOKUP only supports exact match (match mode 0) at this time"))
    (unless (member search-mode '(nil 1 -1))
      (error "XLOOKUP only searches from the first or the last item"))
    (unless (= (length lookup-array) (length return-array))
      (error "XLOOKUP ranges must have the same size"))
    (let ((pos (cl-position lookup-value lookup-array
                            :test #'grid-calc--equal
                            :from-end (eql search-mode -1))))
      (cond (pos (nth pos return-array))
            ((nth 3 args-ast-list)
             (grid-calc-eval-ast model (nth 3 args-ast-list)))
            (t (error "No match found in XLOOKUP"))))))

;;;----------------------------------------------------------------------
;;; Built-in Function Library
;;;----------------------------------------------------------------------

;;; Logical functions
(defun grid-calc--flatten (arguments)
  "Return the values of ARGUMENTS as one list.
Each argument is a value or the list of values of a range."
  (cl-loop for argument in arguments
           if (listp argument) append argument
           else collect argument))

(grid-calc-defun COUNTA (&rest arguments)
  "Counts the values that are not empty among numbers and ranges."
  (cl-count-if (lambda (v) (not (or (null v) (equal v ""))))
               (grid-calc--flatten arguments)))

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
  (let* ((num (grid-calc--operand number))
         (digits (grid-calc--operand num_digits))
         (p (expt 10.0 digits)))
    (grid-calc--tidy-number (/ (funcall round-fn (* num p)) p))))

(grid-calc-defun ROUND (number num_digits)
  "Rounds a number to a specified number of digits."
  (grid-calc--round-by #'grid-format-round number num_digits))

(grid-calc-defun ROUNDUP (number num_digits)
  "Rounds a number up, away from zero."
  (let ((num (grid-calc--operand number)))
    (if (>= num 0)
        (grid-calc--round-by #'ceiling number num_digits)
      (grid-calc--round-by #'floor number num_digits))))

(grid-calc-defun ROUNDDOWN (number num_digits)
  "Rounds a number down, toward zero."
  (let ((num (grid-calc--operand number)))
    (if (>= num 0)
        (grid-calc--round-by #'floor number num_digits)
      (grid-calc--round-by #'ceiling number num_digits))))

(grid-calc-defun INT (number)
  "Rounds a number down to the nearest integer."
  (floor (grid-calc--operand number)))

(grid-calc-defun MOD (number divisor)
  "Returns the remainder after a number is divided by a divisor."
  (mod (grid-calc--operand number) (grid-calc--operand divisor)))

(grid-calc-defun ABS (number)
  "Returns the absolute value of a number."
  (abs (grid-calc--operand number)))

(grid-calc-defun SQRT (number)
  "Returns a positive square root."
  (let ((num (grid-calc--operand number)))
    (if (< num 0)
        (signal 'grid-calc-error '("#NUM!"))
      (grid-calc--tidy-number (sqrt num)))))

(grid-calc-defun POWER (number power)
  "Returns the result of a number raised to a power."
  (expt (grid-calc--operand number) (grid-calc--operand power)))

(defun grid-calc--get-numeric-values (values)
  "From a list of VALUES, return a new list containing only numbers.
Strings are converted to numbers if possible. Non-numeric values are ignored.
Uses grid-calc-to-number for consistent conversion."
  (let ((numbers '()))
    (dolist (v values)
      (let ((num (grid-calc-to-number v)))
        (when (and num (or (numberp v) (grid-calc--numeric v)))
          (push num numbers))))
    (nreverse numbers)))



(defun grid-calc-call-function (model func-name args)
  "Calls a built-in function."
  (let ((func (gethash (upcase func-name) grid-calc-functions)))
    (if func
        (apply func model args) ; Pass model as first arg
      (error "Unknown function: %s" func-name))))

;; Math functions
(defun grid-calc--numbers-among (arguments)
  "Return the numbers among ARGUMENTS, each a value or a range's values."
  (grid-calc--get-numeric-values (grid-calc--flatten arguments)))

(grid-calc-defun SUM (&rest arguments)
  "Sums numbers and ranges. Ignores non-numeric values."
  (apply #'+ (grid-calc--numbers-among arguments)))

(grid-calc-defun AVERAGE (&rest arguments)
  "Averages numbers and ranges. Ignores non-numeric values."
  (let ((numbers (grid-calc--numbers-among arguments)))
    (if (null numbers) 0
      (grid-calc--tidy-number
       (/ (apply #'+ numbers) (float (length numbers)))))))

(grid-calc-defun COUNT (&rest arguments)
  "Counts the numbers among values and ranges."
  (length (grid-calc--numbers-among arguments)))

(grid-calc-defun MAX (&rest arguments)
  "Returns the largest of numbers and ranges. Ignores non-numeric values."
  (let ((numbers (grid-calc--numbers-among arguments)))
    (if (null numbers) 0
      (apply #'max numbers))))

(grid-calc-defun MIN (&rest arguments)
  "Returns the smallest of numbers and ranges. Ignores non-numeric values."
  (let ((numbers (grid-calc--numbers-among arguments)))
    (if (null numbers) 0
      (apply #'min numbers))))

(grid-calc-defun MEDIAN (&rest arguments)
  "Returns the middle of numbers and ranges. Ignores non-numeric values."
  (let* ((numbers (sort (grid-calc--numbers-among arguments) #'<))
         (count (length numbers)))
    (cond ((zerop count) (signal 'grid-calc-error '("#NUM!")))
          ((cl-oddp count) (nth (/ count 2) numbers))
          (t (grid-calc--tidy-number
              (/ (+ (nth (1- (/ count 2)) numbers) (nth (/ count 2) numbers))
                 2.0))))))

(defun grid-calc--nth-of (array k descending)
  "Return the K-th number of ARRAY, counting from the largest if DESCENDING."
  (let ((numbers (sort (grid-calc--numbers-among (list array))
                       (if descending #'> #'<)))
        (k (grid-calc--operand k)))
    (unless (and (integerp k) (<= 1 k (length numbers)))
      (signal 'grid-calc-error '("#NUM!")))
    (nth (1- k) numbers)))

(grid-calc-defun LARGE (array k)
  "Returns the K-th largest number of a range."
  (grid-calc--nth-of array k t))

(grid-calc-defun SMALL (array k)
  "Returns the K-th smallest number of a range."
  (grid-calc--nth-of array k nil))

(grid-calc-defun RANK (number ref &optional order)
  "Returns the rank of a number among the numbers of a range.
The largest is first, or the smallest when ORDER is not 0."
  (let ((number (grid-calc--operand number))
        (numbers (grid-calc--numbers-among (list ref)))
        (ascending (and order (/= (grid-calc--operand order) 0))))
    (unless (cl-member number numbers :test #'=)
      (error "Value not available in RANK"))
    (1+ (cl-count-if (lambda (other)
                       (if ascending (< other number) (> other number)))
                     numbers))))

(grid-calc-defun SUMPRODUCT (&rest arrays)
  "Multiplies ranges of the same size cell by cell and sums the products.
Cells that hold no number count as 0."
  (let* ((lists (mapcar (lambda (array) (if (listp array) array (list array)))
                        arrays))
         (count (length (car lists))))
    (unless (and lists (cl-every (lambda (list) (= (length list) count)) lists))
      (signal 'grid-calc-error '("#VALUE!")))
    (grid-calc--tidy-number
     (cl-loop for index below count
              sum (cl-loop with product = 1
                           for list in lists
                           do (setq product
                                    (* product
                                       (or (grid-calc--numeric (nth index list))
                                           (and (numberp (nth index list))
                                                (nth index list))
                                           0)))
                           finally return product)))))

(grid-calc-defun TRUNC (number &optional num_digits)
  "Cuts a number off at a number of digits, toward zero."
  (grid-calc--round-by #'truncate number num_digits))

(defun grid-calc--to-multiple (round-fn number significance)
  "Round NUMBER to a multiple of SIGNIFICANCE with ROUND-FN."
  (let ((number (grid-calc--operand number))
        (significance (if significance (grid-calc--operand significance) 1)))
    (cond ((zerop number) 0)
          ((zerop significance) (error "Division by zero"))
          (t (grid-calc--tidy-number
              (/ (grid-format-round
                  (* (funcall round-fn (/ (float number) significance))
                     significance 1e10))
                 1e10))))))

(grid-calc-defun FLOOR (number &optional significance)
  "Rounds a number down to a multiple of SIGNIFICANCE."
  (grid-calc--to-multiple #'floor number significance))

(grid-calc-defun CEILING (number &optional significance)
  "Rounds a number up to a multiple of SIGNIFICANCE."
  (grid-calc--to-multiple #'ceiling number significance))

(grid-calc-defun MROUND (number multiple)
  "Rounds a number to the nearest multiple of MULTIPLE."
  (grid-calc--to-multiple #'grid-format-round number multiple))

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
        (cl-dolist (pair parsed-pairs)
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
        (cl-dolist (pair parsed-pairs)
          (let* ((range (car pair))
                 (predicate (cdr pair)))
            (unless (funcall predicate (nth i range))
              (setq all-match nil)
              (cl-return)))) ; Break inner loop
        (when all-match
          (setq total (+ total (grid-calc-to-number (nth i sum_range)))))))
    total))

(grid-calc-defun MATCH (lookup-value lookup-array &optional match-type)
  "Finds the position of a value in a single-row or single-column range.
NOTE: Only exact match (match-type = 0) is currently implemented."
  (setq match-type (or match-type 0))
  (when (/= match-type 0)
    (error "MATCH only supports exact match (match-type 0) at this time"))
  (let ((pos (cl-position lookup-value lookup-array
                          :test #'grid-calc--equal)))
    (if pos
        (1+ pos) ; Return 1-based index
      (error "Value not available in MATCH"))))

;; Logical functions

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

;; Date/Time functions.  A date is a serial number: days since 1899-12-30,
;; the time of day as its fraction.  Give the cell a date format to read it.
(grid-calc-defun NOW ()
  "Returns this moment as a serial number."
  (grid-format-now))

(grid-calc-defun TODAY ()
  "Returns today's date as a serial number."
  (grid-format-today))

(grid-calc-defun DATE (year month day)
  "Returns the serial number of a date; months and days carry over."
  (grid-format-date-to-serial (truncate (grid-calc-to-number year))
                              (truncate (grid-calc-to-number month))
                              (truncate (grid-calc-to-number day))))

(grid-calc-defun DATEVALUE (text)
  "Returns the serial number of a date written as text, year first."
  (or (grid-format-parse-date (grid-calc-to-string text))
      (error "Value cannot be read as a date: %s" text)))

(grid-calc-defun YEAR (date)
  "Returns the year of a date."
  (nth 0 (grid-format-serial-to-date (grid-calc--to-serial date))))

(grid-calc-defun MONTH (date)
  "Returns the month of a date."
  (nth 1 (grid-format-serial-to-date (grid-calc--to-serial date))))

(grid-calc-defun DAY (date)
  "Returns the day of the month of a date."
  (nth 2 (grid-format-serial-to-date (grid-calc--to-serial date))))

(grid-calc-defun HOUR (date)
  "Returns the hour of a time."
  (nth 3 (grid-format-serial-to-date (grid-calc--to-serial date))))

(grid-calc-defun MINUTE (date)
  "Returns the minute of a time."
  (nth 4 (grid-format-serial-to-date (grid-calc--to-serial date))))

(grid-calc-defun WEEKDAY (date &optional return_type)
  "Returns the day of the week corresponding to a date.
return_type 1 (default): Sunday=1 through Saturday=7.
return_type 2: Monday=1 through Sunday=7."
  (let ((dow (nth 6 (grid-format-serial-to-date (grid-calc--to-serial date))))
        (type (if return_type (grid-calc-to-number return_type) 1)))
    (pcase type
      (1 (1+ dow))
      (2 (if (= dow 0) 7 dow))
      (_ (error "Unsupported WEEKDAY return_type: %s" type)))))

(defun grid-calc--add-months (date months)
  "Return (YEAR MONTH DAY) of DATE with MONTHS added to its month."
  (pcase-let ((`(,year ,month ,day)
               (grid-format-serial-to-date (grid-calc--to-serial date))))
    (list year (+ month (truncate (grid-calc-to-number months))) day)))

(grid-calc-defun EOMONTH (start_date months)
  "Returns the last day of the month MONTHS months from a date."
  (pcase-let ((`(,year ,month ,_) (grid-calc--add-months start_date months)))
    (grid-format-date-to-serial year (1+ month) 0)))

(grid-calc-defun EDATE (start_date months)
  "Returns the same day of the month MONTHS months from a date.
A day the target month does not have becomes its last day."
  (pcase-let ((`(,year ,month ,day) (grid-calc--add-months start_date months)))
    (min (grid-format-date-to-serial year month day)
         (grid-format-date-to-serial year (1+ month) 0))))

(grid-calc-defun DAYS (end_date start_date)
  "Returns the number of days from START_DATE to END_DATE."
  (- (floor (grid-calc--to-serial end_date))
     (floor (grid-calc--to-serial start_date))))

(grid-calc-defun VALUE (text)
  "Returns the number that text reads as; a date gives its serial number."
  (let ((text (substring-no-properties (grid-calc-to-string text))))
    (or (grid-calc--numeric text)
        (let ((typed (grid-format-parse-entry text)))
          (and typed (string-to-number (car typed))))
        (error "Value cannot be read as a number: %s" text))))

(grid-calc-defun TEXT (value format_text)
  "Returns a number written after a format code such as 0.00 or yyyy-mm-dd."
  (let ((format (grid-format-from-code (grid-calc-to-string format_text))))
    (when (eq (car-safe format) 'code)
      (error "Unsupported format in TEXT: %s" format_text))
    (grid-format-display (if (eq (car-safe format) 'date)
                             (grid-calc--to-serial value)
                           value)
                         format)))

;; Lookup functions (Placeholder)
(grid-calc-defun VLOOKUP (lookup-value table-range col-index exact-match)
  "Vertical lookup function (Placeholder)."
  (error "VLOOKUP not yet implemented."))

(provide 'grid-table-calc)
