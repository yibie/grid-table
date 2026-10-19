;;; grid-table-calc.el --- AST evaluator and calculation engine -*- lexical-binding: t -*-

(require 'grid-table-parser)
(require 'grid-table-format)
(require 'grid-table-calc-classes)

(defvar grid-calc-functions)
(defvar grid-table-allow-elisp)

(defvar grid-cell--parsed)
(defvar grid-cell--reads)
(declare-function grid-data-model-styles "grid-data-model")
(declare-function grid-data-model-cols "grid-data-model")
(declare-function grid-model-legacy-at "grid-data-model")
(declare-function grid-model-array-at "grid-data-model")

;;;----------------------------------------------------------------------
;;; Main Calculation Entry Point
;;;----------------------------------------------------------------------

(define-error 'grid-calc-error "Formula error")
(define-error 'grid-calc-cycle "Circular reference" 'grid-calc-error)

(defconst grid-calc-error-values
  '("#DIV/0!" "#NAME?" "#N/A" "#REF!" "#VALUE!" "#NUM!" "#NULL!" "#CALC!" "#SPILL!"
    "#GETTING_DATA" "#CYCLE!" "#ERROR!")
  "Values a cell shows when its formula cannot be computed.")

(defvar grid-calc--pass nil
  "State of the recalculation in progress, or nil.
A hash table from each model the recalculation has reached to a vector
with one entry per cell: nil when the cell has not been computed yet,
`visiting' while its formula is being evaluated and `done' afterwards.
One table serves all the sheets of a workbook, so that a formula reading
another sheet has that cell computed first and a cycle through several
sheets is found.")

(declare-function grid-model-find-merge "grid-data-model")
(declare-function grid-calc--day-number "grid-table-calc-date")
(declare-function grid-calc--day-checked "grid-table-calc-date")

(defvar grid-calc--spills (make-hash-table :test 'eq :weakness 'key)
  "For each model, the formulas whose values spill over the cells below
and to the right of them: a vector [ANCHORS LIVE WANTS].  ANCHORS maps
the index of such a formula's cell to (ROWS COLS BLOCKED), the size of
its values and whether a cell in the way keeps them from spilling, when
its formula shows #SPILL!.  LIVE maps each cell showing a spilled value
to the index of the formula it comes from.  WANTS maps each cell the
values would cover to the formulas that would spill into it, spilling
or blocked, so that a change to it computes them again.")

(defvar grid-calc--spill-changed nil
  "Non-nil when the recalculation in progress moved a spill.
A cell may then have read a spilled cell before the value it shows
now was there, and the cells are computed again.")

(defvar grid-calc-sheets nil
  "Function that returns the model of the sheet with a given name, or nil.
Called with :names instead of a name, it returns the names of all the
sheets in order, for a reference through several of them such as
Sheet1:Sheet3!A1.  Bound by whoever keeps several sheets together;
without it a reference into another sheet is #REF!.")

(defvar grid-calc-externals t
  "Non-nil makes a reference into another workbook read what it cached.
A workbook a sheet is not open from has its values cached in the file
that reads it.  INDIRECT is the exception: Excel gives #REF! for a
reference into a workbook that is not open, whatever the file caches.")

(defvar grid-calc-names nil
  "Function that returns the parsed definition of a defined name, or nil.
A definition is a formula, most often a cell or range reference such as
Sheet1!$B$2:$B$9.  Bound by whoever keeps the names; without it every
name is #NAME?.")

(defvar grid-calc-file-name nil
  "Function that returns where the sheet of a MODEL is, or nil.
It names the workbook's file and the sheet as Excel's CELL(\"filename\")
does: /dir/[Book.xlsx]Sheet1.  Bound by whoever keeps the sheets of a
workbook read from a file; without it CELL(\"filename\") is empty text,
as in a workbook never saved.")

(defvar grid-calc-tables nil
  "Function that finds a table, as a reference such as Table1[Qty] names.
Called with a table NAME, or with nil and the MODEL, ROW and COL of a
cell to find the table that cell is in.  It returns (MODEL TOP LEFT
BOTTOM RIGHT HEADERS TOTALS COLUMNS) or nil: the table's sheet, its
corners, zero-based and inclusive, how many header and totals rows it
has, and the names of its columns.  Bound by whoever keeps the tables;
without it every such reference is #REF!.")

(defvar grid-calc--names-open nil
  "The defined names being evaluated, in capitals, innermost first.")

(defvar grid-calc--locals nil
  "Names LET gives values to, an alist from name in capitals to value.")

(defvar grid-calc--here)
(defvar grid-calc-legacy-arrays)
(defvar grid-calc--array-formula)

(defun grid-calc--name-ast (name &optional sheet model)
  "Return the parsed definition of NAME, or nil when it is not defined.
SHEET is the sheet NAME was qualified with, as in Sheet1!Rate, or nil;
MODEL the sheet of the formula using it, by default the one computed."
  (and grid-calc-names
       (funcall grid-calc-names name sheet (or model (car grid-calc--here)))))

(defconst grid-calc--cell-text-regexp
  "\\`\\(\\$?\\)\\([A-Za-z]\\{1,3\\}\\)\\(\\$?\\)\\([0-9]+\\)\\'"
  "Matches a cell reference, its four parts in groups 1 to 4.")

(defun grid-calc--relative-p (ast)
  "Return non-nil when AST holds a cell reference with a part not anchored."
  (pcase ast
    (`(CELL-REF ,text . ,_)
     (and (string-match grid-calc--cell-text-regexp text)
          (or (string-empty-p (match-string 1 text))
              (string-empty-p (match-string 3 text)))))
    ((pred consp) (cl-some #'grid-calc--relative-p ast))))

(defun grid-calc--relative-to (ast row col)
  "Return the definition AST of a name as the cell at ROW and COL reads it.
A workbook file holds a name's references as the cell A1 would read
them: a part not anchored by $ moves with the cell using the name, and
goes round past the sheet's last row or column, so XFD1 is the column
to the left and A1048576 the row above."
  (pcase ast
    (`(CELL-REF ,text . ,sheet)
     (if (not (string-match grid-calc--cell-text-regexp text))
         ast
       (let* ((col-anchor (match-string 1 text))
              (row-anchor (match-string 3 text))
              (at-col (grid-calc-col-letter-to-number
                       (upcase (match-string 2 text))))
              (at-row (1- (string-to-number (match-string 4 text)))))
         (when (string-empty-p col-anchor)
           (setq at-col (mod (+ at-col col) 16384)))
         (when (string-empty-p row-anchor)
           (setq at-row (mod (+ at-row row) 1048576)))
         `(CELL-REF ,(format "%s%s%s%d" col-anchor
                             (grid-calc-col-number-to-letter at-col)
                             row-anchor (1+ at-row))
                    . ,sheet))))
    ((pred consp) (mapcar (lambda (part) (grid-calc--relative-to part row col))
                          ast))
    (_ ast)))

(defun grid-calc--name-written-p (name)
  "Return non-nil when NAME is spelled as a name may be.
A letter other than $, an underscore or a backslash starts it, and
letters, digits, dots, question marks and quotation marks go on, as the
formula reader takes them."
  (and (stringp name)
       (> (length name) 0)
       (let ((first (aref name 0)))
         (and (not (eq first ?$))
              (grid-parser--name-start-p first)
              (cl-loop for char across (substring name 1)
                       always (and (not (eq char ?$))
                                   (grid-parser--name-char-p char)))))))

(defun grid-calc-name-problem (name)
  "Return why NAME cannot be a defined name, or nil when it can.
A name starts with a letter, an underscore or a backslash and goes on
with those, digits, dots and question marks.  It may not read as a
cell reference, as TAX1 does, though Global1, whose column is past XFD,
and Q1Sales, which only starts like one, may; nor be TRUE or FALSE.
It may share a function's name, as Excel allows: RATE is the name and
RATE(...) the function."
  (let ((upper (and (stringp name) (upcase name))))
    (cond
     ((not (grid-calc--name-written-p name))
      "A name starts with a letter, _ or \\ and holds those, digits, . and ?")
     ((> (length name) 255) "A name cannot be longer than 255 characters")
     ((and (string-match "\\`\\([A-Za-z]+\\)\\([0-9]+\\)\\'" name)
           ;; Global1 is a name: no sheet has a column GLOBAL.
           (grid-parser--cell-text-p
            (upcase (concat (match-string 1 name) (match-string 2 name)))))
      "A name cannot be a cell reference")
     ((member upper '("TRUE" "FALSE")) "TRUE and FALSE cannot be names"))))

(defvar grid-calc-current-sheet nil
  "Name of the sheet whose model is being changed, when it has one.")

(defvar grid-calc-defer nil
  "Non-nil makes `grid-table-calc-evaluate' do nothing.
Whoever binds it recalculates afterwards, all sheets together.")

(defun grid-calc--pass-of (model)
  "Return the state of MODEL's cells in the recalculation in progress.
A vector with an entry per cell when every cell is computed; a hash
table holding only the cells to compute when the others keep their
values."
  (or (gethash model grid-calc--pass)
      (puthash model (make-vector (length (grid-data-model-cells model)) nil)
               grid-calc--pass)))

(defun grid-calc--sheet-model (model sheet)
  "Return the model a reference into SHEET reads; MODEL when SHEET is nil.
A sheet that does not exist makes the reference #REF!."
  (cond ((null sheet) model)
        ((and grid-calc-sheets (funcall grid-calc-sheets sheet)))
        (t (signal 'grid-calc-error '("#REF!")))))

(defun grid-calc--sheet-span (sheet)
  "Return the names of the sheets SHEET runs through, in order, or nil.
SHEET is a sheet name as a reference writes it; one such as
Sheet1:Sheet3 runs through every sheet from the one to the other, as
they stand in the workbook.  Nil for a single sheet, and #REF! when
either end is not a sheet."
  (when (and sheet (string-search ":" sheet))
    (let* ((ends (split-string sheet ":"))
           (names (and grid-calc-sheets (funcall grid-calc-sheets :names)))
           (from (cl-position (car ends) names :test #'grid-calc--same-sheet-p))
           (to (cl-position (cadr ends) names :test #'grid-calc--same-sheet-p)))
      (unless (and from to)
        (signal 'grid-calc-error '("#REF!")))
      (cl-subseq names (min from to) (1+ (max from to))))))

(defun grid-calc--reference-sheet (ast)
  "Return the sheet the CELL-REF or RANGE AST names, or nil."
  (pcase ast
    (`(CELL-REF ,_ . ,sheet) (car sheet))
    (`(RANGE ,start ,_) (nth 2 start))))

(defun grid-calc--on-sheet (ast sheet)
  "Return the CELL-REF or RANGE AST naming SHEET instead."
  (pcase ast
    (`(CELL-REF ,text . ,_) (list 'CELL-REF text sheet))
    (`(RANGE ,start ,end) (list 'RANGE (grid-calc--on-sheet start sheet)
                                (grid-calc--on-sheet end sheet)))))

(defvar grid-calc--depth 0
  "How many formulas are being evaluated on top of each other.")

(defvar grid-calc--legacy-outside 'unset
  "`grid-calc-legacy-arrays' as the recalculation found it, or `unset'.
A formula computed because another reads it is read as its own cell
says, not as the one reading it.")

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

(defun grid-calc--typed-value (raw)
  "Return the value of a cell that holds typed RAW.
Text typed after an apostrophe is text even when it reads as a number.
A cell's format does not change what it holds: a number typed into a
cell with the text format is stored after an apostrophe, as
`grid-table-read-entry' does."
  (if (and (not (string-empty-p raw)) (eq (aref raw 0) ?'))
      (propertize (substring raw 1) 'grid-text t)
    raw))

(defun grid-calc--parsed (cell raw)
  "Return the parsed form of the formula RAW that CELL holds.
The cell keeps it for as long as it holds that text, so a formula is
parsed once and not at every recalculation."
  (let ((kept (aref cell grid-cell--parsed)))
    (if (and (consp kept) (eq (car kept) raw))
        (cdr kept)
      (let ((parsed (grid-parser-parse (substring raw 1))))
        (aset cell grid-cell--parsed (cons raw parsed))
        parsed))))

(defvar grid-calc--deepest 10
  "How many formulas may be computed on top of each other.
A formula reading a cell not yet computed computes it first, and that
cell the ones it reads: a long chain of them, as a column each of whose
cells reads the one below, would go deeper than Lisp may.  The formula
that would go deeper, or that does, is put off, computed on its own, and
the ones waiting on it are computed again after it.")

(defvar grid-calc--put-off nil
  "The depth formulas are counted from while one may be put off, or nil.")

(defvar grid-calc--waiting-on nil
  "The formulas waiting on a put off one, by it, while one may be put off.
A hash table from (MODEL . INDEX) of the put off formula to a list of
(MODEL . INDEX) of each formula its putting off cut short.  Those wait
for it, and through it for every formula put off before it that is not
done: one of them read while any is computed is on a cycle.")

;; Not an `error', so no formula takes it for a failure of its own.
(put 'grid-calc-put-off 'error-conditions '(grid-calc-put-off))
(put 'grid-calc-put-off 'error-message "Formula put off")

(defun grid-calc--compute-cell (model index)
  "Compute the cell at INDEX of MODEL unless this pass already did.
A formula that fails gives its cell one of `grid-calc-error-values'
instead of stopping the recalculation."
  (if grid-calc--put-off
      (grid-calc--compute-cell-1 model index)
    (let ((grid-calc--put-off grid-calc--depth)
          (grid-calc--waiting-on (make-hash-table :test #'equal))
          (waiting (list (cons model index))))
      (while waiting
        (condition-case put-off
            (progn
              (grid-calc--compute-cell-1 (caar waiting) (cdar waiting))
              ;; What waited on it is computed again as it comes.
              (dolist (cell (gethash (car waiting) grid-calc--waiting-on))
                (let ((pass (grid-calc--pass-of (car cell))))
                  (when (eq (car-safe (grid-calc--pass-state pass (cdr cell)))
                            'waiting)
                    (grid-calc--set-pass-state pass (cdr cell) nil))))
              (remhash (pop waiting) grid-calc--waiting-on))
          (grid-calc-put-off (push (cdr put-off) waiting)))))))

(defun grid-calc--compute-cell-1 (model index)
  "Compute the cell at INDEX of MODEL as `grid-calc--compute-cell' does.
Signal `grid-calc-put-off' when its formula would go too deep."
  (let ((cell (aref (grid-data-model-cells model) index))
        (pass (grid-calc--pass-of model)))
    (pcase (if (vectorp pass) (aref pass index) (gethash index pass 'done))
      ('done nil)
      ;; The signal names the cell the cycle comes back to.
      ('visiting (signal 'grid-calc-cycle (list model index)))
      ;; It waits on the formulas being computed, which read it: a cycle
      ;; too long to go round, closed in a formula off the stack.
      (`(waiting . ,_) (signal 'grid-calc-cycle (list model index)))
      (_
       ;; A blank cell may show what a formula above or to the left of
       ;; it spills; that formula is computed first, and writes it.
       (when (equal (grid-cell-raw-input cell) "")
         (dolist (anchor (grid-calc--spill-wanted model index))
           (grid-calc--compute-cell model anchor)))
       (unless (eq (grid-calc--pass-state pass index) 'done)
         (grid-calc--compute-own-cell model index cell pass))))))

(defun grid-calc--pass-state (pass index)
  "Return the state of the cell at INDEX in PASS, a model's pass state."
  (if (vectorp pass) (aref pass index) (gethash index pass 'done)))

(defun grid-calc--set-pass-state (pass index state)
  "Make STATE the state of the cell at INDEX in PASS."
  (if (vectorp pass) (aset pass index state) (puthash index state pass)))

(defun grid-calc--compute-own-cell (model index cell pass)
  "Compute the cell CELL at INDEX of MODEL from what it holds.
PASS is MODEL's state in the recalculation in progress."
  (when (and grid-calc--put-off
             (> grid-calc--depth (+ grid-calc--put-off grid-calc--deepest)))
    (signal 'grid-calc-put-off (cons model index)))
  (grid-calc--set-pass-state pass index 'visiting)
       (let ((raw (grid-cell-raw-input cell))
             cycle-head)
         (set-grid-cell-computed-value
          cell
          (if (and (> (length raw) 0) (eq (aref raw 0) ?=))
              (condition-case err
                  (let* ((grid-calc--depth (1+ grid-calc--depth))
                         (grid-calc--locals nil)
                         (grid-calc--names-open nil)
                         (cols (grid-data-model-cols model))
                         (row (/ index cols))
                         (col (% index cols))
                         (grid-calc--legacy-outside
                          (if (eq grid-calc--legacy-outside 'unset)
                              grid-calc-legacy-arrays
                            grid-calc--legacy-outside))
                         (grid-calc-legacy-arrays
                          (or (grid-model-legacy-at model row col)
                              grid-calc--legacy-outside))
                         (grid-calc--array-formula
                          (and (grid-model-array-at model row col) t))
                         (range (grid-model-array-at model row col))
                         (ast (grid-calc--parsed cell raw))
                         ;; Unstored, as the formula computed it
                         (raw-value (and (or range (not grid-calc-legacy-arrays))
                                         (not (eq (car-safe ast) 'ELISP-CODE))))
                         (value (grid-calc--eval-formula
                                 model ast row col raw-value)))
                    (cond
                     (range (grid-calc--spill
                             model index
                             (grid-calc--array-fill value range row col)
                             t))
                     ((grid-calc-array-p value)
                      (grid-calc--spill model index value))
                     (t (grid-calc--spill-retract model index)
                        (if raw-value (grid-calc--store value) value))))
                (grid-calc-cycle (grid-calc--spill-retract model index)
                                 (setq cycle-head (cdr err)) "#CYCLE!")
                ;; Computed again once what it waits on is
                (grid-calc-put-off
                 (grid-calc--set-pass-state pass index (cons 'waiting (cdr err)))
                 (push (cons model index)
                       (gethash (cdr err) grid-calc--waiting-on))
                 (signal (car err) (cdr err)))
                ;; Names using names can go too deep in fewer formulas:
                ;; this formula is put off, unless nothing waits on it.
                (recursion-error
                 (grid-calc--spill-retract model index)
                 (if (and grid-calc--put-off
                          (> grid-calc--depth grid-calc--put-off))
                     (progn (grid-calc--set-pass-state pass index nil)
                            (signal 'grid-calc-put-off (cons model index)))
                   (grid-calc--error-value err)))
                (error (grid-calc--spill-retract model index)
                       (grid-calc--error-value err)))
            (grid-calc--spill-retract model index)
            (grid-calc--typed-value raw)))
         (grid-calc--set-pass-state pass index 'done)
         ;; Every formula on a cycle is part of it, not only the one that
         ;; closed it: the signal goes on up to the cell the cycle comes
         ;; back to.  A formula that only reads that cell, from outside
         ;; the cycle, sees its #CYCLE! as any error value, whichever of
         ;; them is computed first.
         (when (and cycle-head (> grid-calc--depth 0)
                    (not (equal cycle-head (list model index))))
           (signal 'grid-calc-cycle cycle-head))))

;;;----------------------------------------------------------------------
;;; Spilling
;;;----------------------------------------------------------------------
;;
;; A formula typed in the grid that gives several values shows the first
;; and spills the others over the cells below and to the right of it, as
;; =SEQUENCE(3) fills A1:A3.  Those cells stay empty, showing the values;
;; a cell in the way that holds something, a merged cell, another spill
;; or the sheet's edge makes the formula #SPILL! instead.  Formulas read
;; from a workbook file, and array formulas, do not spill.

(defun grid-calc--spill-state (model)
  "Return MODEL's spills, a vector [ANCHORS LIVE WANTS]."
  (or (gethash model grid-calc--spills)
      (puthash model (vector (make-hash-table) (make-hash-table)
                             (make-hash-table))
               grid-calc--spills)))

(defun grid-calc--spill-wanted (model index)
  "Return the formulas of MODEL that would spill into the cell at INDEX."
  (when-let* ((state (gethash model grid-calc--spills)))
    (gethash index (aref state 2))))

(defun grid-calc-spilled-p (model row col)
  "Return non-nil when the cell at ROW and COL of MODEL shows a spilled value."
  (and (grid-calc--spill-source
        model (+ (* row (grid-data-model-cols model)) col))
       t))

(defun grid-calc-spill-range (model row col)
  "Return the cells the formula at ROW and COL of MODEL spills over, or nil.
A range such as \"A1:B3\"; nil when it spills nothing or is blocked."
  (when-let* ((state (gethash model grid-calc--spills))
              (spill (gethash (+ (* row (grid-data-model-cols model)) col)
                              (aref state 0))))
    (pcase-let ((`(,rows ,cols ,blocked) spill))
      (unless blocked
        (format "%s%d:%s%d"
                (grid-calc-col-number-to-letter col) (1+ row)
                (grid-calc-col-number-to-letter (+ col cols -1)) (+ row rows))))))

(defun grid-calc-spill-blocked-p (model row col)
  "Return non-nil when the formula at ROW and COL of MODEL shows #SPILL!."
  (grid-calc--spill-blocked-p
   model (+ (* row (grid-data-model-cols model)) col)))

(defun grid-calc--many-cells-p (ast)
  "Return non-nil when the formula AST reads a range or array anywhere.
A multi-cell range, an array constant, a union or intersection, a name,
a table, a spill or a reference a function builds all count."
  (cond
   ((not (consp ast)) nil)
   ((memq (car ast) '(RANGE RANGE-OP ARRAY UNION INTERSECT NAME STRUCT-REF))
    t)
   ((and (eq (car ast) 'FUNCTION-CALL)
         (member (upcase (format "%s" (cadr ast)))
                 '("ANCHORARRAY" "OFFSET" "INDIRECT")))
    t)
   (t (or (grid-calc--many-cells-p (car ast))
          (grid-calc--many-cells-p (cdr ast))))))

(defun grid-calc-legacy-differs-p (model row col)
  "Return non-nil when the formula at ROW and COL of MODEL may read
otherwise as a workbook file's.  A formula typed in the grid takes a
range whole where a file's may take one cell of it, as Excel before
dynamic arrays read =MAX(ABS(A1:A3)); a workbook marks such a formula as
a dynamic array of its own cell.  Any formula reading a range or array
counts, and any whose value the old reading changes."
  (when-let* ((cell (grid-model-get-cell model row col))
              (raw (grid-cell-raw-input cell))
              ((grid-calc--formula-p raw))
              ((not (grid-model-legacy-at model row col)))
              ((not (grid-model-array-at model row col)))
              ;; One the grid cannot read is kept as it was written.
              (ast (ignore-errors (grid-calc--parsed cell raw)))
              ((not (eq (car-safe ast) 'ELISP-CODE))))
    (or (grid-calc--many-cells-p ast)
        (not (equal (grid-cell-computed-value cell)
                    (condition-case err
                        (let ((grid-calc--legacy-outside
                               (if (eq grid-calc--legacy-outside 'unset)
                                   grid-calc-legacy-arrays
                                 grid-calc--legacy-outside))
                              (grid-calc-legacy-arrays t)
                              (grid-calc--array-formula nil)
                              (grid-calc--depth (1+ grid-calc--depth))
                              (grid-calc--locals nil)
                              (grid-calc--names-open nil))
                          (grid-calc--eval-formula model ast row col))
                      (error (grid-calc--error-value err))))))))

(defun grid-calc--spill-blocked-p (model index)
  "Return non-nil when the formula at INDEX of MODEL shows #SPILL!."
  (when-let* ((state (gethash model grid-calc--spills)))
    (nth 2 (gethash index (aref state 0)))))

(defun grid-calc--spill-source (model index)
  "Return the formula whose value the cell at INDEX of MODEL shows, or nil."
  (when-let* ((state (gethash model grid-calc--spills)))
    (gethash index (aref state 1))))

(defun grid-calc--spill-cells (model anchor rows cols)
  "Return the cells of MODEL a spill of ROWS by COLS from ANCHOR covers.
Only those on the sheet, and not ANCHOR itself."
  (let* ((width (grid-data-model-cols model))
         (top (/ anchor width))
         (left (% anchor width))
         cells)
    (dotimes (row (min rows (- (grid-data-model-rows model) top)))
      (dotimes (col (min cols (- width left)))
        (unless (and (= row 0) (= col 0))
          (push (+ (* (+ top row) width) left col) cells))))
    (nreverse cells)))

(defun grid-calc--spill-clear (model index state)
  "Take the spill of the formula at INDEX of MODEL off its cells.
STATE is MODEL's spill state.  The cells it covered show what they hold."
  (pcase (gethash index (aref state 0))
    (`(,rows ,cols ,_)
     (let ((pass (and grid-calc--pass (grid-calc--pass-of model)))
           (cells (grid-data-model-cells model)))
       (dolist (at (grid-calc--spill-cells model index rows cols))
         (puthash at (delq index (gethash at (aref state 2))) (aref state 2))
         (unless (gethash at (aref state 2)) (remhash at (aref state 2)))
         (when (eql (gethash at (aref state 1)) index)
           (remhash at (aref state 1))
           (let ((cell (aref cells at)))
             (when (and pass (eq (grid-calc--pass-state pass at) 'done)
                        (not (equal (grid-cell-computed-value cell)
                                    (grid-cell-raw-input cell))))
               (setq grid-calc--spill-changed t))
             (set-grid-cell-computed-value
              cell (grid-calc--typed-value (grid-cell-raw-input cell)))))))
     (remhash index (aref state 0)))))

(defun grid-calc--spill-retract (model index)
  "Note that the cell at INDEX of MODEL spills nothing."
  (when-let* ((state (gethash model grid-calc--spills)))
    (when (gethash index (aref state 0))
      (grid-calc--spill-clear model index state)
      (setq grid-calc--spill-changed t))))

(defun grid-calc--array-fill (value range row col)
  "Return VALUE as the array formula at ROW and COL fills RANGE with it.
RANGE, such as \"A1:B3\", is the cells the formula was entered over.  A
single value fills them all, a single row or column repeats down or
across, and a cell past the values shows #N/A, as in a spreadsheet."
  (let* ((corner (and (string-match "\\`\\$?[A-Z]+\\$?[0-9]+:\\(\\$?[A-Z]+\\$?[0-9]+\\)\\'"
                                    range)
                      (grid-calc-parse-cell-ref (match-string 1 range))))
         (rows (if corner (1+ (- (plist-get corner :row) row)) 1))
         (cols (if corner (1+ (- (plist-get corner :col) col)) 1))
         (value (if (grid-calc-array-p value) value
                  (grid-calc-array--make 1 1 (vector value))))
         (have-rows (grid-calc-array-rows value))
         (have-cols (grid-calc-array-cols value))
         (data (make-vector (* (max rows 1) (max cols 1)) nil)))
    (setq rows (max rows 1) cols (max cols 1))
    (dotimes (r rows)
      (dotimes (c cols)
        (let ((r* (if (= have-rows 1) 0 r))
              (c* (if (= have-cols 1) 0 c)))
          (aset data (+ (* r cols) c)
                (if (and (< r* have-rows) (< c* have-cols))
                    (grid-calc-aref value r* c*)
                  (grid-calc-err "#N/A"))))))
    (grid-calc-array--make rows cols data)))

(defun grid-calc--spill (model index array &optional fixed)
  "Spill ARRAY, the values of the formula at INDEX of MODEL.
Return the value the formula's own cell shows: the first of them, or
#SPILL! when they cannot spill.  FIXED, for an array formula, fills
the cells of ARRAY's size that hold nothing and is never blocked."
  (let* ((rows (grid-calc-array-rows array))
         (cols (grid-calc-array-cols array))
         (state (grid-calc--spill-state model))
         (width (grid-data-model-cols model))
         (top (/ index width))
         (left (% index width))
         (cells (grid-calc--spill-cells model index rows cols))
         (all-cells (grid-data-model-cells model))
         (old (gethash index (aref state 0)))
         (blocked
          (and
           (not fixed)
           (or (> (+ top rows) (grid-data-model-rows model))
              (> (+ left cols) width)
              (cl-some (lambda (at)
                         (or (not (equal (grid-cell-raw-input (aref all-cells at)) ""))
                             (let ((source (gethash at (aref state 1))))
                               (and source (/= source index)))
                             (grid-model-find-merge model (/ at width) (% at width))))
                       cells)
              (and (grid-model-find-merge model top left) t))))
         (new (list rows cols (and blocked t))))
    (if (and (= rows 1) (= cols 1) (not fixed))
        (progn (grid-calc--spill-retract model index)
               (grid-calc--store (grid-calc-aref array 0 0)))
      (unless (equal old new)
        (grid-calc--spill-clear model index state)
        (setq grid-calc--spill-changed t))
      (puthash index new (aref state 0))
      (dolist (at cells)
        (cl-pushnew index (gethash at (aref state 2))))
      (if blocked
          "#SPILL!"
        (let ((pass (grid-calc--pass-of model)))
          (dolist (at cells)
            (let* ((cell (aref all-cells at))
                   (taken (or (not (equal (grid-cell-raw-input cell) ""))
                              (let ((source (gethash at (aref state 1))))
                                (and source (/= source index)))))
                   (value (grid-calc--store
                           (grid-calc-aref array (- (/ at width) top)
                                           (- (% at width) left)))))
              (unless taken
                (when (and (eq (grid-calc--pass-state pass at) 'done)
                           (not (equal value (grid-cell-computed-value cell))))
                  (setq grid-calc--spill-changed t))
                (puthash at index (aref state 1))
                (set-grid-cell-computed-value cell value)
                (grid-calc--set-pass-state pass at 'done)))))
        (grid-calc--store (grid-calc-aref array 0 0))))))

;;;----------------------------------------------------------------------
;;; Recalculating only what an edit reaches
;;;----------------------------------------------------------------------

(defvar grid-calc-incremental t
  "Non-nil recalculates only the cells an edit can change.
After an edit those are the cell itself, the formulas that read it and,
in turn, the ones that read them.  With nil every cell is computed at
every recalculation.")

(defconst grid-calc--scan-limit 3000000
  "How many range checks finding the cells to recalculate may take.
Past it, computing every cell is the faster way.")

(defconst grid-calc--volatile-functions
  '("TODAY" "NOW" "RAND" "RANDBETWEEN" "RANDARRAY"
    ;; These read cells their arguments do not name, or what is not in
    ;; a cell at all.
    "OFFSET" "INDIRECT" "CELL" "INFO" "SUBTOTAL" "AGGREGATE" "ANCHORARRAY")
  "Functions whose value changes without any cell they name changing.")

(defvar grid-calc--known-functions (make-hash-table :test 'equal)
  "Functions that read nothing but their arguments.
A formula calling any other function is computed at every recalculation,
since nothing says what such a function reads.")

(defvar grid-calc--graphs (make-hash-table :test 'eq :weakness 'key)
  "For each model, who reads what: a vector of six slots.
[CELLS RANGES ALWAYS DIRTY OTHER-CELLS OTHER-RANGES].  CELLS maps the
index of a cell to the indices of the formulas that name it, RANGES
lists (FORMULA TOP BOTTOM LEFT RIGHT) for the ranges formulas name,
ALWAYS lists the formulas computed at every recalculation and DIRTY the
cells changed since the last one.  OTHER-CELLS and OTHER-RANGES are the
same as CELLS and RANGES for formulas on other sheets, each named by
\(MODEL . INDEX).  A model without an entry has every cell computed.")

(defun grid-calc-declare-known-functions ()
  "Record the functions defined so far as reading only their arguments."
  (maphash (lambda (name _function)
             (unless (member name grid-calc--volatile-functions)
               (puthash name t grid-calc--known-functions)))
           grid-calc-functions))

(defun grid-calc-touch (model row col)
  "Note that the cell at ROW and COL of MODEL changed.
The next recalculation computes it and what reads it."
  (when-let* ((graph (gethash model grid-calc--graphs)))
    (push (+ (* row (grid-data-model-cols model)) col) (aref graph 3))))

(defun grid-calc-forget (model)
  "Have every cell of MODEL computed at the next recalculation.
For changes made behind the model's back, such as formulas rewritten in
place."
  (remhash model grid-calc--graphs))

(defun grid-calc--reads (model ast)
  "Return what the formula AST reads in MODEL.
A list of cell indices and of (TOP BOTTOM LEFT RIGHT) ranges of MODEL,
and of (OTHER . INDEX) and (OTHER TOP BOTTOM LEFT RIGHT) for the cells
and ranges of another sheet's model OTHER.  Or the symbol `always' for a
formula to compute at every recalculation: one that reads a sheet that
cannot be found, calls a function that may read anything or whose value
changes by itself, or runs Lisp."
  (catch 'always
    (let (reads)
      (cl-labels
          ((sheet (name)
             ;; A sheet naming the formula's own is the formula's own.
             (cond ((null name) model)
                   (grid-calc-sheets (or (funcall grid-calc-sheets name)
                                         (throw 'always 'always)))
                   (t (throw 'always 'always))))
           (walk (node)
             (pcase node
               ;; Through several sheets: each of them
               ((and (or `(CELL-REF . ,_) `(RANGE . ,_))
                     (let span (condition-case nil
                                   (grid-calc--sheet-span
                                    (grid-calc--reference-sheet node))
                                 (grid-calc-error (throw 'always 'always))))
                     (guard span))
                (dolist (name span)
                  (walk (grid-calc--on-sheet node name))))
               (`(CELL-REF ,ref . ,name)
                (let* ((target (sheet (car name)))
                       (rows (grid-data-model-rows target))
                       (cols (grid-data-model-cols target))
                       (at (grid-calc--reference-coords ref)))
                  (when (and (car at) (cdr at)
                             (< (car at) rows) (< (cdr at) cols))
                    (let ((index (+ (* (car at) cols) (cdr at))))
                      (push (if (eq target model) index (cons target index))
                            reads)))))
               (`(RANGE ,start ,end)
                (pcase-let ((`(,target ,top ,bottom ,left ,right)
                             (progn (sheet (nth 2 start))
                                    (grid-calc--range-bounds model start end))))
                  (when (and (<= top bottom) (<= left right))
                    (push (if (eq target model)
                              (list top bottom left right)
                            (list target top bottom left right))
                          reads))))
               (`(FUNCTION-CALL ,name ,arguments)
                (unless (gethash (upcase name) grid-calc--known-functions)
                  (throw 'always 'always))
                (mapc #'walk arguments))
               (`(ELISP-CODE . ,_) (throw 'always 'always))
               ;; A table reference reads the whole table, which is
               ;; found again should its sheet change.
               (`(STRUCT-REF ,name ,_)
                (pcase (and grid-calc-tables
                            (if name
                                (funcall grid-calc-tables name)
                              (throw 'always 'always)))
                  (`(,target ,top ,left ,bottom ,right . ,_)
                   (push (if (eq target model)
                             (list top bottom left right)
                           (list target top bottom left right))
                         reads))
                  (_ (throw 'always 'always))))
               ;; A name reads what it is defined as; one not defined
               ;; reads nothing until defining it recomputes everything.
               (`(NAME ,name . ,qualifier)
                (unless grid-calc-names (throw 'always 'always))
                (when-let* ((definition (grid-calc--name-ast
                                         name (car qualifier) model)))
                  ;; What a name with references not anchored reads
                  ;; depends on the cell using it.
                  (when (grid-calc--relative-p definition)
                    (throw 'always 'always))
                  (walk definition)))
               ((pred consp)
                (dolist (part node)
                  (when (consp part) (walk part)))))))
        (walk ast))
      reads)))

(defun grid-calc-precedents (model row col)
  "Return the cells and ranges the formula at ROW and COL of MODEL reads.
Each is (SHEET TOP LEFT BOTTOM RIGHT), zero-based and inclusive.  SHEET
is the sheet name the formula wrote, or nil for MODEL's own sheet.  The
rows of a whole column, and the columns of a whole row, are nil.  A cell
that holds no formula, or one that does not parse, reads nothing."
  (let* ((cell (grid-model-get-cell model row col))
         (raw (and cell (grid-cell-raw-input cell)))
         (ast (and (stringp raw) (> (length raw) 1) (eq (aref raw 0) ?=)
                   (condition-case nil (grid-calc--parsed cell raw)
                     (error nil))))
         found)
    (cl-labels
        ((add (sheet start end)
           (let ((from (grid-calc--reference-coords start))
                 (to (grid-calc--reference-coords end)))
             (when (and from to)
               (push (list sheet
                           (and (car from) (car to) (min (car from) (car to)))
                           (and (cdr from) (cdr to) (min (cdr from) (cdr to)))
                           (and (car from) (car to) (max (car from) (car to)))
                           (and (cdr from) (cdr to) (max (cdr from) (cdr to))))
                     found))))
         (walk (node)
           (pcase node
             (`(CELL-REF ,ref . ,sheet) (add (car sheet) ref ref))
             (`(RANGE (CELL-REF ,start . ,sheet) (CELL-REF ,end . ,_))
              (add (car sheet) start end))
             (`(NAME ,name . ,qualifier)
              (when-let* ((definition (grid-calc--name-ast
                                       name (car qualifier) model)))
                (walk (grid-calc--relative-to definition row col))))
             ((pred consp)
              (dolist (part node)
                (when (consp part) (walk part)))))))
      (when ast (walk ast)))
    (delete-dups (nreverse found))))

(defun grid-calc-reference-covers-p (reference row col)
  "Return non-nil when REFERENCE takes in the cell at ROW and COL.
REFERENCE is (SHEET TOP LEFT BOTTOM RIGHT) as `grid-calc-precedents'
returns it; its sheet is not looked at."
  (pcase-let ((`(,_ ,top ,left ,bottom ,right) reference))
    (and (or (null top) (<= top row bottom))
         (or (null left) (<= left col right)))))

(defun grid-calc--register (model graph index)
  "Record in GRAPH, MODEL's, what the cell at INDEX of MODEL reads.
What it reads on another sheet is recorded in that sheet's graph; a
sheet without one makes the cell one to compute every time."
  (let* ((cell (aref (grid-data-model-cells model) index))
         (raw (grid-cell-raw-input cell))
         (reads (and (> (length raw) 0) (eq (aref raw 0) ?=)
                     ;; A formula that does not parse reads nothing; one
                     ;; whose ranges make no sense is anyone's guess.
                     (condition-case nil
                         (let ((parsed (grid-calc--parsed cell raw)))
                           (condition-case nil
                               (grid-calc--reads model parsed)
                             (error 'always)))
                       (error nil)))))
    (when (and (consp reads)
               (cl-some (lambda (read)
                          (and (consp read) (vectorp (car read))
                               (null (gethash (car read) grid-calc--graphs))))
                        reads))
      (setq reads 'always))
    (aset cell grid-cell--reads reads)
    (if (eq reads 'always)
        (push index (aref graph 2))
      (let ((reader (cons model index)))
        (dolist (read reads)
          (pcase read
            ((pred integerp)
             (push index (gethash read (aref graph 0))))
            (`(,(and other (pred vectorp)) . ,(and at (pred integerp)))
             (let ((cells (aref (gethash other grid-calc--graphs) 4)))
               (push reader (gethash at cells))))
            (`(,(and other (pred vectorp)) . ,range)
             (let ((graph (gethash other grid-calc--graphs)))
               (aset graph 5 (cons (cons reader range) (aref graph 5)))))
            (_ (push (cons index read) (aref graph 1)))))))))

(defun grid-calc--unregister (model graph index)
  "Take out of GRAPH what the cell at INDEX of MODEL was recorded as reading."
  (let* ((cell (aref (grid-data-model-cells model) index))
         (reads (aref cell grid-cell--reads))
         (reader (cons model index)))
    (cond ((eq reads 'always)
           (aset graph 2 (delq index (aref graph 2))))
          (reads
           (dolist (read reads)
             (pcase read
               ((pred integerp)
                (puthash read (delq index (gethash read (aref graph 0)))
                         (aref graph 0)))
               (`(,(and other (pred vectorp)) . ,(and at (pred integerp)))
                (when-let* ((graph (gethash other grid-calc--graphs)))
                  (puthash at (cl-delete reader (gethash at (aref graph 4))
                                         :test #'equal)
                           (aref graph 4))))
               (`(,(and other (pred vectorp)) . ,_)
                (when-let* ((graph (gethash other grid-calc--graphs)))
                  (aset graph 5 (cl-delete reader (aref graph 5)
                                           :key #'car :test #'equal))))))
           (when (cl-some (lambda (read) (and (consp read) (integerp (car read))))
                          reads)
             (aset graph 1 (cl-delete index (aref graph 1) :key #'car)))))
    (aset cell grid-cell--reads nil)))

(defun grid-calc--build-graphs (models)
  "Record who reads what in MODELS, whose cells were all just computed.
A sheet outside MODELS that read one of them loses its record, as the
new one does not know about it, and is computed in full next time."
  (dolist (model models)
    (when-let* ((old (gethash model grid-calc--graphs)))
      (cl-flet ((forget-reader (reader)
                  (unless (memq (car reader) models)
                    (grid-calc-forget (car reader)))))
        (maphash (lambda (_at readers) (mapc #'forget-reader readers))
                 (aref old 4))
        (dolist (entry (aref old 5))
          (forget-reader (car entry)))))
    (puthash model (vector (make-hash-table) nil nil nil
                           (make-hash-table) nil)
             grid-calc--graphs))
  (dolist (model models)
    (let ((graph (gethash model grid-calc--graphs))
          (cells (grid-data-model-cells model)))
      (dotimes (index (length cells))
        (let ((raw (grid-cell-raw-input (aref cells index))))
          (if (and (> (length raw) 0) (eq (aref raw 0) ?=))
              (grid-calc--register model graph index)
            (aset (aref cells index) grid-cell--reads nil)))))))

(defun grid-calc--evaluate-dirty (models)
  "Compute the cells of MODELS that changed and the formulas they reach.
The formulas reached may be on any of the MODELS.  Return nil, having
computed nothing, when a model has no record of who reads what, when a
formula reached is on a sheet outside MODELS, or when finding the cells
would take longer than computing them all."
  (when (cl-every (lambda (model) (gethash model grid-calc--graphs)) models)
    (let ((work 0)
          (states (mapcar (lambda (model) (cons model (make-hash-table)))
                          models))
          queue)
      ;; A changed cell may read other cells than before.
      (dolist (model models)
        (let* ((graph (gethash model grid-calc--graphs))
               (dirty (delete-dups (aref graph 3))))
          (aset graph 3 nil)
          (dolist (index dirty)
            (grid-calc--unregister model graph index)
            (grid-calc--register model graph index)
            (push (cons model index) queue))))
      (dolist (model models)
        (dolist (index (aref (gethash model grid-calc--graphs) 2))
          (push (cons model index) queue)))
      (when (catch 'everything
              (while queue
                (pcase-let* ((`(,model . ,index) (pop queue))
                             (state (or (cdr (assq model states))
                                        (throw 'everything nil))))
                  (when (eq (gethash index state 'clean) 'clean)
                    (puthash index nil state)
                    ;; A cell a formula spills into, or would, has that
                    ;; formula computed again; a formula that spills, the
                    ;; cells showing its values.
                    (when-let* ((spills (gethash model grid-calc--spills)))
                      (dolist (anchor (gethash index (aref spills 2)))
                        (push (cons model anchor) queue))
                      (pcase (gethash index (aref spills 0))
                        (`(,rows ,cols nil)
                         (dolist (at (grid-calc--spill-cells model index rows cols))
                           (when (eql (gethash at (aref spills 1)) index)
                             (push (cons model at) queue))))))
                    (let* ((graph (gethash model grid-calc--graphs))
                           (cols (grid-data-model-cols model))
                           (row (/ index cols))
                           (col (% index cols)))
                      (dolist (reader (gethash index (aref graph 0)))
                        (push (cons model reader) queue))
                      (dolist (reader (gethash index (aref graph 4)))
                        (push reader queue))
                      (when (or (aref graph 1) (aref graph 5))
                        (setq work (+ work (length (aref graph 1))
                                      (length (aref graph 5))))
                        (when (> work grid-calc--scan-limit)
                          (throw 'everything nil))
                        (dolist (range (aref graph 1))
                          (when (and (<= (nth 1 range) row (nth 2 range))
                                     (<= (nth 3 range) col (nth 4 range)))
                            (push (cons model (car range)) queue)))
                        (dolist (range (aref graph 5))
                          (when (and (<= (nth 1 range) row (nth 2 range))
                                     (<= (nth 3 range) col (nth 4 range)))
                            (push (car range) queue))))))))
              t)
        (pcase-dolist (`(,model . ,state) states)
          (puthash model state grid-calc--pass))
        (pcase-dolist (`(,model . ,state) states)
          (let (indices)
            (maphash (lambda (index _state) (push index indices)) state)
            (dolist (index indices)
              (grid-calc--compute-cell model index))))
        ;; A spill that moved may cover cells whose readers were not
        ;; computed: compute everything.
        (not grid-calc--spill-changed)))))

(defvar grid-calc--exact-indexes)

(defun grid-table-calc-evaluate-models (models &optional everything)
  "Evaluate the formulas of MODELS, the sheets of one workbook.
A formula computes the cells it reads before itself, whatever their
position and whichever sheet they are in.  Only the cells that changed
since the last time and the formulas they reach are computed, unless
EVERYTHING is non-nil or a model has no record of who reads what; then
every cell is, once."
  (let ((grid-calc--pass (make-hash-table :test 'eq))
        (grid-calc--depth 0)
        (grid-calc--spill-changed nil)
        (grid-calc--exact-indexes (make-hash-table :test 'eq :weakness 'key)))
    (unless (and grid-calc-incremental (not everything)
                 (grid-calc--evaluate-dirty models))
      ;; A cell read before a spill reached it is computed again, with
      ;; the spills now known; twice is enough unless spills chase each
      ;; other.
      (let ((passes 0))
        (while (progn
                 (setq grid-calc--spill-changed nil)
                 (clrhash grid-calc--pass)
                 (clrhash grid-calc--exact-indexes)
                 (dolist (model models)
                   (dotimes (i (length (grid-data-model-cells model)))
                     (grid-calc--compute-cell model i)))
                 (and grid-calc--spill-changed (< (cl-incf passes) 3)))))
      (if grid-calc-incremental
          (grid-calc--build-graphs models)
        (mapc #'grid-calc-forget models)))))

(defun grid-table-calc-evaluate (model &optional everything)
  "Main entry point for evaluating the formulas in MODEL.
See `grid-table-calc-evaluate-models' for what is computed and for
EVERYTHING."
  (unless grid-calc-defer
    (grid-table-calc-evaluate-models (list model) everything)))


;;;----------------------------------------------------------------------
;;; Values
;;;----------------------------------------------------------------------
;;
;; Inside a formula a value is one of these:
;;
;;   a number;
;;   a string, which is text, whatever it reads as;
;;   t or `:false', TRUE and FALSE;
;;   `:blank', what an empty cell holds;
;;   a `grid-calc-err', an error value such as #N/A;
;;   a `grid-calc-array', several values in rows and columns;
;;   a `grid-calc-ref', cells not read yet, as A1:B5 is until a function
;;   or an operator reads it.
;;
;; nil is no value: an argument left out.  A cell stores what a formula
;; gives in another form, the one the rest of grid-table reads; see
;; `grid-calc--store' and `grid-calc--from-result'.

(cl-defstruct (grid-calc-err (:constructor grid-calc--make-err (code)))
  "An error value, such as #N/A; CODE is its text."
  code)

(cl-defstruct (grid-calc-lambda (:constructor grid-calc-lambda--make))
  "A function a formula makes, as LAMBDA does.
PARAMETERS are the names of its arguments, in capitals, BODY the parsed
form it computes and LOCALS the names LET gave values to where it was
made.  FUNCTION is instead the name of a built-in one, as SUM is where
BYROW(A1:B2,SUM) passes it."
  parameters body locals function)

(defconst grid-calc--errors
  (mapcar (lambda (code) (cons code (grid-calc--make-err code)))
          grid-calc-error-values)
  "The error values, one object each, from their text.")

(defun grid-calc-err (code)
  "Return the error value whose text is CODE."
  (or (cdr (assoc code grid-calc--errors))
      (error "No error value %s" code)))

(defun grid-calc-raise (code)
  "Make the function being computed give the error value CODE."
  (signal 'grid-calc-error (list code)))

(cl-defstruct (grid-calc-array (:constructor grid-calc-array--make
                                              (rows cols data)))
  "ROWS by COLS values, DATA holding them row after row."
  rows cols data)

(defun grid-calc-array (rows cols &optional function)
  "Return a ROWS by COLS array of what FUNCTION gives for each ROW and COL.
Without FUNCTION every value is blank."
  (let ((data (make-vector (* rows cols) :blank)))
    (when function
      (dotimes (row rows)
        (dotimes (col cols)
          (aset data (+ (* row cols) col) (funcall function row col)))))
    (grid-calc-array--make rows cols data)))

(defun grid-calc-array-from-list (values &optional across)
  "Return VALUES as an array of one column, or of one row when ACROSS."
  (let ((data (vconcat values)))
    (if across
        (grid-calc-array--make 1 (length data) data)
      (grid-calc-array--make (length data) 1 data))))

(defun grid-calc-array-from-rows (rows)
  "Return an array of ROWS, a list of rows each a list of values."
  (let ((height (length rows)) (width (length (car rows))))
    (grid-calc-array--make height width (vconcat (apply #'append rows)))))

(defun grid-calc-aref (array row col)
  "Return the value at ROW and COL of ARRAY.
An array of one row gives that row whatever ROW is, and one of one
column that column, as when an array meets a larger one; outside it
the value is #N/A."
  (let ((rows (grid-calc-array-rows array))
        (cols (grid-calc-array-cols array)))
    (when (= rows 1) (setq row 0))
    (when (= cols 1) (setq col 0))
    (if (and (< row rows) (< col cols))
        (aref (grid-calc-array-data array) (+ (* row cols) col))
      (grid-calc-err "#N/A"))))

(defun grid-calc-array-values (array)
  "Return the values of ARRAY as a list, row after row."
  (append (grid-calc-array-data array) nil))

(defun grid-calc-array-rows-list (array)
  "Return the rows of ARRAY, each a list of values."
  (let ((cols (grid-calc-array-cols array)) rows)
    (dotimes (row (grid-calc-array-rows array))
      (push (append (cl-subseq (grid-calc-array-data array)
                               (* row cols) (* (1+ row) cols))
                    nil)
            rows))
    (nreverse rows)))

(defun grid-calc-array-map (function array)
  "Return the array of FUNCTION applied to each value of ARRAY."
  (grid-calc-array--make (grid-calc-array-rows array)
                         (grid-calc-array-cols array)
                         (cl-map 'vector function
                                 (grid-calc-array-data array))))

(cl-defstruct (grid-calc-ref (:constructor grid-calc-ref--make
                                            (model top left bottom right
                                                   &optional whole)))
  "The cells from TOP and LEFT to BOTTOM and RIGHT of MODEL, inclusive.
They may reach past the cells MODEL has; those are blank.  WHOLE is
`column' for whole columns, as A:B, and `row' for whole rows, as 2:3:
they stop at the sheet's cells, but count as reaching its edges."
  model top left bottom right whole)

(defun grid-calc-ref-rows (ref)
  "Return how many rows REF takes in."
  (1+ (- (grid-calc-ref-bottom ref) (grid-calc-ref-top ref))))

(defun grid-calc-ref-cols (ref)
  "Return how many columns REF takes in."
  (1+ (- (grid-calc-ref-right ref) (grid-calc-ref-left ref))))

(defun grid-calc-ref-cell (ref row col)
  "Return the value at ROW and COL of REF, counted from its corner."
  (grid-calc--cell (grid-calc-ref-model ref)
                   (+ (grid-calc-ref-top ref) row)
                   (+ (grid-calc-ref-left ref) col)))

(defun grid-calc-ref-to-array (ref)
  "Return the values REF takes in, as an array.
Rows and columns past the sheet's cells are not read; they are blank."
  (let* ((model (grid-calc-ref-model ref))
         (top (grid-calc-ref-top ref))
         (left (grid-calc-ref-left ref))
         (rows (grid-calc-ref-rows ref))
         (cols (grid-calc-ref-cols ref))
         (filled-rows (max 0 (min rows (- (grid-data-model-rows model) top))))
         (filled-cols (max 0 (min cols (- (grid-data-model-cols model) left))))
         (data (make-vector (* rows cols) :blank)))
    (dotimes (row filled-rows)
      (dotimes (col filled-cols)
        (aset data (+ (* row cols) col)
              (grid-calc--cell model (+ top row) (+ left col)))))
    (grid-calc-array--make rows cols data)))

(defun grid-calc-ref-filled-rows (ref)
  "Return how many rows of REF are on its sheet, at most."
  (max 0 (min (grid-calc-ref-rows ref)
              (- (grid-data-model-rows (grid-calc-ref-model ref))
                 (grid-calc-ref-top ref)))))

(defun grid-calc-blank-p (value)
  "Return non-nil when VALUE is an empty cell."
  (eq value :blank))

(defun grid-calc-bool (truth)
  "Return TRUE or FALSE as TRUTH is non-nil or nil."
  (if truth t :false))

(defun grid-calc-boolean-p (value)
  "Return non-nil when VALUE is TRUE or FALSE."
  (memq value '(t :false)))

;;;----------------------------------------------------------------------
;;; What a cell holds, as a formula reads it
;;;----------------------------------------------------------------------

(defvar grid-calc-legacy-arrays nil
  "Non-nil reads formulas as a workbook file holds them.
A range where one value is wanted then gives the one in the formula's
row or column (implicit intersection), and a formula that comes to
several values shows the first.  That is what Excel makes of a formula
that is not an array formula, and so of every formula grid-table writes
to a workbook file.  With nil a range is its values wherever it stands,
and a formula that comes to several values is #VALUE!.")

(defvar grid-calc--here nil
  "(MODEL ROW COL) of the formula being computed, or nil.
ROW and COL are nil for a formula that belongs to no cell.")

(defvar grid-calc--array-context nil
  "Non-nil while computing where ranges are taken whole.
That is an array formula, or an argument that takes several values,
as SUMPRODUCT's do.  A range then never stands for one of its cells.")

(defvar grid-calc--array-formula nil
  "Non-nil while computing an array formula.")

(defun grid-calc--formula-p (raw)
  "Return non-nil when RAW, a cell's raw input, is a formula."
  (and (stringp raw) (> (length raw) 1) (eq (aref raw 0) ?=)))

(defun grid-calc--from-constant (raw)
  "Return the value of a cell that holds RAW, which is no formula.
Text after an apostrophe is text; otherwise a number, TRUE, FALSE or an
error value reads as itself."
  (cond
   ((or (null raw) (string-empty-p raw)) :blank)
   ((eq (aref raw 0) ?') (substring-no-properties raw 1))
   ((grid-format-text-p raw) (substring-no-properties raw))
   ((grid-format-number raw))
   ((string-equal-ignore-case raw "TRUE") t)
   ((string-equal-ignore-case raw "FALSE") :false)
   ((and (eq (aref raw 0) ?#) (assoc raw grid-calc--errors))
    (grid-calc-err raw))
   (t raw)))

(defun grid-calc-error-text-p (value)
  "Return non-nil when VALUE, as a cell stores it, is an error value.
Text that spells one, as =\"#N/A\" gives, is marked as text and is not."
  (and (stringp value) (> (length value) 0) (eq (aref value 0) ?#)
       (member value grid-calc-error-values)
       (not (get-text-property 0 'grid-text value))))

(defun grid-calc--from-result (value)
  "Return the value a formula's cell holds, from the stored VALUE.
The stored form is the one `grid-calc--store' gives."
  (cond
   ((null value) :false)
   ((eq value t) t)
   ((numberp value) value)
   ((stringp value)
    (if (grid-calc-error-text-p value) (grid-calc-err value) value))
   ;; A chart, or whatever an Elisp formula gave
   (t value)))

(defun grid-calc--cell (model row col)
  "Return the value at ROW and COL of MODEL, as a formula reads it.
During a recalculation a formula there is computed first, so it is
never read stale.  A cell past the sheet's edge is blank."
  (if (or (< row 0) (< col 0)
          (>= row (grid-data-model-rows model))
          (>= col (grid-data-model-cols model)))
      :blank
    (let ((index (+ (* row (grid-data-model-cols model)) col)))
      (when grid-calc--pass
        (grid-calc--compute-cell model index))
      (let* ((cell (aref (grid-data-model-cells model) index))
             (raw (grid-cell-raw-input cell)))
        (if (or (grid-calc--formula-p raw)
                (and (equal raw "") (grid-calc--spill-source model index)))
            (grid-calc--from-result (grid-cell-computed-value cell))
          (grid-calc--from-constant raw))))))

(defun grid-calc--store (value)
  "Return VALUE, a formula's result, in the form its cell stores.
That is a number, a string, t for TRUE, nil for FALSE or the text of an
error value.  An empty cell is 0.  Several values are the first one in
an array formula or under `grid-calc-legacy-arrays', else #VALUE!."
  (cond
   ((numberp value)
    (if (or (and (floatp value) (or (isnan value) (= value 1.0e+INF)
                                    (= value -1.0e+INF))))
        "#NUM!"
      (grid-calc--tidy-number value)))
   ;; Text that spells an error value is marked, to stay text.
   ((stringp value)
    (if (and (member value grid-calc-error-values)
             (not (get-text-property 0 'grid-text value)))
        (propertize (copy-sequence value) 'grid-text t)
      value))
   ((eq value t) t)
   ((eq value :false) nil)
   ((eq value :blank) 0)
   ((grid-calc-err-p value) (grid-calc-err-code value))
   ((grid-calc-ref-p value)
    (grid-calc--store (grid-calc--deref value)))
   ((grid-calc-array-p value)
    (if (or (and (= (grid-calc-array-rows value) 1)
                 (= (grid-calc-array-cols value) 1))
            grid-calc--array-formula grid-calc-legacy-arrays)
        (grid-calc--store (grid-calc-aref value 0 0))
      "#VALUE!"))
   ((null value) 0)
   ;; Several areas are no value a cell can show.
   ((eq (car-safe value) 'union) "#VALUE!")
   ;; A function is no value either.
   ((grid-calc-lambda-p value) "#CALC!")
   ;; A chart
   (t value)))

;;;----------------------------------------------------------------------
;;; From references to values
;;;----------------------------------------------------------------------

(defun grid-calc--intersect (ref)
  "Return the cell of REF in the formula's own row or column.
#VALUE! when there is none."
  (let ((row (nth 1 grid-calc--here))
        (col (nth 2 grid-calc--here))
        (top (grid-calc-ref-top ref)) (bottom (grid-calc-ref-bottom ref))
        (left (grid-calc-ref-left ref)) (right (grid-calc-ref-right ref)))
    (cond
     ((and (= top bottom) (= left right)) (grid-calc-ref-cell ref 0 0))
     ((and (= left right) row (<= top row bottom))
      (grid-calc-ref-cell ref (- row top) 0))
     ((and (= top bottom) col (<= left col right))
      (grid-calc-ref-cell ref 0 (- col left)))
     ((and row col (<= top row bottom) (<= left col right))
      (grid-calc-ref-cell ref (- row top) (- col left)))
     (t (grid-calc-err "#VALUE!")))))

(defun grid-calc--deref (value)
  "Return VALUE with a reference read: one cell is its value, more an array."
  (if (grid-calc-ref-p value)
      (if (and (= (grid-calc-ref-top value) (grid-calc-ref-bottom value))
               (= (grid-calc-ref-left value) (grid-calc-ref-right value)))
          (grid-calc-ref-cell value 0 0)
        (grid-calc-ref-to-array value))
    value))

(defun grid-calc--scalar-or-array (value)
  "Return VALUE read where one value is wanted.
A range is the cell of it in the formula's row or column under
`grid-calc-legacy-arrays' outside an array context, else its values."
  (if (and (grid-calc-ref-p value)
           grid-calc-legacy-arrays (not grid-calc--array-context))
      (grid-calc--intersect value)
    (grid-calc--deref value)))

(defun grid-calc--first (value)
  "Return the one value VALUE stands for: the first of several."
  (let ((value (grid-calc--scalar-or-array value)))
    (if (grid-calc-array-p value) (grid-calc-aref value 0 0) value)))

;;;----------------------------------------------------------------------
;;; Converting values
;;;----------------------------------------------------------------------
;;
;; Each of these signals the error value VALUE is, or #VALUE! when VALUE
;; cannot be what is wanted, for `grid-calc--call' to give.

(defun grid-calc--tidy-number (number)
  "Return NUMBER as an integer when it has no fraction."
  (if (and (floatp number) (= number (ftruncate number))
           (< (abs number) 1e15))
      (truncate number)
    number))

;; Excel counts days from 1900-01-01 as 1, and counts a 29 February
;; 1900 that never was, as Lotus 1-2-3 did: serial 60.  Days from
;; 1 March 1900 on are one more than the calendar gives.

(defun grid-calc--civil-days (year month day)
  "Return the days from 1970-01-01 to YEAR, MONTH and DAY, a real date."
  (let* ((y (if (<= month 2) (1- year) year))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (doy (+ (/ (+ (* 153 (+ month (if (> month 2) -3 9))) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (/ yoe 4) (- (/ yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun grid-calc--civil-date (days)
  "Return (YEAR MONTH DAY) DAYS from 1970-01-01."
  (let* ((z (+ days 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (/ (- doe (/ doe 1460) (- (/ doe 36524)) (/ doe 146096)) 365))
         (doy (- doe (- (+ (* 365 yoe) (/ yoe 4)) (/ yoe 100))))
         (mp (/ (+ (* 5 doy) 2) 153))
         (day (1+ (- doy (/ (+ (* 153 mp) 2) 5))))
         (month (if (< mp 10) (+ mp 3) (- mp 9)))
         (year (+ yoe (* era 400) (if (<= month 2) 1 0))))
    (list year month day)))

(defun grid-calc-date-serial (year month day)
  "Return Excel's serial number of YEAR, MONTH and DAY.
A month or day outside its range carries over, so month 13 is January
of the next year and day 0 the last day of the month before."
  (let* ((months (+ (* year 12) (1- month)))
         (year (floor months 12))
         (month (1+ (mod months 12)))
         (first (grid-calc--civil-days year month 1))
         (first (+ first (if (or (> year 1900) (and (= year 1900) (> month 2)))
                             25569 25568))))
    (- (+ first (1- day)) (grid-format-date-offset))))

(defun grid-calc-serial-date (serial)
  "Return (YEAR MONTH DAY) of the whole part of Excel's SERIAL.
Serial 60 is the 29 February 1900 Excel counts, and 0 is 1900-01-00.
In the 1904 system serial 0 is 1904-01-01."
  (let ((whole (+ (floor serial) (grid-format-date-offset))))
    (cond ((= whole 60) (list 1900 2 29))
          ((= whole 0) (list 1900 1 0))
          ((< whole 60) (grid-calc--civil-date (- whole 25568)))
          (t (grid-calc--civil-date (- whole 25569))))))

(defconst grid-calc--month-names
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12))
  "Months by the first three letters of their names.")

(defconst grid-calc--time-colon-regexp "[ \t]*:[ \t]*"
  "The colon between two fields of a time, and the spaces around it.")

(defun grid-calc--parse-time (text)
  "Return the fraction of a day TEXT names as a time, or nil.
As 13:30, 1:30 PM, 13:30:15.5 or 1 PM, the spaces around a colon
allowed.  A meridiem needs an hour of 12 or less; hours and a leap
second of 60 carry into the next day."
  (let ((case-fold-search t))
    (when (string-match
           (concat "\\`\\([0-9]\\{1,2\\}\\)"
                   "\\(?:" grid-calc--time-colon-regexp "\\([0-9]\\{1,2\\}\\)"
                   "\\(?:" grid-calc--time-colon-regexp
                   "\\([0-9]\\{1,2\\}\\(?:\\.[0-9]+\\)?\\)\\)?\\)?"
                   "[ \t]*\\([ap]m?\\)?\\'")
           text)
      (let ((hour (string-to-number (match-string 1 text)))
            (minute (if (match-beginning 2)
                        (string-to-number (match-string 2 text)) 0))
            (second (if (match-beginning 3)
                        (string-to-number (match-string 3 text)) 0))
            (half (and (match-beginning 4)
                       (downcase (substring (match-string 4 text) 0 1)))))
        (when (or (match-beginning 2) half)
          (let ((valid t))
            (when half
              (if (<= 1 hour 12)
                  (setq hour (+ (% hour 12) (if (equal half "p") 12 0)))
                (setq valid nil)))
            (when (and valid (< minute 60) (<= second 60))
              (/ (+ (* hour 3600) (* minute 60) second) 86400.0))))))))

(defcustom grid-calc-date-order 'month-day-year
  "The order text such as 1/2/2024 names the day and the month in.
As Excel follows the region it runs in: `month-day-year' reads it as
2 January, the way the United States writes it, and `day-month-year'
as 1 February, the way most of the rest of the world does."
  :type '(choice (const month-day-year) (const day-month-year))
  :group 'grid-table)

(defun grid-calc--year (text)
  "Return the year written as TEXT, two digits being 1930 to 2029."
  (let ((year (string-to-number text)))
    (cond ((> (length text) 2) year)
          ((< year 30) (+ 2000 year))
          (t (+ 1900 year)))))

(defconst grid-calc--date-separator-regexp "[ \t]*[-/][ \t]*"
  "The hyphen or slash between two fields of a date, and its spaces.")

(defun grid-calc--parse-day (text)
  "Return the serial number of the date TEXT names, or nil.
As 2024-03-15, 2024/3/15, 3/15/2024, 15-Mar-2024, Mar 15, 2024 or
March 2024; `grid-calc-date-order' says which of 3/15 is the month.
The year, month and day of the first form may have spaces about the
separators, as Excel takes them."
  (let ((case-fold-search t)
        (month-regexp "\\([a-z]\\{3\\}\\)[a-z]*\\.?")
        (month-first (if (eq grid-calc-date-order 'day-month-year) 2 1)))
    (cl-flet ((date (year month day)
                (when (and (<= 1 month 12) (<= 1 day 31) (<= 1900 year 9999)
                           (<= day (nth 2 (grid-calc-serial-date
                                           (grid-calc-date-serial
                                            year (1+ month) 0)))))
                  (grid-calc-date-serial year month day)))
              (month (name) (cdr (assoc (downcase name) grid-calc--month-names))))
      (cond
       ((string-match (concat "\\`\\([0-9]\\{4\\}\\)"
                              grid-calc--date-separator-regexp
                              "\\([0-9]\\{1,2\\}\\)"
                              grid-calc--date-separator-regexp
                              "\\([0-9]\\{1,2\\}\\)\\'")
                      text)
        (date (string-to-number (match-string 1 text))
              (string-to-number (match-string 2 text))
              (string-to-number (match-string 3 text))))
       ((string-match "\\`\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{2\\}\\|[0-9]\\{4\\}\\)\\'" text)
        (date (grid-calc--year (match-string 3 text))
              (string-to-number (match-string month-first text))
              (string-to-number (match-string (- 3 month-first) text))))
       ((string-match "\\`\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\'" text)
        (date (nth 5 (decode-time))
              (string-to-number (match-string month-first text))
              (string-to-number (match-string (- 3 month-first) text))))
       ((string-match (concat "\\`\\([0-9]\\{1,2\\}\\)[- ]" month-regexp
                              "\\(?:[- ,]+\\([0-9]\\{2\\}\\|[0-9]\\{4\\}\\)\\)?\\'")
                      text)
        (when-let* ((number (month (match-string 2 text))))
          (date (if (match-beginning 3)
                    (grid-calc--year (match-string 3 text))
                  (nth 5 (decode-time)))
                number (string-to-number (match-string 1 text)))))
       ((string-match (concat "\\`" month-regexp
                              "[- ]\\([0-9]\\{1,2\\}\\)\\(?:[ ,]+\\([0-9]\\{4\\}\\)\\)?\\'")
                      text)
        (when-let* ((number (month (match-string 1 text))))
          (date (if (match-beginning 3)
                    (string-to-number (match-string 3 text))
                  (nth 5 (decode-time)))
                number (string-to-number (match-string 2 text)))))
       ((string-match (concat "\\`" month-regexp "[- ,]+\\([0-9]\\{4\\}\\)\\'")
                      text)
        (when-let* ((number (month (match-string 1 text))))
          (date (string-to-number (match-string 2 text)) number 1)))))))

(defun grid-calc--parse-date-time (text)
  "Return the serial number of the date, time or both TEXT names, or nil."
  (let ((text (string-trim text)))
    (or (grid-calc--parse-day text)
        (grid-calc--parse-time text)
        (and (string-match "\\`\\(.*[0-9a-z]\\)[ T]+\\([0-9]\\{1,2\\}:.*\\)\\'"
                           text)
             ;; Both parts are taken before reading the day, whose own
             ;; matching would lose where the time is.
             (let* ((time-text (match-string 2 text))
                    (day (grid-calc--parse-day (match-string 1 text)))
                    (time (grid-calc--parse-time time-text)))
               (and day time (+ day time)))))))

(defun grid-calc-text-number (text)
  "Return the number TEXT reads as, the way Excel reads text, or nil.
Spaces around it, thousands separators, a currency sign, a percent sign
and parentheses for a negative are allowed; a date or a time is its
serial number, but only with no spaces around it."
  (let* ((raw (substring-no-properties text))
         (text (string-trim raw)))
    (unless (string-empty-p text)
      (or (and (string-match-p "\\`[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
                               text)
               (string-to-number text))
          (let ((case-fold-search nil) (sign 1) (scale 1) (body text))
            (when (string-match "\\`(\\(.*\\))\\'" body)
              (setq sign -1 body (string-trim (match-string 1 body))))
            (when (string-match "\\`\\([-+]\\)[ ]*" body)
              (when (equal (match-string 1 body) "-") (setq sign (- sign)))
              (setq body (substring body (match-end 0))))
            (when (string-match "\\`[$€£¥][ ]*" body)
              (setq body (substring body (match-end 0))))
            (when (string-match "[ ]*%\\'" body)
              (setq scale 0.01 body (substring body 0 (match-beginning 0))))
            (when (string-match-p
                   "\\`\\(?:[0-9]\\{1,3\\}\\(?:,[0-9]\\{3\\}\\)+\\|[0-9]+\\)\\(?:\\.[0-9]*\\)?\\(?:[eE][-+]?[0-9]+\\)?\\'\\|\\`\\.[0-9]+\\'"
                   body)
              (let ((number (string-to-number (string-replace "," "" body))))
                (if (= scale 1)
                    (* sign number)
                  (* sign number scale)))))
          (and (string= text raw) (grid-calc--parse-date-time text))))))

(defun grid-calc-num (value)
  "Return VALUE as a number, as arithmetic takes it.
Blank is 0, TRUE 1 and FALSE 0; text is the number it reads as."
  (cond
   ((numberp value) value)
   ((eq value :blank) 0)
   ((eq value t) 1)
   ((eq value :false) 0)
   ((stringp value) (or (grid-calc-text-number value) (grid-calc-raise "#VALUE!")))
   ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
   ((or (grid-calc-ref-p value) (grid-calc-array-p value))
    (grid-calc-num (grid-calc--first value)))
   (t (grid-calc-raise "#VALUE!"))))

(defun grid-calc--strict-num (value)
  "Return VALUE as a number, or #VALUE! where TRUE or FALSE is given."
  (when (memq value '(t :false)) (grid-calc-raise "#VALUE!"))
  (grid-calc-num value))

(defun grid-calc-int (value)
  "Return VALUE as a number with its fraction cut off.
A number past 1e18 is #NUM!, a whole one as much as a fractional."
  (let ((number (grid-calc-num value)))
    (if (> (abs number) 1e18)
        (grid-calc-raise "#NUM!")
      (if (integerp number) number (truncate number)))))

(defun grid-calc-number-text (number)
  "Return NUMBER written as Excel writes it into text: up to 15 digits."
  (cond
   ((integerp number) (number-to-string number))
   ((or (isnan number) (> (abs number) 1e308)) "#NUM!")
   ((and (= number (ftruncate number)) (< (abs number) 1e15))
    (format "%d" (truncate number)))
   ((zerop number) "0")
   (t
    ;; 15 significant digits, then the zeros left at the end gone
    (let ((text (format "%.14e" number)))
      (string-match "\\`\\(-?\\)\\([0-9]\\)\\.\\([0-9]*\\)e\\([-+][0-9]+\\)\\'" text)
      (let* ((sign (match-string 1 text))
             (digits (string-trim-right
                      (concat (match-string 2 text) (match-string 3 text)) "0+"))
             (exponent (string-to-number (match-string 4 text))))
        (if (or (>= exponent 15) (< exponent -9)
                (and (< exponent 0) (> (- (length digits) exponent) 20)))
            (format "%s%s%sE%s%02d" sign (substring digits 0 1)
                    (if (> (length digits) 1)
                        (concat "." (substring digits 1)) "")
                    (if (< exponent 0) "-" "+") (abs exponent))
          (let ((point (1+ exponent)))
            (cond
             ((<= point 0)
              (concat sign "0." (make-string (- point) ?0) digits))
             ((>= point (length digits))
              (concat sign digits (make-string (- point (length digits)) ?0)))
             (t (concat sign (substring digits 0 point) "."
                        (substring digits point)))))))))))

(defun grid-calc-text (value)
  "Return VALUE as text, as & takes it.
Blank is empty text and a number is written with up to 15 digits."
  (cond
   ((stringp value) value)
   ((numberp value) (grid-calc-number-text value))
   ((eq value :blank) "")
   ((eq value t) "TRUE")
   ((eq value :false) "FALSE")
   ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
   ((or (grid-calc-ref-p value) (grid-calc-array-p value))
    (grid-calc-text (grid-calc--first value)))
   (t (grid-calc-raise "#VALUE!"))))

(defun grid-calc-truth (value)
  "Return non-nil when VALUE is true, as IF takes it.
A number is true unless 0; text must be TRUE or FALSE."
  (cond
   ((eq value t) t)
   ((memq value '(:false :blank)) nil)
   ((numberp value) (/= value 0))
   ((stringp value)
    (cond ((string-equal-ignore-case value "TRUE") t)
          ((string-equal-ignore-case value "FALSE") nil)
          (t (grid-calc-raise "#VALUE!"))))
   ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
   ((or (grid-calc-ref-p value) (grid-calc-array-p value))
    (grid-calc-truth (grid-calc--first value)))
   (t (grid-calc-raise "#VALUE!"))))

(defun grid-calc--type-rank (value)
  "Return where VALUE's kind sorts: numbers, then text, then truth values."
  (cond ((numberp value) 0) ((stringp value) 1) (t 2)))

(defun grid-calc--numbers-equal-p (left right)
  "Return non-nil when LEFT and RIGHT are equal to 15 digits."
  (or (= left right)
      (<= (abs (- left right))
          (* 1e-15 (max (abs left) (abs right))))))

(defun grid-calc-compare (left right)
  "Return -1, 0 or 1 as LEFT sorts before, with or after RIGHT.
Numbers sort before text and text before FALSE and TRUE; text compares
without regard to case.  Blank is 0, empty text or FALSE, as the other
side is a number, text or a truth value."
  (when (eq left :blank)
    (setq left (cond ((stringp right) "") ((grid-calc-boolean-p right) :false)
                     (t 0))))
  (when (eq right :blank)
    (setq right (cond ((stringp left) "") ((grid-calc-boolean-p left) :false)
                      (t 0))))
  (let ((left-rank (grid-calc--type-rank left))
        (right-rank (grid-calc--type-rank right)))
    (cond
     ((/= left-rank right-rank) (if (< left-rank right-rank) -1 1))
     ((numberp left)
      (cond ((grid-calc--numbers-equal-p left right) 0)
            ((< left right) -1)
            (t 1)))
     ((stringp left)
      (let ((a (downcase left)) (b (downcase right)))
        (cond ((string= a b) 0) ((string< a b) -1) (t 1))))
     (t (cond ((eq left right) 0) ((eq left :false) -1) (t 1))))))

(defun grid-calc-equal-p (left right)
  "Return non-nil when LEFT and RIGHT are equal, as = takes them."
  (= (grid-calc-compare left right) 0))

;; Filters, sorting and charts read the values cells store, outside any
;; formula; these take them as they are stored.

(defun grid-calc--numeric (value)
  "Return the stored VALUE as a number when it is one or reads as one.
Text marked as text is not a number, whatever it reads as."
  (grid-format-number value))

(defun grid-calc--equal (left right)
  "Return non-nil when the stored LEFT and RIGHT are the same value.
Numbers compare by value whether typed or computed; text ignores case."
  (let ((left-number (grid-calc--numeric left))
        (right-number (grid-calc--numeric right)))
    (cond ((and left-number right-number) (= left-number right-number))
          ((and (stringp left) (stringp right))
           (string= (downcase left) (downcase right)))
          (t (equal left right)))))

(defun grid-calc-to-number (value)
  "Return the stored VALUE as a number, 0 when it is none."
  (cond
   ((numberp value) value)
   ((stringp value)
    (or (grid-format-number (substring-no-properties value)) 0))
   (t 0)))

;;;----------------------------------------------------------------------
;;; Applying a function to each of several values
;;;----------------------------------------------------------------------

(defun grid-calc--broadcast (function values)
  "Apply FUNCTION to VALUES, or to each set of their elements.
When any of VALUES is an array the result is an array as large as the
largest, each element FUNCTION of the elements at that place; an array
of one row or column stands for each row or column."
  (if (not (cl-some #'grid-calc-array-p values))
      (apply function values)
    (let ((rows 1) (cols 1))
      (dolist (value values)
        (when (grid-calc-array-p value)
          (setq rows (max rows (grid-calc-array-rows value))
                cols (max cols (grid-calc-array-cols value)))))
      (grid-calc-array
       rows cols
       (lambda (row col)
         (apply function
                (mapcar (lambda (value)
                          (if (grid-calc-array-p value)
                              (grid-calc-aref value row col)
                            value))
                        values)))))))

(defmacro grid-calc--catching (&rest body)
  "Run BODY and return its value, or the error value it signaled."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     (grid-calc-cycle (signal (car err) (cdr err)))
     (grid-calc-error (grid-calc-err (grid-calc--error-code err)))
     (arith-error (grid-calc-err "#DIV/0!"))
     (overflow-error (grid-calc-err "#NUM!"))
     (range-error (grid-calc-err "#NUM!"))))

(defun grid-calc--error-code (err)
  "Return the error value text the formula error ERR stands for."
  (let ((code (cadr err)))
    (if (member code grid-calc-error-values) code "#VALUE!")))

;;;----------------------------------------------------------------------
;;; Operators
;;;----------------------------------------------------------------------

(defun grid-calc--arithmetic (op left right)
  "Return LEFT OP RIGHT for the arithmetic operator OP."
  (let ((a (grid-calc-num left))
        (b (grid-calc-num right)))
    (pcase op
      ('+ (grid-calc--sum-of a b))
      ('- (grid-calc--sum-of a (- b)))
      ('* (* a b))
      ('/ (if (zerop b) (grid-calc-raise "#DIV/0!") (/ (float a) b)))
      ('^ (grid-calc--power a b)))))

(defun grid-calc--sum-of (a b)
  "Return A plus B, 0 when they cancel out to within 15 digits."
  (let ((sum (+ a b)))
    (if (and (floatp sum) (/= sum 0)
             (< (abs sum) (* 1e-15 (max (abs a) (abs b)))))
        0
      sum)))

(defun grid-calc--power (base exponent)
  "Return BASE raised to EXPONENT, as ^ and POWER do."
  (cond
   ((and (zerop base) (zerop exponent)) (grid-calc-raise "#NUM!"))
   ((and (zerop base) (< exponent 0)) (grid-calc-raise "#DIV/0!"))
   ((and (< base 0) (/= exponent (truncate exponent)))
    ;; Excel has no root of a negative number, not even an odd one:
    ;; =(-8)^(1/3) is #NUM!.
    (grid-calc-raise "#NUM!"))
   ((and (integerp base) (integerp exponent) (>= exponent 0)
         (< (* exponent (log (max 2 (abs base)) 2)) 62))
    (expt base exponent))
   (t (let ((result (expt (float base) exponent)))
        (if (or (isnan result) (> (abs result) 1.7976931348623157e308))
            (grid-calc-raise "#NUM!")
          result)))))

(defun grid-calc-apply-binary-op (op left right)
  "Return LEFT OP RIGHT, each a single value."
  (grid-calc--catching
    (cond
     ((grid-calc-err-p left) left)
     ((grid-calc-err-p right) right)
     (t
      (pcase op
        ((or '+ '- '* '/ '^) (grid-calc--arithmetic op left right))
        ('& (concat (grid-calc-text left) (grid-calc-text right)))
        ('= (grid-calc-bool (= (grid-calc-compare left right) 0)))
        ('<> (grid-calc-bool (/= (grid-calc-compare left right) 0)))
        ('< (grid-calc-bool (< (grid-calc-compare left right) 0)))
        ('> (grid-calc-bool (> (grid-calc-compare left right) 0)))
        ('<= (grid-calc-bool (<= (grid-calc-compare left right) 0)))
        ('>= (grid-calc-bool (>= (grid-calc-compare left right) 0)))
        ;; grid-table's own: A1>0 AND B1>0
        ('AND (grid-calc-bool (and (grid-calc-truth left)
                                   (grid-calc-truth right))))
        ('OR (grid-calc-bool (or (grid-calc-truth left)
                                 (grid-calc-truth right))))
        (_ (error "Unknown binary operator: %s" op)))))))

(defun grid-calc-apply-unary-op (op operand)
  "Return OP applied to OPERAND, a single value."
  (grid-calc--catching
    (if (grid-calc-err-p operand)
        operand
      (pcase op
        ('+ (if (eq operand :blank) 0 operand))
        ('- (- (grid-calc-num operand)))
        ('% (/ (grid-calc-num operand) 100.0))
        ('NOT (grid-calc-bool (not (grid-calc-truth operand))))
        (_ (error "Unknown unary operator: %s" op))))))

;;;----------------------------------------------------------------------
;;; Evaluating a formula
;;;----------------------------------------------------------------------

(defun grid-calc--reference (model ast)
  "Return the reference the CELL-REF or RANGE AST names in MODEL.
One through several sheets is the union of its area on each."
  (if-let* ((span (grid-calc--sheet-span (grid-calc--reference-sheet ast))))
      (let ((areas (mapcar (lambda (sheet)
                             (grid-calc--reference
                              model (grid-calc--on-sheet ast sheet)))
                           span)))
        (if (cdr areas) (cons 'union areas) (car areas)))
    (grid-calc--one-sheet-reference model ast)))

(defun grid-calc--one-sheet-reference (model ast)
  "Return the reference the CELL-REF or RANGE AST names in MODEL.
AST names one sheet at most."
  (pcase ast
    (`(CELL-REF ,text . ,sheet)
     (let ((target (grid-calc--sheet-model model (car sheet)))
           (at (grid-calc--reference-coords text)))
       (unless (and at (car at) (cdr at))
         ;; A whole row or column on its own, as 3:3 is
         (setq at (grid-calc--reference-coords text)))
       (pcase-let ((`(,top . ,left) at))
         (if (and top left)
             (grid-calc-ref--make target top left top left)
           (grid-calc-err "#REF!")))))
    (`(RANGE ,start ,end)
     (pcase-let ((`(,target ,top ,bottom ,left ,right)
                  (grid-calc--range-bounds model start end))
                 (whole (grid-calc--range-whole start end)))
       (if (and (<= top bottom) (<= left right))
           (grid-calc-ref--make target top left bottom right whole)
         ;; A whole column of a sheet without rows
         (grid-calc-ref--make target top left (max top bottom)
                              (max left right) whole))))))

(defun grid-calc--range-whole (start-cell-ref-ast end-cell-ref-ast)
  "Return `column' when a range takes whole columns, `row' for whole rows."
  (let ((start (grid-calc--reference-coords (cadr start-cell-ref-ast)))
        (end (grid-calc--reference-coords (cadr end-cell-ref-ast))))
    (cond ((and start end (null (car start)) (null (car end))) 'column)
          ((and start end (null (cdr start)) (null (cdr end))) 'row))))

;;; Structured references, as Table1[[#Headers],[Qty]:[Price]]

(defun grid-calc--struct-unescape (text)
  "Return the column name TEXT with its escapes taken out.
A column name escapes [, ], # and ' with a ' before them."
  (replace-regexp-in-string "'\\(.\\)" "\\1" text t))

(defun grid-calc--struct-parts (text)
  "Return the parts of the bracketed TEXT of a structured reference.
A list of (ITEM . KEYWORD), for #All, #Data, #Headers, #Totals and #This
Row, and of (COLUMN NAME) and (COLUMNS FIRST LAST)."
  (let ((pos 1) (end (1- (length text))) parts)
    (unless (and (> (length text) 1) (eq (aref text 0) ?\[)
                 (eq (aref text end) ?\]))
      (signal 'grid-calc-error '("#REF!")))
    (cl-labels
        ((bracket ()
           ;; The text of one [...] at POS, escapes left in
           (let ((start (1+ pos)) (at (1+ pos)))
             (while (and (< at end) (not (eq (aref text at) ?\])))
               (setq at (+ at (if (eq (aref text at) ?') 2 1))))
             (prog1 (substring text start (min at end))
               (setq pos (1+ at)))))
         (skip ()
           (while (and (< pos end) (memq (aref text pos) '(?\s ?,)))
             (cl-incf pos)))
         (item (word)
           (pcase (downcase (string-trim word))
             ("#all" '(ITEM . all)) ("#data" '(ITEM . data))
             ("#headers" '(ITEM . headers)) ("#totals" '(ITEM . totals))
             ("#this row" '(ITEM . this-row))
             (_ (if (string-prefix-p "#" (string-trim word))
                    (signal 'grid-calc-error '("#REF!"))
                  (list 'COLUMN (grid-calc--struct-unescape word))))))
         (column-or-span (first)
           ;; FIRST, or FIRST:[LAST]
           (if (and (< pos end) (eq (aref text pos) ?:))
               (progn (cl-incf pos)
                      (unless (eq (aref text pos) ?\[)
                        (signal 'grid-calc-error '("#REF!")))
                      (list 'COLUMNS (grid-calc--struct-unescape first)
                            (grid-calc--struct-unescape (bracket))))
             (item first))))
      (cond
       ((>= pos end))
       ;; [@], [@Qty] and [@[Unit Price]]
       ((eq (aref text pos) ?@)
        (push '(ITEM . this-row) parts)
        (cl-incf pos)
        (cond ((>= pos end))
              ((eq (aref text pos) ?\[)
               (push (column-or-span (bracket)) parts))
              (t (push (list 'COLUMN (grid-calc--struct-unescape
                                      (substring text pos end)))
                       parts))))
       ;; [[#Headers],[Qty]:[Price]]
       ((eq (aref text pos) ?\[)
        (while (progn (skip) (< pos end))
          (unless (eq (aref text pos) ?\[)
            (signal 'grid-calc-error '("#REF!")))
          (push (column-or-span (bracket)) parts)))
       ;; [Qty] and [#Totals]
       (t (push (item (substring text pos end)) parts))))
    (nreverse parts)))

(defun grid-calc--same-column-p (one other)
  "Return non-nil when the column names ONE and OTHER are the same.
Case does not count, nor how a line break is written: a formula may
write it as CR LF where the table has LF."
  (cl-flet ((lines (name) (replace-regexp-in-string "\r\n?" "\n" name t t)))
    (string-equal-ignore-case (lines one) (lines other))))

(defun grid-calc--struct-area (table text row)
  "Return (TOP LEFT BOTTOM RIGHT) of TABLE that the bracketed TEXT takes.
TABLE is as `grid-calc-tables' returns it; ROW is the row of the formula,
for #This Row."
  (pcase-let* ((`(,_ ,top ,left ,bottom ,right ,headers ,totals ,columns) table)
               (parts (grid-calc--struct-parts text))
               (items (cl-loop for part in parts
                               when (eq (car part) 'ITEM) collect (cdr part)))
               (spans (cl-remove-if (lambda (part) (eq (car part) 'ITEM)) parts))
               (data-top (+ top headers))
               (data-bottom (- bottom totals)))
    (cl-flet ((column (name)
                (or (cl-position name columns :test #'grid-calc--same-column-p)
                    (signal 'grid-calc-error '("#REF!")))))
      (pcase-let
          ((`(,first-row . ,last-row)
            (pcase (sort (copy-sequence items) #'string<)
              ('() (cons data-top data-bottom))
              ((pred (memq 'all)) (cons top bottom))
              ('(this-row)
               (if (and row (<= data-top row data-bottom))
                   (cons row row)
                 (signal 'grid-calc-error '("#VALUE!"))))
              ('(data) (cons data-top data-bottom))
              ('(headers)
               (if (> headers 0) (cons top (1- data-top))
                 (signal 'grid-calc-error '("#REF!"))))
              ('(totals)
               (if (> totals 0) (cons (1+ data-bottom) bottom)
                 (signal 'grid-calc-error '("#REF!"))))
              ('(data headers) (cons top data-bottom))
              ('(data totals) (cons data-top bottom))
              (_ (signal 'grid-calc-error '("#REF!")))))
           (`(,first-col . ,last-col)
            (pcase spans
              ('() (cons left right))
              (`((COLUMN ,name)) (let ((at (+ left (column name)))) (cons at at)))
              (`((COLUMNS ,from ,to))
               (let ((a (+ left (column from))) (b (+ left (column to))))
                 (cons (min a b) (max a b))))
              (_ (signal 'grid-calc-error '("#REF!"))))))
        (when (> first-row last-row)
          ;; A table without data rows
          (signal 'grid-calc-error '("#REF!")))
        (list first-row first-col last-row last-col)))))

(defun grid-calc--struct-table (model name)
  "Return the table NAME, or the one the formula computed is in, or nil."
  (and grid-calc-tables
       (if name
           (funcall grid-calc-tables name)
         (funcall grid-calc-tables nil (or (car grid-calc--here) model)
                  (nth 1 grid-calc--here) (nth 2 grid-calc--here)))))

(defun grid-calc--struct-reference (model name text)
  "Return the reference the structured reference NAME[TEXT] makes on MODEL."
  (let ((table (grid-calc--struct-table model name)))
    (unless table (signal 'grid-calc-error '("#REF!")))
    (pcase-let ((`(,top ,left ,bottom ,right)
                 (grid-calc--struct-area table text (nth 1 grid-calc--here))))
      (grid-calc-ref--make (car table) top left bottom right))))

(defun grid-calc--areas (value)
  "Return the areas of VALUE, a reference or a union of them.
nil when VALUE is neither."
  (cond ((grid-calc-ref-p value) (list value))
        ((eq (car-safe value) 'union) (cdr value))))

(defun grid-calc--span (left right)
  "Return the reference from the corner of LEFT to that of RIGHT.
Both are references into the same sheet, or unions of them; the result
takes in every area of both."
  (let* ((areas (append (grid-calc--areas left) (grid-calc--areas right)))
         (model (and areas (grid-calc-ref-model (car areas)))))
    (if (not (and (grid-calc--areas left) (grid-calc--areas right)
                  (cl-every (lambda (area) (eq (grid-calc-ref-model area) model))
                            areas)))
        (grid-calc-err "#VALUE!")
      (grid-calc-ref--make model
                           (apply #'min (mapcar #'grid-calc-ref-top areas))
                           (apply #'min (mapcar #'grid-calc-ref-left areas))
                           (apply #'max (mapcar #'grid-calc-ref-bottom areas))
                           (apply #'max (mapcar #'grid-calc-ref-right areas))))))

(defun grid-calc--area-intersection (left right)
  "Return the cells LEFT and RIGHT, two areas of one sheet, have in common.
nil when they have none."
  (let ((top (max (grid-calc-ref-top left) (grid-calc-ref-top right)))
        (left-edge (max (grid-calc-ref-left left) (grid-calc-ref-left right)))
        (bottom (min (grid-calc-ref-bottom left) (grid-calc-ref-bottom right)))
        (right-edge (min (grid-calc-ref-right left) (grid-calc-ref-right right))))
    (and (<= top bottom) (<= left-edge right-edge)
         (grid-calc-ref--make (grid-calc-ref-model left)
                              top left-edge bottom right-edge))))

(defun grid-calc--intersection (left right)
  "Return the cells LEFT and RIGHT, references or unions, have in common.
Each area of one meets each area of the other, as in Excel; #NULL!
when none meet."
  (let ((as (grid-calc--areas left))
        (bs (grid-calc--areas right)))
    (if (not (and as bs
                  (cl-every (lambda (area)
                              (eq (grid-calc-ref-model area)
                                  (grid-calc-ref-model (car as))))
                            (append as bs))))
        (grid-calc-err "#VALUE!")
      (let ((areas (cl-loop for a in as
                            append (cl-loop for b in bs
                                            for area = (grid-calc--area-intersection a b)
                                            when area collect area))))
        (cond ((null areas) (grid-calc-err "#NULL!"))
              ((cdr areas) (cons 'union areas))
              (t (car areas)))))))

;;;----------------------------------------------------------------------
;;; Where a formula takes one value of a range
;;;----------------------------------------------------------------------

;; A formula that is not an array formula takes one value of a range
;; wherever Excel's tokens for it say so, inside a function's arguments
;; as well as at its top: =MAX(ABS(A1:B1)) in B4 is MAX(ABS(B1)), while
;; =SUMPRODUCT(ABS(A1:B1)) takes the whole range.  Each token has a
;; class, a reference, a value or an array, and each argument of a
;; function converts the class of the tokens in it as
;; `grid-calc-argument-classes' says.  The rules below follow
;; RecalcTokenClass in LibreOffice's Excel export,
;; sc/source/filter/excel/xeformula.cxx (Mozilla Public License 2.0).

(defvar grid-calc-functions)

(defvar grid-calc--classes nil
  "The class of each part of the formula being computed, or nil.
A table from the parts of its parsed form to REF, VAL or ARR, made by
`grid-calc--token-classes' for a formula read as a workbook file holds
it.  A part whose class is VAL comes to one value.")

(defvar grid-calc--class-cache (make-hash-table :test #'eq :weakness 'key)
  "Each parsed formula's table of classes, once worked out.")

(defconst grid-calc--class-overrides
  `(;; Excel takes the third argument on as an array, whatever the
    ;; table says: AGGREGATE(14,6,A1:A9/(B1:B9="x"),1) needs no array
    ;; formula.
    ("AGGREGATE" V (VR RO RA))
    ;; Excel's TYPE of several cells is 64, not the type of one of them.
    ("TYPE" V (RX))
    ;; Newer than the tables, and they give references: A1# is
    ;; ANCHORARRAY(A1), and SUM(XLOOKUP(2,A1:A3,B1:D3)) adds a row.
    ("ANCHORARRAY" R (RO))
    ("XLOOKUP" R (VR RA RA VR VR VR))
    ;; Like IF and CHOOSE, they give the reference they choose:
    ;; SUM((A1,IFS(TRUE,B1))) adds both cells.
    ("IFS" R (VO RO) . t)
    ("SWITCH" R (VR RO))
    ;; A name LET or LAMBDA gives a reference is that reference:
    ;; LET(x,A1,ISREF(x)) is TRUE.
    ("LET" R (RO))
    ("LAMBDA" V (RO))
    ("(APPLY)" R (RO))
    ;; Newer than the tables, and it takes its range whole.
    ("ARRAYTOTEXT" V (RA VR))
    ;; The tables take these for value arguments, but Excel hands them
    ;; the reference, as the Analysis ToolPak's: in an array formula
    ;; ERF.PRECISE(A1:C1) is #VALUE!, not the ERF of each cell.
    ,@(mapcar (lambda (name) (list name 'V '(RR)))
              '("ERF.PRECISE" "ERFC.PRECISE" "IMCOSH" "IMCOT" "IMCSC"
                "IMCSCH" "IMSEC" "IMSECH" "IMSINH" "IMTAN")))
  "Classes that stand in for those of `grid-calc-argument-classes'.")

(defun grid-calc--kind-class (kind)
  "Return the class of an argument of grid-table's KIND, for a function
Excel's tables leave out."
  (pcase kind
    ((or 'values 'array 'areas) 'RA)
    ('ref 'RO)
    (_ 'VR)))

(defun grid-calc--function-classes (name)
  "Return (RETURN CLASSES . PAIRS) for the function NAME.
A function Excel's tables leave out takes its arguments as it always
has: a range whole where it takes several values, one value elsewhere."
  (let ((name (upcase name)))
    (or (cdr (assoc name grid-calc--class-overrides))
        (gethash name grid-calc-argument-classes)
        (let ((function (gethash name grid-calc-functions)))
          (if (and function (not (functionp function))
                   (not (grid-calc-function-legacy function)))
              (list 'V (mapcar #'grid-calc--kind-class
                               (append (grid-calc-function-kinds function)
                                       (list (or (grid-calc-function-rest function)
                                                 'any)))))
            (list 'V '(RA)))))))

(defun grid-calc--argument-class (classes index pairs)
  "Return the class of the argument at INDEX, from CLASSES.
The last of CLASSES repeats, or with PAIRS the last two in turn."
  (let ((count (length classes)))
    (cond ((< index count) (nth index classes))
          ((and pairs (>= count 2))
           (nth (+ count -2 (% (- index count) 2)) classes))
          (t (car (last classes))))))

(defun grid-calc--token-class (ast)
  "Return the class AST's token has before any conversion.
REF for a reference, VAL for a value, ARR for an array, nil for an
operator."
  (pcase (car-safe ast)
    ((or 'CELL-REF 'RANGE 'STRUCT-REF 'NAME) 'REF)
    ('ARRAY 'ARR)
    ('FUNCTION-CALL
     (pcase (car (grid-calc--function-classes (cadr ast)))
       ('R 'REF) ('A 'ARR) (_ 'VAL)))
    ((or 'BINARY-OP 'UNARY-OP 'RANGE-OP 'INTERSECT 'UNION) nil)
    (_ 'VAL)))

(defun grid-calc--operands (ast)
  "Return each operand of AST with how it is converted.
Each is (OPERAND CONVERSION . VALUE-TYPE)."
  (pcase ast
    (`(FUNCTION-CALL ,name ,arguments)
     (pcase-let ((`(,_ ,classes . ,pairs) (grid-calc--function-classes name)))
       (cl-loop for argument in arguments
                for index from 0
                collect
                (let ((class (symbol-name (grid-calc--argument-class
                                           classes index pairs))))
                  (cons argument
                        (cons (pcase (aref class 1)
                                (?O 'ORG) (?V 'VAL) (?A 'ARR)
                                (?R 'RPT) (?X 'RPX))
                              (eq (aref class 0) ?V)))))))
    (`(UNARY-OP @ ,operand) (list (cons operand (cons 'ORG nil))))
    (`(UNARY-OP ,_ ,operand) (list (cons operand (cons 'RPO t))))
    (`(BINARY-OP ,_ ,left ,right)
     (list (cons left (cons 'RPO t)) (cons right (cons 'RPO t))))
    ((or `(RANGE-OP ,left ,right) `(INTERSECT ,left ,right))
     (list (cons left (cons 'RPO nil)) (cons right (cons 'RPO nil))))
    (`(UNION . ,parts)
     (mapcar (lambda (part) (cons part (cons 'RPO nil))) parts))))

(defun grid-calc--classify (ast table conversion value-type
                                previous previous-class was-ref)
  "Record in TABLE the class of AST and of each part of it.
AST stands where CONVERSION applies, in an argument that takes a value
when VALUE-TYPE is non-nil.  PREVIOUS is the conversion of the token it
is an operand of and PREVIOUS-CLASS the class conversion done there;
WAS-REF is non-nil when that token is a reference."
  (let* ((class (grid-calc--token-class ast))
         (class (if (and value-type (eq class 'REF)) 'VAL class))
         (conversion (if (eq conversion 'RPO) previous conversion))
         (class-conversion
          (pcase conversion
            ((or 'ORG 'VAL 'ARR) conversion)
            ('RPT (pcase previous
                    ((or 'ORG 'VAL 'ARR) (if was-ref 'VAL previous-class))
                    ('RPT previous-class)
                    (_ (if was-ref previous-class 'ORG))))
            (_ (if (or (eq class 'REF) (eq previous-class 'ARR))
                   previous-class
                 'ORG)))))
    (pcase class-conversion
      ('VAL (when (eq class 'ARR) (setq class 'VAL)))
      ('ARR (when (eq class 'VAL) (setq class 'ARR))))
    (if class
        (puthash ast class table)
      ;; The reference an operator makes has no token of its own, but
      ;; is read as one would be: =E:F 11:12 in A9 is #VALUE!.
      (when (memq (car-safe ast) '(RANGE-OP INTERSECT UNION))
        (puthash ast (cond ((not value-type) 'REF)
                           ((eq class-conversion 'ARR) 'ARR)
                           (t 'VAL))
                 table)))
    (dolist (operand (grid-calc--operands ast))
      (grid-calc--classify (car operand) table (cadr operand) (cddr operand)
                           conversion class-conversion (eq class 'REF)))))

(defun grid-calc--token-classes (ast)
  "Return the table of classes of the cell formula AST.
See `grid-calc--classes'."
  (or (gethash ast grid-calc--class-cache)
      (let ((table (make-hash-table :test #'eq)))
        ;; A cell formula starts as one value.
        (grid-calc--classify ast table 'VAL t 'VAL 'VAL nil)
        (puthash ast table grid-calc--class-cache))))

(defun grid-calc--one-value (ast value)
  "Return the one value VALUE, that of AST, comes to as a value token.
A range is its cell in the formula's row or column.  Several values
written out or named are the first; those a function or an operator
computes stay as they are, as SIGN's do in SUM(SIGN(FREQUENCY(...))):
its value class only says how it reads the references given it."
  (cond ((grid-calc-ref-p value) (grid-calc--intersect value))
        ((eq (car-safe value) 'union) (grid-calc-err "#VALUE!"))
        ((and (grid-calc-array-p value) (memq (car-safe ast) '(ARRAY NAME)))
         (grid-calc-aref value 0 0))
        (t value)))

(defun grid-calc--eval (model ast)
  "Return the value of AST, a parsed formula, computed on MODEL's sheet.
A reference stays a `grid-calc-ref'; see `grid-calc--scalar-or-array'.
Under `grid-calc--classes' a part whose class is VAL is one value."
  (let ((value (grid-calc--eval-node model ast)))
    (if (and grid-calc--classes
             (eq (gethash ast grid-calc--classes) 'VAL))
        (grid-calc--one-value ast value)
      value)))

(defun grid-calc--eval-node (model ast)
  "Return the value of AST, a parsed formula, computed on MODEL's sheet,
whatever its class.  See `grid-calc--eval'."
  (pcase ast
    (`(NUMBER ,value) value)
    (`(STRING ,value) value)
    (`(LITERAL ,value) (grid-calc-bool value))
    (`(ERROR ,code) (grid-calc-err code))
    ('(MISSING) :blank)
    ;; A value computed already, as `grid-calc-call-function' passes one.
    (`(QUOTED ,value) value)
    ((or `(CELL-REF . ,_) `(RANGE . ,_))
     (condition-case err
         (grid-calc--reference model ast)
       (grid-calc-error (grid-calc-err (grid-calc--error-code err)))))
    (`(STRUCT-REF ,name ,text)
     (condition-case err
         (grid-calc--struct-reference model name text)
       (grid-calc-error (grid-calc-err (grid-calc--error-code err)))))
    (`(ARRAY ,rows)
     (grid-calc-array-from-rows
      (mapcar (lambda (row) (mapcar (lambda (item) (grid-calc--eval model item))
                                    row))
              rows)))
    (`(NAME ,name . ,qualifier)
     (if-let* ((local (assoc (upcase name) grid-calc--locals)))
         ;; An argument a LAMBDA was not given is blank.
         (if (eq (cdr local) 'grid-calc--omitted) :blank (cdr local))
       (let ((definition (grid-calc--name-ast name (car qualifier))))
         (if (or (null definition)
                 ;; A name defined by way of itself
                 (member (upcase name) grid-calc--names-open))
             (grid-calc-err "#NAME?")
           (let ((grid-calc--names-open (cons (upcase name) grid-calc--names-open))
                 (row (nth 1 grid-calc--here))
                 (col (nth 2 grid-calc--here)))
             (grid-calc--eval model (if (and row col)
                                        (grid-calc--relative-to definition row col)
                                      definition)))))))
    (`(RANGE-OP ,left ,right)
     (let ((a (grid-calc--eval model left))
           (b (grid-calc--eval model right)))
       (cond ((grid-calc-err-p a) a)
             ((grid-calc-err-p b) b)
             (t (grid-calc--span a b)))))
    (`(INTERSECT ,left ,right)
     (let ((a (grid-calc--eval model left))
           (b (grid-calc--eval model right)))
       (cond ((grid-calc-err-p a) a)
             ((grid-calc-err-p b) b)
             (t (grid-calc--intersection a b)))))
    (`(UNION . ,parts)
     ;; Several areas: only functions that take references read them
     ;; whole.  A union in a union adds its areas, and each part must
     ;; be a reference: (A1,B1%) is #VALUE!.
     (let ((values (mapcar (lambda (part) (grid-calc--eval model part)) parts)))
       (or (cl-find-if #'grid-calc-err-p values)
           (if (cl-every #'grid-calc--areas values)
               (cons 'union (mapcan (lambda (value)
                                      (copy-sequence (grid-calc--areas value)))
                                    values))
             (grid-calc-err "#VALUE!")))))
    (`(BINARY-OP ,op ,left ,right)
     (let ((a (grid-calc--scalar-or-array (grid-calc--eval model left)))
           (b (grid-calc--scalar-or-array (grid-calc--eval model right))))
       (grid-calc--broadcast (lambda (x y) (grid-calc-apply-binary-op op x y))
                             (list a b))))
    (`(UNARY-OP @ ,operand)
     (let ((value (grid-calc--eval model operand)))
       (if (grid-calc-ref-p value)
           (let ((grid-calc-legacy-arrays t) (grid-calc--array-context nil))
             (grid-calc--intersect value))
         (if (grid-calc-array-p value) (grid-calc-aref value 0 0) value))))
    (`(UNARY-OP ,op ,operand)
     (let ((value (grid-calc--scalar-or-array (grid-calc--eval model operand))))
       (grid-calc--broadcast (lambda (x) (grid-calc-apply-unary-op op x))
                             (list value))))
    (`(FUNCTION-CALL ,name ,arguments)
     (grid-calc--call model name arguments))
    (`(ELISP-CODE ,code)
     (if grid-table-allow-elisp
         (let ((cell (lambda (ref) (grid-calc-get-cell-value model ref))))
           (ignore cell)
           (condition-case err
               (eval (read code))
             (error (format "Elisp Error: %s" (error-message-string err)))))
       (error "Elisp disabled: see `grid-table-allow-elisp'")))
    (_ (error "Unknown AST node type: %s" ast))))

(defun grid-calc--eval-formula (model ast &optional row col spill)
  "Return what the formula AST at ROW and COL of MODEL comes to.
The value is in the form a cell stores; see `grid-calc--store'.  With
SPILL it is returned as the formula computed it, an array of several
values included, for `grid-calc--spill' to store."
  (let* ((grid-calc--here (list model row col))
         (classes (and grid-calc-legacy-arrays row col
                       (not grid-calc--array-formula) (not spill)
                       (not (eq (car-safe ast) 'ELISP-CODE))
                       (grid-calc--token-classes ast)))
         (grid-calc--classes classes)
         ;; With classes, they alone say where a range is one value.
         (grid-calc--array-context
          (or grid-calc--array-formula spill classes))
         (value (grid-calc--eval model ast)))
    (if (eq (car-safe ast) 'ELISP-CODE)
        value
      (let ((value (if (grid-calc-ref-p value)
                       (grid-calc--scalar-or-array value)
                     value)))
        (if spill
            value
          (grid-calc--store value))))))

(defun grid-calc-eval-ast (model ast)
  "Return the value of the parsed formula AST on MODEL's sheet.
A single value is in the form a cell stores; several values are a list
of them, row after row."
  (let* ((grid-calc--here (list model nil nil))
         (value (grid-calc--deref (grid-calc--eval model ast))))
    (if (grid-calc-array-p value)
        (mapcar #'grid-calc--store (grid-calc-array-values value))
      (grid-calc--store value))))

(defun grid-calc-get-cell-value (model ref-str)
  "Return the value of the cell REF-STR names in MODEL, as a cell stores it."
  (let ((ref (grid-calc-parse-cell-ref ref-str)))
    (when ref
      (let ((value (grid-calc--cell model (plist-get ref :row)
                                    (plist-get ref :col))))
        (unless (eq value :blank)
          (grid-calc--store value))))))

;;;----------------------------------------------------------------------
;;; Functions
;;;----------------------------------------------------------------------
;;
;; A function says what each of its arguments is to be, and
;; `grid-calc--call' makes it so before running the function's body:
;;
;;   number   a number, as arithmetic reads one (text that reads as a
;;            number, TRUE as 1, blank as 0);
;;   integer  a number with its fraction cut off;
;;   text     text, as & writes values;
;;   bool     TRUE or FALSE, as IF reads a condition: t or nil;
;;   date     a day, the serial number of a date without its time (text
;;            that reads as a date, a number from 0 to 9999's end);
;;   any      one value of any kind, a range being the cell of it in the
;;            formula's row or column;
;;   values   the argument as written: a `grid-calc-ref', an array or a
;;            single value, for functions that take several values, as
;;            SUM does;
;;   areas    as values, but several areas, as (A1:A3,C1:C3), stay one
;;            argument, for RANK and its like to read them all;
;;   array    an array, whatever was written: a range is its values and
;;            one value an array of one;
;;   ref      a reference, `grid-calc-ref', or #VALUE!;
;;   lazy     the argument unevaluated, for `grid-calc-arg' to compute
;;            when the function wants it, as IF's branches are.
;;
;; An argument of the first six kinds that is an error value makes the
;; function give that error, unless the function says :errors.  One that
;; is several values makes the function give several values, computed for
;; each of them.  A function that says :toolpak t, one Excel once took
;; from the Analysis ToolPak, gives #VALUE! for TRUE or FALSE in any of
;; them, and checks each date as it reads it, where the others check the
;; dates once all are read.

(cl-defstruct (grid-calc-function (:constructor grid-calc-function--make))
  "A function formulas can call.
KINDS lists the kind of each argument and REST the kind of those past
them, or nil; MIN is how many must be given.  ERRORS non-nil takes error
values in as arguments.  BODY is the Lisp function.  LEGACY marks one
defined with `grid-calc-defun'.  EMPTY, when non-nil, lists the default
of each argument, which then also stands in for one left empty, as in
SORT(A1:A3,,-1); otherwise an empty argument is blank, so 0 or FALSE.
TOOLPAK non-nil marks one Excel once took from the Analysis ToolPak."
  name kinds rest min errors body legacy empty toolpak)

(defvar grid-calc-functions (make-hash-table :test 'equal)
  "The functions formulas can call, by their names in capitals.")

(defvar grid-calc--model nil
  "The model of the sheet whose formula calls the function being run.")

(defconst grid-calc--kinds
  '(number integer text bool date any values areas array ref lazy)
  "The kinds an argument may be; see the commentary above.")

(defun grid-calc--parse-arglist (arglist)
  "Return (NAMES KINDS REST-KIND MIN DEFAULTS) for a function's ARGLIST.
Each element is NAME, of kind any, or (NAME KIND [DEFAULT]), with
&optional and &rest as in Lisp."
  (let (names kinds defaults rest optional (min 0) state)
    (dolist (item arglist)
      (cond
       ((eq item '&optional) (setq state 'optional))
       ((eq item '&rest) (setq state 'rest))
       (t
        (let ((name (if (consp item) (car item) item))
              (kind (if (consp item) (nth 1 item) 'any))
              (default (and (consp item) (nth 2 item))))
          (unless (memq kind grid-calc--kinds)
            (error "Unknown argument kind %s" kind))
          (if (eq state 'rest)
              (setq rest (cons name kind))
            (push name names)
            (push kind kinds)
            (push default defaults)
            (if (eq state 'optional) (setq optional t)
              (unless optional (cl-incf min))))))))
    (list (nreverse names) (nreverse kinds) rest min (nreverse defaults))))

(defmacro grid-calc-define (name arglist &rest body)
  "Define NAME as a function formulas can call, taking ARGLIST.
See the commentary above for ARGLIST's kinds.  BODY may start with a
docstring and with :errors t, to take error values as arguments, with
:empty t, for an empty argument to take its default, and with :toolpak
t, for one of the Analysis ToolPak.  It
returns a value as a formula has it, or signals one with
`grid-calc-raise'; for TRUE and FALSE see `grid-calc-bool'."
  (declare (indent 2) (doc-string 3))
  (pcase-let ((`(,names ,kinds ,rest ,min ,defaults)
               (grid-calc--parse-arglist arglist))
              (errors nil) (empty nil) (toolpak nil))
    (when (stringp (car body)) (pop body))
    (while (and (keywordp (car body)) (cdr body))
      (pcase (pop body)
        (:errors (setq errors (pop body)))
        (:empty (setq empty (pop body)))
        (:toolpak (setq toolpak (pop body)))
        (key (error "Unknown keyword %s" key))))
    `(puthash ,(upcase (symbol-name name))
              (grid-calc-function--make
               :name ,(upcase (symbol-name name))
               :kinds ',kinds :rest ',(cdr rest) :min ,min :errors ,errors
               :toolpak ,toolpak
               :empty ,(and empty `(list ,@defaults))
               ;; A default stands in only for an argument left out, not
               ;; for one that came to nil, as FALSE does for bool.
               :body (cl-function
                      (lambda (,@(cl-subseq names 0 min)
                               ,@(and (< min (length names)) '(&optional))
                               ,@(cl-loop for arg in (nthcdr min names)
                                          for default in (nthcdr min defaults)
                                          collect (if default (list arg default) arg))
                               ,@(and rest (list '&rest (car rest))))
                        ,@body)))
              grid-calc-functions)))

(defmacro grid-calc-alias (name original)
  "Make NAME call the same function as ORIGINAL."
  `(puthash ,(upcase (symbol-name name))
            (gethash ,(upcase (symbol-name original)) grid-calc-functions)
            grid-calc-functions))

(defvar grid-calc--lazy-model nil
  "The model a lazy argument is computed on.")

(defun grid-calc-arg (ast &optional kind)
  "Compute the lazy argument AST, as an argument of KIND.
KIND is any by default.  An error value signals, unless KIND is values."
  (when ast
    (let ((value (grid-calc--eval grid-calc--lazy-model ast)))
      (pcase (or kind 'any)
        ('values value)
        (_ (grid-calc--coerce (grid-calc--scalar-or-array value) kind))))))

(defun grid-calc--coerce (value kind)
  "Return VALUE, one value, as an argument of KIND is given it.
An error value signals."
  (if (grid-calc-err-p value)
      (grid-calc-raise (grid-calc-err-code value))
    (pcase kind
      ('number (grid-calc-num value))
      ('integer (grid-calc-int value))
      ('text (grid-calc-text value))
      ('bool (grid-calc-truth value))
      ('date (grid-calc--day-number value))
      (_ value))))

(defun grid-calc--scalar-kind-p (kind)
  "Return non-nil when an argument of KIND is one value."
  (memq kind '(number integer text bool date any)))

(defun grid-calc--argument (model ast kind &optional reference)
  "Return the argument AST computed on MODEL as one of KIND wants it.
A value for a scalar KIND may still be an array, or an error value.
REFERENCE non-nil says Excel hands the function the reference itself,
as it does those it took from the Analysis ToolPak: a range of several
cells is then #VALUE! where one value is wanted, as in DELTA(A1:A3),
though an array is taken a value at a time."
  (pcase kind
    ('lazy ast)
    ('ref
     ;; A reference, whatever Excel would make of it
     (let ((value (grid-calc--eval-node model ast)))
       (if (or (grid-calc-ref-p value) (grid-calc-err-p value)
               (eq (car-safe value) 'union))
           value
         (grid-calc-err "#VALUE!"))))
    ((or 'values 'array 'areas)
     (let* ((grid-calc--array-context t)
            (value (grid-calc--eval model ast)))
       ;; Several areas are only for an argument that may repeat, which
       ;; `grid-calc--call' spreads them over; elsewhere one is wanted.
       (when (eq (car-safe value) 'union)
         (setq value (if (eq kind 'areas) value (grid-calc-err "#VALUE!"))))
       (if (eq kind 'array)
           (let ((value (grid-calc--deref value)))
             (if (grid-calc-array-p value) value
               (grid-calc-array--make 1 1 (vector value))))
         value)))
    (_
     (let ((value (grid-calc--eval model ast)))
       (when (or (eq (car-safe value) 'union)
                 (and reference (grid-calc-ref-p value)
                      (not (and (= (grid-calc-ref-top value)
                                   (grid-calc-ref-bottom value))
                                (= (grid-calc-ref-left value)
                                   (grid-calc-ref-right value))))))
         (setq value (grid-calc-err "#VALUE!")))
       (grid-calc--scalar-or-array value)))))

(defun grid-calc--call (model name arguments)
  "Return what the function NAME gives for the ARGUMENTS ASTs on MODEL."
  (let ((function (gethash (upcase name) grid-calc-functions)))
    ;; A plain Lisp function in the table is called as the old engine
    ;; called every function: with the model and the values.
    (when (functionp function)
      (setq function (grid-calc-function--make :name name :body function
                                               :legacy t)))
    (cond
     ((null function)
      ;; A name LET or the workbook gives a LAMBDA calls it.
      (let ((callee (grid-calc--eval model (list 'NAME name))))
        (if (and (grid-calc-lambda-p callee)
                 (null (grid-calc-lambda-function callee)))
            (grid-calc--catching
              (let ((grid-calc--lazy-model model))
                (grid-calc-apply-lambda
                 callee (mapcar #'grid-calc--lambda-argument arguments))))
          (grid-calc-err "#NAME?"))))
     ((grid-calc-function-legacy function)
      (grid-calc--call-legacy model function arguments))
     (t
      (let* ((kinds (grid-calc-function-kinds function))
             (rest (grid-calc-function-rest function))
             (count (length arguments)))
        (if (or (< count (grid-calc-function-min function))
                (and (null rest) (> count (length kinds))))
            (grid-calc-err "#VALUE!")
          (let* ((all-kinds (cl-loop for i below count
                                     collect (if (< i (length kinds))
                                                 (nth i kinds)
                                               rest)))
                 (empty (grid-calc-function-empty function))
                 (classes (grid-calc--function-classes name))
                 (values (cl-loop for ast in arguments
                                  for kind in all-kinds
                                  for index from 0
                                  collect
                                  (if (and (equal ast '(MISSING)) empty
                                           (<= 0 index (1- (length empty)))
                                           (or (>= index
                                                   (grid-calc-function-min function))
                                               (nth index empty)))
                                      ;; Left empty, it is left out:
                                      ;; OFFSET(A1:B2,0,0,,1) is 2 high.
                                      ;; A first argument that is required
                                      ;; takes its default too, as
                                      ;; SEQUENCE(,12) does; without one it
                                      ;; is no argument, as before.
                                      (or (nth index empty) 'grid-calc--omitted)
                                    (grid-calc--argument
                                     model ast
                                     (if (and (eq kind 'values)
                                              (>= index (length kinds)))
                                         'areas
                                       kind)
                                     (eq (grid-calc--argument-class
                                          (cadr classes) index (cddr classes))
                                         'RR))))))
            ;; Each area of a union, as in SUM((A1,C3)), is an argument
            ;; of its own where the arguments may repeat.
            (when (cl-some (lambda (value) (eq (car-safe value) 'union)) values)
              (setq values (cl-loop for value in values
                                    for kind in all-kinds
                                    if (and (eq (car-safe value) 'union)
                                            (eq kind 'values))
                                    append (cdr value) into spread
                                    and append (make-list (length (cdr value)) kind)
                                    into spread-kinds
                                    else collect value into spread
                                    and collect kind into spread-kinds
                                    finally (setq all-kinds spread-kinds)
                                    finally return spread)))
            (grid-calc--apply function model all-kinds values))))))))

(defun grid-calc--lambda-argument (ast)
  "Return the lazy argument AST computed for a LAMBDA to take.
A reference stays one; an argument left out is `grid-calc--omitted'."
  (if (equal ast '(MISSING))
      'grid-calc--omitted
    (let ((grid-calc--array-context t))
      (grid-calc--eval grid-calc--lazy-model ast))))

(defun grid-calc-apply-lambda (function values)
  "Return what FUNCTION, a `grid-calc-lambda', gives for VALUES.
A value may be `grid-calc--omitted', as one past the last given is;
ISOMITTED is TRUE of it.  Too many values signal #VALUE!."
  (if-let* ((name (grid-calc-lambda-function function)))
      (grid-calc--call grid-calc--lazy-model name
                       (mapcar (lambda (value)
                                 (if (eq value 'grid-calc--omitted)
                                     '(MISSING)
                                   (list 'QUOTED value)))
                               values))
    (let ((parameters (grid-calc-lambda-parameters function))
          (grid-calc--locals (grid-calc-lambda-locals function)))
      (when (> (length values) (length parameters))
        (grid-calc-raise "#VALUE!"))
      (dolist (parameter parameters)
        (push (cons parameter (if values (pop values) 'grid-calc--omitted))
              grid-calc--locals))
      (grid-calc-arg (grid-calc-lambda-body function) 'values))))

(defun grid-calc--apply (function model kinds values)
  "Run FUNCTION on VALUES, its arguments as KINDS take them, on MODEL.
Several values for one value make the result several values too."
  (let* ((errors (grid-calc-function-errors function))
         (scalar (mapcar #'grid-calc--scalar-kind-p kinds))
         ;; An error where Excel wants a reference, as in
         ;; COUNTIF(#REF!,1), is the function's answer too, even of a
         ;; function that takes errors, as COUNTBLANK(#REF!) is #REF!.
         ;; ISREF(#REF!), COUNTA(#REF!) and the like, whose class
         ;; ends in X, take it as a value.
         (stops (pcase-let ((`(,_ ,classes . ,pairs)
                             (grid-calc--function-classes
                              (grid-calc-function-name function))))
                  (cl-loop for kind in kinds
                           for one in scalar
                           for index from 0
                           collect
                           (or (and one (not errors))
                               (and (memq kind '(values array areas))
                                    (let ((class (symbol-name
                                                  (grid-calc--argument-class
                                                   classes index pairs))))
                                      (and (string-prefix-p "R" class)
                                           (not (and errors
                                                     (string-suffix-p
                                                      "X" class)))))))))))
    (if (cl-loop for value in values
                 for one in scalar
                 thereis (and one (grid-calc-array-p value)))
        ;; Several values where the function takes one
        (let ((positions (cl-loop for value in values
                                  for one in scalar
                                  collect (and one (grid-calc-array-p value)))))
          (grid-calc--broadcast
           (lambda (&rest elements)
             ;; An array cannot hold arrays: where one call gives
             ;; several values, its first stands for them, as
             ;; SEQUENCE({2,3}) is {1,1}.
             (let ((value (grid-calc--apply-once
                           function model kinds scalar stops errors
                           (cl-loop for value in values
                                    for lifted in positions
                                    collect (if lifted (pop elements) value)))))
               (if (grid-calc-array-p value) (grid-calc-aref value 0 0) value)))
           (cl-loop for value in values
                    for lifted in positions
                    when lifted collect value)))
      (grid-calc--apply-once function model kinds scalar stops errors
                             values))))

(defun grid-calc--apply-once (function model kinds scalar stops errors values)
  "Run FUNCTION once on VALUES, for `grid-calc--apply'.
KINDS, SCALAR, STOPS and ERRORS are as that function works them out."
  (grid-calc--catching
    (let ((grid-calc--model model)
          (grid-calc--lazy-model model))
      (let ((arguments
             ;; The arguments are taken from the left, so the first one
             ;; that fails, as text where a number is wanted or an
             ;; error value, gives the function's error.
             (cl-loop for value in values
                      for kind in kinds
                      for one in scalar
                      for stop in stops
                      collect
                      (cond
                       ((eq value 'grid-calc--omitted) nil)
                       ((and stop (grid-calc-err-p value))
                        (grid-calc-raise (grid-calc-err-code value)))
                       ((and one (memq value '(t :false))
                             (grid-calc-function-toolpak function))
                        (grid-calc-raise "#VALUE!"))
                       ((and one (not (grid-calc-err-p value)))
                        (let ((argument (grid-calc--coerce value kind)))
                          (if (and (eq kind 'date)
                                   (grid-calc-function-toolpak function))
                              (grid-calc--day-checked argument t)
                            argument)))
                       ((and (eq kind 'ref) (grid-calc-err-p value)
                             (not errors))
                        (grid-calc-raise (grid-calc-err-code value)))
                       (t value)))))
        ;; Only then is a date seen to be one, so WEEKDAY(-1,"x") is
        ;; #VALUE! and WEEKDAY(-1,1) #NUM!; WEEKNUM(-1,"x") is #NUM!.
        (cl-loop for kind in kinds
                 for argument in arguments
                 when (and (eq kind 'date) argument)
                 do (grid-calc--day-checked argument))
        (apply (grid-calc-function-body function) arguments)))))

;;;----------------------------------------------------------------------
;;; Functions defined the earlier way
;;;----------------------------------------------------------------------

(defmacro grid-calc-defun (name args &rest body)
  "Define NAME as a function formulas can call, the earlier way.
It is called with the model, then each argument: a value as a cell
stores it, or a list of such values for a range.  An error value among
them makes the function give that error."
  `(puthash ,(symbol-name name)
            (grid-calc-function--make
             :name ,(symbol-name name) :legacy t
             :body (lambda (model . ,args) ; All functions receive model as first arg
                     ,@body))
            grid-calc-functions))

(defun grid-calc--legacy-value (value)
  "Return VALUE as functions defined with `grid-calc-defun' take it."
  (cond ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
        ((eq value :blank) "")
        ((grid-calc-array-p value)
         (mapcar #'grid-calc--legacy-value (grid-calc-array-values value)))
        (t (grid-calc--store value))))

(defun grid-calc--legacy-cells (ref)
  "Return the cells of REF as functions defined with `grid-calc-defun'
take them: each as its cell stores it, text as typed for a constant and
\"\" for an empty cell, row after row."
  (let* ((model (grid-calc-ref-model ref))
         (cols (grid-data-model-cols model))
         (bottom (min (grid-calc-ref-bottom ref) (1- (grid-data-model-rows model))))
         (right (min (grid-calc-ref-right ref) (1- cols)))
         values)
    (cl-loop for row from (grid-calc-ref-top ref) to bottom do
             (cl-loop for col from (grid-calc-ref-left ref) to right do
                      (let ((index (+ (* row cols) col)))
                        (when grid-calc--pass
                          (grid-calc--compute-cell model index))
                        (let ((stored (grid-cell-computed-value
                                       (aref (grid-data-model-cells model) index))))
                          (when (grid-calc-error-text-p stored)
                            (grid-calc-raise stored))
                          (push (or stored "") values)))))
    (nreverse values)))

(defun grid-calc--call-legacy (model function arguments)
  "Return what FUNCTION, defined with `grid-calc-defun', gives."
  (grid-calc--catching
    (let* ((values (mapcar (lambda (ast)
                             (let ((value (grid-calc--argument model ast 'values)))
                               (if (grid-calc-ref-p value)
                                   ;; A range is its cells as they are
                                   ;; stored, as these functions always
                                   ;; had it; one cell written alone is
                                   ;; one value.
                                   (let ((cells (grid-calc--legacy-cells value)))
                                     (if (eq (car-safe ast) 'CELL-REF)
                                         (car cells)
                                       cells))
                                 (grid-calc--legacy-value
                                  (grid-calc--deref value)))))
                           arguments))
           (result (apply (grid-calc-function-body function) model values)))
      (cond ((null result) :false)
            ((and (consp result) (not (keywordp (car result))))
             (grid-calc-array-from-list
              (mapcar #'grid-calc--from-result result)))
            ((consp result) result)
            ((stringp result) (grid-calc--from-result result))
            (t result)))))

(defun grid-table-calc-set-cell-formula (model cell-coords formula)
  "Set the cell at CELL-COORDS, (ROW . COL), of MODEL to FORMULA.
Then compute what that changes."
  (let* ((row (car cell-coords))
         (col (cdr cell-coords))
         (cell (grid-model-get-cell model row col)))
    (when cell
      (set-grid-cell-raw-input cell formula)
      (grid-calc-touch model row col)
      (grid-table-calc-evaluate model))))

(defun grid-calc-call-function (model func-name args)
  "Return what the function FUNC-NAME gives for ARGS on MODEL.
ARGS are values as cells store them, a list standing for a range's
values; the result is in the same form."
  (let ((asts (mapcar (lambda (value)
                        (list 'QUOTED
                              (if (and (consp value) (not (keywordp (car value))))
                                  (grid-calc-array-from-list
                                   (mapcar #'grid-calc--from-result value))
                                (grid-calc--from-result value))))
                      args)))
    (let ((value (grid-calc--deref (grid-calc--call model func-name asts))))
      (if (grid-calc-array-p value)
          (mapcar #'grid-calc--store (grid-calc-array-values value))
        (grid-calc--store value)))))

;;;----------------------------------------------------------------------
;;; Going through the values of arguments
;;;----------------------------------------------------------------------

(defun grid-calc-each-value (argument function)
  "Call FUNCTION on each value of ARGUMENT, as a `values' argument has it.
FUNCTION gets the value and whether it was written directly as an
argument, rather than being one of a range's or an array's values.
Cells past the sheet's filled cells are not visited."
  (cond
   ((grid-calc-ref-p argument)
    (let* ((model (grid-calc-ref-model argument))
           (top (grid-calc-ref-top argument))
           (left (grid-calc-ref-left argument))
           (rows (grid-calc-ref-filled-rows argument))
           (cols (max 0 (min (grid-calc-ref-cols argument)
                             (- (grid-data-model-cols model) left)))))
      (dotimes (row rows)
        (dotimes (col cols)
          (funcall function (grid-calc--cell model (+ top row) (+ left col))
                   nil)))))
   ((grid-calc-array-p argument)
    (mapc (lambda (value) (funcall function value nil))
          (grid-calc-array-data argument)))
   ((eq (car-safe argument) 'union)
    (dolist (area (cdr argument))
      (grid-calc-each-value area function)))
   ((null argument))
   (t (funcall function argument t))))

(defun grid-calc-numbers (arguments &optional how)
  "Return the numbers of ARGUMENTS, `values' arguments, in order.
In a range or array only numbers count, unless HOW is `a', which counts
text as 0 and TRUE as 1 there too, as AVERAGEA does.  A number written
as an argument counts, as does TRUE (1), FALSE (0) and text that reads
as a number; other text is #VALUE!.  An error value signals."
  (let (numbers)
    (dolist (argument arguments)
      (grid-calc-each-value
       argument
       (lambda (value direct)
         (cond
          ((numberp value) (push value numbers))
          ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
          (direct (push (grid-calc-num value) numbers))
          ((eq how 'a)
           (cond ((eq value t) (push 1 numbers))
                 ((memq value '(:false)) (push 0 numbers))
                 ((stringp value) (push 0 numbers))))))))
    (nreverse numbers)))

(defun grid-calc-flat-values (argument)
  "Return the values of ARGUMENT, a `values' argument, as a list."
  (let (values)
    (grid-calc-each-value argument (lambda (value _) (push value values)))
    (nreverse values)))

(defun grid-calc-arg-array (argument)
  "Return ARGUMENT, a `values' argument, as an array."
  (let ((value (grid-calc--deref argument)))
    (if (grid-calc-array-p value) value
      (grid-calc-array--make 1 1 (vector value)))))

;;;----------------------------------------------------------------------
;;; Reference text
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
ROW the number written.  The tail of a name, the name of a function and
the name of a table, such as Table1 in Table1[Qty], are not references,
and nor is a column past XFD."
  (let ((len (length formula)))
    (when (and (eq (string-match grid-calc--reference-regexp formula pos) pos)
               (<= (length (match-string 2 formula)) 3)
               (<= (grid-calc-col-letter-to-number (match-string 2 formula))
                   16383)
               (not (and (> pos 0)
                         (string-match-p "[A-Za-z0-9_.$]"
                                         (substring formula (1- pos) pos))))
               (not (and (< (match-end 0) len)
                         (string-match-p "[A-Za-z0-9_.(![]"
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
A name that is not plain letters, digits and underscores is quoted; so
is a span of sheets, Sheet1:Sheet3, unless both its ends are plain.  A
name that reads as a cell, as A1 or R1C1 does, is quoted too."
  (if (and (string-match-p (concat "\\`[[:alpha:]_][[:alnum:]_.]*"
                                   "\\(?::[[:alpha:]_][[:alnum:]_.]*\\)?\\'")
                           name)
           (not (cl-some (lambda (part)
                           (let ((case-fold-search t))
                             (or (grid-parser--cell-text-p (upcase part))
                                 (string-match-p
                                  "\\`\\(?:R[0-9]*\\)?\\(?:C[0-9]*\\)?\\'" part))))
                         (split-string name ":"))))
      (concat name "!")
    (concat "'" (string-replace "'" "''" name) "'!")))

(defun grid-calc--same-sheet-p (left right)
  "Return non-nil when LEFT and RIGHT name the same sheet."
  (and left right
       (eq t (compare-strings left nil nil right nil nil t))))

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
           ;; The brackets of a table reference, Table1[[#This Row],[Q1]]:
           ;; what is inside names columns, not cells
           ((and (eq (aref formula pos) ?\[) (not prefix))
            (let ((end pos) (depth 0))
              (while (progn
                       (pcase (aref formula end)
                         (?\[ (cl-incf depth))
                         (?\] (cl-decf depth))
                         (?' (cl-incf end)))
                       (cl-incf end)
                       (and (> depth 0) (< end len))))
              (setq end (min end len))
              (push (substring formula pos end) parts)
              (setq pos end)))
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
     ;; Either end of a reference through several sheets, Sheet1:Sheet3
     (let* ((ends (split-string sheet ":"))
            (renamed (mapcar (lambda (end)
                               (if (grid-calc--same-sheet-p end old) new end))
                             ends)))
       (and (not (equal renamed ends)) (string-join renamed ":"))))))

(defun grid-calc-remove-sheet-in-formula (formula old names)
  "Return FORMULA for the sheet OLD taken out of the sheets NAMES.
NAMES are all of them, OLD included, in order.  A reference through
several sheets that ends at OLD then ends at the sheet next to it on
the inside, as Sheet1:Sheet3 becomes Sheet1:Sheet2; others into OLD are
left to be #REF!."
  (grid-calc--map-references
   formula
   (lambda (start end _sheet)
     (grid-calc--range-text start end #'grid-calc--reference-text))
   (lambda (sheet)
     (let* ((ends (split-string sheet ":"))
            (from (cl-position (car ends) names
                               :test #'grid-calc--same-sheet-p))
            (to (cl-position (cadr ends) names
                             :test #'grid-calc--same-sheet-p)))
       (when (and (cdr ends) from to (/= from to)
                  (cl-some (lambda (end) (grid-calc--same-sheet-p end old))
                           ends))
         (let* ((step (if (< from to) 1 -1))
                (first (if (grid-calc--same-sheet-p (car ends) old)
                           (nth (+ from step) names)
                         (car ends)))
                (last (if (grid-calc--same-sheet-p (cadr ends) old)
                          (nth (- to step) names)
                        (cadr ends))))
           (if (grid-calc--same-sheet-p first last)
               first
             (concat first ":" last))))))))

(defun grid-calc--lines-before (lines index)
  "Return how many of the line indexes LINES are below INDEX."
  (cl-count-if (lambda (line) (< line index)) lines))

(defun grid-calc-restructure-formula (formula axis at delta
                                              &optional home target)
  "Return FORMULA rewritten for a row or column inserted or deleted.
AXIS is `row' or `col' and AT the zero-based index of the line inserted
\(DELTA 1) or deleted (DELTA -1).  For a deletion AT may also be a list
of indexes, the lines deleted together.  A reference keeps pointing at
the cell it pointed at, anchored or not: one at or past an inserted line
moves on, one past a deleted line moves back.  A reference to a deleted
cell becomes #REF!; a range that loses a line shrinks, and becomes #REF!
only when nothing of it is left.

HOME names the sheet FORMULA is in and TARGET the sheet that changed;
TARGET defaults to HOME.  Only references into the changed sheet are
rewritten: those written without a sheet when the formula lives there,
and those that name it."
  (let* ((target (or target home))
         (lines (if (listp at) at (list at)))
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
           (if (memql from lines)
               "#REF!"
             (funcall moved start
                      (- from (grid-calc--lines-before lines from)))))
          (t
           ;; Keep the ends in the order they were written.
           (let* ((low (min from to)) (high (max from to))
                  (new-low (- low (grid-calc--lines-before lines low)))
                  (new-high (- high (grid-calc--lines-before
                                     lines (1+ high)))))
             (if (< new-high new-low)
                 "#REF!"
               (concat (funcall moved start (if (<= from to) new-low new-high))
                       ":"
                       (funcall moved end (if (<= from to) new-high new-low))))))))))))

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
the edges of the sheet's cells; an end before its start means nothing
of the range is on the sheet.  A range written out keeps its size where
it reaches past them, as ROWS(A2:A12) is 11 on a sheet of 5 rows: the
cells there are blank."
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
            (max 0 (min row-a row-b)) (max row-a row-b)
            (max 0 (min col-a col-b)) (max col-a col-b)))))

(provide 'grid-table-calc)

(require 'grid-table-calc-logic)
(require 'grid-table-calc-lookup)
(require 'grid-table-calc-math)
(require 'grid-table-calc-stats)
(require 'grid-table-calc-dist)
(require 'grid-table-calc-text)
(require 'grid-table-calc-date)
(require 'grid-table-calc-finance)
(require 'grid-table-calc-engineering)

(grid-calc-declare-known-functions)

;;; grid-table-calc.el ends here
