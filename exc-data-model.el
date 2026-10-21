;;; exc-data-model.el --- Data model and calculation engine for exc -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'exc-parser) ; For parsing formulas
(require 'exc-calc)   ; For evaluating formulas


;;;----------------------------------------------------------------------
;;; Manual Grid Cell "Struct" Implementation (using vectors)
;;;----------------------------------------------------------------------

;; Constants for slot indices
(defconst exc-cell--raw-input 0)
(defconst exc-cell--computed-value 1)
;; (RAW . PARSED): the formula the cell holds, parsed
(defconst exc-cell--parsed 3)
(defconst exc-cell--error-msg 4)
(defconst exc-cell--row 5)
(defconst exc-cell--col 6)
;; What the formula was recorded as reading; see `exc-calc--register'
(defconst exc-cell--reads 7)
(defconst exc-cell--length 8)

;; Manual constructor
(defun make-exc-cell (row col)
  "Create a new grid cell vector."
  (let ((cell (make-vector exc-cell--length nil)))
    (aset cell exc-cell--row row)
    (aset cell exc-cell--col col)
    (aset cell exc-cell--raw-input "")
    cell))

;; Manual accessors and setters
(defun exc-cell-raw-input (cell) (aref cell exc-cell--raw-input))
(defun exc-cell-computed-value (cell) (aref cell exc-cell--computed-value))
(defun exc-cell-row (cell) (aref cell exc-cell--row))
(defun exc-cell-col (cell) (aref cell exc-cell--col))
(defun exc-cell-error-msg (cell) (aref cell exc-cell--error-msg))

(defun set-exc-cell-raw-input (cell value) (aset cell exc-cell--raw-input value))
(defun set-exc-cell-computed-value (cell value) (aset cell exc-cell--computed-value value))

;;;----------------------------------------------------------------------
;;; Data Model
;;;----------------------------------------------------------------------

(defvar exc-model-first-row-is-header t
  "Non-nil when the first row of the model at work holds column titles.
A merge may then not reach from that row into the rows below it.  A data
source whose sheet has no header row binds this to nil.")

;; Manual grid data model implementation
(defun make-exc-data-model (rows cols cells &optional merges styles
                                 conditionals)
  "Create a new grid data model.
STYLES is a hash table from (ROW . COL) to a style plist and
CONDITIONALS a list of conditional format rules."
  (vector rows cols cells merges
          (or styles (make-hash-table :test 'equal))
          conditionals
          ;; Column widths set by hand: column index to characters
          (make-hash-table)
          ;; Leading rows and columns that stay in place: (ROWS . COLS)
          (cons 0 0)
          ;; Hidden rows and columns: (ROWS . COLS), sorted lists
          (cons nil nil)
          ;; Validation rules: (TOP LEFT BOTTOM RIGHT KIND SPEC)
          nil
          ;; Floating charts, plists; see exc-chart-svg.el
          nil
          ;; Array formulas: ((ROW . COL) . RANGE), RANGE the cells each
          ;; fills, as text such as B2:C3
          nil
          ;; The formulas read as a workbook file holds them, a hash
          ;; table of (ROW . COL); see `exc-calc-legacy-arrays'
          nil
          ;; Filtered ranges: (TOP LEFT BOTTOM RIGHT) each, TOP the
          ;; header; a hidden row under it is one the filter left out
          nil))

(defun exc-data-model-rows (model) (aref model 0))
(defun exc-data-model-cols (model) (aref model 1))
(defun exc-data-model-cells (model) (aref model 2))
(defun exc-data-model-merges (model)
  (when (> (length model) 3)
    (aref model 3)))
(defun set-exc-data-model-merges (model value)
  (aset model 3 value))
(defun exc-data-model-styles (model)
  (when (> (length model) 4)
    (aref model 4)))
(defun exc-data-model-conditionals (model)
  (when (> (length model) 5)
    (aref model 5)))
(defun exc-data-model-widths (model)
  "Return MODEL's table of column widths set by hand, column to characters."
  (when (> (length model) 6)
    (aref model 6)))
(defun exc-data-model-frozen (model)
  "Return (ROWS . COLS): the leading rows and columns of MODEL kept in place."
  (or (and (> (length model) 7) (aref model 7))
      (cons 0 0)))
(defun exc-data-model-hidden (model)
  "Return (ROWS . COLS): the rows and columns of MODEL hidden, sorted lists."
  (or (and (> (length model) 8) (aref model 8))
      (cons nil nil)))

;;;----------------------------------------------------------------------
;;; Model Creation and Access
;;;----------------------------------------------------------------------

(defun exc-model-create (rows cols &optional initial-data merges styles
                              conditionals)
  "Create a new data model of ROWS and COLS.
Optionally populate with INITIAL-DATA (a list of lists of strings).
STYLES and CONDITIONALS, when given, become the model's cell styles and
conditional format rules; the model owns them from then on."
  (let* ((model (make-exc-data-model rows cols (make-vector (* rows cols) nil)
                                      nil styles conditionals))
         (cells (exc-data-model-cells model)))
    ;; 1. Initialize all cells and populate with initial data
    ;; Walk INITIAL-DATA alongside the indexes: looking a row up by number
    ;; for every cell is quadratic in the row count.
    (let ((rest-rows initial-data))
      (dotimes (r rows)
        (let ((rest-values (pop rest-rows)))
          (dotimes (c cols)
            (let ((cell (make-exc-cell r c))
                  (raw-val (if rest-values (pop rest-values) "")))
              (set-exc-cell-raw-input cell raw-val)
              ;; For non-formula cells, set computed value to raw value
              (unless (string-prefix-p "=" raw-val)
                (set-exc-cell-computed-value cell raw-val))
              (aset cells (+ (* r cols) c) cell))))))
    ;; 2. Perform initial calculation
    (exc-model-recalculate-all model)
    (when merges
      (set-exc-data-model-merges
       model (exc-model-validate-merges model merges)))
    model))

(defun exc-model-get-cell (model row col)
  "Get the cell struct at ROW and COL from the MODEL."
  (when (and (>= row 0) (< row (exc-data-model-rows model))
             (>= col 0) (< col (exc-data-model-cols model)))
    (aref (exc-data-model-cells model) (+ (* row (exc-data-model-cols model)) col))))

(defun exc-model-get-computed-value (model row col)
  "Get the computed value of the cell at ROW and COL.
If the cell is a formula, it will be evaluated on demand."
  (when-let* ((cell (exc-model-get-cell model row col)))
    (exc-cell-computed-value cell)))

;;;----------------------------------------------------------------------
;;; Cell Styles and Conditional Formats
;;;----------------------------------------------------------------------

;; A style is a plist with any of :bold, :italic (non-nil), :color,
;; :background (color strings) and :align (left, center or right).
;; A conditional format rule is (TOP LEFT BOTTOM RIGHT OP VALUE STYLE): cells
;; of the range whose value satisfies OP against VALUE take STYLE.

(defun exc-model-get-style (model row col)
  "Return a copy of the style plist of the cell at ROW and COL in MODEL."
  (let ((styles (exc-data-model-styles model)))
    (and styles (copy-sequence (gethash (cons row col) styles)))))

(defun exc-model-set-style (model row col style)
  "Give the cell at ROW and COL in MODEL the plist STYLE; nil clears it."
  (let ((styles (exc-data-model-styles model)))
    (when styles
      (if style
          (puthash (cons row col) (copy-sequence style) styles)
        (remhash (cons row col) styles))
      ;; The text format changes what a typed cell is worth.
      (exc-calc-touch model row col))
    style))

(defun exc-model-get-conditionals (model)
  "Return a copy of MODEL's conditional format rules."
  (copy-tree (exc-data-model-conditionals model)))

(defun exc-model-set-conditionals (model rules)
  "Replace MODEL's conditional format rules with RULES."
  (when (> (length model) 5)
    (aset model 5 (copy-tree rules)))
  rules)

(defun exc-model--map-styles (model function)
  "Return MODEL's styles re-keyed for a structural change.
FUNCTION receives a cell's ROW and COL and returns its new (ROW . COL),
or nil when the cell is gone."
  (let ((new (make-hash-table :test 'equal))
        (styles (exc-data-model-styles model)))
    (when styles
      (maphash (lambda (key style)
                 (let ((moved (funcall function (car key) (cdr key))))
                   (when moved
                     (puthash moved style new))))
               styles))
    new))

(defun exc-model--shift-index (index at delta)
  "Return INDEX after a line is inserted at AT (DELTA 1) or deleted (-1).
A deleted line itself gives nil.  For a deletion AT may be a list of the
lines deleted together."
  (cond ((> delta 0) (if (>= index at) (1+ index) index))
        ((listp at)
         (and (not (memql index at))
              (- index (exc-calc--lines-before at index))))
        ((= index at) nil)
        ((> index at) (1- index))
        (t index)))

(defun exc-model--shift-rules (rules axis at delta)
  "Return conditional RULES after a line change along AXIS.
AXIS is `row' or `col', AT the index of the line and DELTA 1 for an
insertion or -1 for a deletion; for a deletion AT may be a list of the
lines deleted together.  A rule whose range is deleted entirely is
dropped."
  (if (listp at)
      ;; From the last line up, so the ones still to go keep their place.
      (dolist (line (sort (copy-sequence at) #'>) rules)
        (setq rules (exc-model--shift-rules rules axis line delta)))
  (let ((first (if (eq axis 'row) 0 1))
        (last (if (eq axis 'row) 2 3)))
    (delq nil
          (mapcar
           (lambda (rule)
             (let ((rule (copy-sequence rule))
                   (start (nth first rule))
                   (end (nth last rule)))
               (cond
                ((> delta 0)
                 (cond ((<= at start)
                        (setf (nth first rule) (1+ start)
                              (nth last rule) (1+ end)))
                       ((<= at end) (setf (nth last rule) (1+ end))))
                 rule)
                ((< at start)
                 (setf (nth first rule) (1- start)
                       (nth last rule) (1- end))
                 rule)
                ((> at end) rule)
                ((= start end) nil)
                (t (setf (nth last rule) (1- end)) rule))))
           rules)))))

(defun exc-model-set-column-width (model col width)
  "Give column COL of MODEL a width of WIDTH characters; nil forgets it."
  (when-let* ((widths (exc-data-model-widths model)))
    (if width
        (puthash col width widths)
      (remhash col widths))
    width))

(defun exc-model-set-frozen (model rows cols)
  "Keep the first ROWS rows and COLS columns of MODEL in place.
A row and a column are always left to scroll."
  (when (> (length model) 7)
    (aset model 7
          (cons (max 0 (min rows (1- (exc-data-model-rows model))))
                (max 0 (min cols (1- (exc-data-model-cols model))))))))

(defun exc-data-model-validations (model)
  "Return MODEL's validation rules, (TOP LEFT BOTTOM RIGHT KIND SPEC) each.
KIND `list' allows the values SPEC lists: a list of strings, or a
formula such as =$F$2:$F$9 or =Sizes whose values are the choices."
  (and (> (length model) 9) (aref model 9)))

(defun exc-model-set-validations (model rules)
  "Replace MODEL's validation rules with RULES."
  (when (> (length model) 9)
    (aset model 9 (copy-tree rules)))
  rules)

(defun exc-model-validation-at (model row col)
  "Return the validation rule of the cell at ROW and COL of MODEL, or nil.
The last rule added wins where rules overlap."
  (car (last (cl-remove-if-not
              (lambda (rule)
                (and (<= (nth 0 rule) row (nth 2 rule))
                     (<= (nth 1 rule) col (nth 3 rule))))
              (exc-data-model-validations model)))))

(defun exc-model-carry-validations (old new &optional axis at delta sheet)
  "Give model NEW the validation rules of model OLD and return NEW.
AXIS, AT and DELTA are as for `exc-model-carry-frozen': the ranges move
with the lines, and a range that reads a list from the sheet, named
SHEET, follows them too.  Sorting leaves the ranges in place."
  (unless (eq old new)
    (let ((rules (exc-data-model-validations old)))
      (when axis
        (setq rules
              (mapcar (lambda (rule)
                        (if (stringp (nth 5 rule))
                            (append (cl-subseq rule 0 5)
                                    (list (exc-calc-restructure-formula
                                           (nth 5 rule) axis at delta
                                           sheet sheet)))
                          rule))
                      (exc-model--shift-rules rules axis at delta))))
      (exc-model-set-validations new rules)))
  new)

;;;----------------------------------------------------------------------
;;; Filtered rows
;;;----------------------------------------------------------------------

(defun exc-data-model-filters (model)
  "Return MODEL's filtered ranges, (TOP LEFT BOTTOM RIGHT) each.
TOP is the range's header row; a hidden row below it is one a filter
left out, which SUBTOTAL leaves out too."
  (and (> (length model) 13) (aref model 13)))

(defun exc-model-set-filters (model ranges)
  "Replace MODEL's filtered ranges with RANGES."
  (when (> (length model) 13)
    (aset model 13 (copy-tree ranges)))
  ranges)

(defun exc-model-filtered-rows (model)
  "Return the rows of MODEL a filter left out: hidden under a header."
  (let ((ranges (exc-data-model-filters model)))
    (and ranges
         (cl-remove-if-not
          (lambda (row)
            (cl-some (lambda (range) (< (nth 0 range) row (1+ (nth 2 range))))
                     ranges))
          (car (exc-data-model-hidden model))))))

(defun exc-model-carry-filters (old new &optional axis at delta)
  "Give model NEW the filtered ranges of model OLD and return NEW.
AXIS, AT and DELTA are as for `exc-model-carry-frozen': the ranges move
with the lines.  Sorting leaves them in place."
  (unless (eq old new)
    (let ((ranges (exc-data-model-filters old)))
      (exc-model-set-filters
       new (if axis (exc-model--shift-rules ranges axis at delta) ranges))))
  new)

;;;----------------------------------------------------------------------
;;; Floating charts
;;;----------------------------------------------------------------------

(defun exc-data-model-charts (model)
  "Return MODEL's floating charts, a list of chart plists.
Each holds :anchor (ROW . COL), :size (COLUMNS . LINES), :type, :range
\(text such as A1:D7), :by and :title, and may hold an :offset (X . 0),
X characters right of the anchor cell's left edge."
  (and (> (length model) 10) (aref model 10)))

(defun exc-model-set-charts (model charts)
  "Replace MODEL's floating charts with CHARTS and return them."
  (when (> (length model) 10)
    (aset model 10 (copy-tree charts)))
  charts)

;;;----------------------------------------------------------------------
;;; Array formulas
;;;----------------------------------------------------------------------

(defun exc-data-model-arrays (model)
  "Return MODEL's array formulas, ((ROW . COL) . RANGE) each.
RANGE, text such as B2:C3, is the cells the formula at ROW and COL
fills.  An array formula takes ranges whole where a plain one takes the
cell of a range in its own row or column."
  (and (> (length model) 11) (aref model 11)))

(defun exc-model-set-arrays (model arrays)
  "Make ARRAYS, as `exc-data-model-arrays' gives them, MODEL's own.
The formulas fill other cells now, so every cell is computed again at
the next recalculation."
  (when (> (length model) 11)
    (aset model 11 (copy-tree arrays))
    (exc-calc-forget model))
  arrays)

(defvar exc-model--array-index (make-hash-table :test 'eq :weakness 'key)
  "Maps a model's list of array formulas to a table of the same entries.
The list is never changed in place, only replaced, so a table made for
it stays right.")

(defun exc-model-array-at (model row col)
  "Return the range the array formula at ROW and COL of MODEL fills, or nil."
  (let ((arrays (exc-data-model-arrays model)))
    (if (length< arrays 16)
        (cdr (assoc (cons row col) arrays))
      (gethash (cons row col)
               (or (gethash arrays exc-model--array-index)
                   (let ((table (make-hash-table :test 'equal
                                                 :size (length arrays))))
                     (dolist (array (reverse arrays))
                       (puthash (car array) (cdr array) table))
                     (puthash arrays table exc-model--array-index)))))))

(defun exc-model-forget-array (model row col)
  "Make the formula at ROW and COL of MODEL a plain one, if it was not."
  (when (exc-model-array-at model row col)
    (aset model 11 (cl-remove (cons row col) (aref model 11)
                              :key #'car :test #'equal))))

(defun exc-model-legacy-at (model row col)
  "Return non-nil when the formula at ROW and COL of MODEL reads as a
workbook file holds it.  See `exc-calc-legacy-arrays'."
  (and (> (length model) 12) (aref model 12)
       (gethash (cons row col) (aref model 12))))

(defun exc-model-set-legacy (model cells)
  "Make the formulas at CELLS of MODEL read as a workbook file holds them.
CELLS is a list of (ROW . COL)."
  (when (> (length model) 12)
    (let ((table (make-hash-table :test 'equal)))
      (dolist (cell cells) (puthash cell t table))
      (aset model 12 (and cells table)))))

(defun exc-model-forget-legacy (model row col)
  "Make the formula at ROW and COL of MODEL read as one typed in the grid."
  (when (and (> (length model) 12) (aref model 12))
    (remhash (cons row col) (aref model 12))))

(defun exc-model--legacy-cells (model)
  "Return the cells of MODEL whose formulas read as a file holds them."
  (let (cells)
    (when (and (> (length model) 12) (aref model 12))
      (maphash (lambda (cell _) (push cell cells)) (aref model 12)))
    cells))

(defun exc-model-carry-arrays (old new &optional axis at delta)
  "Give model NEW the array formulas of OLD and how it reads them.
AXIS, AT and DELTA are as for `exc-model-carry-frozen'.  An array
formula stays one only while its cell still holds a formula; one whose
line is deleted goes."
  (unless (eq old new)
    (exc-model-set-legacy
     new
     (delq nil
           (mapcar
            (pcase-lambda (`(,row . ,col))
              (let ((row (if (eq axis 'row) (exc-model--shift-index row at delta) row))
                    (col (if (eq axis 'col) (exc-model--shift-index col at delta) col)))
                (and row col
                     (< row (exc-data-model-rows new))
                     (< col (exc-data-model-cols new))
                     (cons row col))))
            (exc-model--legacy-cells old))))
    (exc-model-set-arrays
     new
     (delq nil
           (mapcar
            (pcase-lambda (`((,row . ,col) . ,range))
              (let ((row (if (eq axis 'row) (exc-model--shift-index row at delta) row))
                    (col (if (eq axis 'col) (exc-model--shift-index col at delta) col)))
                (when (and row col
                           (< row (exc-data-model-rows new))
                           (< col (exc-data-model-cols new))
                           (let ((raw (exc-cell-raw-input
                                       (exc-model-get-cell new row col))))
                             (and (stringp raw) (string-prefix-p "=" raw))))
                  (cons (cons row col)
                        (if axis
                            (substring (exc-calc-restructure-formula
                                        (concat "=" range) axis at delta)
                                       1)
                          range)))))
            (exc-data-model-arrays old)))))
  new)

(defun exc-model--shift-anchor (index at delta)
  "Return line INDEX after the line AT was inserted (DELTA 1) or deleted.
For a deletion AT may be a list of the lines deleted together.  A chart
whose own line is deleted stays where the line was."
  (if (> delta 0)
      (if (<= at index) (1+ index) index)
    (- index (cl-count-if (lambda (line) (< line index))
                          (if (listp at) at (list at))))))

(defun exc-model-carry-charts (old new &optional axis at delta sheet)
  "Give model NEW the floating charts of model OLD and return NEW.
AXIS, AT and DELTA are as for `exc-model-carry-frozen': a chart's
anchor moves with its line and its range with the cells it reads in the
sheet named SHEET.  A sort leaves the charts in place."
  (unless (eq old new)
    (exc-model-set-charts
     new
     (mapcar
      (lambda (chart)
        (let* ((chart (copy-sequence chart))
               (anchor (plist-get chart :anchor))
               (rows (exc-data-model-rows new)))
          (when axis
            (setq anchor
                  (if (eq axis 'row)
                      (cons (exc-model--shift-anchor (car anchor) at delta)
                            (cdr anchor))
                    (cons (car anchor)
                          (exc-model--shift-anchor (cdr anchor) at delta))))
            (let ((range (exc-calc-restructure-formula
                          (concat "=" (plist-get chart :range))
                          axis at delta sheet sheet)))
              (setq chart (plist-put chart :range (substring range 1)))))
          ;; A chart may stand right of the last column, on the empty
          ;; space there, so only its row is kept inside the sheet.
          (plist-put chart :anchor
                     (cons (max 0 (min (car anchor) (1- rows)))
                           (max 0 (cdr anchor))))))
      (exc-data-model-charts old))))
  new)

(defun exc-model-set-hidden (model rows cols)
  "Hide the ROWS and COLS of MODEL, lists of indexes; nil shows them all.
A line past the last cell stays hidden as well, as a workbook keeps it."
  (when (> (length model) 8)
    (cl-flet ((clean (indexes)
                (sort (delete-dups (cl-remove-if-not #'natnump
                                                     (copy-sequence indexes)))
                      #'<)))
      (aset model 8 (cons (clean rows) (clean cols))))))

(defun exc-model-subtotals-changed (model)
  "Compute again what SUBTOTAL and AGGREGATE give in MODEL, and what reads
them, for its hidden or filtered rows changed."
  (let ((cols (exc-data-model-cols model))
        touched)
    (dotimes (index (length (exc-data-model-cells model)))
      (let ((row (/ index cols)) (col (% index cols)))
        (when (exc-calc--subtotal-cell-p model row col)
          (exc-calc-touch model row col)
          (setq touched t))))
    (when touched
      (exc-calc-evaluate model))))

(defun exc-model--lines-after (lines at delta)
  "Return the line indexes LINES after a line was inserted or deleted.
A line inserted at AT (DELTA 1) pushes the ones from AT on down; the
line deleted at AT (DELTA -1) leaves LINES, and the ones after it move
up."
  (if (null at)
      lines
    (delq nil (mapcar (lambda (line)
                        (if (listp at)
                            (exc-model--shift-index line at delta)
                          (cond ((< line at) line)
                                ((and (= line at) (< delta 0)) nil)
                                (t (+ line delta)))))
                      lines))))

(defun exc-model-carry-hidden (old new &optional axis at delta)
  "Give model NEW the hidden rows and columns of model OLD and return NEW.
AXIS, AT and DELTA are as for `exc-model-carry-frozen'."
  (unless (eq old new)
    (let ((hidden (exc-data-model-hidden old)))
      (exc-model-set-hidden
       new
       (exc-model--lines-after (car hidden) (and (eq axis 'row) at) delta)
       (exc-model--lines-after (cdr hidden) (and (eq axis 'col) at) delta))))
  new)

(defun exc-model--frozen-after (count at delta)
  "Return frozen COUNT after a line was inserted or deleted at AT by DELTA.
For a deletion AT may be a list of the lines deleted together."
  (cond ((null at) count)
        ((listp at) (- count (exc-calc--lines-before at count)))
        ((< at count) (+ count delta))
        (t count)))

(defun exc-model-carry-frozen (old new &optional axis at delta)
  "Give model NEW the frozen rows and columns of model OLD and return NEW.
AXIS is `row' or `col' when a line was inserted at AT (DELTA 1) or
deleted there (DELTA -1) on the way; a line inside the frozen ones
changes how many there are."
  (unless (eq old new)
    (let ((frozen (exc-data-model-frozen old)))
      (exc-model-set-frozen
       new
       (exc-model--frozen-after (car frozen) (and (eq axis 'row) at) delta)
       (exc-model--frozen-after (cdr frozen) (and (eq axis 'col) at) delta))))
  new)

(defun exc-model-carry-widths (old new &optional at delta)
  "Give model NEW the column widths of model OLD and return NEW.
AT and DELTA describe a column inserted at AT (DELTA 1) or deleted there
\(DELTA -1) on the way from OLD to NEW; the widths move with their
columns.  A column past the last cell keeps its width as well."
  (let ((from (exc-data-model-widths old))
        (to (exc-data-model-widths new)))
    (when (and from to (not (eq old new)))
      (clrhash to)
      (maphash (lambda (col width)
                 (let ((moved (if at
                                  (exc-model--shift-index col at delta)
                                col)))
                   (when moved
                     (puthash moved width to))))
               from))
    new))

(defun exc-model-grow (model rows cols)
  "Return MODEL with ROWS more rows at the bottom and COLS more columns.
The new cells are empty; nothing that was there moves."
  (let* ((old-cols (exc-data-model-cols model))
         (values (mapcar (lambda (row) (append row (make-list cols "")))
                         (exc-model-get-all-raw-values model))))
    (exc-model-carry-widths
     model
     (exc-model-create
      (+ (exc-data-model-rows model) rows) (+ old-cols cols)
      (append values
              (cl-loop repeat rows collect (make-list (+ old-cols cols) "")))
      (exc-data-model-merges model)
      (exc-model--same-styles model)
      (exc-data-model-conditionals model)))))

(defun exc-model--same-styles (model)
  "Return a copy of MODEL's styles for a change that moves no cell."
  (exc-model--map-styles model #'cons))

;;;----------------------------------------------------------------------
;;; Cell Merges
;;;----------------------------------------------------------------------

(defun exc-model-get-merges (model)
  "Return MODEL's merge quads as (TOP LEFT BOTTOM RIGHT)."
  (copy-tree (exc-data-model-merges model)))

(defun exc-model-find-merge (model row col)
  "Return the merge covering ROW and COL in MODEL, if any."
  (cl-find-if (lambda (merge)
                (and (<= (nth 0 merge) row)
                     (<= row (nth 2 merge))
                     (<= (nth 1 merge) col)
                     (<= col (nth 3 merge))))
              (exc-data-model-merges model)))

(defun exc-model--merge-overlaps-p (left right)
  "Return non-nil when LEFT and RIGHT merge quads overlap."
  (and (<= (nth 0 left) (nth 2 right))
       (<= (nth 0 right) (nth 2 left))
       (<= (nth 1 left) (nth 3 right))
       (<= (nth 1 right) (nth 3 left))))

(defun exc-model--normalize-merge (merge)
  "Return MERGE with endpoints ordered as TOP LEFT BOTTOM RIGHT."
  (list (min (nth 0 merge) (nth 2 merge))
        (min (nth 1 merge) (nth 3 merge))
        (max (nth 0 merge) (nth 2 merge))
        (max (nth 1 merge) (nth 3 merge))))

(defun exc-model--merge-intersection (left right)
  "Return the closed rectangle where LEFT and RIGHT overlap."
  (list (max (nth 0 left) (nth 0 right))
        (max (nth 1 left) (nth 1 right))
        (min (nth 2 left) (nth 2 right))
        (min (nth 3 left) (nth 3 right))))

(defun exc-model--vertical-merge-p (merge)
  "Return non-nil when MERGE spans rows only."
  (and (< (nth 0 merge) (nth 2 merge))
       (= (nth 1 merge) (nth 3 merge))))

(defun exc-model--merge-shape-error (model merge)
  "Return an error string for malformed MERGE in MODEL, or nil."
  (cond
   ((not (and (listp merge)
              (= (length merge) 4)
              (cl-every #'integerp merge)))
    "malformed merge")
   ((or (< (nth 0 merge) 0)
        (>= (nth 2 merge) (exc-data-model-rows model))
        (< (nth 1 merge) 0)
        (>= (nth 3 merge) (exc-data-model-cols model)))
    "merge out of bounds")
   ((and (= (nth 0 merge) (nth 2 merge))
         (= (nth 1 merge) (nth 3 merge)))
    "merge must span at least two cells")
   ((and exc-model-first-row-is-header
         (= (nth 0 merge) 0)
         (< (nth 0 merge) (nth 2 merge)))
    "vertical merges cannot cross row 0")
   (t nil)))

(defun exc-model--nonempty-covered-cells (model merge)
  "Return coordinates of non-empty covered cells in MERGE."
  (cl-loop for row from (nth 0 merge) to (nth 2 merge)
           append
           (cl-loop for col from (nth 1 merge) to (nth 3 merge)
                    unless (or (and (= row (nth 0 merge))
                                    (= col (nth 1 merge)))
                               (equal (exc-cell-raw-input
                                       (exc-model-get-cell model row col))
                                      ""))
                    collect (list row col))))

(defun exc-model-validate-merges (model merges &optional discard-invalid)
  "Validate MERGES for MODEL.
When DISCARD-INVALID is non-nil, warn and return only valid merges."
  (let ((accepted nil)
        ;; Each row's accepted merges, and the order they came in, so a
        ;; merge is set only against those on its rows.
        (by-row (make-hash-table))
        (order (make-hash-table :test #'eq)))
    (dolist (raw-merge merges)
      (let* ((merge (and (listp raw-merge)
                         (= (length raw-merge) 4)
                         (cl-every #'integerp raw-merge)
                         (exc-model--normalize-merge raw-merge)))
             (reason (exc-model--merge-shape-error
                      model (or merge raw-merge)))
             (covered (and (not reason)
                           (exc-model--nonempty-covered-cells model merge))))
        (cond
         (reason
          (if discard-invalid
              (display-warning 'exc
                               (format "Discarding %S: %s" raw-merge reason))
            (error "Invalid merge %S: %s" raw-merge reason)))
         (covered
          (let ((message
                 (format "covered cells are not empty at %S"
                         covered)))
            (if discard-invalid
                (display-warning
                 'exc (format "Discarding %S: %s" merge message))
              (error "Invalid merge %S: %s" merge message))))
         (t
          (let ((conflicts
                 (sort (cl-remove-if-not
                        (lambda (kept)
                          (exc-model--merge-overlaps-p kept merge))
                        (delete-dups
                         (cl-loop for row from (nth 0 merge) to (nth 2 merge)
                                  append (gethash row by-row))))
                       ;; The latest first, as ACCEPTED holds them.
                       (lambda (a b) (> (gethash a order) (gethash b order))))))
            (if conflicts
                (let ((message
                       (format "conflicts with %S at %S"
                               (nreverse (copy-sequence conflicts))
                               (nreverse
                                (mapcar
                                 (lambda (kept)
                                   (exc-model--merge-intersection
                                    kept merge))
                                 conflicts)))))
                  (if discard-invalid
                      (display-warning
                       'exc (format "Discarding %S: %s" merge message))
                    (error "Invalid merge %S: %s" merge message)))
              (puthash merge (hash-table-count order) order)
              (cl-loop for row from (nth 0 merge) to (nth 2 merge)
                       do (push merge (gethash row by-row)))
              (push merge accepted)))))))
    (sort (nreverse accepted)
          (lambda (left right)
            (cl-loop for a in left
                     for b in right
                     when (/= a b) return (< a b)
                     finally return nil)))))

(defun exc-model-merge-cells (model top left bottom &optional right)
  "Return a new MODEL with a horizontal or vertical merge.
With four arguments, merge LEFT through BOTTOM on TOP for compatibility.
With five arguments, merge TOP LEFT BOTTOM RIGHT."
  (let* ((merge (if right
                    (list top left bottom right)
                  (list top left top bottom)))
         (new-merges (exc-model-validate-merges
                      model
                      (append (exc-data-model-merges model)
                              (list merge)))))
    (exc-model-create (exc-data-model-rows model)
                       (exc-data-model-cols model)
                       (exc-model-get-all-raw-values model)
                       new-merges
                       (exc-model--same-styles model)
                       (exc-data-model-conditionals model))))

(defun exc-model-unmerge-cell (model row col)
  "Return a new MODEL with the merge covering ROW and COL removed."
  (let ((merge (exc-model-find-merge model row col)))
    (exc-model-create (exc-data-model-rows model)
                       (exc-data-model-cols model)
                       (exc-model-get-all-raw-values model)
                       (cl-remove merge (exc-data-model-merges model)
                                  :test #'equal)
                       (exc-model--same-styles model)
                       (exc-data-model-conditionals model))))

;;;----------------------------------------------------------------------
;;; Dependency Graph & Recalculation
;;;----------------------------------------------------------------------

(defun exc-model-recalculate-all (model)
  "Recalculate all formula cells in the model by simple iteration.
This is inefficient but guaranteed to terminate. Includes a circuit breaker."
  (let* ((cells-vec (exc-data-model-cells model))
         (total-cells (length cells-vec))
         (max-iterations (* total-cells 2))) ; Circuit breaker: max iterations = 2 * total cells
    ;; Use exc-calc to evaluate all formulas
    (exc-calc-evaluate model)
    (let (cell-list)
      (dotimes (i total-cells) (push (aref cells-vec i) cell-list))
      (nreverse cell-list))))

;;;----------------------------------------------------------------------
;;; Public API
;;;----------------------------------------------------------------------

(define-error 'exc-model-covered-cell
  "Cannot set non-empty value in covered merged cell")

(defun exc-model-set-raw-value (model row col raw-value)
  "Set the raw value for a cell at ROW, COL and trigger recalculation.
This is the main entry point for changing data in the model.
Returns the modified cell, or nil if the cell does not exist."
  (when-let* ((cell (exc-model-get-cell model row col)))
    (when (and (not (equal raw-value ""))
               (let ((merge (exc-model-find-merge model row col)))
                 (and merge
                      (not (and (= row (nth 0 merge))
                                (= col (nth 1 merge)))))))
      (signal 'exc-model-covered-cell (list row col)))
    (unless (equal raw-value (exc-cell-raw-input cell))
      (exc-model-forget-array model row col)
      (exc-model-forget-legacy model row col))
    (set-exc-cell-raw-input cell raw-value)
    ;; Use exc-calc to set formula and trigger recalculation
    (exc-calc-set-cell-formula model (cons row col) raw-value)
    cell)) ; Return the modified cell

(defun exc-model-get-raw-column-values (model col-idx)
  "Extracts all raw values from a specific column in the model.
Returns a list of raw values for the given column."
  (let* ((num-rows (exc-data-model-rows model))
         (values '()))
    (dotimes (r num-rows)
      (push (exc-cell-raw-input (exc-model-get-cell model r col-idx)) values))
    (nreverse values)))

(defun exc-model--sort-key (value)
  "Return the sort key of computed VALUE as (CLASS . KEY).
Numbers, typed or computed, come first and compare by value; then text,
ignoring case; empty cells always come last."
  (let ((number (and (not (member value '(nil "")))
                     (exc-calc--numeric value))))
    (cond (number (cons 0 number))
          ((member value '(nil "")) (cons 2 nil))
          (t (cons 1 (downcase (format "%s" value)))))))

(defun exc-model--compare-sort-keys (left right descending)
  "Compare sort keys LEFT and RIGHT, returning -1, 0 or 1.
DESCENDING reverses the order inside a class.  Empty cells stay last in
both directions."
  (let ((order
         (cond ((/= (car left) (car right))
                (if (< (car left) (car right)) -1 1))
               ((= (car left) 2) 0)
               ((= (car left) 0)
                (cond ((< (cdr left) (cdr right)) -1)
                      ((> (cdr left) (cdr right)) 1)
                      (t 0)))
               ((string< (cdr left) (cdr right)) -1)
               ((string< (cdr right) (cdr left)) 1)
               (t 0))))
    (if (and descending (/= (car left) 2) (/= (car right) 2))
        (- order)
      order)))

(defun exc-model-sort-by-column (model col-idx sort-order &optional whole)
  "Sorts the model's data rows based on the values in COL-IDX.
SORT-ORDER can be 'ascending or 'descending.
Returns a new model instance with sorted data.

COL-IDX can also be a list of keys, (COL . ORDER) each: rows that the
first key leaves equal are ordered by the second, and so on.  SORT-ORDER
is then not used.

Note: This function sorts the actual data rows, excluding the first row
which is considered user-defined headers. The first row will remain in place.
With WHOLE, the sheet has no header row and its first row is sorted too.
Rows frozen in place stay where they are as well.

A formula in a row that moves has its relative references moved by as
many rows, as if the row had been cut and pasted; anchored parts and
references into other sheets stay.  A reference that names the sheet
being sorted moves like one that names none."
  (let* ((num-rows (exc-data-model-rows model))
         (num-cols (exc-data-model-cols model))
         (keys (if (listp col-idx) col-idx (list (cons col-idx sort-order))))
         (_ (unless keys (error "No column to sort by")))
         (_ (pcase-dolist (`(,col . ,order) keys)
              (unless (and (integerp col) (>= col 0) (< col num-cols))
                (error "Invalid column index: %s" col))
              (unless (memq order '(ascending descending))
                (error "Invalid sort order: %s" order))))
         (_ (when (cl-some #'exc-model--vertical-merge-p
                           (exc-data-model-merges model))
              (error "Cannot sort a model containing vertical merges; unmerge them first")))
         (all-raw-values (exc-model-get-all-raw-values model))
         ;; How many rows stay at the top: the header row and the rows
         ;; frozen in place, which hold titles more than data.
         (kept (max (if whole 0 1)
                    (car (exc-data-model-frozen model))))
         (user-defined-headers (cl-subseq all-raw-values 0 kept))
         (data-rows (nthcdr kept all-raw-values))
         ;; Rows sort by what the column shows, so a formula sorts by its
         ;; result.
         (decorated-data-rows
          (cl-loop for row in data-rows
                   for old-idx from 0
                   collect (list old-idx row
                                 (mapcar (lambda (key)
                                           (exc-model--sort-key
                                            (exc-model-get-computed-value
                                             model (+ kept old-idx) (car key))))
                                         keys))))
         (sorted-data-rows
          (sort decorated-data-rows
                (lambda (row1 row2)
                  (let ((order (cl-loop
                                for left in (nth 2 row1)
                                for right in (nth 2 row2)
                                for key in keys
                                for order = (exc-model--compare-sort-keys
                                             left right
                                             (eq (cdr key) 'descending))
                                unless (= order 0) return order
                                finally return 0)))
                    ;; Equal values keep their order, in either direction.
                    (if (= order 0)
                        (< (nth 0 row1) (nth 0 row2))
                      (< order 0)))))))
    ;; Reconstruct the new raw values list with user-defined headers at the top
    (let* ((new-raw-values
            (append user-defined-headers
                  ;; A formula moves with its row, so what it reads in its
                  ;; own row is still its own row afterwards.
                  (cl-loop for (old-idx row) in sorted-data-rows
                           for new-row from 0
                           collect
                           (if (= old-idx new-row)
                               row
                             (mapcar (lambda (raw)
                                       (exc-calc-shift-formula
                                        raw (- new-row old-idx) 0
                                        (or exc-calc-current-sheet t)))
                                     row)))))
           (old-to-new (make-vector (length data-rows) nil))
           (new-idx 0)
           (new-merges
            (mapcar (lambda (merge)
                      (if (< (nth 0 merge) kept)
                          merge
                        (list (+ kept (aref old-to-new (- (nth 0 merge) kept)))
                              (nth 1 merge)
                              (+ kept (aref old-to-new (- (nth 2 merge) kept)))
                              (nth 3 merge))))
                    (progn
                      (dolist (row sorted-data-rows)
                        (aset old-to-new (car row) new-idx)
                        (setq new-idx (1+ new-idx)))
                      (exc-data-model-merges model)))))
      ;; Create a new model with the sorted data
      (let ((sorted
             (exc-model-create
              num-rows num-cols new-raw-values new-merges
              ;; A style travels with its row; conditional ranges stay in place.
              (exc-model--map-styles
               model (lambda (row col)
                       (cons (if (< row kept)
                                 row
                               (+ kept (aref old-to-new (- row kept))))
                             col)))
              (exc-data-model-conditionals model)))
            (hidden (exc-data-model-hidden model)))
        ;; A hidden row stays hidden wherever it goes.
        (exc-model-set-hidden
         sorted
         (mapcar (lambda (row)
                   ;; One past the last cell is not sorted.
                   (if (or (< row kept)
                           (>= (- row kept) (length old-to-new)))
                       row
                     (+ kept (aref old-to-new (- row kept)))))
                 (car hidden))
         (cdr hidden))
        sorted))))

(defun exc-model-get-all-raw-values (model)
  "Extracts all raw values (headers and cell contents) from the model.
Returns a cons cell: (HEADERS . LIST-OF-LISTS-OF-RAW-VALUES)."
  (let* ((num-rows (exc-data-model-rows model))
         (num-cols (exc-data-model-cols model))
         (all-raw-rows (cl-loop for r from 0 below num-rows
                                collect (cl-loop for c from 0 below num-cols
                                                 collect (exc-cell-raw-input
                                                         (exc-model-get-cell model r c))))))
    (cons (nth 0 all-raw-rows) (cl-subseq all-raw-rows 1)))) ; Return first row as headers, rest as data

(defun exc-model--restructure-formulas (raw-rows axis at delta)
  "Return RAW-ROWS with every formula rewritten for a structural change.
AXIS, AT and DELTA are as for `exc-calc-restructure-formula'."
  (mapcar (lambda (row)
            (mapcar (lambda (value)
                      (exc-calc-restructure-formula
                       value axis at delta exc-calc-current-sheet))
                    row))
          raw-rows))

(defun exc-model-add-row (model &optional row-idx)
  "Adds a new empty row to the model at ROW-IDX (0-based).
If ROW-IDX is nil or out of bounds, adds to the end.
Returns a new model instance."
  (let* ((current-rows (exc-data-model-rows model))
         (current-cols (exc-data-model-cols model))
         (all-raw-values (exc-model-get-all-raw-values model))
         (user-defined-headers (car all-raw-values)) ; Get user-defined headers (first row)
         (data-rows (cdr all-raw-values)) ; Actual data rows (excluding user-defined headers)
         (new-row (make-list current-cols ""))
         (new-data-rows (copy-sequence data-rows)))
    (if (and row-idx (>= row-idx 0) (< row-idx (length data-rows)))
        (setf new-data-rows (append (cl-subseq new-data-rows 0 row-idx)
                                    (list new-row)
                                    (cl-subseq new-data-rows row-idx)))
      (setf new-data-rows (append new-data-rows (list new-row))))
    ;; Reconstruct the complete raw values with user-defined headers at the top
    ;; A row index of -1 puts the new row above the first row.
    (let* ((above (eql row-idx -1))
           (new-raw-values (if above
                               (cons new-row (cons user-defined-headers data-rows))
                             (cons user-defined-headers new-data-rows)))
           (insert-row (cond (above 0)
                             ((and row-idx (>= row-idx 0) (< row-idx (length data-rows)))
                              (1+ row-idx))
                             (t current-rows)))
           (new-merges (mapcar (lambda (merge)
                                 (let ((top (nth 0 merge))
                                       (left (nth 1 merge))
                                       (bottom (nth 2 merge))
                                       (right (nth 3 merge)))
                                   (cond
                                    ((<= insert-row top)
                                     (list (1+ top) left (1+ bottom) right))
                                    ((<= insert-row bottom)
                                     (list top left (1+ bottom) right))
                                    (t merge))))
                               (exc-data-model-merges model))))
      (exc-model-create
       (1+ current-rows) current-cols
       (exc-model--restructure-formulas new-raw-values 'row insert-row 1)
       new-merges
       (exc-model--map-styles
        model (lambda (row col)
                (cons (exc-model--shift-index row insert-row 1) col)))
       (exc-model--shift-rules (exc-data-model-conditionals model)
                                'row insert-row 1)))))

(defun exc-model-delete-row (model row-idx)
  "Deletes the row at ROW-IDX (0-based) from the model.
Returns a new model instance. Errors if ROW-IDX is invalid or only one row remains."
  (let* ((current-rows (exc-data-model-rows model))
         (current-cols (exc-data-model-cols model))
         (all-raw-values (exc-model-get-all-raw-values model))
         (user-defined-headers (car all-raw-values)) ; Get user-defined headers (first row)
         (data-rows (cdr all-raw-values)) ; Actual data rows (excluding user-defined headers)
         (new-data-rows (copy-sequence data-rows)))
    ;; A row index of -1 names the first row of the sheet.
    (unless (and (>= row-idx -1) (< row-idx (length data-rows)))
      (error "Invalid row index: %s" row-idx))
    (when (if (= row-idx -1) (null data-rows) (= (length data-rows) 1))
      (error "Cannot delete the last row."))
    (let ((delete-row (1+ row-idx)))
      (dolist (merge (exc-data-model-merges model))
        (when (and (= delete-row (nth 0 merge))
                   (< (nth 0 merge) (nth 2 merge)))
          (let ((raw-rows (cons user-defined-headers new-data-rows)))
            (setf (nth (nth 1 merge) (nth (1+ delete-row) raw-rows))
                  (nth (nth 1 merge) (nth delete-row raw-rows)))))))
    (if (= row-idx -1)
        (setq user-defined-headers (car new-data-rows)
              new-data-rows (cdr new-data-rows))
      (setf new-data-rows (append (cl-subseq new-data-rows 0 row-idx)
                                  (cl-subseq new-data-rows (1+ row-idx)))))
    ;; Reconstruct the complete raw values with user-defined headers at the top
    (let* ((new-raw-values (cons user-defined-headers new-data-rows))
           (delete-row (1+ row-idx))
           (new-merges
            (delq nil
                  (mapcar
                   (lambda (merge)
                     (let ((top (nth 0 merge))
                           (left (nth 1 merge))
                           (bottom (nth 2 merge))
                           (right (nth 3 merge)))
                       (cond
                        ((< delete-row top)
                         (list (1- top) left (1- bottom) right))
                        ((> delete-row bottom) merge)
                        ((= top bottom) nil)
                        (t (let ((candidate (list top left (1- bottom) right)))
                             (unless (and (= (nth 0 candidate) (nth 2 candidate))
                                          (= (nth 1 candidate) (nth 3 candidate)))
                               candidate))))))
                   (exc-data-model-merges model)))))
      (exc-model-create
       (1- current-rows) current-cols
       (exc-model--restructure-formulas new-raw-values 'row delete-row -1)
       new-merges
       (exc-model--map-styles
        model (lambda (row col)
                (let ((moved (exc-model--shift-index row delete-row -1)))
                  (and moved (cons moved col)))))
       (exc-model--shift-rules (exc-data-model-conditionals model)
                                'row delete-row -1)))))

(defun exc-model-delete-rows (model rows)
  "Return MODEL without its ROWS, a list of row indexes counted from 0.
The result is the same as deleting the rows one at a time, but the
model is built once: formulas, merges, styles and conditional formats
follow the rows that stay, and references to deleted cells become
#REF!.  Errors when ROWS would leave no row."
  (let* ((rows (sort (delete-dups (copy-sequence rows)) #'<))
         (count (exc-data-model-rows model))
         (cols (exc-data-model-cols model))
         (raw (vconcat (mapcar #'copy-sequence
                               (exc-model-get-all-raw-values model))))
         (merges (exc-data-model-merges model)))
    (unless (cl-every (lambda (row) (and (natnump row) (< row count))) rows)
      (error "Invalid row index in %S" rows))
    (when (>= (length rows) count)
      (error "Cannot delete the last row."))
    ;; From the last row up, as deleting them one at a time would.
    (dolist (row (reverse rows))
      ;; A vertical merge that loses its top row keeps its value, in the
      ;; row now under it: the next one that stays, as the rows below
      ;; ROW in ROWS are gone already.
      (dolist (merge merges)
        (when (and (= row (nth 0 merge)) (< (nth 0 merge) (nth 2 merge)))
          (let ((next (cl-loop for r from (1+ row)
                               unless (memql r rows) return r)))
            (setf (nth (nth 1 merge) (aref raw next))
                  (nth (nth 1 merge) (aref raw row))))))
      (setq merges
            (delq nil
                  (mapcar
                   (lambda (merge)
                     (pcase-let ((`(,top ,left ,bottom ,right) merge))
                       (cond ((< row top)
                              (list (1- top) left (1- bottom) right))
                             ((> row bottom) merge)
                             ((= top bottom) nil)
                             ((and (= top (1- bottom)) (= left right)) nil)
                             (t (list top left (1- bottom) right)))))
                   merges))))
    (exc-model-create
     (- count (length rows)) cols
     (exc-model--restructure-formulas
      (cl-loop for row below count
               unless (memql row rows) collect (aref raw row))
      'row rows -1)
     merges
     (exc-model--map-styles
      model (lambda (row col)
              (let ((moved (exc-model--shift-index row rows -1)))
                (and moved (cons moved col)))))
     (exc-model--shift-rules (exc-data-model-conditionals model)
                              'row rows -1))))

(defun exc-model-add-column (model &optional col-idx)
  "Adds a new empty column to the model at COL-IDX (0-based).
If COL-IDX is nil or out of bounds, adds to the end.
Returns a new model instance."
  (let* ((current-rows (exc-data-model-rows model))
         (current-cols (exc-data-model-cols model))
         (all-raw-values (exc-model-get-all-raw-values model))
         (user-defined-headers (car all-raw-values)) ; Get user-defined headers (first row)
         (data-rows (cdr all-raw-values)) ; Actual data rows (excluding user-defined headers)
         (new-user-defined-headers (copy-sequence user-defined-headers))
         (new-data-rows (cl-loop for row in data-rows
                                  collect (let ((new-row (copy-sequence row)))
                                            (if (and col-idx (>= col-idx 0) (<= col-idx current-cols))
                                                (append (cl-subseq new-row 0 col-idx)
                                                        (list "")
                                                        (cl-subseq new-row col-idx))
                                              (append new-row (list "")))))))
    ;; Add empty column to user-defined headers as well
    (if (and col-idx (>= col-idx 0) (<= col-idx (length new-user-defined-headers)))
        (setf new-user-defined-headers (append (cl-subseq new-user-defined-headers 0 col-idx)
                                               (list "")
                                               (cl-subseq new-user-defined-headers col-idx)))
      (setf new-user-defined-headers (append new-user-defined-headers (list ""))))
    ;; Reconstruct the complete raw values with user-defined headers at the top
    (let* ((new-raw-values (cons new-user-defined-headers new-data-rows))
           (insert-col (if (and col-idx (>= col-idx 0) (<= col-idx current-cols))
                           col-idx
                         current-cols))
           (new-merges (mapcar (lambda (merge)
                                 (let ((top (nth 0 merge))
                                       (left (nth 1 merge))
                                       (bottom (nth 2 merge))
                                       (right (nth 3 merge)))
                                   (cond
                                    ((<= insert-col left)
                                     (list top (1+ left) bottom (1+ right)))
                                    ((<= insert-col right)
                                     (list top left bottom (1+ right)))
                                    (t merge))))
                               (exc-data-model-merges model))))
      (exc-model-create
       current-rows (1+ current-cols)
       (exc-model--restructure-formulas new-raw-values 'col insert-col 1)
       new-merges
       (exc-model--map-styles
        model (lambda (row col)
                (cons row (exc-model--shift-index col insert-col 1))))
       (exc-model--shift-rules (exc-data-model-conditionals model)
                                'col insert-col 1)))))

(defun exc-model-delete-column (model col-idx)
  "Deletes the column at COL-IDX (0-based) from the model.
Returns a new model instance. Errors if COL-IDX is invalid or only one column remains."
  (let* ((current-rows (exc-data-model-rows model))
         (current-cols (exc-data-model-cols model))
         (raw-rows (exc-model-get-all-raw-values model)))
    (unless (and (>= col-idx 0) (< col-idx current-cols))
      (error "Invalid column index: %s" col-idx))
    (when (= current-cols 1)
      (error "Cannot delete the last column."))
    (dolist (merge (exc-data-model-merges model))
      (when (and (= col-idx (nth 1 merge))
                 (< (nth 1 merge) (nth 3 merge)))
        (let* ((row (nth 0 merge))
               (raw-row (nth row raw-rows)))
          (setf (nth (1+ col-idx) raw-row)
                (nth col-idx raw-row)))))
    (let* ((new-raw-rows (cl-loop for row in raw-rows
                                  collect (append (cl-subseq row 0 col-idx)
                                                  (cl-subseq row (1+ col-idx)))))
           (new-user-defined-headers (car new-raw-rows))
           (new-data-rows (cdr new-raw-rows))
           (new-merges
            (delq nil
                  (mapcar
                   (lambda (merge)
                     (let ((top (nth 0 merge))
                           (left (nth 1 merge))
                           (bottom (nth 2 merge))
                           (right (nth 3 merge)))
                       (cond
                        ((< col-idx left)
                         (list top (1- left) bottom (1- right)))
                        ((> col-idx right) merge)
                        ((= left right) nil)
                        (t (let ((candidate (list top left bottom (1- right))))
                             (unless (and (= (nth 0 candidate) (nth 2 candidate))
                                          (= (nth 1 candidate) (nth 3 candidate)))
                               candidate))))))
                   (exc-data-model-merges model)))))
    ;; Delete column from user-defined headers as well
    ;; Reconstruct the complete raw values with user-defined headers at the top
    (let ((new-raw-values (cons new-user-defined-headers new-data-rows)))
      (exc-model-create
       current-rows (1- current-cols)
       (exc-model--restructure-formulas new-raw-values 'col col-idx -1)
       new-merges
       (exc-model--map-styles
        model (lambda (row col)
                (let ((moved (exc-model--shift-index col col-idx -1)))
                  (and moved (cons row moved)))))
       (exc-model--shift-rules (exc-data-model-conditionals model)
                                'col col-idx -1))))))

(provide 'exc-data-model)

;;; exc-data-model.el ends here
