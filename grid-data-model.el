;;; grid-data-model.el --- Data model and calculation engine for grid-table -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'grid-table-parser) ; For parsing formulas
(require 'grid-table-calc)   ; For evaluating formulas


;;;----------------------------------------------------------------------
;;; Manual Grid Cell "Struct" Implementation (using vectors)
;;;----------------------------------------------------------------------

;; Constants for slot indices
(defconst grid-cell--raw-input 0)
(defconst grid-cell--computed-value 1)
;; (RAW . PARSED): the formula the cell holds, parsed
(defconst grid-cell--parsed 3)
(defconst grid-cell--error-msg 4)
(defconst grid-cell--row 5)
(defconst grid-cell--col 6)
;; What the formula was recorded as reading; see `grid-calc--register'
(defconst grid-cell--reads 7)
(defconst grid-cell--length 8)

;; Manual constructor
(defun make-grid-cell (row col)
  "Create a new grid cell vector."
  (let ((cell (make-vector grid-cell--length nil)))
    (aset cell grid-cell--row row)
    (aset cell grid-cell--col col)
    (aset cell grid-cell--raw-input "")
    cell))

;; Manual accessors and setters
(defun grid-cell-raw-input (cell) (aref cell grid-cell--raw-input))
(defun grid-cell-computed-value (cell) (aref cell grid-cell--computed-value))
(defun grid-cell-row (cell) (aref cell grid-cell--row))
(defun grid-cell-col (cell) (aref cell grid-cell--col))
(defun grid-cell-error-msg (cell) (aref cell grid-cell--error-msg))

(defun set-grid-cell-raw-input (cell value) (aset cell grid-cell--raw-input value))
(defun set-grid-cell-computed-value (cell value) (aset cell grid-cell--computed-value value))

;;;----------------------------------------------------------------------
;;; Data Model
;;;----------------------------------------------------------------------

(defvar grid-model-first-row-is-header t
  "Non-nil when the first row of the model at work holds column titles.
A merge may then not reach from that row into the rows below it.  A data
source whose sheet has no header row binds this to nil.")

;; Manual grid data model implementation
(defun make-grid-data-model (rows cols cells &optional merges styles
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
          (cons 0 0)))

(defun grid-data-model-rows (model) (aref model 0))
(defun grid-data-model-cols (model) (aref model 1))
(defun grid-data-model-cells (model) (aref model 2))
(defun grid-data-model-merges (model)
  (when (> (length model) 3)
    (aref model 3)))
(defun set-grid-data-model-merges (model value)
  (aset model 3 value))
(defun grid-data-model-styles (model)
  (when (> (length model) 4)
    (aref model 4)))
(defun grid-data-model-conditionals (model)
  (when (> (length model) 5)
    (aref model 5)))
(defun grid-data-model-widths (model)
  "Return MODEL's table of column widths set by hand, column to characters."
  (when (> (length model) 6)
    (aref model 6)))
(defun grid-data-model-frozen (model)
  "Return (ROWS . COLS): the leading rows and columns of MODEL kept in place."
  (or (and (> (length model) 7) (aref model 7))
      (cons 0 0)))

;;;----------------------------------------------------------------------
;;; Model Creation and Access
;;;----------------------------------------------------------------------

(defun grid-model-create (rows cols &optional initial-data merges styles
                              conditionals)
  "Create a new data model of ROWS and COLS.
Optionally populate with INITIAL-DATA (a list of lists of strings).
STYLES and CONDITIONALS, when given, become the model's cell styles and
conditional format rules; the model owns them from then on."
  (let* ((model (make-grid-data-model rows cols (make-vector (* rows cols) nil)
                                      nil styles conditionals))
         (cells (grid-data-model-cells model)))
    ;; 1. Initialize all cells and populate with initial data
    ;; Walk INITIAL-DATA alongside the indexes: looking a row up by number
    ;; for every cell is quadratic in the row count.
    (let ((rest-rows initial-data))
      (dotimes (r rows)
        (let ((rest-values (pop rest-rows)))
          (dotimes (c cols)
            (let ((cell (make-grid-cell r c))
                  (raw-val (if rest-values (pop rest-values) "")))
              (set-grid-cell-raw-input cell raw-val)
              ;; For non-formula cells, set computed value to raw value
              (unless (string-prefix-p "=" raw-val)
                (set-grid-cell-computed-value cell raw-val))
              (aset cells (+ (* r cols) c) cell))))))
    ;; 2. Perform initial calculation
    (grid-model-recalculate-all model)
    (when merges
      (set-grid-data-model-merges
       model (grid-model-validate-merges model merges)))
    model))

(defun grid-model-get-cell (model row col)
  "Get the cell struct at ROW and COL from the MODEL."
  (when (and (>= row 0) (< row (grid-data-model-rows model))
             (>= col 0) (< col (grid-data-model-cols model)))
    (aref (grid-data-model-cells model) (+ (* row (grid-data-model-cols model)) col))))

(defun grid-model-get-computed-value (model row col)
  "Get the computed value of the cell at ROW and COL.
If the cell is a formula, it will be evaluated on demand."
  (when-let* ((cell (grid-model-get-cell model row col)))
    (grid-cell-computed-value cell)))

;;;----------------------------------------------------------------------
;;; Cell Styles and Conditional Formats
;;;----------------------------------------------------------------------

;; A style is a plist with any of :bold, :italic (non-nil), :color,
;; :background (color strings) and :align (left, center or right).
;; A conditional format rule is (TOP LEFT BOTTOM RIGHT OP VALUE STYLE): cells
;; of the range whose value satisfies OP against VALUE take STYLE.

(defun grid-model-get-style (model row col)
  "Return a copy of the style plist of the cell at ROW and COL in MODEL."
  (let ((styles (grid-data-model-styles model)))
    (and styles (copy-sequence (gethash (cons row col) styles)))))

(defun grid-model-set-style (model row col style)
  "Give the cell at ROW and COL in MODEL the plist STYLE; nil clears it."
  (let ((styles (grid-data-model-styles model)))
    (when styles
      (if style
          (puthash (cons row col) (copy-sequence style) styles)
        (remhash (cons row col) styles))
      ;; The text format changes what a typed cell is worth.
      (grid-calc-touch model row col))
    style))

(defun grid-model-get-conditionals (model)
  "Return a copy of MODEL's conditional format rules."
  (copy-tree (grid-data-model-conditionals model)))

(defun grid-model-set-conditionals (model rules)
  "Replace MODEL's conditional format rules with RULES."
  (when (> (length model) 5)
    (aset model 5 (copy-tree rules)))
  rules)

(defun grid-model--map-styles (model function)
  "Return MODEL's styles re-keyed for a structural change.
FUNCTION receives a cell's ROW and COL and returns its new (ROW . COL),
or nil when the cell is gone."
  (let ((new (make-hash-table :test 'equal))
        (styles (grid-data-model-styles model)))
    (when styles
      (maphash (lambda (key style)
                 (let ((moved (funcall function (car key) (cdr key))))
                   (when moved
                     (puthash moved style new))))
               styles))
    new))

(defun grid-model--shift-index (index at delta)
  "Return INDEX after a line is inserted at AT (DELTA 1) or deleted (-1).
A deleted line itself gives nil."
  (cond ((> delta 0) (if (>= index at) (1+ index) index))
        ((= index at) nil)
        ((> index at) (1- index))
        (t index)))

(defun grid-model--shift-rules (rules axis at delta)
  "Return conditional RULES after a line change along AXIS.
AXIS is `row' or `col', AT the index of the line and DELTA 1 for an
insertion or -1 for a deletion.  A rule whose range is deleted
entirely is dropped."
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
           rules))))

(defun grid-model-set-column-width (model col width)
  "Give column COL of MODEL a width of WIDTH characters; nil forgets it."
  (when-let* ((widths (grid-data-model-widths model)))
    (if width
        (puthash col width widths)
      (remhash col widths))
    width))

(defun grid-model-set-frozen (model rows cols)
  "Keep the first ROWS rows and COLS columns of MODEL in place.
A row and a column are always left to scroll."
  (when (> (length model) 7)
    (aset model 7
          (cons (max 0 (min rows (1- (grid-data-model-rows model))))
                (max 0 (min cols (1- (grid-data-model-cols model))))))))

(defun grid-model--frozen-after (count at delta)
  "Return frozen COUNT after a line was inserted or deleted at AT by DELTA."
  (if (and at (< at count)) (+ count delta) count))

(defun grid-model-carry-frozen (old new &optional axis at delta)
  "Give model NEW the frozen rows and columns of model OLD and return NEW.
AXIS is `row' or `col' when a line was inserted at AT (DELTA 1) or
deleted there (DELTA -1) on the way; a line inside the frozen ones
changes how many there are."
  (unless (eq old new)
    (let ((frozen (grid-data-model-frozen old)))
      (grid-model-set-frozen
       new
       (grid-model--frozen-after (car frozen) (and (eq axis 'row) at) delta)
       (grid-model--frozen-after (cdr frozen) (and (eq axis 'col) at) delta))))
  new)

(defun grid-model-carry-widths (old new &optional at delta)
  "Give model NEW the column widths of model OLD and return NEW.
AT and DELTA describe a column inserted at AT (DELTA 1) or deleted there
\(DELTA -1) on the way from OLD to NEW; the widths move with their
columns."
  (let ((from (grid-data-model-widths old))
        (to (grid-data-model-widths new)))
    (when (and from to (not (eq old new)))
      (clrhash to)
      (maphash (lambda (col width)
                 (let ((moved (if at
                                  (grid-model--shift-index col at delta)
                                col)))
                   (when (and moved (< moved (grid-data-model-cols new)))
                     (puthash moved width to))))
               from))
    new))

(defun grid-model-grow (model rows cols)
  "Return MODEL with ROWS more rows at the bottom and COLS more columns.
The new cells are empty; nothing that was there moves."
  (let* ((old-cols (grid-data-model-cols model))
         (values (mapcar (lambda (row) (append row (make-list cols "")))
                         (grid-model-get-all-raw-values model))))
    (grid-model-carry-widths
     model
     (grid-model-create
      (+ (grid-data-model-rows model) rows) (+ old-cols cols)
      (append values
              (cl-loop repeat rows collect (make-list (+ old-cols cols) "")))
      (grid-data-model-merges model)
      (grid-model--same-styles model)
      (grid-data-model-conditionals model)))))

(defun grid-model--same-styles (model)
  "Return a copy of MODEL's styles for a change that moves no cell."
  (grid-model--map-styles model #'cons))

;;;----------------------------------------------------------------------
;;; Cell Merges
;;;----------------------------------------------------------------------

(defun grid-model-get-merges (model)
  "Return MODEL's merge quads as (TOP LEFT BOTTOM RIGHT)."
  (copy-tree (grid-data-model-merges model)))

(defun grid-model-find-merge (model row col)
  "Return the merge covering ROW and COL in MODEL, if any."
  (cl-find-if (lambda (merge)
                (and (<= (nth 0 merge) row)
                     (<= row (nth 2 merge))
                     (<= (nth 1 merge) col)
                     (<= col (nth 3 merge))))
              (grid-data-model-merges model)))

(defun grid-model--merge-overlaps-p (left right)
  "Return non-nil when LEFT and RIGHT merge quads overlap."
  (and (<= (nth 0 left) (nth 2 right))
       (<= (nth 0 right) (nth 2 left))
       (<= (nth 1 left) (nth 3 right))
       (<= (nth 1 right) (nth 3 left))))

(defun grid-model--normalize-merge (merge)
  "Return MERGE with endpoints ordered as TOP LEFT BOTTOM RIGHT."
  (list (min (nth 0 merge) (nth 2 merge))
        (min (nth 1 merge) (nth 3 merge))
        (max (nth 0 merge) (nth 2 merge))
        (max (nth 1 merge) (nth 3 merge))))

(defun grid-model--merge-intersection (left right)
  "Return the closed rectangle where LEFT and RIGHT overlap."
  (list (max (nth 0 left) (nth 0 right))
        (max (nth 1 left) (nth 1 right))
        (min (nth 2 left) (nth 2 right))
        (min (nth 3 left) (nth 3 right))))

(defun grid-model--vertical-merge-p (merge)
  "Return non-nil when MERGE spans rows only."
  (and (< (nth 0 merge) (nth 2 merge))
       (= (nth 1 merge) (nth 3 merge))))

(defun grid-model--merge-shape-error (model merge)
  "Return an error string for malformed MERGE in MODEL, or nil."
  (cond
   ((not (and (listp merge)
              (= (length merge) 4)
              (cl-every #'integerp merge)))
    "malformed merge")
   ((or (< (nth 0 merge) 0)
        (>= (nth 2 merge) (grid-data-model-rows model))
        (< (nth 1 merge) 0)
        (>= (nth 3 merge) (grid-data-model-cols model)))
    "merge out of bounds")
   ((and (= (nth 0 merge) (nth 2 merge))
         (= (nth 1 merge) (nth 3 merge)))
    "merge must span at least two cells")
   ((and grid-model-first-row-is-header
         (= (nth 0 merge) 0)
         (< (nth 0 merge) (nth 2 merge)))
    "vertical merges cannot cross row 0")
   (t nil)))

(defun grid-model--nonempty-covered-cells (model merge)
  "Return coordinates of non-empty covered cells in MERGE."
  (cl-loop for row from (nth 0 merge) to (nth 2 merge)
           append
           (cl-loop for col from (nth 1 merge) to (nth 3 merge)
                    unless (or (and (= row (nth 0 merge))
                                    (= col (nth 1 merge)))
                               (equal (grid-cell-raw-input
                                       (grid-model-get-cell model row col))
                                      ""))
                    collect (list row col))))

(defun grid-model-validate-merges (model merges &optional discard-invalid)
  "Validate MERGES for MODEL.
When DISCARD-INVALID is non-nil, warn and return only valid merges."
  (let (accepted)
    (dolist (raw-merge merges)
      (let* ((merge (and (listp raw-merge)
                         (= (length raw-merge) 4)
                         (cl-every #'integerp raw-merge)
                         (grid-model--normalize-merge raw-merge)))
             (reason (grid-model--merge-shape-error
                      model (or merge raw-merge)))
             (covered (and (not reason)
                           (grid-model--nonempty-covered-cells model merge))))
        (cond
         (reason
          (if discard-invalid
              (display-warning 'grid-table
                               (format "Discarding %S: %s" raw-merge reason))
            (error "Invalid merge %S: %s" raw-merge reason)))
         (covered
          (let ((message
                 (format "covered cells are not empty at %S"
                         covered)))
            (if discard-invalid
                (display-warning
                 'grid-table (format "Discarding %S: %s" merge message))
              (error "Invalid merge %S: %s" merge message))))
         (t
          (let ((conflicts
                 (cl-remove-if-not
                  (lambda (kept)
                    (grid-model--merge-overlaps-p kept merge))
                  accepted)))
            (if conflicts
                (let ((message
                       (format "conflicts with %S at %S"
                               (nreverse (copy-sequence conflicts))
                               (nreverse
                                (mapcar
                                 (lambda (kept)
                                   (grid-model--merge-intersection
                                    kept merge))
                                 conflicts)))))
                  (if discard-invalid
                      (display-warning
                       'grid-table (format "Discarding %S: %s" merge message))
                    (error "Invalid merge %S: %s" merge message)))
              (push merge accepted)))))))
    (sort (nreverse accepted)
          (lambda (left right)
            (cl-loop for a in left
                     for b in right
                     when (/= a b) return (< a b)
                     finally return nil)))))

(defun grid-model-merge-cells (model top left bottom &optional right)
  "Return a new MODEL with a horizontal or vertical merge.
With four arguments, merge LEFT through BOTTOM on TOP for compatibility.
With five arguments, merge TOP LEFT BOTTOM RIGHT."
  (let* ((merge (if right
                    (list top left bottom right)
                  (list top left top bottom)))
         (new-merges (grid-model-validate-merges
                      model
                      (append (grid-data-model-merges model)
                              (list merge)))))
    (grid-model-create (grid-data-model-rows model)
                       (grid-data-model-cols model)
                       (grid-model-get-all-raw-values model)
                       new-merges
                       (grid-model--same-styles model)
                       (grid-data-model-conditionals model))))

(defun grid-model-unmerge-cell (model row col)
  "Return a new MODEL with the merge covering ROW and COL removed."
  (let ((merge (grid-model-find-merge model row col)))
    (grid-model-create (grid-data-model-rows model)
                       (grid-data-model-cols model)
                       (grid-model-get-all-raw-values model)
                       (cl-remove merge (grid-data-model-merges model)
                                  :test #'equal)
                       (grid-model--same-styles model)
                       (grid-data-model-conditionals model))))

;;;----------------------------------------------------------------------
;;; Dependency Graph & Recalculation
;;;----------------------------------------------------------------------

(defun grid-model-recalculate-all (model)
  "Recalculate all formula cells in the model by simple iteration.
This is inefficient but guaranteed to terminate. Includes a circuit breaker."
  (let* ((cells-vec (grid-data-model-cells model))
         (total-cells (length cells-vec))
         (max-iterations (* total-cells 2))) ; Circuit breaker: max iterations = 2 * total cells
    ;; Use grid-table-calc to evaluate all formulas
    (grid-table-calc-evaluate model)
    (let (cell-list)
      (dotimes (i total-cells) (push (aref cells-vec i) cell-list))
      (nreverse cell-list))))

;;;----------------------------------------------------------------------
;;; Public API
;;;----------------------------------------------------------------------

(defun grid-model-set-raw-value (model row col raw-value)
  "Set the raw value for a cell at ROW, COL and trigger recalculation.
This is the main entry point for changing data in the model.
Returns the modified cell, or nil if the cell does not exist."
  (when-let* ((cell (grid-model-get-cell model row col)))
    (when (and (not (equal raw-value ""))
               (let ((merge (grid-model-find-merge model row col)))
                 (and merge
                      (not (and (= row (nth 0 merge))
                                (= col (nth 1 merge)))))))
      (error "Cannot set non-empty value in covered merged cell"))
    (set-grid-cell-raw-input cell raw-value)
    ;; Use grid-table-calc to set formula and trigger recalculation
    (grid-table-calc-set-cell-formula model (cons row col) raw-value)
    cell)) ; Return the modified cell

(defun grid-model-get-raw-column-values (model col-idx)
  "Extracts all raw values from a specific column in the model.
Returns a list of raw values for the given column."
  (let* ((num-rows (grid-data-model-rows model))
         (values '()))
    (dotimes (r num-rows)
      (push (grid-cell-raw-input (grid-model-get-cell model r col-idx)) values))
    (nreverse values)))

(defun grid-model--sort-key (value)
  "Return the sort key of computed VALUE as (CLASS . KEY).
Numbers, typed or computed, come first and compare by value; then text,
ignoring case; empty cells always come last."
  (let ((number (and (not (member value '(nil "")))
                     (grid-calc--numeric value))))
    (cond (number (cons 0 number))
          ((member value '(nil "")) (cons 2 nil))
          (t (cons 1 (downcase (format "%s" value)))))))

(defun grid-model--compare-sort-keys (left right descending)
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

(defun grid-model-sort-by-column (model col-idx sort-order &optional whole)
  "Sorts the model's data rows based on the values in COL-IDX.
SORT-ORDER can be 'ascending or 'descending.
Returns a new model instance with sorted data.

Note: This function sorts the actual data rows, excluding the first row
which is considered user-defined headers. The first row will remain in place.
With WHOLE, the sheet has no header row and its first row is sorted too.

A formula in a row that moves has its relative references moved by as
many rows, as if the row had been cut and pasted; anchored parts and
references into other sheets stay.  A reference that names the sheet
being sorted moves like one that names none."
  (let* ((num-rows (grid-data-model-rows model))
         (num-cols (grid-data-model-cols model))
         (_ (unless (and (integerp col-idx)
                         (>= col-idx 0)
                         (< col-idx num-cols))
              (error "Invalid column index: %s" col-idx)))
         (_ (unless (memq sort-order '(ascending descending))
              (error "Invalid sort order: %s" sort-order)))
         (_ (when (cl-some #'grid-model--vertical-merge-p
                           (grid-data-model-merges model))
              (error "Cannot sort a model containing vertical merges; unmerge them first")))
         (all-raw-values (grid-model-get-all-raw-values model))
         ;; How many rows stay at the top: the header row, or none
         (kept (if whole 0 1))
         (user-defined-headers (cl-subseq all-raw-values 0 kept))
         (data-rows (nthcdr kept all-raw-values))
         ;; Rows sort by what the column shows, so a formula sorts by its
         ;; result.
         (decorated-data-rows
          (cl-loop for row in data-rows
                   for old-idx from 0
                   collect (list old-idx row
                                 (grid-model--sort-key
                                  (grid-model-get-computed-value
                                   model (+ kept old-idx) col-idx)))))
         (sorted-data-rows
          (sort decorated-data-rows
                (lambda (row1 row2)
                  (let ((order (grid-model--compare-sort-keys
                                (nth 2 row1) (nth 2 row2)
                                (eq sort-order 'descending))))
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
                                       (grid-calc-shift-formula
                                        raw (- new-row old-idx) 0
                                        (or grid-calc-current-sheet t)))
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
                      (grid-data-model-merges model)))))
      ;; Create a new model with the sorted data
      (grid-model-create
       num-rows num-cols new-raw-values new-merges
       ;; A style travels with its row; conditional ranges stay in place.
       (grid-model--map-styles
        model (lambda (row col)
                (cons (if (< row kept)
                          row
                        (+ kept (aref old-to-new (- row kept))))
                      col)))
       (grid-data-model-conditionals model)))))

(defun grid-model-get-all-raw-values (model)
  "Extracts all raw values (headers and cell contents) from the model.
Returns a cons cell: (HEADERS . LIST-OF-LISTS-OF-RAW-VALUES)."
  (let* ((num-rows (grid-data-model-rows model))
         (num-cols (grid-data-model-cols model))
         (all-raw-rows (cl-loop for r from 0 below num-rows
                                collect (cl-loop for c from 0 below num-cols
                                                 collect (grid-cell-raw-input
                                                         (grid-model-get-cell model r c))))))
    (cons (nth 0 all-raw-rows) (cl-subseq all-raw-rows 1)))) ; Return first row as headers, rest as data

(defun grid-model--restructure-formulas (raw-rows axis at delta)
  "Return RAW-ROWS with every formula rewritten for a structural change.
AXIS, AT and DELTA are as for `grid-calc-restructure-formula'."
  (mapcar (lambda (row)
            (mapcar (lambda (value)
                      (grid-calc-restructure-formula
                       value axis at delta grid-calc-current-sheet))
                    row))
          raw-rows))

(defun grid-model-add-row (model &optional row-idx)
  "Adds a new empty row to the model at ROW-IDX (0-based).
If ROW-IDX is nil or out of bounds, adds to the end.
Returns a new model instance."
  (let* ((current-rows (grid-data-model-rows model))
         (current-cols (grid-data-model-cols model))
         (all-raw-values (grid-model-get-all-raw-values model))
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
                               (grid-data-model-merges model))))
      (grid-model-create
       (1+ current-rows) current-cols
       (grid-model--restructure-formulas new-raw-values 'row insert-row 1)
       new-merges
       (grid-model--map-styles
        model (lambda (row col)
                (cons (grid-model--shift-index row insert-row 1) col)))
       (grid-model--shift-rules (grid-data-model-conditionals model)
                                'row insert-row 1)))))

(defun grid-model-delete-row (model row-idx)
  "Deletes the row at ROW-IDX (0-based) from the model.
Returns a new model instance. Errors if ROW-IDX is invalid or only one row remains."
  (let* ((current-rows (grid-data-model-rows model))
         (current-cols (grid-data-model-cols model))
         (all-raw-values (grid-model-get-all-raw-values model))
         (user-defined-headers (car all-raw-values)) ; Get user-defined headers (first row)
         (data-rows (cdr all-raw-values)) ; Actual data rows (excluding user-defined headers)
         (new-data-rows (copy-sequence data-rows)))
    ;; A row index of -1 names the first row of the sheet.
    (unless (and (>= row-idx -1) (< row-idx (length data-rows)))
      (error "Invalid row index: %s" row-idx))
    (when (if (= row-idx -1) (null data-rows) (= (length data-rows) 1))
      (error "Cannot delete the last row."))
    (let ((delete-row (1+ row-idx)))
      (dolist (merge (grid-data-model-merges model))
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
                   (grid-data-model-merges model)))))
      (grid-model-create
       (1- current-rows) current-cols
       (grid-model--restructure-formulas new-raw-values 'row delete-row -1)
       new-merges
       (grid-model--map-styles
        model (lambda (row col)
                (let ((moved (grid-model--shift-index row delete-row -1)))
                  (and moved (cons moved col)))))
       (grid-model--shift-rules (grid-data-model-conditionals model)
                                'row delete-row -1)))))

(defun grid-model-add-column (model &optional col-idx)
  "Adds a new empty column to the model at COL-IDX (0-based).
If COL-IDX is nil or out of bounds, adds to the end.
Returns a new model instance."
  (let* ((current-rows (grid-data-model-rows model))
         (current-cols (grid-data-model-cols model))
         (all-raw-values (grid-model-get-all-raw-values model))
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
                               (grid-data-model-merges model))))
      (grid-model-create
       current-rows (1+ current-cols)
       (grid-model--restructure-formulas new-raw-values 'col insert-col 1)
       new-merges
       (grid-model--map-styles
        model (lambda (row col)
                (cons row (grid-model--shift-index col insert-col 1))))
       (grid-model--shift-rules (grid-data-model-conditionals model)
                                'col insert-col 1)))))

(defun grid-model-delete-column (model col-idx)
  "Deletes the column at COL-IDX (0-based) from the model.
Returns a new model instance. Errors if COL-IDX is invalid or only one column remains."
  (let* ((current-rows (grid-data-model-rows model))
         (current-cols (grid-data-model-cols model))
         (raw-rows (grid-model-get-all-raw-values model)))
    (unless (and (>= col-idx 0) (< col-idx current-cols))
      (error "Invalid column index: %s" col-idx))
    (when (= current-cols 1)
      (error "Cannot delete the last column."))
    (dolist (merge (grid-data-model-merges model))
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
                   (grid-data-model-merges model)))))
    ;; Delete column from user-defined headers as well
    ;; Reconstruct the complete raw values with user-defined headers at the top
    (let ((new-raw-values (cons new-user-defined-headers new-data-rows)))
      (grid-model-create
       current-rows (1- current-cols)
       (grid-model--restructure-formulas new-raw-values 'col col-idx -1)
       new-merges
       (grid-model--map-styles
        model (lambda (row col)
                (let ((moved (grid-model--shift-index col col-idx -1)))
                  (and moved (cons row moved)))))
       (grid-model--shift-rules (grid-data-model-conditionals model)
                                'col col-idx -1))))))

(provide 'grid-data-model)

;;; grid-data-model.el ends here
