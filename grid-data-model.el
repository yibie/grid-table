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
(defconst grid-cell--dependencies 3)
(defconst grid-cell--error-msg 4)
(defconst grid-cell--row 5)
(defconst grid-cell--col 6)
(defconst grid-cell--dependents 7) ; New slot for dependency graph
(defconst grid-cell--length 8)

;; Manual constructor
(defun make-grid-cell (row col)
  "Create a new grid cell vector."
  (let ((cell (make-vector grid-cell--length nil)))
    (aset cell grid-cell--row row)
    (aset cell grid-cell--col col)
    (aset cell grid-cell--raw-input "")
    (aset cell grid-cell--dependencies '())
    cell))

;; Manual accessors and setters
(defun grid-cell-raw-input (cell) (aref cell grid-cell--raw-input))
(defun grid-cell-computed-value (cell) (aref cell grid-cell--computed-value))
(defun grid-cell-row (cell) (aref cell grid-cell--row))
(defun grid-cell-col (cell) (aref cell grid-cell--col))
(defun grid-cell-dependencies (cell) (aref cell grid-cell--dependencies))
(defun grid-cell-error-msg (cell) (aref cell grid-cell--error-msg))
(defun grid-cell-dependents (cell) (aref cell grid-cell--dependents))

(defun set-grid-cell-raw-input (cell value) (aset cell grid-cell--raw-input value))
(defun set-grid-cell-computed-value (cell value) (aset cell grid-cell--computed-value value))
(defun set-grid-cell-dependencies (cell value) (aset cell grid-cell--dependencies value))
(defun set-grid-cell-dependents (cell value) (aset cell grid-cell--dependents value))

;;;----------------------------------------------------------------------
;;; Data Model
;;;----------------------------------------------------------------------

;; Manual grid data model implementation
(defun make-grid-data-model (rows cols cells &optional merges)
  "Create a new grid data model."
  (vector rows cols cells merges))

(defun grid-data-model-rows (model) (aref model 0))
(defun grid-data-model-cols (model) (aref model 1))
(defun grid-data-model-cells (model) (aref model 2))
(defun grid-data-model-merges (model)
  (when (> (length model) 3)
    (aref model 3)))
(defun set-grid-data-model-merges (model value)
  (aset model 3 value))

;;;----------------------------------------------------------------------
;;; Model Creation and Access
;;;----------------------------------------------------------------------

(defun grid-model-create (rows cols &optional initial-data merges)
  "Create a new data model of ROWS and COLS.
Optionally populate with INITIAL-DATA (a list of lists of strings)."
  (let* ((model (make-grid-data-model rows cols (make-vector (* rows cols) nil)))
         (cells (grid-data-model-cells model)))
    ;; 1. Initialize all cells and populate with initial data
    (dotimes (r rows)
      (dotimes (c cols)
        (let ((cell (make-grid-cell r c))
              (raw-val (if (and initial-data (< r (length initial-data)) (< c (length (nth r initial-data))))
                         (nth c (nth r initial-data))
                       "")))
          (set-grid-cell-raw-input cell raw-val)
          ;; For non-formula cells, set computed value to raw value
          (unless (string-prefix-p "=" raw-val)
            (set-grid-cell-computed-value cell raw-val))
          (aset cells (+ (* r cols) c) cell))))
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
   ((and (< (nth 0 merge) (nth 2 merge))
         (< (nth 1 merge) (nth 3 merge)))
    "rectangular merges are not supported")
   ((and (= (nth 0 merge) 0)
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
                       new-merges)))

(defun grid-model-unmerge-cell (model row col)
  "Return a new MODEL with the merge covering ROW and COL removed."
  (let ((merge (grid-model-find-merge model row col)))
    (grid-model-create (grid-data-model-rows model)
                       (grid-data-model-cols model)
                       (grid-model-get-all-raw-values model)
                       (cl-remove merge (grid-data-model-merges model)
                                  :test #'equal))))

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

(defun grid-model-sort-by-column (model col-idx sort-order)
  "Sorts the model's data rows based on the values in COL-IDX.
SORT-ORDER can be 'ascending or 'descending.
Returns a new model instance with sorted data.

Note: This function sorts the actual data rows, excluding the first row
which is considered user-defined headers. The first row will remain in place."
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
         (user-defined-headers (car all-raw-values)) ; Get user-defined headers (first row)
         (data-rows (cdr all-raw-values)) ; Actual data rows (excluding user-defined headers)
         (decorated-data-rows (cl-loop for row in data-rows
                                       for old-idx from 0
                                       collect (list old-idx row)))
         (sorted-data-rows (sort decorated-data-rows
                                 (lambda (row1 row2)
                                   (let* ((old1 (nth 0 row1))
                                          (old2 (nth 0 row2))
                                          (val1 (or (nth col-idx (nth 1 row1)) ""))
                                          (val2 (or (nth col-idx (nth 1 row2)) "")))
                                     (pcase sort-order
                                       ('ascending (if (equal val1 val2)
                                                       (< old1 old2)
                                                     (string< val1 val2)))
                                       ('descending (if (equal val1 val2)
                                                        (< old1 old2)
                                                      (string> val1 val2)))
                                       (_ (error "Invalid sort order: %s" sort-order))))))))
    ;; Reconstruct the new raw values list with user-defined headers at the top
    (let* ((new-raw-values (cons user-defined-headers
                                 (mapcar #'cadr sorted-data-rows)))
           (old-to-new (make-vector (length data-rows) nil))
           (new-idx 0)
           (new-merges
            (mapcar (lambda (merge)
                      (if (= (nth 0 merge) 0)
                          merge
                        (list (1+ (aref old-to-new (1- (nth 0 merge))))
                              (nth 1 merge)
                              (1+ (aref old-to-new (1- (nth 2 merge))))
                              (nth 3 merge))))
                    (progn
                      (dolist (row sorted-data-rows)
                        (aset old-to-new (car row) new-idx)
                        (setq new-idx (1+ new-idx)))
                      (grid-data-model-merges model)))))
      ;; Create a new model with the sorted data
      (grid-model-create num-rows num-cols new-raw-values new-merges))))

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
    (let* ((new-raw-values (cons user-defined-headers new-data-rows))
           (insert-row (if (and row-idx (>= row-idx 0) (< row-idx (length data-rows)))
                           (1+ row-idx)
                         current-rows))
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
      (grid-model-create (1+ current-rows) current-cols new-raw-values new-merges))))

(defun grid-model-delete-row (model row-idx)
  "Deletes the row at ROW-IDX (0-based) from the model.
Returns a new model instance. Errors if ROW-IDX is invalid or only one row remains."
  (let* ((current-rows (grid-data-model-rows model))
         (current-cols (grid-data-model-cols model))
         (all-raw-values (grid-model-get-all-raw-values model))
         (user-defined-headers (car all-raw-values)) ; Get user-defined headers (first row)
         (data-rows (cdr all-raw-values)) ; Actual data rows (excluding user-defined headers)
         (new-data-rows (copy-sequence data-rows)))
    (unless (and (>= row-idx 0) (< row-idx (length data-rows)))
      (error "Invalid row index: %s" row-idx))
    (when (= (length data-rows) 1)
      (error "Cannot delete the last row."))
    (let ((delete-row (1+ row-idx)))
      (dolist (merge (grid-data-model-merges model))
        (when (and (= delete-row (nth 0 merge))
                   (< (nth 0 merge) (nth 2 merge)))
          (let ((raw-rows (cons user-defined-headers new-data-rows)))
            (setf (nth (nth 1 merge) (nth (1+ delete-row) raw-rows))
                  (nth (nth 1 merge) (nth delete-row raw-rows)))))))
    (setf new-data-rows (append (cl-subseq new-data-rows 0 row-idx)
                                (cl-subseq new-data-rows (1+ row-idx))))
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
      (grid-model-create (1- current-rows) current-cols new-raw-values new-merges))))

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
      (grid-model-create current-rows (1+ current-cols) new-raw-values new-merges))))

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
      (grid-model-create current-rows (1- current-cols) new-raw-values new-merges)))))

(provide 'grid-data-model)

;;; grid-data-model.el ends here
