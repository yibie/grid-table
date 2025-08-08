;;; grid-data-model.el --- Data model and calculation engine for grid-table -*- lexical-binding: t -*-

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
(defun make-grid-data-model (rows cols cells)
  "Create a new grid data model."
  (vector rows cols cells))

(defun grid-data-model-rows (model) (aref model 0))
(defun grid-data-model-cols (model) (aref model 1))
(defun grid-data-model-cells (model) (aref model 2))

;;;----------------------------------------------------------------------
;;; Model Creation and Access
;;;----------------------------------------------------------------------

(defun grid-model-create (rows cols &optional initial-data)
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
    model))

(defun grid-model-get-cell (model row col)
  "Get the cell struct at ROW and COL from the MODEL."
  (when (and (>= row 0) (< row (grid-data-model-rows model))
             (>= col 0) (< col (grid-data-model-cols model)))
    (aref (grid-data-model-cells model) (+ (* row (grid-data-model-cols model)) col))))

(defun grid-model-get-computed-value (model row col)
  "Get the computed value of the cell at ROW and COL.
If the cell is a formula, it will be evaluated on demand."
  (when-let ((cell (grid-model-get-cell model row col)))
    (grid-cell-computed-value cell)))

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
  (when-let ((cell (grid-model-get-cell model row col)))
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
         (all-raw-values (grid-model-get-all-raw-values model))
         (user-defined-headers (car all-raw-values)) ; Get user-defined headers (first row)
         (data-rows (cdr all-raw-values)) ; Actual data rows (excluding user-defined headers)
         (sorted-data-rows (sort (copy-sequence data-rows)
                                 (lambda (row1 row2)
                                   (let* ((val1 (nth col-idx row1))
                                          (val2 (nth col-idx row2)))
                                     (pcase sort-order
                                       ('ascending (string< val1 val2))
                                       ('descending (string> val1 val2))
                                       (_ (error "Invalid sort order: %s" sort-order))))))))
    ;; Reconstruct the new raw values list with user-defined headers at the top
    (let ((new-raw-values (cons user-defined-headers sorted-data-rows)))
      ;; Create a new model with the sorted data
      (grid-model-create num-rows num-cols new-raw-values))))

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
    (let ((new-raw-values (cons user-defined-headers new-data-rows)))
      (grid-model-create (1+ current-rows) current-cols new-raw-values))))

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
    (setf new-data-rows (append (cl-subseq new-data-rows 0 row-idx)
                                (cl-subseq new-data-rows (1+ row-idx))))
    ;; Reconstruct the complete raw values with user-defined headers at the top
    (let ((new-raw-values (cons user-defined-headers new-data-rows)))
      (grid-model-create (1- current-rows) current-cols new-raw-values))))

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
    (let ((new-raw-values (cons new-user-defined-headers new-data-rows)))
      (grid-model-create current-rows (1+ current-cols) new-raw-values))))

(defun grid-model-delete-column (model col-idx)
  "Deletes the column at COL-IDX (0-based) from the model.
Returns a new model instance. Errors if COL-IDX is invalid or only one column remains."
  (let* ((current-rows (grid-data-model-rows model))
         (current-cols (grid-data-model-cols model))
         (all-raw-values (grid-model-get-all-raw-values model))
         (user-defined-headers (car all-raw-values)) ; Get user-defined headers (first row)
         (data-rows (cdr all-raw-values)) ; Actual data rows (excluding user-defined headers)
         (new-user-defined-headers (copy-sequence user-defined-headers))
         (new-data-rows (cl-loop for row in data-rows
                                  collect (let ((new-row (copy-sequence row)))
                                            (unless (and (>= col-idx 0) (< col-idx current-cols))
                                              (error "Invalid column index: %s" col-idx))
                                            (when (= current-cols 1)
                                              (error "Cannot delete the last column."))
                                            (append (cl-subseq new-row 0 col-idx)
                                                    (cl-subseq new-row (1+ col-idx)))))))
    ;; Delete column from user-defined headers as well
    (unless (and (>= col-idx 0) (< col-idx (length new-user-defined-headers)))
      (error "Invalid column index: %s" col-idx))
    (when (= (length new-user-defined-headers) 1)
      (error "Cannot delete the last column."))
    (setf new-user-defined-headers (append (cl-subseq new-user-defined-headers 0 col-idx)
                                           (cl-subseq new-user-defined-headers (1+ col-idx))))
    ;; Reconstruct the complete raw values with user-defined headers at the top
    (let ((new-raw-values (cons new-user-defined-headers new-data-rows)))
      (grid-model-create current-rows (1- current-cols) new-raw-values))))

(provide 'grid-data-model)

;;; grid-data-model.el ends here
