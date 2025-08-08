;; -*- lexical-binding: t; -*-

;;; grid-table-nav.el --- Navigation functions for grid-table 

(require 'cl-lib)

;;;----------------------------------------------------------------------
;;; Variable and Function Declarations
;;;----------------------------------------------------------------------

;; These functions are defined in grid-table.el
(declare-function grid-table-get-cell-at-point "grid-table.el")
(declare-function grid-get-row-count "grid-table.el")
(declare-function grid-get-column-count "grid-table.el")

;; These variables are defined as buffer-local in grid-table.el
(defvar grid-table--data-source)
(defvar grid-table--highlight-overlay)
(defvar grid-table--col-header-row-idx)

;;;----------------------------------------------------------------------
;;; Internal Helper Functions
;;;----------------------------------------------------------------------

(defun grid-table--move-to-cell (display-row col)
  "Move point to the beginning of the cell at logical DISPLAY-ROW and COL."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (let ((props (text-properties-at (point))))
        (when (and (plist-get props 'grid-cell)
                   (eq (plist-get props 'grid-display-row-idx) display-row)
                   (eq (plist-get props 'grid-display-col-idx) col))
          (throw 'found t)))
      (forward-char 1))
    ;; If not found, maybe do nothing or move to a default position
    (message "Cell (%s, %s) not found." display-row col)
    nil))

(defun grid-table--get-cell-bounds (display-row col)
  "Get the buffer start and end positions of the cell at logical DISPLAY-ROW and COL."
  (save-excursion
    (when (grid-table--move-to-cell display-row col)
      (cons (point) (next-single-property-change (point) 'grid-cell nil (point-max))))))

(defun grid-table--highlight-cell (display-row col)
  "Highlight the cell at the given DISPLAY-ROW and COL using an overlay."
  (unless grid-table--highlight-overlay
    (setq grid-table--highlight-overlay (make-overlay (point-min) (point-min)))
    (overlay-put grid-table--highlight-overlay 'face 'grid-table-highlight-face)
    (overlay-put grid-table--highlight-overlay 'priority -1))

  (if-let* ((bounds (grid-table--get-cell-bounds display-row col)))
      (move-overlay grid-table--highlight-overlay (car bounds) (cdr bounds))
    ;; If cell not found, hide the overlay
    (move-overlay grid-table--highlight-overlay (point-min) (point-min))))

;;;----------------------------------------------------------------------
;;; Public Navigation Commands
;;;----------------------------------------------------------------------

(defun grid-table-next-line ()
  "Move to the cell in the next row, same column."
  (interactive)
  (when grid-table--data-source
    (when-let* ((coords (grid-table-get-cell-at-point))
                (current-row (plist-get coords :display-row-idx))
                (current-col (plist-get coords :display-col-idx))
                (num-rows (grid-get-row-count grid-table--data-source)))
      (let ((target-row
             (cond
              ;; from ABC header to first data row
              ((= current-row grid-table--col-header-row-idx) 0)
              ;; from last data row to ABC header
              ((= current-row (1- num-rows)) grid-table--col-header-row-idx)
              ;; normal downward movement
              (t (1+ current-row)))))
        (grid-table--move-to-cell target-row current-col)
        (grid-table--highlight-cell target-row current-col)))))

(defun grid-table-previous-line ()
  "Move to the cell in the previous row, same column."
  (interactive)
  (when grid-table--data-source
    (when-let* ((coords (grid-table-get-cell-at-point))
                (current-row (plist-get coords :display-row-idx))
                (current-col (plist-get coords :display-col-idx))
                (num-rows (grid-get-row-count grid-table--data-source)))
      (let ((target-row
             (cond
              ;; from ABC header to last data row
              ((= current-row grid-table--col-header-row-idx) (1- num-rows))
              ;; from first data row to ABC header
              ((= current-row 0) grid-table--col-header-row-idx)
              ;; normal upward movement
              (t (1- current-row)))))
        (grid-table--move-to-cell target-row current-col)
        (grid-table--highlight-cell target-row current-col)))))

(defun grid-table-next-cell ()
  "Move to the next cell in the table, wrapping to the next row."
  (interactive)
  (when grid-table--data-source
    (when-let* ((coords (grid-table-get-cell-at-point))
                (current-row (plist-get coords :display-row-idx))
                (current-col (plist-get coords :display-col-idx))
                (num-rows (grid-get-row-count grid-table--data-source))
                (num-cols (grid-get-column-count grid-table--data-source)))
      (let* ((total-data-cols (1+ num-cols)) ; data cols + row header
             (target-col (1+ current-col))
             (target-row current-row))
        ;; Check for wrapping to the next row
        (when (>= target-col total-data-cols)
          (setq target-col 0) ; Wrap to the first column (row header)
          (setq target-row (cond
                            ((= current-row grid-table--col-header-row-idx) 0)
                            ((= current-row (1- num-rows)) grid-table--col-header-row-idx)
                            (t (1+ current-row)))))
        (grid-table--move-to-cell target-row target-col)
        (grid-table--highlight-cell target-row target-col)))))

(defun grid-table-previous-cell ()
  "Move to the previous cell in the table, wrapping to the previous row."
  (interactive)
  (when grid-table--data-source
    (when-let* ((coords (grid-table-get-cell-at-point))
                (current-row (plist-get coords :display-row-idx))
                (current-col (plist-get coords :display-col-idx))
                (num-rows (grid-get-row-count grid-table--data-source))
                (num-cols (grid-get-column-count grid-table--data-source)))
      (let* ((total-data-cols (1+ num-cols))
             (target-col (1- current-col))
             (target-row current-row))
        ;; Check for wrapping to the previous row
        (when (< target-col 0)
          (setq target-col (1- total-data-cols)) ; Wrap to the last column
          (setq target-row (cond
                            ((= current-row grid-table--col-header-row-idx) (1- num-rows))
                            ((= current-row 0) grid-table--col-header-row-idx)
                            (t (1- current-row)))))
        (grid-table--move-to-cell target-row target-col)
        (grid-table--highlight-cell target-row target-col)))))

(provide 'grid-table-nav)

;;; grid-table-nav.el ends here
