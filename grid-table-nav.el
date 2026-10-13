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

;; The ETAF renderer keeps its own cursor; these commands dispatch to it.
(declare-function grid-table--etaf-p "grid-table.el")
(declare-function grid-table-etaf-goto "grid-table-etaf")
(declare-function grid-table-etaf-move "grid-table-etaf")
(declare-function grid-table-etaf-move-tab "grid-table-etaf")
(declare-function grid-table-etaf-row-start "grid-table-etaf")
(declare-function grid-table-etaf-row-end "grid-table-etaf")

;; These variables are defined as buffer-local in grid-table.el
(defvar grid-table--data-source)
(defvar grid-table--highlight-overlay)
(defvar grid-table--col-header-row-idx)

(defvar-local grid-table--nav-row nil
  "Logical row represented by point.")

(defvar-local grid-table--nav-col nil
  "Logical column represented by point.")

;;;----------------------------------------------------------------------
;;; Internal Helper Functions
;;;----------------------------------------------------------------------

(defun grid-table--move-to-cell (display-row col)
  "Move point to the slice at DISPLAY-ROW whose range contains COL."
  (if (grid-table--etaf-p)
      (grid-table-etaf-goto (max 0 display-row) (max 0 (1- col)))
    (grid-table--classic-move-to-cell display-row col)))

(defun grid-table--classic-move-to-cell (display-row col)
  "Move point in classic text to DISPLAY-ROW's slice containing COL."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (let* ((props (text-properties-at (point)))
             (start (or (plist-get props 'grid-display-col-start)
                        (plist-get props 'grid-display-col-idx)))
             (end (or (plist-get props 'grid-display-col-end)
                      (plist-get props 'grid-display-col-end-idx)
                      start))
             (slice-row (or (plist-get props 'grid-display-slice-row)
                            (plist-get props 'grid-display-row-idx))))
        (when (and (plist-get props 'grid-cell)
                   (eq slice-row display-row)
                   start end
                   (<= start col)
                   (<= col end))
          (setq grid-table--nav-row display-row
                grid-table--nav-col col)
          (throw 'found t)))
      (forward-char 1))
    (message "Cell (%s, %s) not found." display-row col)
    nil))

(defun grid-table--highlight-cell (display-row col)
  "Highlight every rendered slice of the cell at DISPLAY-ROW and COL."
  (unless (grid-table--etaf-p)
    (grid-table--classic-highlight-cell display-row col)))

(defun grid-table--classic-highlight-cell (display-row col)
  "Overlay every classic slice of the cell at DISPLAY-ROW and COL."
  (dolist (overlay (if (listp grid-table--highlight-overlay)
                       grid-table--highlight-overlay
                     (and (overlayp grid-table--highlight-overlay)
                          (list grid-table--highlight-overlay))))
    (delete-overlay overlay))
  (setq grid-table--highlight-overlay nil)
  (save-excursion
    (when (grid-table--move-to-cell display-row col)
      (let* ((coords (grid-table-get-cell-at-point))
             (row-start (plist-get coords :row-start))
             (row-end (plist-get coords :row-end))
             (col-start (plist-get coords :col-start))
             (col-end (plist-get coords :col-end))
             (pos (point-min)))
        (while (< pos (point-max))
          (if (not (get-text-property pos 'grid-cell))
              (setq pos (or (next-single-property-change
                             pos 'grid-cell nil (point-max))
                            (point-max)))
            (let* ((end (or (next-single-property-change
                             pos 'grid-cell nil (point-max))
                            (point-max)))
                   (row (or (get-text-property
                             pos 'grid-display-row-start)
                            (get-text-property
                             pos 'grid-display-row-idx)))
                   (last-row (or (get-text-property
                                  pos 'grid-display-row-end)
                                 row))
                   (start (or (get-text-property
                               pos 'grid-display-col-start)
                              (get-text-property
                               pos 'grid-display-col-idx)))
                   (last-col (or (get-text-property
                                  pos 'grid-display-col-end)
                                 (get-text-property
                                  pos 'grid-display-col-end-idx)
                                 start)))
              (when (and (= row row-start) (= last-row row-end)
                         (= start col-start) (= last-col col-end))
                (let ((overlay (make-overlay pos end)))
                  (overlay-put overlay 'face 'grid-table-highlight-face)
                  (overlay-put overlay 'priority -1)
                  (push overlay grid-table--highlight-overlay)))
              (setq pos end))))))))

(defun grid-table--tab-nodes ()
  "Return unique rendered nodes in buffer order."
  (let ((pos (point-min))
        seen
        nodes)
    (while (< pos (point-max))
      (if (not (get-text-property pos 'grid-cell))
          (setq pos (or (next-single-property-change
                         pos 'grid-cell nil (point-max))
                        (point-max)))
        (let* ((end (or (next-single-property-change
                         pos 'grid-cell nil (point-max))
                        (point-max)))
               (row (or (get-text-property pos 'grid-display-anchor-row)
                        (get-text-property pos 'grid-display-row-start)
                        (get-text-property pos 'grid-display-row-idx)))
               (col (or (get-text-property pos 'grid-display-anchor-col)
                        (get-text-property pos 'grid-display-col-start)
                        (get-text-property pos 'grid-display-col-idx)))
               (key (cons row col)))
          (unless (member key seen)
            (push key seen)
            (push key nodes))
          (setq pos end))))
    (nreverse nodes)))

(defun grid-table--move-tab (step)
  "Move STEP nodes through the rendered cell order."
  (when-let* ((coords (grid-table-get-cell-at-point))
              (nodes (grid-table--tab-nodes))
              (current (cons (plist-get coords :anchor-row)
                             (plist-get coords :anchor-col)))
              (index (cl-position current nodes :test #'equal)))
    (let* ((count (length nodes))
           (target (nth (mod (+ index step) count) nodes)))
      (when (grid-table--move-to-cell (car target) (cdr target))
        (grid-table--highlight-cell (car target) (cdr target))))))

;;;----------------------------------------------------------------------
;;; Public Navigation Commands
;;;----------------------------------------------------------------------

(defun grid-table-next-line ()
  "Move to the cell in the next row, same column."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-move 1 0)
    (when grid-table--data-source
      (when-let* ((coords (grid-table-get-cell-at-point))
                  (current-row (plist-get coords :slice-row))
                  (bottom (plist-get coords :row-end))
                  (current-col (plist-get coords :slice-col))
                  (num-rows (grid-get-row-count grid-table--data-source)))
        (let ((target-row
               (cond
                ((= current-row grid-table--col-header-row-idx) 0)
                ((= bottom (1- num-rows)) grid-table--col-header-row-idx)
                (t (1+ bottom)))))
          (when (grid-table--move-to-cell target-row current-col)
            (grid-table--highlight-cell target-row current-col)))))))

(defun grid-table-previous-line ()
  "Move to the cell in the previous row, same column."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-move -1 0)
    (when grid-table--data-source
      (when-let* ((coords (grid-table-get-cell-at-point))
                  (current-row (plist-get coords :slice-row))
                  (top (plist-get coords :row-start))
                  (current-col (plist-get coords :slice-col))
                  (num-rows (grid-get-row-count grid-table--data-source)))
        (let ((target-row
               (cond
                ((= current-row grid-table--col-header-row-idx) (1- num-rows))
                ((= top 0) grid-table--col-header-row-idx)
                (t (1- top)))))
          (when (grid-table--move-to-cell target-row current-col)
            (grid-table--highlight-cell target-row current-col)))))))

(defun grid-table-next-cell ()
  "Move to the next rendered cell node."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-move-tab 1)
    (when grid-table--data-source
      (grid-table--move-tab 1))))

(defun grid-table-previous-cell ()
  "Move to the previous rendered cell node."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-move-tab -1)
    (when grid-table--data-source
      (grid-table--move-tab -1))))

(defun grid-table-cell-right ()
  "Move to the next visible cell segment in the current row."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-move 0 1)
    (when-let* ((coords (grid-table-get-cell-at-point))
                (row (plist-get coords :slice-row))
                (end (plist-get coords :col-end))
                (num-cols (grid-get-column-count grid-table--data-source))
                (target (and (< end num-cols) (1+ end))))
      (when (grid-table--move-to-cell row target)
        (grid-table--highlight-cell row target)))))

(defun grid-table-cell-left ()
  "Move to the previous visible cell segment in the current row."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-move 0 -1)
    (when-let* ((coords (grid-table-get-cell-at-point))
                (row (plist-get coords :slice-row))
                (start (plist-get coords :col-start))
                (target (and (> start 0) (1- start))))
      (when (grid-table--move-to-cell row target)
        (grid-table--highlight-cell row target)))))

(defun grid-table-scroll-left ()
  "Scroll the table view left by a few columns."
  (interactive)
  (scroll-left 5))

(defun grid-table-scroll-right ()
  "Scroll the table view right by a few columns."
  (interactive)
  (scroll-right 5))

(defun grid-table-move-beginning-of-line ()
  "Move point to the beginning of the visual line (accounting for horizontal scroll)."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-row-start)
    (move-beginning-of-line nil)))

(defun grid-table-move-end-of-line ()
  "Move point to the end of the visual line (accounting for horizontal scroll)."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-row-end)
    (move-end-of-line nil)))

(provide 'grid-table-nav)

;;; grid-table-nav.el ends here
