;;; grid-table-rst.el --- Export/insert grid-table as reStructuredText format -*- lexical-binding: t -*-

(require 'grid-table)
(require 'grid-table-persistence)

;;;----------------------------------------------------------------------
;;; Helper Functions
;;;----------------------------------------------------------------------

(defun grid-table-rst--draw-separator (widths line-char)
  "Draw an rST-style horizontal separator."
  (let* ((segments (mapcar (lambda (w) (make-string w (aref line-char 0))) widths)))
    (concat "+" (string-join segments "+") "+")))

(defun grid-table-rst--draw-row (row-data widths)
  "Draw an rST-style data row, handling multi-line cells and padding correctly."
  ;; 1. For each cell, wrap its text to fit (column-width - 2) to leave space for padding.
  (let* ((wrapped-cells (cl-loop for cell in row-data
                                 for width in widths
                                 collect (let ((content-width (- width 2)))
                                           (if (<= content-width 0)
                                               (list "") ; Handle very narrow columns.
                                             (grid-table--wrap-text (if cell (format "%s" cell) "") content-width)))))
         ;; 2. Find the maximum height of this entire data row.
         (max-height (apply #'max 1 (mapcar #'length wrapped-cells)))
         (output-lines '()))
    ;; 3. Build the multi-line row block line by line.
    (cl-loop for i from 0 below max-height
             do (let ((line-segments
                      (cl-loop for wrapped-cell in wrapped-cells
                               for width in widths
                               collect (let* ((text (or (nth i wrapped-cell) ""))
                                              ;; FINAL FIX: Correct padding calculation.
                                              ;; width - 2 (for spaces) - text width
                                              (padding (max 0 (- width 2 (string-width text)))))
                                         (concat " " text (make-string padding ?\s) " ")))))
                  (push (concat "|" (string-join line-segments "|") "|") output-lines)))
    ;; 4. Return the joined block, with lines in the correct order.
    (string-join (nreverse output-lines) "\n")))


(defun grid-table-rst--generate-table-string (data-source)
  "Generate and return an rST-formatted table string from a DATA-SOURCE."
  (let* ((get-col-count-fn (gethash :get-column-count data-source))
         (get-row-count-fn (gethash :get-row-count data-source))
         (get-computed-fn (gethash :get-computed-value-at data-source))
         (get-header-fn (gethash :get-header-value data-source))
         (num-rows (funcall get-row-count-fn data-source))
         (num-cols (funcall get-col-count-fn data-source))
         (headers (cl-loop for c from 0 below num-cols
                           collect (funcall get-header-fn data-source c)))
         (all-rows (cl-loop for r from 0 below num-rows
                            collect (cl-loop for c from 0 below num-cols
                                             collect (funcall get-computed-fn data-source r c))))
         (widths (grid-table--calculate-column-widths headers all-rows)))
    (with-temp-buffer
      (insert (grid-table-rst--draw-separator widths "-") "\n")
      (insert (grid-table-rst--draw-row headers widths) "\n")
      (insert (grid-table-rst--draw-separator widths "-") "\n")
      (dolist (row (cdr all-rows))
        (insert (grid-table-rst--draw-row row widths) "\n")
        (insert (grid-table-rst--draw-separator widths "-") "\n"))
      (buffer-string))))

;;;----------------------------------------------------------------------
;;; Public API
;;;----------------------------------------------------------------------

;;;###autoload
(defun grid-table-export-as-rst ()
  "Export the current grid table to a reStructuredText-formatted file."
  (interactive)
  (unless (eq major-mode 'grid-table-mode)
    (error "Not in a grid-table buffer"))
  (when-let ((data-source grid-table--data-source))
    (let ((file-path (read-file-name "Export as rST to: " nil nil nil ".rst")))
      (when file-path
        (with-temp-buffer
          (insert (grid-table-rst--generate-table-string data-source))
          (write-file file-path nil))
        (message "Grid exported as rST to %s" file-path)))))

;;;###autoload
(defun grid-table-rst-insert-table-from-file (file)
  "Parse a .grid file and insert its content as an rST table at point."
  (interactive "fFind grid file: ")
  (when (and file (file-exists-p file))
    (let* ((data-source (grid-table-persistence-create-data-source-from-file file))
           (rst-table-string (grid-table-rst--generate-table-string data-source)))
      (insert rst-table-string)
      (message "Inserted rST table from %s" file))))

(provide 'grid-table-rst)

;;; grid-table-rst.el ends here
