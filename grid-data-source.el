;;; grid-data-source.el --- Abstract data source layer for grid-table -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'grid-data-model)

;;;----------------------------------------------------------------------
;;; Default Data Source Implementation (using a function bundle)
;;;----------------------------------------------------------------------

(defun make-default-data-source (headers initial-data &optional title)
  "Create a default data source instance.
This data source is a hash-table containing functions for the grid-table to call.
HEADERS is a list of strings.
INITIAL-DATA is a list of lists of strings. The HEADERS will be prepended to INITIAL-DATA. The HEADERS are now the first data row.
TITLE is an optional string for the grid title."
  (let* ((rows (length initial-data))
         (cols (length headers))
         (model (grid-model-create rows cols initial-data))
         (source (make-hash-table)))
    (puthash :get-column-count
             (lambda (s) (grid-data-model-cols (gethash :model s)))
             source)
    (puthash :get-row-count
             (lambda (s) (grid-data-model-rows (gethash :model s)))
             source)
    (puthash :get-header-value
             (lambda (s col) (nth col (gethash :headers s)))
             source)
    (puthash :get-raw-value-at
             (lambda (s row col)
               (grid-cell-raw-input (grid-model-get-cell (gethash :model s) row col)))
             source)
    (puthash :get-computed-value-at
             (lambda (s row col)
               (grid-model-get-computed-value (gethash :model s) row col))
             source)
    (puthash :set-raw-value-at
             (lambda (s row col value)
               (grid-model-set-raw-value (gethash :model s) row col value))
             source)
    (puthash :sort-by-column
             (lambda (s col-idx sort-order)
                (let* ((old-model (gethash :model s))
                       (new-model (grid-model-sort-by-column old-model col-idx sort-order)))
                  (puthash :model new-model s)
                  ;; headers stored in :headers remain the same (first data row),
                  ;; grid-model-sort-by-column preserves header row at index 0.
                  s))
             source)
    (puthash :set-header-value-at
             (lambda (s col value)
               (let ((headers (gethash :headers s)))
                 (setf (nth col headers) value)
                 (puthash :headers headers s)))
             source)
    (puthash :add-row
             (lambda (s &optional row-idx)
               (let* ((old-model (gethash :model s))
                      (new-model (grid-model-add-row old-model row-idx)))
                 ;; Update the model in the current source instance
                 (puthash :model new-model s)
                 ;; Return the updated source for redraw
                 s))
             source)
    (puthash :delete-row
             (lambda (s row-idx)
               (let* ((old-model (gethash :model s))
                      (new-model (grid-model-delete-row old-model row-idx)))
                 (puthash :model new-model s)
                 s))
             source)
    (puthash :add-column
             (lambda (s &optional col-idx)
               (let* ((old-model (gethash :model s))
                      (old-headers (gethash :headers s))
                      (new-model (grid-model-add-column old-model col-idx))
                      (new-header (make-string 1 ?\s))) ;; Default new header
                 (if (and col-idx (>= col-idx 0) (<= col-idx (length old-headers)))
                     (setf old-headers (append (cl-subseq old-headers 0 col-idx)
                                               (list new-header)
                                               (cl-subseq old-headers col-idx)))
                   (setf old-headers (append old-headers (list new-header))))
                 (puthash :model new-model s)
                 (puthash :headers old-headers s)
                 s))
             source)
    (puthash :delete-column
             (lambda (s col-idx)
               (let* ((old-model (gethash :model s))
                      (old-headers (gethash :headers s))
                      (new-model (grid-model-delete-column old-model col-idx)))
                 (unless (and (>= col-idx 0) (< col-idx (length old-headers)))
                   (error "Invalid column index: %s" col-idx))
                 (when (= (length old-headers) 1)
                   (error "Cannot delete the last column."))
                 (setf old-headers (append (cl-subseq old-headers 0 col-idx)
                                           (cl-subseq old-headers (1+ col-idx))))
                 (puthash :model new-model s)
                 (puthash :headers old-headers s)
                 s))
             source)
    ;; Store the actual data inside the hash-table as well
    (puthash :model model source)
    (puthash :headers headers source)
    (puthash :title title source)
    source))

(provide 'grid-data-source)

;;; grid-data-source.el ends here
