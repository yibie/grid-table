;;; grid-table-persistence.el --- Save and load grid-table data -*- lexical-binding: t -*-

(require 'grid-data-source)
(require 'cl-lib)

;;;----------------------------------------------------------------------
;;; Public API
;;;----------------------------------------------------------------------

(defun grid-table-persistence--save-to-file (data-source file-path)
  "Save the DATA-SOURCE to FILE-PATH using Lisp's S-expression format.
This saves the headers and all raw cell values (including formulas)."
  (when-let ((get-col-count-fn (gethash :get-column-count data-source))
             (get-row-count-fn (gethash :get-row-count data-source))
             (get-header-fn (gethash :get-header-value data-source))
             (get-raw-fn (gethash :get-raw-value-at data-source)))
    (let* ((title (gethash :title data-source)) ; Get title from data-source
           (num-cols (funcall get-col-count-fn data-source))
           (num-rows (funcall get-row-count-fn data-source))
           (user-defined-headers (cl-loop for c from 0 below num-cols ; Get user-defined headers (now first data row)
                                          collect (funcall get-raw-fn data-source 0 c)))
           (all-rows (cl-loop for r from 0 below num-rows
                              collect (cl-loop for c from 0 below num-cols
                                               collect (funcall get-raw-fn data-source r c))))
           (data-to-save (list :title title :headers user-defined-headers :rows (cl-subseq all-rows 1)))) ; Save user-defined headers as :headers, and remaining rows as :rows
      (with-temp-buffer
        (prin1 data-to-save (current-buffer))
        (write-file file-path nil)
        (message "Grid saved to %s" file-path)))))

(defun grid-table-persistence-create-data-source-from-file (file-path)
  "Load data from FILE-PATH and create a new data source.
Returns a new data source instance, or nil on error."
  (if (not (file-exists-p file-path))
      (error "File not found: %s" file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((data (read (current-buffer))))
        (let (headers rows title)
          (cond
           ;; Case 1: Plist format (saved by this tool)
           ((keywordp (car data))
            (setq headers (plist-get data :headers)
                  rows (plist-get data :rows)
                  title (plist-get data :title)))

           ;; Case 2: Alist format (legacy/example files)
           ((listp (car data))
            (setq headers (cadr (assoc :headers data))
                  title (cdr (assoc :title data)))
            ;; Handle :rows or :model
            (if-let ((rows-data (cdr (assoc :rows data))))
                (setq rows rows-data)
              (when-let ((model-data (cdr (assoc :model data))))
                ;; model-data is a vector: [(rows cols ?) [row1 row2 ...]]
                ;; We want the second element (index 1), which is the vector of rows.
                ;; And we need to convert it to a list of lists.
                (when (and (vectorp model-data) (> (length model-data) 1))
                  (setq rows (append (aref model-data 1) nil)))))) ; convert vector to list

           (t (error "Unknown grid file format in %s" file-path)))

          (if (and (listp headers) (listp rows))
              (make-default-data-source headers (append (list headers) rows) title)
            (error "Invalid grid file format in %s" file-path)))))))

(provide 'grid-table-persistence)

;;; grid-table-persistence.el ends here
