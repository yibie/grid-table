;;; grid-table-persistence.el --- Save and load grid-table data -*- lexical-binding: t -*-

(require 'grid-data-source)

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
              (let* ((data (read (current-buffer)))
               (headers (plist-get data :headers))
               (rows (plist-get data :rows)) ; These are the actual data rows (excluding the first row which is user-defined headers)
               (title (plist-get data :title)))
        (if (and (listp headers) (listp rows))
            (make-default-data-source headers (append (list headers) rows) title) ; Prepend user-defined headers to rows
          (error "Invalid grid file format in %s" file-path))))))

(provide 'grid-table-persistence)

;;; grid-table-persistence.el ends here
