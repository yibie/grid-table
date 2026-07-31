;;; grid-table-persistence.el --- Save and load grid-table data -*- lexical-binding: t -*-

(require 'grid-data-source)
(require 'cl-lib)

;;;----------------------------------------------------------------------
;;; Public API
;;;----------------------------------------------------------------------

(defun grid-table-persistence--plist-p (value)
  "Return non-nil when VALUE is a keyword plist."
  (and (listp value)
       (zerop (mod (length value) 2))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun grid-table-persistence--table-data-p (headers rows)
  "Return non-nil when HEADERS and ROWS form a rectangular string grid."
  (and (consp headers)
       (cl-every #'stringp headers)
       (listp rows)
       (cl-every (lambda (row)
                   (and (listp row)
                        (= (length row) (length headers))
                        (cl-every #'stringp row)))
                 rows)))

(defun grid-table-persistence--save-to-file (data-source file-path)
  "Save the DATA-SOURCE to FILE-PATH using Lisp's S-expression format.
This saves the headers and all raw cell values (including formulas)."
  (when-let* ((get-col-count-fn (gethash :get-column-count data-source))
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
           (merges (when-let* ((get-merges-fn (gethash :get-merges data-source)))
                     (funcall get-merges-fn data-source)))
           (rows (cl-subseq all-rows 1))
           (data-to-save (if merges
                             (let* ((validation-model
                                     (or (gethash :model data-source)
                                         (grid-model-create
                                          num-rows num-cols all-rows)))
                                    (validated
                                     (grid-model-validate-merges
                                      validation-model merges))
                                    (has-vertical
                                     (cl-some (lambda (merge)
                                                (< (nth 0 merge) (nth 2 merge)))
                                              validated)))
                               (list 'grid-table-file
                                     :version (if has-vertical 3 2)
                                     :title title
                                     :headers user-defined-headers
                                     :rows rows
                                     :merges (if has-vertical
                                                 validated
                                               (mapcar
                                                (lambda (merge)
                                                  (list (nth 0 merge)
                                                        (nth 1 merge)
                                                        (nth 3 merge)))
                                                validated))))
                           (list :title title :headers user-defined-headers :rows rows)))) ; Save user-defined headers as :headers, and remaining rows as :rows
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
        (let (headers rows title merges)
          (cond
           ;; Case 1: Tagged v2 format with merge metadata
           ((eq (car-safe data) 'grid-table-file)
            (let ((plist (cdr data)))
              (unless (and (grid-table-persistence--plist-p plist)
                           (memq (plist-get plist :version) '(2 3)))
                (error "Unsupported grid file version in %s" file-path))
              (setq headers (plist-get plist :headers)
                    rows (plist-get plist :rows)
                    title (plist-get plist :title)
                    merges (let ((raw-merges (plist-get plist :merges)))
                             (if (and (eq (plist-get plist :version) 2)
                                      (listp raw-merges))
                                 (mapcar (lambda (merge)
                                           (and (listp merge)
                                                (= (length merge) 3)
                                                (list (nth 0 merge)
                                                      (nth 1 merge)
                                                      (nth 0 merge)
                                                      (nth 2 merge))))
                                         raw-merges)
                               raw-merges)))))

           ;; Case 1: Plist format (saved by this tool)
           ((keywordp (car data))
            (unless (grid-table-persistence--plist-p data)
              (error "Invalid grid file format in %s" file-path))
            (when (plist-get data :version)
              (error "Unsupported grid file version in %s" file-path))
            (setq headers (plist-get data :headers)
                  rows (plist-get data :rows)
                  title (plist-get data :title)))

           ;; Case 2: Alist format (legacy/example files)
           ((listp (car data))
            (setq headers (cadr (assoc :headers data))
                  title (cdr (assoc :title data)))
            ;; Handle :rows or :model
            (if-let* ((rows-data (cdr (assoc :rows data))))
                (setq rows rows-data)
              (when-let* ((model-data (cdr (assoc :model data))))
                ;; model-data is a vector: [(rows cols ?) [row1 row2 ...]]
                ;; We want the second element (index 1), which is the vector of rows.
                ;; And we need to convert it to a list of lists.
                (when (and (vectorp model-data) (> (length model-data) 1))
                  (setq rows (append (aref model-data 1) nil)))))) ; convert vector to list

           (t (error "Unknown grid file format in %s" file-path)))

          (if (grid-table-persistence--table-data-p headers rows)
              (let ((source (make-default-data-source
                             headers (append (list headers) rows) title)))
                (when merges
                  (let ((model (gethash :model source)))
                    (if (listp merges)
                        (set-grid-data-model-merges
                         model (grid-model-validate-merges model merges t))
                      (display-warning
                       'grid-table
                       "Discarding malformed merge metadata"))))
                source)
            (error "Invalid grid file format in %s" file-path)))))))

(provide 'grid-table-persistence)

;;; grid-table-persistence.el ends here
