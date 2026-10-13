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

(defun grid-table-persistence--sheet-data (data-source)
  "Return what a file keeps of DATA-SOURCE, as a plist.
Its keys are :title, :headers, :rows, :merges, :styles and
:conditional-formats; raw cell values are kept, formulas included."
  (let* ((title (gethash :title data-source))
         (num-cols (grid-get-column-count data-source))
         (num-rows (grid-get-row-count data-source))
         (all-rows (cl-loop for r from 0 below num-rows
                            collect (cl-loop for c from 0 below num-cols
                                             collect (grid-get-raw-value-at
                                                      data-source r c))))
         (merges (grid-get-merges data-source))
         (model (gethash :model data-source))
         (styles (let (entries)
                   (when-let* ((table (and model
                                           (grid-data-model-styles model))))
                     (maphash (lambda (key style)
                                (when style
                                  (push (cons (car key) (cons (cdr key) style))
                                        entries)))
                              table))
                   (sort entries
                         (lambda (a b)
                           (or (< (car a) (car b))
                               (and (= (car a) (car b))
                                    (< (cadr a) (cadr b))))))))
         (validated
          (when merges
            (grid-model-validate-merges
             (or model (grid-model-create num-rows num-cols all-rows))
             merges))))
    (list :title title
          ;; The header row is the first row of the grid.
          :headers (car all-rows)
          :rows (cdr all-rows)
          :merges validated
          :styles styles
          :conditional-formats (and model
                                    (grid-data-model-conditionals model)))))

(defun grid-table-persistence--file-data (data-source)
  "Return the form a file holds for DATA-SOURCE and the sheets beside it.
Each feature raises the version only when a file uses it, so older
releases keep reading the files they can represent."
  (let* ((workbook (grid-workbook-of data-source))
         (sheets (and workbook (grid-workbook-sheets workbook))))
    (if (cdr sheets)
        (list 'grid-table-file
              :version 5
              :active (cl-position data-source sheets)
              :sheets (mapcar
                       (lambda (sheet)
                         (append (list :name (grid-workbook-sheet-name sheet))
                                 (grid-table-persistence--sheet-data sheet)))
                       sheets))
      (let* ((data (grid-table-persistence--sheet-data data-source))
             (merges (plist-get data :merges))
             (has-vertical
              (cl-some (lambda (merge) (< (nth 0 merge) (nth 2 merge)))
                       merges)))
        (cond
         ((or (plist-get data :styles) (plist-get data :conditional-formats))
          (cons 'grid-table-file (append (list :version 4) data)))
         (merges
          (list 'grid-table-file
                :version (if has-vertical 3 2)
                :title (plist-get data :title)
                :headers (plist-get data :headers)
                :rows (plist-get data :rows)
                :merges (if has-vertical
                            merges
                          (mapcar (lambda (merge)
                                    (list (nth 0 merge) (nth 1 merge)
                                          (nth 3 merge)))
                                  merges))))
         (t (list :title (plist-get data :title)
                  :headers (plist-get data :headers)
                  :rows (plist-get data :rows))))))))

(defun grid-table-persistence--save-to-file (data-source file-path)
  "Save the DATA-SOURCE to FILE-PATH using Lisp's S-expression format.
This saves the headers and all raw cell values (including formulas).
When DATA-SOURCE is one of several sheets, all of them are saved."
  (when (and (gethash :get-column-count data-source)
             (gethash :get-row-count data-source)
             (gethash :get-raw-value-at data-source))
    (let ((data-to-save (grid-table-persistence--file-data data-source)))
      (with-temp-buffer
        (prin1 data-to-save (current-buffer))
        (write-file file-path nil)
        (message "Grid saved to %s" file-path)))))

(defun grid-table-persistence--restore-formats (model styles conditionals)
  "Give MODEL the saved STYLES and CONDITIONALS, skipping malformed ones."
  (let ((rows (grid-data-model-rows model))
        (cols (grid-data-model-cols model)))
    (when (listp styles)
      (dolist (entry styles)
        (when (and (consp entry) (consp (cdr entry))
                   (integerp (car entry)) (integerp (cadr entry))
                   (< -1 (car entry) rows) (< -1 (cadr entry) cols)
                   (grid-table-persistence--plist-p (cddr entry)))
          (grid-model-set-style model (car entry) (cadr entry) (cddr entry)))))
    (when (listp conditionals)
      (grid-model-set-conditionals
       model
       (cl-remove-if-not
        (lambda (rule)
          (and (listp rule) (= (length rule) 7)
               (cl-every #'integerp (cl-subseq rule 0 4))
               (stringp (nth 4 rule))
               (grid-table-persistence--plist-p (nth 6 rule))))
        conditionals)))))

(defun grid-table-persistence--make-source (file-path headers rows title
                                                   merges styles conditionals)
  "Return a data source for the fields a file at FILE-PATH holds for a sheet."
  (unless (grid-table-persistence--table-data-p headers rows)
    (error "Invalid grid file format in %s" file-path))
  (let ((source (make-default-data-source
                 headers (append (list headers) rows) title)))
    (when merges
      (let ((model (gethash :model source)))
        (if (listp merges)
            (set-grid-data-model-merges
             model (grid-model-validate-merges model merges t))
          (display-warning 'grid-table "Discarding malformed merge metadata"))))
    (grid-table-persistence--restore-formats
     (gethash :model source) styles conditionals)
    source))

(defun grid-table-persistence--make-workbook (file-path plist)
  "Return the active sheet of the workbook that PLIST, read from FILE-PATH, holds."
  (let* ((sheets (plist-get plist :sheets))
         (workbook (grid-workbook--create))
         (active (plist-get plist :active)))
    (unless (and (consp sheets)
                 (cl-every #'grid-table-persistence--plist-p sheets))
      (error "Invalid grid file format in %s" file-path))
    (dolist (sheet sheets)
      (grid-workbook-add-sheet
       workbook
       (grid-table-persistence--make-source
        file-path
        (plist-get sheet :headers) (plist-get sheet :rows)
        (plist-get sheet :title) (plist-get sheet :merges)
        (plist-get sheet :styles) (plist-get sheet :conditional-formats))
       ;; A name another sheet has, or none, would lose the sheet.
       (grid-workbook-fresh-name workbook (plist-get sheet :name))))
    ;; Each sheet was calculated before the ones after it existed.
    (grid-workbook-recalculate workbook)
    (nth (if (and (integerp active) (< -1 active (length sheets))) active 0)
         (grid-workbook-sheets workbook))))

(defun grid-table-persistence-create-data-source-from-file (file-path)
  "Load data from FILE-PATH and create a new data source.
Returns a new data source instance, or nil on error.  For a file that
holds several sheets it is the sheet that was shown when the file was
saved; the others are in its workbook."
  (if (not (file-exists-p file-path))
      (error "File not found: %s" file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((data (read (current-buffer))))
        (if (and (eq (car-safe data) 'grid-table-file)
                 (grid-table-persistence--plist-p (cdr data))
                 (eql (plist-get (cdr data) :version) 5))
            (grid-table-persistence--make-workbook file-path (cdr data))
        (let (headers rows title merges styles conditionals)
          (cond
           ;; Case 1: Tagged v2 format with merge metadata
           ((eq (car-safe data) 'grid-table-file)
            (let ((plist (cdr data)))
              (unless (and (grid-table-persistence--plist-p plist)
                           (memq (plist-get plist :version) '(2 3 4)))
                (error "Unsupported grid file version in %s" file-path))
              (setq headers (plist-get plist :headers)
                    rows (plist-get plist :rows)
                    title (plist-get plist :title)
                    styles (plist-get plist :styles)
                    conditionals (plist-get plist :conditional-formats)
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

          (grid-table-persistence--make-source
           file-path headers rows title merges styles conditionals)))))))

(provide 'grid-table-persistence)

;;; grid-table-persistence.el ends here
