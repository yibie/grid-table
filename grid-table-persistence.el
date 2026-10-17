;;; grid-table-persistence.el --- Save and load grid-table data -*- lexical-binding: t -*-

(require 'grid-data-source)
(require 'cl-lib)
(require 'grid-table-chart-svg)

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
         (grid-model-first-row-is-header
          (not (gethash :no-header-row data-source)))
         (validated
          (when merges
            (grid-model-validate-merges
             (or model (grid-model-create num-rows num-cols all-rows))
             merges))))
    (append
     (list :title title
           ;; The header row is the first row of the grid.
           :headers (car all-rows)
           :rows (cdr all-rows)
           :merges validated
           :styles styles
           :conditional-formats (and model
                                     (grid-data-model-conditionals model)))
     ;; Keys a file only holds when they are set
     (let (entries)
       (when-let* ((table (grid-get-column-widths data-source)))
         (maphash (lambda (col width) (push (cons col width) entries))
                  table))
       (and entries
            (list :column-widths
                  (sort entries (lambda (a b) (< (car a) (car b)))))))
     (and (gethash :no-header-row data-source)
          (list :no-header-row t))
     (let ((frozen (grid-table-frozen data-source)))
       (and (or (> (car frozen) 0) (> (cdr frozen) 0))
            (list :frozen (list (car frozen) (cdr frozen)))))
     (let ((hidden (grid-table-hidden data-source)))
       (and (or (car hidden) (cdr hidden))
            (list :hidden (append (list (car hidden) (cdr hidden))
                                  (and (grid-table-filters data-source)
                                       (list (grid-table-filters data-source)))))))
     (and (grid-table-validations data-source)
          (list :validations (grid-table-validations data-source)))
     (and (grid-table-charts data-source)
          (list :charts (grid-table-charts data-source))))))

(defun grid-table-persistence--file-data (data-source)
  "Return the form a file holds for DATA-SOURCE and the sheets beside it.
Each feature raises the version only when a file uses it, so older
releases keep reading the files they can represent."
  (let* ((workbook (grid-workbook-of data-source))
         (sheets (and workbook (grid-workbook-sheets workbook))))
    (if (cdr sheets)
        (append
         (list 'grid-table-file
               :version 5
               :active (cl-position data-source sheets)
               :sheets (mapcar
                        (lambda (sheet)
                          (append (list :name (grid-workbook-sheet-name sheet))
                                  (grid-table-persistence--sheet-data sheet)))
                        sheets))
         (and (grid-workbook-names workbook)
              (list :names (grid-workbook-names workbook))))
      (let* ((data (append
                    (grid-table-persistence--sheet-data data-source)
                    ;; Names name the sheet, so a lone sheet keeps its name.
                    (and workbook (grid-workbook-names workbook)
                         (list :names (grid-workbook-names workbook)
                               :sheet-name (grid-workbook-sheet-name
                                            data-source)))))
             (merges (plist-get data :merges))
             (has-vertical
              (cl-some (lambda (merge) (< (nth 0 merge) (nth 2 merge)))
                       merges)))
        (cond
         ((or (plist-get data :styles) (plist-get data :conditional-formats))
          (cons 'grid-table-file (append (list :version 4) data)))
         ;; Column widths are an extra key older releases pass over, so
         ;; they do not raise the version.
         (merges
          (append
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
                                   merges)))
           (grid-table-persistence--extras data)))
         (t (append
             (list :title (plist-get data :title)
                   :headers (plist-get data :headers)
                   :rows (plist-get data :rows))
             (grid-table-persistence--extras data))))))))

(defun grid-table-persistence--extras (data)
  "Return the keys of sheet DATA that a file only holds when they are set.
Older releases pass over them, so they do not raise the file version."
  (append (and (plist-get data :column-widths)
               (list :column-widths (plist-get data :column-widths)))
          (and (plist-get data :no-header-row)
               (list :no-header-row t))
          (and (plist-get data :frozen)
               (list :frozen (plist-get data :frozen)))
          (and (plist-get data :hidden)
               (list :hidden (plist-get data :hidden)))
          (and (plist-get data :validations)
               (list :validations (plist-get data :validations)))
          (and (plist-get data :charts)
               (list :charts (plist-get data :charts)))
          (and (plist-get data :names)
               (list :names (plist-get data :names)
                     :sheet-name (plist-get data :sheet-name)))))

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
                                                   merges styles conditionals
                                                   &optional widths no-header
                                                   frozen hidden validations
                                                   charts)
  "Return a data source for the fields a file at FILE-PATH holds for a sheet.
WIDTHS is a list of (COLUMN . WIDTH) for the columns sized by hand.
NO-HEADER says the first row of the sheet is a row like any other.
FROZEN is (ROWS COLS), the leading rows and columns kept in place.
HIDDEN is (ROWS COLS), lists of the rows and columns hidden, and then
the filtered ranges, which say which hidden rows a filter left out.
VALIDATIONS are the sheet's validation rules and CHARTS its floating
charts."
  (unless (grid-table-persistence--table-data-p headers rows)
    (error "Invalid grid file format in %s" file-path))
  (let ((source (make-default-data-source
                 headers (append (list headers) rows) title)))
    (when no-header
      (puthash :no-header-row t source))
    (when merges
      (let ((model (gethash :model source))
            (grid-model-first-row-is-header (not no-header)))
        (if (listp merges)
            (set-grid-data-model-merges
             model (grid-model-validate-merges model merges t))
          (display-warning 'grid-table "Discarding malformed merge metadata"))))
    (grid-table-persistence--restore-formats
     (gethash :model source) styles conditionals)
    ;; Whether a cell holds text depends on its format, which came after
    ;; the first calculation.
    (when styles
      (grid-table-calc-evaluate (gethash :model source)))
    (when (listp widths)
      (dolist (entry widths)
        (when (and (consp entry) (integerp (car entry)) (integerp (cdr entry))
                   (< -1 (car entry) (grid-get-column-count source))
                   (< 0 (cdr entry) 1000))
          (grid-set-column-width source (car entry) (cdr entry)))))
    (when (and (listp frozen) (= (length frozen) 2)
               (cl-every #'natnump frozen))
      (grid-table-set-frozen source (nth 0 frozen) (nth 1 frozen)))
    (when (and (listp hidden) (<= 2 (length hidden) 3)
               (cl-every #'listp hidden))
      (grid-table-set-hidden source (nth 0 hidden) (nth 1 hidden))
      (grid-table-set-filters
       source (cl-remove-if-not (lambda (range)
                                  (and (proper-list-p range) (= (length range) 4)
                                       (cl-every #'natnump range)))
                                (nth 2 hidden))))
    (when (listp validations)
      (grid-table-set-validations
       source
       (cl-remove-if-not
        (lambda (rule)
          (and (listp rule) (= (length rule) 6)
               (cl-every #'natnump (cl-subseq rule 0 4))
               (eq (nth 4 rule) 'list)
               (or (stringp (nth 5 rule))
                   (and (listp (nth 5 rule)) (cl-every #'stringp (nth 5 rule))))))
        validations)))
    (when (listp charts)
      (grid-table-set-charts
       source (cl-remove-if-not #'grid-table-persistence--chart-p charts)))
    source))

(defun grid-table-persistence--chart-p (chart)
  "Return non-nil when CHART, read from a file, is a well-formed chart."
  (and (grid-table-persistence--plist-p chart)
       (let ((anchor (plist-get chart :anchor))
             (size (plist-get chart :size)))
         (and (consp anchor) (natnump (car anchor)) (natnump (cdr anchor))
              (consp size) (integerp (car size)) (integerp (cdr size))
              (< 0 (car size) 1000) (< 0 (cdr size) 1000)))
       (assq (plist-get chart :type) grid-chart-svg-types)
       (grid-chart-svg-parse-range (plist-get chart :range))
       (memq (plist-get chart :by) '(nil rows columns))
       (or (null (plist-get chart :title)) (stringp (plist-get chart :title)))))

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
        (plist-get sheet :styles) (plist-get sheet :conditional-formats)
        (plist-get sheet :column-widths) (plist-get sheet :no-header-row)
        (plist-get sheet :frozen) (plist-get sheet :hidden)
        (plist-get sheet :validations) (plist-get sheet :charts))
       ;; A name another sheet has, or none, would lose the sheet.
       (grid-workbook-fresh-name workbook (plist-get sheet :name))))
    (setf (grid-workbook-names workbook)
          (grid-table-persistence--names (plist-get plist :names)))
    ;; Each sheet was calculated before the ones after it existed.
    (grid-workbook-recalculate workbook t)
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
        (let (headers rows title merges styles conditionals widths no-header
                      frozen hidden validations charts)
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
                    widths (plist-get plist :column-widths)
                    no-header (plist-get plist :no-header-row)
                    frozen (plist-get plist :frozen)
                    hidden (plist-get plist :hidden)
                    validations (plist-get plist :validations)
                    charts (plist-get plist :charts)
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
                  title (plist-get data :title)
                  widths (plist-get data :column-widths)
                  no-header (plist-get data :no-header-row)
                  frozen (plist-get data :frozen)
                  hidden (plist-get data :hidden)
                  validations (plist-get data :validations)
                  charts (plist-get data :charts)))

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

          (let* ((source (grid-table-persistence--make-source
                          file-path headers rows title merges styles conditionals
                          widths no-header frozen hidden validations
                          charts))
                 (plist (cond ((eq (car-safe data) 'grid-table-file) (cdr data))
                              ((keywordp (car-safe data)) data)))
                 (names (grid-table-persistence--names
                         (plist-get plist :names))))
            ;; Names name the sheet, which is called as it was when saved.
            (when names
              (grid-workbook-set-names
               (grid-workbook-create
                (list (cons (grid-workbook-fresh-name
                             nil (plist-get plist :sheet-name))
                            source)))
               names))
            source)))))))

(defun grid-table-persistence--names (names)
  "Return the entries of NAMES that are good, or nil.
A good entry is (NAME . DEFINITION) with a valid name and a definition
that is a formula."
  (and (listp names)
       (cl-remove-if-not
        (lambda (entry)
          (and (consp entry) (stringp (car entry)) (stringp (cdr entry))
               (string-prefix-p "=" (cdr entry))
               (null (grid-calc-name-problem (car entry)))))
        names)))

(provide 'grid-table-persistence)

;;; grid-table-persistence.el ends here
