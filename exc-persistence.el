;;; exc-persistence.el --- Save and load exc data -*- lexical-binding: t -*-

(require 'exc-data-source)
(require 'cl-lib)
(require 'exc-chart-svg)

;; The getters and setters of a data source are in exc.el, which loads
;; this file: requiring it here would be a circular load.
(declare-function exc-get-column-count "exc")
(declare-function exc-get-row-count "exc")
(declare-function exc-get-raw-value-at "exc")
(declare-function exc-get-merges "exc")
(declare-function exc-get-column-widths "exc")
(declare-function exc-frozen "exc")
(declare-function exc-hidden "exc")
(declare-function exc-filters "exc")
(declare-function exc-validations "exc")
(declare-function exc-charts "exc")
(declare-function exc-set-column-width "exc")
(declare-function exc-set-frozen "exc")
(declare-function exc-set-hidden "exc")
(declare-function exc-set-filters "exc")
(declare-function exc-set-validations "exc")
(declare-function exc-set-charts "exc")

;;;----------------------------------------------------------------------
;;; The tag a saved file starts with
;;;----------------------------------------------------------------------

(defconst exc-persistence-file-tag 'exc-file
  "Symbol a saved grid starts with.")

(defconst exc-persistence-old-file-tag 'grid-table-file
  "Symbol a grid saved before the rename starts with; still read.")

(defun exc-persistence--tagged-p (data)
  "Return non-nil when DATA is a form a saved grid could start with."
  (memq (car-safe data) (list exc-persistence-file-tag
                              exc-persistence-old-file-tag)))

;;;----------------------------------------------------------------------
;;; Public API
;;;----------------------------------------------------------------------

(defun exc-persistence--plist-p (value)
  "Return non-nil when VALUE is a keyword plist."
  (and (listp value)
       (zerop (mod (length value) 2))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun exc-persistence--table-data-p (headers rows)
  "Return non-nil when HEADERS and ROWS form a rectangular string grid."
  (and (consp headers)
       (cl-every #'stringp headers)
       (listp rows)
       (cl-every (lambda (row)
                   (and (listp row)
                        (= (length row) (length headers))
                        (cl-every #'stringp row)))
                 rows)))

(defun exc-persistence--sheet-data (data-source)
  "Return what a file keeps of DATA-SOURCE, as a plist.
Its keys are :title, :headers, :rows, :merges, :styles and
:conditional-formats; raw cell values are kept, formulas included."
  (let* ((title (gethash :title data-source))
         (num-cols (exc-get-column-count data-source))
         (num-rows (exc-get-row-count data-source))
         (all-rows (cl-loop for r from 0 below num-rows
                            collect (cl-loop for c from 0 below num-cols
                                             collect (exc-get-raw-value-at
                                                      data-source r c))))
         (merges (exc-get-merges data-source))
         (model (gethash :model data-source))
         (styles (let (entries)
                   (when-let* ((table (and model
                                           (exc-data-model-styles model))))
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
         (exc-model-first-row-is-header
          (not (gethash :no-header-row data-source)))
         (validated
          (when merges
            (exc-model-validate-merges
             (or model (exc-model-create num-rows num-cols all-rows))
             merges))))
    (append
     (list :title title
           ;; The header row is the first row of the grid.
           :headers (car all-rows)
           :rows (cdr all-rows)
           :merges validated
           :styles styles
           :conditional-formats (and model
                                     (exc-data-model-conditionals model)))
     ;; Keys a file only holds when they are set
     (let (entries)
       (when-let* ((table (exc-get-column-widths data-source)))
         (maphash (lambda (col width) (push (cons col width) entries))
                  table))
       (and entries
            (list :column-widths
                  (sort entries (lambda (a b) (< (car a) (car b)))))))
     (and (gethash :no-header-row data-source)
          (list :no-header-row t))
     (let ((frozen (exc-frozen data-source)))
       (and (or (> (car frozen) 0) (> (cdr frozen) 0))
            (list :frozen (list (car frozen) (cdr frozen)))))
     (let ((hidden (exc-hidden data-source)))
       (and (or (car hidden) (cdr hidden))
            (list :hidden (append (list (car hidden) (cdr hidden))
                                  (and (exc-filters data-source)
                                       (list (exc-filters data-source)))))))
     (and (exc-validations data-source)
          (list :validations (exc-validations data-source)))
     (and (exc-charts data-source)
          (list :charts (exc-charts data-source))))))

(defun exc-persistence--file-data (data-source)
  "Return the form a file holds for DATA-SOURCE and the sheets beside it.
Each feature raises the version only when a file uses it, so older
releases keep reading the files they can represent."
  (let* ((workbook (exc-workbook-of data-source))
         (sheets (and workbook (exc-workbook-sheets workbook))))
    (if (cdr sheets)
        (append
         (list exc-persistence-file-tag
               :version 5
               :active (cl-position data-source sheets)
               :sheets (mapcar
                        (lambda (sheet)
                          (append (list :name (exc-workbook-sheet-name sheet))
                                  (exc-persistence--sheet-data sheet)))
                        sheets))
         (and (exc-workbook-names workbook)
              (list :names (exc-workbook-names workbook))))
      (let* ((data (append
                    (exc-persistence--sheet-data data-source)
                    ;; Names name the sheet, so a lone sheet keeps its name.
                    (and workbook (exc-workbook-names workbook)
                         (list :names (exc-workbook-names workbook)
                               :sheet-name (exc-workbook-sheet-name
                                            data-source)))))
             (merges (plist-get data :merges))
             (has-vertical
              (cl-some (lambda (merge) (< (nth 0 merge) (nth 2 merge)))
                       merges)))
        (cond
         ((or (plist-get data :styles) (plist-get data :conditional-formats))
          (cons exc-persistence-file-tag (append (list :version 4) data)))
         ;; Column widths are an extra key older releases pass over, so
         ;; they do not raise the version.
         (merges
          (append
           (list exc-persistence-file-tag
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
           (exc-persistence--extras data)))
         (t (append
             (list :title (plist-get data :title)
                   :headers (plist-get data :headers)
                   :rows (plist-get data :rows))
             (exc-persistence--extras data))))))))

(defun exc-persistence--extras (data)
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

(defun exc-persistence--save-to-file (data-source file-path)
  "Save the DATA-SOURCE to FILE-PATH using Lisp's S-expression format.
This saves the headers and all raw cell values (including formulas).
When DATA-SOURCE is one of several sheets, all of them are saved."
  (when (and (gethash :get-column-count data-source)
             (gethash :get-row-count data-source)
             (gethash :get-raw-value-at data-source))
    (let ((data-to-save (exc-persistence--file-data data-source)))
      (with-temp-buffer
        ;; The whole grid, whatever the user's print settings: a print
        ;; length or level would write `...' for the rows and lose them.
        (let ((print-length nil) (print-level nil) (print-circle nil))
          (prin1 data-to-save (current-buffer)))
        ;; Not `write-file', which would visit the file and show the
        ;; text as a grid, a .grid file's mode.
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region nil nil file-path nil 'silent))))))

(defun exc-persistence--restore-formats (model styles conditionals)
  "Give MODEL the saved STYLES and CONDITIONALS, skipping malformed ones."
  (let ((rows (exc-data-model-rows model))
        (cols (exc-data-model-cols model)))
    (when (listp styles)
      (dolist (entry styles)
        (when (and (consp entry) (consp (cdr entry))
                   (integerp (car entry)) (integerp (cadr entry))
                   (< -1 (car entry) rows) (< -1 (cadr entry) cols)
                   (exc-persistence--plist-p (cddr entry)))
          (exc-model-set-style model (car entry) (cadr entry) (cddr entry)))))
    (when (listp conditionals)
      (exc-model-set-conditionals
       model
       (cl-remove-if-not
        (lambda (rule)
          (and (listp rule) (= (length rule) 7)
               (cl-every #'integerp (cl-subseq rule 0 4))
               (stringp (nth 4 rule))
               (exc-persistence--plist-p (nth 6 rule))))
        conditionals)))))

(defun exc-persistence--make-source (file-path headers rows title
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
  (unless (exc-persistence--table-data-p headers rows)
    (error "Invalid grid file format in %s" file-path))
  (let ((source (make-exc-default-data-source
                 headers (append (list headers) rows) title)))
    (when no-header
      (puthash :no-header-row t source))
    (when merges
      (let ((model (gethash :model source))
            (exc-model-first-row-is-header (not no-header)))
        (if (listp merges)
            (set-exc-data-model-merges
             model (exc-model-validate-merges model merges t))
          (display-warning 'exc "Discarding malformed merge metadata"))))
    (exc-persistence--restore-formats
     (gethash :model source) styles conditionals)
    ;; Whether a cell holds text depends on its format, which came after
    ;; the first calculation.
    (when styles
      (exc-calc-evaluate (gethash :model source)))
    (when (listp widths)
      (dolist (entry widths)
        (when (and (consp entry) (integerp (car entry)) (integerp (cdr entry))
                   (< -1 (car entry) (exc-get-column-count source))
                   (< 0 (cdr entry) 1000))
          (exc-set-column-width source (car entry) (cdr entry)))))
    (when (and (listp frozen) (= (length frozen) 2)
               (cl-every #'natnump frozen))
      (exc-set-frozen source (nth 0 frozen) (nth 1 frozen)))
    (when (and (listp hidden) (<= 2 (length hidden) 3)
               (cl-every #'listp hidden))
      (exc-set-hidden source (nth 0 hidden) (nth 1 hidden))
      (exc-set-filters
       source (cl-remove-if-not (lambda (range)
                                  (and (proper-list-p range) (= (length range) 4)
                                       (cl-every #'natnump range)))
                                (nth 2 hidden))))
    (when (listp validations)
      (exc-set-validations
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
      (exc-set-charts
       source (cl-remove-if-not #'exc-persistence--chart-p charts)))
    source))

(defun exc-persistence--chart-p (chart)
  "Return non-nil when CHART, read from a file, is a well-formed chart."
  (and (exc-persistence--plist-p chart)
       (let ((anchor (plist-get chart :anchor))
             (size (plist-get chart :size)))
         (and (consp anchor) (natnump (car anchor)) (natnump (cdr anchor))
              (consp size) (integerp (car size)) (integerp (cdr size))
              (< 0 (car size) 1000) (< 0 (cdr size) 1000)))
       (let ((offset (plist-get chart :offset)))
         (or (null offset)
             (and (consp offset)
                  (numberp (car offset)) (<= 0 (car offset) 1000)
                  (numberp (cdr offset)) (<= 0 (cdr offset) 1000))))
       (assq (plist-get chart :type) exc-chart-svg-types)
       (exc-chart-svg-parse-range (plist-get chart :range))
       (memq (plist-get chart :by) '(nil rows columns))
       (or (null (plist-get chart :title)) (stringp (plist-get chart :title)))))

(defun exc-persistence--make-workbook (file-path plist)
  "Return the active sheet of the workbook that PLIST, read from FILE-PATH, holds."
  (let* ((sheets (plist-get plist :sheets))
         (workbook (exc-workbook--create))
         (active (plist-get plist :active)))
    (unless (and (consp sheets)
                 (cl-every #'exc-persistence--plist-p sheets))
      (error "Invalid grid file format in %s" file-path))
    (dolist (sheet sheets)
      (exc-workbook-add-sheet
       workbook
       (exc-persistence--make-source
        file-path
        (plist-get sheet :headers) (plist-get sheet :rows)
        (plist-get sheet :title) (plist-get sheet :merges)
        (plist-get sheet :styles) (plist-get sheet :conditional-formats)
        (plist-get sheet :column-widths) (plist-get sheet :no-header-row)
        (plist-get sheet :frozen) (plist-get sheet :hidden)
        (plist-get sheet :validations) (plist-get sheet :charts))
       ;; A name another sheet has, or none, would lose the sheet.
       (exc-workbook-fresh-name workbook (plist-get sheet :name))))
    (setf (exc-workbook-names workbook)
          (exc-persistence--names (plist-get plist :names)))
    ;; Each sheet was calculated before the ones after it existed.
    (exc-workbook-recalculate workbook t)
    (nth (if (and (integerp active) (< -1 active (length sheets))) active 0)
         (exc-workbook-sheets workbook))))

(defun exc-persistence-create-data-source-from-file (file-path)
  "Load data from FILE-PATH and create a new data source.
Returns a new data source instance, or nil on error.  For a file that
holds several sheets it is the sheet that was shown when the file was
saved; the others are in its workbook."
  (if (not (file-exists-p file-path))
      (error "File not found: %s" file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((data (read (current-buffer))))
        (if (and (exc-persistence--tagged-p data)
                 (exc-persistence--plist-p (cdr data))
                 (eql (plist-get (cdr data) :version) 5))
            (exc-persistence--make-workbook file-path (cdr data))
        (let (headers rows title merges styles conditionals widths no-header
                      frozen hidden validations charts)
          (cond
           ;; Case 1: Tagged v2 format with merge metadata
           ((exc-persistence--tagged-p data)
            (let ((plist (cdr data)))
              (unless (and (exc-persistence--plist-p plist)
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
            (unless (exc-persistence--plist-p data)
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

          (let* ((source (exc-persistence--make-source
                          file-path headers rows title merges styles conditionals
                          widths no-header frozen hidden validations
                          charts))
                 (plist (cond ((exc-persistence--tagged-p data) (cdr data))
                              ((keywordp (car-safe data)) data)))
                 (names (exc-persistence--names
                         (plist-get plist :names))))
            ;; Names name the sheet, which is called as it was when saved.
            (when names
              (exc-workbook-set-names
               (exc-workbook-create
                (list (cons (exc-workbook-fresh-name
                             nil (plist-get plist :sheet-name))
                            source)))
               names))
            source)))))))

(defun exc-persistence--names (names)
  "Return the entries of NAMES that are good, or nil.
A good entry is (NAME . DEFINITION) with a valid name and a definition
that is a formula."
  (and (listp names)
       (cl-remove-if-not
        (lambda (entry)
          (and (consp entry) (stringp (car entry)) (stringp (cdr entry))
               (string-prefix-p "=" (cdr entry))
               (null (exc-calc-name-problem (car entry)))))
        names)))

(provide 'exc-persistence)

;;; exc-persistence.el ends here
