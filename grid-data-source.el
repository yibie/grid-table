;;; grid-data-source.el --- Abstract data source layer for grid-table -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'grid-data-model)

;;;----------------------------------------------------------------------
;;; Default Data Source Implementation (using a function bundle)
;;;----------------------------------------------------------------------

(defun make-default-data-source (headers initial-data &optional title merges)
  "Create a default data source instance.
This data source is a hash-table containing functions for the grid-table to call.
HEADERS is a list of strings.
INITIAL-DATA is a list of lists of strings. The HEADERS will be prepended to INITIAL-DATA. The HEADERS are now the first data row.
TITLE is an optional string for the grid title."
  (let* ((rows (length initial-data))
         (cols (length headers))
         (model (grid-model-create rows cols initial-data merges))
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
    (puthash :get-merges
             (lambda (s)
               (grid-model-get-merges (gethash :model s)))
             source)
    (puthash :get-style-at
             (lambda (s row col)
               (grid-model-get-style (gethash :model s) row col))
             source)
    (puthash :set-style-at
             (lambda (s row col style)
               (grid-model-set-style (gethash :model s) row col style))
             source)
    (puthash :get-column-widths
             (lambda (s)
               (grid-data-model-widths (gethash :model s)))
             source)
    (puthash :set-column-width
             (lambda (s col width)
               (grid-model-set-column-width (gethash :model s) col width))
             source)
    (puthash :get-frozen
             (lambda (s)
               (grid-data-model-frozen (gethash :model s)))
             source)
    (puthash :set-frozen
             (lambda (s rows cols)
               (grid-model-set-frozen (gethash :model s) rows cols))
             source)
    (puthash :get-hidden
             (lambda (s)
               (grid-data-model-hidden (gethash :model s)))
             source)
    (puthash :set-hidden
             (lambda (s rows cols)
               (let* ((model (gethash :model s))
                      (old (grid-model-filtered-rows model))
                      (hidden (car (grid-data-model-hidden model))))
                 (grid-model-set-hidden model rows cols)
                 (unless (and (equal old (grid-model-filtered-rows model))
                              (equal hidden (car (grid-data-model-hidden model))))
                   (grid-model-subtotals-changed model))))
             source)
    (puthash :get-filters
             (lambda (s) (grid-data-model-filters (gethash :model s)))
             source)
    (puthash :set-filters
             (lambda (s ranges)
               (let* ((model (gethash :model s))
                      (old (grid-model-filtered-rows model)))
                 (grid-model-set-filters model ranges)
                 (unless (equal old (grid-model-filtered-rows model))
                   (grid-model-subtotals-changed model))))
             source)
    (puthash :get-charts
             (lambda (s) (grid-data-model-charts (gethash :model s)))
             source)
    (puthash :set-charts
             (lambda (s charts) (grid-model-set-charts (gethash :model s) charts))
             source)
    (puthash :get-validations
             (lambda (s)
               (grid-data-model-validations (gethash :model s)))
             source)
    (puthash :set-validations
             (lambda (s rules)
               (grid-model-set-validations (gethash :model s) rules))
             source)
    (puthash :grow
             (lambda (s rows cols)
               (puthash :model (grid-model-grow (gethash :model s) rows cols) s)
               (when (> cols 0)
                 (puthash :headers
                          (append (gethash :headers s)
                                  (make-list cols (make-string 1 ?\s)))
                          s))
               s)
             source)
    (puthash :get-conditional-formats
             (lambda (s)
               (grid-model-get-conditionals (gethash :model s)))
             source)
    (puthash :set-conditional-formats
             (lambda (s rules)
               (grid-model-set-conditionals (gethash :model s) rules))
             source)
    (puthash :merge-cells
             (lambda (s row start-col end-col &optional right-col)
               (puthash :model
                        (grid-model-merge-cells (gethash :model s)
                                                row start-col end-col right-col)
                        s)
               s)
             source)
    (puthash :unmerge-cell
             (lambda (s row col)
               (puthash :model
                        (grid-model-unmerge-cell (gethash :model s) row col)
                        s)
               s)
             source)
    (puthash :sort-by-column
             (lambda (s col-idx sort-order)
                (let* ((old-model (gethash :model s))
                       (new-model (grid-model-sort-by-column
                                   old-model col-idx sort-order
                                   (gethash :no-header-row s))))
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
    ;; Several rows at once, counted from the first row of the sheet.
    (puthash :delete-rows
             (lambda (s rows)
               (puthash :model (grid-model-delete-rows (gethash :model s) rows)
                        s)
               s)
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
                      (old-headers (copy-sequence (gethash :headers s)))
                      (header-merge
                       (grid-model-find-merge old-model 0 col-idx))
                      (new-model (grid-model-delete-column old-model col-idx)))
                 (unless (and (>= col-idx 0) (< col-idx (length old-headers)))
                   (error "Invalid column index: %s" col-idx))
                 (when (= (length old-headers) 1)
                   (error "Cannot delete the last column."))
                 (when (and header-merge
                            (= col-idx (nth 1 header-merge)))
                   (setf (nth (1+ col-idx) old-headers)
                         (nth col-idx old-headers)))
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
    (dolist (key grid-workbook--changing-keys)
      (grid-workbook--wrap source key))
    source))

;;;----------------------------------------------------------------------
;;; Workbooks
;;;----------------------------------------------------------------------

;; A workbook keeps several default data sources together as sheets, so
;; that a formula in one can read another by name.  Each sheet knows its
;; workbook (:workbook) and its name (:sheet-name).  A data source with
;; no workbook stands alone, as before.

(cl-defstruct (grid-workbook (:constructor grid-workbook--create))
  "Sheets kept together: SHEETS is the list of their data sources, in order.
NAMES is an alist of the defined names, from each to its definition as a
formula, such as =Sheet1!$B$2:$B$9.  LOCAL-NAMES holds the names that
belong to one sheet, as a workbook file may have them: an alist from
\(SHEET . NAME) to definition.  A formula on that sheet, or one naming
the sheet as SHEET!NAME, finds such a name before one of the workbook.
TABLES holds the tables a workbook file has, for references such as
Table1[Qty]: each is (NAME SHEET TOP LEFT BOTTOM RIGHT HEADERS TOTALS
COLUMNS), as `grid-calc-tables' describes.  DATE-SYSTEM is 1904 when
its dates count from 1904, as `grid-format-date-system' says.  FILE is
the file it was read from, or nil, for CELL(\"filename\")."
  sheets names local-names tables date-system file)

(defvar grid-workbook--parsed-names (make-hash-table :test 'equal)
  "Definitions of names already parsed, from their text.")

(defvar grid-workbook--name-index (make-hash-table :test 'eq :weakness 'key)
  "Maps a workbook's list of names to (LENGTH . TABLE).
TABLE holds each entry of the list under its name in upper case.  Its
entries change in place, so the table holds them, not definitions; one
added makes the list longer, and a new table is made.")

(defun grid-workbook-name-definition (workbook name)
  "Return the definition of NAME in WORKBOOK, ignoring case, or nil."
  (let ((names (grid-workbook-names workbook)))
    (if (length< names 16)
        (cdr (assoc-string name names t))
      (let ((index (gethash names grid-workbook--name-index))
            (length (safe-length names)))
        (unless (eql (car index) length)
          (let ((table (make-hash-table :test 'equal :size length)))
            (dolist (entry (reverse names))
              (puthash (upcase (car entry)) entry table))
            (setq index (puthash names (cons length table)
                                 grid-workbook--name-index))))
        (cdr (gethash (upcase name) (cdr index)))))))

(defun grid-workbook--local-definition (workbook sheet name)
  "Return the definition of NAME belonging to the sheet called SHEET, or nil."
  (cl-loop for ((owner . local) . definition) in (grid-workbook-local-names workbook)
           when (and (string-equal-ignore-case local name)
                     (grid-calc--same-sheet-p owner sheet))
           return definition))

(defun grid-workbook--names-resolver (workbook)
  "Return the function that finds the parsed definition of a name of WORKBOOK.
It takes the NAME, the SHEET it was qualified with or nil, and the MODEL
of the formula using it.  A name of SHEET, or else of MODEL's own sheet,
comes before one of the workbook."
  (lambda (name &optional sheet model)
    (when-let* ((text
                 (or (and (grid-workbook-local-names workbook)
                          (let ((owner (or sheet
                                           (and model
                                                (cl-loop for source in (grid-workbook-sheets workbook)
                                                         when (eq (gethash :model source) model)
                                                         return (grid-workbook-sheet-name source))))))
                            (and owner
                                 (grid-workbook--local-definition workbook owner name))))
                     (grid-workbook-name-definition workbook name))))
      (with-memoization (gethash text grid-workbook--parsed-names)
        (condition-case nil
            (grid-parser-parse (substring text 1))
          (error nil))))))

(defun grid-workbook--tables-resolver (workbook)
  "Return the function that finds a table of WORKBOOK, as `grid-calc-tables'."
  (when (grid-workbook-tables workbook)
    (lambda (name &optional model row col)
      (cl-flet ((found (table)
                  (when-let* ((source (cl-find (nth 1 table)
                                               (grid-workbook-sheets workbook)
                                               :key #'grid-workbook-sheet-name
                                               :test #'grid-calc--same-sheet-p)))
                    (cons (gethash :model source) (nthcdr 2 table)))))
        (if name
            (when-let* ((table (cl-find name (grid-workbook-tables workbook)
                                        :key #'car
                                        :test #'string-equal-ignore-case)))
              (found table))
          (cl-loop for table in (grid-workbook-tables workbook)
                   for result = (found table)
                   when (and result (eq (car result) model) row col
                             (<= (nth 1 result) row (nth 3 result))
                             (<= (nth 2 result) col (nth 4 result)))
                   return result))))))

(defun grid-workbook-set-names (workbook names)
  "Give WORKBOOK the defined NAMES, an alist from name to definition.
Every formula is computed again, as any of them may use a name."
  (setf (grid-workbook-names workbook) (copy-tree names))
  (mapc #'grid-calc-forget (grid-workbook--models workbook))
  (grid-workbook-recalculate workbook t))

(defun grid-workbook-define-name (workbook name definition)
  "Define NAME in WORKBOOK as DEFINITION, a reference such as =Sheet1!$B$2.
A name defined already is redefined, keeping how it was written."
  (when-let* ((problem (grid-calc-name-problem name)))
    (error "%s" problem))
  (let* ((names (copy-tree (grid-workbook-names workbook)))
         (entry (assoc-string name names t)))
    (if entry
        (setcdr entry definition)
      (setq names (append names (list (cons name definition)))))
    (grid-workbook-set-names workbook names)))

(defun grid-workbook-delete-name (workbook name)
  "Take the defined NAME out of WORKBOOK."
  (grid-workbook-set-names
   workbook
   (cl-remove-if (lambda (entry) (string-equal-ignore-case (car entry) name))
                 (grid-workbook-names workbook))))

(defconst grid-workbook--changing-keys
  '(:set-raw-value-at :merge-cells :unmerge-cell :sort-by-column
    :add-row :delete-row :delete-rows :add-column :delete-column :grow)
  "Data source functions after which a workbook recalculates.")

(defvar grid-workbook--rewrites (make-hash-table :test 'eq :weakness 'key)
  "Formulas rewritten in other sheets by a structural change.
Maps the model the change produced to (SOURCE ROW COL OLD NEW) entries,
so that undoing the change can put the formulas back.")

(defun grid-workbook-of (source)
  "Return the workbook the data source SOURCE is a sheet of, or nil."
  (gethash :workbook source))

(defun grid-workbook-sheet-name (source)
  "Return the name SOURCE has as a sheet, or nil."
  (gethash :sheet-name source))

(defun grid-workbook-sheet-names (workbook)
  "Return the names of the sheets of WORKBOOK, in order."
  (mapcar #'grid-workbook-sheet-name (grid-workbook-sheets workbook)))

(defun grid-workbook-sheet (workbook name)
  "Return the sheet of WORKBOOK called NAME, ignoring case, or nil."
  (let ((sheets (grid-workbook-sheets workbook)))
    (while (and sheets (not (grid-calc--same-sheet-p
                             (grid-workbook-sheet-name (car sheets)) name)))
      (setq sheets (cdr sheets)))
    (car sheets)))

(defun grid-workbook-name-problem (workbook name &optional except as-read)
  "Return why NAME cannot name a sheet of WORKBOOK, or nil when it can.
EXCEPT is a sheet allowed to have that name already.  AS-READ, for a name
read from a file, lets it be longer than a name typed here can be."
  (cond
   ((or (not (stringp name)) (string-empty-p (string-trim name)))
    "A sheet name cannot be empty")
   ((string-match-p "[][:*?/\\]" name)
    "A sheet name cannot contain any of [ ] : * ? / \\")
   ((or (string-prefix-p "'" name) (string-suffix-p "'" name))
    "A sheet name cannot begin or end with an apostrophe")
   ((and (not as-read) (> (length name) 31))
    "A sheet name cannot be longer than 31 characters")
   ((let ((holder (and workbook (grid-workbook-sheet workbook name))))
      (and holder (not (eq holder except))))
    (format "There is already a sheet called %s" name))))

(defun grid-workbook-fresh-name (workbook &optional wanted)
  "Return a name no sheet of WORKBOOK has: WANTED, or the next SheetN.
WANTED is taken as it is when it can name a sheet, and otherwise without
the characters that keep it from being one."
  (let ((cleaned (and (stringp wanted)
                      (string-trim
                       (replace-regexp-in-string "[][:*?/\\']" " " wanted)))))
    (when (and cleaned (> (length cleaned) 31))
      (setq cleaned (substring cleaned 0 31)))
    (cond
     ((and (stringp wanted) (not (grid-workbook-name-problem workbook wanted)))
      wanted)
     ((and cleaned (not (grid-workbook-name-problem workbook cleaned)))
      cleaned)
     (t (let ((number 1))
          (while (and workbook
                      (grid-workbook-sheet workbook (format "Sheet%d" number)))
            (setq number (1+ number)))
          (format "Sheet%d" number))))))

(defun grid-workbook--models (workbook)
  "Return the models of the sheets of WORKBOOK."
  (delq nil (mapcar (lambda (source) (gethash :model source))
                    (grid-workbook-sheets workbook))))

(defun grid-workbook--resolver (workbook)
  "Return the function that finds the model of a sheet of WORKBOOK by name.
Called with :names, it returns the names of the sheets in order."
  (lambda (name)
    (if (eq name :names)
        (grid-workbook-sheet-names workbook)
      (when-let* ((source (grid-workbook-sheet workbook name)))
        (gethash :model source)))))

(defun grid-workbook--file-name-resolver (workbook)
  "Return the function that names the file and sheet of a model of WORKBOOK.
See `grid-calc-file-name'."
  (lambda (model)
    (when-let* ((file (grid-workbook-file workbook))
                (source (cl-find model (grid-workbook-sheets workbook)
                                 :key (lambda (source) (gethash :model source)))))
      (concat (file-name-directory file) "[" (file-name-nondirectory file) "]"
              (grid-workbook-sheet-name source)))))

(defvar grid-workbook-holding nil
  "Non-nil while a run of changes holds back recalculation.
`grid-workbook-recalculate' then only notes that it was asked, and
whoever binds this recalculates everything once at the end; see
`grid-workbook-with-held-recalculation'.")

(defvar grid-workbook--held nil
  "Non-nil when a recalculation was held back.")

(defmacro grid-workbook-with-held-recalculation (&rest body)
  "Run BODY, recalculating the workbooks it changes once, at the end.
Each structural change otherwise recomputes every sheet."
  (declare (indent 0))
  (let ((outer (make-symbol "outer")))
    `(let ((,outer grid-workbook-holding))
       (if ,outer
           (progn ,@body)
         (let ((grid-workbook--held nil))
           (prog1 (let ((grid-workbook-holding t)
                        (grid-calc-defer t))
                    ,@body)
             (dolist (workbook (delete-dups grid-workbook--held))
               (grid-workbook-recalculate workbook t))))))))

(defmacro grid-workbook-with-batched-edits (&rest body)
  "Run BODY, a series of cell edits, recalculating the workbooks once.
Only the cells that changed and what reads them are computed then, so a
paste or fill of many cells costs one recalculation, not one per cell.
Inside a held recalculation, BODY simply joins it."
  (declare (indent 0))
  (let ((outer (make-symbol "outer")))
    `(let ((,outer grid-workbook-holding))
       (if ,outer
           (progn ,@body)
         (let ((grid-workbook--held nil))
           (prog1 (let ((grid-workbook-holding t)
                        (grid-calc-defer t))
                    ,@body)
             (dolist (workbook (delete-dups grid-workbook--held))
               (grid-workbook-recalculate workbook))))))))

(defun grid-workbook-recalculate (workbook &optional everything)
  "Recalculate the sheets of WORKBOOK, each reading the others' new values.
Only what changed and what reads it is computed, unless EVERYTHING is
non-nil, as it has to be when the sheets themselves changed."
  (if grid-workbook-holding
      (push workbook grid-workbook--held)
  (let ((grid-calc-sheets (grid-workbook--resolver workbook))
        (grid-calc-names (grid-workbook--names-resolver workbook))
        (grid-calc-tables (grid-workbook--tables-resolver workbook))
        (grid-calc-file-name (grid-workbook--file-name-resolver workbook))
        (grid-format-date-system (grid-workbook-date-system-of workbook))
        (grid-calc-defer nil))
    (grid-table-calc-evaluate-models (grid-workbook--models workbook)
                                     everything))))

(defun grid-workbook-date-system-of (workbook)
  "Return the date system of WORKBOOK, 1900 or 1904; nil for no workbook."
  (and workbook (or (grid-workbook-date-system workbook) 1900)))

(defun grid-workbook-source-date-system (source)
  "Return the date system of SOURCE's workbook, or nil without one."
  (and source (grid-workbook-date-system-of (grid-workbook-of source))))

(defun grid-workbook-create (sheets &optional as-read date-system)
  "Return a workbook of SHEETS, a list of (NAME . SOURCE) in order.
AS-READ means the names come from a file, which may hold one longer than
a sheet can be given here; they are kept as they are.  DATE-SYSTEM is
1904 for a workbook whose dates count from 1904."
  (let ((workbook (grid-workbook--create :date-system date-system)))
    (pcase-dolist (`(,name . ,source) sheets)
      (grid-workbook-add-sheet workbook source name nil as-read))
    (grid-workbook-recalculate workbook t)
    workbook))

(defun grid-workbook-ensure (source)
  "Return the workbook of SOURCE, making it the only sheet of a new one.
Its name is then its title when that can name a sheet, else Sheet1."
  (or (grid-workbook-of source)
      (grid-workbook-create
       (list (cons (grid-workbook-fresh-name nil (gethash :title source))
                   source)))))

(defun grid-workbook-add-sheet (workbook source name &optional position as-read)
  "Add SOURCE to WORKBOOK as the sheet NAME and return SOURCE.
POSITION is the index it takes; it goes last by default.  AS-READ is as
for `grid-workbook-create'."
  (when-let* ((problem (grid-workbook-name-problem workbook name nil as-read)))
    (error "%s" problem))
  (puthash :workbook workbook source)
  (puthash :sheet-name name source)
  (let ((sheets (grid-workbook-sheets workbook)))
    (setf (grid-workbook-sheets workbook)
          (if (and position (< -1 position (length sheets)))
              (append (cl-subseq sheets 0 position) (list source)
                      (nthcdr position sheets))
            (append sheets (list source)))))
  source)

(defun grid-workbook-new-sheet (workbook &optional name position)
  "Add an empty sheet called NAME to WORKBOOK and return its data source."
  (let ((name (or name (grid-workbook-fresh-name workbook)))
        (headers (list "" "" "" "" "")))
    (prog1 (grid-workbook-add-sheet
            workbook
            (make-default-data-source
             headers
             (cl-loop repeat 10 collect (make-list (length headers) ""))
             name)
            name position)
      ;; References to a sheet of that name were waiting for it.
      (grid-workbook-recalculate workbook t))))

(defun grid-workbook-remove-sheet (workbook source)
  "Take SOURCE out of WORKBOOK.  References into it become #REF!.
One through several sheets that ends at SOURCE ends next to it instead."
  (when (null (cdr (grid-workbook-sheets workbook)))
    (error "A workbook keeps at least one sheet"))
  (let ((old (grid-workbook-sheet-name source))
        (names (grid-workbook-sheet-names workbook)))
    (grid-workbook--rewrite-formulas
     workbook
     (lambda (formula _sheet)
       (grid-calc-remove-sheet-in-formula formula old names))
     source))
  (setf (grid-workbook-sheets workbook)
        (delq source (grid-workbook-sheets workbook)))
  (remhash :workbook source)
  (grid-workbook-recalculate workbook t))

(defun grid-workbook-move-sheet (workbook source delta)
  "Move SOURCE by DELTA places among the sheets of WORKBOOK."
  (let* ((sheets (delq source (copy-sequence (grid-workbook-sheets workbook))))
         (index (cl-position source (grid-workbook-sheets workbook)))
         (target (max 0 (min (length sheets) (+ index delta)))))
    (setf (grid-workbook-sheets workbook)
          (append (cl-subseq sheets 0 target) (list source)
                  (nthcdr target sheets)))
    ;; A reference through several sheets may now take in others.
    (unless (= target index)
      (grid-workbook-recalculate workbook t))))

(defun grid-workbook--rewrite-formulas (workbook function &optional except)
  "Pass every formula of WORKBOOK's sheets through FUNCTION.
FUNCTION is called with the formula and the name of its sheet, and
returns the formula to store.  The sheet EXCEPT is left alone.  Return
the (SOURCE ROW COL OLD NEW) entries of the formulas that changed."
  (let (changes)
    (dolist (source (grid-workbook-sheets workbook))
      (unless (eq source except)
        (let* ((model (gethash :model source))
               (cols (grid-data-model-cols model))
               (cells (grid-data-model-cells model))
               (name (grid-workbook-sheet-name source)))
          (dotimes (index (length cells))
            (let* ((cell (aref cells index))
                   (old (grid-cell-raw-input cell)))
              ;; Only a formula that names a sheet can be affected.
              (when (and (stringp old) (string-prefix-p "=" old)
                         (string-search "!" old))
                (let ((new (funcall function old name)))
                  (unless (equal old new)
                    (set-grid-cell-raw-input cell new)
                    (grid-calc-forget model)
                    (push (list source (/ index cols) (% index cols) old new)
                          changes)))))))))
    ;; Definitions of names name their sheet: they are read as formulas
    ;; of no sheet.
    (dolist (entry (grid-workbook-names workbook))
      (let* ((old (cdr entry))
             (new (funcall function old nil)))
        (unless (equal old new)
          (setcdr entry new)
          (push (list :name (car entry) nil old new) changes))))
    (nreverse changes)))

(defun grid-workbook-rename-sheet (workbook source name)
  "Call the sheet SOURCE of WORKBOOK NAME, in the formulas that read it too."
  (when-let* ((problem (grid-workbook-name-problem workbook name source)))
    (error "%s" problem))
  (let ((old (grid-workbook-sheet-name source)))
    (puthash :sheet-name name source)
    (grid-workbook--rewrite-formulas
     workbook
     (lambda (formula _sheet)
       (grid-calc-rename-sheet-in-formula formula old name)))
    (grid-workbook-recalculate workbook t)))

(defun grid-workbook--line-change (key args old-model)
  "Return (AXIS AT DELTA) for the line that KEY, called with ARGS, moved.
OLD-MODEL is the model before the change.  Return nil when no cell
moved, as when a line is added at the end."
  (let ((index (car args))
        (rows (grid-data-model-rows old-model))
        (cols (grid-data-model-cols old-model)))
    (pcase key
      ;; A row is counted from the first row under the header row.
      (:add-row (and index (< -2 index (1- rows)) (list 'row (1+ index) 1)))
      (:delete-row (and index (list 'row (1+ index) -1)))
      ;; A list of rows, counted from the first row of the sheet
      (:delete-rows (and index (list 'row index -1)))
      (:add-column (and index (< -1 index cols) (list 'col index 1)))
      (:delete-column (and index (list 'col index -1))))))

(defun grid-workbook--new-column-name (names)
  "Return the name Excel gives a column added to a table of column NAMES."
  (cl-loop for n from 1
           for name = (format "Column%d" n)
           unless (member-ignore-case name names) return name))

(defun grid-workbook--shift-table (table axis at delta)
  "Return TABLE moved for a line inserted or deleted, or nil when it goes.
TABLE is (NAME SHEET TOP LEFT BOTTOM RIGHT HEADERS TOTALS COLUMNS) and
AXIS, AT and DELTA are as for `grid-calc-restructure-formula'.  A line
inserted before a table moves it, and one inserted inside it, or just
above its totals row, makes it a line longer; a column inserted so gets
a name of its own.  A deleted line inside it makes it shorter.  A table
whose header row is deleted, or that is left no data row or no column,
goes."
  (pcase-let* ((`(,name ,sheet ,top ,left ,bottom ,right ,headers ,totals ,columns)
                table)
               (lines (if (listp at) at (list at)))
               (before (lambda (index &optional inclusive)
                         (cl-count-if (lambda (line)
                                        (if inclusive (<= line index) (< line index)))
                                      lines))))
    (if (eq axis 'row)
        (if (> delta 0)
            (list name sheet (if (<= at top) (1+ top) top) left
                  (if (<= at bottom) (1+ bottom) bottom) right
                  headers totals columns)
          (let* ((totals (if (and (> totals 0) (memq bottom lines)) 0 totals))
                 (new-top (- top (funcall before top)))
                 (new-bottom (- bottom (funcall before bottom t))))
            (unless (or (and (> headers 0) (memq top lines))
                        (< (- new-bottom new-top -1 headers totals) 1))
              (list name sheet new-top left new-bottom right
                    headers totals columns))))
      (if (> delta 0)
          (cond ((<= at left)
                 (list name sheet top (1+ left) bottom (1+ right)
                       headers totals columns))
                ((<= at right)
                 (let ((index (- at left)))
                   (list name sheet top left bottom (1+ right) headers totals
                         (append (seq-take columns index)
                                 (list (grid-workbook--new-column-name columns))
                                 (seq-drop columns index)))))
                (t table))
        (let ((kept (cl-loop for column in columns
                             for index from left
                             unless (memq index lines) collect column)))
          (when kept
            (list name sheet top (- left (funcall before left)) bottom
                  (- right (funcall before right t)) headers totals kept)))))))

(defun grid-workbook--shift-tables (workbook source change)
  "Move WORKBOOK's tables on the sheet SOURCE along CHANGE, (AXIS AT DELTA).
A column added inside a table has its name written in the table's
header cell, as the file wants it there.  Return the tables as they
were, or nil when none changed."
  (let* ((old (grid-workbook-tables workbook))
         (new (delq nil
                    (mapcar
                     (lambda (table)
                       (if (grid-calc--same-sheet-p
                            (nth 1 table) (grid-workbook-sheet-name source))
                           (apply #'grid-workbook--shift-table table change)
                         table))
                     old))))
    (unless (equal old new)
      (when (and (eq (nth 0 change) 'col) (> (nth 2 change) 0))
        (let ((model (gethash :model source))
              (at (nth 1 change)))
          (dolist (table new)
            (pcase-let ((`(,_ ,sheet ,top ,left ,_ ,right ,headers ,_ ,columns)
                         table))
              (when (and (> headers 0) (< left at) (<= at right)
                         (grid-calc--same-sheet-p
                          sheet (grid-workbook-sheet-name source)))
                (set-grid-cell-raw-input
                 (grid-model-get-cell model top at)
                 (nth (- at left) columns)))))))
      (setf (grid-workbook-tables workbook) new)
      (or old t))))

(defun grid-workbook--changed (workbook source key args old-model)
  "Bring WORKBOOK up to date after KEY, called with ARGS, changed SOURCE.
The other sheets' references into SOURCE follow a line that moved, and
so do the tables on SOURCE; every sheet is recalculated."
  (let ((model (gethash :model source))
        (change (and old-model (grid-workbook--line-change key args old-model)))
        (name (grid-workbook-sheet-name source))
        tables)
    (when (and change (not (eq model old-model)))
      (let* ((changes
              (grid-workbook--rewrite-formulas
               workbook
               (lambda (formula home)
                 (grid-calc-restructure-formula
                  formula (nth 0 change) (nth 1 change) (nth 2 change)
                  home name))
               source))
             (old-tables (grid-workbook--shift-tables workbook source change)))
        (when old-tables
          (setq tables t)
          (push (list :tables nil nil
                      (and (listp old-tables) old-tables)
                      (grid-workbook-tables workbook))
                changes))
        (when changes
          (puthash model changes grid-workbook--rewrites))))
    ;; Formulas that name a table read other cells now.
    (when tables
      (mapc #'grid-calc-forget (grid-workbook--models workbook)))
    (grid-workbook-recalculate workbook tables)))

(defun grid-workbook-model-restored (source from to)
  "Bring SOURCE's workbook up to date after its model went from FROM to TO.
That is what undoing or redoing a structural change does.  Formulas the
change rewrote in other sheets go back with it."
  (when-let* ((workbook (grid-workbook-of source)))
    (cl-flet ((put-back (entries pick)
                (pcase-dolist (`(,sheet ,row ,col ,old ,new) entries)
                  (cond
                   ((eq sheet :tables)
                    (setf (grid-workbook-tables workbook) (funcall pick old new))
                    (mapc #'grid-calc-forget (grid-workbook--models workbook)))
                   ((eq sheet :name)
                    (when-let* ((entry (assoc-string
                                        row (grid-workbook-names workbook) t)))
                      (setcdr entry (funcall pick old new))))
                   ((memq sheet (grid-workbook-sheets workbook))
                    (set-grid-cell-raw-input
                     (grid-model-get-cell (gethash :model sheet) row col)
                     (funcall pick old new)))))))
      (put-back (gethash from grid-workbook--rewrites)
                (lambda (old _new) old))
      (put-back (gethash to grid-workbook--rewrites)
                (lambda (_old new) new)))
    ;; Formulas were put back in place, behind the models' backs.
    (grid-workbook-recalculate workbook t)))

(defun grid-workbook--wrap (source key)
  "Make the function KEY of SOURCE keep its workbook up to date."
  (when-let* ((function (gethash key source)))
    (puthash
     key
     (lambda (s &rest args)
       (let ((workbook (grid-workbook-of s))
             (old-model (gethash :model s))
             (grid-model-first-row-is-header (not (gethash :no-header-row s))))
         (prog1 (if (null workbook)
                    (apply function s args)
                  (prog1 (let ((grid-calc-defer t)
                               (grid-calc-current-sheet
                                (grid-workbook-sheet-name s)))
                           (apply function s args))
                    (grid-workbook--changed workbook s key args old-model)))
           ;; Column widths set by hand stay with their columns, and the
           ;; frozen rows and columns stay frozen.
           (let ((model (gethash :model s)))
             (unless (or (eq model old-model) (null old-model))
               (pcase (grid-workbook--line-change key args old-model)
                 (`(col ,at ,delta)
                  (grid-model-carry-widths old-model model at delta)
                  (grid-model-carry-frozen old-model model 'col at delta)
                  (grid-model-carry-hidden old-model model 'col at delta)
                  (grid-model-carry-filters old-model model 'col at delta)
                  (grid-model-carry-arrays old-model model 'col at delta)
                  (grid-model-carry-validations
                   old-model model 'col at delta (grid-workbook-sheet-name s))
                  (grid-model-carry-charts
                   old-model model 'col at delta (grid-workbook-sheet-name s)))
                 (`(row ,at ,delta)
                  (grid-model-carry-widths old-model model)
                  (grid-model-carry-frozen old-model model 'row at delta)
                  (grid-model-carry-hidden old-model model 'row at delta)
                  (grid-model-carry-filters old-model model 'row at delta)
                  (grid-model-carry-arrays old-model model 'row at delta)
                  (grid-model-carry-validations
                   old-model model 'row at delta (grid-workbook-sheet-name s))
                  (grid-model-carry-charts
                   old-model model 'row at delta (grid-workbook-sheet-name s)))
                 (_ (grid-model-carry-widths old-model model)
                    (grid-model-carry-frozen old-model model)
                    (grid-model-carry-validations old-model model)
                    (grid-model-carry-charts old-model model)
                    (grid-model-carry-arrays old-model model)
                    (grid-model-carry-filters old-model model)
                    ;; A sort moved the hidden rows along itself.
                    (unless (eq key :sort-by-column)
                      (grid-model-carry-hidden old-model model)))))))))
     source)))

(provide 'grid-data-source)

;;; grid-data-source.el ends here
