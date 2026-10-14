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
               (grid-model-set-hidden (gethash :model s) rows cols))
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
formula, such as =Sheet1!$B$2:$B$9."
  sheets names)

(defvar grid-workbook--parsed-names (make-hash-table :test 'equal)
  "Definitions of names already parsed, from their text.")

(defun grid-workbook-name-definition (workbook name)
  "Return the definition of NAME in WORKBOOK, ignoring case, or nil."
  (cdr (assoc-string name (grid-workbook-names workbook) t)))

(defun grid-workbook--names-resolver (workbook)
  "Return the function that finds the parsed definition of a name of WORKBOOK."
  (lambda (name)
    (when-let* ((text (grid-workbook-name-definition workbook name)))
      (with-memoization (gethash text grid-workbook--parsed-names)
        (condition-case nil
            (let ((ast (grid-parser-parse (substring text 1))))
              (and (memq (car-safe ast) '(CELL-REF RANGE)) ast))
          (error nil))))))

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
  (cl-find-if (lambda (source)
                (grid-calc--same-sheet-p (grid-workbook-sheet-name source) name))
              (grid-workbook-sheets workbook)))

(defun grid-workbook-name-problem (workbook name &optional except)
  "Return why NAME cannot name a sheet of WORKBOOK, or nil when it can.
EXCEPT is a sheet allowed to have that name already."
  (cond
   ((or (not (stringp name)) (string-empty-p (string-trim name)))
    "A sheet name cannot be empty")
   ((string-match-p "[][:*?/\\]" name)
    "A sheet name cannot contain any of [ ] : * ? / \\")
   ((or (string-prefix-p "'" name) (string-suffix-p "'" name))
    "A sheet name cannot begin or end with an apostrophe")
   ((> (length name) 31) "A sheet name cannot be longer than 31 characters")
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
  "Return the function that finds the model of a sheet of WORKBOOK by name."
  (lambda (name)
    (when-let* ((source (grid-workbook-sheet workbook name)))
      (gethash :model source))))

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

(defun grid-workbook-recalculate (workbook &optional everything)
  "Recalculate the sheets of WORKBOOK, each reading the others' new values.
Only what changed and what reads it is computed, unless EVERYTHING is
non-nil, as it has to be when the sheets themselves changed."
  (if grid-workbook-holding
      (push workbook grid-workbook--held)
  (let ((grid-calc-sheets (grid-workbook--resolver workbook))
        (grid-calc-names (grid-workbook--names-resolver workbook))
        (grid-calc-defer nil))
    (grid-table-calc-evaluate-models (grid-workbook--models workbook)
                                     everything))))

(defun grid-workbook-create (sheets)
  "Return a workbook of SHEETS, a list of (NAME . SOURCE) in order."
  (let ((workbook (grid-workbook--create)))
    (pcase-dolist (`(,name . ,source) sheets)
      (grid-workbook-add-sheet workbook source name))
    (grid-workbook-recalculate workbook t)
    workbook))

(defun grid-workbook-ensure (source)
  "Return the workbook of SOURCE, making it the only sheet of a new one.
Its name is then its title when that can name a sheet, else Sheet1."
  (or (grid-workbook-of source)
      (grid-workbook-create
       (list (cons (grid-workbook-fresh-name nil (gethash :title source))
                   source)))))

(defun grid-workbook-add-sheet (workbook source name &optional position)
  "Add SOURCE to WORKBOOK as the sheet NAME and return SOURCE.
POSITION is the index it takes; it goes last by default."
  (when-let* ((problem (grid-workbook-name-problem workbook name)))
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
  "Take SOURCE out of WORKBOOK.  References into it become #REF!."
  (when (null (cdr (grid-workbook-sheets workbook)))
    (error "A workbook keeps at least one sheet"))
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
                  (nthcdr target sheets)))))

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

(defun grid-workbook--changed (workbook source key args old-model)
  "Bring WORKBOOK up to date after KEY, called with ARGS, changed SOURCE.
The other sheets' references into SOURCE follow a line that moved, and
every sheet is recalculated."
  (let ((model (gethash :model source))
        (change (and old-model (grid-workbook--line-change key args old-model)))
        (name (grid-workbook-sheet-name source)))
    (when (and change (not (eq model old-model)))
      (when-let* ((changes
                   (grid-workbook--rewrite-formulas
                    workbook
                    (lambda (formula home)
                      (grid-calc-restructure-formula
                       formula (nth 0 change) (nth 1 change) (nth 2 change)
                       home name))
                    source)))
        (puthash model changes grid-workbook--rewrites)))
    (grid-workbook-recalculate workbook)))

(defun grid-workbook-model-restored (source from to)
  "Bring SOURCE's workbook up to date after its model went from FROM to TO.
That is what undoing or redoing a structural change does.  Formulas the
change rewrote in other sheets go back with it."
  (when-let* ((workbook (grid-workbook-of source)))
    (cl-flet ((put-back (entries pick)
                (pcase-dolist (`(,sheet ,row ,col ,old ,new) entries)
                  (cond
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
                  (grid-model-carry-validations
                   old-model model 'col at delta (grid-workbook-sheet-name s))
                  (grid-model-carry-charts
                   old-model model 'col at delta (grid-workbook-sheet-name s)))
                 (`(row ,at ,delta)
                  (grid-model-carry-widths old-model model)
                  (grid-model-carry-frozen old-model model 'row at delta)
                  (grid-model-carry-hidden old-model model 'row at delta)
                  (grid-model-carry-validations
                   old-model model 'row at delta (grid-workbook-sheet-name s))
                  (grid-model-carry-charts
                   old-model model 'row at delta (grid-workbook-sheet-name s)))
                 (_ (grid-model-carry-widths old-model model)
                    (grid-model-carry-frozen old-model model)
                    (grid-model-carry-validations old-model model)
                    (grid-model-carry-charts old-model model)
                    ;; A sort moved the hidden rows along itself.
                    (unless (eq key :sort-by-column)
                      (grid-model-carry-hidden old-model model)))))))))
     source)))

(provide 'grid-data-source)

;;; grid-data-source.el ends here
