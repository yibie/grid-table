;;; grid-table.el --- Grid table for displaying and editing tabular data -*- lexical-binding: t -*-
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1"))

(require 'cl-lib)
(require 'cl-macs)

(when-let* ((library-file (or load-file-name buffer-file-name))
            (plugin-directory
             (expand-file-name "plugins" (file-name-directory library-file)))
            ((file-directory-p plugin-directory)))
  (add-to-list 'load-path plugin-directory))

(require 'grid-data-source)
(require 'grid-data-model)
(require 'grid-table-parser)
(require 'grid-table-format)
(require 'grid-table-persistence)
(require 'grid-table-text)
(require 'grid-table-chart)
(require 'grid-table-chart-svg)
(require 'grid-table-plugins)

;; Public version identifier.
(defconst grid-table-version "0.4.0")

;;;----------------------------------------------------------------------
;;; Helper functions for data source access
;;;----------------------------------------------------------------------

(defun grid-get-column-count (source)
  "Get column count from data source."
  (funcall (gethash :get-column-count source) source))

(defun grid-get-row-count (source)
  "Get row count from data source."
  (funcall (gethash :get-row-count source) source))

(defun grid-get-header-value (source col)
  "Get header value from data source."
  (funcall (gethash :get-header-value source) source col))

(defun grid-table-outside-p (source row col)
  "Return non-nil when ROW and COL lie outside the cells SOURCE has.
Such a cell is one of the empty cells past a sheet's last row or column,
where a spreadsheet's cursor may go: it reads as empty."
  (or (< row 0) (< col 0)
      (>= row (grid-get-row-count source))
      (>= col (grid-get-column-count source))))

(defun grid-get-raw-value-at (source row col)
  "Get raw value from data source.
A cell outside the sheet's cells is empty, \"\"."
  (if (grid-table-outside-p source row col)
      ""
    (funcall (gethash :get-raw-value-at source) source row col)))

(defun grid-get-computed-value-at (source row col)
  "Get computed value from data source.
A cell outside the sheet's cells is empty, \"\"."
  (if (grid-table-outside-p source row col)
      ""
    (funcall (gethash :get-computed-value-at source) source row col)))

(defun grid-set-raw-value-at (source row col value)
  "Set raw value in data source."
  (funcall (gethash :set-raw-value-at source) source row col value))

(defun grid-get-merges (source)
  "Return merge ranges from SOURCE."
  (when-let* ((fn (gethash :get-merges source)))
    (funcall fn source)))

(defun grid-merge-cells (source top left bottom &optional right)
  "Merge TOP LEFT through BOTTOM RIGHT in SOURCE.
When RIGHT is nil, use the legacy horizontal TOP LEFT BOTTOM call."
  (funcall (gethash :merge-cells source)
           source top left bottom right))

(defun grid-get-style-at (source row col)
  "Return the style plist of the cell at ROW and COL in SOURCE.
Sources that do not store styles give nil, as does a cell outside the
sheet's cells."
  (when-let* ((function (gethash :get-style-at source))
              ((not (grid-table-outside-p source row col))))
    (funcall function source row col)))

(defun grid-set-style-at (source row col style)
  "Give the cell at ROW and COL in SOURCE the plist STYLE; nil clears it."
  (when-let* ((function (gethash :set-style-at source)))
    (funcall function source row col style)))

(defcustom grid-table-currency-symbol "$"
  "Currency symbol the currency format is offered with."
  :type 'string
  :group 'grid-table)

(defconst grid-table--date-formula-regexp
  "\\`=[ \t]*\\(TODAY\\|DATE\\|DATEVALUE\\|EDATE\\|EOMONTH\\|NOW\\)[ \t]*("
  "Matches a formula that starts by computing a date.")

(defun grid-table-cell-format (source row col)
  "Return the format the cell at ROW and COL of SOURCE is shown with.
That is the format of its style.  A cell without one whose formula
starts by computing a date, such as =TODAY()+7, is shown as a date."
  (let ((own (plist-get (grid-get-style-at source row col) :format)))
    (cond
     ;; The general format asked for by name: not even a date formula
     ;; is shown as a date.
     ((eq (car-safe own) 'general) nil)
     (own)
     (t (let ((raw (and (gethash :get-raw-value-at source)
                        (grid-get-raw-value-at source row col)))
              (case-fold-search t))
          (and (stringp raw)
               (string-match grid-table--date-formula-regexp raw)
               (list 'date (if (string= (upcase (match-string 1 raw)) "NOW")
                               "yyyy-mm-dd hh:mm"
                             "yyyy-mm-dd"))))))))

(defun grid-table-false-value-p (source row col value)
  "Return non-nil when VALUE, computed at ROW and COL of SOURCE, is FALSE.
A formula that comes out false computes nil, as an empty cell does not;
so does a cell showing a FALSE that a formula spills."
  (and (null value)
       (gethash :get-raw-value-at source)
       (let ((raw (grid-get-raw-value-at source row col))
             (model (gethash :model source)))
         (or (and (stringp raw) (string-prefix-p "=" raw))
             (and model (grid-calc-spilled-p model row col))))))

;; Dates shown and typed in a grid count as its workbook's do.
(setq grid-format-date-system-function
      (lambda ()
        (and (boundp 'grid-table--data-source)
             (grid-workbook-source-date-system grid-table--data-source))))

(defun grid-get-display-value-at (source row col)
  "Return what the cell at ROW and COL of SOURCE shows.
That is its computed value written after its format.  A value that is
not plain data, such as a chart, is returned as it is."
  (let ((value (grid-get-computed-value-at source row col)))
    (cond ((grid-table-false-value-p source row col value) "FALSE")
          ((or (stringp value) (numberp value) (memq value '(nil t)))
           (grid-format-display value (grid-table-cell-format source row col)))
          (t value))))

(defun grid-table-edit-text (source row col)
  "Return the text an edit of the cell at ROW and COL of SOURCE starts from.
A date and a percentage are edited as they read, not as the number
stored for them."
  (grid-format-edit-text (or (grid-get-raw-value-at source row col) "")
                         (plist-get (grid-get-style-at source row col)
                                    :format)))

(defun grid-table--reads-as-other-than-text-p (text)
  "Return non-nil when TEXT, stored as it is, is not plain text.
That is a formula, a number, a date, a truth value, an error value or
text that starts with the apostrophe that marks text."
  (or (string-prefix-p "=" text)
      (string-prefix-p "'" text)
      (grid-format-number text)
      (grid-format-parse-entry text)
      (member (upcase (string-trim text)) '("TRUE" "FALSE"))
      (member text grid-calc-error-values)))

(defun grid-table-read-entry (source row col text)
  "Read TEXT as typed into the cell at ROW and COL of SOURCE.
Return (RAW . STYLE): RAW is the text to store and STYLE the style to
give the cell, or the symbol `keep' to leave its style alone.  A date
written year first, a percentage, an amount of money and a number with
thousands separators are stored as numbers; a cell without a format
takes the one that shows the number as it was typed.  Sources that keep
no styles store TEXT as it is.  In a cell with the text format TEXT is
text: it is stored after an apostrophe when it would read otherwise, so
the number already in such a cell stays a number, as in a spreadsheet."
  (let* ((style (grid-get-style-at source row col))
         (format (plist-get style :format))
         (typed (and (gethash :set-style-at source)
                     (not (eq (car-safe format) 'text))
                     (or (grid-format-parse-entry text)
                         ;; A bare number typed into a percentage is that
                         ;; many percent.
                         (and (eq (car-safe format) 'percent)
                              (grid-format-number text)
                              (grid-format-parse-entry
                               (concat (string-trim text) "%")))))))
    (cond ((and (eq (car-safe format) 'text)
                (grid-table--reads-as-other-than-text-p text))
           (cons (concat "'" text) 'keep))
          ((null typed) (cons text 'keep))
          (format (cons (car typed) 'keep))
          (t (cons (car typed)
                   (plist-put (copy-sequence style) :format (cdr typed)))))))

(defun grid-table-enter-value (source row col text)
  "Store TEXT in the cell at ROW and COL of SOURCE as a user types it.
See `grid-table-read-entry' for how dates and percentages are read."
  (let ((entry (grid-table-read-entry source row col text)))
    (unless (eq (cdr entry) 'keep)
      (grid-set-style-at source row col (cdr entry)))
    (grid-set-raw-value-at source row col (car entry))))

(defun grid-table-recalculate (source &optional everything)
  "Bring the formulas of SOURCE, and of the sheets it shares a book with, up to date.
Only what changed and what reads it is computed, unless EVERYTHING is
non-nil."
  (let ((workbook (grid-workbook-of source))
        (model (gethash :model source)))
    (cond (workbook (grid-workbook-recalculate workbook everything))
          (model (grid-table-calc-evaluate model everything)))))

(defun grid-table-header-row-p (source)
  "Return non-nil when the first row of SOURCE holds its column titles.
That row then stays in place when the sheet is sorted or filtered and
cannot be deleted.  A sheet without a header row treats its first row as
any other."
  (not (gethash :no-header-row source)))

(defun grid-table-set-header-row (source header)
  "Say whether the first row of SOURCE holds its column titles: HEADER."
  (when (and header (not (grid-table-header-row-p source)))
    (when (cl-some (lambda (merge)
                     (and (= (nth 0 merge) 0) (< (nth 0 merge) (nth 2 merge))))
                   (grid-get-merges source))
      (user-error "A merge reaches down from the first row; unmerge it first")))
  (if header
      (remhash :no-header-row source)
    (puthash :no-header-row t source))
  header)

(defun grid-get-column-widths (source)
  "Return SOURCE's table of column widths set by hand, or nil.
It maps a column index to a width in characters."
  (when-let* ((function (gethash :get-column-widths source)))
    (funcall function source)))

(defun grid-set-column-width (source col width)
  "Give column COL of SOURCE a width of WIDTH characters; nil forgets it.
Return non-nil when SOURCE keeps column widths."
  (when-let* ((function (gethash :set-column-width source)))
    (funcall function source col width)
    t))

(defun grid-table-frozen (source)
  "Return (ROWS . COLS): the leading rows and columns of SOURCE kept in place.
They stay at the top and the left of the window while the rest scrolls."
  (if-let* ((function (gethash :get-frozen source)))
      (funcall function source)
    (cons 0 0)))

(defun grid-table-set-frozen (source rows cols)
  "Keep the first ROWS rows and COLS columns of SOURCE in place.
Return non-nil when SOURCE can keep rows and columns in place."
  (when-let* ((function (gethash :set-frozen source)))
    (funcall function source rows cols)
    t))

(defun grid-table-hidden (source)
  "Return (ROWS . COLS): the rows and columns of SOURCE hidden, sorted lists."
  (if-let* ((function (gethash :get-hidden source)))
      (funcall function source)
    (cons nil nil)))

(defun grid-table-set-hidden (source rows cols)
  "Hide the ROWS and COLS of SOURCE, lists of indexes; nil shows them all.
Return non-nil when SOURCE can hide rows and columns."
  (when-let* ((function (gethash :set-hidden source)))
    (funcall function source rows cols)
    t))

(defun grid-table-filters (source)
  "Return SOURCE's filtered ranges, (TOP LEFT BOTTOM RIGHT) each.
TOP is a range's header row; its hidden rows below are filtered out."
  (when-let* ((function (gethash :get-filters source)))
    (funcall function source)))

(defun grid-table-set-filters (source ranges)
  "Replace SOURCE's filtered ranges with RANGES.
Return non-nil when SOURCE keeps filtered ranges."
  (when-let* ((function (gethash :set-filters source)))
    (funcall function source ranges)
    t))

(defun grid-table-charts (source)
  "Return the floating charts of SOURCE, chart plists."
  (when-let* ((function (gethash :get-charts source)))
    (funcall function source)))

(defun grid-table-set-charts (source charts)
  "Replace the floating charts of SOURCE with CHARTS.
Return nil when SOURCE keeps no charts."
  (when-let* ((function (gethash :set-charts source)))
    (funcall function source charts)
    t))

(defun grid-table-chart-data (source chart)
  "Return the data floating CHART reads from SOURCE.
It is what `grid-chart-svg-data' returns, for the part of the chart's
range inside the sheet.  A chart's :series-names, one for each series,
name its series instead: nil numbers one, (text TEXT) is its name, and
a range names it with its cells' text, joined by spaces."
  (let* ((range (grid-chart-svg-parse-range (plist-get chart :range)))
         (rows (grid-get-row-count source))
         (cols (grid-get-column-count source))
         (label (lambda (row col)
                  (let ((value (and (< row rows) (< col cols)
                                    (grid-get-display-value-at source row col))))
                    (if (stringp value) (substring-no-properties value)
                      (format "%s" (or value "")))))))
    (if (or (null range) (>= (nth 0 range) rows) (>= (nth 1 range) cols))
        (list :categories nil :series nil)
      (let ((data (grid-chart-svg-data
                   (list (nth 0 range) (nth 1 range)
                         (min (nth 2 range) (1- rows)) (min (nth 3 range) (1- cols)))
                   (lambda (row col) (grid-get-computed-value-at source row col))
                   label
                   (plist-get chart :by) (plist-get chart :type)
                   (plist-get chart :labels) (plist-get chart :names)))
            (names (plist-get chart :series-names)))
        (when (and names (= (length names) (length (plist-get data :series))))
          (setq data (plist-put
                      data :series
                      (cl-loop
                       for (name . values) in (plist-get data :series)
                       for given in names
                       collect
                       (cons (pcase given
                               (`(text ,text) text)
                               ((pred stringp)
                                (pcase (grid-chart-svg-parse-range given)
                                  (`(,top ,left ,bottom ,right)
                                   (mapconcat
                                    #'identity
                                    (cl-loop for row from top to bottom
                                             append (cl-loop for col from left to right
                                                             collect (funcall label row col)))
                                    " "))
                                  (_ name)))
                               (_ name))
                             values)))))
        data))))

(defun grid-table-validations (source)
  "Return SOURCE's validation rules, (TOP LEFT BOTTOM RIGHT KIND SPEC) each."
  (when-let* ((function (gethash :get-validations source)))
    (funcall function source)))

(defun grid-table-set-validations (source rules)
  "Replace SOURCE's validation rules with RULES.
Return non-nil when SOURCE keeps validation rules."
  (when-let* ((function (gethash :set-validations source)))
    (funcall function source rules)
    t))

(defun grid-table-validation-at (source row col)
  "Return the validation rule of the cell at ROW and COL of SOURCE, or nil."
  (when-let* ((model (gethash :model source)))
    (grid-model-validation-at model row col)))

(defun grid-table-validation-options (source rule)
  "Return the values validation RULE of SOURCE allows, as strings.
A formula in the rule is computed on SOURCE's sheet; blanks and errors
in what it gives are left out."
  (let ((spec (nth 5 rule)))
    (if (listp spec)
        spec
      (let* ((workbook (grid-workbook-of source))
             (grid-format-date-system (grid-workbook-date-system-of workbook))
             (grid-calc-sheets (and workbook (grid-workbook--resolver workbook)))
             (grid-calc-names (and workbook
                                   (grid-workbook--names-resolver workbook)))
             (grid-calc-tables (and workbook
                                    (grid-workbook--tables-resolver workbook)))
             (value (condition-case nil
                        (grid-calc-eval-ast (gethash :model source)
                                            (grid-parser-parse (substring spec 1)))
                      (error nil))))
        (delete-dups
         (delq nil
               (mapcar (lambda (item)
                         (let ((text (if (stringp item) item
                                       (grid-format-general item))))
                           (and (not (string-empty-p text))
                                (not (grid-calc-error-text-p text))
                                text)))
                       (if (listp value) value (list value)))))))))

(defun grid-table-validation-problem (source row col text)
  "Return why TEXT may not go in the cell at ROW and COL of SOURCE, or nil.
An empty TEXT is always allowed, as are formulas."
  (when-let* ((rule (grid-table-validation-at source row col)))
    (unless (or (string-empty-p text) (string-prefix-p "=" text))
      (pcase (nth 4 rule)
        ('list
         (let ((options (grid-table-validation-options source rule)))
           (unless (member (downcase text) (mapcar #'downcase options))
             (format "%s is not one of: %s" text
                     (string-join options ", ")))))))))

(defun grid-get-conditional-formats (source)
  "Return SOURCE's conditional format rules.
Each rule is (TOP LEFT BOTTOM RIGHT OP VALUE STYLE)."
  (when-let* ((function (gethash :get-conditional-formats source)))
    (funcall function source)))

(defun grid-set-conditional-formats (source rules)
  "Replace SOURCE's conditional format rules with RULES."
  (when-let* ((function (gethash :set-conditional-formats source)))
    (funcall function source rules)))

(defconst grid-table-conditional-operators
  '((">" . "greater than") ("<" . "less than")
    (">=" . "at least") ("<=" . "at most")
    ("=" . "equal to") ("<>" . "not equal to")
    ("contains" . "text contains")
    ("empty" . "is empty") ("error" . "is an error"))
  "Operators of conditional format rules, with their descriptions.")

(defun grid-table-conditional-match-p (op operand value)
  "Return non-nil when computed VALUE satisfies OP against OPERAND."
  (let ((number (grid-calc--numeric value))
        (target (grid-calc--numeric operand)))
    (pcase op
      (">" (and number target (> number target)))
      ("<" (and number target (< number target)))
      (">=" (and number target (>= number target)))
      ("<=" (and number target (<= number target)))
      ("=" (and (not (member value '(nil "")))
                (grid-calc--equal value operand)))
      ("<>" (not (grid-calc--equal value operand)))
      ("contains" (and (stringp operand) (not (string-empty-p operand))
                       (string-match-p (regexp-quote (downcase operand))
                                       (downcase (format "%s" (or value ""))))))
      ("empty" (member value '(nil "")))
      ("error" (grid-calc-error-text-p value)))))

(defun grid-table-effective-style (source row col &optional rules)
  "Return the style the cell at ROW and COL of SOURCE is shown with.
The cell's own style comes first; every conditional format rule its
value satisfies then overrides the properties it names, later rules
winning.  RULES, when given, saves fetching the rules for every cell."
  (let ((style (grid-get-style-at source row col))
        (rules (or rules (grid-get-conditional-formats source))))
    (when rules
      (let ((value (grid-get-computed-value-at source row col)))
        (dolist (rule rules)
          (when (and (<= (nth 0 rule) row) (<= row (nth 2 rule))
                     (<= (nth 1 rule) col) (<= col (nth 3 rule))
                     (grid-table-conditional-match-p
                      (nth 4 rule) (nth 5 rule) value))
            (cl-loop for (key property) on (nth 6 rule) by #'cddr do
                     (setq style (plist-put style key property)))))))
    style))

(defun grid-unmerge-cell (source row col)
  "Unmerge the range covering ROW and COL in SOURCE."
  (funcall (gethash :unmerge-cell source) source row col))

;;;----------------------------------------------------------------------
;;; Grid Table Configuration
;;;----------------------------------------------------------------------


(defcustom grid-table-title-align 'center
  "Title alignment above the table: left, center, or right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right))
  :group 'grid-table)

(defcustom grid-table-title-decoration 'none
  "Title decoration style above the table. When 'rule, draw a horizontal rule under the title."
  :type '(choice (const :tag "None" none)
                 (const :tag "Rule" rule))
  :group 'grid-table)


(defvar-local grid-table-allow-elisp nil
  "Buffer-local variable to enable elisp evaluation in table cells.

If this is nil, elisp formulas are ignored. Set it to non-nil to enable evaluation of elisp formulas in tables.
Warning: Only set this in trusted files! I am not responsible for any issues, damages, or bug reports caused by using this feature, and will not handle related issues.")
(put 'grid-table-allow-elisp 'risky-local-variable t)


(defcustom grid-table-default-save-directory nil
  "The default directory to save new grid files.
If nil, uses the `default-directory` of the current buffer."
  :type '(choice (const :tag "None (use current directory)" nil)
                 (directory :tag "Specify directory"))
  :group 'grid-table)


(defvar-local grid-table--data-source nil
  "Buffer-local variable to hold the current data source instance.")

(defvar-local grid-table--file-path nil
  "Buffer-local variable to hold the file path of the current grid.")

(defvar-local grid-table--title nil
  "Buffer-local variable to hold the title of the current grid.")


(declare-function grid-table-etaf-show-in-current-buffer "grid-table-etaf")
(declare-function grid-table-etaf-refresh "grid-table-etaf")
(declare-function grid-table-etaf-cell-at-point "grid-table-etaf")
(declare-function grid-table-etaf-edit-cell "grid-table-etaf")
(declare-function grid-table-etaf-set-cell "grid-table-etaf")
(declare-function grid-table-etaf-note-frozen "grid-table-etaf")


;;;----------------------------------------------------------------------
;;; Rendering
;;;----------------------------------------------------------------------

(defun grid-table--require-view ()
  "Load the view that shows grids, or say what it needs."
  (unless (featurep 'grid-table-etaf)
    (condition-case err
        (require 'grid-table-etaf)
      (error
       (user-error "Showing a grid needs ETAF: put the ecss, ebox and etaf \
checkouts from https://github.com/emacs-taf on `load-path' (%s)"
                   (error-message-string err))))))

(define-derived-mode grid-table-mode special-mode "Grid Table"
  "Parent of the major mode that shows a grid.
Grids are shown in `grid-table-etaf-mode', which derives from this mode,
so hooks and checks written for `grid-table-mode' keep working."
  :group 'grid-table)

(defvar-local grid-table--nav-row nil
  "Row of the cursor in the current grid, as a display row.")

(defvar-local grid-table--nav-col nil
  "Column of the cursor in the current grid, as a display column.
Display column 0 is the row numbers; the first column of the grid is 1.")

(declare-function grid-table-etaf-goto "grid-table-etaf")
(declare-function grid-table-etaf-move "grid-table-etaf")
(declare-function grid-table-etaf-move-tab "grid-table-etaf")
(declare-function grid-table-etaf-row-start "grid-table-etaf")
(declare-function grid-table-etaf-row-end "grid-table-etaf")

(defun grid-table-get-cell-at-point ()
  "Return the complete logical descriptor for the cell at point."
  (interactive)
  (grid-table-etaf-cell-at-point))

(defun grid-table--move-to-cell (display-row col)
  "Move the cursor to DISPLAY-ROW and display column COL."
  (grid-table-etaf-goto (max 0 display-row) (max 0 (1- col))))

(defun grid-table-next-line ()
  "Move to the cell in the next row, same column."
  (interactive)
  (grid-table-etaf-move 1 0))

(defun grid-table-previous-line ()
  "Move to the cell in the previous row, same column."
  (interactive)
  (grid-table-etaf-move -1 0))

(defun grid-table-next-cell ()
  "Move to the next cell in reading order."
  (interactive)
  (grid-table-etaf-move-tab 1))

(defun grid-table-previous-cell ()
  "Move to the previous cell in reading order."
  (interactive)
  (grid-table-etaf-move-tab -1))

(defun grid-table-cell-right ()
  "Move to the cell on the right."
  (interactive)
  (grid-table-etaf-move 0 1))

(defun grid-table-cell-left ()
  "Move to the cell on the left."
  (interactive)
  (grid-table-etaf-move 0 -1))

(defun grid-table-move-beginning-of-line ()
  "Move to the first cell of the row."
  (interactive)
  (grid-table-etaf-row-start))

(defun grid-table-move-end-of-line ()
  "Move to the last cell of the row."
  (interactive)
  (grid-table-etaf-row-end))


;;----------------------------------------------------------------------
;; Column Width Calculation
;;----------------------------------------------------------------------


;;----------------------------------------------------------------------
;; Title rendering helpers
;;----------------------------------------------------------------------


;;----------------------------------------------------------------------
;; Draw Row
;;----------------------------------------------------------------------


(defun grid-table--parse-cell-address (address)
  "Parse ADDRESS into zero-based (ROW COL), or return nil."
  (when (string-match "\\`\\([A-Za-z]+\\)\\([1-9][0-9]*\\)\\'" address)
    (list (1- (string-to-number (match-string 2 address)))
          (grid-calc-col-letter-to-number (match-string 1 address)))))

;;;###autoload
(defun grid-table-merge-cells (range)
  "Merge the cells of A1-style RANGE, a row, a column or a rectangle."
  (interactive (list (read-string "Merge range (e.g. B1:D1 or B2:B9): ")))
  (let* ((parts (split-string range ":" t "[[:space:]]*"))
         (start (and (= (length parts) 2)
                     (grid-table--parse-cell-address (car parts))))
         (end (and start
                   (grid-table--parse-cell-address (cadr parts)))))
    (unless (and start end)
      (user-error "Merge range must look like B1:D1"))
    (let* ((top (min (car start) (car end)))
           (left (min (cadr start) (cadr end)))
           (bottom (max (car start) (car end)))
           (right (max (cadr start) (cadr end))))
      (setq-local grid-table--data-source
                  (grid-merge-cells grid-table--data-source
                                    top left bottom right))
      (grid-table-refresh top (1+ left))
      (message "Merged %s." range))))

;;;###autoload
(defun grid-table-unmerge-cell ()
  "Unmerge the merged cell at point."
  (interactive)
  (when-let* ((coords (grid-table-get-cell-at-point))
              (row (plist-get coords :anchor-row))
              (display-col (plist-get coords :anchor-col)))
    (when (or (= row grid-table--col-header-row-idx)
              (= display-col grid-table--row-header-col-idx))
      (user-error "Place point on a data cell"))
    (unless (or (/= (plist-get coords :row-start)
                    (plist-get coords :row-end))
                (/= (plist-get coords :col-start)
                    (plist-get coords :col-end)))
      (user-error "Cell is not merged"))
    (setq-local grid-table--data-source
                (grid-unmerge-cell grid-table--data-source
                                   row (1- display-col)))
    (grid-table-refresh row display-col)
    (message "Cell unmerged.")))

(defun grid-table-edit-cell ()
  "Edit the value of the current cell."
  (interactive)
  (grid-table-etaf-edit-cell))


(defun grid-table-edit-title ()
  "Edit the title of the current grid with alignment and decoration options."
  (interactive)
  (let* ((new-title (read-string (format "Edit Title: %s -> " (or grid-table--title "")) (or grid-table--title "")))
         (new-align (when (not (string-empty-p new-title))
                      (intern (completing-read "Title alignment: " 
                                               '("left" "center" "right") 
                                               nil t 
                                               (symbol-name grid-table-title-align)))))
         (new-decoration (when (not (string-empty-p new-title))
                           (intern (completing-read "Title decoration: " 
                                                    '("none" "rule") 
                                                    nil t 
                                                    (symbol-name grid-table-title-decoration))))))
    (unless (and (string= new-title (or grid-table--title ""))
                 (eq new-align grid-table-title-align)
                 (eq new-decoration grid-table-title-decoration))
      (setq-local grid-table--title new-title)
      ;; The sheet keeps its title, to show it again and write it out.
      (when grid-table--data-source
        (puthash :title new-title grid-table--data-source))
      (when new-align
        (setq-local grid-table-title-align new-align))
      (when new-decoration
        (setq-local grid-table-title-decoration new-decoration))
      (grid-table-refresh)
      (message "Grid title updated (align: %s, decoration: %s)." 
               (or new-align grid-table-title-align) 
               (or new-decoration grid-table-title-decoration)))))

(defun grid-table--read-sort-order (prompt)
  "Read with PROMPT whether to sort ascending or descending."
  (intern (completing-read prompt '("ascending" "descending") nil t
                           nil nil "ascending")))

(defun grid-table-sort-column (&optional more)
  "Sort the rows by the current column, asking for the direction.
With a prefix argument MORE, then ask for more columns, each with its
direction, to order the rows the ones before leave equal; an empty
column ends the list."
  (interactive "P")
  (when grid-table--data-source
    (let* ((source grid-table--data-source)
           (coords (grid-table-get-cell-at-point))
           (current-col (if coords (plist-get coords :anchor-col) 1))
           (col-idx (if (and coords (> current-col 0)) ; Skip row header column
                        (1- current-col) ; Adjust for row header column
                      (read-number "Sort by column (0-based index): " 0)))
           (keys (list (cons col-idx
                             (grid-table--read-sort-order
                              (format "Sort by %s: "
                                      (grid-calc-col-number-to-letter col-idx))))))
           (sort-fn (gethash :sort-by-column source)))
      (when more
        (cl-loop
         for letters = (string-trim
                        (read-string "Then by column (letter, RET to sort): "))
         until (string-empty-p letters) do
         (let ((col (and (string-match-p "\\`[A-Za-z]+\\'" letters)
                         (grid-calc-col-letter-to-number letters))))
           (unless (and col (< col (grid-get-column-count source)))
             (user-error "No column %s" letters))
           (push (cons col (grid-table--read-sort-order
                            (format "Then by %s: " (upcase letters))))
                 keys)))
        (setq keys (nreverse keys))
        (when (and (cdr keys) (null (gethash :model source)))
          (user-error "This grid sorts by one column at a time")))
      (when sort-fn
        (let* ((current-coords (grid-table-get-cell-at-point))
               (restore-row-idx
                (when current-coords
                  (plist-get current-coords :slice-row)))
               (restore-col-idx
                (when current-coords
                  (plist-get current-coords :slice-col)))
               (new-data-source
                (if (cdr keys)
                    (funcall sort-fn source keys nil)
                  (funcall sort-fn source (caar keys) (cdar keys)))))
          (setq-local grid-table--data-source new-data-source)
          (grid-table-refresh restore-row-idx restore-col-idx) ; Force full refresh to display sorted results
          (message "Sorted by %s"
                   (mapconcat (lambda (key)
                                (format "%s (%s)"
                                        (grid-calc-col-number-to-letter (car key))
                                        (cdr key)))
                              keys ", then ")))))))

;;----------------------------------------------------------------------
;; Column Letter Conversion
;;----------------------------------------------------------------------


;;----------------------------------------------------------------------
;; Add/Delete Row/Column
;;----------------------------------------------------------------------

(defun grid-table-add-row (&optional row-idx)
  "Add a new row at ROW-IDX, or at the end if ROW-IDX is nil.
If called interactively, adds a row below the current cell."
  (interactive (list (when-let ((coords (grid-table-get-cell-at-point)))
                       (plist-get coords :slice-row))))
  (when grid-table--data-source
    (cl-block grid-table-add-row
      ;; Validate row-idx before proceeding; -1 is above the first row
      (unless (and row-idx (numberp row-idx) (>= row-idx -1))
        (message "Error: Invalid row index. Please place cursor on a valid table cell.")
        (cl-return-from grid-table-add-row))
      (when (and (= row-idx -1)
                 (grid-table-header-row-p grid-table--data-source))
        (message "Cannot insert above the header row.")
        (cl-return-from grid-table-add-row))
      (let* ((current-coords (grid-table-get-cell-at-point))
             (restore-row-idx
              (when current-coords (plist-get current-coords :slice-row)))
             (restore-col-idx
              (when current-coords (plist-get current-coords :slice-col)))
             (add-row-fn (gethash :add-row grid-table--data-source))
             (data-model-row-idx row-idx) ; row-idx is already correct (0-based data row index)
             (new-data-source (funcall add-row-fn grid-table--data-source data-model-row-idx)))
        (setq-local grid-table--data-source new-data-source)
        (grid-table-refresh restore-row-idx restore-col-idx)
        (message "Row added.")))))

(defun grid-table-delete-row (&optional row-idx)
  "Delete the row at ROW-IDX.
If called interactively, deletes the current row."
  (interactive (list (when-let ((coords (grid-table-get-cell-at-point)))
                       (plist-get coords :slice-row))))
  (when grid-table--data-source
    (cl-block grid-table-delete-row
      ;; Validate row-idx before proceeding
      (unless (and row-idx (numberp row-idx) (>= row-idx 0))
        (message "Error: Invalid row index. Please place cursor on a valid table cell.")
        (cl-return-from grid-table-delete-row))
      ;; Row index 0 is the user-defined header row, not allowed to be deleted
      (when (and (= row-idx 0)
                 (grid-table-header-row-p grid-table--data-source))
        (message "Cannot delete header row.")
        (cl-return-from grid-table-delete-row))
      (let* ((current-coords (grid-table-get-cell-at-point))
             (restore-row-idx
              (when current-coords (plist-get current-coords :slice-row)))
             (restore-col-idx
              (when current-coords (plist-get current-coords :slice-col)))
             ;; Convert display row index to data area (excluding header): data row index = display row index - 1
             (data-model-row-idx (1- row-idx)))
        (condition-case err
            (let* ((delete-row-fn (gethash :delete-row grid-table--data-source))
                   (new-data-source (funcall delete-row-fn grid-table--data-source data-model-row-idx)))
              (setq-local grid-table--data-source new-data-source)
              ;; After deletion, calculate a safe recovery position to avoid the cursor falling outside the table (especially when deleting the last row)
              (let* ((get-row-count-fn (gethash :get-row-count grid-table--data-source))
                     (get-col-count-fn (gethash :get-column-count grid-table--data-source))
                     (new-num-rows (funcall get-row-count-fn grid-table--data-source))
                     (new-num-cols (funcall get-col-count-fn grid-table--data-source))
                     ;; Display layer row index range: [0 .. new-num-rows-1]
                     (safe-row (max 0 (min (or restore-row-idx 0) (1- new-num-rows))))
                     ;; Display layer column index range: [0 .. new-num-cols] (including row header column 0)
                     (safe-col (max 0 (min (or restore-col-idx 0) new-num-cols))))
                (grid-table-refresh safe-row safe-col))
              (message "Row deleted."))
          (error (message "Error deleting row: %s" (error-message-string err))))))))

(defun grid-table-add-column (&optional col-idx)
  "Add a new column at COL-IDX, or at the end if COL-IDX is nil.
If called interactively, adds a column to the right of the current cell."
  (interactive (list (when-let ((coords (grid-table-get-cell-at-point)))
                       (plist-get coords :slice-col))))
  (when grid-table--data-source
    (cl-block grid-table-add-column
      ;; Validate col-idx before proceeding
      (unless (and col-idx (numberp col-idx) (>= col-idx 0))
        (message "Error: Invalid column index. Please place cursor on a valid table cell.")
        (cl-return-from grid-table-add-column))
      (let* ((current-coords (grid-table-get-cell-at-point))
             (restore-row-idx
              (when current-coords (plist-get current-coords :slice-row)))
             (restore-col-idx
              (when current-coords (plist-get current-coords :slice-col)))
             ;; To insert to the right of the current column, the new column should appear after the current data column.
             ;; grid-model-add-column inserts the new column before the given index (left).
             ;; Therefore we pass the display index (including header 0), which is equivalent to (data-col-index + 1).
             ;; This ensures insertion to the right of the current data column.
             (insertion-col-idx col-idx))
        (let* ((add-column-fn (gethash :add-column grid-table--data-source))
               (new-data-source (funcall add-column-fn grid-table--data-source insertion-col-idx)))
          (setq-local grid-table--data-source new-data-source)
          (grid-table-refresh restore-row-idx restore-col-idx)
          (message "Column added."))))))

(defun grid-table-delete-column (&optional col-idx)
  "Delete the column at COL-IDX.
If called interactively, deletes the current column."
  (interactive (list (when-let ((coords (grid-table-get-cell-at-point)))
                       (plist-get coords :slice-col))))
  (when grid-table--data-source
    (cl-block grid-table-delete-column
      ;; Validate col-idx before proceeding
      (unless (and col-idx (numberp col-idx) (>= col-idx 0))
        (message "Error: Invalid column index. Please place cursor on a valid table cell.")
        (cl-return-from grid-table-delete-column))
      (let* ((current-coords (grid-table-get-cell-at-point))
             (restore-row-idx
              (when current-coords (plist-get current-coords :slice-row)))
             (restore-col-idx
              (when current-coords (plist-get current-coords :slice-col)))
             (data-model-col-idx (1- col-idx))) ; display col 1 = data col 0, display col 2 = data col 1, etc.
        (condition-case err
            (let* ((delete-column-fn (gethash :delete-column grid-table--data-source))
                   (new-data-source (funcall delete-column-fn grid-table--data-source data-model-col-idx)))
              (setq-local grid-table--data-source new-data-source)
              (grid-table-refresh restore-row-idx restore-col-idx)
              (message "Column deleted."))
          (error (message "Error deleting column: %s" (error-message-string err))))))))

(defun grid-table-insert-row-above ()
  "Insert a new row above the current row.
In a sheet with a header row, nothing goes above that row."
  (interactive)
  (when-let ((coords (grid-table-get-cell-at-point)))
    (grid-table-add-row (1- (plist-get coords :slice-row)))))

(defun grid-table-insert-column-left ()
  "Insert a new column to the left of the current column."
  (interactive)
  (when-let ((coords (grid-table-get-cell-at-point)))
    ;; To insert to the left of the current column, reuse the implementation of "insert to the right":
    ;; Pass (display-col-idx - 1), which is equivalent to the left of the current column.
    (let* ((display-col (plist-get coords :slice-col))
           (target (max 0 (1- display-col))))
      (grid-table-add-column target))))

;;----------------------------------------------------------------------
;; Save/Load
;;----------------------------------------------------------------------

(defun grid-table-write-file ()
  "Save the current grid to its associated file, or prompt for a new one."
  (interactive)
  (let ((file-path (or (buffer-file-name) ; Use existing path if available
                       (let* ((default-dir (or grid-table-default-save-directory default-directory))
                              (prompt (format "Save Grid in %s as: " (file-name-as-directory default-dir))))
                         (read-file-name prompt default-dir nil nil)))))
    (when file-path
      ;; If user provides a relative path, make it absolute based on the default dir
      (unless (file-name-absolute-p file-path)
        (setq file-path (expand-file-name file-path (or grid-table-default-save-directory default-directory))))
      ;; Add .grid extension if not present
      (when (not (string-equal ".grid" (file-name-extension file-path t)))
        (setq file-path (concat file-path ".grid")))
      ;; Update the title in the data source before saving
      (puthash :title grid-table--title grid-table--data-source)
      (grid-table-persistence--save-to-file grid-table--data-source file-path)
      (setq-local grid-table--file-path file-path)
      (unless (and buffer-file-name
                   (file-equal-p buffer-file-name file-path))
        (set-visited-file-name file-path))
      ;; Saved here rather than by `save-buffer', which would find the
      ;; file changed since it was visited.
      (set-visited-file-modtime)
      (set-buffer-modified-p nil)
      (message "Grid saved to %s" file-path))))

(defun grid-table-find-file (file-path)
  "Open a grid from a file.
The buffer visits FILE-PATH, so `save-buffer' saves the grid there."
  (interactive "fFind grid file: ")
  (let ((buffer (find-file-noselect file-path)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'grid-table-mode)
        (grid-table-visit)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun grid-table-visit ()
  "Show the grid file the current buffer visits as a grid.
`find-file' calls this for a .grid file; `save-buffer' then saves the
grid to the file.  A file that does not exist yet starts as an empty
grid."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer visits no file"))
  (let ((data-source
         (if (file-exists-p buffer-file-name)
             (grid-table-persistence-create-data-source-from-file buffer-file-name)
           ;; A new file starts as an empty grid, saved on the first save.
           (make-default-data-source (make-list 4 "") (make-list 4 (make-list 4 ""))
                                     (file-name-base buffer-file-name)))))
    (unless data-source
      (user-error "Failed to load grid from %s" buffer-file-name))
    (setq-local grid-table--file-path buffer-file-name)
    (setq-local grid-table--title (gethash :title data-source))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (grid-table--show-grid-in-current-buffer data-source)
    (set-buffer-modified-p nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grid\\'" . grid-table-visit))

;;;###autoload
(defun grid-open (file-path)
  "Open a .grid file directly in grid-table view."
  (interactive "fOpen grid file: ")
  (grid-table-find-file file-path))


;;----------------------------------------------------------------------
;; Main Functions & Mode Definition
;;----------------------------------------------------------------------


(defun grid-table--show-grid-in-current-buffer (data-source &optional title)
  "Display the grid table in the current buffer.

DATA-SOURCE is a hash table containing the data source functions.
TITLE is an optional title for the grid."
  (grid-table--require-view)
  (grid-table-etaf-show-in-current-buffer data-source title))

;;----------------------------------------------------------------------
;; Sheets
;;----------------------------------------------------------------------

(defun grid-table--workbook ()
  "Return the workbook of the current grid, making one when it stands alone."
  (unless grid-table--data-source
    (user-error "No grid in this buffer"))
  (grid-workbook-ensure grid-table--data-source))

(defun grid-table-sheets ()
  "Return the sheets of the current grid's workbook, or nil without one.
A grid that stands alone has no workbook until a sheet is added."
  (when-let* ((workbook (and grid-table--data-source
                             (grid-workbook-of grid-table--data-source))))
    (grid-workbook-sheets workbook)))

(defun grid-table-switch-to-sheet (source)
  "Show SOURCE, a sheet of the current grid's workbook, in this buffer."
  (unless (eq source grid-table--data-source)
    (let ((modified (buffer-modified-p)))
      (setq grid-table--title (gethash :title source))
      (grid-table--show-grid-in-current-buffer source)
      (set-buffer-modified-p modified))))

(defun grid-table--sheet-changed (source)
  "Show SOURCE after the workbook's sheets changed, and mark the buffer changed."
  (let ((current grid-table--data-source))
    (setq grid-table--title (gethash :title source))
    (if (eq source current)
        (grid-table-refresh)
      (grid-table--show-grid-in-current-buffer source))
    (set-buffer-modified-p t)))

(defun grid-table--read-sheet-name (prompt workbook &optional except)
  "Read with PROMPT a name a sheet of WORKBOOK can take.
EXCEPT is a sheet allowed to have that name already."
  (let* ((default (if except
                      (grid-workbook-sheet-name except)
                    (grid-workbook-fresh-name workbook)))
         (name (string-trim
                (read-string (format "%s (default %s): " prompt default)
                             nil nil default))))
    (when-let* ((problem (grid-workbook-name-problem workbook name except)))
      (user-error "%s" problem))
    name))

(defun grid-table-sheet-step (count)
  "Show the sheet COUNT places after the current one, wrapping around."
  (let* ((sheets (grid-table-sheets))
         (index (cl-position grid-table--data-source sheets)))
    (if (null (cdr sheets))
        (message "This grid has one sheet; C-c w n adds another")
      (grid-table-switch-to-sheet
       (nth (mod (+ index count) (length sheets)) sheets)))))

(defun grid-table-sheet-next ()
  "Show the next sheet of the workbook."
  (interactive)
  (grid-table-sheet-step 1))

(defun grid-table-sheet-previous ()
  "Show the previous sheet of the workbook."
  (interactive)
  (grid-table-sheet-step -1))

(defun grid-table-sheet-select (name)
  "Show the sheet called NAME."
  (interactive
   (let ((names (mapcar #'grid-workbook-sheet-name (grid-table-sheets))))
     (unless (cdr names)
       (user-error "This grid has one sheet; C-c w n adds another"))
     (list (completing-read "Sheet: " names nil t))))
  (grid-table-switch-to-sheet
   (or (grid-workbook-sheet (grid-table--workbook) name)
       (user-error "No sheet called %s" name))))

(defun grid-table-sheet-new (name)
  "Add an empty sheet called NAME after the current one and show it."
  (interactive
   (list (grid-table--read-sheet-name "New sheet" (grid-table--workbook))))
  (let* ((workbook (grid-table--workbook))
         (index (cl-position grid-table--data-source
                             (grid-workbook-sheets workbook))))
    (when-let* ((problem (grid-workbook-name-problem workbook name)))
      (user-error "%s" problem))
    (grid-table--sheet-changed
     (grid-workbook-new-sheet workbook name (1+ index)))))

(defun grid-table-toggle-header-row ()
  "Say whether the first row of the current sheet holds column titles.
With a header row, which every sheet starts with, the first row stays in
place when the sheet is sorted or filtered and cannot be deleted.
Without one the first row is a row like any other, for a sheet that is
not a single table."
  (interactive)
  (let ((source (or grid-table--data-source
                    (user-error "No grid in this buffer"))))
    (grid-table-set-header-row source (not (grid-table-header-row-p source)))
    (grid-table-refresh)
    (message (if (grid-table-header-row-p source)
                 "The first row is the header row again"
               "The first row is now a row like any other"))))

(defun grid-table-unfreeze-panes ()
  "Let every row and column of the current sheet scroll again."
  (interactive)
  (let* ((source (or grid-table--data-source
                     (user-error "No grid in this buffer")))
         (old (grid-table-frozen source)))
    (unless (grid-table-set-frozen source 0 0)
      (user-error "This grid cannot keep rows and columns in place"))
    (grid-table-etaf-note-frozen old)
    (grid-table-refresh)
    (message "No row or column is frozen")))

(defun grid-table-sheet-rename (name)
  "Call the current sheet NAME.
Formulas that read the sheet are rewritten to use the new name."
  (interactive
   (list (grid-table--read-sheet-name "Rename sheet to" (grid-table--workbook)
                                      grid-table--data-source)))
  (let ((workbook (grid-table--workbook)))
    (when-let* ((problem (grid-workbook-name-problem
                          workbook name grid-table--data-source)))
      (user-error "%s" problem))
    (grid-workbook-rename-sheet workbook grid-table--data-source name)
    (grid-table--sheet-changed grid-table--data-source)))

(defun grid-table-sheet-delete ()
  "Delete the current sheet.  This cannot be undone.
Formulas that read the sheet show #REF! afterwards."
  (interactive)
  (let* ((workbook (grid-table--workbook))
         (sheets (grid-workbook-sheets workbook))
         (source grid-table--data-source)
         (index (cl-position source sheets)))
    (unless (cdr sheets)
      (user-error "A grid keeps at least one sheet"))
    (when (or (not (called-interactively-p 'interactive))
              (yes-or-no-p (format "Delete sheet %s? This cannot be undone. "
                                   (grid-workbook-sheet-name source))))
      (grid-workbook-remove-sheet workbook source)
      (grid-table--sheet-changed
       (nth (min index (- (length sheets) 2))
            (grid-workbook-sheets workbook))))))

(defun grid-table-sheet-move (delta)
  "Move the current sheet DELTA places among the sheets."
  (let ((workbook (grid-table--workbook)))
    (grid-workbook-move-sheet workbook grid-table--data-source delta)
    (grid-table--sheet-changed grid-table--data-source)))

(defun grid-table-sheet-move-left ()
  "Move the current sheet one place towards the first."
  (interactive)
  (grid-table-sheet-move -1))

(defun grid-table-sheet-move-right ()
  "Move the current sheet one place towards the last."
  (interactive)
  (grid-table-sheet-move 1))


(defun grid-table--show-grid (data-source &optional buffer-name)
  "Show a grid view in a dedicated buffer using DATA-SOURCE."
  (let ((buffer (get-buffer-create (or buffer-name "*Grid Table*"))))
    (with-current-buffer buffer
      (grid-table--show-grid-in-current-buffer data-source))
    (switch-to-buffer buffer)))

(defun grid-table-refresh (&optional restore-row-idx restore-col-idx)
  "Refresh the table, restoring point to RESTORE-ROW-IDX and RESTORE-COL-IDX."
  (interactive)
  (grid-table-etaf-refresh restore-row-idx restore-col-idx))

(defun grid-table-recalculate-all ()
  "Compute every formula of the grid anew and show the result.
Edits only recompute the cells they reach; this is for whatever they
cannot see, such as a formula function defined since."
  (interactive)
  (let ((source (or grid-table--data-source
                    (user-error "No grid in this buffer"))))
    (when (gethash :model source)
      (grid-calc-calc-forget)
      (grid-table-recalculate source t))
    (grid-table-refresh)))


(defun grid-table--chart-type-defaults (type)
  "Return default parameters for chart TYPE."
  (pcase type
    ('bar '(:width 50 :height 10 :grid nil))
    ('column '(:width 40 :height 15 :grid nil))
    ('line '(:width 60 :height 20 :grid t))
    ('scatter '(:width 60 :height 20 :grid t))
    ('histogram '(:width 50 :height 10 :grid nil))
    ('boxplot '(:width 50 :height 8 :grid nil))
    ('multiline '(:width 70 :height 25 :grid t))
    ('density '(:width 50 :height 20 :grid nil))
    ('count '(:width 50 :height 10 :grid nil))
    (_ '(:width 60 :height 20 :grid t))))

(defun grid-table-chart-content (chart)
  "Return the text drawing of CHART, the value of a chart formula."
  (let* ((type (plist-get chart :type))
         (data (plist-get chart :data))
         (options (plist-get chart :options))
         (defaults (grid-table--chart-type-defaults type))
         (params (grid-chart--options-to-params options defaults)))
    (cond
     ((eq type 'bar)
      (grid-chart-barplot (plist-get data :labels)
                          (plist-get data :values)
                          params))
     ((eq type 'column)
      (grid-chart-columnplot (plist-get data :labels)
                             (plist-get data :values)
                             params))
     ((eq type 'line)
      (grid-chart-lineplot (plist-get data :x)
                           (plist-get data :y)
                           params
                           'line))
     ((eq type 'scatter)
      (grid-chart-lineplot (plist-get data :x)
                           (plist-get data :y)
                           params
                           'scatter))
     ((eq type 'histogram)
      (grid-chart-histogram (plist-get data :values)
                            params))
     ((eq type 'boxplot)
      (grid-chart-boxplot (plist-get data :series)
                          params))
     ((eq type 'multiline)
      (grid-chart-multiline (plist-get data :x)
                            (plist-get data :series)
                            params))
     ((eq type 'density)
      (grid-chart-density (plist-get data :data)
                          nil  ; unused parameter for API compatibility
                          params))
     ((eq type 'count)
      (grid-chart-count (plist-get data :values)
                        params))
     (t (format "Unknown chart type: %s" type)))))

(defun grid-table-view-chart ()
  "View the chart in the current cell in a separate buffer."
  (interactive)
  (when-let* ((coords (grid-table-get-cell-at-point))
              (row (plist-get coords :anchor-row))
              (col (plist-get coords :anchor-col))
              ;; Adjust col for data source (0-based data cols start at display col 1)
              (data-col (1- col))
              (val (grid-get-computed-value-at grid-table--data-source row data-col)))
    (if (and (listp val) (plist-get val :grid-chart))
        (let ((chart-content (grid-table-chart-content val)))
          (with-current-buffer (get-buffer-create "*Grid Plot*")
            (read-only-mode -1)
            (erase-buffer)
            (insert chart-content)
            (goto-char (point-min))
            (special-mode)
            (display-buffer (current-buffer))))
      (message "No chart found in this cell."))))

(defun grid-table-insert-chart ()
  "Insert a chart formula into the current cell."
  (interactive)
  (let* ((type (completing-read "Chart Type: " '("BARPLOT" "LINEPLOT" "SCATTER")))
         (formula
          (cond
           ((string= type "BARPLOT")
            (let ((labels (read-string "Labels Range (e.g., A1:A5): "))
                  (values (read-string "Values Range (e.g., B1:B5): "))
                  (title (read-string "Title: ")))
              (format "=BARPLOT(%s, %s, \"%s\")" labels values title)))
           ((or (string= type "LINEPLOT") (string= type "SCATTER"))
            (let ((x (read-string "X Range (e.g., A1:A10): "))
                  (y (read-string "Y Range (e.g., B1:B10): "))
                  (title (read-string "Title: ")))
              (format "=%s(%s, %s, \"%s\")" type x y title))))))
    (when formula
      (grid-table-edit-cell-with-value formula))))

(defun grid-table-edit-cell-with-value (value)
  "Helper to set cell value directly."
  (when-let* ((coords (grid-table-get-cell-at-point))
              (display-row-idx (plist-get coords :anchor-row))
              (display-col-idx (plist-get coords :anchor-col)))
    ;; The view records the change for undo.
    (grid-table-etaf-set-cell display-row-idx (1- display-col-idx) value)))

(defun grid-table--open-demo ()
  "Open a grid-table buffer with a demonstration table for internal testing."
  (interactive)
  (let* ((headers '("Item" "Quantity" "Unit Price" "Total" "Notes"))
         (rows
          '(("Laptop" "2" "1200" "=B1*C1" "For new hires")
            ("Monitor" "2" "350" "=B2*C2" "*Urgent* requirement")
            ("Keyboard" "5" "75.50" "=B4*C4" "Mechanical keyboards")
            ("Subtotal" "" "" "=SUM(D1:D3)" "Auto-calculated")
            ("Tax (8%)" "" "" "=D4*0.08" "")
            ("Grand Total" "" "" "=D4+D5" "Final Price")
            ("" "" "" "" "/Users/chenyibin/Pictures/GEdZiU0WUAAoaSj.jpg")
            ("Stats" "Count:" "=COUNT(B1:B3)" "Max Price:" "=MAX(C1:C3)")))
         (data-source (make-default-data-source headers rows "Demo Grid Table")))
    (setq-local grid-table--title "Demo Grid Table") ; Set default title for demo
    (grid-table--show-grid data-source)))

;;;###autoload
(defun grid-table-create (headers rows)
  "Create and display a new grid view from raw data.
This is the simple, user-facing API.
HEADERS is a list of strings (now the first data row).
ROWS is a list of lists of strings."
  (interactive
   (list (list "Column 1" "Column 2" "Column 3" "Column 4") ; Default 4 user-defined headers (now first data row)
         (list (list "" "" "" "") ; Default 4x4 empty data (excluding user-defined headers)
               (list "" "" "" "")
               (list "" "" "" "")
               (list "" "" "" ""))))
  (let ((data-source (make-default-data-source headers rows "New Grid Table"))) ; headers are now the first data row
    (setq-local grid-table--title "New Grid Table") ; Set default title for new grids
    (grid-table--show-grid data-source (generate-new-buffer-name "*grid*"))))


(provide 'grid-table)

;;; grid-table.el ends here 
