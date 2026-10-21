;;; exc.el --- A spreadsheet inside Emacs -*- lexical-binding: t -*-
;; Version: 0.6.0
;; Package-Requires: ((emacs "29.1"))

(require 'cl-lib)
(require 'cl-macs)

(when-let* ((library-file (or load-file-name buffer-file-name))
            (plugin-directory
             (expand-file-name "plugins" (file-name-directory library-file)))
            ((file-directory-p plugin-directory)))
  (add-to-list 'load-path plugin-directory))

(require 'exc-data-source)
(require 'exc-data-model)
(require 'exc-parser)
(require 'exc-format)
(require 'exc-persistence)
(require 'exc-text)
(require 'exc-chart)
(require 'exc-chart-svg)
(require 'exc-plugins)
(require 'exc-obsolete)

(defgroup exc nil
  "A spreadsheet inside Emacs."
  :group 'applications
  :prefix "exc-")

;; Public version identifier.
(defconst exc-version "0.6.0")

;;;----------------------------------------------------------------------
;;; Helper functions for data source access
;;;----------------------------------------------------------------------

(defun exc-get-column-count (source)
  "Get column count from data source."
  (funcall (gethash :get-column-count source) source))

(defun exc-get-row-count (source)
  "Get row count from data source."
  (funcall (gethash :get-row-count source) source))

(defun exc-get-header-value (source col)
  "Get header value from data source."
  (funcall (gethash :get-header-value source) source col))

(defun exc-outside-p (source row col)
  "Return non-nil when ROW and COL lie outside the cells SOURCE has.
Such a cell is one of the empty cells past a sheet's last row or column,
where a spreadsheet's cursor may go: it reads as empty."
  (or (< row 0) (< col 0)
      (>= row (exc-get-row-count source))
      (>= col (exc-get-column-count source))))

(defun exc-get-raw-value-at (source row col)
  "Get raw value from data source.
A cell outside the sheet's cells is empty, \"\"."
  (if (exc-outside-p source row col)
      ""
    (funcall (gethash :get-raw-value-at source) source row col)))

(defun exc-get-computed-value-at (source row col)
  "Get computed value from data source.
A cell outside the sheet's cells is empty, \"\"."
  (if (exc-outside-p source row col)
      ""
    (funcall (gethash :get-computed-value-at source) source row col)))

(defun exc-set-raw-value-at (source row col value)
  "Set raw value in data source."
  (funcall (gethash :set-raw-value-at source) source row col value))

(defun exc-get-merges (source)
  "Return merge ranges from SOURCE."
  (when-let* ((fn (gethash :get-merges source)))
    (funcall fn source)))

(defun exc-source-merge-cells (source top left bottom &optional right)
  "Merge TOP LEFT through BOTTOM RIGHT in SOURCE.
When RIGHT is nil, use the legacy horizontal TOP LEFT BOTTOM call."
  (funcall (gethash :merge-cells source)
           source top left bottom right))

(defun exc-get-style-at (source row col)
  "Return the style plist of the cell at ROW and COL in SOURCE.
Sources that do not store styles give nil, as does a cell outside the
sheet's cells."
  (when-let* ((function (gethash :get-style-at source))
              ((not (exc-outside-p source row col))))
    (funcall function source row col)))

(defun exc-set-style-at (source row col style)
  "Give the cell at ROW and COL in SOURCE the plist STYLE; nil clears it."
  (when-let* ((function (gethash :set-style-at source)))
    (funcall function source row col style)))

(defcustom exc-currency-symbol "$"
  "Currency symbol the currency format is offered with."
  :type 'string
  :group 'exc)

(defconst exc--date-formula-regexp
  "\\`=[ \t]*\\(TODAY\\|DATE\\|DATEVALUE\\|EDATE\\|EOMONTH\\|NOW\\)[ \t]*("
  "Matches a formula that starts by computing a date.")

(defun exc-cell-format (source row col)
  "Return the format the cell at ROW and COL of SOURCE is shown with.
That is the format of its style.  A cell without one whose formula
starts by computing a date, such as =TODAY()+7, is shown as a date."
  (let ((own (plist-get (exc-get-style-at source row col) :format)))
    (cond
     ;; The general format asked for by name: not even a date formula
     ;; is shown as a date.
     ((eq (car-safe own) 'general) nil)
     (own)
     (t (let ((raw (and (gethash :get-raw-value-at source)
                        (exc-get-raw-value-at source row col)))
              (case-fold-search t))
          (and (stringp raw)
               (string-match exc--date-formula-regexp raw)
               (list 'date (if (string= (upcase (match-string 1 raw)) "NOW")
                               "yyyy-mm-dd hh:mm"
                             "yyyy-mm-dd"))))))))

(defun exc-false-value-p (source row col value)
  "Return non-nil when VALUE, computed at ROW and COL of SOURCE, is FALSE.
A formula that comes out false computes nil, as an empty cell does not;
so does a cell showing a FALSE that a formula spills."
  (and (null value)
       (gethash :get-raw-value-at source)
       (let ((raw (exc-get-raw-value-at source row col))
             (model (gethash :model source)))
         (or (and (stringp raw) (string-prefix-p "=" raw))
             (and model (exc-calc-spilled-p model row col))))))

;; Dates shown and typed in a grid count as its workbook's do.
(setq exc-format-date-system-function
      (lambda ()
        (and (boundp 'exc--data-source)
             (exc-workbook-source-date-system exc--data-source))))

(defun exc-get-display-value-at (source row col)
  "Return what the cell at ROW and COL of SOURCE shows.
That is its computed value written after its format.  A value that is
not plain data, such as a chart, is returned as it is."
  (let ((value (exc-get-computed-value-at source row col)))
    (cond ((exc-false-value-p source row col value) "FALSE")
          ((or (stringp value) (numberp value) (memq value '(nil t)))
           (exc-format-display value (exc-cell-format source row col)))
          (t value))))

(defun exc-edit-text (source row col)
  "Return the text an edit of the cell at ROW and COL of SOURCE starts from.
A date and a percentage are edited as they read, not as the number
stored for them."
  (exc-format-edit-text (or (exc-get-raw-value-at source row col) "")
                         (plist-get (exc-get-style-at source row col)
                                    :format)))

(defun exc--reads-as-other-than-text-p (text)
  "Return non-nil when TEXT, stored as it is, is not plain text.
That is a formula, a number, a date, a truth value, an error value or
text that starts with the apostrophe that marks text."
  (or (string-prefix-p "=" text)
      (string-prefix-p "'" text)
      (exc-format-number text)
      (exc-format-parse-entry text)
      (member (upcase (string-trim text)) '("TRUE" "FALSE"))
      (member text exc-calc-error-values)))

(defun exc-read-entry (source row col text)
  "Read TEXT as typed into the cell at ROW and COL of SOURCE.
Return (RAW . STYLE): RAW is the text to store and STYLE the style to
give the cell, or the symbol `keep' to leave its style alone.  A date
written year first, a percentage, an amount of money and a number with
thousands separators are stored as numbers; a cell without a format
takes the one that shows the number as it was typed.  Sources that keep
no styles store TEXT as it is.  In a cell with the text format TEXT is
text: it is stored after an apostrophe when it would read otherwise, so
the number already in such a cell stays a number, as in a spreadsheet."
  (let* ((style (exc-get-style-at source row col))
         (format (plist-get style :format))
         (typed (and (gethash :set-style-at source)
                     (not (eq (car-safe format) 'text))
                     (or (exc-format-parse-entry text)
                         ;; A bare number typed into a percentage is that
                         ;; many percent.
                         (and (eq (car-safe format) 'percent)
                              (exc-format-number text)
                              (exc-format-parse-entry
                               (concat (string-trim text) "%")))))))
    (cond ((and (eq (car-safe format) 'text)
                (exc--reads-as-other-than-text-p text))
           (cons (concat "'" text) 'keep))
          ((null typed) (cons text 'keep))
          (format (cons (car typed) 'keep))
          (t (cons (car typed)
                   (plist-put (copy-sequence style) :format (cdr typed)))))))

(defun exc-enter-value (source row col text)
  "Store TEXT in the cell at ROW and COL of SOURCE as a user types it.
See `exc-read-entry' for how dates and percentages are read."
  (let ((entry (exc-read-entry source row col text)))
    (unless (eq (cdr entry) 'keep)
      (exc-set-style-at source row col (cdr entry)))
    (exc-set-raw-value-at source row col (car entry))))

(defun exc-recalculate (source &optional everything)
  "Bring the formulas of SOURCE, and of the sheets it shares a book with, up to date.
Only what changed and what reads it is computed, unless EVERYTHING is
non-nil."
  (let ((workbook (exc-workbook-of source))
        (model (gethash :model source)))
    (cond (workbook (exc-workbook-recalculate workbook everything))
          (model (exc-calc-evaluate model everything)))))

(defun exc-header-row-p (source)
  "Return non-nil when the first row of SOURCE holds its column titles.
That row then stays in place when the sheet is sorted or filtered and
cannot be deleted.  A sheet without a header row treats its first row as
any other."
  (not (gethash :no-header-row source)))

(defun exc-set-header-row (source header)
  "Say whether the first row of SOURCE holds its column titles: HEADER."
  (when (and header (not (exc-header-row-p source)))
    (when (cl-some (lambda (merge)
                     (and (= (nth 0 merge) 0) (< (nth 0 merge) (nth 2 merge))))
                   (exc-get-merges source))
      (user-error "A merge reaches down from the first row; unmerge it first")))
  (if header
      (remhash :no-header-row source)
    (puthash :no-header-row t source))
  header)

(defun exc-get-column-widths (source)
  "Return SOURCE's table of column widths set by hand, or nil.
It maps a column index to a width in characters."
  (when-let* ((function (gethash :get-column-widths source)))
    (funcall function source)))

(defun exc-set-column-width (source col width)
  "Give column COL of SOURCE a width of WIDTH characters; nil forgets it.
Return non-nil when SOURCE keeps column widths."
  (when-let* ((function (gethash :set-column-width source)))
    (funcall function source col width)
    t))

(defun exc-frozen (source)
  "Return (ROWS . COLS): the leading rows and columns of SOURCE kept in place.
They stay at the top and the left of the window while the rest scrolls."
  (if-let* ((function (gethash :get-frozen source)))
      (funcall function source)
    (cons 0 0)))

(defun exc-set-frozen (source rows cols)
  "Keep the first ROWS rows and COLS columns of SOURCE in place.
Return non-nil when SOURCE can keep rows and columns in place."
  (when-let* ((function (gethash :set-frozen source)))
    (funcall function source rows cols)
    t))

(defun exc-hidden (source)
  "Return (ROWS . COLS): the rows and columns of SOURCE hidden, sorted lists."
  (if-let* ((function (gethash :get-hidden source)))
      (funcall function source)
    (cons nil nil)))

(defun exc-set-hidden (source rows cols)
  "Hide the ROWS and COLS of SOURCE, lists of indexes; nil shows them all.
Return non-nil when SOURCE can hide rows and columns."
  (when-let* ((function (gethash :set-hidden source)))
    (funcall function source rows cols)
    t))

(defun exc-filters (source)
  "Return SOURCE's filtered ranges, (TOP LEFT BOTTOM RIGHT) each.
TOP is a range's header row; its hidden rows below are filtered out."
  (when-let* ((function (gethash :get-filters source)))
    (funcall function source)))

(defun exc-set-filters (source ranges)
  "Replace SOURCE's filtered ranges with RANGES.
Return non-nil when SOURCE keeps filtered ranges."
  (when-let* ((function (gethash :set-filters source)))
    (funcall function source ranges)
    t))

(defun exc-charts (source)
  "Return the floating charts of SOURCE, chart plists."
  (when-let* ((function (gethash :get-charts source)))
    (funcall function source)))

(defun exc-set-charts (source charts)
  "Replace the floating charts of SOURCE with CHARTS.
Return nil when SOURCE keeps no charts."
  (when-let* ((function (gethash :set-charts source)))
    (funcall function source charts)
    t))

(defun exc-chart-data (source chart)
  "Return the data floating CHART reads from SOURCE.
It is what `exc-chart-svg-data' returns, for the part of the chart's
range inside the sheet.  A chart's :series-names, one for each series,
name its series instead: nil numbers one, (text TEXT) is its name, and
a range names it with its cells' text, joined by spaces."
  (let* ((range (exc-chart-svg-parse-range (plist-get chart :range)))
         (rows (exc-get-row-count source))
         (cols (exc-get-column-count source))
         (label (lambda (row col)
                  (let ((value (and (< row rows) (< col cols)
                                    (exc-get-display-value-at source row col))))
                    (if (stringp value) (substring-no-properties value)
                      (format "%s" (or value "")))))))
    (if (or (null range) (>= (nth 0 range) rows) (>= (nth 1 range) cols))
        (list :categories nil :series nil)
      (let ((data (exc-chart-svg-data
                   (list (nth 0 range) (nth 1 range)
                         (min (nth 2 range) (1- rows)) (min (nth 3 range) (1- cols)))
                   (lambda (row col) (exc-get-computed-value-at source row col))
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
                                (pcase (exc-chart-svg-parse-range given)
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

(defun exc-validations (source)
  "Return SOURCE's validation rules, (TOP LEFT BOTTOM RIGHT KIND SPEC) each."
  (when-let* ((function (gethash :get-validations source)))
    (funcall function source)))

(defun exc-set-validations (source rules)
  "Replace SOURCE's validation rules with RULES.
Return non-nil when SOURCE keeps validation rules."
  (when-let* ((function (gethash :set-validations source)))
    (funcall function source rules)
    t))

(defun exc-validation-at (source row col)
  "Return the validation rule of the cell at ROW and COL of SOURCE, or nil."
  (when-let* ((model (gethash :model source)))
    (exc-model-validation-at model row col)))

(defun exc-validation-options (source rule)
  "Return the values validation RULE of SOURCE allows, as strings.
A formula in the rule is computed on SOURCE's sheet; blanks and errors
in what it gives are left out."
  (let ((spec (nth 5 rule)))
    (if (listp spec)
        spec
      (let* ((workbook (exc-workbook-of source))
             (exc-format-date-system (exc-workbook-date-system-of workbook))
             (exc-calc-sheets (and workbook (exc-workbook--resolver workbook)))
             (exc-calc-names (and workbook
                                   (exc-workbook--names-resolver workbook)))
             (exc-calc-tables (and workbook
                                    (exc-workbook--tables-resolver workbook)))
             (value (condition-case nil
                        (exc-calc-eval-ast (gethash :model source)
                                            (exc-parser-parse (substring spec 1)))
                      (error nil))))
        (delete-dups
         (delq nil
               (mapcar (lambda (item)
                         (let ((text (if (stringp item) item
                                       (exc-format-general item))))
                           (and (not (string-empty-p text))
                                (not (exc-calc-error-text-p text))
                                text)))
                       (if (listp value) value (list value)))))))))

(defun exc-validation-problem (source row col text)
  "Return why TEXT may not go in the cell at ROW and COL of SOURCE, or nil.
An empty TEXT is always allowed, as are formulas."
  (when-let* ((rule (exc-validation-at source row col)))
    (unless (or (string-empty-p text) (string-prefix-p "=" text))
      (pcase (nth 4 rule)
        ('list
         (let ((options (exc-validation-options source rule)))
           (unless (member (downcase text) (mapcar #'downcase options))
             (format "%s is not one of: %s" text
                     (string-join options ", ")))))))))

(defun exc-get-conditional-formats (source)
  "Return SOURCE's conditional format rules.
Each rule is (TOP LEFT BOTTOM RIGHT OP VALUE STYLE)."
  (when-let* ((function (gethash :get-conditional-formats source)))
    (funcall function source)))

(defun exc-set-conditional-formats (source rules)
  "Replace SOURCE's conditional format rules with RULES."
  (when-let* ((function (gethash :set-conditional-formats source)))
    (funcall function source rules)))

(defconst exc-conditional-operators
  '((">" . "greater than") ("<" . "less than")
    (">=" . "at least") ("<=" . "at most")
    ("=" . "equal to") ("<>" . "not equal to")
    ("contains" . "text contains")
    ("empty" . "is empty") ("error" . "is an error"))
  "Operators of conditional format rules, with their descriptions.")

(defun exc-conditional-match-p (op operand value)
  "Return non-nil when computed VALUE satisfies OP against OPERAND."
  (let ((number (exc-calc--numeric value))
        (target (exc-calc--numeric operand)))
    (pcase op
      (">" (and number target (> number target)))
      ("<" (and number target (< number target)))
      (">=" (and number target (>= number target)))
      ("<=" (and number target (<= number target)))
      ("=" (and (not (member value '(nil "")))
                (exc-calc--equal value operand)))
      ("<>" (not (exc-calc--equal value operand)))
      ("contains" (and (stringp operand) (not (string-empty-p operand))
                       (string-match-p (regexp-quote (downcase operand))
                                       (downcase (format "%s" (or value ""))))))
      ("empty" (member value '(nil "")))
      ("error" (exc-calc-error-text-p value)))))

(defun exc-effective-style (source row col &optional rules)
  "Return the style the cell at ROW and COL of SOURCE is shown with.
The cell's own style comes first; every conditional format rule its
value satisfies then overrides the properties it names, later rules
winning.  RULES, when given, saves fetching the rules for every cell."
  (let ((style (exc-get-style-at source row col))
        (rules (or rules (exc-get-conditional-formats source))))
    (when rules
      (let ((value (exc-get-computed-value-at source row col)))
        (dolist (rule rules)
          (when (and (<= (nth 0 rule) row) (<= row (nth 2 rule))
                     (<= (nth 1 rule) col) (<= col (nth 3 rule))
                     (exc-conditional-match-p
                      (nth 4 rule) (nth 5 rule) value))
            (cl-loop for (key property) on (nth 6 rule) by #'cddr do
                     (setq style (plist-put style key property)))))))
    style))

(defun exc-source-unmerge-cell (source row col)
  "Unmerge the range covering ROW and COL in SOURCE."
  (funcall (gethash :unmerge-cell source) source row col))

;;;----------------------------------------------------------------------
;;; Display Configuration
;;;----------------------------------------------------------------------


(defcustom exc-title-align 'center
  "Title alignment above the table: left, center, or right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right))
  :group 'exc)

(defcustom exc-title-decoration 'none
  "Title decoration style above the table. When 'rule, draw a horizontal rule under the title."
  :type '(choice (const :tag "None" none)
                 (const :tag "Rule" rule))
  :group 'exc)


(defvar-local exc-allow-elisp nil
  "Buffer-local variable to enable elisp evaluation in table cells.

If this is nil, elisp formulas are ignored. Set it to non-nil to enable evaluation of elisp formulas in tables.
Warning: Only set this in trusted files! I am not responsible for any issues, damages, or bug reports caused by using this feature, and will not handle related issues.")
(put 'exc-allow-elisp 'risky-local-variable t)


(defcustom exc-default-save-directory nil
  "The default directory to save new grid files.
If nil, uses the `default-directory` of the current buffer."
  :type '(choice (const :tag "None (use current directory)" nil)
                 (directory :tag "Specify directory"))
  :group 'exc)


(defvar-local exc--data-source nil
  "Buffer-local variable to hold the current data source instance.")

(defvar-local exc--file-path nil
  "Buffer-local variable to hold the file path of the current grid.")

(defvar-local exc--title nil
  "Buffer-local variable to hold the title of the current grid.")


(declare-function exc-etaf-show-in-current-buffer "exc-etaf")
(declare-function exc-etaf-refresh "exc-etaf")
(declare-function exc-etaf-cell-at-point "exc-etaf")
(declare-function exc-etaf-edit-cell "exc-etaf")
(declare-function exc-etaf-set-cell "exc-etaf")
(declare-function exc-etaf-note-frozen "exc-etaf")


;;;----------------------------------------------------------------------
;;; Rendering
;;;----------------------------------------------------------------------

(defun exc--require-view ()
  "Load the view that shows grids, or say what it needs."
  (unless (featurep 'exc-etaf)
    (condition-case err
        (require 'exc-etaf)
      (error
       (user-error "Showing a grid needs ETAF: put the ecss, ebox and etaf \
checkouts from https://github.com/emacs-taf on `load-path' (%s)"
                   (error-message-string err))))))

(define-derived-mode exc-mode special-mode "Exc"
  "Parent of the major mode that shows a grid.
Grids are shown in `exc-etaf-mode', which derives from this mode,
so hooks and checks written for `exc-mode' keep working."
  :group 'exc)

(defvar-local exc--nav-row nil
  "Row of the cursor in the current grid, as a display row.")

(defvar-local exc--nav-col nil
  "Column of the cursor in the current grid, as a display column.
Display column 0 is the row numbers; the first column of the grid is 1.")

(declare-function exc-etaf-goto "exc-etaf")
(declare-function exc-etaf-move "exc-etaf")
(declare-function exc-etaf-move-tab "exc-etaf")
(declare-function exc-etaf-row-start "exc-etaf")
(declare-function exc-etaf-row-end "exc-etaf")

(defun exc-get-cell-at-point ()
  "Return the complete logical descriptor for the cell at point."
  (interactive)
  (exc-etaf-cell-at-point))

(defun exc--move-to-cell (display-row col)
  "Move the cursor to DISPLAY-ROW and display column COL."
  (exc-etaf-goto (max 0 display-row) (max 0 (1- col))))

(defun exc-next-line ()
  "Move to the cell in the next row, same column."
  (interactive)
  (exc-etaf-move 1 0))

(defun exc-previous-line ()
  "Move to the cell in the previous row, same column."
  (interactive)
  (exc-etaf-move -1 0))

(defun exc-next-cell ()
  "Move to the next cell in reading order."
  (interactive)
  (exc-etaf-move-tab 1))

(defun exc-previous-cell ()
  "Move to the previous cell in reading order."
  (interactive)
  (exc-etaf-move-tab -1))

(defun exc-cell-right ()
  "Move to the cell on the right."
  (interactive)
  (exc-etaf-move 0 1))

(defun exc-cell-left ()
  "Move to the cell on the left."
  (interactive)
  (exc-etaf-move 0 -1))

(defun exc-move-beginning-of-line ()
  "Move to the first cell of the row."
  (interactive)
  (exc-etaf-row-start))

(defun exc-move-end-of-line ()
  "Move to the last cell of the row."
  (interactive)
  (exc-etaf-row-end))


;;----------------------------------------------------------------------
;; Column Width Calculation
;;----------------------------------------------------------------------


;;----------------------------------------------------------------------
;; Title rendering helpers
;;----------------------------------------------------------------------


;;----------------------------------------------------------------------
;; Draw Row
;;----------------------------------------------------------------------


(defun exc--parse-cell-address (address)
  "Parse ADDRESS into zero-based (ROW COL), or return nil."
  (when (string-match "\\`\\([A-Za-z]+\\)\\([1-9][0-9]*\\)\\'" address)
    (list (1- (string-to-number (match-string 2 address)))
          (exc-calc-col-letter-to-number (match-string 1 address)))))

;;;###autoload
(defun exc-merge-cells (range)
  "Merge the cells of A1-style RANGE, a row, a column or a rectangle."
  (interactive (list (read-string "Merge range (e.g. B1:D1 or B2:B9): ")))
  (let* ((parts (split-string range ":" t "[[:space:]]*"))
         (start (and (= (length parts) 2)
                     (exc--parse-cell-address (car parts))))
         (end (and start
                   (exc--parse-cell-address (cadr parts)))))
    (unless (and start end)
      (user-error "Merge range must look like B1:D1"))
    (let* ((top (min (car start) (car end)))
           (left (min (cadr start) (cadr end)))
           (bottom (max (car start) (car end)))
           (right (max (cadr start) (cadr end))))
      (setq-local exc--data-source
                  (exc-source-merge-cells exc--data-source
                                    top left bottom right))
      (exc-refresh top (1+ left))
      (message "Merged %s." range))))

;;;###autoload
(defun exc-unmerge-cell ()
  "Unmerge the merged cell at point."
  (interactive)
  (when-let* ((coords (exc-get-cell-at-point))
              (row (plist-get coords :anchor-row))
              (display-col (plist-get coords :anchor-col)))
    (when (or (= row exc--col-header-row-idx)
              (= display-col exc--row-header-col-idx))
      (user-error "Place point on a data cell"))
    (unless (or (/= (plist-get coords :row-start)
                    (plist-get coords :row-end))
                (/= (plist-get coords :col-start)
                    (plist-get coords :col-end)))
      (user-error "Cell is not merged"))
    (setq-local exc--data-source
                (exc-source-unmerge-cell exc--data-source
                                   row (1- display-col)))
    (exc-refresh row display-col)
    (message "Cell unmerged.")))

(defun exc-edit-cell ()
  "Edit the value of the current cell."
  (interactive)
  (exc-etaf-edit-cell))


(defun exc-edit-title ()
  "Edit the title of the current grid with alignment and decoration options."
  (interactive)
  (let* ((new-title (read-string (format "Edit Title: %s -> " (or exc--title "")) (or exc--title "")))
         (new-align (when (not (string-empty-p new-title))
                      (intern (completing-read "Title alignment: " 
                                               '("left" "center" "right") 
                                               nil t 
                                               (symbol-name exc-title-align)))))
         (new-decoration (when (not (string-empty-p new-title))
                           (intern (completing-read "Title decoration: " 
                                                    '("none" "rule") 
                                                    nil t 
                                                    (symbol-name exc-title-decoration))))))
    (unless (and (string= new-title (or exc--title ""))
                 (eq new-align exc-title-align)
                 (eq new-decoration exc-title-decoration))
      (setq-local exc--title new-title)
      ;; The sheet keeps its title, to show it again and write it out.
      (when exc--data-source
        (puthash :title new-title exc--data-source))
      (when new-align
        (setq-local exc-title-align new-align))
      (when new-decoration
        (setq-local exc-title-decoration new-decoration))
      (exc-refresh)
      (message "Grid title updated (align: %s, decoration: %s)." 
               (or new-align exc-title-align) 
               (or new-decoration exc-title-decoration)))))

(defun exc--read-sort-order (prompt)
  "Read with PROMPT whether to sort ascending or descending."
  (intern (completing-read prompt '("ascending" "descending") nil t
                           nil nil "ascending")))

(defun exc-sort-column (&optional more)
  "Sort the rows by the current column, asking for the direction.
With a prefix argument MORE, then ask for more columns, each with its
direction, to order the rows the ones before leave equal; an empty
column ends the list."
  (interactive "P")
  (when exc--data-source
    (let* ((source exc--data-source)
           (coords (exc-get-cell-at-point))
           (current-col (if coords (plist-get coords :anchor-col) 1))
           (col-idx (if (and coords (> current-col 0)) ; Skip row header column
                        (1- current-col) ; Adjust for row header column
                      (read-number "Sort by column (0-based index): " 0)))
           (keys (list (cons col-idx
                             (exc--read-sort-order
                              (format "Sort by %s: "
                                      (exc-calc-col-number-to-letter col-idx))))))
           (sort-fn (gethash :sort-by-column source)))
      (when more
        (cl-loop
         for letters = (string-trim
                        (read-string "Then by column (letter, RET to sort): "))
         until (string-empty-p letters) do
         (let ((col (and (string-match-p "\\`[A-Za-z]+\\'" letters)
                         (exc-calc-col-letter-to-number letters))))
           (unless (and col (< col (exc-get-column-count source)))
             (user-error "No column %s" letters))
           (push (cons col (exc--read-sort-order
                            (format "Then by %s: " (upcase letters))))
                 keys)))
        (setq keys (nreverse keys))
        (when (and (cdr keys) (null (gethash :model source)))
          (user-error "This grid sorts by one column at a time")))
      (when sort-fn
        (let* ((current-coords (exc-get-cell-at-point))
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
          (setq-local exc--data-source new-data-source)
          (exc-refresh restore-row-idx restore-col-idx) ; Force full refresh to display sorted results
          (message "Sorted by %s"
                   (mapconcat (lambda (key)
                                (format "%s (%s)"
                                        (exc-calc-col-number-to-letter (car key))
                                        (cdr key)))
                              keys ", then ")))))))

;;----------------------------------------------------------------------
;; Column Letter Conversion
;;----------------------------------------------------------------------


;;----------------------------------------------------------------------
;; Add/Delete Row/Column
;;----------------------------------------------------------------------

(defun exc-add-row (&optional row-idx)
  "Add a new row at ROW-IDX, or at the end if ROW-IDX is nil.
If called interactively, adds a row below the current cell."
  (interactive (list (when-let ((coords (exc-get-cell-at-point)))
                       (plist-get coords :slice-row))))
  (when exc--data-source
    (cl-block exc-add-row
      ;; Validate row-idx before proceeding; -1 is above the first row
      (unless (and row-idx (numberp row-idx) (>= row-idx -1))
        (message "Error: Invalid row index. Please place cursor on a valid table cell.")
        (cl-return-from exc-add-row))
      (when (and (= row-idx -1)
                 (exc-header-row-p exc--data-source))
        (message "Cannot insert above the header row.")
        (cl-return-from exc-add-row))
      (let* ((current-coords (exc-get-cell-at-point))
             (restore-row-idx
              (when current-coords (plist-get current-coords :slice-row)))
             (restore-col-idx
              (when current-coords (plist-get current-coords :slice-col)))
             (add-row-fn (gethash :add-row exc--data-source))
             (data-model-row-idx row-idx) ; row-idx is already correct (0-based data row index)
             (new-data-source (funcall add-row-fn exc--data-source data-model-row-idx)))
        (setq-local exc--data-source new-data-source)
        (exc-refresh restore-row-idx restore-col-idx)
        (message "Row added.")))))

(defun exc-delete-row (&optional row-idx)
  "Delete the row at ROW-IDX.
If called interactively, deletes the current row."
  (interactive (list (when-let ((coords (exc-get-cell-at-point)))
                       (plist-get coords :slice-row))))
  (when exc--data-source
    (cl-block exc-delete-row
      ;; Validate row-idx before proceeding
      (unless (and row-idx (numberp row-idx) (>= row-idx 0))
        (message "Error: Invalid row index. Please place cursor on a valid table cell.")
        (cl-return-from exc-delete-row))
      ;; Row index 0 is the user-defined header row, not allowed to be deleted
      (when (and (= row-idx 0)
                 (exc-header-row-p exc--data-source))
        (message "Cannot delete header row.")
        (cl-return-from exc-delete-row))
      (let* ((current-coords (exc-get-cell-at-point))
             (restore-row-idx
              (when current-coords (plist-get current-coords :slice-row)))
             (restore-col-idx
              (when current-coords (plist-get current-coords :slice-col)))
             ;; Convert display row index to data area (excluding header): data row index = display row index - 1
             (data-model-row-idx (1- row-idx)))
        (condition-case err
            (let* ((delete-row-fn (gethash :delete-row exc--data-source))
                   (new-data-source (funcall delete-row-fn exc--data-source data-model-row-idx)))
              (setq-local exc--data-source new-data-source)
              ;; After deletion, calculate a safe recovery position to avoid the cursor falling outside the table (especially when deleting the last row)
              (let* ((get-row-count-fn (gethash :get-row-count exc--data-source))
                     (get-col-count-fn (gethash :get-column-count exc--data-source))
                     (new-num-rows (funcall get-row-count-fn exc--data-source))
                     (new-num-cols (funcall get-col-count-fn exc--data-source))
                     ;; Display layer row index range: [0 .. new-num-rows-1]
                     (safe-row (max 0 (min (or restore-row-idx 0) (1- new-num-rows))))
                     ;; Display layer column index range: [0 .. new-num-cols] (including row header column 0)
                     (safe-col (max 0 (min (or restore-col-idx 0) new-num-cols))))
                (exc-refresh safe-row safe-col))
              (message "Row deleted."))
          (error (message "Error deleting row: %s" (error-message-string err))))))))

(defun exc-add-column (&optional col-idx)
  "Add a new column at COL-IDX, or at the end if COL-IDX is nil.
If called interactively, adds a column to the right of the current cell."
  (interactive (list (when-let ((coords (exc-get-cell-at-point)))
                       (plist-get coords :slice-col))))
  (when exc--data-source
    (cl-block exc-add-column
      ;; Validate col-idx before proceeding
      (unless (and col-idx (numberp col-idx) (>= col-idx 0))
        (message "Error: Invalid column index. Please place cursor on a valid table cell.")
        (cl-return-from exc-add-column))
      (let* ((current-coords (exc-get-cell-at-point))
             (restore-row-idx
              (when current-coords (plist-get current-coords :slice-row)))
             (restore-col-idx
              (when current-coords (plist-get current-coords :slice-col)))
             ;; To insert to the right of the current column, the new column should appear after the current data column.
             ;; exc-model-add-column inserts the new column before the given index (left).
             ;; Therefore we pass the display index (including header 0), which is equivalent to (data-col-index + 1).
             ;; This ensures insertion to the right of the current data column.
             (insertion-col-idx col-idx))
        (let* ((add-column-fn (gethash :add-column exc--data-source))
               (new-data-source (funcall add-column-fn exc--data-source insertion-col-idx)))
          (setq-local exc--data-source new-data-source)
          (exc-refresh restore-row-idx restore-col-idx)
          (message "Column added."))))))

(defun exc-delete-column (&optional col-idx)
  "Delete the column at COL-IDX.
If called interactively, deletes the current column."
  (interactive (list (when-let ((coords (exc-get-cell-at-point)))
                       (plist-get coords :slice-col))))
  (when exc--data-source
    (cl-block exc-delete-column
      ;; Validate col-idx before proceeding
      (unless (and col-idx (numberp col-idx) (>= col-idx 0))
        (message "Error: Invalid column index. Please place cursor on a valid table cell.")
        (cl-return-from exc-delete-column))
      (let* ((current-coords (exc-get-cell-at-point))
             (restore-row-idx
              (when current-coords (plist-get current-coords :slice-row)))
             (restore-col-idx
              (when current-coords (plist-get current-coords :slice-col)))
             (data-model-col-idx (1- col-idx))) ; display col 1 = data col 0, display col 2 = data col 1, etc.
        (condition-case err
            (let* ((delete-column-fn (gethash :delete-column exc--data-source))
                   (new-data-source (funcall delete-column-fn exc--data-source data-model-col-idx)))
              (setq-local exc--data-source new-data-source)
              (exc-refresh restore-row-idx restore-col-idx)
              (message "Column deleted."))
          (error (message "Error deleting column: %s" (error-message-string err))))))))

(defun exc-insert-row-above ()
  "Insert a new row above the current row.
In a sheet with a header row, nothing goes above that row."
  (interactive)
  (when-let ((coords (exc-get-cell-at-point)))
    (exc-add-row (1- (plist-get coords :slice-row)))))

(defun exc-insert-column-left ()
  "Insert a new column to the left of the current column."
  (interactive)
  (when-let ((coords (exc-get-cell-at-point)))
    ;; To insert to the left of the current column, reuse the implementation of "insert to the right":
    ;; Pass (display-col-idx - 1), which is equivalent to the left of the current column.
    (let* ((display-col (plist-get coords :slice-col))
           (target (max 0 (1- display-col))))
      (exc-add-column target))))

;;----------------------------------------------------------------------
;; Save/Load
;;----------------------------------------------------------------------

(defun exc-write-file ()
  "Save the current grid to its associated file, or prompt for a new one."
  (interactive)
  (let ((file-path (or (buffer-file-name) ; Use existing path if available
                       (let* ((default-dir (or exc-default-save-directory default-directory))
                              (prompt (format "Save Grid in %s as: " (file-name-as-directory default-dir))))
                         (read-file-name prompt default-dir nil nil)))))
    (when file-path
      ;; If user provides a relative path, make it absolute based on the default dir
      (unless (file-name-absolute-p file-path)
        (setq file-path (expand-file-name file-path (or exc-default-save-directory default-directory))))
      ;; Add .grid extension if not present
      (when (not (string-equal ".grid" (file-name-extension file-path t)))
        (setq file-path (concat file-path ".grid")))
      ;; Update the title in the data source before saving
      (puthash :title exc--title exc--data-source)
      (exc-persistence--save-to-file exc--data-source file-path)
      (setq-local exc--file-path file-path)
      (unless (and buffer-file-name
                   (file-equal-p buffer-file-name file-path))
        (set-visited-file-name file-path))
      ;; Saved here rather than by `save-buffer', which would find the
      ;; file changed since it was visited.
      (set-visited-file-modtime)
      (set-buffer-modified-p nil)
      (message "Grid saved to %s" file-path))))

(defun exc-find-file (file-path)
  "Open a grid from a file.
The buffer visits FILE-PATH, so `save-buffer' saves the grid there."
  (interactive "fFind grid file: ")
  (let ((buffer (find-file-noselect file-path)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'exc-mode)
        (exc-visit)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun exc-visit ()
  "Show the grid file the current buffer visits as a grid.
`find-file' calls this for a .grid file; `save-buffer' then saves the
grid to the file.  A file that does not exist yet starts as an empty
grid."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer visits no file"))
  (let ((data-source
         (if (file-exists-p buffer-file-name)
             (exc-persistence-create-data-source-from-file buffer-file-name)
           ;; A new file starts as an empty grid, saved on the first save.
           (make-exc-default-data-source (make-list 4 "") (make-list 4 (make-list 4 ""))
                                     (file-name-base buffer-file-name)))))
    (unless data-source
      (user-error "Failed to load grid from %s" buffer-file-name))
    (setq-local exc--file-path buffer-file-name)
    (setq-local exc--title (gethash :title data-source))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (exc--show-grid-in-current-buffer data-source)
    (set-buffer-modified-p nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grid\\'" . exc-visit))

;;;###autoload
(defun exc-open (file-path)
  "Open a .grid file directly in exc view."
  (interactive "fOpen grid file: ")
  (exc-find-file file-path))


;;----------------------------------------------------------------------
;; Main Functions & Mode Definition
;;----------------------------------------------------------------------


(defun exc--show-grid-in-current-buffer (data-source &optional title)
  "Display the grid in the current buffer.

DATA-SOURCE is a hash table containing the data source functions.
TITLE is an optional title for the grid."
  (exc--require-view)
  (exc-etaf-show-in-current-buffer data-source title))

;;----------------------------------------------------------------------
;; Sheets
;;----------------------------------------------------------------------

(defun exc--workbook ()
  "Return the workbook of the current grid, making one when it stands alone."
  (unless exc--data-source
    (user-error "No grid in this buffer"))
  (exc-workbook-ensure exc--data-source))

(defun exc-sheets ()
  "Return the sheets of the current grid's workbook, or nil without one.
A grid that stands alone has no workbook until a sheet is added."
  (when-let* ((workbook (and exc--data-source
                             (exc-workbook-of exc--data-source))))
    (exc-workbook-sheets workbook)))

(defun exc-switch-to-sheet (source)
  "Show SOURCE, a sheet of the current grid's workbook, in this buffer."
  (unless (eq source exc--data-source)
    (let ((modified (buffer-modified-p)))
      (setq exc--title (gethash :title source))
      (exc--show-grid-in-current-buffer source)
      (set-buffer-modified-p modified))))

(defun exc--sheet-changed (source)
  "Show SOURCE after the workbook's sheets changed, and mark the buffer changed."
  (let ((current exc--data-source))
    (setq exc--title (gethash :title source))
    (if (eq source current)
        (exc-refresh)
      (exc--show-grid-in-current-buffer source))
    (set-buffer-modified-p t)))

(defun exc--read-sheet-name (prompt workbook &optional except)
  "Read with PROMPT a name a sheet of WORKBOOK can take.
EXCEPT is a sheet allowed to have that name already."
  (let* ((default (if except
                      (exc-workbook-sheet-name except)
                    (exc-workbook-fresh-name workbook)))
         (name (string-trim
                (read-string (format "%s (default %s): " prompt default)
                             nil nil default))))
    (when-let* ((problem (exc-workbook-name-problem workbook name except)))
      (user-error "%s" problem))
    name))

(defun exc-sheet-step (count)
  "Show the sheet COUNT places after the current one, wrapping around."
  (let* ((sheets (exc-sheets))
         (index (cl-position exc--data-source sheets)))
    (if (null (cdr sheets))
        (message "This grid has one sheet; C-c w n adds another")
      (exc-switch-to-sheet
       (nth (mod (+ index count) (length sheets)) sheets)))))

(defun exc-sheet-next ()
  "Show the next sheet of the workbook."
  (interactive)
  (exc-sheet-step 1))

(defun exc-sheet-previous ()
  "Show the previous sheet of the workbook."
  (interactive)
  (exc-sheet-step -1))

(defun exc-sheet-select (name)
  "Show the sheet called NAME."
  (interactive
   (let ((names (mapcar #'exc-workbook-sheet-name (exc-sheets))))
     (unless (cdr names)
       (user-error "This grid has one sheet; C-c w n adds another"))
     (list (completing-read "Sheet: " names nil t))))
  (exc-switch-to-sheet
   (or (exc-workbook-sheet (exc--workbook) name)
       (user-error "No sheet called %s" name))))

(defun exc-sheet-new (name)
  "Add an empty sheet called NAME after the current one and show it."
  (interactive
   (list (exc--read-sheet-name "New sheet" (exc--workbook))))
  (let* ((workbook (exc--workbook))
         (index (cl-position exc--data-source
                             (exc-workbook-sheets workbook))))
    (when-let* ((problem (exc-workbook-name-problem workbook name)))
      (user-error "%s" problem))
    (exc--sheet-changed
     (exc-workbook-new-sheet workbook name (1+ index)))))

(defun exc-toggle-header-row ()
  "Say whether the first row of the current sheet holds column titles.
With a header row, which every sheet starts with, the first row stays in
place when the sheet is sorted or filtered and cannot be deleted.
Without one the first row is a row like any other, for a sheet that is
not a single table."
  (interactive)
  (let ((source (or exc--data-source
                    (user-error "No grid in this buffer"))))
    (exc-set-header-row source (not (exc-header-row-p source)))
    (exc-refresh)
    (message (if (exc-header-row-p source)
                 "The first row is the header row again"
               "The first row is now a row like any other"))))

(defun exc-unfreeze-panes ()
  "Let every row and column of the current sheet scroll again."
  (interactive)
  (let* ((source (or exc--data-source
                     (user-error "No grid in this buffer")))
         (old (exc-frozen source)))
    (unless (exc-set-frozen source 0 0)
      (user-error "This grid cannot keep rows and columns in place"))
    (exc-etaf-note-frozen old)
    (exc-refresh)
    (message "No row or column is frozen")))

(defun exc-sheet-rename (name)
  "Call the current sheet NAME.
Formulas that read the sheet are rewritten to use the new name."
  (interactive
   (list (exc--read-sheet-name "Rename sheet to" (exc--workbook)
                                      exc--data-source)))
  (let ((workbook (exc--workbook)))
    (when-let* ((problem (exc-workbook-name-problem
                          workbook name exc--data-source)))
      (user-error "%s" problem))
    (exc-workbook-rename-sheet workbook exc--data-source name)
    (exc--sheet-changed exc--data-source)))

(defun exc-sheet-delete ()
  "Delete the current sheet.  This cannot be undone.
Formulas that read the sheet show #REF! afterwards."
  (interactive)
  (let* ((workbook (exc--workbook))
         (sheets (exc-workbook-sheets workbook))
         (source exc--data-source)
         (index (cl-position source sheets)))
    (unless (cdr sheets)
      (user-error "A grid keeps at least one sheet"))
    (when (or (not (called-interactively-p 'interactive))
              (yes-or-no-p (format "Delete sheet %s? This cannot be undone. "
                                   (exc-workbook-sheet-name source))))
      (exc-workbook-remove-sheet workbook source)
      (exc--sheet-changed
       (nth (min index (- (length sheets) 2))
            (exc-workbook-sheets workbook))))))

(defun exc-sheet-move (delta)
  "Move the current sheet DELTA places among the sheets."
  (let ((workbook (exc--workbook)))
    (exc-workbook-move-sheet workbook exc--data-source delta)
    (exc--sheet-changed exc--data-source)))

(defun exc-sheet-move-left ()
  "Move the current sheet one place towards the first."
  (interactive)
  (exc-sheet-move -1))

(defun exc-sheet-move-right ()
  "Move the current sheet one place towards the last."
  (interactive)
  (exc-sheet-move 1))


(defun exc--show-grid (data-source &optional buffer-name)
  "Show a grid view in a dedicated buffer using DATA-SOURCE."
  (let ((buffer (get-buffer-create (or buffer-name "*Exc*"))))
    (with-current-buffer buffer
      (exc--show-grid-in-current-buffer data-source))
    (switch-to-buffer buffer)))

(defun exc-refresh (&optional restore-row-idx restore-col-idx)
  "Refresh the table, restoring point to RESTORE-ROW-IDX and RESTORE-COL-IDX."
  (interactive)
  (exc-etaf-refresh restore-row-idx restore-col-idx))

(defun exc-recalculate-all ()
  "Compute every formula of the grid anew and show the result.
Edits only recompute the cells they reach; this is for whatever they
cannot see, such as a formula function defined since."
  (interactive)
  (let ((source (or exc--data-source
                    (user-error "No grid in this buffer"))))
    (when (gethash :model source)
      (exc-calc-calc-forget)
      (exc-recalculate source t))
    (exc-refresh)))


(defun exc--chart-type-defaults (type)
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

(defun exc-chart-content (chart)
  "Return the text drawing of CHART, the value of a chart formula."
  (let* ((type (plist-get chart :type))
         (data (plist-get chart :data))
         (options (plist-get chart :options))
         (defaults (exc--chart-type-defaults type))
         (params (exc-chart--options-to-params options defaults)))
    (cond
     ((eq type 'bar)
      (exc-chart-barplot (plist-get data :labels)
                          (plist-get data :values)
                          params))
     ((eq type 'column)
      (exc-chart-columnplot (plist-get data :labels)
                             (plist-get data :values)
                             params))
     ((eq type 'line)
      (exc-chart-lineplot (plist-get data :x)
                           (plist-get data :y)
                           params
                           'line))
     ((eq type 'scatter)
      (exc-chart-lineplot (plist-get data :x)
                           (plist-get data :y)
                           params
                           'scatter))
     ((eq type 'histogram)
      (exc-chart-histogram (plist-get data :values)
                            params))
     ((eq type 'boxplot)
      (exc-chart-boxplot (plist-get data :series)
                          params))
     ((eq type 'multiline)
      (exc-chart-multiline (plist-get data :x)
                            (plist-get data :series)
                            params))
     ((eq type 'density)
      (exc-chart-density (plist-get data :data)
                          nil  ; unused parameter for API compatibility
                          params))
     ((eq type 'count)
      (exc-chart-count (plist-get data :values)
                        params))
     (t (format "Unknown chart type: %s" type)))))

(defun exc-view-chart ()
  "View the chart in the current cell in a separate buffer."
  (interactive)
  (when-let* ((coords (exc-get-cell-at-point))
              (row (plist-get coords :anchor-row))
              (col (plist-get coords :anchor-col))
              ;; Adjust col for data source (0-based data cols start at display col 1)
              (data-col (1- col))
              (val (exc-get-computed-value-at exc--data-source row data-col)))
    (if (and (listp val) (plist-get val :exc-chart))
        (let ((chart-content (exc-chart-content val)))
          (with-current-buffer (get-buffer-create "*Exc Plot*")
            (read-only-mode -1)
            (erase-buffer)
            (insert chart-content)
            (goto-char (point-min))
            (special-mode)
            (display-buffer (current-buffer))))
      (message "No chart found in this cell."))))

(defun exc-insert-chart ()
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
      (exc-edit-cell-with-value formula))))

(defun exc-edit-cell-with-value (value)
  "Helper to set cell value directly."
  (when-let* ((coords (exc-get-cell-at-point))
              (display-row-idx (plist-get coords :anchor-row))
              (display-col-idx (plist-get coords :anchor-col)))
    ;; The view records the change for undo.
    (exc-etaf-set-cell display-row-idx (1- display-col-idx) value)))

(defun exc--open-demo ()
  "Open an exc buffer with a demonstration table for internal testing."
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
         (data-source (make-exc-default-data-source headers rows "Demo Grid")))
    (setq-local exc--title "Demo Grid") ; Set default title for demo
    (exc--show-grid data-source)))

;;;###autoload
(defun exc-create (headers rows)
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
  (let ((data-source (make-exc-default-data-source headers rows "New Grid"))) ; headers are now the first data row
    (setq-local exc--title "New Grid") ; Set default title for new grids
    (exc--show-grid data-source (generate-new-buffer-name "*exc*"))))


(provide 'exc)

;;; exc.el ends here 
