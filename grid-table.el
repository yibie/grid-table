;;; grid-table.el --- Grid table for displaying and editing tabular data -*- lexical-binding: t -*-
;; Version: 0.3.0

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
(require 'grid-table-nav)
(require 'grid-table-chart)
(require 'grid-table-plugins)

;; Public version identifier.
(defconst grid-table-version "0.3.0")

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

(defun grid-get-raw-value-at (source row col)
  "Get raw value from data source."
  (funcall (gethash :get-raw-value-at source) source row col))

(defun grid-get-computed-value-at (source row col)
  "Get computed value from data source."
  (funcall (gethash :get-computed-value-at source) source row col))

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
Sources that do not store styles give nil."
  (when-let* ((function (gethash :get-style-at source)))
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
A formula that comes out false computes nil, as an empty cell does not."
  (and (null value)
       (gethash :get-raw-value-at source)
       (let ((raw (grid-get-raw-value-at source row col)))
         (and (stringp raw) (string-prefix-p "=" raw)))))

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

(defun grid-table-read-entry (source row col text)
  "Read TEXT as typed into the cell at ROW and COL of SOURCE.
Return (RAW . STYLE): RAW is the text to store and STYLE the style to
give the cell, or the symbol `keep' to leave its style alone.  A date
written year first, a percentage, an amount of money and a number with
thousands separators are stored as numbers; a cell without a format
takes the one that shows the number as it was typed.  Sources that keep
no styles, and cells with the text format, store TEXT as it is."
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
    (cond ((null typed) (cons text 'keep))
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
      ("error" (and (stringp value) (member value grid-calc-error-values))))))

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

(defcustom grid-table-renderer 'etaf
  "Renderer used to show a grid.
`etaf' mounts the grid through ETAF and needs the ecss, ebox and etaf
packages on `load-path'.  `classic' draws box characters into the
buffer; it is the fallback while grids migrate to ETAF and is used
whenever ETAF cannot be loaded.  The classic renderer will be removed
once the migration is complete."
  :type '(choice (const :tag "ETAF" etaf)
                 (const :tag "Classic (migration fallback)" classic))
  :group 'grid-table)

(defvar-local grid-table--renderer nil
  "Renderer showing the current buffer: `etaf', or nil for classic.")

(defvar grid-table--etaf-warned nil
  "Non-nil once the ETAF renderer has failed to load or mount this session.")

(defvar-local grid-table--data-source nil
  "Buffer-local variable to hold the current data source instance.")

(defvar-local grid-table--file-path nil
  "Buffer-local variable to hold the file path of the current grid.")

(defvar-local grid-table--title nil
  "Buffer-local variable to hold the title of the current grid.")


(defface grid-table-highlight-face
  '((t :background "gray10"))
  "Face for highlighting the current grid cell."
  :group 'grid-table)

(defface grid-table-markup-face
  '((t :foreground "dim gray"))
  "Face for Org-like markup characters in grid cells."
  :group 'grid-table)

(defvar-local grid-table--highlight-overlay nil)

(declare-function grid-table-etaf-show-in-current-buffer "grid-table-etaf")
(declare-function grid-table-etaf-refresh "grid-table-etaf")
(declare-function grid-table-etaf-cell-at-point "grid-table-etaf")
(declare-function grid-table-etaf-edit-cell "grid-table-etaf")
(declare-function grid-table-etaf-set-cell "grid-table-etaf")

(defun grid-table--etaf-p ()
  "Return non-nil when the current buffer is shown through ETAF."
  (eq grid-table--renderer 'etaf))

(defun grid-table--enter-classic-mode ()
  "Enable `grid-table-mode' unless the grid will be shown through ETAF.
The ETAF renderer sets its own major mode when it mounts."
  (unless (eq (grid-table--resolve-renderer) 'etaf)
    (grid-table-mode)))

(defun grid-table--resolve-renderer ()
  "Return the renderer to show a grid with: `etaf' or `classic'.
Falls back to `classic' when `grid-table-renderer' asks for ETAF and it
cannot be loaded."
  (cond ((not (eq grid-table-renderer 'etaf)) 'classic)
        (grid-table--etaf-warned 'classic)
        ((featurep 'grid-table-etaf) 'etaf)
        ;; `require' with NOERROR still signals when ETAF itself is missing.
        (t (condition-case err
               (progn (require 'grid-table-etaf) 'etaf)
             (error (grid-table--etaf-failed err) 'classic)))))

(defun grid-table--etaf-failed (err)
  "Report ERR from the ETAF renderer and stay on classic for this session."
  (setq grid-table--etaf-warned t)
  (message "grid-table: ETAF renderer unavailable (%s), using the classic renderer"
           (error-message-string err)))

(defvar grid-table-cell-edit-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "M-RET") #'newline)
    map)
  "Minibuffer keymap used while editing a grid cell.")

;;;----------------------------------------------------------------------
;;; Rendering
;;;----------------------------------------------------------------------

(defun grid-table-get-cell-at-point ()
  "Return the complete logical descriptor for the cell at point."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-cell-at-point)
    (grid-table--classic-cell-at-point)))

(defun grid-table--classic-cell-at-point ()
  "Return the cell descriptor read from the classic renderer's text."
  (let ((position
         (if (get-text-property (point) 'grid-cell)
             (point)
           (save-excursion
             (goto-char (line-beginning-position))
             (catch 'cell
               (while (< (point) (line-end-position))
                 (when (get-text-property (point) 'grid-cell)
                   (throw 'cell (point)))
                 (forward-char 1))
               nil)))))
    (when position
      (let* ((old-row
              (get-text-property position 'grid-display-row-idx))
             (old-col
              (get-text-property position 'grid-display-col-idx))
             (row-start
              (or (get-text-property position 'grid-display-row-start)
                  old-row))
             (row-end
              (or (get-text-property position 'grid-display-row-end)
                  row-start))
             (col-start
              (or (get-text-property position 'grid-display-col-start)
                  old-col))
             (col-end
              (or (get-text-property position 'grid-display-col-end)
                  (get-text-property position 'grid-display-col-end-idx)
                  col-start))
             (slice-row
              (or (get-text-property position 'grid-display-slice-row)
                  row-start))
             (property-slice-col
              (or (get-text-property position 'grid-display-slice-col)
                  col-start))
             (slice-col
              (if (and (boundp 'grid-table--nav-row)
                       (boundp 'grid-table--nav-col)
                       (equal grid-table--nav-row slice-row)
                       (integerp grid-table--nav-col)
                       (<= col-start grid-table--nav-col)
                       (<= grid-table--nav-col col-end))
                  grid-table--nav-col
                property-slice-col)))
        (list :row-start row-start :row-end row-end
              :col-start col-start :col-end col-end
              :anchor-row
              (or (get-text-property position 'grid-display-anchor-row)
                  row-start)
              :anchor-col
              (or (get-text-property position 'grid-display-anchor-col)
                  col-start)
              :slice-row slice-row :slice-col slice-col
              ;; Compatibility keys for third-party callers.
              :display-row-idx row-start
              :display-col-idx col-start
              :display-col-end-idx col-end)))))


;;----------------------------------------------------------------------
;; Column Width Calculation
;;----------------------------------------------------------------------

(defconst grid-table--font-lock-keywords
  (list
   ;; *bold*
   '("\\(\\*\\)\\([^\n*]+\\)\\(\\*\\)"
     (1 'grid-table-markup-face) (2 'bold) (3 'grid-table-markup-face))
   ;; /italic/
   '("\\(/\\)\\([^\n/]+\\)\\(/\\)"
     (1 'grid-table-markup-face) (2 'italic) (3 'grid-table-markup-face))
   ;; _underline_
   '("\\(_\\)\\([^\n_]+\\)\\(_\\)"
     (1 'grid-table-markup-face) (2 'underline) (3 'grid-table-markup-face))
   ;; +strike-through+
   '("\\(\\+\\)\\([^\n+]+\\)\\(\\+\\)"
     (1 'grid-table-markup-face) (2 'strike-through) (3 'grid-table-markup-face))
   ;; ~code~
   '("\\(~\\)\\([^\n~]+\\)\\(~\\)"
     (1 'grid-table-markup-face) (2 'fixed-pitch) (3 'grid-table-markup-face)))
  "Font-lock keywords for grid-table-mode.")


(defun grid-table--setup-perfect-display-environment ()
  "Setup perfect display environment, ensure consistent line height."
  ;; Force line spacing to 0 - this is the key setting
  (setq-local line-spacing 0)
  
  ;; Ensure no extra line height factor
  (setq-local line-height-factor 1.0)
  
  ;; Disable features that might affect line height
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  
  (message "Display environment optimized for perfect height matching"))

;;----------------------------------------------------------------------
;; Title rendering helpers
;;----------------------------------------------------------------------


(defun grid-table--render-title-line (data-source)
  "Render title line above the table according to alignment/decoration settings."
  (when grid-table--title
    (let* ((table-width (grid-table--compute-table-width data-source))
           (title-text (format "Title: %s" grid-table--title))
           (title-width (string-width title-text))
           (left-pad (pcase grid-table-title-align
                       ('left 0)
                       ('center (max 0 (floor (/ (max 0 (- table-width title-width)) 2))))
                       ('right (max 0 (- table-width title-width)))
                       (_ 0)))
           (padding (make-string left-pad ?\s))
           (result (concat padding title-text "\n")))
      (when (eq grid-table-title-decoration 'rule)
        (setq result (concat result (make-string table-width ?─) "\n")))
      (concat result "\n"))))

;;----------------------------------------------------------------------
;; Draw Row
;;----------------------------------------------------------------------

(defun grid-table--enforce-line-height (string)
  "Enforce a fixed 'line-height' text property for each line in STRING."
  (propertize string 'line-height (grid-table--get-exact-line-height)))


(defun grid-table--parse-cell-address (address)
  "Parse ADDRESS into zero-based (ROW COL), or return nil."
  (when (string-match "\\`\\([A-Za-z]+\\)\\([1-9][0-9]*\\)\\'" address)
    (list (1- (string-to-number (match-string 2 address)))
          (grid-calc-col-letter-to-number (match-string 1 address)))))

;;;###autoload
(defun grid-table-merge-cells (range)
  "Merge the cells of A1-style RANGE.
The classic renderer only takes a range along one row or one column."
  (interactive (list (read-string "Merge range (e.g. B1:D1 or B2:B9): ")))
  (let* ((parts (split-string range ":" t "[[:space:]]*"))
         (start (and (= (length parts) 2)
                     (grid-table--parse-cell-address (car parts))))
         (end (and start
                   (grid-table--parse-cell-address (cadr parts)))))
    (unless (and start end)
      (user-error "Merge range must look like B1:D1"))
    (unless (or (grid-table--etaf-p)
                (xor (= (car start) (car end))
                     (= (cadr start) (cadr end))))
      (user-error "Merge range must span exactly one row or one column"))
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
  (if (grid-table--etaf-p)
      (grid-table-etaf-edit-cell)
    (grid-table--classic-edit-cell)))

(defun grid-table--classic-edit-cell ()
  "Edit the current cell of a classic grid through the minibuffer."
  (let ((inhibit-read-only t))
    (when-let* ((coords (grid-table-get-cell-at-point))
                (display-row-idx (plist-get coords :anchor-row))
                (display-col-idx (plist-get coords :anchor-col)))
      (cond
       ((= display-col-idx grid-table--row-header-col-idx) ; Editing row header (1, 2, 3...)
        (message "Cannot edit row header (1, 2, 3...) column."))
       ((= display-row-idx grid-table--col-header-row-idx) ; Editing A, B, C... header row
        (message "Cannot edit column header (A, B, C...) row."))

       (t ; Editing data cell
        (let* (;; Data model row index is the same as display row index for data rows.
               ;; Data model col index is display col index - 1 (for row header).
               (current-value (grid-table-edit-text grid-table--data-source display-row-idx (1- display-col-idx)))
               (new-value
                (read-from-minibuffer
                 (format "New value for %s%s (M-RET inserts newline): "
                         (grid-table--col-number-to-letter
                          (1- display-col-idx))
                         (1+ display-row-idx))
                 current-value grid-table-cell-edit-map))
               (modified-cell (grid-table-enter-value grid-table--data-source display-row-idx (1- display-col-idx) new-value)))
          (when modified-cell ; Only redraw if a cell was actually modified
            (grid-table-refresh) ; Full refresh is simpler and safer for now
            (grid-table--move-to-cell display-row-idx display-col-idx)
            (grid-table--highlight-cell display-row-idx display-col-idx)
            (message "Cell updated."))))))))

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
      (when new-align
        (setq-local grid-table-title-align new-align))
      (when new-decoration
        (setq-local grid-table-title-decoration new-decoration))
      (grid-table-refresh)
      (message "Grid title updated (align: %s, decoration: %s)." 
               (or new-align grid-table-title-align) 
               (or new-decoration grid-table-title-decoration)))))

(defun grid-table-sort-column ()
  "Sort the table by the current column or a user-specified column.
Prompts for sort direction (ascending/descending)."
  (interactive)
  (when grid-table--data-source
    (let* ((coords (grid-table-get-cell-at-point))
           (current-col (if coords (plist-get coords :anchor-col) 1))
           (col-idx (if (and coords (> current-col 0)) ; Skip row header column
                       (1- current-col) ; Adjust for row header column
                     (read-number "Sort by column (0-based index): " 0)))
           (sort-direction (completing-read "Sort direction: " '("ascending" "descending") nil t "ascending"))
           (sort-order (if (string= sort-direction "ascending") 'ascending 'descending))
           (sort-fn (gethash :sort-by-column grid-table--data-source)))
      (when sort-fn
        (let* ((current-coords (grid-table-get-cell-at-point))
               (restore-row-idx
                (when current-coords
                  (plist-get current-coords :slice-row)))
               (restore-col-idx
                (when current-coords
                  (plist-get current-coords :slice-col)))
               (new-data-source (funcall sort-fn grid-table--data-source col-idx sort-order)))
          (setq-local grid-table--data-source new-data-source)
          (grid-table-refresh restore-row-idx restore-col-idx) ; Force full refresh to display sorted results
          (message "Table sorted by column %s (%s)." col-idx sort-direction))))))

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
      (rename-buffer (file-name-nondirectory file-path))
      (set-visited-file-name file-path)
      (set-buffer-modified-p nil)
      (message "Grid saved to %s" file-path))))

(defun grid-table-find-file (file-path)
  "Open a grid from a file."
  (interactive "fFind grid file: " nil nil ".grid") ; Suggest .grid extension
  (let* ((data-source (grid-table-persistence-create-data-source-from-file file-path))
         (buffer-name (file-name-nondirectory file-path)))
    (if data-source
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (grid-table--enter-classic-mode)
            (setq-local grid-table--file-path file-path)
            ;; Get title from data source
            (setq-local grid-table--title (gethash :title data-source))
            (grid-table--show-grid-in-current-buffer data-source))
          (switch-to-buffer buffer))
      (message "Failed to load grid from %s" file-path))))

;;;###autoload
(defun grid-open (file-path)
  "Open a .grid file directly in grid-table view."
  (interactive "fOpen grid file: ")
  (grid-table-find-file file-path))


;;----------------------------------------------------------------------
;; Main Functions & Mode Definition
;;----------------------------------------------------------------------

(defun grid-table--insert-table-from-data-source ()
  "Insert a grid table into the current buffer from `grid-table--data-source`."
  (insert (grid-table-text-render grid-table--data-source)))

(defun grid-table--show-grid-in-current-buffer (data-source &optional title)
  "Display the grid table in the current buffer.

DATA-SOURCE is a hash table containing the data source functions.
TITLE is an optional title for the grid."
  (interactive)
  (unless (and (eq (grid-table--resolve-renderer) 'etaf)
               (condition-case err
                   (progn
                     (grid-table-etaf-show-in-current-buffer data-source title)
                     t)
                 ;; A stale ETAF checkout can load and still fail to mount.
                 (error
                  (grid-table--etaf-failed err)
                  (let ((file-path grid-table--file-path)
                        (old-title grid-table--title))
                    (grid-table-mode)
                    (setq grid-table--file-path file-path
                          grid-table--title old-title))
                  nil)))
    (grid-table--classic-show-in-current-buffer data-source title)))

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
  (let ((source (or grid-table--data-source
                    (user-error "No grid in this buffer"))))
    (unless (grid-table-set-frozen source 0 0)
      (user-error "This grid cannot keep rows and columns in place"))
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

(defun grid-table--classic-show-in-current-buffer (data-source &optional title)
  "Draw DATA-SOURCE into the current buffer with the classic renderer.
TITLE is an optional title for the grid."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq grid-table--data-source data-source)
    (when title
      (setq grid-table--title title))
    (when-let ((title-content (grid-table--render-title-line data-source)))
      (insert title-content))
    (grid-table--insert-table-from-data-source)
    (grid-table--fontify-text)
    (redisplay) ; Force redisplay to ensure text properties are applied
    (goto-char (point-min))
    (when (> (grid-get-row-count data-source) 0)
      (grid-table--move-to-cell 0 1) ; Move to first user-defined header cell (row header is 0, first data column is 1)
      (grid-table--highlight-cell 0 1)))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun grid-table--show-grid (data-source &optional buffer-name)
  "Show a grid view in a dedicated buffer using DATA-SOURCE."
  (let ((buffer (get-buffer-create (or buffer-name "*Grid Table*"))))
    (with-current-buffer buffer
      (grid-table--enter-classic-mode)
      (grid-table--show-grid-in-current-buffer data-source))
    (switch-to-buffer buffer)))

(defun grid-table-refresh (&optional restore-row-idx restore-col-idx)
  "Refresh the table, restoring point to RESTORE-ROW-IDX and RESTORE-COL-IDX."
  (interactive)
  (if (grid-table--etaf-p)
      (grid-table-etaf-refresh restore-row-idx restore-col-idx)
    (grid-table--classic-refresh restore-row-idx restore-col-idx)))

(defun grid-table-recalculate-all ()
  "Compute every formula of the grid anew and show the result.
Edits only recompute the cells they reach; this is for whatever they
cannot see, such as a formula function defined since."
  (interactive)
  (let ((source (or grid-table--data-source
                    (user-error "No grid in this buffer"))))
    (when (gethash :model source)
      (grid-table-recalculate source t))
    (grid-table-refresh)))

(defun grid-table--classic-refresh (&optional restore-row-idx restore-col-idx)
  "Redraw a classic grid, restoring point to RESTORE-ROW-IDX and RESTORE-COL-IDX."
  (let* ((inhibit-read-only t)
         (restore-coords (grid-table-get-cell-at-point))
         (restore-row-idx (or restore-row-idx
                              (plist-get restore-coords :slice-row)
                              grid-table--nav-row))
         (restore-col-idx (or restore-col-idx
                              (plist-get restore-coords :slice-col)
                              grid-table--nav-col)))
    (when (and grid-table--data-source (current-buffer))
      (let ((num-rows (grid-get-row-count grid-table--data-source))
            (num-cols (grid-get-column-count grid-table--data-source)))
        (when restore-row-idx
          (setq restore-row-idx
                (if (= restore-row-idx grid-table--col-header-row-idx)
                    restore-row-idx
                  (max 0 (min restore-row-idx (1- num-rows))))))
        (when restore-col-idx
          (setq restore-col-idx (max 0 (min restore-col-idx num-cols))))
        (when grid-table--nav-row
          (setq grid-table--nav-row restore-row-idx))
        (when grid-table--nav-col
          (setq grid-table--nav-col restore-col-idx)))
      (erase-buffer)
      (when-let ((title-content (grid-table--render-title-line grid-table--data-source)))
        (insert title-content))
      (grid-table--insert-table-from-data-source)
      (grid-table--fontify-text)
      (when (and restore-row-idx restore-col-idx)
        (grid-table--move-to-cell restore-row-idx restore-col-idx)
        (grid-table--highlight-cell restore-row-idx restore-col-idx)))))

(defun grid-table--fontify-text ()
  "Apply `fixed-pitch' face to all text in the buffer,
skipping regions with a `display' property (like images)."
  (interactive)
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next-change (next-single-property-change pos 'display nil (point-max))))
        (unless (get-text-property pos 'display)
          (add-face-text-property pos next-change 'fixed-pitch 'append))
        (setq pos next-change)))))

(define-derived-mode grid-table-mode special-mode "Grid Table"
  "Major mode for displaying and editing grid data."
  :group 'grid-table
  ;; Setup perfect display environment to eliminate image gaps
  (grid-table--setup-perfect-display-environment)
  (setq truncate-lines t)  ; Changed from nil to t to prevent line wrapping
  (setq auto-hscroll-mode t)  ; Enable automatic horizontal scrolling
  (setq hscroll-margin 0)  ; Allow cursor to reach window edge before scrolling
  (setq hscroll-step 1)  ; Smooth horizontal scrolling
  (setq buffer-read-only t)
  (setq header-line-format
        (propertize " Grid Table" 'face '(:weight bold)))
  (setq-local font-lock-defaults '(grid-table--font-lock-keywords))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'grid-table-recalculate-all)
    (define-key map (kbd "e") 'grid-table-edit-cell)
    (define-key map (kbd "<tab>") 'grid-table-next-cell)
    (define-key map (kbd "<backtab>") 'grid-table-previous-cell)
    (define-key map (kbd "n") 'grid-table-next-line)
    (define-key map (kbd "p") 'grid-table-previous-line)
    (define-key map (kbd "C-c t") 'grid-table-edit-title) ; New keybinding for editing title
    (define-key map (kbd "C-c r a") 'grid-table-add-row)
    (define-key map (kbd "C-c r d") 'grid-table-delete-row)
    (define-key map (kbd "C-c c a") 'grid-table-add-column)
    (define-key map (kbd "C-c c d") 'grid-table-delete-column)
    (define-key map (kbd "C-c C-w") 'grid-table-write-file)
    (define-key map (kbd "C-c C-f") 'grid-table-find-file)
    (define-key map (kbd "C-c s") 'grid-table-sort-column) ; New keybinding for sorting
    (define-key map (kbd "C-<next>") 'grid-table-sheet-next)
    (define-key map (kbd "C-<prior>") 'grid-table-sheet-previous)
    (define-key map (kbd "C-c w n") 'grid-table-sheet-new)
    (define-key map (kbd "C-c w r") 'grid-table-sheet-rename)
    (define-key map (kbd "C-c w d") 'grid-table-sheet-delete)
    (define-key map (kbd "C-c w s") 'grid-table-sheet-select)
    (define-key map (kbd "C-c w <") 'grid-table-sheet-move-left)
    (define-key map (kbd "C-c w >") 'grid-table-sheet-move-right)
    (define-key map (kbd "C-c w h") 'grid-table-toggle-header-row)
    (define-key map (kbd "C-c w u") 'grid-table-unfreeze-panes)
    (define-key map (kbd "C-c r i") 'grid-table-insert-row-above)
    (define-key map (kbd "C-c m") 'grid-table-merge-cells)
    (define-key map (kbd "C-c u") 'grid-table-unmerge-cell)
     (define-key map (kbd "C-c v") 'grid-table-view-chart)
     (define-key map (kbd "C-c i") 'grid-table-insert-chart)
     ;; Horizontal scrolling and navigation
     (define-key map (kbd "C-x <") 'scroll-left)
     (define-key map (kbd "C-x >") 'scroll-right)
     (define-key map (kbd "<right>") 'grid-table-cell-right)
     (define-key map (kbd "<left>") 'grid-table-cell-left)
     (define-key map (kbd "<down>") 'grid-table-next-line)
     (define-key map (kbd "<up>") 'grid-table-previous-line)
     (define-key map (kbd "C-f") 'forward-char)
     (define-key map (kbd "C-b") 'backward-char)
     (define-key map (kbd "C-a") 'grid-table-move-beginning-of-line)
     (define-key map (kbd "C-e") 'grid-table-move-end-of-line)
     (define-key map (kbd "M-<") 'beginning-of-buffer)
     (define-key map (kbd "M->") 'end-of-buffer)

     (use-local-map map)))

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
    (if (grid-table--etaf-p)
        ;; The ETAF renderer records the change for undo.
        (grid-table-etaf-set-cell display-row-idx (1- display-col-idx) value)
      (let ((set-raw-fn (gethash :set-raw-value-at grid-table--data-source)))
        (funcall set-raw-fn grid-table--data-source display-row-idx (1- display-col-idx) value)
        (grid-table-refresh)
        (grid-table--move-to-cell display-row-idx display-col-idx)
        (grid-table--highlight-cell display-row-idx display-col-idx)))))

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


(defun grid-table--test-newline-wrapping ()
  "Test function to verify newline-aware text wrapping."
  (interactive)
  (let ((test-cases
         '(("Short text" 20)
           ("This is a very long line that should be wrapped" 15)
           ("Line 1\nLine 2\nLine 3" 20)
           ("Short\nVery long line that needs to be wrapped at spaces" 15)
           ("First\nSecond line that is way too long and should wrap\nThird" 20))))
    (dolist (test-case test-cases)
      (let* ((text (car test-case))
             (width (cadr test-case))
             (result (grid-table--wrap-text text width)))
        (message "Text: %S, Width: %d" text width)
        (message "Result: %S" result)
        (message "Max line width: %d" (apply #'max (mapcar #'string-width result)))
        (message "")))))

(provide 'grid-table)

;;; grid-table.el ends here 
