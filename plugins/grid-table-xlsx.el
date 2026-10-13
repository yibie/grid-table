;;; grid-table-xlsx.el --- Read and write xlsx files for grid-table -*- lexical-binding: t; -*-

;;; Commentary:

;; This plugin opens one sheet of an xlsx workbook as a grid and writes a
;; grid back as a sheet of a workbook.  Values, formulas, merges, cell
;; formats and conditional formats travel both ways.
;;
;; The file format is handled by a dynamic module written in Rust, whose
;; source is in plugins/xlsx/.  A prebuilt grid-table-xlsx-module.dylib
;; (.so, .dll) next to this file is used when there is one for the
;; platform; otherwise build the module with
;; `grid-table-xlsx-build-module', which needs cargo.

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'color)
(require 'grid-table)
(require 'grid-table-plugins)

(declare-function grid-table-xlsx-module-sheets "grid-table-xlsx-module")
(declare-function grid-table-xlsx-module-read "grid-table-xlsx-module")
(declare-function grid-table-xlsx-module-write "grid-table-xlsx-module")

(defcustom grid-table-xlsx-module-file nil
  "File of the xlsx dynamic module, or nil to look in the usual places.
Those are the cargo build directory under plugins/xlsx/ and then the
prebuilt grid-table-xlsx-module file next to this plugin."
  :type '(choice (const :tag "Look in the usual places" nil) file)
  :group 'grid-table)

(defconst grid-table-xlsx-max-cells 2000000
  "Largest sheet, in cells, that the plugin opens.")

(defconst grid-table-xlsx--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this plugin.")

(defconst grid-table-xlsx--record (string #x1e)
  "Separator of the records handed to the module.")

(defconst grid-table-xlsx--field (string #x1f)
  "Separator of the fields of a record.")

(defconst grid-table-xlsx--number-regexp
  "\\`[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
  "Matches the text of a number.")

;;;----------------------------------------------------------------------
;;; The module
;;;----------------------------------------------------------------------

(defun grid-table-xlsx--module-candidates ()
  "Return the files the module may be in, most specific first.
A module built here comes before the prebuilt one: whoever built it
meant it to be used."
  (let ((suffix (or module-file-suffix ".so"))
        (build (expand-file-name "xlsx/target/release/"
                                 grid-table-xlsx--directory)))
    (delq nil
          (list grid-table-xlsx-module-file
                (expand-file-name (concat "libgrid_table_xlsx_module" suffix)
                                  build)
                (expand-file-name (concat "grid_table_xlsx_module" suffix)
                                  build)
                (expand-file-name (concat "grid-table-xlsx-module" suffix)
                                  grid-table-xlsx--directory)))))

(defun grid-table-xlsx--ensure-module ()
  "Load the xlsx module unless it is loaded; signal when there is none."
  (unless (featurep 'grid-table-xlsx-module)
    (unless (fboundp 'module-load)
      (user-error "This Emacs was built without dynamic module support"))
    (let ((file (cl-find-if #'file-exists-p
                            (grid-table-xlsx--module-candidates))))
      (unless file
        (user-error "No prebuilt xlsx module (grid-table-xlsx-module%s) for this platform; run M-x grid-table-xlsx-build-module"
                    (or module-file-suffix ".so")))
      (module-load file))))

(defun grid-table-xlsx-build-module ()
  "Build the xlsx module from plugins/xlsx/ with cargo."
  (interactive)
  (unless (executable-find "cargo")
    (user-error "Building the xlsx module needs cargo, the Rust build tool"))
  (let ((default-directory (expand-file-name "xlsx/"
                                             grid-table-xlsx--directory)))
    (compile "cargo build --release")))

;;;----------------------------------------------------------------------
;;; Cell references
;;;----------------------------------------------------------------------

(defun grid-table-xlsx--column-name (col)
  "Return the letters of the zero-based column COL."
  (let ((name ""))
    (setq col (1+ col))
    (while (> col 0)
      (setq name (concat (string (+ ?A (% (1- col) 26))) name)
            col (/ (1- col) 26)))
    name))

(defun grid-table-xlsx--cell-name (row col)
  "Return the A1 name of the zero-based ROW and COL."
  (format "%s%d" (grid-table-xlsx--column-name col) (1+ row)))

(defun grid-table-xlsx--range-name (top left bottom right)
  "Return the A1 name of the range from TOP, LEFT to BOTTOM, RIGHT."
  (format "%s:%s" (grid-table-xlsx--cell-name top left)
          (grid-table-xlsx--cell-name bottom right)))

(defun grid-table-xlsx--parse-cell (name)
  "Return (ROW . COL), zero-based, of the A1 cell NAME, or nil."
  (when (string-match "\\`\\$?\\([A-Za-z]\\{1,3\\}\\)\\$?\\([0-9]+\\)\\'" name)
    (let ((col 0))
      (mapc (lambda (letter) (setq col (+ (* col 26) (- letter ?A -1))))
            (upcase (match-string 1 name)))
      (cons (1- (string-to-number (match-string 2 name))) (1- col)))))

(defun grid-table-xlsx--parse-range (name)
  "Return (TOP LEFT BOTTOM RIGHT) of the A1 range or cell NAME, or nil."
  (let* ((ends (split-string name ":"))
         (start (grid-table-xlsx--parse-cell (car ends)))
         (end (if (cdr ends)
                  (grid-table-xlsx--parse-cell (cadr ends))
                start)))
    (when (and start end (<= (length ends) 2))
      (list (min (car start) (car end)) (min (cdr start) (cdr end))
            (max (car start) (car end)) (max (cdr start) (cdr end))))))

;;;----------------------------------------------------------------------
;;; Reading
;;;----------------------------------------------------------------------

(defun grid-table-xlsx-sheet-names (file)
  "Return the names of the sheets of the xlsx FILE."
  (grid-table-xlsx--ensure-module)
  (car (read-from-string
        (grid-table-xlsx-module-sheets (expand-file-name file)))))

(defun grid-table-xlsx--date-format-p (code)
  "Return non-nil when the number format CODE shows a date or a time."
  (let ((case-fold-search t))
    (string-match-p
     "[ymdhs]"
     (replace-regexp-in-string "\"[^\"]*\"\\|\\[[^]]*\\]\\|\\\\." "" code))))

(defun grid-table-xlsx--date-text (serial)
  "Return the date, time or both that the workbook number SERIAL stands for.
The date is written year first, whatever format the workbook shows it
in: a built-in date format follows the locale of the reader, and
10/12/2026 does not say which number is the month.  SERIAL counts days
from the end of 1899, as workbooks do by default."
  (let* ((days (floor serial))
         (seconds (round (* 86400 (- serial days))))
         (date (calendar-gregorian-from-absolute
                (+ days (calendar-absolute-from-gregorian '(12 30 1899)))))
         (day (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date)))
         (time (format "%02d:%02d:%02d" (/ seconds 3600)
                       (% (/ seconds 60) 60) (% seconds 60))))
    (cond ((zerop seconds) day)
          ((zerop days) time)
          (t (concat day " " time)))))

(defun grid-table-xlsx--raw (cell)
  "Return the raw input for CELL, a (ROW COL VALUE . PROPERTIES) entry.
grid-table has no date type, so a date comes in as text."
  (let ((value (nth 2 cell))
        (format (plist-get (nthcdr 3 cell) :format)))
    (if (and format
             (grid-table-xlsx--date-format-p format)
             (string-match-p grid-table-xlsx--number-regexp value))
        (grid-table-xlsx--date-text (string-to-number value))
      value)))

(defun grid-table-xlsx--style (properties)
  "Return the grid-table style for the module's cell PROPERTIES.
Black text is the default of a workbook and is not taken as a color."
  (let ((color (plist-get properties :color))
        (fill (plist-get properties :fill))
        (align (plist-get properties :align)))
    (append (and (plist-get properties :bold) (list :bold t))
            (and (plist-get properties :italic) (list :italic t))
            (and color (not (equal color "000000"))
                 (list :color (concat "#" color)))
            (and fill (list :background (concat "#" fill)))
            (and align (list :align (intern align))))))

(defun grid-table-xlsx--unquote (text)
  "Return TEXT without the double quotes of a formula string."
  (if (string-match "\\`\"\\(\\(?:.\\|\n\\)*\\)\"\\'" text)
      (replace-regexp-in-string "\"\"" "\"" (match-string 1 text))
    text))

(defun grid-table-xlsx--rule-test (rule)
  "Return (OP . VALUE) for the module's conditional format RULE, or nil."
  (pcase-let ((`(,_range ,type ,operator ,formula ,text . ,_) rule))
    (pcase type
      ("CellIs"
       (when-let* ((op (cdr (assoc operator
                                   '(("GreaterThan" . ">")
                                     ("LessThan" . "<")
                                     ("GreaterThanOrEqual" . ">=")
                                     ("LessThanOrEqual" . "<=")
                                     ("Equal" . "=")
                                     ("NotEqual" . "<>"))))))
         (cons op (grid-table-xlsx--unquote formula))))
      ;; The searched text is an optional attribute; the formula always
      ;; holds it, as SEARCH("text",CELL).
      ("ContainsText"
       (cond ((not (string-empty-p text)) (cons "contains" text))
             ((string-match "SEARCH(\\(\"\\(?:[^\"]\\|\"\"\\)*\"\\)," formula)
              (cons "contains"
                    (grid-table-xlsx--unquote (match-string 1 formula))))))
      ("ContainsBlanks" (cons "empty" ""))
      ("ContainsErrors" (cons "error" "")))))

(defun grid-table-xlsx--rules (conditionals)
  "Return (RULES . SKIPPED) for the module's CONDITIONALS.
RULES are grid-table rules, the one that wins last; SKIPPED counts the
conditional formats grid-table has no counterpart for."
  (let ((skipped 0)
        rules)
    ;; In a workbook the lowest priority number wins.
    (dolist (rule (sort (copy-sequence conditionals)
                        (lambda (a b) (> (nth 5 a) (nth 5 b)))))
      (let ((test (grid-table-xlsx--rule-test rule))
            (style (grid-table-xlsx--style (nthcdr 6 rule))))
        (if (not (and test style))
            (setq skipped (1+ skipped))
          (dolist (name (split-string (car rule) " " t))
            (when-let* ((range (grid-table-xlsx--parse-range name)))
              (push (append range (list (car test) (cdr test) style))
                    rules))))))
    (cons (nreverse rules) skipped)))

(defun grid-table-xlsx-create-data-source (file &optional sheet)
  "Return a data source holding sheet SHEET of the xlsx FILE.
SHEET defaults to the first sheet.  The first row of the sheet becomes
the header row."
  (grid-table-xlsx--ensure-module)
  (setq file (expand-file-name file))
  (let* ((sheet (or sheet (car (grid-table-xlsx-sheet-names file))
                    (error "No sheet in %s" file)))
         (data (car (read-from-string
                     (grid-table-xlsx-module-read file sheet))))
         (cells (plist-get data :cells))
         (merges (delq nil (mapcar #'grid-table-xlsx--parse-range
                                   (plist-get data :merges))))
         (rows 1)
         (cols 1))
    (dolist (cell cells)
      (setq rows (max rows (1+ (nth 0 cell)))
            cols (max cols (1+ (nth 1 cell)))))
    (dolist (merge merges)
      (setq rows (max rows (1+ (nth 2 merge)))
            cols (max cols (1+ (nth 3 merge)))))
    (when (> (* rows cols) grid-table-xlsx-max-cells)
      (error "Sheet %s spans %d by %d cells, more than grid-table opens"
             sheet rows cols))
    (let ((table (make-vector rows nil)))
      (dotimes (row rows)
        (aset table row (make-vector cols "")))
      (dolist (cell cells)
        (aset (aref table (nth 0 cell)) (nth 1 cell)
              (grid-table-xlsx--raw cell)))
      (let* ((values (mapcar (lambda (row) (append row nil)) table))
             (source (make-default-data-source
                      (copy-sequence (car values)) values sheet))
             (model (gethash :model source))
             (rules (grid-table-xlsx--rules (plist-get data :conditionals))))
        (when merges
          (set-grid-data-model-merges
           model (grid-model-validate-merges model merges t)))
        (dolist (cell cells)
          (when-let* ((style (grid-table-xlsx--style (nthcdr 3 cell))))
            (grid-model-set-style model (nth 0 cell) (nth 1 cell) style)))
        (grid-model-set-conditionals model (car rules))
        (puthash :xlsx-file file source)
        (puthash :xlsx-sheet sheet source)
        (puthash :xlsx-skipped-conditionals (cdr rules) source)
        source))))

;;;###autoload
(defun grid-table-xlsx-find-file (file &optional sheet)
  "Open sheet SHEET of the xlsx FILE as a grid.
Interactively, ask which sheet when the workbook has several.  The grid
is not tied to the file: `grid-table-xlsx-export' writes it back."
  (interactive
   (let* ((file (read-file-name "Find xlsx file: " nil nil t))
          (names (grid-table-xlsx-sheet-names file)))
     (list file
           (if (cdr names)
               (completing-read "Sheet: " names nil t nil nil (car names))
             (car names)))))
  (let* ((source (grid-table-xlsx-create-data-source file sheet))
         (sheet (gethash :xlsx-sheet source))
         (skipped (gethash :xlsx-skipped-conditionals source))
         (buffer (generate-new-buffer
                  (format "%s[%s]" (file-name-nondirectory file) sheet))))
    (with-current-buffer buffer
      (grid-table--enter-classic-mode)
      (setq-local grid-table--title sheet)
      (grid-table--show-grid-in-current-buffer source))
    (switch-to-buffer buffer)
    (when (> skipped 0)
      (message "%d conditional format%s grid-table has no counterpart for %s left out"
               skipped (if (= skipped 1) "" "s")
               (if (= skipped 1) "was" "were")))
    buffer))

;;;----------------------------------------------------------------------
;;; Writing
;;;----------------------------------------------------------------------

(defconst grid-table-xlsx--own-formula-regexp
  (concat "\\`=[ \t]*"
          (regexp-opt '("BARPLOT" "COLUMNPLOT" "HISTOGRAM" "BOXPLOT"
                        "MULTILINE" "DENSITY" "COUNTPLOT" "LINEPLOT"
                        "SCATTER" "SPARKLINE" "LINECHART"))
          "[ \t]*(")
  "Matches a chart formula, which only grid-table can evaluate.")

(defun grid-table-xlsx--clean (text)
  "Return TEXT without the characters that separate records and fields."
  (replace-regexp-in-string "[\x1e\x1f]" "" text))

(defun grid-table-xlsx--rgb (color)
  "Return COLOR, a name or #RRGGBB, as RRGGBB, or \"\" when unknown."
  (cond
   ((not (stringp color)) "")
   ((string-match "\\`#\\([[:xdigit:]]\\{6\\}\\)\\'" color)
    (upcase (match-string 1 color)))
   (t (let ((rgb (ignore-errors (color-name-to-rgb color))))
        (if rgb
            (apply #'format "%02X%02X%02X"
                   (mapcar (lambda (part) (round (* 255 part))) rgb))
          "")))))

(defun grid-table-xlsx--style-fields (style)
  "Return the fields that describe the grid-table STYLE to the module."
  (list (if (plist-get style :bold) "1" "")
        (if (plist-get style :italic) "1" "")
        (grid-table-xlsx--rgb (plist-get style :color))
        (grid-table-xlsx--rgb (plist-get style :background))
        (if-let* ((align (plist-get style :align)))
            (format "%s" align)
          "")))

(defun grid-table-xlsx--cell-record (row col raw style)
  "Return the record for the cell at ROW and COL, or nil for nothing.
RAW is its raw input and STYLE its style."
  (let* ((case-fold-search t)
         (text (grid-table-xlsx--clean (format "%s" (or raw ""))))
         (kind (cond
                ((and (string-prefix-p "=" text) (> (length text) 1)
                      (not (string-match-p
                            grid-table-xlsx--own-formula-regexp text)))
                 (setq text (substring text 1))
                 "f")
                ((string-match-p grid-table-xlsx--number-regexp text) "n")
                ((member text '("TRUE" "FALSE")) "b")
                (t "s"))))
    (unless (and (string-empty-p text) (null style))
      (mapconcat #'identity
                 (append (list "C" (number-to-string row)
                               (number-to-string col) kind text)
                         (grid-table-xlsx--style-fields style))
                 grid-table-xlsx--field))))

(defun grid-table-xlsx--rule-record (rule)
  "Return the record for the conditional format RULE."
  (pcase-let* ((`(,top ,left ,bottom ,right ,op ,value ,style) rule)
               (first (grid-table-xlsx--cell-name top left))
               (value (grid-table-xlsx--clean (format "%s" (or value ""))))
               (quoted (format "\"%s\""
                               (replace-regexp-in-string "\"" "\"\"" value))))
    (mapconcat
     #'identity
     (append
      (list "F" (grid-table-xlsx--range-name top left bottom right) op
            (pcase op
              ("contains" (format "NOT(ISERROR(SEARCH(%s,%s)))" quoted first))
              ("empty" (format "LEN(TRIM(%s))=0" first))
              ("error" (format "ISERROR(%s)" first))
              (_ (if (string-match-p grid-table-xlsx--number-regexp value)
                     value
                   quoted)))
            value)
      ;; A conditional format has no alignment.
      (butlast (grid-table-xlsx--style-fields style)))
     grid-table-xlsx--field)))

(defun grid-table-xlsx--payload (source)
  "Return the records that describe the data source SOURCE."
  (let ((rows (grid-get-row-count source))
        (cols (grid-get-column-count source))
        records)
    (dotimes (row rows)
      (dotimes (col cols)
        (when-let* ((record (grid-table-xlsx--cell-record
                             row col
                             (grid-get-raw-value-at source row col)
                             (grid-get-style-at source row col))))
          (push record records))))
    (dolist (merge (grid-get-merges source))
      (push (concat "M" grid-table-xlsx--field
                    (apply #'grid-table-xlsx--range-name merge))
            records))
    (dolist (rule (grid-get-conditional-formats source))
      (push (grid-table-xlsx--rule-record rule) records))
    (mapconcat #'identity (nreverse records) grid-table-xlsx--record)))

(defun grid-table-xlsx--sheet-name (name)
  "Return NAME as a name a workbook accepts for a sheet."
  (let ((name (string-trim
               (replace-regexp-in-string "[][:*?/\\\\]" " " (or name "")))))
    (cond ((string-empty-p name) "Sheet1")
          ((> (length name) 31) (substring name 0 31))
          (t name))))

(defun grid-table-xlsx-write-data-source (source file &optional sheet)
  "Write the data source SOURCE as sheet SHEET of the xlsx FILE.
When FILE exists its other sheets are kept and SHEET is replaced or
added.  SHEET defaults to the sheet SOURCE came from, else its title."
  (grid-table-xlsx--ensure-module)
  (setq file (expand-file-name file))
  (let* ((sheet (grid-table-xlsx--sheet-name
                 (or sheet (gethash :xlsx-sheet source)
                     (gethash :title source))))
         ;; Write beside FILE and rename, so that a failure leaves it whole.
         (temporary (make-temp-name (concat file ".tmp"))))
    (unwind-protect
        (progn
          (grid-table-xlsx-module-write
           temporary (and (file-exists-p file) file) sheet
           (grid-table-xlsx--payload source))
          (rename-file temporary file t))
      (when (file-exists-p temporary)
        (delete-file temporary)))
    (puthash :xlsx-file file source)
    (puthash :xlsx-sheet sheet source)
    sheet))

;;;###autoload
(defun grid-table-xlsx-export (file &optional sheet)
  "Write the current grid as sheet SHEET of the xlsx FILE.
The file and sheet the grid was opened from are the defaults.  An
existing workbook keeps its other sheets."
  (interactive
   (let* ((source (or grid-table--data-source
                      (user-error "No grid in this buffer")))
          (origin (gethash :xlsx-file source))
          (file (expand-file-name
                 (read-file-name "Export to xlsx file: "
                                 (and origin (file-name-directory origin))
                                 origin nil
                                 (and origin
                                      (file-name-nondirectory origin)))))
          (default (grid-table-xlsx--sheet-name
                    (or (gethash :xlsx-sheet source) grid-table--title)))
          (sheet (read-string (format "Sheet (default %s): " default)
                              nil nil default)))
     (list file sheet)))
  (unless grid-table--data-source
    (user-error "No grid in this buffer"))
  (unless (string-suffix-p ".xlsx" file t)
    (setq file (concat file ".xlsx")))
  (let ((sheet (grid-table-xlsx--sheet-name
                (or sheet (gethash :xlsx-sheet grid-table--data-source)
                    grid-table--title))))
    (when (and (called-interactively-p 'interactive)
               (file-exists-p file)
               (not (and (equal (expand-file-name file)
                                (gethash :xlsx-file grid-table--data-source))
                         (equal sheet (gethash :xlsx-sheet
                                               grid-table--data-source))))
               (member sheet (grid-table-xlsx-sheet-names file))
               (not (y-or-n-p (format "Replace sheet %s of %s? " sheet
                                      (file-name-nondirectory file)))))
      (user-error "Export canceled"))
    (grid-table-xlsx-write-data-source grid-table--data-source file sheet)
    (message "Sheet %s written to %s" sheet (abbreviate-file-name file))))

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun grid-table-xlsx-init ()
  "Register the xlsx plugin."
  (grid-table-register-plugin 'xlsx)
  (grid-table-register-data-source-type
   'xlsx #'grid-table-xlsx-create-data-source))

(grid-table-xlsx-init)

(provide 'grid-table-xlsx)

;;; grid-table-xlsx.el ends here
