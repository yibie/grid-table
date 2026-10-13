;;; grid-table-xlsx.el --- Read and write xlsx files for grid-table -*- lexical-binding: t; -*-

;;; Commentary:

;; This plugin opens an xlsx workbook as a grid with a sheet for each of
;; its sheets, and writes a grid back as a workbook.  Values, formulas,
;; merges, cell formats and conditional formats travel both ways.
;;
;; The file format is handled by a dynamic module written in Rust, whose
;; source is in plugins/xlsx/.  A prebuilt grid-table-xlsx-module.dylib
;; (.so, .dll) next to this file is used when there is one for the
;; platform; otherwise build the module with
;; `grid-table-xlsx-build-module', which needs cargo.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'grid-table)
(require 'grid-table-plugins)

(declare-function grid-table-xlsx-module-sheets "grid-table-xlsx-module")
(declare-function grid-table-xlsx-module-read "grid-table-xlsx-module")
(declare-function grid-table-xlsx-module-write "grid-table-xlsx-module")
(declare-function grid-table-xlsx-module-prune "grid-table-xlsx-module")

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

(defun grid-table-xlsx--raw (cell)
  "Return the raw input for CELL, a (ROW COL VALUE . PROPERTIES) entry.
Text that reads as a number is marked with an apostrophe, so it stays
text."
  (let ((value (nth 2 cell)))
    (if (plist-get (nthcdr 3 cell) :text)
        (concat "'" value)
      value)))

(defun grid-table-xlsx--style (properties)
  "Return the grid-table style for the module's cell PROPERTIES.
Black text is the default of a workbook and is not taken as a color.
A number format the grid has no counterpart for is kept as it is, to be
written back."
  (let ((color (plist-get properties :color))
        (fill (plist-get properties :fill))
        (align (plist-get properties :align))
        (format (grid-format-from-code (plist-get properties :format))))
    (append (and (plist-get properties :bold) (list :bold t))
            (and (plist-get properties :italic) (list :italic t))
            (and color (not (equal color "000000"))
                 (list :color (concat "#" color)))
            (and fill (list :background (concat "#" fill)))
            (and align (list :align (intern align)))
            (and format (list :format format)))))

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
(defun grid-table-xlsx-create-workbook (file)
  "Return a workbook holding every sheet of the xlsx FILE."
  (grid-workbook-create
   (mapcar (lambda (name)
             (cons name (grid-table-xlsx-create-data-source file name)))
           (or (grid-table-xlsx-sheet-names file)
               (error "No sheet in %s" file)))))

;;;###autoload
(defun grid-table-xlsx-find-file (file &optional sheet)
  "Open the xlsx FILE as a grid, showing its sheet SHEET.
Every sheet of the workbook is opened; SHEET defaults to the first.  The
grid is not tied to the file: `grid-table-xlsx-export' writes it back."
  (interactive (list (read-file-name "Find xlsx file: " nil nil t)))
  (let* ((workbook (grid-table-xlsx-create-workbook file))
         (sheets (grid-workbook-sheets workbook))
         (source (or (and sheet (grid-workbook-sheet workbook sheet))
                     (car sheets)))
         (skipped (apply #'+ (mapcar (lambda (sheet)
                                       (gethash :xlsx-skipped-conditionals
                                                sheet))
                                     sheets)))
         (buffer (generate-new-buffer (file-name-nondirectory file))))
    (with-current-buffer buffer
      (grid-table--enter-classic-mode)
      (setq-local grid-table--title (gethash :title source))
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
          "")
        (or (grid-format-to-code (plist-get style :format)) "")))

(defun grid-table-xlsx--cell-record (row col raw style)
  "Return the record for the cell at ROW and COL, or nil for nothing.
RAW is its raw input and STYLE its style."
  (let* ((case-fold-search t)
         (text (grid-table-xlsx--clean (format "%s" (or raw ""))))
         (kind (cond
                ;; Text by its apostrophe or by its format
                ((string-prefix-p "'" text)
                 (setq text (substring text 1))
                 "s")
                ((eq (car-safe (plist-get style :format)) 'text) "s")
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
      ;; A conditional format has no alignment and no number format.
      (butlast (grid-table-xlsx--style-fields style) 2))
     grid-table-xlsx--field)))

(defun grid-table-xlsx--payload (source)
  "Return the records that describe the data source SOURCE."
  (let ((rows (grid-get-row-count source))
        (cols (grid-get-column-count source))
        records)
    (dotimes (row rows)
      (dotimes (col cols)
        (when-let* ((record
                     (grid-table-xlsx--cell-record
                      row col
                      (grid-get-raw-value-at source row col)
                      ;; A date formula is shown as a date without having
                      ;; the format; a workbook needs to be told.
                      (let ((style (grid-get-style-at source row col))
                            (format (grid-table-cell-format source row col)))
                        (if (and format (not (plist-get style :format)))
                            (plist-put (copy-sequence style) :format format)
                          style)))))
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

(defun grid-table-xlsx-write-workbook (workbook file)
  "Write every sheet of WORKBOOK to the xlsx FILE.
When FILE exists, sheets of it with other names are kept, unless
WORKBOOK was opened from FILE: then FILE ends up with WORKBOOK's sheets
and no others."
  (grid-table-xlsx--ensure-module)
  (setq file (expand-file-name file))
  (let* ((sheets (grid-workbook-sheets workbook))
         ;; Sheets added since have no file yet; one that came from FILE
         ;; says the workbook did.
         (own (cl-some (lambda (sheet) (equal (gethash :xlsx-file sheet) file))
                       sheets))
         ;; Each sheet is written from the file the one before it produced,
         ;; into another, and the last is renamed over FILE.
         (written (and (file-exists-p file) file))
         (spare (list (make-temp-name (concat file ".tmp"))
                      (make-temp-name (concat file ".tmp")))))
    (unwind-protect
        (progn
          (dolist (sheet sheets)
            (let ((target (car spare)))
              (grid-table-xlsx-module-write
               target written (grid-workbook-sheet-name sheet)
               (grid-table-xlsx--payload sheet))
              (setq written target
                    spare (reverse spare))))
          (when own
            (grid-table-xlsx-module-prune
             written
             (mapconcat #'grid-workbook-sheet-name sheets
                        grid-table-xlsx--record)))
          (rename-file written file t))
      (dolist (temporary spare)
        (when (file-exists-p temporary)
          (delete-file temporary))))
    (dolist (sheet sheets)
      (puthash :xlsx-file file sheet)
      (puthash :xlsx-sheet (grid-workbook-sheet-name sheet) sheet))
    (length sheets)))

;;;###autoload
(defun grid-table-xlsx-export (file &optional sheet)
  "Write the current grid to the xlsx FILE.
A grid with several sheets is written whole.  A grid that stands alone
becomes the sheet SHEET of FILE.  The file the grid was opened from is
the default, and an existing workbook keeps the sheets the grid does not
replace."
  (interactive
   (let* ((source (or grid-table--data-source
                      (user-error "No grid in this buffer")))
          (origin (gethash :xlsx-file source))
          (file (expand-file-name
                 (read-file-name "Export to xlsx file: "
                                 (and origin (file-name-directory origin))
                                 origin nil
                                 (and origin
                                      (file-name-nondirectory origin))))))
     (list file
           (unless (cdr (grid-table-sheets))
             (let ((default (grid-table-xlsx--sheet-name
                             (or (grid-workbook-sheet-name source)
                                 (gethash :xlsx-sheet source)
                                 grid-table--title))))
               (read-string (format "Sheet (default %s): " default)
                            nil nil default))))))
  (unless grid-table--data-source
    (user-error "No grid in this buffer"))
  (unless (string-suffix-p ".xlsx" file t)
    (setq file (concat file ".xlsx")))
  (setq file (expand-file-name file))
  (let* ((source grid-table--data-source)
         (sheets (grid-table-sheets))
         (names (if (cdr sheets)
                    (mapcar #'grid-workbook-sheet-name sheets)
                  (list (grid-table-xlsx--sheet-name
                         (or sheet (grid-workbook-sheet-name source)
                             (gethash :xlsx-sheet source)
                             grid-table--title)))))
         (origin (equal file (gethash :xlsx-file source))))
    (when (and (called-interactively-p 'interactive)
               (file-exists-p file)
               (not origin)
               (cl-intersection names (grid-table-xlsx-sheet-names file)
                                :test #'string=)
               (not (y-or-n-p (format "Replace sheet%s %s of %s? "
                                      (if (cdr names) "s" "")
                                      (string-join names ", ")
                                      (file-name-nondirectory file)))))
      (user-error "Export canceled"))
    (if (cdr sheets)
        (grid-table-xlsx-write-workbook (grid-workbook-of source) file)
      (grid-table-xlsx-write-data-source source file (car names)))
    (message "%s written to %s"
             (if (cdr names)
                 (format "%d sheets" (length names))
               (format "Sheet %s" (car names)))
             (abbreviate-file-name file))))

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
