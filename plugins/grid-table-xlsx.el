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
(declare-function grid-table-xlsx-module-names "grid-table-xlsx-module")
(declare-function grid-table-xlsx-module-set-names "grid-table-xlsx-module")

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

(defconst grid-table-xlsx--max-cols 16384
  "How many columns a worksheet can have.")

(defconst grid-table-xlsx--default-width 8.43
  "Width in characters of a column a workbook has not sized.")

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

(defconst grid-table-xlsx--future-functions
  (append
   '(("FILTER" . "_xlfn._xlws.") ("SORT" . "_xlfn._xlws."))
   (mapcar
    (lambda (name) (cons name "_xlfn."))
    '("ACOT" "ACOTH" "AGGREGATE" "ANCHORARRAY" "ARABIC" "ARRAYTOTEXT" "BASE"
      "BETA.DIST" "BETA.INV" "BINOM.DIST" "BINOM.DIST.RANGE" "BINOM.INV"
      "BITAND" "BITLSHIFT" "BITOR" "BITRSHIFT" "BITXOR" "BYCOL" "BYROW"
      "CEILING.MATH" "CEILING.PRECISE" "CHISQ.DIST" "CHISQ.DIST.RT"
      "CHISQ.INV" "CHISQ.INV.RT" "CHISQ.TEST" "CHOOSECOLS" "CHOOSEROWS"
      "COMBINA" "CONCAT" "CONFIDENCE.NORM" "CONFIDENCE.T" "COT" "COTH"
      "COVARIANCE.P" "COVARIANCE.S" "CSC" "CSCH" "DAYS" "DECIMAL" "DROP"
      "ERF.PRECISE" "ERFC.PRECISE" "EXPAND" "EXPON.DIST" "F.DIST" "F.DIST.RT"
      "F.INV" "F.INV.RT" "F.TEST" "FILTERXML" "FLOOR.MATH" "FLOOR.PRECISE"
      "FORECAST.ETS" "FORECAST.ETS.CONFINT" "FORECAST.ETS.SEASONALITY"
      "FORECAST.ETS.STAT" "FORECAST.LINEAR" "FORMULATEXT" "GAMMA" "GAMMA.DIST"
      "GAMMA.INV" "GAMMALN.PRECISE" "GAUSS" "GROUPBY" "HSTACK" "HYPGEOM.DIST"
      "IFNA" "IFS" "IMCOSH" "IMCOT" "IMCSC" "IMCSCH" "IMSEC" "IMSECH" "IMSINH"
      "IMTAN" "ISFORMULA" "ISOMITTED" "ISOWEEKNUM" "LAMBDA" "LET"
      "LOGNORM.DIST" "LOGNORM.INV" "MAKEARRAY" "MAP" "MAXIFS" "MINIFS"
      "MODE.MULT" "MODE.SNGL" "MUNIT" "NEGBINOM.DIST" "NETWORKDAYS.INTL"
      "NORM.DIST" "NORM.INV" "NORM.S.DIST" "NORM.S.INV" "NUMBERVALUE"
      "PDURATION" "PERCENTILE.EXC" "PERCENTILE.INC" "PERCENTOF"
      "PERCENTRANK.EXC" "PERCENTRANK.INC" "PERMUTATIONA" "PHI" "PIVOTBY"
      "POISSON.DIST" "QUARTILE.EXC" "QUARTILE.INC" "RANDARRAY" "RANK.AVG"
      "RANK.EQ" "REDUCE" "REGEXEXTRACT" "REGEXREPLACE" "REGEXTEST" "RRI"
      "SCAN" "SEC" "SECH" "SEQUENCE" "SHEET" "SHEETS" "SINGLE" "SKEW.P"
      "SORTBY" "STDEV.P" "STDEV.S" "SWITCH" "T.DIST" "T.DIST.2T" "T.DIST.RT"
      "T.INV" "T.INV.2T" "T.TEST" "TAKE" "TEXTAFTER" "TEXTBEFORE" "TEXTJOIN"
      "TEXTSPLIT" "TOCOL" "TOROW" "TRIMRANGE" "UNICHAR" "UNICODE" "UNIQUE"
      "VALUETOTEXT" "VAR.P" "VAR.S" "VSTACK" "WEIBULL.DIST" "WORKDAY.INTL"
      "WRAPCOLS" "WRAPROWS" "XLOOKUP" "XMATCH" "XOR" "Z.TEST")))
  "Functions a workbook names with a prefix, as Excel 2007 did not know them.
Excel and LibreOffice read the bare name as an unknown function.")

(defun grid-table-xlsx--outside-strings (formula function)
  "Return FORMULA with FUNCTION applied to each part outside its strings.
A string is in double quotes, with a doubled quote inside it."
  (let ((parts nil) (start 0))
    (while (string-match "\"\\(?:[^\"]\\|\"\"\\)*\"" formula start)
      ;; FUNCTION may match too, so the string's bounds are taken first.
      (let ((from (match-beginning 0)) (to (match-end 0)))
        (push (funcall function (substring formula start from)) parts)
        (push (substring formula from to) parts)
        (setq start to)))
    (push (funcall function (substring formula start)) parts)
    (apply #'concat (nreverse parts))))

(defun grid-table-xlsx--operand-end (formula start)
  "Return where the operand at START in FORMULA ends.
It is a reference, a name, a function call or a group in parentheses."
  (let ((pos start) (end (length formula)) (done nil))
    (while (and (not done) (< pos end))
      (let ((char (aref formula pos)))
        (cond ((eq char ?\()
               (setq pos (1+ (or (grid-calc-closing-paren formula pos) (1- end)))))
              ((eq char ?\[)
               (let ((depth 0))
                 (while (and (< pos end)
                             (progn (pcase (aref formula pos)
                                      (?\[ (setq depth (1+ depth)))
                                      (?\] (setq depth (1- depth))))
                                    (> depth 0)))
                   (setq pos (1+ pos)))
                 (setq pos (1+ pos))))
              ((eq char ?')
               (setq pos (1+ pos))
               (while (and (< pos end)
                           (or (/= (aref formula pos) ?')
                               (and (< (1+ pos) end) (= (aref formula (1+ pos)) ?')
                                    (setq pos (1+ pos)))))
                 (setq pos (1+ pos)))
               (setq pos (1+ pos)))
              ((string-match-p "[[:alnum:]_.$!:\\]" (string char))
               (setq pos (1+ pos)))
              (t (setq done t)))))
    pos))

(defun grid-table-xlsx--outside-brackets (text function)
  "Return TEXT with FUNCTION applied to each part outside square brackets.
What is in brackets names a table's columns."
  (let ((parts nil) (start 0) (pos 0) (depth 0))
    (while (< pos (length text))
      (pcase (aref text pos)
        (?\[ (when (= depth 0)
               (push (funcall function (substring text start pos)) parts)
               (setq start pos))
             (setq depth (1+ depth)))
        (?\] (when (> depth 0)
               (setq depth (1- depth))
               (when (= depth 0)
                 (push (substring text start (1+ pos)) parts)
                 (setq start (1+ pos))))))
      (setq pos (1+ pos)))
    (push (if (> depth 0) (substring text start)
            (funcall function (substring text start)))
          parts)
    (apply #'concat (nreverse parts))))

(defun grid-table-xlsx--at-as-single (formula)
  "Return FORMULA with each @X written SINGLE(X), as a workbook keeps it.
An @ inside a table's brackets, as in T[@Qty], names the row instead."
  (let ((pos 0) (quote nil) (depth 0))
    (while (< pos (length formula))
      (let ((char (aref formula pos)))
        (cond (quote (when (eq char quote) (setq quote nil)))
              ((memq char '(?\" ?')) (setq quote char))
              ((eq char ?\[) (setq depth (1+ depth)))
              ((eq char ?\]) (setq depth (max 0 (1- depth))))
              ((and (eq char ?@) (= depth 0))
               (let ((end (grid-table-xlsx--operand-end formula (1+ pos))))
                 (setq formula (concat (substring formula 0 pos)
                                       "SINGLE(" (substring formula (1+ pos) end) ")"
                                       (substring formula end))))))
        (setq pos (1+ pos))))
    formula))

(defun grid-table-xlsx--spill-as-anchor (formula)
  "Return FORMULA with each A1# written ANCHORARRAY(A1), as a workbook
keeps it.  A # in a string or a table's brackets stays as it is."
  (let ((pos 0) (quote nil) (depth 0)
        (reference "\\(?:\\(?:'\\(?:[^']\\|''\\)+'\\|[A-Za-z0-9_.]+\\)!\\)?\\$?[A-Za-z]\\{1,3\\}\\$?[0-9]+\\'"))
    (while (< pos (length formula))
      (let ((char (aref formula pos)))
        (cond (quote (when (eq char quote) (setq quote nil)))
              ((eq char ?\") (setq quote char))
              ((eq char ?\[) (setq depth (1+ depth)))
              ((eq char ?\]) (setq depth (max 0 (1- depth))))
              ((and (eq char ?#) (= depth 0)
                    (string-match reference (substring formula 0 pos)))
               (let ((start (match-beginning 0)))
                 (setq formula (concat (substring formula 0 start)
                                       "ANCHORARRAY(" (substring formula start pos) ")"
                                       (substring formula (1+ pos)))
                       pos (+ pos (length "ANCHORARRAY()") -1)))))
        (setq pos (1+ pos))))
    formula))

(defun grid-table-xlsx--call-arguments (formula open)
  "Return the arguments of the call whose parenthesis is at OPEN in FORMULA.
Each is the text between its commas, as is."
  (let ((close (or (grid-calc-closing-paren formula open) (length formula)))
        (pos (1+ open)) (start (1+ open)) (depth 0) (quote nil) (arguments nil))
    (while (< pos close)
      (let ((char (aref formula pos)))
        (cond (quote (when (eq char quote) (setq quote nil)))
              ((memq char '(?\" ?')) (setq quote char))
              ((memq char '(?\( ?\{ ?\[)) (setq depth (1+ depth)))
              ((memq char '(?\) ?\} ?\])) (setq depth (1- depth)))
              ((and (eq char ?,) (= depth 0))
               (push (substring formula start pos) arguments)
               (setq start (1+ pos)))))
      (setq pos (1+ pos)))
    (nreverse (cons (substring formula start close) arguments))))

(defun grid-table-xlsx--parameter-names (formula)
  "Return the names the LET and LAMBDA calls of FORMULA declare."
  (let ((case-fold-search t) (start 0) (names nil))
    (while (string-match "\\(?:\\`\\|[^[:alnum:]_.]\\)\\(LET\\|LAMBDA\\)(" formula start)
      (let* ((let-p (string-equal (upcase (match-string 1 formula)) "LET"))
             (arguments (butlast (grid-table-xlsx--call-arguments
                                  formula (1- (match-end 0))))))
        (cl-loop for argument in arguments
                 for index from 0
                 for name = (string-trim argument)
                 when (and (or (not let-p) (cl-evenp index))
                           (string-match-p "\\`[[:alpha:]_][[:alnum:]_.]*\\'" name))
                 do (cl-pushnew (downcase name) names :test #'equal))
        (setq start (match-end 0))))
    names))

(defun grid-table-xlsx--prefix-functions (formula)
  "Return FORMULA, without its =, naming newer functions as a workbook does.
@ becomes SINGLE, A1# ANCHORARRAY(A1), and the names LET and LAMBDA
declare take _xlpm."
  (let* ((case-fold-search t)
         (formula (grid-table-xlsx--at-as-single
                   (grid-table-xlsx--spill-as-anchor formula)))
         (parameters (grid-table-xlsx--parameter-names formula)))
    (grid-table-xlsx--outside-strings
     formula
     (lambda (text)
       (grid-table-xlsx--outside-brackets
        text
        (lambda (text)
          (let ((pattern "[[:alpha:]_][[:alnum:]_.]*")
                (parts nil) (start 0))
            (while (string-match pattern text start)
              (let* ((at (match-beginning 0))
                     (name (match-string 0 text))
                     (call (and (< (match-end 0) (length text))
                                (eq (aref text (match-end 0)) ?\()))
                     ;; A name only stands alone, not inside a longer one,
                     ;; after a prefix it already has, or naming a sheet.
                     (alone (and (or (= at 0)
                                     (not (memq (aref text (1- at)) '(?$ ?! ?'))))
                                 (not (and (< (match-end 0) (length text))
                                           (eq (aref text (match-end 0)) ?!)))))
                     (prefix (and alone
                                  (if call
                                      (cdr (assoc (upcase name)
                                                  grid-table-xlsx--future-functions))
                                    (and (member (downcase name) parameters)
                                         "_xlpm.")))))
                (push (substring text start at) parts)
                (when prefix (push prefix parts))
                (push name parts)
                (setq start (match-end 0))))
            (push (substring text start) parts)
            (apply #'concat (nreverse parts)))))))))

(defun grid-table-xlsx--unprefix-functions (formula)
  "Return FORMULA with a workbook's function and parameter prefixes off.
SINGLE(X) and ANCHORARRAY(A1) read as @X and A1#, as Excel shows them."
  (let ((case-fold-search t))
    (grid-calc-excel-formula-text
     (grid-table-xlsx--outside-strings
      formula
      (lambda (text)
        (replace-regexp-in-string "_xlfn\\.\\(?:_xlws\\.\\)?\\|_xlpm\\." "" text t t))))))

(defconst grid-table-xlsx--own-formula-regexp
  (concat "\\`=[ \t]*"
          (regexp-opt '("BARPLOT" "COLUMNPLOT" "HISTOGRAM" "BOXPLOT"
                        "MULTILINE" "DENSITY" "COUNTPLOT" "LINEPLOT"
                        "SCATTER" "SPARKLINE" "LINECHART"))
          "[ \t]*(")
  "Matches a chart formula, which only grid-table can evaluate.")

(defun grid-table-xlsx--raw (cell)
  "Return the raw input for CELL, a (ROW COL VALUE . PROPERTIES) entry.
Text that would read as a number, a formula or a truth value, or that
starts with an apostrophe, is marked with an apostrophe so it stays what
it is; a formula of grid-table's own, kept in a workbook as text, is
not.  A formula loses the prefixes a workbook gives newer functions."
  (let ((value (nth 2 cell)))
    (cond ((plist-get (nthcdr 3 cell) :text)
           ;; grid-table's own formulas travel as text and come back.
           (if (and (grid-table--reads-as-other-than-text-p value)
                    (let ((case-fold-search t))
                      (not (string-match-p grid-table-xlsx--own-formula-regexp
                                           value))))
               (concat "'" value)
             value))
          ((and (stringp value) (string-prefix-p "=" value))
           (grid-table-xlsx--unprefix-functions value))
          (t value))))

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
            (and format (list :format format))
            (and (plist-get properties :link)
                 (list :link (plist-get properties :link))))))

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

(defun grid-table-xlsx--validations (validations)
  "Return grid-table rules for the list VALIDATIONS of a sheet.
Each of VALIDATIONS is (SQREF FORMULA): the cells of SQREF, ranges
separated by spaces, take the values FORMULA gives, a quoted list such
as \"Yes,No\" or a reference."
  (let (rules)
    (pcase-dolist (`(,sqref ,formula) validations)
      (let ((spec (if (string-match "\\`\"\\(.*\\)\"\\'" formula)
                      (mapcar #'string-trim
                              (split-string (match-string 1 formula) ","))
                    (concat "=" formula))))
        (dolist (range (split-string sqref " " t))
          (when-let* ((bounds (grid-table-xlsx--parse-range range)))
            (push (append bounds (list 'list spec)) rules)))))
    (nreverse rules)))

(defun grid-table-xlsx--all-names (file)
  "Return the names of the xlsx FILE as (NAME DEFINITION SHEET) entries.
DEFINITION is a formula; SHEET is the sheet a name belongs to, or nil
for one of the whole workbook.  Names grid-table cannot take are left
out."
  (cl-remove-if
   (lambda (entry) (grid-calc-name-problem (car entry)))
   (mapcar (pcase-lambda (`(,name ,definition ,sheet))
             (list name (concat "=" definition) sheet))
           (car (read-from-string (grid-table-xlsx-module-names file))))))

(defun grid-table-xlsx--names (file)
  "Return the names defined across the workbook of the xlsx FILE.
An alist from each name to its definition as a formula; names that
belong to one sheet, and names grid-table cannot take, are left out."
  (delq nil (mapcar (pcase-lambda (`(,name ,definition ,sheet))
                      (unless sheet (cons name definition)))
                    (grid-table-xlsx--all-names file))))

(defun grid-table-xlsx-give-names (workbook file)
  "Give WORKBOOK the names defined in the xlsx FILE it was read from.
A name that belongs to one sheet is kept with that sheet, and the tables
of its sheets are kept for the formulas that name them; both are read,
not written, as the file keeps them."
  (let (global local tables)
    (pcase-dolist (`(,name ,definition ,sheet) (grid-table-xlsx--all-names file))
      (if sheet
          (push (cons (cons sheet name) definition) local)
        (push (cons name definition) global)))
    (dolist (source (grid-workbook-sheets workbook))
      (pcase-dolist (`(,name . ,place) (gethash :xlsx-tables source))
        (push (cons name (cons (grid-workbook-sheet-name source) place))
              tables)))
    (setf (grid-workbook-local-names workbook) (nreverse local))
    (setf (grid-workbook-tables workbook) (nreverse tables))
    (when (or global local tables)
      (grid-workbook-set-names workbook (nreverse global)))))

(defun grid-table-xlsx--write-names (file names &optional keep)
  "Give the xlsx FILE the defined NAMES, an alist from name to formula.
With KEEP, the names FILE has besides are kept; otherwise they go."
  (let ((names (append names
                       (and keep
                            (cl-remove-if
                             (lambda (entry) (assoc-string (car entry) names t))
                             (grid-table-xlsx--names file))))))
    (grid-table-xlsx-module-set-names
     file
     (mapconcat (lambda (entry)
                  (concat (grid-table-xlsx--clean (car entry))
                          grid-table-xlsx--field
                          (grid-table-xlsx--clean (substring (cdr entry) 1))))
                names grid-table-xlsx--record))))

(defun grid-table-xlsx--chart (source entry)
  "Return the floating chart of SOURCE that ENTRY, read from a workbook, is.
ENTRY is (TYPE TITLE FROM-ROW FROM-COL TO-ROW TO-COL REFS), each of REFS
\(ROLE RANGE) with ROLE v for values, x for x values, c for categories
and n for a series name.  The chart draws the block covering the
ranges, its series running the way its first values do.  Return nil
for an ENTRY that reads no cells."
  (pcase-let* ((`(,type ,title ,top ,left ,bottom ,right ,refs) entry)
               (refs (delq nil (mapcar (lambda (ref)
                                         (when-let* ((box (grid-chart-svg-parse-range
                                                           (cadr ref))))
                                           (cons (car ref) box)))
                                       refs)))
               (boxes (mapcar #'cdr refs))
               (widths (grid-get-column-widths source)))
    (when (and boxes (assq (intern type) grid-chart-svg-types))
      (let* ((first (or (cdr (assq 'v refs)) (car boxes)))
             (box (list (apply #'min (mapcar (lambda (b) (nth 0 b)) boxes))
                        (apply #'min (mapcar (lambda (b) (nth 1 b)) boxes))
                        (apply #'max (mapcar (lambda (b) (nth 2 b)) boxes))
                        (apply #'max (mapcar (lambda (b) (nth 3 b)) boxes))))
             (single (lambda (b) (and (= (nth 0 b) (nth 2 b))
                                      (= (nth 1 b) (nth 3 b)))))
             ;; A series of one value runs the way its name sits from it,
             ;; or across from the next series.
             (beside (lambda (b)
                       (cond ((= (nth 1 b) (nth 1 first)) 'columns)
                             ((= (nth 0 b) (nth 0 first)) 'rows))))
             (by (cond ((and (= (nth 1 first) (nth 3 first))
                             (< (nth 0 first) (nth 2 first)))
                        'columns)
                       ((and (= (nth 0 first) (nth 2 first))
                             (< (nth 1 first) (nth 3 first)))
                        'rows)
                       ((funcall single first)
                        (or (when-let* ((name (cdr (assq 'n refs))))
                              (funcall beside name))
                            (when-let* ((next (cl-find-if
                                               (lambda (ref)
                                                 (and (eq (car ref) 'v)
                                                      (not (eq (cdr ref) first))))
                                               refs)))
                              (pcase (funcall beside (cdr next))
                                ('columns 'rows)
                                ('rows 'columns)))))))
             ;; What the first column and row hold, when the workbook
             ;; says so and they are where grid-table looks.
             (place (lambda (roles side)
                      (let ((held (cl-loop for (role . b) in refs
                                           when (memq role roles) collect b)))
                        (cond ((null held) 'none)
                              ((cl-every (lambda (b)
                                           (if (eq side 'col)
                                               (= (nth 1 b) (nth 3 b) (nth 1 box))
                                             (= (nth 0 b) (nth 2 b) (nth 0 box))))
                                         held)
                               t)))))
             (labels (and by (funcall place '(c x) (if (eq by 'columns) 'col 'row))))
             (names (and by (funcall place '(n) (if (eq by 'columns) 'row 'col)))))
        (append
         (list :anchor (cons top left)
               ;; Columns count their width in characters; a row is a line.
               :size (cons (max 8 (min 999 (round (cl-loop
                                                   for col from left below (max right (1+ left))
                                                   sum (or (and widths (gethash col widths))
                                                           grid-table-xlsx--default-width)))))
                           (max 4 (min 999 (- bottom top))))
               :type (intern type)
               :range (apply #'grid-chart-svg-range-text box)
               :by by
               :title (and (not (string-empty-p title)) title))
         (and labels (list :labels labels))
         (and names (list :names names)))))))

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
    ;; An array formula fills its range, written cells or not.
    (dolist (cell cells)
      (when-let* ((range (plist-get (nthcdr 3 cell) :array))
                  (box (grid-chart-svg-parse-range range)))
        (setq rows (max rows (1+ (nth 2 box)))
              cols (max cols (1+ (nth 3 box))))))
    ;; A note may sit on an empty cell, and a chart over empty ones
    ;; beside the data; the grid keeps them, so the chart stays put.
    (dolist (note (plist-get data :notes))
      (setq rows (max rows (1+ (nth 0 note)))
            cols (max cols (1+ (nth 1 note)))))
    (dolist (chart (plist-get data :charts))
      (setq rows (max rows (1+ (nth 2 chart)))
            cols (max cols (1+ (nth 3 chart))))
      ;; The cells it reads too, empty ones past the data included, so
      ;; its ranges come back whole; unless that makes the grid too big.
      (dolist (ref (nth 6 chart))
        (when-let* ((box (grid-chart-svg-parse-range (cadr ref)))
                    (more-rows (max rows (1+ (nth 2 box))))
                    (more-cols (max cols (1+ (nth 3 box))))
                    ((<= (* more-rows more-cols) grid-table-xlsx-max-cells)))
          (setq rows more-rows cols more-cols))))
    (when (> (* rows cols) grid-table-xlsx-max-cells)
      (error "Sheet %s spans %d by %d cells, more than grid-table opens"
             sheet rows cols))
    (let ((table (make-vector rows nil)))
      (dotimes (row rows)
        (aset table row (make-vector cols "")))
      (dolist (cell cells)
        (aset (aref table (nth 0 cell)) (nth 1 cell)
              (grid-table-xlsx--raw cell)))
      ;; The other cells of an array formula's range hold the values it
      ;; last gave; the formula fills them again.
      (dolist (cell cells)
        (when-let* ((range (plist-get (nthcdr 3 cell) :array))
                    (box (grid-chart-svg-parse-range range)))
          (cl-loop for row from (nth 0 box) to (min (nth 2 box) (1- rows)) do
                   (cl-loop for col from (nth 1 box) to (min (nth 3 box) (1- cols))
                            unless (and (= row (nth 0 cell)) (= col (nth 1 cell)))
                            do (aset (aref table row) col "")))))
      (let* ((values (mapcar (lambda (row) (append row nil)) table))
             ;; Formulas read as Excel reads a file's.
             (source (let ((grid-calc-legacy-arrays t))
                       (make-default-data-source
                        (copy-sequence (car values)) values sheet)))
             (model (gethash :model source))
             (arrays (delq nil (mapcar (lambda (cell)
                                         (when-let* ((range (plist-get (nthcdr 3 cell)
                                                                       :array)))
                                           (cons (cons (nth 0 cell) (nth 1 cell))
                                                 range)))
                                       cells)))
             (rules (grid-table-xlsx--rules (plist-get data :conditionals))))
        ;; A merge that reaches down from the first row, such as a title
        ;; over two rows, only exists in a sheet without a header row.
        (when (cl-some (lambda (merge)
                         (and (= (nth 0 merge) 0) (< (nth 0 merge) (nth 2 merge))))
                       merges)
          (puthash :no-header-row t source))
        (when merges
          (let ((grid-model-first-row-is-header
                 (not (gethash :no-header-row source))))
            (set-grid-data-model-merges
             model (grid-model-validate-merges model merges t))))
        (dolist (cell cells)
          (when-let* ((style
                       (let ((style (grid-table-xlsx--style (nthcdr 3 cell)))
                             (case-fold-search t))
                         ;; A workbook shows a date formula without a date
                         ;; format as a number, and so does the grid.
                         (if (and (not (plist-get style :format))
                                  (stringp (nth 2 cell))
                                  (string-match-p grid-table--date-formula-regexp
                                                  (nth 2 cell)))
                             (append style (list :format (list 'general)))
                           style))))
            (grid-model-set-style model (nth 0 cell) (nth 1 cell) style)))
        (pcase-dolist (`(,row ,col ,text) (plist-get data :notes))
          (grid-model-set-style
           model row col
           (plist-put (copy-sequence (grid-model-get-style model row col))
                      :note text)))
        (grid-model-set-conditionals model (car rules))
        ;; A width is in characters on both sides; a workbook counts a
        ;; little padding in, which rounding takes off again.  A column
        ;; past the last cell keeps its width too, to be written back.
        (pcase-dolist (`(,col . ,width) (plist-get data :widths))
          (when (and (integerp col) (< -1 col grid-table-xlsx--max-cols)
                     (numberp width))
            (grid-set-column-width source col
                                   (max 1 (min 200 (round width))))))
        (when-let* ((frozen (plist-get data :frozen)))
          (grid-table-set-frozen source (nth 0 frozen) (nth 1 frozen)))
        (grid-table-set-hidden source (plist-get data :hidden-rows)
                               (plist-get data :hidden-cols))
        (grid-table-set-validations
         source (grid-table-xlsx--validations (plist-get data :validations)))
        (grid-table-set-charts
         source (delq nil (mapcar (lambda (entry)
                                    (grid-table-xlsx--chart source entry))
                                  (plist-get data :charts))))
        ;; The formulas the file holds read as Excel reads them; one
        ;; typed in afterwards is a formula of today, and may spill.
        (grid-model-set-legacy
         model
         (let ((cols (grid-data-model-cols model))
               (cells (grid-data-model-cells model))
               found)
           (dotimes (index (length cells) found)
             (when (grid-calc--formula-p (grid-cell-raw-input (aref cells index)))
               (push (cons (/ index cols) (% index cols)) found)))))
        (when arrays
          (grid-model-set-arrays model arrays)
          (grid-model-recalculate-all model))
        (puthash :xlsx-tables (plist-get data :tables) source)
        (puthash :xlsx-file file source)
        (puthash :xlsx-sheet sheet source)
        (puthash :xlsx-skipped-conditionals (cdr rules) source)
        source))))

;;;###autoload
(defun grid-table-xlsx-create-workbook (file)
  "Return a workbook holding every sheet of the xlsx FILE, and its names."
  (let ((workbook
         (grid-workbook-create
          (mapcar (lambda (name)
                    (cons name (grid-table-xlsx-create-data-source file name)))
                  (or (grid-table-xlsx-sheet-names file)
                      (error "No sheet in %s" file)))
          t)))
    (grid-table-xlsx-give-names workbook (expand-file-name file))
    workbook))

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

(defun grid-table-xlsx--clean (text)
  "Return TEXT without the characters that separate records and fields."
  (replace-regexp-in-string "[\x1d\x1e\x1f]" "" text))

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

(defun grid-table-xlsx--cell-record (row col raw style &optional array)
  "Return the record for the cell at ROW and COL, or nil for nothing.
RAW is its raw input and STYLE its style.  ARRAY, for an array formula,
is the range of the cells it fills."
  (let* ((case-fold-search t)
         (text (grid-table-xlsx--clean (format "%s" (or raw ""))))
         (kind (cond
                ;; Text by its apostrophe; a number keeps its kind under
                ;; the text format, as it does in a spreadsheet.
                ((string-prefix-p "'" text)
                 (setq text (substring text 1))
                 "s")
                ((and (string-prefix-p "=" text) (> (length text) 1)
                      (not (string-match-p
                            grid-table-xlsx--own-formula-regexp text)))
                 (setq text (grid-table-xlsx--prefix-functions
                             (substring text 1)))
                 (if array (concat "a:" array) "f"))
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

(defconst grid-table-xlsx--item (string #x1d)
  "Separator of the items of a list inside a field.")

(defun grid-table-xlsx--chart-record (source chart sheet)
  "Return the record for floating CHART of SOURCE, written as sheet SHEET.
Its data ranges name SHEET, so the workbook's chart reads the cells it
was drawn from; its size in characters and lines becomes the columns
and rows it covers."
  (let* ((data (grid-table-chart-data source chart))
         (type (plist-get chart :type))
         (prefix (grid-calc-sheet-prefix sheet))
         (absolute (lambda (range)
                     (pcase-let ((`(,top ,left ,bottom ,right) range))
                       (format "%s$%s$%d:$%s$%d" prefix
                               (grid-table-xlsx--column-name left) (1+ top)
                               (grid-table-xlsx--column-name right) (1+ bottom)))))
         (ranges (plist-get data :ranges))
         (labels (plist-get data :category-range))
         (anchor (plist-get chart :anchor))
         (size (plist-get chart :size))
         (widths (grid-get-column-widths source))
         ;; The columns the chart covers, to the nearest whole one, so
         ;; that reading it back gives the same columns again.
         (right (let ((col (cdr anchor)) (room (car size)))
                  (while (let ((width (or (and widths (gethash col widths))
                                          grid-table-xlsx--default-width)))
                           (when (> room (/ width 2.0))
                             (setq room (- room width)
                                   col (1+ col))))
                    nil)
                  (max col (1+ (cdr anchor)))))
         (refs (pcase type
                 ;; Labels and values in turn; with no labels the values
                 ;; stand in for them.
                 ('doughnut (cl-loop for range in ranges
                                     append (list (funcall absolute (or labels range))
                                                  (funcall absolute range))))
                 ('scatter (and labels
                                (cl-loop for range in ranges
                                         append (list (funcall absolute labels)
                                                      (funcall absolute range)))))
                 (_ (mapcar absolute ranges)))))
    (when refs
      (mapconcat
       #'identity
       (append
        (list "G" (symbol-name type)
              (grid-table-xlsx--cell-name (car anchor) (cdr anchor))
              (grid-table-xlsx--cell-name (+ (car anchor) (cdr size)) right)
              (grid-table-xlsx--clean (or (plist-get chart :title) ""))
              (mapconcat absolute (plist-get data :names) grid-table-xlsx--item)
              ;; The cells holding the category labels; none when the
              ;; categories are only numbered.
              (if (and labels (not (eq type 'scatter)))
                  (funcall absolute labels)
                ""))
        refs)
       grid-table-xlsx--field))))

(defun grid-table-xlsx--payload (source &optional sheet)
  "Return the records that describe the data source SOURCE.
SHEET is the name SOURCE is written as, which its charts' ranges name."
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
                          style))
                      ;; A formula that spills is saved as an array
                      ;; formula over the cells it fills.
                      (let ((model (gethash :model source)))
                        (or (grid-model-array-at model row col)
                            (grid-calc-spill-range model row col))))))
          (push record records))
        (let ((style (grid-get-style-at source row col)))
          (dolist (entry '((:note . "N") (:link . "L")))
            (when-let* ((text (plist-get style (car entry))))
              (push (mapconcat #'identity
                               (list (cdr entry) (number-to-string row)
                                     (number-to-string col)
                                     (grid-table-xlsx--clean text))
                               grid-table-xlsx--field)
                    records))))))
    (dolist (merge (grid-get-merges source))
      (push (concat "M" grid-table-xlsx--field
                    (apply #'grid-table-xlsx--range-name merge))
            records))
    (dolist (rule (grid-get-conditional-formats source))
      (push (grid-table-xlsx--rule-record rule) records))
    ;; Columns sized by hand; the others keep what the workbook has.
    (when-let* ((widths (grid-get-column-widths source)))
      (maphash (lambda (col width)
                 (when (< col grid-table-xlsx--max-cols)
                   (push (format "W%s%d%s%d" grid-table-xlsx--field col
                                 grid-table-xlsx--field width)
                         records)))
               widths))
    (let ((frozen (grid-table-frozen source)))
      (push (format "P%s%d%s%d" grid-table-xlsx--field (car frozen)
                    grid-table-xlsx--field (cdr frozen))
            records))
    (let ((hidden (grid-table-hidden source)))
      (dolist (row (car hidden))
        (push (format "H%sr%s%d" grid-table-xlsx--field grid-table-xlsx--field row)
              records))
      (dolist (col (cdr hidden))
        (push (format "H%sc%s%d" grid-table-xlsx--field grid-table-xlsx--field col)
              records)))
    (dolist (rule (grid-table-validations source))
      (pcase-let ((`(,top ,left ,bottom ,right ,_kind ,spec) rule))
        (push (concat "V" grid-table-xlsx--field
                      (grid-table-xlsx--range-name top left bottom right)
                      grid-table-xlsx--field
                      (grid-table-xlsx--clean
                       (if (stringp spec)
                           (substring spec 1)
                         (format "\"%s\""
                                 (string-join
                                  (mapcar (lambda (item)
                                            (string-replace "\"" "\"\"" item))
                                          spec)
                                  ",")))))
              records)))
    (when sheet
      (dolist (chart (grid-table-charts source))
        (when-let* ((record (grid-table-xlsx--chart-record source chart sheet)))
          (push record records))))
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
           (grid-table-xlsx--payload source sheet))
          ;; Names, written for the sheet as the workbook calls it, join
          ;; the ones the file has.
          (when-let* ((workbook (grid-workbook-of source))
                      (names (grid-workbook-names workbook)))
            (let ((own (grid-workbook-sheet-name source)))
              (grid-table-xlsx--write-names
               temporary
               (mapcar (lambda (entry)
                         (cons (car entry)
                               (grid-calc-rename-sheet-in-formula
                                (cdr entry) own sheet)))
                       names)
               t)))
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
               (grid-table-xlsx--payload sheet (grid-workbook-sheet-name sheet)))
              (setq written target
                    spare (reverse spare))))
          (when own
            (grid-table-xlsx-module-prune
             written
             (mapconcat #'grid-workbook-sheet-name sheets
                        grid-table-xlsx--record)))
          ;; The workbook's names are FILE's when it came from FILE; a
          ;; file it did not come from keeps its own besides.
          (when (or own (grid-workbook-names workbook))
            (grid-table-xlsx--write-names
             written (grid-workbook-names workbook) (not own)))
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
