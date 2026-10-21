;;; exc-xlsx.el --- Read and write xlsx files for exc -*- lexical-binding: t; -*-

;;; Commentary:

;; This plugin opens an xlsx workbook as a grid with a sheet for each of
;; its sheets, and writes a grid back as a workbook.  Values, formulas,
;; merges, cell formats and conditional formats travel both ways.
;;
;; The file format is handled by a dynamic module written in Rust, whose
;; source is in plugins/xlsx/.  A prebuilt exc-xlsx-module.dylib
;; (.so, .dll) next to this file is used when there is one for the
;; platform; otherwise build the module with
;; `exc-xlsx-build-module', which needs cargo.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'exc)
(require 'exc-plugins)
(require 'exc-obsolete)

(declare-function exc-xlsx-module-sheets "exc-xlsx-module")
(declare-function exc-xlsx-module-read "exc-xlsx-module")
(declare-function exc-xlsx-module-write "exc-xlsx-module")
(declare-function exc-xlsx-module-date1904 "exc-xlsx-module")
(declare-function exc-xlsx-module-prune "exc-xlsx-module")
(declare-function exc-xlsx-module-names "exc-xlsx-module")
(declare-function exc-xlsx-module-links "exc-xlsx-module")
(declare-function exc-xlsx-module-externals "exc-xlsx-module")
(declare-function exc-xlsx-module-external-sheets "exc-xlsx-module")
(declare-function exc-xlsx-module-external-names "exc-xlsx-module")
(declare-function exc-xlsx-module-set-names "exc-xlsx-module")

(defcustom exc-xlsx-module-file nil
  "File of the xlsx dynamic module, or nil to look in the usual places.
Those are the cargo build directory under plugins/xlsx/ and then the
prebuilt exc-xlsx-module file next to this plugin."
  :type '(choice (const :tag "Look in the usual places" nil) file)
  :group 'exc)

(defconst exc-xlsx-max-cells 2000000
  "Largest sheet, in cells, that the plugin opens.")

(defconst exc-xlsx--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this plugin.")

(defconst exc-xlsx--record (string #x1e)
  "Separator of the records handed to the module.")

(defconst exc-xlsx--field (string #x1f)
  "Separator of the fields of a record.")

(defconst exc-xlsx--max-cols 16384
  "How many columns a worksheet can have.")

(defconst exc-xlsx--default-width 8.43
  "Width in characters of a column a workbook has not sized.")

(defconst exc-xlsx--emu-per-char 66675
  "EMU in a character of a column's width.
A workbook measures a drawing's place within a cell in EMU, 9525 to a
pixel, and a character of the default font is 7 pixels wide.")

(defconst exc-xlsx--number-regexp
  "\\`[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
  "Matches the text of a number.")

;;;----------------------------------------------------------------------
;;; The module
;;;----------------------------------------------------------------------

(defun exc-xlsx--module-candidates ()
  "Return the files the module may be in, most specific first.
A module built here comes before the prebuilt one: whoever built it
meant it to be used."
  (let ((suffix (or module-file-suffix ".so"))
        (build (expand-file-name "xlsx/target/release/"
                                 exc-xlsx--directory)))
    (delq nil
          (list exc-xlsx-module-file
                (expand-file-name (concat "libexc_xlsx_module" suffix)
                                  build)
                (expand-file-name (concat "exc_xlsx_module" suffix)
                                  build)
                (expand-file-name (concat "exc-xlsx-module" suffix)
                                  exc-xlsx--directory)))))

(defun exc-xlsx--ensure-module ()
  "Load the xlsx module unless it is loaded; signal when there is none."
  (unless (featurep 'exc-xlsx-module)
    (unless (fboundp 'module-load)
      (user-error "This Emacs was built without dynamic module support"))
    (let ((file (cl-find-if #'file-exists-p
                            (exc-xlsx--module-candidates))))
      (unless file
        (user-error "No prebuilt xlsx module (exc-xlsx-module%s) for this platform; run M-x exc-xlsx-build-module"
                    (or module-file-suffix ".so")))
      (module-load file))))

(defun exc-xlsx-build-module ()
  "Build the xlsx module from plugins/xlsx/ with cargo."
  (interactive)
  (unless (executable-find "cargo")
    (user-error "Building the xlsx module needs cargo, the Rust build tool"))
  (let ((default-directory (expand-file-name "xlsx/"
                                             exc-xlsx--directory)))
    (compile "cargo build --release")))

;;;----------------------------------------------------------------------
;;; Cell references
;;;----------------------------------------------------------------------

(defun exc-xlsx--column-name (col)
  "Return the letters of the zero-based column COL."
  (let ((name ""))
    (setq col (1+ col))
    (while (> col 0)
      (setq name (concat (string (+ ?A (% (1- col) 26))) name)
            col (/ (1- col) 26)))
    name))

(defun exc-xlsx--cell-name (row col)
  "Return the A1 name of the zero-based ROW and COL."
  (format "%s%d" (exc-xlsx--column-name col) (1+ row)))

(defun exc-xlsx--range-name (top left bottom right)
  "Return the A1 name of the range from TOP, LEFT to BOTTOM, RIGHT."
  (format "%s:%s" (exc-xlsx--cell-name top left)
          (exc-xlsx--cell-name bottom right)))

(defun exc-xlsx--parse-cell (name)
  "Return (ROW . COL), zero-based, of the A1 cell NAME, or nil."
  (when (string-match "\\`\\$?\\([A-Za-z]\\{1,3\\}\\)\\$?\\([0-9]+\\)\\'" name)
    (let ((col 0))
      (mapc (lambda (letter) (setq col (+ (* col 26) (- letter ?A -1))))
            (upcase (match-string 1 name)))
      (cons (1- (string-to-number (match-string 2 name))) (1- col)))))

(defun exc-xlsx--parse-range (name)
  "Return (TOP LEFT BOTTOM RIGHT) of the A1 range or cell NAME, or nil."
  (let* ((ends (split-string name ":"))
         (start (exc-xlsx--parse-cell (car ends)))
         (end (if (cdr ends)
                  (exc-xlsx--parse-cell (cadr ends))
                start)))
    (when (and start end (<= (length ends) 2))
      (list (min (car start) (car end)) (min (cdr start) (cdr end))
            (max (car start) (car end)) (max (cdr start) (cdr end))))))

;;;----------------------------------------------------------------------
;;; Reading
;;;----------------------------------------------------------------------

(defun exc-xlsx-sheet-names (file)
  "Return the names of the sheets of the xlsx FILE."
  (exc-xlsx--ensure-module)
  (car (read-from-string
        (exc-xlsx-module-sheets (expand-file-name file)))))

(defun exc-xlsx-date-system (file)
  "Return 1904 when the dates of the xlsx FILE count from 1904, else 1900."
  (exc-xlsx--ensure-module)
  (if (exc-xlsx-module-date1904 (expand-file-name file)) 1904 1900))

(defconst exc-xlsx--future-functions
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

(defun exc-xlsx--outside-strings (formula function)
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

(defun exc-xlsx--operand-end (formula start)
  "Return where the operand at START in FORMULA ends.
It is a reference, a name, a function call or a group in parentheses."
  (let ((pos start) (end (length formula)) (done nil))
    (while (and (not done) (< pos end))
      (let ((char (aref formula pos)))
        (cond ((eq char ?\()
               (setq pos (1+ (or (exc-calc-closing-paren formula pos) (1- end)))))
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

(defun exc-xlsx--outside-brackets (text function)
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

(defun exc-xlsx--at-as-single (formula)
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
               (let ((end (exc-xlsx--operand-end formula (1+ pos))))
                 (setq formula (concat (substring formula 0 pos)
                                       "SINGLE(" (substring formula (1+ pos) end) ")"
                                       (substring formula end))))))
        (setq pos (1+ pos))))
    formula))

(defun exc-xlsx--spill-as-anchor (formula)
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

(defun exc-xlsx--call-arguments (formula open)
  "Return the arguments of the call whose parenthesis is at OPEN in FORMULA.
Each is the text between its commas, as is."
  (let ((close (or (exc-calc-closing-paren formula open) (length formula)))
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

(defun exc-xlsx--parameter-names (formula)
  "Return the names the LET and LAMBDA calls of FORMULA declare."
  (let ((case-fold-search t) (start 0) (names nil))
    (while (string-match "\\(?:\\`\\|[^[:alnum:]_.]\\)\\(LET\\|LAMBDA\\)(" formula start)
      (let* ((let-p (string-equal (upcase (match-string 1 formula)) "LET"))
             (arguments (butlast (exc-xlsx--call-arguments
                                  formula (1- (match-end 0))))))
        (cl-loop for argument in arguments
                 for index from 0
                 for name = (string-trim argument)
                 when (and (or (not let-p) (cl-evenp index))
                           (string-match-p "\\`[[:alpha:]_][[:alnum:]_.]*\\'" name))
                 do (cl-pushnew (downcase name) names :test #'equal))
        (setq start (match-end 0))))
    names))

(defun exc-xlsx--prefix-functions (formula)
  "Return FORMULA, without its =, naming newer functions as a workbook does.
@ becomes SINGLE, A1# ANCHORARRAY(A1), and the names LET and LAMBDA
declare take _xlpm."
  (let* ((case-fold-search t)
         (formula (exc-xlsx--at-as-single
                   (exc-xlsx--spill-as-anchor formula)))
         (parameters (exc-xlsx--parameter-names formula)))
    (exc-xlsx--outside-strings
     formula
     (lambda (text)
       (exc-xlsx--outside-brackets
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
                                                  exc-xlsx--future-functions))
                                    (and (member (downcase name) parameters)
                                         "_xlpm.")))))
                (push (substring text start at) parts)
                (when prefix (push prefix parts))
                (push name parts)
                (setq start (match-end 0))))
            (push (substring text start) parts)
            (apply #'concat (nreverse parts)))))))))

(defun exc-xlsx--unprefix-functions (formula)
  "Return FORMULA with a workbook's function and parameter prefixes off.
SINGLE(X) and ANCHORARRAY(A1) read as @X and A1#, as Excel shows them."
  (let ((case-fold-search t))
    (exc-calc-excel-formula-text
     (exc-xlsx--outside-strings
      formula
      (lambda (text)
        (replace-regexp-in-string "_xlfn\\.\\(?:_xlws\\.\\)?\\|_xl\\(?:pm\\|op\\|eta\\)\\." "" text t t))))))

(defconst exc-xlsx--own-formula-regexp
  (concat "\\`=[ \t]*"
          (regexp-opt '("BARPLOT" "COLUMNPLOT" "HISTOGRAM" "BOXPLOT"
                        "MULTILINE" "DENSITY" "COUNTPLOT" "LINEPLOT"
                        "SCATTER" "SPARKLINE" "LINECHART"))
          "[ \t]*(")
  "Matches a chart formula, which only exc can evaluate.")

(defun exc-xlsx--raw (cell)
  "Return the raw input for CELL, a (ROW COL VALUE . PROPERTIES) entry.
Text that would read as a number, a formula or a truth value, or that
starts with an apostrophe, is marked with an apostrophe so it stays what
it is; a formula of exc's own, kept in a workbook as text, is
not.  A formula loses the prefixes a workbook gives newer functions."
  (let ((value (nth 2 cell)))
    (cond ((plist-get (nthcdr 3 cell) :text)
           ;; exc's own formulas travel as text and come back.
           ;; Empty text is no blank: =A1 of it is "", not 0.
           (if (and (or (equal value "")
                        (exc--reads-as-other-than-text-p value))
                    (let ((case-fold-search t))
                      (not (string-match-p exc-xlsx--own-formula-regexp
                                           value))))
               (concat "'" value)
             value))
          ((and (stringp value) (string-prefix-p "=" value))
           (exc-xlsx--unprefix-functions value))
          (t value))))

(defun exc-xlsx--style (properties)
  "Return the exc style for the module's cell PROPERTIES.
Black text is the default of a workbook and is not taken as a color.
A number format the grid has no counterpart for is kept as it is, to be
written back."
  (let ((color (plist-get properties :color))
        (fill (plist-get properties :fill))
        (align (plist-get properties :align))
        (format (exc-format-from-code (plist-get properties :format))))
    (append (and (plist-get properties :bold) (list :bold t))
            (and (plist-get properties :italic) (list :italic t))
            (and color (not (equal color "000000"))
                 (list :color (concat "#" color)))
            (and fill (list :background (concat "#" fill)))
            (and align (list :align (intern align)))
            (and format (list :format format))
            (and (plist-get properties :link)
                 (list :link (plist-get properties :link))))))

(defun exc-xlsx--unquote (text)
  "Return TEXT without the double quotes of a formula string."
  (if (string-match "\\`\"\\(\\(?:.\\|\n\\)*\\)\"\\'" text)
      (replace-regexp-in-string "\"\"" "\"" (match-string 1 text))
    text))

(defun exc-xlsx--rule-test (rule)
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
         (cons op (exc-xlsx--unquote formula))))
      ;; The searched text is an optional attribute; the formula always
      ;; holds it, as SEARCH("text",CELL).
      ("ContainsText"
       (cond ((not (string-empty-p text)) (cons "contains" text))
             ((string-match "SEARCH(\\(\"\\(?:[^\"]\\|\"\"\\)*\"\\)," formula)
              (cons "contains"
                    (exc-xlsx--unquote (match-string 1 formula))))))
      ("ContainsBlanks" (cons "empty" ""))
      ("ContainsErrors" (cons "error" "")))))

(defun exc-xlsx--rules (conditionals)
  "Return (RULES . SKIPPED) for the module's CONDITIONALS.
RULES are exc rules, the one that wins last; SKIPPED counts the
conditional formats exc has no counterpart for."
  (let ((skipped 0)
        rules)
    ;; In a workbook the lowest priority number wins.
    (dolist (rule (sort (copy-sequence conditionals)
                        (lambda (a b) (> (nth 5 a) (nth 5 b)))))
      (let ((test (exc-xlsx--rule-test rule))
            (style (exc-xlsx--style (nthcdr 6 rule))))
        (if (not (and test style))
            (setq skipped (1+ skipped))
          (dolist (name (split-string (car rule) " " t))
            (when-let* ((range (exc-xlsx--parse-range name)))
              (push (append range (list (car test) (cdr test) style))
                    rules))))))
    (cons (nreverse rules) skipped)))

(defun exc-xlsx--validations (validations)
  "Return exc rules for the list VALIDATIONS of a sheet.
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
          (when-let* ((bounds (exc-xlsx--parse-range range)))
            (push (append bounds (list 'list spec)) rules)))))
    (nreverse rules)))

(defun exc-xlsx--all-names (file)
  "Return the names of the xlsx FILE as (NAME DEFINITION SHEET) entries.
DEFINITION is a formula; SHEET is the sheet a name belongs to, or nil
for one of the whole workbook.  Names exc cannot take are left
out."
  (cl-remove-if
   (lambda (entry) (exc-calc-name-problem (car entry)))
   (mapcar (pcase-lambda (`(,name ,definition ,sheet))
             (list name (concat "=" definition) sheet))
           (car (read-from-string (exc-xlsx-module-names file))))))

(defun exc-xlsx--names (file)
  "Return the names defined across the workbook of the xlsx FILE.
An alist from each name to its definition as a formula; names that
belong to one sheet, and names exc cannot take, are left out."
  (delq nil (mapcar (pcase-lambda (`(,name ,definition ,sheet))
                      (unless sheet (cons name definition)))
                    (exc-xlsx--all-names file))))

(defun exc-xlsx-give-names (workbook file)
  "Give WORKBOOK the names defined in the xlsx FILE it was read from.
A name that belongs to one sheet is kept with that sheet, and the tables
of its sheets are kept for the formulas that name them; both are read,
not written, as the file keeps them.  WORKBOOK keeps FILE too, which
CELL(\"filename\") names.  The workbooks FILE links to are read
from what FILE cached of them, when a formula first reads one."
  (setf (exc-workbook-file workbook) (expand-file-name file))
  (when-let* ((text (ignore-errors
                      (exc-xlsx-module-externals (expand-file-name file)))))
    ;; A formula's [N] counts the links as the workbook names them.
    (when (plist-get (car (read-from-string text)) :links)
      (setf (exc-workbook-externals workbook)
            (exc-xlsx--external-reader (expand-file-name file)))))
  (let (global local tables)
    (pcase-dolist (`(,name ,definition ,sheet) (exc-xlsx--all-names file))
      (if sheet
          (push (cons (cons sheet name) definition) local)
        (push (cons name definition) global)))
    (dolist (source (exc-workbook-sheets workbook))
      (pcase-dolist (`(,name . ,place) (gethash :xlsx-tables source))
        (push (cons name (cons (exc-workbook-sheet-name source) place))
              tables)))
    (setf (exc-workbook-local-names workbook) (nreverse local))
    (setf (exc-workbook-tables workbook) (nreverse tables))
    (when (or global local tables (exc-workbook-externals workbook))
      (exc-workbook-set-names workbook (nreverse global)))))

(defun exc-xlsx--write-names (file names &optional keep)
  "Give the xlsx FILE the defined NAMES, an alist from name to formula.
With KEEP, the names FILE has besides are kept; otherwise they go."
  (let ((names (append names
                       (and keep
                            (cl-remove-if
                             (lambda (entry) (assoc-string (car entry) names t))
                             (exc-xlsx--names file))))))
    (exc-xlsx-module-set-names
     file
     (mapconcat (lambda (entry)
                  (concat (exc-xlsx--clean (car entry))
                          exc-xlsx--field
                          (exc-xlsx--clean (substring (cdr entry) 1))))
                names exc-xlsx--record))))

(defun exc-xlsx--series-count (chart)
  "Return how many series the floating CHART draws from its block."
  (pcase-let ((`(,top ,left ,bottom ,right)
               (exc-chart-svg-parse-range (plist-get chart :range))))
    (- (if (eq (plist-get chart :by) 'rows)
           (1+ (- bottom top))
         (1+ (- right left)))
       (if (eq (plist-get chart :labels) t) 1 0))))

(defun exc-xlsx--chart (source entry)
  "Return the floating chart of SOURCE that ENTRY, read from a workbook, is.
ENTRY is (TYPE TITLE FROM-ROW FROM-COL TO-ROW TO-COL REFS NAMES
FROM-OFFSET TO-OFFSET), the offsets how far into their columns the
corners sit in EMU, each
of REFS (ROLE RANGE) with ROLE v for values, x for x values, c for
categories and n for a series name, and NAMES each series' name: nil,
its cells or (text TEXT).  The chart draws the block covering the
ranges, its series running the way its first values do.  Names that
are not the block's first row or column, as text of their own or cells
elsewhere, are kept as :series-names, and the block leaves them out.
Return nil for an ENTRY that reads no cells."
  (pcase-let* ((`(,type ,title ,top ,left ,bottom ,right ,refs . ,given) entry)
               (refs (delq nil (mapcar (lambda (ref)
                                         (when-let* ((box (exc-chart-svg-parse-range
                                                           (cadr ref))))
                                           (cons (car ref) box)))
                                       refs)))
               (boxes (mapcar #'cdr refs))
               (widths (exc-get-column-widths source))
               (width (lambda (col)
                        (or (and widths (gethash col widths))
                            exc-xlsx--default-width)))
               (chars (lambda (emu)
                        (/ (or (and (natnump emu) emu) 0)
                           (float exc-xlsx--emu-per-char))))
               (to-offset (funcall chars (nth 2 given)))
               (offset (/ (round (* 100 (min (funcall chars (nth 1 given))
                                             (funcall width left))))
                          100.0))
               (offset (if (< offset (funcall width left)) offset 0)))
    (when (and boxes (assq (intern type) exc-chart-svg-types))
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
             ;; says so and they are where exc looks.
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
        (if-let* (((cl-some #'identity (car given)))
                  ((or (not (eq names t))
                       (cl-some #'consp (car given))))
                  ;; Names exc would not find in the block.
                  (chart (exc-xlsx--chart
                          source
                          (list type title top left bottom right
                                (cl-remove 'n (nth 6 entry) :key #'car)
                                nil (nth 1 given) (nth 2 given))))
                  ;; One for each series the block gives, or the chart
                  ;; holds series of its own that the block cannot.
                  ((= (length (car given)) (exc-xlsx--series-count chart))))
            (append chart (list :series-names (car given)))
        (append
         (list :anchor (cons top left))
         ;; Within its first column, to a hundredth of a character.
         (and (> offset 0) (list :offset (cons offset 0)))
         (list
               ;; Columns count their width in characters; a row is a line.
               :size (cons (max 8 (min 999 (round (if (> right left)
                                                       (- (+ (cl-loop for col from left below right
                                                                      sum (funcall width col))
                                                             to-offset)
                                                          offset)
                                                     (if (> to-offset offset)
                                                         (- to-offset offset)
                                                       (funcall width left))))))
                           (max 4 (min 999 (- bottom top))))
               :type (intern type)
               :range (apply #'exc-chart-svg-range-text box)
               :by by
               :title (and (not (string-empty-p title)) title))
         (and labels (list :labels labels))
         (and names (list :names names))))))))

(defun exc-xlsx-create-data-source (file &optional sheet)
  "Return a data source holding sheet SHEET of the xlsx FILE.
SHEET defaults to the first sheet.  The first row of the sheet becomes
the header row."
  (exc-xlsx--ensure-module)
  (setq file (expand-file-name file))
  (let* ((sheet (or sheet (car (exc-xlsx-sheet-names file))
                    (error "No sheet in %s" file)))
         (data (car (read-from-string
                     (exc-xlsx-module-read file sheet))))
         (cells (plist-get data :cells))
         (merges (delq nil (mapcar #'exc-xlsx--parse-range
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
                  (box (exc-chart-svg-parse-range range)))
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
        (when-let* ((box (exc-chart-svg-parse-range (cadr ref)))
                    (more-rows (max rows (1+ (nth 2 box))))
                    (more-cols (max cols (1+ (nth 3 box))))
                    ((<= (* more-rows more-cols) exc-xlsx-max-cells)))
          (setq rows more-rows cols more-cols))))
    (when (> (* rows cols) exc-xlsx-max-cells)
      (error "Sheet %s spans %d by %d cells, more than exc opens"
             sheet rows cols))
    (let ((table (make-vector rows nil)))
      (dotimes (row rows)
        (aset table row (make-vector cols "")))
      (dolist (cell cells)
        (aset (aref table (nth 0 cell)) (nth 1 cell)
              (exc-xlsx--raw cell)))
      ;; The other cells of an array formula's range hold the values it
      ;; last gave; the formula fills them again.
      (dolist (cell cells)
        (when-let* ((range (plist-get (nthcdr 3 cell) :array))
                    (box (exc-chart-svg-parse-range range)))
          (cl-loop for row from (nth 0 box) to (min (nth 2 box) (1- rows)) do
                   (cl-loop for col from (nth 1 box) to (min (nth 3 box) (1- cols))
                            unless (and (= row (nth 0 cell)) (= col (nth 1 cell)))
                            do (aset (aref table row) col "")))))
      (let* ((values (mapcar (lambda (row) (append row nil)) table))
             ;; Formulas read as Excel reads a file's.
             (source (let ((exc-calc-legacy-arrays t))
                       (make-exc-default-data-source
                        (copy-sequence (car values)) values
                        ;; A title exc gave the sheet, kept in the
                        ;; workbook's custom properties, else its name.
                        (or (plist-get data :title) sheet))))
             (model (gethash :model source))
             ;; A dynamic array spills, as a formula typed today does;
             ;; only the others are array formulas.
             (arrays (delq nil (mapcar (lambda (cell)
                                         (when-let* ((more (nthcdr 3 cell))
                                                     (range (plist-get more :array))
                                                     ((not (plist-get more :dynamic))))
                                           (cons (cons (nth 0 cell) (nth 1 cell))
                                                 range)))
                                       cells)))
             (dynamic (let ((table (make-hash-table :test 'equal)))
                        (dolist (cell cells table)
                          (when (plist-get (nthcdr 3 cell) :dynamic)
                            (puthash (cons (nth 0 cell) (nth 1 cell)) t table)))))
             (rules (exc-xlsx--rules (plist-get data :conditionals))))
        ;; A merge that reaches down from the first row, such as a title
        ;; over two rows, only exists in a sheet without a header row.
        (when (cl-some (lambda (merge)
                         (and (= (nth 0 merge) 0) (< (nth 0 merge) (nth 2 merge))))
                       merges)
          (puthash :no-header-row t source))
        (when merges
          (let ((exc-model-first-row-is-header
                 (not (gethash :no-header-row source))))
            (set-exc-data-model-merges
             model (exc-model-validate-merges model merges t))))
        (dolist (cell cells)
          (when-let* ((style
                       (let ((style (exc-xlsx--style (nthcdr 3 cell)))
                             (case-fold-search t))
                         ;; A workbook shows a date formula without a date
                         ;; format as a number, and so does the grid.
                         (if (and (not (plist-get style :format))
                                  (stringp (nth 2 cell))
                                  (string-match-p exc--date-formula-regexp
                                                  (nth 2 cell)))
                             (append style (list :format (list 'general)))
                           style))))
            (exc-model-set-style model (nth 0 cell) (nth 1 cell) style)))
        (pcase-dolist (`(,row ,col ,text) (plist-get data :notes))
          (exc-model-set-style
           model row col
           (plist-put (copy-sequence (exc-model-get-style model row col))
                      :note text)))
        (exc-model-set-conditionals model (car rules))
        ;; A width is in characters on both sides; a workbook counts a
        ;; little padding in, which rounding takes off again.  A column
        ;; past the last cell keeps its width too, to be written back.
        (pcase-dolist (`(,col . ,width) (plist-get data :widths))
          (when (and (integerp col) (< -1 col exc-xlsx--max-cols)
                     (numberp width))
            (exc-set-column-width source col
                                   (max 1 (min 200 (round width))))))
        (when-let* ((frozen (plist-get data :frozen)))
          (exc-set-frozen source (nth 0 frozen) (nth 1 frozen)))
        (exc-set-hidden source (plist-get data :hidden-rows)
                               (plist-get data :hidden-cols))
        (exc-set-filters source (plist-get data :filters))
        (exc-set-validations
         source (exc-xlsx--validations (plist-get data :validations)))
        (exc-set-charts
         source (delq nil (mapcar (lambda (entry)
                                    (exc-xlsx--chart source entry))
                                  (plist-get data :charts))))
        ;; The formulas the file holds read as Excel reads them; one
        ;; typed in afterwards is a formula of today, and may spill.
        (exc-model-set-legacy
         model
         (let ((cols (exc-data-model-cols model))
               (cells (exc-data-model-cells model))
               found)
           (dotimes (index (length cells))
             (let ((at (cons (/ index cols) (% index cols))))
               (when (and (exc-calc--formula-p
                           (exc-cell-raw-input (aref cells index)))
                          (not (gethash at dynamic)))
                 (push at found))))
           found))
        (when arrays
          (exc-model-set-arrays model arrays))
        ;; The sheet was computed with every formula read as a file's;
        ;; a dynamic array is computed again, and spills.
        (when (> (hash-table-count dynamic) 0)
          (exc-calc-forget model))
        (when (or arrays (> (hash-table-count dynamic) 0))
          (exc-model-recalculate-all model))
        (puthash :xlsx-tables (plist-get data :tables) source)
        (puthash :xlsx-file file source)
        (puthash :xlsx-sheet sheet source)
        (puthash :xlsx-skipped-conditionals (cdr rules) source)
        source))))

(defun exc-xlsx--external-reader (file)
  "Return the function that reads the workbooks FILE links to.
It takes :sheet, a LINK number, counted from 1 as the file's formulas
count them, and a sheet name, and returns the read-only model of that
sheet; or :name, a LINK, a sheet name and a NAME, and returns NAME read on
the sheets of that workbook, as a formula of the file reads it.  Nil when
the link has no such sheet, or keeps no such name, and #REF! for a name of
a link that is a DDE or an OLE one.  What the link cached is read when a
formula first asks for it: it may hold thousands of cells no formula reads."
  (let ((sheets (make-hash-table :test 'eql))
        (books (make-hash-table :test 'eql))
        (models (make-hash-table :test 'equal)))
    (lambda (op link &rest args)
      (pcase op
        (:sheet
         (when-let* ((pair (assoc-string
                            (car args)
                            (exc-xlsx--external-sheets file link sheets)
                            t)))
           (or (gethash (cons link (car pair)) models)
               (puthash (cons link (car pair))
                        (exc-xlsx--external-model (delq nil (cadr pair)))
                        models))))
        (:name
         (pcase-let ((`(,sheet ,name) args))
           (exc-xlsx--external-name
            (exc-xlsx--external-book file link books)
            link sheet name)))))))

(defun exc-xlsx--external-sheets (file link cache)
  "Return the sheets the LINKth workbook FILE links to cached: (NAME CELLS).
CACHE keeps what was read, under LINK."
  (if (gethash link cache)
      (gethash link cache)
    (puthash link
             (condition-case nil
                 (when-let* ((text (exc-xlsx-module-external-sheets
                                    file link)))
                   (plist-get (car (read-from-string text)) :sheets))
               (error nil))
             cache)))

(defun exc-xlsx--external-book (file link cache)
  "Return what the LINKth workbook FILE links to says of itself, or nil.
It is (:kind KIND :names ((NAME SHEET TEXT) ...)), KIND the kind of link it
is and SHEET the sheet of that workbook the name belongs to, or nil.  CACHE
keeps what was read, under LINK."
  (if (gethash link cache)
      (gethash link cache)
    (puthash link
             (condition-case nil
                 (when-let* ((text (exc-xlsx-module-external-names
                                    file link)))
                   (car (read-from-string text)))
               (error nil))
             cache)))

(defun exc-xlsx--external-name (book link sheet name)
  "Return the definition of NAME, a name of the LINKth workbook, parsed.
BOOK is what the link says of itself, and SHEET the sheet NAME was
qualified with, or nil for one of the whole workbook.  Nil when the link
keeps no such name, and #REF! for one a DDE or an OLE link holds, or no
link of the file holds, as Excel reads no name of a workbook it has no
values of."
  (if (not (eq (plist-get book :kind) 'externalBook))
      (list 'ERROR "#REF!")
    (let* ((names (plist-get book :names))
           (qualified (and (stringp sheet) (not (string-empty-p sheet))))
           (found (or (and qualified
                           (cl-find-if (lambda (entry)
                                         (and (string-equal-ignore-case
                                               name (nth 0 entry))
                                              (stringp (nth 1 entry))
                                              (string-equal-ignore-case
                                               sheet (nth 1 entry))))
                                       names))
                      ;; A name of one sheet does not stand for one of the
                      ;; whole workbook, nor the other way about.
                      (cl-find-if (lambda (entry)
                                    (and (string-equal-ignore-case
                                          name (nth 0 entry))
                                         (null (nth 1 entry))))
                                  names))))
      (when-let* ((text (nth 2 found)))
        (unless (string-empty-p text)
          (condition-case nil
              (exc-calc--external-ast
               (exc-parser-parse (string-trim-left text "="))
               link (nth 1 found))
            (error nil)))))))

(defun exc-xlsx--external-model (cells)
  "Return the model of a sheet of another workbook, of the cached CELLS.
A cell is (ROW COL KIND VALUE), KIND n a number, s a string, b a boolean
or e an error, and CELLS is empty when the link cached none of the sheet.
A cell the link holds no value for is blank."
  (let ((rows 1) (cols 1))
    (dolist (cell cells)
      (setq rows (max rows (1+ (nth 0 cell)))
            cols (max cols (1+ (nth 1 cell)))))
    (let ((model (exc-model-create rows cols)))
      (dolist (cell cells)
        (when-let* ((entry (exc-model-get-cell model (nth 0 cell) (nth 1 cell))))
          (set-exc-cell-raw-input
           entry (exc-xlsx--external-raw (nth 2 cell) (nth 3 cell)))))
      model)))

(defun exc-xlsx--external-raw (kind value)
  "Return the raw input a cached VALUE of KIND is kept as.
Text is marked with an apostrophe, so that it stays text however it reads."
  (pcase kind
    ('n (number-to-string value))
    ('s (concat "'" value))
    ('b (if (eq value :true) "TRUE" "FALSE"))
    (_ value)))

;;;###autoload
(defun exc-xlsx-create-workbook (file)
  "Return a workbook holding every sheet of the xlsx FILE, and its names."
  (let ((exc-format-date-system (exc-xlsx-date-system file)))
    ;; Each sheet, the workbook and its names would each compute every
    ;; formula; only once all are there can a formula read another sheet
    ;; or a name, so they are computed then, once.
    (exc-workbook-with-held-recalculation
      (let ((workbook
             (exc-workbook-create
              (mapcar (lambda (name)
                        (cons name (exc-xlsx-create-data-source
                                    file name)))
                      (or (exc-xlsx-sheet-names file)
                          (error "No sheet in %s" file)))
              t exc-format-date-system)))
        (exc-xlsx-give-names workbook (expand-file-name file))
        workbook))))

;;;###autoload
(defun exc-xlsx-find-file (file &optional sheet)
  "Open the xlsx FILE as a grid, showing its sheet SHEET.
Every sheet of the workbook is opened; SHEET defaults to the first.  The
grid is not tied to the file: `exc-xlsx-export' writes it back."
  (interactive (list (read-file-name "Find xlsx file: " nil nil t)))
  (let* ((workbook (exc-xlsx-create-workbook file))
         (sheets (exc-workbook-sheets workbook))
         (source (or (and sheet (exc-workbook-sheet workbook sheet))
                     (car sheets)))
         (skipped (apply #'+ (mapcar (lambda (sheet)
                                       (gethash :xlsx-skipped-conditionals
                                                sheet))
                                     sheets)))
         (buffer (generate-new-buffer (file-name-nondirectory file))))
    (with-current-buffer buffer
      (setq-local exc--title (gethash :title source))
      (exc--show-grid-in-current-buffer source))
    (switch-to-buffer buffer)
    (when (> skipped 0)
      (message "%d conditional format%s exc has no counterpart for %s left out"
               skipped (if (= skipped 1) "" "s")
               (if (= skipped 1) "was" "were")))
    buffer))

;;;----------------------------------------------------------------------
;;; Writing
;;;----------------------------------------------------------------------

(defun exc-xlsx--clean (text)
  "Return TEXT without the characters that separate records and fields."
  (replace-regexp-in-string "[\x1d\x1e\x1f]" "" text))

(defun exc-xlsx--escape-x (text)
  "Return TEXT as a file keeps it.
A control character XML can't hold, or a carriage return it would take
for a line break, is written _xHHHH_, and the _ of text that reads as
such an escape _x005F_."
  (replace-regexp-in-string
   "_\\(x[[:xdigit:]]\\{4\\}_\\)\\|[\x00-\x08\x0b-\x1f\ufffe\uffff]"
   (lambda (match)
     (if (> (length match) 1)
         (concat "_x005F_" (substring match 1))
       (format "_x%04X_" (aref match 0))))
   text t t))

(defun exc-xlsx--rgb (color)
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

(defun exc-xlsx--style-fields (style)
  "Return the fields that describe the exc STYLE to the module."
  (list (if (plist-get style :bold) "1" "")
        (if (plist-get style :italic) "1" "")
        (exc-xlsx--rgb (plist-get style :color))
        (exc-xlsx--rgb (plist-get style :background))
        (if-let* ((align (plist-get style :align)))
            (format "%s" align)
          "")
        (or (exc-format-to-code (plist-get style :format)) "")))

(defun exc-xlsx--array-kind (model row col)
  "Return the record kind of the formula at ROW and COL of MODEL, or nil.
a:RANGE for an array formula filling RANGE, d:RANGE for a formula that
spills over RANGE, which a workbook marks as a dynamic array; nil for a
plain formula."
  (cond
   ((exc-model-array-at model row col)
    (concat "a:" (exc-model-array-at model row col)))
   ((exc-calc-spill-range model row col)
    (concat "d:" (exc-calc-spill-range model row col)))
   ;; Blocked, it is still a dynamic array, of its own cell; so is one
   ;; giving a single value that a file's formula would compute otherwise.
   ((or (exc-calc-spill-blocked-p model row col)
        (exc-calc-legacy-differs-p model row col))
    (format "d:%s%d" (exc-calc-col-number-to-letter col) (1+ row)))))

(defun exc-xlsx--cell-record (row col raw style &optional array)
  "Return the record for the cell at ROW and COL, or nil for nothing.
RAW is its raw input and STYLE its style.  ARRAY, for an array formula,
is its record kind, as `exc-xlsx--array-kind' gives it."
  (let* ((case-fold-search t)
         (text (format "%s" (or raw "")))
         (kind (cond
                ;; Text by its apostrophe; a number keeps its kind under
                ;; the text format, as it does in a spreadsheet.
                ((string-prefix-p "'" text)
                 (setq text (substring text 1))
                 ;; Empty text, which is not nothing, is t.
                 (if (string-empty-p text) "t" "s"))
                ((and (string-prefix-p "=" text) (> (length text) 1)
                      (not (string-match-p
                            exc-xlsx--own-formula-regexp text)))
                 (setq text (exc-xlsx--prefix-functions
                             (substring text 1)))
                 (or array "f"))
                ((string-match-p exc-xlsx--number-regexp text) "n")
                ((member text '("TRUE" "FALSE")) "b")
                ((member text exc-calc-error-values) "e")
                (t "s"))))
    ;; Text keeps the characters XML can't hold, and those that separate
    ;; records and fields here, escaped; the rest loses them.
    (setq text (if (equal kind "s")
                   (exc-xlsx--escape-x text)
                 (exc-xlsx--clean text)))
    (unless (and (string-empty-p text) (null style) (not (equal kind "t")))
      (mapconcat #'identity
                 (append (list "C" (number-to-string row)
                               (number-to-string col) kind text)
                         (exc-xlsx--style-fields style))
                 exc-xlsx--field))))

(defun exc-xlsx--rule-record (rule)
  "Return the record for the conditional format RULE."
  (pcase-let* ((`(,top ,left ,bottom ,right ,op ,value ,style) rule)
               (first (exc-xlsx--cell-name top left))
               (value (exc-xlsx--clean (format "%s" (or value ""))))
               (quoted (format "\"%s\""
                               (replace-regexp-in-string "\"" "\"\"" value))))
    (mapconcat
     #'identity
     (append
      (list "F" (exc-xlsx--range-name top left bottom right) op
            (pcase op
              ("contains" (format "NOT(ISERROR(SEARCH(%s,%s)))" quoted first))
              ("empty" (format "LEN(TRIM(%s))=0" first))
              ("error" (format "ISERROR(%s)" first))
              (_ (if (string-match-p exc-xlsx--number-regexp value)
                     value
                   quoted)))
            value)
      ;; A conditional format has no alignment and no number format.
      (butlast (exc-xlsx--style-fields style) 2))
     exc-xlsx--field)))

(defconst exc-xlsx--item (string #x1d)
  "Separator of the items of a list inside a field.")

(defun exc-xlsx--chart-record (source chart sheet)
  "Return the record for floating CHART of SOURCE, written as sheet SHEET.
Its data ranges name SHEET, so the workbook's chart reads the cells it
was drawn from; its size in characters and lines becomes the columns
and rows it covers, and its :offset how far into its first column it
starts."
  (let* ((data (exc-chart-data source chart))
         (type (plist-get chart :type))
         (prefix (exc-calc-sheet-prefix sheet))
         (absolute (lambda (range)
                     (pcase-let ((`(,top ,left ,bottom ,right) range))
                       (format "%s$%s$%d:$%s$%d" prefix
                               (exc-xlsx--column-name left) (1+ top)
                               (exc-xlsx--column-name right) (1+ bottom)))))
         (ranges (plist-get data :ranges))
         (labels (plist-get data :category-range))
         (anchor (plist-get chart :anchor))
         (size (plist-get chart :size))
         (widths (exc-get-column-widths source))
         (offset (or (car (plist-get chart :offset)) 0))
         ;; The column the chart's right edge reaches and how far into
         ;; it, in EMU, from where it starts within its first column.
         (right (let ((col (cdr anchor)) (room (+ offset (car size))))
                  (while (let ((width (or (and widths (gethash col widths))
                                          exc-xlsx--default-width)))
                           (when (and (> width 0) (>= room width))
                             (setq room (- room width)
                                   col (1+ col))))
                    nil)
                  (cons col (round (* room exc-xlsx--emu-per-char)))))
         (corner (lambda (row col emu)
                   (let ((cell (exc-xlsx--cell-name row col)))
                     (if (> emu 0)
                         (format "%s%s%d" cell exc-xlsx--item emu)
                       cell))))
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
              (funcall corner (car anchor) (cdr anchor)
                       (round (* offset exc-xlsx--emu-per-char)))
              (funcall corner (+ (car anchor) (cdr size)) (car right) (cdr right))
              (exc-xlsx--clean (or (plist-get chart :title) ""))
              (let ((given (plist-get chart :series-names)))
                (if (and given (= (length given) (length ranges)))
                    ;; Names of their own: none, text, or cells elsewhere.
                    (mapconcat (lambda (name)
                                 (pcase name
                                   (`(text ,text)
                                    (concat "\"" (exc-xlsx--clean text)))
                                   ((pred stringp)
                                    (pcase (exc-chart-svg-parse-range name)
                                      (`(,top ,left ,top ,left)
                                       (format "%s$%s$%d" prefix
                                               (exc-xlsx--column-name left)
                                               (1+ top)))
                                      ((and box (pred consp))
                                       (funcall absolute box))
                                      (_ "")))
                                   (_ "")))
                               given exc-xlsx--item)
                  (mapconcat absolute (plist-get data :names) exc-xlsx--item)))
              ;; The cells holding the category labels; none when the
              ;; categories are only numbered.
              (if (and labels (not (eq type 'scatter)))
                  (funcall absolute labels)
                ""))
        refs)
       exc-xlsx--field))))

(defun exc-xlsx--column-text (name)
  "Return the column NAME of a table as its header cell shows it.
A file may keep a line break as _x000a_ and a character as an XML
entity, even one escaped again."
  (let ((text name) last)
    (while (not (equal text last))
      (setq last text
            text (replace-regexp-in-string
                  "_x\\([[:xdigit:]]\\{4\\}\\)_\\|&#x\\([[:xdigit:]]+\\);\\|&#\\([0-9]+\\);\\|&\\(amp\\|lt\\|gt\\|quot\\|apos\\);"
                  (lambda (match)
                    (save-match-data
                      (string-match "\\`\\(?:_x\\(.*\\)_\\|&#x\\(.*\\);\\|&#\\(.*\\);\\|&\\(.*\\);\\)\\'" match)
                      (let ((hex (or (match-string 1 match) (match-string 2 match)))
                            (decimal (match-string 3 match))
                            (entity (match-string 4 match)))
                        (cond (hex (string (string-to-number hex 16)))
                              (decimal (string (string-to-number decimal)))
                              (t (cdr (assoc entity '(("amp" . "&") ("lt" . "<")
                                                      ("gt" . ">") ("quot" . "\"")
                                                      ("apos" . "'")))))))))
                  text t t)))
    text))

(defun exc-xlsx--table-record (source table)
  "Return the T record of TABLE, on the sheet SOURCE.
TABLE is (NAME SHEET TOP LEFT BOTTOM RIGHT HEADERS TOTALS COLUMNS).  A
file must name a column as its header cell shows it: a header edited
names its column after it, unless it holds a formula or nothing.  A
name the header agrees with is kept as the file wrote it."
  (pcase-let ((`(,name ,_ ,top ,left ,bottom ,right ,headers ,totals ,columns)
               table))
    (mapconcat
     #'identity
     (append
      (list "T" name)
      (mapcar #'number-to-string (list top left bottom right headers totals))
      (cl-loop for column in columns
               for col from left
               for raw = (and (> headers 0) (exc-get-raw-value-at source top col))
               ;; A number kept as text is written with a quote first.
               for header = (if (and (stringp raw) (string-prefix-p "'" raw))
                                (substring raw 1)
                              raw)
               collect (exc-xlsx--clean
                        (if (and (stringp header) (not (string-empty-p header))
                                 (not (string-prefix-p "=" header))
                                 ;; A line break may be kept as CR LF in the cell.
                                 (not (equal (string-replace "\r" "" header)
                                             (string-replace
                                              "\r" "" (exc-xlsx--column-text
                                                       column)))))
                            header
                          column))))
     exc-xlsx--field)))

(defun exc-xlsx--payload (source &optional sheet)
  "Return the records that describe the data source SOURCE.
SHEET is the name SOURCE is written as, which its charts' ranges name."
  (let* ((rows (exc-get-row-count source))
         (cols (exc-get-column-count source))
         ;; Formulas are computed again to tell how a workbook reads
         ;; them, among the sheet's workbook as a recalculation is.
         (workbook (exc-workbook-of source))
         (exc-calc-sheets (and workbook (exc-workbook--resolver workbook)))
         (exc-calc-names (and workbook (exc-workbook--names-resolver workbook)))
         (exc-calc-tables (and workbook (exc-workbook--tables-resolver workbook)))
         (exc-format-date-system (exc-workbook-date-system-of workbook))
         (title (gethash :title source))
         ;; The workbook's date system, which a new file would not know,
         ;; and a title other than the sheet's name, which Excel has no
         ;; place for and the workbook's custom properties keep.
         (records (append
                   (and workbook
                        (list (concat "Y" exc-xlsx--field
                                      (number-to-string exc-format-date-system))))
                   (and (stringp title) (not (string-empty-p title))
                        (not (equal title sheet))
                        (list (concat "S" exc-xlsx--field
                                      (exc-xlsx--clean title)))))))
    (dotimes (row rows)
      (dotimes (col cols)
        (when-let* ((record
                     (exc-xlsx--cell-record
                      row col
                      (exc-get-raw-value-at source row col)
                      ;; A date formula is shown as a date without having
                      ;; the format; a workbook needs to be told.
                      (let ((style (exc-get-style-at source row col))
                            (format (exc-cell-format source row col)))
                        (if (and format (not (plist-get style :format)))
                            (plist-put (copy-sequence style) :format format)
                          style))
                      (exc-xlsx--array-kind
                       (gethash :model source) row col))))
          (push record records))
        (let ((style (exc-get-style-at source row col)))
          (dolist (entry '((:note . "N") (:link . "L")))
            (when-let* ((text (plist-get style (car entry))))
              (push (mapconcat #'identity
                               (list (cdr entry) (number-to-string row)
                                     (number-to-string col)
                                     (exc-xlsx--clean text))
                               exc-xlsx--field)
                    records))))))
    ;; The sheet's tables, all of them: U says the T records that follow
    ;; are every table the sheet has.
    (when workbook
      (push "U" records)
      (dolist (table (exc-workbook-tables workbook))
        (when (exc-calc--same-sheet-p (nth 1 table)
                                       (exc-workbook-sheet-name source))
          (push (exc-xlsx--table-record source table) records))))
    (dolist (merge (exc-get-merges source))
      (push (concat "M" exc-xlsx--field
                    (apply #'exc-xlsx--range-name merge))
            records))
    (dolist (rule (exc-get-conditional-formats source))
      (push (exc-xlsx--rule-record rule) records))
    ;; Columns sized by hand; the others keep what the workbook has.
    (when-let* ((widths (exc-get-column-widths source)))
      (maphash (lambda (col width)
                 (when (< col exc-xlsx--max-cols)
                   (push (format "W%s%d%s%d" exc-xlsx--field col
                                 exc-xlsx--field width)
                         records)))
               widths))
    (let ((frozen (exc-frozen source)))
      (push (format "P%s%d%s%d" exc-xlsx--field (car frozen)
                    exc-xlsx--field (cdr frozen))
            records))
    (let ((hidden (exc-hidden source)))
      (dolist (row (car hidden))
        (push (format "H%sr%s%d" exc-xlsx--field exc-xlsx--field row)
              records))
      (dolist (col (cdr hidden))
        (push (format "H%sc%s%d" exc-xlsx--field exc-xlsx--field col)
              records)))
    (dolist (range (exc-filters source))
      (push (mapconcat #'identity (cons "A" (mapcar #'number-to-string range))
                       exc-xlsx--field)
            records))
    (dolist (rule (exc-validations source))
      (pcase-let ((`(,top ,left ,bottom ,right ,_kind ,spec) rule))
        (push (concat "V" exc-xlsx--field
                      (exc-xlsx--range-name top left bottom right)
                      exc-xlsx--field
                      (exc-xlsx--clean
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
      (dolist (chart (exc-charts source))
        (when-let* ((record (exc-xlsx--chart-record source chart sheet)))
          (push record records))))
    (mapconcat #'identity (nreverse records) exc-xlsx--record)))

(defun exc-xlsx--sheet-name (name)
  "Return NAME as a name a workbook accepts for a sheet."
  (let ((name (string-trim
               (replace-regexp-in-string "[][:*?/\\\\]" " " (or name "")))))
    (cond ((string-empty-p name) "Sheet1")
          ((> (length name) 31) (substring name 0 31))
          (t name))))

(defun exc-xlsx-write-data-source (source file &optional sheet)
  "Write the data source SOURCE as sheet SHEET of the xlsx FILE.
When FILE exists its other sheets are kept and SHEET is replaced or
added.  SHEET defaults to the sheet SOURCE came from, else its title."
  (exc-xlsx--ensure-module)
  (setq file (expand-file-name file))
  (let* ((sheet (exc-xlsx--sheet-name
                 (or sheet (gethash :xlsx-sheet source)
                     (gethash :title source))))
         ;; Write beside FILE and rename, so that a failure leaves it whole.
         (temporary (make-temp-name (concat file ".tmp"))))
    (unwind-protect
        (progn
          (exc-xlsx-module-write
           temporary (and (file-exists-p file) file) sheet
           (exc-xlsx--payload source sheet))
          ;; Names, written for the sheet as the workbook calls it, join
          ;; the ones the file has.
          (when-let* ((workbook (exc-workbook-of source))
                      (names (exc-workbook-names workbook)))
            (let ((own (exc-workbook-sheet-name source)))
              (exc-xlsx--write-names
               temporary
               (mapcar (lambda (entry)
                         (cons (car entry)
                               (exc-calc-rename-sheet-in-formula
                                (cdr entry) own sheet)))
                       names)
               t)))
          (rename-file temporary file t))
      (when (file-exists-p temporary)
        (delete-file temporary)))
    (puthash :xlsx-file file source)
    (puthash :xlsx-sheet sheet source)
    sheet))

(defun exc-xlsx-write-workbook (workbook file)
  "Write every sheet of WORKBOOK to the xlsx FILE.
When FILE exists, sheets of it with other names are kept, unless
WORKBOOK was opened from FILE: then FILE ends up with WORKBOOK's sheets
and no others."
  (exc-xlsx--ensure-module)
  (setq file (expand-file-name file))
  (let* ((sheets (exc-workbook-sheets workbook))
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
              (exc-xlsx-module-write
               target written (exc-workbook-sheet-name sheet)
               (exc-xlsx--payload sheet (exc-workbook-sheet-name sheet)))
              (setq written target
                    spare (reverse spare))))
          (when own
            (exc-xlsx-module-prune
             written
             (mapconcat #'exc-workbook-sheet-name sheets
                        exc-xlsx--record)))
          ;; The workbook's names are FILE's when it came from FILE; a
          ;; file it did not come from keeps its own besides.
          (when (or own (exc-workbook-names workbook))
            (exc-xlsx--write-names
             written (exc-workbook-names workbook) (not own)))
          (rename-file written file t))
      (dolist (temporary spare)
        (when (file-exists-p temporary)
          (delete-file temporary))))
    (dolist (sheet sheets)
      (puthash :xlsx-file file sheet)
      (puthash :xlsx-sheet (exc-workbook-sheet-name sheet) sheet))
    (length sheets)))

;;;###autoload
(defun exc-xlsx-export (file &optional sheet)
  "Write the current grid to the xlsx FILE.
A grid with several sheets is written whole.  A grid that stands alone
becomes the sheet SHEET of FILE.  The file the grid was opened from is
the default, and an existing workbook keeps the sheets the grid does not
replace."
  (interactive
   (let* ((source (or exc--data-source
                      (user-error "No grid in this buffer")))
          (origin (gethash :xlsx-file source))
          (file (expand-file-name
                 (read-file-name "Export to xlsx file: "
                                 (and origin (file-name-directory origin))
                                 origin nil
                                 (and origin
                                      (file-name-nondirectory origin))))))
     (list file
           (unless (cdr (exc-sheets))
             (let ((default (exc-xlsx--sheet-name
                             (or (exc-workbook-sheet-name source)
                                 (gethash :xlsx-sheet source)
                                 exc--title))))
               (read-string (format "Sheet (default %s): " default)
                            nil nil default))))))
  (unless exc--data-source
    (user-error "No grid in this buffer"))
  (unless (string-suffix-p ".xlsx" file t)
    (setq file (concat file ".xlsx")))
  (setq file (expand-file-name file))
  (let* ((source exc--data-source)
         (sheets (exc-sheets))
         (names (if (cdr sheets)
                    (mapcar #'exc-workbook-sheet-name sheets)
                  (list (exc-xlsx--sheet-name
                         (or sheet (exc-workbook-sheet-name source)
                             (gethash :xlsx-sheet source)
                             exc--title)))))
         (origin (equal file (gethash :xlsx-file source))))
    (when (and (called-interactively-p 'interactive)
               (file-exists-p file)
               (not origin)
               (cl-intersection names (exc-xlsx-sheet-names file)
                                :test #'string=)
               (not (y-or-n-p (format "Replace sheet%s %s of %s? "
                                      (if (cdr names) "s" "")
                                      (string-join names ", ")
                                      (file-name-nondirectory file)))))
      (user-error "Export canceled"))
    ;; The title shown is the one the sheet keeps.
    (puthash :title exc--title source)
    (if (cdr sheets)
        (exc-xlsx-write-workbook (exc-workbook-of source) file)
      (exc-xlsx-write-data-source source file (car names)))
    (message "%s written to %s"
             (if (cdr names)
                 (format "%d sheets" (length names))
               (format "Sheet %s" (car names)))
             (abbreviate-file-name file))))

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun exc-xlsx-init ()
  "Register the xlsx plugin."
  (exc-register-plugin 'xlsx)
  (exc-register-data-source-type
   'xlsx #'exc-xlsx-create-data-source))

(exc-xlsx-init)

(provide 'exc-xlsx)

;;; exc-xlsx.el ends here
