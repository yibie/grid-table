;;; exc-xlsx-test.el --- xlsx plugin tests -*- lexical-binding: t; -*-

;;; Commentary:

;; The tests that read or write a workbook need the xlsx module; build it
;; with `make module' and run them with `make test-xlsx'.  They are skipped
;; when the module cannot be loaded.
;;
;; test/fixtures/styled.xlsx was written by openpyxl.  Sheet 销售 has a
;; styled header in B2:E2, three rows of data with =C*D formulas in column
;; E, a boolean, a date and a percentage in column F, a merge over B7:D8, a
;; "greater than 5" conditional format on C3:C5 and a total in E6.  Sheet
;; Sheet2 holds a formula that reads the first sheet and a two-line text.
;;
;; test/fixtures/links.xlsx is a hand-written package with two external
;; links: xl/externalLinks/externalLink1.xml, an externalBook relating it
;; to other.xlsx, and externalLink2.xml, a DDE one.  The first caches a
;; small sheet, A1:B3 numbers, C1 text, D1 TRUE and E1 #N/A, lists two
;; more sheets, one whose refresh failed and one it read as empty, and
;; keeps five names, one of a cell, one of a range, one of a sheet, one
;; defined as #REF! and one with no definition at all, and the workbook
;; itself a name of two cells added together.  The workbook's
;; Sheet1 holds in A1 to A25 the formulas that read all of it: cells,
;; ranges, a whole column, a VLOOKUP, a name of this workbook pointing
;; into it, sheets and links it has not, INDIRECT naming it, what it
;; cached as text, TRUE, an error and a blank, and those names, read
;; through [1]!Name, [1]Sheet1!Name, [0]!Name and a table of it, and in
;; A26 the name pair the workbook keeps.
;;
;; test/fixtures/compat.xlsx began as a workbook exc wrote and was
;; then edited by hand into shapes other writers produce and umya used to
;; panic on or drop: cells of row 2 carry no reference, A3:B3 holds one
;; hyperlink, the page setup names printer settings the package lacks, the
;; chart frame links to #Data!A1, and C2 is the text 'quoted.  It also has
;; a hidden row 40 and a width for column Z, both past the data.

;;; Code:

(require 'ert)
(setq load-prefer-newer t)
(require 'exc)
(require 'exc-xlsx)

(defconst exc-xlsx-test--fixture
  (expand-file-name "fixtures/styled.xlsx"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst exc-xlsx-test--compat
  (expand-file-name "compat.xlsx"
                    (file-name-directory exc-xlsx-test--fixture)))

(defconst exc-xlsx-test--links
  (expand-file-name "links.xlsx"
                    (file-name-directory exc-xlsx-test--fixture))
  "A workbook with one external link, as Excel writes it.")

(defconst exc-xlsx-test--available
  (ignore-errors (exc-xlsx--ensure-module) t))

(defmacro exc-xlsx-test--with-file (var &rest body)
  "Run BODY with VAR bound to the name of an xlsx file that does not exist."
  (declare (indent 1))
  `(let ((,var (concat (make-temp-name
                        (expand-file-name "exc-xlsx-"
                                          temporary-file-directory))
                       ".xlsx")))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defun exc-xlsx-test--unzip (file part)
  "Return PART of the xlsx FILE, as unzip gives it.
A part named [Content_Types].xml is no pattern to unzip."
  (with-temp-buffer
    (call-process "unzip" nil t nil "-p" file
                  (mapconcat (lambda (char)
                               (if (memq char '(?[ ?] ?* ?? ?\\))
                                   (concat "\\" (string char))
                                 (string char)))
                             part ""))
    (buffer-string)))

(defun exc-xlsx-test--raw (source row col)
  "Return the raw value at ROW and COL of SOURCE."
  (exc-get-raw-value-at source row col))

(ert-deftest exc-xlsx-test-cell-references ()
  (should (equal (exc-xlsx--column-name 0) "A"))
  (should (equal (exc-xlsx--column-name 25) "Z"))
  (should (equal (exc-xlsx--column-name 26) "AA"))
  (should (equal (exc-xlsx--column-name 701) "ZZ"))
  (should (equal (exc-xlsx--column-name 702) "AAA"))
  (should (equal (exc-xlsx--parse-cell "AA10") '(9 . 26)))
  (should (equal (exc-xlsx--parse-cell "$B$7") '(6 . 1)))
  (should (equal (exc-xlsx--parse-range "B7:D8") '(6 1 7 3)))
  (should (equal (exc-xlsx--parse-range "C3") '(2 2 2 2)))
  (should-not (exc-xlsx--parse-range "Sheet1"))
  (should (equal (exc-xlsx--range-name 6 1 7 3) "B7:D8")))

(ert-deftest exc-xlsx-test-number-formats ()
  ;; A date stays the number it is and takes a date format.
  (should (equal (exc-xlsx--raw '(0 0 "46307" :format "m/d/yyyy"))
                 "46307"))
  (should (equal (exc-xlsx--style '(:format "m/d/yyyy"))
                 '(:format (date "m/d/yyyy"))))
  (should (equal (exc-xlsx--style '(:bold t :format "0.0%"))
                 '(:bold t :format (percent 1))))
  (should-not (exc-xlsx--style '(:format "General")))
  ;; A format without a counterpart is kept for the way back.
  (should (equal (exc-xlsx--style '(:format "#,##0 \"days\""))
                 '(:format (code "#,##0 \"days\""))))
  (should (equal (nth 5 (exc-xlsx--style-fields
                         '(:format (code "#,##0 \"days\""))))
                 "#,##0 \"days\""))
  (should (equal (nth 5 (exc-xlsx--style-fields
                         '(:format (date "yyyy-mm-dd"))))
                 "yyyy-mm-dd"))
  ;; Text that reads as a number is marked as text.
  (should (equal (exc-xlsx--raw '(0 0 "007" :text "1")) "'007"))
  ;; So is text that spells an error value.
  (should (equal (exc-xlsx--raw '(0 0 "#N/A" :text "1")) "'#N/A")))

(ert-deftest exc-xlsx-test-cell-records ()
  (let ((fields (lambda (record)
                  (split-string record exc-xlsx--field))))
    (should (equal (seq-take (funcall fields (exc-xlsx--cell-record
                                              2 4 "=C3*D3" nil))
                             5)
                   '("C" "2" "4" "f" "C3*D3")))
    (should (equal (nth 3 (funcall fields (exc-xlsx--cell-record
                                           0 0 "-1.5e3" nil)))
                   "n"))
    (should (equal (nth 3 (funcall fields (exc-xlsx--cell-record
                                           0 0 "1.2.3" nil)))
                   "s"))
    ;; Only exc evaluates a chart formula; it leaves as text.
    (should (equal (seq-subseq (funcall fields (exc-xlsx--cell-record
                                                0 0 "=barplot(A1:A3)" nil))
                               3 5)
                   '("s" "=barplot(A1:A3)")))
    ;; Text marked as text leaves as text, without its mark.
    (should (equal (seq-subseq (funcall fields (exc-xlsx--cell-record
                                                0 0 "'007" nil))
                               3 5)
                   '("s" "007")))
    ;; A number under the text format is still a number.
    (should (equal (nth 3 (funcall fields (exc-xlsx--cell-record
                                           0 0 "12" '(:format (text)))))
                   "n"))
    (should (equal (seq-subseq (funcall fields (exc-xlsx--cell-record
                                                0 0 "''x" nil))
                               3 5)
                   '("s" "'x")))
    (should-not (exc-xlsx--cell-record 0 0 "" nil))
    (should (equal (seq-drop (funcall fields
                                      (exc-xlsx--cell-record
                                       0 0 "" '(:bold t :background "#ffc7ce"
                                                :align center)))
                             5)
                   '("1" "" "" "FFC7CE" "center" "")))))

(ert-deftest exc-xlsx-test-reads-a-workbook ()
  (skip-unless exc-xlsx-test--available)
  (should (equal (exc-xlsx-sheet-names exc-xlsx-test--fixture)
                 '("销售" "Sheet2")))
  (let* ((source (exc-xlsx-create-data-source
                  exc-xlsx-test--fixture))
         (model (gethash :model source)))
    (should (equal (gethash :xlsx-sheet source) "销售"))
    (should (equal (list (exc-get-row-count source)
                         (exc-get-column-count source))
                   '(8 6)))
    (should (equal (exc-xlsx-test--raw source 1 1) "产品"))
    (should (equal (exc-xlsx-test--raw source 3 1) "He said \"hi\""))
    (should (equal (exc-xlsx-test--raw source 4 1) "back\\slash"))
    (should (equal (exc-xlsx-test--raw source 2 4) "=C3*D3"))
    (should (equal (exc-get-computed-value-at source 2 4) 7.5))
    (should (equal (exc-get-computed-value-at source 5 4) 56.25))
    (should (equal (exc-xlsx-test--raw source 2 5) "TRUE"))
    ;; A date and a percentage come in as the numbers they hold, shown
    ;; through their formats.
    (should (equal (exc-xlsx-test--raw source 3 5) "46307"))
    (should (equal (exc-get-display-value-at source 3 5) "2026-10-12"))
    (should (equal (exc-xlsx-test--raw source 4 5) "0.125"))
    (should (equal (exc-get-display-value-at source 4 5) "12.5%"))
    (should (equal (exc-model-get-merges model) '((6 1 7 3))))
    (should (equal (exc-model-get-style model 1 1)
                   '(:bold t :color "#FFFFFF" :background "#4472C4"
                           :align center)))
    (should (equal (exc-model-get-style model 2 1)
                   '(:italic t :color "#00B050")))
    (should (equal (exc-model-get-style model 3 3)
                   '(:align right :format (number 2 nil))))
    (should (equal (exc-model-get-conditionals model)
                   '((2 2 4 2 ">" "5" (:bold t :background "#FFC7CE")))))
    (should (equal (gethash :xlsx-skipped-conditionals source) 0)))
  (let ((source (exc-xlsx-create-data-source
                 exc-xlsx-test--fixture "Sheet2")))
    (should (equal (exc-xlsx-test--raw source 0 0) "=销售!E6*2"))
    (should (equal (exc-xlsx-test--raw source 1 0) "line1\nline2"))))

(ert-deftest exc-xlsx-test-formats-round-trip ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let ((source (make-exc-default-data-source
                   '("" "")
                   '(("label" "value") ("date" "") ("pct" "") ("money" "")
                     ("text" "") ("later" "") ("days" "5")))))
      (exc-enter-value source 1 1 "2026-10-13")
      (exc-enter-value source 2 1 "12.5%")
      (exc-enter-value source 3 1 "¥1,234.50")
      (exc-enter-value source 4 1 "'007")
      (exc-set-raw-value-at source 5 1 "=DATE(2026,10,13)+7")
      (exc-set-style-at source 6 1 '(:format (code "#,##0 \"days\"")))
      (exc-xlsx-write-data-source source file "Formats")
      (let ((back (exc-xlsx-create-data-source file "Formats")))
        (should (equal (exc-get-raw-value-at back 1 1) "46308"))
        (should (equal (exc-get-display-value-at back 1 1) "2026-10-13"))
        (should (equal (exc-get-display-value-at back 2 1) "12.5%"))
        (should (equal (exc-get-display-value-at back 3 1) "¥1,234.50"))
        ;; Text that reads as a number is still text.
        (should (equal (exc-get-raw-value-at back 4 1) "'007"))
        ;; A date formula is given its format on the way out.
        (should (equal (exc-get-display-value-at back 5 1) "2026-10-20"))
        (should (equal (plist-get (exc-get-style-at back 5 1) :format)
                       '(date "yyyy-mm-dd")))
        ;; A format the grid does not know comes back untouched.
        (should (equal (plist-get (exc-get-style-at back 6 1) :format)
                       '(code "#,##0 \"days\"")))
        (should (exc-header-row-p back)))
      ;; A title merged down from the first row makes the sheet one
      ;; without a header row, so the merge is kept.
      (exc-set-header-row source nil)
      (exc-set-raw-value-at source 1 0 "")
      (exc-source-merge-cells source 0 0 1 0)
      (exc-xlsx-write-data-source source file "Formats")
      (let ((back (exc-xlsx-create-data-source file "Formats")))
        (should (equal (exc-get-merges back) '((0 0 1 0))))
        (should-not (exc-header-row-p back))))))

;; Text a workbook holds stays the text it was, whatever it reads as, and
;; a number stays a number under the text format.
(ert-deftest exc-xlsx-test-text-keeps-what-it-is ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((texts '("'" "'[Book1]Sheet1'!$C2" "=A1" "TRUE" "007" "plain"))
           (source (make-exc-default-data-source
                    '("" "")
                    (append (mapcar (lambda (text) (list (concat "'" text) ""))
                                    texts)
                            '(("3" "=SUM(A7:A7)") ("'#N/A" "") ("#N/A" "") ("#SPILL!" "")
                              ("=\"#N/A\"" "=ISTEXT(A11)") ("=NA()" "=ISERROR(A12)")
                              ("a\^Ab\rc\x1d" "") ("_x0041_" ""))))))
      (exc-set-style-at source 6 0 '(:format (text)))
      (exc-xlsx-write-data-source source file "Texts")
      (let ((back (exc-xlsx-create-data-source file "Texts")))
        (seq-do-indexed
         (lambda (text row)
           (should (equal (exc-get-display-value-at back row 0) text)))
         texts)
        ;; Text that reads as nothing else needs no apostrophe.
        (should (equal (exc-get-raw-value-at back 5 0) "plain"))
        (should (equal (exc-get-raw-value-at back 6 0) "3"))
        (should (equal (plist-get (exc-get-style-at back 6 0) :format)
                       '(text)))
        (should (equal (exc-get-computed-value-at back 6 1) 3))
        ;; Text that spells an error value stays text, and an error
        ;; value stays one.
        (should (equal (exc-get-raw-value-at back 7 0) "'#N/A"))
        (should (equal (exc-get-raw-value-at back 8 0) "#N/A"))
        (should (equal (exc-get-raw-value-at back 9 0) "#SPILL!"))
        ;; So with what a formula gives.
        (should (equal (exc-get-computed-value-at back 10 1) t))
        (should (equal (exc-get-computed-value-at back 11 1) t))
        ;; A control character, which a file keeps escaped, and text
        ;; that reads as such an escape
        (should (equal (exc-get-raw-value-at back 12 0) "a\^Ab\rc\x1d"))
        (should (equal (exc-get-raw-value-at back 13 0) "_x0041_"))))))

(ert-deftest exc-xlsx-test-empty-text-is-no-blank ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    ;; A1 holds empty text, B1 nothing.
    (let ((source (make-exc-default-data-source
                   '("" "" "" "" "")
                   '(("'" "" "=A1" "=ISBLANK(A1)" "=COUNTA(A1:B1)")))))
      (exc-xlsx-write-data-source source file "Empty")
      (let ((back (exc-xlsx-create-data-source file "Empty")))
        (should (equal (exc-get-raw-value-at back 0 0) "'"))
        (should (equal (exc-get-display-value-at back 0 0) ""))
        (should (equal (exc-get-raw-value-at back 0 1) ""))
        (should (equal (exc-get-computed-value-at back 0 2) ""))
        (should-not (exc-get-computed-value-at back 0 3))
        (should (equal (exc-get-computed-value-at back 0 4) 1))))))

(ert-deftest exc-xlsx-test-filtered-rows ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let ((source (make-exc-default-data-source
                   '("" "")
                   '(("V" "") ("1" "") ("2" "") ("3" "")
                     ("" "=SUBTOTAL(9,A2:A4)")))))
      (exc-set-hidden source '(2) nil)
      (exc-set-filters source '((0 0 3 0)))
      (exc-xlsx-write-data-source source file "Filtered")
      (let ((back (exc-xlsx-create-data-source file "Filtered")))
        (should (equal (exc-filters back) '((0 0 3 0))))
        (should (equal (exc-get-computed-value-at back 4 1) 4))))))

(ert-deftest exc-xlsx-test-spills-and-array-formulas ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let ((source (make-exc-default-data-source
                   '("" "" "")
                   '(("=SEQUENCE(3,2)" "" "") ("" "" "") ("" "" "")
                     ("=SUM(A1:B3)" "" "")))))
      (cl-flet ((shown (source)
                  (cl-loop for row below 4 collect
                           (cl-loop for col below 2 collect
                                    (exc-get-display-value-at source row col)))))
        (should (equal (shown source)
                       '(("1" "2") ("3" "4") ("5" "6") ("21" ""))))
        ;; A formula that spills is saved as a dynamic array over the
        ;; cells it fills, and reads back spilling, not as {=...}.
        (should (equal (exc-xlsx--array-kind (gethash :model source) 0 0)
                       "d:A1:B3"))
        (exc-xlsx-write-data-source source file "Spill")
        (let* ((back (exc-xlsx-create-data-source file "Spill"))
               (model (gethash :model back)))
          (should (equal (shown back) (shown source)))
          (should-not (exc-model-array-at model 0 0))
          (should-not (exc-model-legacy-at model 0 0))
          (should (equal (exc-calc-spill-range model 0 0) "A1:B3"))
          (should (equal (exc-get-raw-value-at back 1 0) ""))
          ;; Saved again, it stays so.
          (exc-xlsx-write-data-source back file "Spill")
          (should (equal (shown (exc-xlsx-create-data-source file "Spill"))
                         (shown source))))
        ;; Blocked, it is a dynamic array of its own cell, and still
        ;; blocked when read back.
        (exc-model-set-raw-value (gethash :model source) 2 1 "x")
        (exc-model-recalculate-all (gethash :model source))
        (should (equal (exc-xlsx--array-kind (gethash :model source) 0 0)
                       "d:A1"))
        (exc-xlsx-write-data-source source file "Spill")
        (should (equal (exc-get-display-value-at
                        (exc-xlsx-create-data-source file "Spill") 0 0)
                       "#SPILL!"))
        ;; One value from a range is marked too, which Excel before
        ;; dynamic arrays would read otherwise; a formula of single
        ;; cells is left plain.
        (let ((model (gethash :model source)))
          (exc-model-set-raw-value model 3 1 "=MAX(ABS(A1:B1))")
          (exc-model-set-raw-value model 3 2 "=A1+1")
          (exc-model-recalculate-all model)
          (should (equal (exc-xlsx--array-kind model 3 1) "d:B4"))
          (should-not (exc-xlsx--array-kind model 3 2))
          (exc-xlsx-write-data-source source file "Spill")
          (let ((model (gethash :model
                                (exc-xlsx-create-data-source file "Spill"))))
            (should-not (exc-model-legacy-at model 3 1))
            (should (equal (exc-xlsx--array-kind model 3 1) "d:B4"))))))))

(ert-deftest exc-xlsx-test-round-trip ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((rows '(("Item" "Qty" "Price" "Total" "Note")
                   ("苹果" "3" "2.5" "=B2*C2" "say \"hi\"")
                   ("pear" "10" "4" "=B3*C3" "two\nlines")
                   ("" "" "" "=SUM(D2:D3)" "=BARPLOT(B2:B3)")
                   ("merged" "" "" "FALSE" "")
                   ("" "" "" "" "")))
           (source (make-exc-default-data-source
                    (copy-sequence (car rows)) rows "Stock"))
           (model (gethash :model source))
           (rules '((1 1 2 1 ">" "5" (:bold t :background "#FFC7CE"))
                    (1 0 2 0 "contains" "pe" (:italic t :color "#0000FF"))
                    (1 4 2 4 "=" "x y" (:background "#00FF00"))
                    (1 2 3 2 "empty" "" (:background "#CCCCCC"))
                    (1 3 3 3 "error" "" (:color "#FF0000")))))
      (puthash :model (exc-model-merge-cells model 4 0 5 2) source)
      (setq model (gethash :model source))
      (exc-model-set-style model 0 0 '(:bold t :align center
                                              :background "#4472C4"))
      (exc-model-set-style model 1 1 '(:italic t :color "#00B050"))
      ;; A style on a cell that holds nothing still travels.
      (exc-model-set-style model 3 0 '(:background "#FFFF00"))
      (exc-model-set-conditionals model rules)
      (should (equal (exc-xlsx-write-data-source source file) "Stock"))
      (should (equal (exc-xlsx-sheet-names file) '("Stock")))
      (let* ((back (exc-xlsx-create-data-source file))
             (back-model (gethash :model back)))
        (should (equal (list (exc-get-row-count back)
                             (exc-get-column-count back))
                       '(6 5)))
        (dotimes (row 6)
          (dotimes (col 5)
            (should (equal (cons (cons row col)
                                 (exc-xlsx-test--raw back row col))
                           (cons (cons row col) (nth col (nth row rows)))))))
        (should (equal (exc-get-computed-value-at back 3 3) 47.5))
        (should (equal (exc-model-get-merges back-model) '((4 0 5 2))))
        (should (equal (exc-model-get-style back-model 0 0)
                       '(:bold t :background "#4472C4" :align center)))
        (should (equal (exc-model-get-style back-model 1 1)
                       '(:italic t :color "#00B050")))
        (should (equal (exc-model-get-style back-model 3 0)
                       '(:background "#FFFF00")))
        (should-not (exc-model-get-style back-model 2 2))
        (should (equal (exc-model-get-conditionals back-model) rules))))))

(ert-deftest exc-xlsx-test-newer-functions-carry-their-prefix ()
  "XLOOKUP, FILTER and their kind are written as a workbook names them.
Excel and LibreOffice read them only with the _xlfn. prefix, and the
grid reads them back without it."
  (should (equal (exc-xlsx--prefix-functions
                  "COUNTA(UNIQUE(SORT(A1:A3)))+LEN(\"FILTER(\")")
                 "COUNTA(_xlfn.UNIQUE(_xlfn._xlws.SORT(A1:A3)))+LEN(\"FILTER(\")"))
  (should (equal (exc-xlsx--prefix-functions "MYXLOOKUP(1)") "MYXLOOKUP(1)"))
  ;; @ is stored as SINGLE, and the names LET declares carry _xlpm.
  (should (equal (exc-xlsx--prefix-functions "@A1:A3*2")
                 "_xlfn.SINGLE(A1:A3)*2"))
  (should (equal (exc-xlsx--prefix-functions "LET(rate,0.1,A1*rate)+LEN(\"rate\")")
                 "_xlfn.LET(_xlpm.rate,0.1,A1*_xlpm.rate)+LEN(\"rate\")"))
  (should (equal (exc-xlsx--unprefix-functions "=_xlfn.LET(_xlpm.x,A1,_xlpm.x*2)")
                 "=LET(x,A1,x*2)"))
  ;; A1# is stored as ANCHORARRAY, and reads back as Excel shows it.
  (should (equal (exc-xlsx--prefix-functions "SUM(C1#)+@'My Sheet'!A1#")
                 "SUM(_xlfn.ANCHORARRAY(C1))+_xlfn.SINGLE(_xlfn.ANCHORARRAY('My Sheet'!A1))"))
  (should (equal (exc-xlsx--prefix-functions "\"A1#\"&IFERROR(A1,#N/A)")
                 "\"A1#\"&IFERROR(A1,#N/A)"))
  (should (equal (exc-xlsx--unprefix-functions
                  "=SUM(_xlfn.ANCHORARRAY(C1))+_xlfn.SINGLE(A1:A3)")
                 "=SUM(C1#)+@A1:A3"))
  (should (equal (exc-xlsx--unprefix-functions
                  "=_xlfn.XLOOKUP(1,A:A,B:B)+SUM(_xlfn._xlws.FILTER(B:B,A:A=1))")
                 "=XLOOKUP(1,A:A,B:B)+SUM(FILTER(B:B,A:A=1))"))
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((rows '(("Key" "Value" "Found" "Sum")
                   ("a" "1" "=XLOOKUP(\"b\",A2:A3,B2:B3)" "=SUM(FILTER(B2:B3,A2:A3=\"a\"))")
                   ("b" "2" "" "")))
           (source (make-exc-default-data-source (copy-sequence (car rows)) rows "Keys")))
      (exc-xlsx-write-data-source source file)
      (should (string-match-p
               "_xlfn\\.XLOOKUP(.*_xlfn\\._xlws\\.FILTER("
               (shell-command-to-string
                (format "unzip -p %s xl/worksheets/sheet1.xml"
                        (shell-quote-argument file)))))
      (let ((back (exc-xlsx-create-data-source file)))
        (should (equal (exc-get-raw-value-at back 1 2) "=XLOOKUP(\"b\",A2:A3,B2:B3)"))
        (should (equal (exc-get-raw-value-at back 1 3) "=SUM(FILTER(B2:B3,A2:A3=\"a\"))"))
        (should (equal (exc-get-display-value-at back 1 2) "2"))
        (should (equal (exc-get-display-value-at back 1 3) "1"))))))

(ert-deftest exc-xlsx-test-widths-and-frozen-panes ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((rows '(("Item" "Qty" "Note") ("a" "1" "x") ("b" "2" "y")
                   ("c" "3" "z")))
           (source (make-exc-default-data-source
                    (copy-sequence (car rows)) rows "Stock")))
      (exc-set-column-width source 0 24)
      (exc-set-column-width source 2 7)
      (exc-set-frozen source 1 2)
      (exc-xlsx-write-data-source source file)
      (let ((back (exc-xlsx-create-data-source file)))
        (should (equal (gethash 0 (exc-get-column-widths back)) 24))
        (should (equal (gethash 2 (exc-get-column-widths back)) 7))
        (should-not (gethash 1 (exc-get-column-widths back)))
        (should (equal (exc-frozen back) '(1 . 2)))
        ;; Rows alone, then nothing: the workbook follows the grid.
        (exc-set-frozen back 2 0)
        (exc-xlsx-write-data-source back file)
        (setq back (exc-xlsx-create-data-source file))
        (should (equal (exc-frozen back) '(2 . 0)))
        (exc-set-frozen back 0 0)
        (exc-set-column-width back 0 nil)
        (exc-xlsx-write-data-source back file)
        (setq back (exc-xlsx-create-data-source file))
        (should (equal (exc-frozen back) '(0 . 0)))
        ;; A column the grid no longer sizes keeps what the file has.
        (should (equal (gethash 0 (exc-get-column-widths back)) 24))
        (should (equal (exc-get-raw-value-at back 3 2) "z"))))))

(ert-deftest exc-xlsx-test-hidden-lines-validations-and-names ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  '("" "" "" "")
                  '(("Item" "Qty" "Ok" "Sizes") ("a" "2" "Yes" "S")
                    ("b" "3" "No" "M") ("c" "4" "" "L"))))
           (sum (make-exc-default-data-source '("") '(("=SUM(Qty)"))))
           (workbook (exc-workbook-create (list (cons "Data" data)
                                                 (cons "Sum" sum)))))
      (exc-set-hidden data '(2) '(3))
      (exc-set-validations
       data '((1 2 3 2 list ("Yes" "No")) (1 0 3 0 list "=$D$2:$D$4")))
      (exc-workbook-define-name workbook "Qty" "=Data!$B$2:$B$4")
      (should (equal (exc-get-computed-value-at sum 0 0) 9))
      (exc-xlsx-write-workbook workbook file)
      (let* ((back (exc-xlsx-create-workbook file))
             (data (exc-workbook-sheet back "Data"))
             (sum (exc-workbook-sheet back "Sum")))
        (should (equal (exc-hidden data) '((2) 3)))
        (should (equal (exc-validations data)
                       '((1 2 3 2 list ("Yes" "No")) (1 0 3 0 list "=$D$2:$D$4"))))
        (should (equal (exc-workbook-names back) '(("Qty" . "=Data!$B$2:$B$4"))))
        (should (equal (exc-get-computed-value-at sum 0 0) 9))
        ;; Written back to the file it came from, the grid's names are the
        ;; file's: one deleted is gone.
        (exc-workbook-delete-name back "Qty")
        (exc-xlsx-write-workbook back file)
        (should-not (exc-workbook-names (exc-xlsx-create-workbook file)))))))

(ert-deftest exc-xlsx-test-dates-from-1904 ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  '("" "") '(("0" "=TEXT(A1,\"yyyy-mm-dd\")"))))
           (workbook (exc-workbook-create (list (cons "Data" data)) nil 1904)))
      (should (equal (exc-get-computed-value-at data 0 1) "1904-01-01"))
      (exc-xlsx-write-workbook workbook file)
      (should (= (exc-xlsx-date-system file) 1904))
      (let* ((back (exc-xlsx-create-workbook file))
             (data (exc-workbook-sheet back "Data")))
        (should (= (exc-workbook-date-system-of back) 1904))
        (should (equal (exc-get-computed-value-at data 0 1) "1904-01-01")))
      ;; A workbook of the usual system writes it.
      (exc-xlsx-write-workbook
       (exc-workbook-create (list (cons "Data" (make-exc-default-data-source
                                                 '("") '(("0"))))))
       file)
      (should (= (exc-xlsx-date-system file) 1900)))))

(ert-deftest exc-xlsx-test-keeps-sheet-titles ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source '("") '(("1"))))
           (sum (make-exc-default-data-source '("") '(("2"))))
           (workbook (exc-workbook-create (list (cons "Data" data)
                                                 (cons "Sum" sum)))))
      (puthash :title "Northwind · Data" data)
      (puthash :title "Sum" sum)
      (exc-xlsx-write-workbook workbook file)
      (let ((back (exc-xlsx-create-workbook file)))
        (should (equal (gethash :title (exc-workbook-sheet back "Data"))
                       "Northwind · Data"))
        ;; A title that is the sheet's name needs nothing kept.
        (should (equal (gethash :title (exc-workbook-sheet back "Sum")) "Sum"))
        ;; A sheet taken out of the workbook takes its title with it, and a
        ;; title given back as the sheet's name is no longer kept.
        (puthash :title "Data" (exc-workbook-sheet back "Data"))
        (exc-workbook-remove-sheet back (exc-workbook-sheet back "Sum"))
        (exc-xlsx-write-workbook back file)
        (should (equal (gethash :title (exc-xlsx-create-data-source
                                        file "Data"))
                       "Data"))
        (should-not (string-match-p
                     "exc title"
                     (shell-command-to-string
                      (format "unzip -p %s docProps/custom.xml 2>/dev/null"
                              (shell-quote-argument file)))))))))

(ert-deftest exc-xlsx-test-reads-a-title-kept-under-the-old-prefix ()
  "A workbook written before the rename keeps its grid title."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source '("") '(("1"))))
           (workbook (exc-workbook-create (list (cons "Data" data)))))
      (puthash :title "Northwind · Data" data)
      (exc-xlsx-write-workbook workbook file)
      ;; Rewrite the custom property the way the old version named it.
      (let ((dir (make-temp-file "exc-xlsx-old-" t)))
        (unwind-protect
            (progn
              (call-process "unzip" nil nil nil "-o" "-q" file
                            "docProps/custom.xml" "-d" dir)
              (let ((xml (expand-file-name "docProps/custom.xml" dir)))
                (with-temp-file xml
                  (insert-file-contents xml)
                  (goto-char (point-min))
                  (while (search-forward "exc title: " nil t)
                    (replace-match "grid-table title: " t t)))
                (let ((default-directory (file-name-as-directory dir)))
                  (call-process "zip" nil nil nil "-q" file
                                "docProps/custom.xml")))
              (should (string-match-p
                       "grid-table title: Data"
                       (shell-command-to-string
                        (format "unzip -p %s docProps/custom.xml"
                                (shell-quote-argument file))))))
          (delete-directory dir t)))
      ;; The old name reads back.
      (should (equal (gethash :title
                              (exc-workbook-sheet
                               (exc-xlsx-create-workbook file) "Data"))
                     "Northwind · Data"))
      ;; Saving again writes the new name, and only that one.
      (exc-xlsx-write-workbook (exc-xlsx-create-workbook file) file)
      (let ((xml (shell-command-to-string
                  (format "unzip -p %s docProps/custom.xml"
                          (shell-quote-argument file)))))
        (should (string-match-p "exc title: Data" xml))
        (should-not (string-match-p "grid-table title" xml))))))

(ert-deftest exc-xlsx-test-table-column-with-a-line-break ()
  "A column name with a line break is read whole, however written.
Excel writes it as _x000a_ in the table, and a formula may write it
as CR LF."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  '("" "")
                  '(("Item" "Two\nlines") ("a" "2") ("b" "3")
                    ("" "=SUM(T1[Two\r\nlines])"))))
           (workbook (exc-workbook-create (list (cons "Data" data)))))
      (setf (exc-workbook-tables workbook)
            (list (list "T1" "Data" 0 0 2 1 1 0 '("Item" "Two\nlines"))))
      (exc-xlsx-write-workbook workbook file)
      (let ((back (exc-xlsx-create-workbook file)))
        (should (equal (car (last (car (exc-workbook-tables back))))
                       '("Item" "Two\nlines")))
        (should (equal (exc-get-computed-value-at
                        (exc-workbook-sheet back "Data") 3 1)
                       5))))))

(ert-deftest exc-xlsx-test-bare-carriage-returns ()
  "A carriage return written as itself, not _x000D_, is read as a line feed.
XML reads it so, as Excel does a formula's text and what it gave."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (exc-xlsx-write-data-source
     (make-exc-default-data-source '("") '(("=\"a\r\nb\rc\"")))
     file "Data")
    (let ((back (exc-xlsx-create-data-source file "Data")))
      (should (equal (exc-get-raw-value-at back 0 0) "=\"a\nb\nc\""))
      (should (equal (exc-get-computed-value-at back 0 0) "a\nb\nc")))))

(ert-deftest exc-xlsx-test-tables-move-with-their-cells ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  '("" "" "")
                  '(("Item" "Qty" "") ("a" "2" "") ("b" "3" "")
                    ("" "" "=SUM(Table1[Qty])"))))
           (workbook (exc-workbook-create (list (cons "Data" data)))))
      (setf (exc-workbook-tables workbook)
            (list (list "Table1" "Data" 0 0 2 1 1 0 '("Item" "Qty"))))
      ;; A table exc holds is written, also to a new file.
      (exc-xlsx-write-workbook workbook file)
      (let* ((back (exc-xlsx-create-workbook file))
             (data (exc-workbook-sheet back "Data")))
        (should (equal (exc-workbook-tables back)
                       '(("Table1" "Data" 0 0 2 1 1 0 ("Item" "Qty")))))
        (funcall (gethash :add-row data) data -1)
        (funcall (gethash :add-column data) data 1)
        ;; A header edited names its column.
        (exc-set-raw-value-at data 1 0 "Name")
        (exc-xlsx-write-workbook back file)
        (let* ((again (exc-xlsx-create-workbook file)))
          (should (equal (exc-workbook-tables again)
                         '(("Table1" "Data" 1 0 3 2 1 0
                            ("Name" "Column1" "Qty")))))
          (should (equal (exc-get-computed-value-at
                          (exc-workbook-sheet again "Data") 4 3)
                         5))
          ;; A name kept escaped, as a header shows it, stays as written.
          (let ((sheet (make-exc-default-data-source
                        '("" "") '(("A\r\nB" "'12") ("1" "2")))))
            (should (equal (split-string
                            (exc-xlsx--table-record
                             sheet '("T1" "S" 0 0 1 1 1 0 ("A_x000a_B" "12")))
                            exc-xlsx--field)
                           '("T" "T1" "0" "0" "1" "1" "1" "0" "A_x000a_B" "12"))))
          ;; A table that went is gone from the file too.
          (setf (exc-workbook-tables again) nil)
          (exc-xlsx-write-workbook again file)
          (should-not (exc-workbook-tables
                       (exc-xlsx-create-workbook file))))))))

(ert-deftest exc-xlsx-test-names-of-constants-keep-their-text ()
  "A name for an array or for text with commas is read back whole."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((sheet (make-exc-default-data-source
                   '("" "" "") '(("=INDEX(Months,2)" "=Label" "=Global1"))))
           (workbook (exc-workbook-create (list (cons "Sheet1" sheet)))))
      (exc-workbook-define-name workbook "Months" "={\"Jan\",\"Feb\"}")
      (exc-workbook-define-name workbook "Label" "=\"a, b\"")
      (exc-workbook-define-name workbook "Global1" "=7")
      (exc-xlsx-write-workbook workbook file)
      (let ((sheet (exc-workbook-sheet (exc-xlsx-create-workbook file)
                                        "Sheet1")))
        (should (equal (list (exc-get-computed-value-at sheet 0 0)
                             (exc-get-computed-value-at sheet 0 1)
                             (exc-get-computed-value-at sheet 0 2))
                       '("Feb" "a, b" 7)))))))

(ert-deftest exc-xlsx-test-table-names-keep-their-text ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  '("" "") '(("A & B" "One\nTwo") ("1" "2"))))
           (workbook (exc-workbook-create (list (cons "Data" data))))
           (table '("Table1" "Data" 0 0 1 1 1 0 ("A & B" "One\nTwo"))))
      (setf (exc-workbook-tables workbook) (list table))
      (exc-xlsx-write-workbook workbook file)
      ;; Saved again from what was read, the names are not escaped twice
      ;; and the line break does not become a space.
      (exc-xlsx-write-workbook (exc-xlsx-create-workbook file)
                                      file)
      (should (equal (exc-workbook-tables
                      (exc-xlsx-create-workbook file))
                     (list table))))))

(ert-deftest exc-xlsx-test-notes-and-links ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  '("" "" "")
                  '(("Item" "Site" "") ("a" "home" "") ("b" "back" ""))))
           (workbook (exc-workbook-create (list (cons "Data" data)))))
      (exc-set-style-at data 1 0 '(:bold t :note "Checked\nby Ann"))
      (exc-set-style-at data 1 1 '(:link "https://example.org/a?b=1"))
      (exc-set-style-at data 2 1 '(:link "#Data!A1"))
      ;; A note on an empty cell
      (exc-set-style-at data 2 2 '(:note "to do"))
      (exc-xlsx-write-workbook workbook file)
      (let ((back (exc-workbook-sheet (exc-xlsx-create-workbook file)
                                       "Data")))
        (should (equal (exc-get-style-at back 1 0)
                       '(:bold t :note "Checked\nby Ann")))
        (should (equal (exc-get-style-at back 1 1)
                       '(:link "https://example.org/a?b=1")))
        (should (equal (exc-get-style-at back 2 1) '(:link "#Data!A1")))
        (should (equal (exc-get-style-at back 2 2) '(:note "to do")))
        ;; Written back without them, they are gone from the file.
        (exc-set-style-at back 1 0 '(:bold t))
        (exc-xlsx-write-workbook (exc-workbook-of back) file)
        (let ((again (exc-workbook-sheet (exc-xlsx-create-workbook file)
                                          "Data")))
          (should (equal (exc-get-style-at again 1 0) '(:bold t)))
          (should (equal (exc-get-style-at again 2 2) '(:note "to do"))))))))

(ert-deftest exc-xlsx-test-charts ()
  "Floating charts become workbook charts and come back from them."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  (make-list 5 "")
                  '(("Region" "Q1" "Q2" "Q3" "Q4") ("North" "1" "2" "3" "4")
                    ("South" "5" "6" "7" "8"))))
           (workbook (exc-workbook-create (list (cons "My Data" data))))
           (charts (list (list :anchor '(4 . 0) :size '(51 . 15) :type 'column
                               :range "A1:E3" :by 'rows :title "Sales")
                         ;; Within its cell a chart keeps its place.
                         (list :anchor '(4 . 7) :offset '(3.25 . 0)
                               :size '(42 . 12) :type 'line
                               :range "A1:E3" :by 'columns :title nil)
                         (list :anchor '(20 . 0) :size '(42 . 12) :type 'pie
                               :range "A1:E2" :by 'rows :title "North")))
           ;; The workbook says where the labels and names are, which
           ;; are where the grid found them.
           (written (mapcar (lambda (chart) (append chart '(:labels t :names t)))
                            charts)))
      (exc-set-charts data charts)
      (exc-xlsx-write-workbook workbook file)
      (let ((back (exc-workbook-sheet (exc-xlsx-create-workbook file)
                                       "My Data")))
        (should (equal (exc-charts back) written))
        ;; In the file it stands as far into its column, in EMU.
        (should (string-match-p
                 "<xdr:colOff>216694</xdr:colOff>"
                 (shell-command-to-string
                  (format "unzip -p %s 'xl/drawings/*.xml'"
                          (shell-quote-argument file)))))
        (should (equal (mapcar (lambda (chart) (exc-chart-data back chart))
                               written)
                       (mapcar (lambda (chart) (exc-chart-data data chart))
                               charts)))
        ;; The sheet reaches the cells the charts stand on, so a chart
        ;; beside the data is not pushed onto it.
        (should (>= (exc-get-column-count back) 8))
        (should (>= (exc-get-row-count back) 21))
        ;; Written back, the workbook holds them once, not twice.
        (exc-xlsx-write-workbook (exc-workbook-of back) file)
        (let ((again (exc-workbook-sheet (exc-xlsx-create-workbook file)
                                          "My Data")))
          (should (equal (exc-charts again) written))
          ;; Deleted in the grid, a chart is gone from the file.
          (exc-set-charts again (cdr written))
          (exc-xlsx-write-workbook (exc-workbook-of again) file)
          (should (equal (exc-charts
                          (exc-workbook-sheet (exc-xlsx-create-workbook file)
                                               "My Data"))
                         (cdr written))))))))

(ert-deftest exc-xlsx-test-chart-series-names-of-their-own ()
  "Series named by text, by cells elsewhere or not at all keep those names."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  '("" "" "")
                  '(("1" "2" "3") ("4" "5" "6") ("" "" "")
                    ("Top" "" "") ("Half" "" ""))))
           (workbook (exc-workbook-create (list (cons "Sheet1" data))))
           (chart (list :anchor '(6 . 0) :size '(42 . 12) :type 'column
                        :range "A1:C2" :by 'columns :title nil
                        :labels 'none :names 'none
                        :series-names '(nil (text "Apple") "$A$4:$A$5"))))
      (exc-set-charts data (list chart))
      (exc-xlsx-write-workbook workbook file)
      (let ((back (exc-workbook-sheet (exc-xlsx-create-workbook file)
                                       "Sheet1")))
        (should (equal (exc-charts back) (list chart)))
        ;; A name across two cells joins their text, as Excel shows it.
        (should (equal (mapcar #'car (plist-get (exc-chart-data back chart)
                                                :series))
                       '("Series 1" "Apple" "Top Half")))))))

(ert-deftest exc-xlsx-test-export-keeps-other-sheets ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (copy-file exc-xlsx-test--fixture file)
    (let ((source (exc-xlsx-create-data-source file "销售")))
      (exc-set-raw-value-at source 2 2 "30")
      (should (equal (exc-xlsx-write-data-source source file) "销售")))
    (should (equal (exc-xlsx-sheet-names file) '("销售" "Sheet2")))
    (let ((first (exc-xlsx-create-data-source file "销售"))
          (second (exc-xlsx-create-data-source file "Sheet2")))
      (should (equal (exc-xlsx-test--raw first 2 2) "30"))
      (should (equal (exc-get-computed-value-at first 2 4) 75))
      (should (equal (exc-model-get-merges (gethash :model first))
                     '((6 1 7 3))))
      (should (equal (exc-xlsx-test--raw second 0 0) "=销售!E6*2"))
      (should (equal (exc-xlsx-test--raw second 1 0) "line1\nline2")))
    ;; A grid written under another name becomes one more sheet.
    (let ((source (make-exc-default-data-source
                   (list "a" "b") '(("a" "b") ("1" "2")) "Extra: 1/2")))
      (should (equal (exc-xlsx-write-data-source source file)
                     "Extra  1 2")))
    (should (equal (exc-xlsx-sheet-names file)
                   '("销售" "Sheet2" "Extra  1 2")))))

(ert-deftest exc-xlsx-test-whole-workbook ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (copy-file exc-xlsx-test--fixture file)
    (let* ((workbook (exc-xlsx-create-workbook file))
           (sales (exc-workbook-sheet workbook "销售"))
           (second (exc-workbook-sheet workbook "Sheet2")))
      (should (equal (exc-workbook-sheet-names workbook) '("销售" "Sheet2")))
      ;; The formula that reads the other sheet has a value now.
      (should (equal (exc-get-raw-value-at second 0 0) "=销售!E6*2"))
      (should (equal (exc-get-computed-value-at second 0 0) 112.5))
      (exc-set-raw-value-at sales 2 2 "30")
      (should (equal (exc-get-computed-value-at second 0 0) 247.5))
      ;; A renamed sheet replaces the one it was, and a new one is added.
      (exc-workbook-rename-sheet workbook second "Summary")
      (exc-set-raw-value-at (exc-workbook-new-sheet workbook "My Notes")
                             0 0 "=Summary!A1+'My Notes'!B1")
      (should (= (exc-xlsx-write-workbook workbook file) 3)))
    (should (equal (exc-xlsx-sheet-names file)
                   '("销售" "Summary" "My Notes")))
    (let* ((workbook (exc-xlsx-create-workbook file))
           (summary (exc-workbook-sheet workbook "Summary"))
           (notes (exc-workbook-sheet workbook "My Notes")))
      (should (equal (exc-get-computed-value-at summary 0 0) 247.5))
      (should (equal (exc-get-raw-value-at notes 0 0)
                     "=Summary!A1+'My Notes'!B1"))
      (should (equal (exc-get-computed-value-at notes 0 0) 247.5))
      (should (equal (exc-model-get-merges
                      (gethash :model (exc-workbook-sheet workbook "销售")))
                     '((6 1 7 3)))))
    ;; Written into a workbook it did not come from, a grid leaves that
    ;; workbook's other sheets alone.
    (exc-xlsx-test--with-file other
      (copy-file exc-xlsx-test--fixture other)
      (exc-xlsx-write-workbook (exc-xlsx-create-workbook file)
                                      other)
      (should (equal (exc-xlsx-sheet-names other)
                     '("销售" "Sheet2" "Summary" "My Notes"))))))

(ert-deftest exc-xlsx-test-export-command ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (with-temp-buffer
      (setq-local exc--data-source
                  (make-exc-default-data-source
                   (list "a" "b") '(("a" "b") ("1" "=A2*2")) nil))
      (should-error (exc-xlsx-module-read file "Sheet1"))
      ;; An untitled grid goes out as Sheet1, and the extension is added.
      (exc-xlsx-export (file-name-sans-extension file))
      (should (equal (exc-xlsx-sheet-names file) '("Sheet1")))
      (should (equal (gethash :xlsx-sheet exc--data-source) "Sheet1"))
      (exc-set-raw-value-at exc--data-source 1 0 "5")
      (exc-xlsx-export file)
      (should (equal (exc-xlsx-sheet-names file) '("Sheet1"))))
    (let ((back (exc-xlsx-create-data-source file)))
      (should (equal (exc-get-computed-value-at back 1 1) 10))
      (should-error (exc-xlsx-create-data-source file "Missing")))))

(ert-deftest exc-xlsx-test-failed-export-leaves-the-file ()
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (with-temp-file file (insert "not a workbook"))
    (let ((source (make-exc-default-data-source
                   (list "a") '(("a") ("1")) "Sheet")))
      (should-error (exc-xlsx-write-data-source source file)))
    (should (equal (with-temp-buffer (insert-file-contents file)
                                     (buffer-string))
                   "not a workbook"))
    (should-not (directory-files (file-name-directory file) nil
                                 (concat (regexp-quote
                                          (file-name-nondirectory file))
                                         "\\.tmp")))))

(ert-deftest exc-xlsx-test-charts-other-shapes ()
  "Charts of one-value series, unlabelled doughnuts and short data return."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (let* ((data (make-exc-default-data-source
                  (make-list 3 "")
                  '(("" "Linux" "Mac") ("Share" "60" "30") ("" "" "")
                    ("" "" ""))))
           (workbook (exc-workbook-create (list (cons "Data" data))))
           (charts (list
                    ;; Each series holds one value, under its name.
                    (list :anchor '(6 . 0) :size '(42 . 10) :type 'column
                          :range "A1:C2" :by 'columns :title nil
                          :labels t :names t)
                    ;; Two rings and nothing to label them.
                    (list :anchor '(6 . 6) :size '(42 . 10) :type 'doughnut
                          :range "B2:C3" :by 'columns :title nil
                          :labels 'none :names 'none)
                    ;; Its range runs into empty rows the file does not hold.
                    (list :anchor '(18 . 0) :size '(42 . 10) :type 'line
                          :range "B2:B4" :by 'columns :title nil
                          :labels 'none :names 'none))))
      (exc-set-charts data charts)
      (exc-xlsx-write-workbook workbook file)
      (let ((back (exc-workbook-sheet (exc-xlsx-create-workbook file)
                                       "Data")))
        (should (equal (exc-charts back) charts))))))

(ert-deftest exc-xlsx-test-other-writers ()
  "A workbook in shapes other writers produce reads and writes back."
  (skip-unless exc-xlsx-test--available)
  (let ((source (car (exc-workbook-sheets
                      (exc-xlsx-create-workbook
                       exc-xlsx-test--compat)))))
    (should (equal (exc-xlsx-test--raw source 1 0) "4"))
    (should (equal (exc-xlsx-test--raw source 1 1) "30"))
    (should (equal (exc-xlsx-test--raw source 1 2) "''quoted"))
    (should (equal (exc-hidden source) '((39))))
    (should (equal (gethash 25 (exc-get-column-widths source)) 20))
    (should (= (length (exc-charts source)) 1))
    (dolist (col '(0 1))
      (should (equal (plist-get (exc-get-style-at source 2 col) :link)
                     "#Data!A1")))
    (exc-xlsx-test--with-file file
      (exc-xlsx-write-workbook (exc-workbook-of source) file)
      (let ((back (car (exc-workbook-sheets
                        (exc-xlsx-create-workbook file)))))
        (dotimes (row 3)
          (dotimes (col 3)
            (should (equal (exc-xlsx-test--raw back row col)
                           (exc-xlsx-test--raw source row col)))))
        (should (equal (exc-hidden back) '((39))))
        (should (equal (gethash 25 (exc-get-column-widths back)) 20))))))

(ert-deftest exc-xlsx-test-write-keeps-external-links ()
  "A write gives a workbook back the external links it read.
umya-spreadsheet has no model for them, so the parts, the references
and the relationships of xl/externalLinks are copied from the file the
write began with, numbered among those the write made itself."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (copy-file exc-xlsx-test--links file)
    (should (equal (split-string (exc-xlsx-module-links file) "\u001e")
                   '("externalLinks/externalLink1.xml"
                     "externalLinks/externalLink2.xml")))
    (exc-xlsx-write-workbook (exc-xlsx-create-workbook file) file)
    ;; The link is still there, and so are the cells and name that name it.
    (should (equal (split-string (exc-xlsx-module-links file) "\u001e")
                   '("externalLinks/externalLink1.xml"
                     "externalLinks/externalLink2.xml")))
    (let* ((workbook (exc-xlsx-create-workbook file))
           (sheet (exc-workbook-sheet workbook "Sheet1")))
      (should (equal (exc-get-raw-value-at sheet 0 0) "=[1]Sheet1!$A$1"))
      (should (equal (exc-get-computed-value-at sheet 0 0) 2))
      (should (equal (cdr (assoc-string "othername" (exc-workbook-names workbook) t))
                     "=[1]!MonthNames"))
      (should (equal (cdr (assoc-string "named_cell" (exc-workbook-names workbook) t))
                     "=[1]Sheet1!$B$3")))
    ;; Saving a sheet of it, and writing only its names, keep the link too.
    (let ((source (exc-xlsx-create-data-source file "Sheet1")))
      (exc-set-raw-value-at source 1 1 "8")
      (exc-xlsx-write-data-source source file "Sheet1"))
    (should (equal (split-string (exc-xlsx-module-links file) "\u001e")
                   '("externalLinks/externalLink1.xml"
                     "externalLinks/externalLink2.xml")))
    (exc-xlsx-module-set-names file "")
    (should (equal (split-string (exc-xlsx-module-links file) "\u001e")
                   '("externalLinks/externalLink1.xml"
                     "externalLinks/externalLink2.xml")))
    (when (executable-find "unzip")
      (let ((workbook (exc-xlsx-test--unzip file "xl/workbook.xml"))
            (rels (exc-xlsx-test--unzip file "xl/_rels/workbook.xml.rels"))
            (types (exc-xlsx-test--unzip file "[Content_Types].xml"))
            (part (exc-xlsx-test--unzip file "xl/externalLinks/externalLink1.xml")))
        ;; The references come after the sheets, as the schema orders them,
        ;; one for each link.
        (should (string-match-p
                 (concat "</sheets><externalReferences>"
                         "<externalReference r:id=\"rId[0-9]+\"/>"
                         "<externalReference r:id=\"rId[0-9]+\"/>"
                         "</externalReferences>")
                 workbook))
        ;; Its r:id names an external link of the relationships.
        (should (string-match-p "Id=\"rId[0-9]+\" Type=\"[^\"]*/externalLink\" Target=\"externalLinks/externalLink1.xml\""
                                rels))
        (should (string-match-p "/xl/externalLinks/externalLink1.xml" types))
        (should (string-match-p "<externalBook" part))))))

(ert-deftest exc-xlsx-test-reads-what-another-workbook-cached ()
  "A formula that reads another workbook reads the values the file cached.
That workbook is not open: the values are the ones the linking file holds,
and a cell it holds no value for is blank."
  (skip-unless exc-xlsx-test--available)
  (let* ((workbook (exc-xlsx-create-workbook exc-xlsx-test--links))
         (sheet (exc-workbook-sheet workbook "Sheet1"))
         (value (lambda (row) (exc-get-computed-value-at sheet (1- row) 0))))
    ;; A cell, a range and a whole column of it.
    (should (equal (funcall value 1) 2))
    (should (equal (funcall value 2) 35))
    (should (equal (funcall value 3) 9))
    ;; VLOOKUP over what it cached, a range with text in it, and a name
    ;; of this workbook that points into the other one.
    (should (equal (funcall value 7) 20))
    (should (equal (funcall value 12) 30))
    (should (equal (funcall value 13) 69))
    ;; A number, text, TRUE, an error and a blank, as it cached them.
    (should (equal (funcall value 8) t))
    (should (equal (funcall value 9) "x"))
    (should (equal (funcall value 10) "#N/A"))
    (should (equal (funcall value 11) 0))
    ;; A sheet or a link it has not is #REF!, one whose refresh failed and
    ;; cached nothing too, and INDIRECT finds none of them: Excel does not
    ;; read a workbook that is not open.
    (should (equal (funcall value 4) "#REF!"))
    (should (equal (funcall value 5) "#REF!"))
    (should (equal (funcall value 6) "#REF!"))
    (should (equal (funcall value 14) "#REF!"))
    ;; A sheet it cached no cell for but did read holds nothing to find.
    (should (equal (funcall value 15) 0))
    ;; The names it keeps, of a cell, of a range, and of one of its sheets.
    (should (equal (funcall value 16) 2))
    (should (equal (funcall value 17) 35))
    (should (equal (funcall value 18) 30))
    ;; A name it defines as #REF!, and one it keeps no definition of, read
    ;; as those do; a name it has not is #NAME?; a table of it is #REF!;
    ;; and a DDE link, which no formula reads, is #REF!.
    (should (equal (funcall value 19) "#REF!"))
    (should (equal (funcall value 20) "#NAME?"))
    (should (equal (funcall value 21) "#REF!"))
    (should (equal (funcall value 22) "#REF!"))
    ;; INDIRECT reads no name of another workbook either, and [0] is the
    ;; name of this one.
    (should (equal (funcall value 23) "#REF!"))
    (should (equal (funcall value 24) 30))
    (should (equal (funcall value 25) "#NAME?"))
    ;; What it reads is no sheet of this workbook.
    (should (equal (exc-workbook-sheet-names workbook) '("Sheet1")))))

(ert-deftest exc-xlsx-test-external-raw-keeps-false ()
  "A cached FALSE stays FALSE, and a cached text stays text."
  (should (equal (exc-xlsx--external-raw 'b :false) "FALSE"))
  (should (equal (exc-xlsx--external-raw 'b :true) "TRUE"))
  (should (equal (exc-xlsx--external-raw 's "=1") "'=1")))

(ert-deftest exc-xlsx-test-keeps-a-name-of-two-references ()
  "A name working two references out is read and written as it stands.
It is kept whole rather than parsed into one address, which would read
Sheet1!$A$1+Sheet1!$A$2 as pointing into a sheet named
\"Sheet1!$A$1+Sheet1\"; the test/fixtures/links.xlsx name pair is
Sheet1!$A$1+Sheet1!$A$2 and A26 is 37 by it."
  (skip-unless exc-xlsx-test--available)
  (exc-xlsx-test--with-file file
    (copy-file exc-xlsx-test--links file)
    (let ((workbook (exc-xlsx-create-workbook file)))
      (should (equal (cdr (assoc-string "pair" (exc-workbook-names workbook) t))
                     "=Sheet1!$A$1+Sheet1!$A$2"))
      (should (equal (exc-get-computed-value-at
                      (exc-workbook-sheet workbook "Sheet1") 25 0)
                     37)))
    ;; Written back, the name is still the same two references.
    (exc-xlsx-write-workbook (exc-xlsx-create-workbook file) file)
    (should (equal (cdr (assoc-string "pair" (exc-workbook-names
                                              (exc-xlsx-create-workbook file))
                                      t))
                   "=Sheet1!$A$1+Sheet1!$A$2"))))

(provide 'exc-xlsx-test)

;;; exc-xlsx-test.el ends here
