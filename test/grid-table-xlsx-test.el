;;; grid-table-xlsx-test.el --- xlsx plugin tests -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ert)
(setq load-prefer-newer t)
(require 'grid-table)
(require 'grid-table-xlsx)

(defconst grid-table-xlsx-test--fixture
  (expand-file-name "fixtures/styled.xlsx"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst grid-table-xlsx-test--available
  (ignore-errors (grid-table-xlsx--ensure-module) t))

(defmacro grid-table-xlsx-test--with-file (var &rest body)
  "Run BODY with VAR bound to the name of an xlsx file that does not exist."
  (declare (indent 1))
  `(let ((,var (concat (make-temp-name
                        (expand-file-name "grid-table-xlsx-"
                                          temporary-file-directory))
                       ".xlsx")))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defun grid-table-xlsx-test--raw (source row col)
  "Return the raw value at ROW and COL of SOURCE."
  (grid-get-raw-value-at source row col))

(ert-deftest grid-table-xlsx-test-cell-references ()
  (should (equal (grid-table-xlsx--column-name 0) "A"))
  (should (equal (grid-table-xlsx--column-name 25) "Z"))
  (should (equal (grid-table-xlsx--column-name 26) "AA"))
  (should (equal (grid-table-xlsx--column-name 701) "ZZ"))
  (should (equal (grid-table-xlsx--column-name 702) "AAA"))
  (should (equal (grid-table-xlsx--parse-cell "AA10") '(9 . 26)))
  (should (equal (grid-table-xlsx--parse-cell "$B$7") '(6 . 1)))
  (should (equal (grid-table-xlsx--parse-range "B7:D8") '(6 1 7 3)))
  (should (equal (grid-table-xlsx--parse-range "C3") '(2 2 2 2)))
  (should-not (grid-table-xlsx--parse-range "Sheet1"))
  (should (equal (grid-table-xlsx--range-name 6 1 7 3) "B7:D8")))

(ert-deftest grid-table-xlsx-test-number-formats ()
  ;; A date stays the number it is and takes a date format.
  (should (equal (grid-table-xlsx--raw '(0 0 "46307" :format "m/d/yyyy"))
                 "46307"))
  (should (equal (grid-table-xlsx--style '(:format "m/d/yyyy"))
                 '(:format (date "m/d/yyyy"))))
  (should (equal (grid-table-xlsx--style '(:bold t :format "0.0%"))
                 '(:bold t :format (percent 1))))
  (should-not (grid-table-xlsx--style '(:format "General")))
  ;; A format without a counterpart is kept for the way back.
  (should (equal (grid-table-xlsx--style '(:format "#,##0 \"days\""))
                 '(:format (code "#,##0 \"days\""))))
  (should (equal (nth 5 (grid-table-xlsx--style-fields
                         '(:format (code "#,##0 \"days\""))))
                 "#,##0 \"days\""))
  (should (equal (nth 5 (grid-table-xlsx--style-fields
                         '(:format (date "yyyy-mm-dd"))))
                 "yyyy-mm-dd"))
  ;; Text that reads as a number is marked as text.
  (should (equal (grid-table-xlsx--raw '(0 0 "007" :text "1")) "'007")))

(ert-deftest grid-table-xlsx-test-cell-records ()
  (let ((fields (lambda (record)
                  (split-string record grid-table-xlsx--field))))
    (should (equal (seq-take (funcall fields (grid-table-xlsx--cell-record
                                              2 4 "=C3*D3" nil))
                             5)
                   '("C" "2" "4" "f" "C3*D3")))
    (should (equal (nth 3 (funcall fields (grid-table-xlsx--cell-record
                                           0 0 "-1.5e3" nil)))
                   "n"))
    (should (equal (nth 3 (funcall fields (grid-table-xlsx--cell-record
                                           0 0 "1.2.3" nil)))
                   "s"))
    ;; Only grid-table evaluates a chart formula; it leaves as text.
    (should (equal (seq-subseq (funcall fields (grid-table-xlsx--cell-record
                                                0 0 "=barplot(A1:A3)" nil))
                               3 5)
                   '("s" "=barplot(A1:A3)")))
    ;; Text marked as text leaves as text, without its mark.
    (should (equal (seq-subseq (funcall fields (grid-table-xlsx--cell-record
                                                0 0 "'007" nil))
                               3 5)
                   '("s" "007")))
    (should (equal (nth 3 (funcall fields (grid-table-xlsx--cell-record
                                           0 0 "12" '(:format (text)))))
                   "s"))
    (should-not (grid-table-xlsx--cell-record 0 0 "" nil))
    (should (equal (seq-drop (funcall fields
                                      (grid-table-xlsx--cell-record
                                       0 0 "" '(:bold t :background "#ffc7ce"
                                                :align center)))
                             5)
                   '("1" "" "" "FFC7CE" "center" "")))))

(ert-deftest grid-table-xlsx-test-reads-a-workbook ()
  (skip-unless grid-table-xlsx-test--available)
  (should (equal (grid-table-xlsx-sheet-names grid-table-xlsx-test--fixture)
                 '("销售" "Sheet2")))
  (let* ((source (grid-table-xlsx-create-data-source
                  grid-table-xlsx-test--fixture))
         (model (gethash :model source)))
    (should (equal (gethash :xlsx-sheet source) "销售"))
    (should (equal (list (grid-get-row-count source)
                         (grid-get-column-count source))
                   '(8 6)))
    (should (equal (grid-table-xlsx-test--raw source 1 1) "产品"))
    (should (equal (grid-table-xlsx-test--raw source 3 1) "He said \"hi\""))
    (should (equal (grid-table-xlsx-test--raw source 4 1) "back\\slash"))
    (should (equal (grid-table-xlsx-test--raw source 2 4) "=C3*D3"))
    (should (equal (grid-get-computed-value-at source 2 4) 7.5))
    (should (equal (grid-get-computed-value-at source 5 4) 56.25))
    (should (equal (grid-table-xlsx-test--raw source 2 5) "TRUE"))
    ;; A date and a percentage come in as the numbers they hold, shown
    ;; through their formats.
    (should (equal (grid-table-xlsx-test--raw source 3 5) "46307"))
    (should (equal (grid-get-display-value-at source 3 5) "2026-10-12"))
    (should (equal (grid-table-xlsx-test--raw source 4 5) "0.125"))
    (should (equal (grid-get-display-value-at source 4 5) "12.5%"))
    (should (equal (grid-model-get-merges model) '((6 1 7 3))))
    (should (equal (grid-model-get-style model 1 1)
                   '(:bold t :color "#FFFFFF" :background "#4472C4"
                           :align center)))
    (should (equal (grid-model-get-style model 2 1)
                   '(:italic t :color "#00B050")))
    (should (equal (grid-model-get-style model 3 3)
                   '(:align right :format (number 2 nil))))
    (should (equal (grid-model-get-conditionals model)
                   '((2 2 4 2 ">" "5" (:bold t :background "#FFC7CE")))))
    (should (equal (gethash :xlsx-skipped-conditionals source) 0)))
  (let ((source (grid-table-xlsx-create-data-source
                 grid-table-xlsx-test--fixture "Sheet2")))
    (should (equal (grid-table-xlsx-test--raw source 0 0) "=销售!E6*2"))
    (should (equal (grid-table-xlsx-test--raw source 1 0) "line1\nline2"))))

(ert-deftest grid-table-xlsx-test-formats-round-trip ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (let ((source (make-default-data-source
                   '("" "")
                   '(("label" "value") ("date" "") ("pct" "") ("money" "")
                     ("text" "") ("later" "") ("days" "5")))))
      (grid-table-enter-value source 1 1 "2026-10-13")
      (grid-table-enter-value source 2 1 "12.5%")
      (grid-table-enter-value source 3 1 "¥1,234.50")
      (grid-table-enter-value source 4 1 "'007")
      (grid-set-raw-value-at source 5 1 "=DATE(2026,10,13)+7")
      (grid-set-style-at source 6 1 '(:format (code "#,##0 \"days\"")))
      (grid-table-xlsx-write-data-source source file "Formats")
      (let ((back (grid-table-xlsx-create-data-source file "Formats")))
        (should (equal (grid-get-raw-value-at back 1 1) "46308"))
        (should (equal (grid-get-display-value-at back 1 1) "2026-10-13"))
        (should (equal (grid-get-display-value-at back 2 1) "12.5%"))
        (should (equal (grid-get-display-value-at back 3 1) "¥1,234.50"))
        ;; Text that reads as a number is still text.
        (should (equal (grid-get-raw-value-at back 4 1) "'007"))
        ;; A date formula is given its format on the way out.
        (should (equal (grid-get-display-value-at back 5 1) "2026-10-20"))
        (should (equal (plist-get (grid-get-style-at back 5 1) :format)
                       '(date "yyyy-mm-dd")))
        ;; A format the grid does not know comes back untouched.
        (should (equal (plist-get (grid-get-style-at back 6 1) :format)
                       '(code "#,##0 \"days\"")))
        (should (grid-table-header-row-p back)))
      ;; A title merged down from the first row makes the sheet one
      ;; without a header row, so the merge is kept.
      (grid-table-set-header-row source nil)
      (grid-set-raw-value-at source 1 0 "")
      (grid-merge-cells source 0 0 1 0)
      (grid-table-xlsx-write-data-source source file "Formats")
      (let ((back (grid-table-xlsx-create-data-source file "Formats")))
        (should (equal (grid-get-merges back) '((0 0 1 0))))
        (should-not (grid-table-header-row-p back))))))

(ert-deftest grid-table-xlsx-test-round-trip ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (let* ((rows '(("Item" "Qty" "Price" "Total" "Note")
                   ("苹果" "3" "2.5" "=B2*C2" "say \"hi\"")
                   ("pear" "10" "4" "=B3*C3" "two\nlines")
                   ("" "" "" "=SUM(D2:D3)" "=BARPLOT(B2:B3)")
                   ("merged" "" "" "FALSE" "")
                   ("" "" "" "" "")))
           (source (make-default-data-source
                    (copy-sequence (car rows)) rows "Stock"))
           (model (gethash :model source))
           (rules '((1 1 2 1 ">" "5" (:bold t :background "#FFC7CE"))
                    (1 0 2 0 "contains" "pe" (:italic t :color "#0000FF"))
                    (1 4 2 4 "=" "x y" (:background "#00FF00"))
                    (1 2 3 2 "empty" "" (:background "#CCCCCC"))
                    (1 3 3 3 "error" "" (:color "#FF0000")))))
      (puthash :model (grid-model-merge-cells model 4 0 5 2) source)
      (setq model (gethash :model source))
      (grid-model-set-style model 0 0 '(:bold t :align center
                                              :background "#4472C4"))
      (grid-model-set-style model 1 1 '(:italic t :color "#00B050"))
      ;; A style on a cell that holds nothing still travels.
      (grid-model-set-style model 3 0 '(:background "#FFFF00"))
      (grid-model-set-conditionals model rules)
      (should (equal (grid-table-xlsx-write-data-source source file) "Stock"))
      (should (equal (grid-table-xlsx-sheet-names file) '("Stock")))
      (let* ((back (grid-table-xlsx-create-data-source file))
             (back-model (gethash :model back)))
        (should (equal (list (grid-get-row-count back)
                             (grid-get-column-count back))
                       '(6 5)))
        (dotimes (row 6)
          (dotimes (col 5)
            (should (equal (cons (cons row col)
                                 (grid-table-xlsx-test--raw back row col))
                           (cons (cons row col) (nth col (nth row rows)))))))
        (should (equal (grid-get-computed-value-at back 3 3) 47.5))
        (should (equal (grid-model-get-merges back-model) '((4 0 5 2))))
        (should (equal (grid-model-get-style back-model 0 0)
                       '(:bold t :background "#4472C4" :align center)))
        (should (equal (grid-model-get-style back-model 1 1)
                       '(:italic t :color "#00B050")))
        (should (equal (grid-model-get-style back-model 3 0)
                       '(:background "#FFFF00")))
        (should-not (grid-model-get-style back-model 2 2))
        (should (equal (grid-model-get-conditionals back-model) rules))))))

(ert-deftest grid-table-xlsx-test-newer-functions-carry-their-prefix ()
  "XLOOKUP, FILTER and their kind are written as a workbook names them.
Excel and LibreOffice read them only with the _xlfn. prefix, and the
grid reads them back without it."
  (should (equal (grid-table-xlsx--prefix-functions
                  "COUNTA(UNIQUE(SORT(A1:A3)))+LEN(\"FILTER(\")")
                 "COUNTA(_xlfn.UNIQUE(_xlfn._xlws.SORT(A1:A3)))+LEN(\"FILTER(\")"))
  (should (equal (grid-table-xlsx--prefix-functions "MYXLOOKUP(1)") "MYXLOOKUP(1)"))
  (should (equal (grid-table-xlsx--unprefix-functions
                  "=_xlfn.XLOOKUP(1,A:A,B:B)+SUM(_xlfn._xlws.FILTER(B:B,A:A=1))")
                 "=XLOOKUP(1,A:A,B:B)+SUM(FILTER(B:B,A:A=1))"))
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (let* ((rows '(("Key" "Value" "Found" "Sum")
                   ("a" "1" "=XLOOKUP(\"b\",A2:A3,B2:B3)" "=SUM(FILTER(B2:B3,A2:A3=\"a\"))")
                   ("b" "2" "" "")))
           (source (make-default-data-source (copy-sequence (car rows)) rows "Keys")))
      (grid-table-xlsx-write-data-source source file)
      (should (string-match-p
               "_xlfn\\.XLOOKUP(.*_xlfn\\._xlws\\.FILTER("
               (shell-command-to-string
                (format "unzip -p %s xl/worksheets/sheet1.xml"
                        (shell-quote-argument file)))))
      (let ((back (grid-table-xlsx-create-data-source file)))
        (should (equal (grid-get-raw-value-at back 1 2) "=XLOOKUP(\"b\",A2:A3,B2:B3)"))
        (should (equal (grid-get-raw-value-at back 1 3) "=SUM(FILTER(B2:B3,A2:A3=\"a\"))"))
        (should (equal (grid-get-display-value-at back 1 2) "2"))
        (should (equal (grid-get-display-value-at back 1 3) "1"))))))

(ert-deftest grid-table-xlsx-test-widths-and-frozen-panes ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (let* ((rows '(("Item" "Qty" "Note") ("a" "1" "x") ("b" "2" "y")
                   ("c" "3" "z")))
           (source (make-default-data-source
                    (copy-sequence (car rows)) rows "Stock")))
      (grid-set-column-width source 0 24)
      (grid-set-column-width source 2 7)
      (grid-table-set-frozen source 1 2)
      (grid-table-xlsx-write-data-source source file)
      (let ((back (grid-table-xlsx-create-data-source file)))
        (should (equal (gethash 0 (grid-get-column-widths back)) 24))
        (should (equal (gethash 2 (grid-get-column-widths back)) 7))
        (should-not (gethash 1 (grid-get-column-widths back)))
        (should (equal (grid-table-frozen back) '(1 . 2)))
        ;; Rows alone, then nothing: the workbook follows the grid.
        (grid-table-set-frozen back 2 0)
        (grid-table-xlsx-write-data-source back file)
        (setq back (grid-table-xlsx-create-data-source file))
        (should (equal (grid-table-frozen back) '(2 . 0)))
        (grid-table-set-frozen back 0 0)
        (grid-set-column-width back 0 nil)
        (grid-table-xlsx-write-data-source back file)
        (setq back (grid-table-xlsx-create-data-source file))
        (should (equal (grid-table-frozen back) '(0 . 0)))
        ;; A column the grid no longer sizes keeps what the file has.
        (should (equal (gethash 0 (grid-get-column-widths back)) 24))
        (should (equal (grid-get-raw-value-at back 3 2) "z"))))))

(ert-deftest grid-table-xlsx-test-hidden-lines-validations-and-names ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (let* ((data (make-default-data-source
                  '("" "" "" "")
                  '(("Item" "Qty" "Ok" "Sizes") ("a" "2" "Yes" "S")
                    ("b" "3" "No" "M") ("c" "4" "" "L"))))
           (sum (make-default-data-source '("") '(("=SUM(Qty)"))))
           (workbook (grid-workbook-create (list (cons "Data" data)
                                                 (cons "Sum" sum)))))
      (grid-table-set-hidden data '(2) '(3))
      (grid-table-set-validations
       data '((1 2 3 2 list ("Yes" "No")) (1 0 3 0 list "=$D$2:$D$4")))
      (grid-workbook-define-name workbook "Qty" "=Data!$B$2:$B$4")
      (should (equal (grid-get-computed-value-at sum 0 0) 9))
      (grid-table-xlsx-write-workbook workbook file)
      (let* ((back (grid-table-xlsx-create-workbook file))
             (data (grid-workbook-sheet back "Data"))
             (sum (grid-workbook-sheet back "Sum")))
        (should (equal (grid-table-hidden data) '((2) 3)))
        (should (equal (grid-table-validations data)
                       '((1 2 3 2 list ("Yes" "No")) (1 0 3 0 list "=$D$2:$D$4"))))
        (should (equal (grid-workbook-names back) '(("Qty" . "=Data!$B$2:$B$4"))))
        (should (equal (grid-get-computed-value-at sum 0 0) 9))
        ;; Written back to the file it came from, the grid's names are the
        ;; file's: one deleted is gone.
        (grid-workbook-delete-name back "Qty")
        (grid-table-xlsx-write-workbook back file)
        (should-not (grid-workbook-names (grid-table-xlsx-create-workbook file)))))))

(ert-deftest grid-table-xlsx-test-notes-and-links ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (let* ((data (make-default-data-source
                  '("" "" "")
                  '(("Item" "Site" "") ("a" "home" "") ("b" "back" ""))))
           (workbook (grid-workbook-create (list (cons "Data" data)))))
      (grid-set-style-at data 1 0 '(:bold t :note "Checked\nby Ann"))
      (grid-set-style-at data 1 1 '(:link "https://example.org/a?b=1"))
      (grid-set-style-at data 2 1 '(:link "#Data!A1"))
      ;; A note on an empty cell
      (grid-set-style-at data 2 2 '(:note "to do"))
      (grid-table-xlsx-write-workbook workbook file)
      (let ((back (grid-workbook-sheet (grid-table-xlsx-create-workbook file)
                                       "Data")))
        (should (equal (grid-get-style-at back 1 0)
                       '(:bold t :note "Checked\nby Ann")))
        (should (equal (grid-get-style-at back 1 1)
                       '(:link "https://example.org/a?b=1")))
        (should (equal (grid-get-style-at back 2 1) '(:link "#Data!A1")))
        (should (equal (grid-get-style-at back 2 2) '(:note "to do")))
        ;; Written back without them, they are gone from the file.
        (grid-set-style-at back 1 0 '(:bold t))
        (grid-table-xlsx-write-workbook (grid-workbook-of back) file)
        (let ((again (grid-workbook-sheet (grid-table-xlsx-create-workbook file)
                                          "Data")))
          (should (equal (grid-get-style-at again 1 0) '(:bold t)))
          (should (equal (grid-get-style-at again 2 2) '(:note "to do"))))))))

(ert-deftest grid-table-xlsx-test-charts ()
  "Floating charts become workbook charts and come back from them."
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (let* ((data (make-default-data-source
                  (make-list 5 "")
                  '(("Region" "Q1" "Q2" "Q3" "Q4") ("North" "1" "2" "3" "4")
                    ("South" "5" "6" "7" "8"))))
           (workbook (grid-workbook-create (list (cons "My Data" data))))
           (charts (list (list :anchor '(4 . 0) :size '(51 . 15) :type 'column
                               :range "A1:E3" :by 'rows :title "Sales")
                         (list :anchor '(4 . 7) :size '(42 . 12) :type 'line
                               :range "A1:E3" :by 'columns :title nil)
                         (list :anchor '(20 . 0) :size '(42 . 12) :type 'pie
                               :range "A1:E2" :by 'rows :title "North"))))
      (grid-table-set-charts data charts)
      (grid-table-xlsx-write-workbook workbook file)
      (let ((back (grid-workbook-sheet (grid-table-xlsx-create-workbook file)
                                       "My Data")))
        (should (equal (grid-table-charts back) charts))
        ;; The sheet reaches the cells the charts stand on, so a chart
        ;; beside the data is not pushed onto it.
        (should (>= (grid-get-column-count back) 8))
        (should (>= (grid-get-row-count back) 21))
        ;; Written back, the workbook holds them once, not twice.
        (grid-table-xlsx-write-workbook (grid-workbook-of back) file)
        (let ((again (grid-workbook-sheet (grid-table-xlsx-create-workbook file)
                                          "My Data")))
          (should (equal (grid-table-charts again) charts))
          ;; Deleted in the grid, a chart is gone from the file.
          (grid-table-set-charts again (cdr charts))
          (grid-table-xlsx-write-workbook (grid-workbook-of again) file)
          (should (equal (grid-table-charts
                          (grid-workbook-sheet (grid-table-xlsx-create-workbook file)
                                               "My Data"))
                         (cdr charts))))))))

(ert-deftest grid-table-xlsx-test-export-keeps-other-sheets ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (copy-file grid-table-xlsx-test--fixture file)
    (let ((source (grid-table-xlsx-create-data-source file "销售")))
      (grid-set-raw-value-at source 2 2 "30")
      (should (equal (grid-table-xlsx-write-data-source source file) "销售")))
    (should (equal (grid-table-xlsx-sheet-names file) '("销售" "Sheet2")))
    (let ((first (grid-table-xlsx-create-data-source file "销售"))
          (second (grid-table-xlsx-create-data-source file "Sheet2")))
      (should (equal (grid-table-xlsx-test--raw first 2 2) "30"))
      (should (equal (grid-get-computed-value-at first 2 4) 75))
      (should (equal (grid-model-get-merges (gethash :model first))
                     '((6 1 7 3))))
      (should (equal (grid-table-xlsx-test--raw second 0 0) "=销售!E6*2"))
      (should (equal (grid-table-xlsx-test--raw second 1 0) "line1\nline2")))
    ;; A grid written under another name becomes one more sheet.
    (let ((source (make-default-data-source
                   (list "a" "b") '(("a" "b") ("1" "2")) "Extra: 1/2")))
      (should (equal (grid-table-xlsx-write-data-source source file)
                     "Extra  1 2")))
    (should (equal (grid-table-xlsx-sheet-names file)
                   '("销售" "Sheet2" "Extra  1 2")))))

(ert-deftest grid-table-xlsx-test-whole-workbook ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (copy-file grid-table-xlsx-test--fixture file)
    (let* ((workbook (grid-table-xlsx-create-workbook file))
           (sales (grid-workbook-sheet workbook "销售"))
           (second (grid-workbook-sheet workbook "Sheet2")))
      (should (equal (grid-workbook-sheet-names workbook) '("销售" "Sheet2")))
      ;; The formula that reads the other sheet has a value now.
      (should (equal (grid-get-raw-value-at second 0 0) "=销售!E6*2"))
      (should (equal (grid-get-computed-value-at second 0 0) 112.5))
      (grid-set-raw-value-at sales 2 2 "30")
      (should (equal (grid-get-computed-value-at second 0 0) 247.5))
      ;; A renamed sheet replaces the one it was, and a new one is added.
      (grid-workbook-rename-sheet workbook second "Summary")
      (grid-set-raw-value-at (grid-workbook-new-sheet workbook "My Notes")
                             0 0 "=Summary!A1+'My Notes'!B1")
      (should (= (grid-table-xlsx-write-workbook workbook file) 3)))
    (should (equal (grid-table-xlsx-sheet-names file)
                   '("销售" "Summary" "My Notes")))
    (let* ((workbook (grid-table-xlsx-create-workbook file))
           (summary (grid-workbook-sheet workbook "Summary"))
           (notes (grid-workbook-sheet workbook "My Notes")))
      (should (equal (grid-get-computed-value-at summary 0 0) 247.5))
      (should (equal (grid-get-raw-value-at notes 0 0)
                     "=Summary!A1+'My Notes'!B1"))
      (should (equal (grid-get-computed-value-at notes 0 0) 247.5))
      (should (equal (grid-model-get-merges
                      (gethash :model (grid-workbook-sheet workbook "销售")))
                     '((6 1 7 3)))))
    ;; Written into a workbook it did not come from, a grid leaves that
    ;; workbook's other sheets alone.
    (grid-table-xlsx-test--with-file other
      (copy-file grid-table-xlsx-test--fixture other)
      (grid-table-xlsx-write-workbook (grid-table-xlsx-create-workbook file)
                                      other)
      (should (equal (grid-table-xlsx-sheet-names other)
                     '("销售" "Sheet2" "Summary" "My Notes"))))))

(ert-deftest grid-table-xlsx-test-export-command ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (with-temp-buffer
      (setq-local grid-table--data-source
                  (make-default-data-source
                   (list "a" "b") '(("a" "b") ("1" "=A2*2")) nil))
      (should-error (grid-table-xlsx-module-read file "Sheet1"))
      ;; An untitled grid goes out as Sheet1, and the extension is added.
      (grid-table-xlsx-export (file-name-sans-extension file))
      (should (equal (grid-table-xlsx-sheet-names file) '("Sheet1")))
      (should (equal (gethash :xlsx-sheet grid-table--data-source) "Sheet1"))
      (grid-set-raw-value-at grid-table--data-source 1 0 "5")
      (grid-table-xlsx-export file)
      (should (equal (grid-table-xlsx-sheet-names file) '("Sheet1"))))
    (let ((back (grid-table-xlsx-create-data-source file)))
      (should (equal (grid-get-computed-value-at back 1 1) 10))
      (should-error (grid-table-xlsx-create-data-source file "Missing")))))

(ert-deftest grid-table-xlsx-test-failed-export-leaves-the-file ()
  (skip-unless grid-table-xlsx-test--available)
  (grid-table-xlsx-test--with-file file
    (with-temp-file file (insert "not a workbook"))
    (let ((source (make-default-data-source
                   (list "a") '(("a") ("1")) "Sheet")))
      (should-error (grid-table-xlsx-write-data-source source file)))
    (should (equal (with-temp-buffer (insert-file-contents file)
                                     (buffer-string))
                   "not a workbook"))
    (should-not (directory-files (file-name-directory file) nil
                                 (concat (regexp-quote
                                          (file-name-nondirectory file))
                                         "\\.tmp")))))

(provide 'grid-table-xlsx-test)

;;; grid-table-xlsx-test.el ends here
