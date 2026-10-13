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
(setq grid-table-renderer 'classic)
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

(ert-deftest grid-table-xlsx-test-date-formats ()
  (should (grid-table-xlsx--date-format-p "yyyy-mm-dd"))
  (should (grid-table-xlsx--date-format-p "h:mm AM/PM"))
  (should-not (grid-table-xlsx--date-format-p "0.00"))
  (should-not (grid-table-xlsx--date-format-p "0.0%"))
  (should-not (grid-table-xlsx--date-format-p "#,##0 \"days\""))
  (should-not (grid-table-xlsx--date-format-p "[Red]0.00"))
  (should (equal (grid-table-xlsx--date-text 46307) "2026-10-12"))
  (should (equal (grid-table-xlsx--date-text 61) "1900-03-01"))
  (should (equal (grid-table-xlsx--date-text 0.75) "18:00:00"))
  (should (equal (grid-table-xlsx--date-text 46307.5) "2026-10-12 12:00:00"))
  (should (equal (grid-table-xlsx--raw '(0 0 "46307" :format "m/d/yyyy"))
                 "2026-10-12"))
  (should (equal (grid-table-xlsx--raw '(0 0 "0.125" :format "0.0%")) "0.125"))
  (should (equal (grid-table-xlsx--raw '(0 0 "text" :format "yyyy-mm-dd"))
                 "text")))

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
    (should-not (grid-table-xlsx--cell-record 0 0 "" nil))
    (should (equal (seq-drop (funcall fields
                                      (grid-table-xlsx--cell-record
                                       0 0 "" '(:bold t :background "#ffc7ce"
                                                :align center)))
                             5)
                   '("1" "" "" "FFC7CE" "center")))))

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
    ;; A date comes in as text, a percentage as the number it holds.
    (should (equal (grid-table-xlsx-test--raw source 3 5) "2026-10-12"))
    (should (equal (grid-table-xlsx-test--raw source 4 5) "0.125"))
    (should (equal (grid-model-get-merges model) '((6 1 7 3))))
    (should (equal (grid-model-get-style model 1 1)
                   '(:bold t :color "#FFFFFF" :background "#4472C4"
                           :align center)))
    (should (equal (grid-model-get-style model 2 1)
                   '(:italic t :color "#00B050")))
    (should (equal (grid-model-get-style model 3 3) '(:align right)))
    (should (equal (grid-model-get-conditionals model)
                   '((2 2 4 2 ">" "5" (:bold t :background "#FFC7CE")))))
    (should (equal (gethash :xlsx-skipped-conditionals source) 0)))
  (let ((source (grid-table-xlsx-create-data-source
                 grid-table-xlsx-test--fixture "Sheet2")))
    (should (equal (grid-table-xlsx-test--raw source 0 0) "=销售!E6*2"))
    (should (equal (grid-table-xlsx-test--raw source 1 0) "line1\nline2"))))

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
