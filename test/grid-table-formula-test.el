;;; grid-table-formula-test.el --- Formula engine tests -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'grid-table)

(defun grid-table-formula-test--values (rows)
  "Return the computed values of a model built from ROWS of raw strings."
  (let* ((cols (length (car rows)))
         (model (grid-model-create (length rows) cols rows)))
    (cl-loop for row below (length rows) collect
             (cl-loop for col below cols collect
                      (grid-model-get-computed-value model row col)))))

(defun grid-table-formula-test--eval (formula &rest cells)
  "Return the value of FORMULA in A1 of a row whose other cells are CELLS."
  (caar (grid-table-formula-test--values (list (cons formula cells)))))

(ert-deftest grid-table-formula-operators-follow-spreadsheet-rules ()
  (should (equal (grid-table-formula-test--eval "=2^3") 8))
  (should (equal (grid-table-formula-test--eval "=-5") -5))
  (should (equal (grid-table-formula-test--eval "=+5") 5))
  (should (equal (grid-table-formula-test--eval "=--5") 5))
  (should (equal (grid-table-formula-test--eval "=-B1*2" "3") -6))
  (should (equal (grid-table-formula-test--eval "=10/4") 2.5))
  (should (equal (grid-table-formula-test--eval "=10/5") 2))
  (should (equal (grid-table-formula-test--eval "=MOD(7,4)") 3))
  (should (equal (grid-table-formula-test--eval "=200*5%") 10))
  (should (equal (grid-table-formula-test--eval "=1+2*3") 7))
  (should (equal (grid-table-formula-test--eval "=(1+2)*3") 9))
  (should (equal (grid-table-formula-test--eval "=B1&\"x\"" "1") "1x"))
  (should (equal (grid-table-formula-test--eval "=\"a\"&1+1") "a2")))

(ert-deftest grid-table-formula-comparisons-coerce-numbers ()
  (should (eq (grid-table-formula-test--eval "=B1<=5" "5") t))
  (should (eq (grid-table-formula-test--eval "=B1>=6" "5") nil))
  (should (eq (grid-table-formula-test--eval "=B1<>5" "5") nil))
  (should (eq (grid-table-formula-test--eval "=B1<>6" "5") t))
  (should (eq (grid-table-formula-test--eval "=B1=5" "5") t))
  (should (eq (grid-table-formula-test--eval "=B1=5" "5.0") t))
  (should (eq (grid-table-formula-test--eval "=B1=\"apple\"" "Apple") t))
  (should (equal (grid-table-formula-test--eval "=IF(B1<=5,\"low\",\"high\")" "3")
                 "low"))
  (should (equal (grid-table-formula-test--eval "=IF(TRUE,1,2)") 1))
  (should (equal (grid-table-formula-test--eval "=IF(false,1,2)") 2)))

(ert-deftest grid-table-formula-references-accept-anchors-and-case ()
  (should (equal (grid-table-formula-test--eval "=$B$1+B$1+$B1" "4") 12))
  (should (equal (grid-table-formula-test--eval "=sum(b1:c1)" "4" "5") 9))
  (should (equal (grid-table-formula-test--eval "=SUM($B$1:$C$1)" "4" "5") 9)))

(ert-deftest grid-table-formula-columns-past-z-resolve ()
  (should (= (grid-calc-col-letter-to-number "A") 0))
  (should (= (grid-calc-col-letter-to-number "Z") 25))
  (should (= (grid-calc-col-letter-to-number "AA") 26))
  (should (= (grid-calc-col-letter-to-number "AB") 27))
  (should (= (grid-calc-col-letter-to-number "BA") 52))
  (dolist (col '(0 25 26 27 52 701 702))
    (should (= (grid-calc-col-letter-to-number
                (grid-calc-col-number-to-letter col))
               col)))
  (let ((row (append (list "=AB1*2") (make-list 26 "") (list "21"))))
    (should (equal (caar (grid-table-formula-test--values (list row))) 42))))

(ert-deftest grid-table-formula-order-does-not-depend-on-position ()
  ;; A1 reads B1, which reads C1: both sit after their reader.
  (should (equal (car (grid-table-formula-test--values
                       '(("=B1+1" "=C1+1" "5"))))
                 '(7 6 "5")))
  (let* ((model (grid-model-create 1 3 '(("=B1+1" "=C1+1" "5")))))
    (grid-model-set-raw-value model 0 2 "10")
    (should (equal (grid-model-get-computed-value model 0 0) 12))
    (should (equal (grid-model-get-computed-value model 0 1) 11))))

(ert-deftest grid-table-formula-cycles-are-reported-not-computed ()
  (should (equal (car (grid-table-formula-test--values
                       '(("=B1+1" "=A1+1" "=C1" "7"))))
                 '("#CYCLE!" "#CYCLE!" "#CYCLE!" "7")))
  ;; A cell reading a cycle from outside is an error too, not a number.
  (should (equal (car (grid-table-formula-test--values
                       '(("=B1*2" "=C1+1" "=B1+1"))))
                 '("#CYCLE!" "#CYCLE!" "#CYCLE!"))))

(ert-deftest grid-table-formula-errors-stay-in-their-cell ()
  (should (equal (car (grid-table-formula-test--values
                       '(("=NOPE(1)" "=1/0" "=A1+1" "=1+" "=1 2" "ok"))))
                 '("#NAME?" "#DIV/0!" "#NAME?" "#ERROR!" "#ERROR!" "ok")))
  (should (equal (grid-table-formula-test--eval "=IFERROR(1/0,\"none\")")
                 "none"))
  (should (equal (grid-table-formula-test--eval "=IFERROR(B1,0)" "=1/0") 0))
  (should (equal (grid-table-formula-test--eval "=SUM(B1:C1)" "=1/0" "2")
                 "#DIV/0!"))
  ;; Fixing the cause clears the error everywhere.
  (let ((model (grid-model-create 1 2 '(("=B1+1" "=1/0")))))
    (should (equal (grid-model-get-computed-value model 0 0) "#DIV/0!"))
    (grid-model-set-raw-value model 0 1 "=1/1")
    (should (equal (grid-model-get-computed-value model 0 0) 2))))

(ert-deftest grid-table-formula-aggregates-count-zeros ()
  (should (equal (grid-table-formula-test--eval "=AVERAGE(B1:D1)" "1" "0" "2")
                 1))
  (should (equal (grid-table-formula-test--eval "=MIN(B1:D1)" "1" "0" "2") 0))
  (should (equal (grid-table-formula-test--eval "=SUM(B1:D1)" "1" "x" "") 1)))

(ert-deftest grid-table-formula-count-counts-numbers ()
  ;; COUNT used to be shadowed by the frequency chart, now COUNTPLOT.
  (should (equal (grid-table-formula-test--eval "=COUNT(B1:E1)" "1" "x" "0" "")
                 2))
  (should (plist-get (grid-table-formula-test--eval
                      "=COUNTPLOT(B1:D1,\"Fruit\")" "a" "b" "a")
                     :grid-chart)))

(ert-deftest grid-table-formula-multi-branch-functions-return ()
  (should (equal (grid-table-formula-test--eval
                  "=IFS(B1>5,\"big\",B1>1,\"mid\",TRUE,\"small\")" "3")
                 "mid"))
  (should (equal (car (car (grid-table-formula-test--values
                            '(("=VLOOKUP(\"b\",B1:C2,2,FALSE)" "a" "1")
                              ("" "b" "2")))))
                 "2"))
  (should (equal (car (car (grid-table-formula-test--values
                            '(("=COUNTIFS(B1:B3,\">1\",C1:C3,\"x\")" "1" "x")
                              ("" "2" "x")
                              ("" "3" "y")))))
                 1))
  (should (equal (car (car (grid-table-formula-test--values
                            '(("=SUMIFS(B1:B3,C1:C3,\"x\")" "1" "x")
                              ("" "2" "x")
                              ("" "3" "y")))))
                 3)))

(ert-deftest grid-table-formula-restructure-follows-cells ()
  ;; Row 3 is index 2.  Inserting there pushes row 3 and below down.
  (should (equal (grid-calc-restructure-formula "=A2+A3+$A$4" 'row 2 1)
                 "=A2+A4+$A$5"))
  (should (equal (grid-calc-restructure-formula "=SUM(A2:A5)" 'row 2 1)
                 "=SUM(A2:A6)"))
  (should (equal (grid-calc-restructure-formula "=SUM(A2:A5)" 'row 5 1)
                 "=SUM(A2:A5)"))
  ;; Deleting row 3.
  (should (equal (grid-calc-restructure-formula "=A2+A3+A4" 'row 2 -1)
                 "=A2+#REF!+A3"))
  (should (equal (grid-calc-restructure-formula "=SUM(A2:A5)" 'row 2 -1)
                 "=SUM(A2:A4)"))
  (should (equal (grid-calc-restructure-formula "=SUM(A3:A5)" 'row 2 -1)
                 "=SUM(A3:A4)"))
  (should (equal (grid-calc-restructure-formula "=SUM(A2:A3)" 'row 2 -1)
                 "=SUM(A2:A2)"))
  (should (equal (grid-calc-restructure-formula "=SUM(A3:B3)" 'row 2 -1)
                 "=SUM(#REF!)"))
  (should (equal (grid-calc-restructure-formula "=SUM(A5:A2)" 'row 2 -1)
                 "=SUM(A4:A2)"))
  ;; Columns: C is index 2.
  (should (equal (grid-calc-restructure-formula "=B1+C1+$D1" 'col 2 1)
                 "=B1+D1+$E1"))
  (should (equal (grid-calc-restructure-formula "=SUM(B1:D1)+Z9" 'col 2 -1)
                 "=SUM(B1:C1)+Y9"))
  (should (equal (grid-calc-restructure-formula "=C1&\"C1\"" 'col 2 -1)
                 "=#REF!&\"C1\""))
  (should (equal (grid-calc-restructure-formula "plain C1" 'col 2 -1)
                 "plain C1")))

(ert-deftest grid-table-formula-structural-edits-rewrite-references ()
  (let* ((rows '(("n" "double" "sum")
                 ("1" "=A2*2" "=SUM(A2:A4)")
                 ("2" "=A3*2" "")
                 ("3" "=A4*2" "")))
         (model (grid-model-create 4 3 rows))
         (raw (lambda (model row col)
                (grid-cell-raw-input (grid-model-get-cell model row col)))))
    (should (equal (grid-model-get-computed-value model 1 2) 6))
    ;; A row inserted before the second data row.
    (let ((added (grid-model-add-row model 1)))
      (should (equal (funcall raw added 1 1) "=A2*2"))
      (should (equal (funcall raw added 3 1) "=A4*2"))
      (should (equal (funcall raw added 1 2) "=SUM(A2:A5)"))
      (should (equal (grid-model-get-computed-value added 4 1) 6))
      (should (equal (grid-model-get-computed-value added 1 2) 6)))
    ;; The second data row deleted.
    (let ((deleted (grid-model-delete-row model 1)))
      (should (equal (funcall raw deleted 2 1) "=A3*2"))
      (should (equal (funcall raw deleted 1 2) "=SUM(A2:A3)"))
      (should (equal (grid-model-get-computed-value deleted 1 2) 4)))
    ;; A column inserted at the front, then the old first column deleted.
    (let ((added (grid-model-add-column model 0)))
      (should (equal (funcall raw added 1 2) "=B2*2"))
      (should (equal (funcall raw added 1 3) "=SUM(B2:B4)"))
      (should (equal (grid-model-get-computed-value added 1 3) 6))
      (let ((deleted (grid-model-delete-column added 1)))
        (should (equal (funcall raw deleted 1 1) "=#REF!*2"))
        (should (equal (funcall raw deleted 1 2) "=SUM(#REF!)"))))))

(ert-deftest grid-table-formula-reads-other-sheets ()
  (should (equal (grid-parser-parse "Sheet2!A1+1")
                 '(BINARY-OP + (CELL-REF "A1" "Sheet2") (NUMBER 1))))
  (should (equal (grid-parser-parse "SUM('My Sheet'!a1:b2)")
                 '(FUNCTION-CALL "SUM" ((RANGE (CELL-REF "A1" "My Sheet")
                                               (CELL-REF "B2" "My Sheet"))))))
  (should (equal (grid-parser-parse "销售!E6*2")
                 '(BINARY-OP * (CELL-REF "E6" "销售") (NUMBER 2))))
  (should (equal (grid-parser-parse "'It''s'!$A$1") '(CELL-REF "$A$1" "It's")))
  (let* ((a (grid-model-create 3 2 '(("x" "y") ("1" "=B!A1+1")
                                     ("=A2*2" "=VLOOKUP(\"k\",b!A3:B3,2,FALSE)"))))
         (b (grid-model-create 3 2 '(("10" "=A!A3") ("=SUM(A!A2:A3)" "=Nope!A1")
                                     ("k" "found"))))
         (sheets `(("A" . ,a) ("B" . ,b)))
         (grid-calc-sheets (lambda (name) (cdr (assoc-string name sheets t)))))
    (grid-table-calc-evaluate-models (list a b))
    (should (equal (grid-model-get-computed-value a 1 1) 11))
    (should (equal (grid-model-get-computed-value a 2 1) "found"))
    ;; B!B1 reads a formula of the other sheet, computed on the way.
    (should (equal (grid-model-get-computed-value b 0 1) 2))
    (should (equal (grid-model-get-computed-value b 1 0) 3))
    (should (equal (grid-model-get-computed-value b 1 1) "#REF!")))
  ;; Without sheets to look in, the reference has nowhere to go.
  (let ((alone (grid-model-create 1 1 '(("=Other!A1")))))
    (should (equal (grid-model-get-computed-value alone 0 0) "#REF!")))
  ;; A cycle through two sheets is a cycle.
  (let* ((a (grid-model-create 1 1 '(("=B!A1"))))
         (b (grid-model-create 1 1 '(("=A!A1"))))
         (sheets `(("A" . ,a) ("B" . ,b)))
         (grid-calc-sheets (lambda (name) (cdr (assoc-string name sheets t)))))
    (grid-table-calc-evaluate-models (list a b))
    (should (equal (grid-model-get-computed-value a 0 0) "#CYCLE!"))
    (should (equal (grid-model-get-computed-value b 0 0) "#CYCLE!"))))

(ert-deftest grid-table-formula-rewrites-respect-sheets ()
  (should (equal (grid-calc-shift-formula
                  "=Sheet2!A1+'My Sheet'!$B$2+销售!C3:D4" 1 1)
                 "=Sheet2!B2+'My Sheet'!$B$2+销售!D4:E5"))
  ;; Only references into the sheet that changed move.
  (should (equal (grid-calc-restructure-formula "=A3+Sheet2!A3+Data!A3" 'row 1 1)
                 "=A4+Sheet2!A3+Data!A3"))
  (should (equal (grid-calc-restructure-formula
                  "=A3+Sheet2!A3+Data!A3" 'row 1 1 "sheet2")
                 "=A4+Sheet2!A4+Data!A3"))
  (should (equal (grid-calc-restructure-formula
                  "=A3+Sheet2!A3+SUM(Data!A1:A3)" 'row 1 1 "Sheet2" "Data")
                 "=A3+Sheet2!A3+SUM(Data!A1:A4)"))
  (should (equal (grid-calc-restructure-formula
                  "=Data!A2" 'row 1 -1 "Sheet2" "Data")
                 "=Data!#REF!"))
  (should (equal (grid-calc-rename-sheet-in-formula
                  "=Sheet2!A1+sheet2!B2:C3+Other!A1+\"Sheet2!A1\""
                  "Sheet2" "My Data")
                 "='My Data'!A1+'My Data'!B2:C3+Other!A1+\"Sheet2!A1\""))
  (should (equal (grid-calc-rename-sheet-in-formula
                  "='My Data'!A1" "My Data" "Plain")
                 "=Plain!A1")))

(ert-deftest grid-table-workbook-keeps-sheets-together ()
  (let* ((data (make-default-data-source
                (list "n" "v")
                '(("n" "v") ("1" "=Totals!A2+1") ("2" "5") ("3" "6")) "Data"))
         (workbook (grid-workbook-ensure data))
         (totals (grid-workbook-new-sheet workbook "Totals"))
         (file (make-temp-file "grid-workbook-" nil ".grid")))
    (unwind-protect
        (progn
          (should (equal (grid-workbook-sheet-names workbook)
                         '("Data" "Totals")))
          (grid-set-raw-value-at totals 1 0 "=SUM(Data!A2:A4)")
          (should (equal (grid-get-computed-value-at totals 1 0) 6))
          ;; Each sheet reads the other; an edit in one reaches both.
          (should (equal (grid-get-computed-value-at data 1 1) 7))
          (grid-set-raw-value-at data 2 0 "20")
          (should (equal (grid-get-computed-value-at totals 1 0) 24))
          (should (equal (grid-get-computed-value-at data 1 1) 25))
          (funcall (gethash :delete-row data) data 2)
          (should (equal (grid-get-raw-value-at totals 1 0)
                         "=SUM(Data!A2:A3)"))
          (should (equal (grid-get-computed-value-at totals 1 0) 21))
          (should (equal (grid-workbook-name-problem workbook "data")
                         "There is already a sheet called data"))
          (should (grid-workbook-name-problem workbook "a:b"))
          (should (equal (grid-workbook-fresh-name workbook) "Sheet1"))
          (should (equal (grid-workbook-fresh-name workbook "Data") "Sheet1"))
          ;; A name with an apostrophe or an exclamation mark is a name.
          (let ((odd (grid-workbook-new-sheet workbook "It's Q1!")))
            (grid-set-raw-value-at odd 0 0 "7")
            (grid-set-raw-value-at totals 2 0 "='It''s Q1!'!A1*2")
            (should (equal (grid-get-computed-value-at totals 2 0) 14)))
          ;; Several sheets are saved together and come back together.
          (let ((inhibit-message t))
            (grid-table-persistence--save-to-file totals file))
          (let* ((back (grid-table-persistence-create-data-source-from-file
                        file))
                 (sheets (grid-workbook-sheets (grid-workbook-of back))))
            (should (equal (grid-workbook-sheet-name back) "Totals"))
            (should (equal (mapcar #'grid-workbook-sheet-name sheets)
                           '("Data" "Totals" "It's Q1!")))
            (should (equal (grid-get-computed-value-at back 2 0) 14))
            (should (equal (gethash :title (car sheets)) "Data"))
            (should (equal (grid-get-computed-value-at back 1 0) 21))
            (should (equal (grid-get-computed-value-at (car sheets) 1 1) 22)))
          ;; One sheet alone is saved as it always was.
          (grid-workbook-remove-sheet
           workbook (grid-workbook-sheet workbook "It's Q1!"))
          (grid-workbook-remove-sheet workbook data)
          (should (equal (grid-get-computed-value-at totals 1 0) "#REF!"))
          (should-not
           (eq (car-safe (grid-table-persistence--file-data totals))
               'grid-table-file))
          (should-error (grid-workbook-remove-sheet workbook totals)))
      (delete-file file))))

(ert-deftest grid-table-formula-shift-moves-relative-references ()
  (should (equal (grid-calc-shift-formula "=B2+C2" 1 0) "=B3+C3"))
  (should (equal (grid-calc-shift-formula "=B2+C2" 0 2) "=D2+E2"))
  (should (equal (grid-calc-shift-formula "=$B$2+B$2+$B2+B2" 3 1)
                 "=$B$2+C$2+$B5+C5"))
  (should (equal (grid-calc-shift-formula "=SUM(A1:B2)*LOG10(Z9)" 1 1)
                 "=SUM(B2:C3)*LOG10(AA10)"))
  (should (equal (grid-calc-shift-formula "=A1&\"A1 stays\"" 1 0)
                 "=A2&\"A1 stays\""))
  (should (equal (grid-calc-shift-formula "=A1+B2" -1 -1) "=#REF!+A1"))
  (should (equal (grid-calc-shift-formula "plain A1" 1 1) "plain A1"))
  (should (equal (grid-calc-shift-formula "=elisp:(cell \"A1\")" 1 1)
                 "=elisp:(cell \"A1\")")))

;;;----------------------------------------------------------------------
;;; Sorting
;;;----------------------------------------------------------------------

(defun grid-table-formula-test--sorted-column (values order)
  "Return column A below its header after sorting VALUES in ORDER."
  (let* ((rows (cons '("h") (mapcar #'list values)))
         (sorted (grid-model-sort-by-column
                  (grid-model-create (length rows) 1 rows) 0 order)))
    (cl-loop for row from 1 below (length rows) collect
             (grid-cell-raw-input (grid-model-get-cell sorted row 0)))))

(ert-deftest grid-table-sort-orders-numbers-by-value ()
  (should (equal (grid-table-formula-test--sorted-column
                  '("10" "9" "100" "-1" "2.5") 'ascending)
                 '("-1" "2.5" "9" "10" "100")))
  (should (equal (grid-table-formula-test--sorted-column
                  '("10" "9" "100") 'descending)
                 '("100" "10" "9"))))

(ert-deftest grid-table-sort-puts-numbers-before-text-and-blanks-last ()
  (should (equal (grid-table-formula-test--sorted-column
                  '("pear" "" "3" "Apple" "20" "banana") 'ascending)
                 '("3" "20" "Apple" "banana" "pear" "")))
  (should (equal (grid-table-formula-test--sorted-column
                  '("pear" "" "3" "Apple" "20" "banana") 'descending)
                 '("pear" "banana" "Apple" "20" "3" ""))))

(ert-deftest grid-table-sort-uses-formula-results-and-is-stable ()
  ;; Column B computes from column A; sorting by B orders by the results.
  (let* ((model (grid-model-create
                 4 2 '(("h" "h") ("b" "=10*3") ("a" "=5+5") ("c" "=10*3"))))
         (sorted (grid-model-sort-by-column model 1 'ascending)))
    (should (equal (cl-loop for row from 1 to 3 collect
                            (grid-cell-raw-input
                             (grid-model-get-cell sorted row 0)))
                   ;; Equal results keep their original order.
                   '("a" "b" "c")))))

(ert-deftest grid-table-formulas-follow-spreadsheet-conventions ()
  (let* ((source (make-default-data-source
                  '("" "" "" "")
                  '(("1" "2" "3" "") ("4" "5" "6" "") ("" "" "" ""))))
         (value (lambda (formula)
                  (grid-set-raw-value-at source 2 3 formula)
                  (grid-get-computed-value-at source 2 3))))
    ;; Aggregates take any number of values and ranges.
    (should (equal (funcall value "=SUM(1,2,3)") 6))
    (should (equal (funcall value "=SUM(A1:A2,B1)") 7))
    (should (equal (funcall value "=AVERAGE(1,2)") 1.5))
    (should (equal (funcall value "=MAX(1,5,3)") 5))
    (should (equal (funcall value "=MIN(A1:C1,0)") 0))
    (should (equal (funcall value "=COUNT(A1:C2,7)") 7))
    (should (equal (funcall value "=COUNTA(A1:B2,\"x\")") 5))
    ;; IF only evaluates the branch it picks.
    (should (equal (funcall value "=IF(TRUE,1,1/0)") 1))
    (should (equal (funcall value "=IF(FALSE,1/0,2)") 2))
    (should (equal (funcall value "=IF(1/0,1,2)") "#DIV/0!"))
    ;; A percent sign divides by 100.
    (should (equal (funcall value "=10%") 0.1))
    (should (equal (funcall value "=A2*50%") 2))
    (should (equal (funcall value "=-10%") -0.1))
    (should (equal (funcall value "=MOD(7,3)") 1))
    ;; ROUND takes a half away from zero.
    (should (equal (funcall value "=ROUND(2.5,0)") 3))
    (should (equal (funcall value "=ROUND(-2.5,0)") -3))
    (should (equal (funcall value "=ROUND(1.005,2)") 1.01))
    ;; Whole columns and whole rows.
    (should (equal (funcall value "=SUM(A:A)") 5))
    (should (equal (funcall value "=SUM(B:C)") 16))
    (should (equal (funcall value "=SUM($1:$2)") 21))
    (should (equal (funcall value "=SUM(D:D)") "#CYCLE!"))
    ;; XLOOKUP
    (should (equal (funcall value "=XLOOKUP(4,A:A,C:C)") "6"))
    (should (equal (funcall value "=XLOOKUP(9,A1:A2,B1:B2,\"none\")") "none"))
    (should (equal (funcall value "=XLOOKUP(9,A1:A2,B1:B2)") "#N/A"))
    (should (equal (funcall value "=XLOOKUP(1,A1:A2,B1:B2,1/0)") "2"))))

(ert-deftest grid-table-sort-keeps-a-formula-reading-its-own-row ()
  (let* ((model (grid-model-create
                 4 4 '(("Key" "Amount" "Double" "Share")
                       ("b" "10" "=B2*2" "=B2/$B$4+Rates!B2")
                       ("a" "20" "=B3*2" "=SUM(B$2:B3)")
                       ("" "30" "" ""))))
         (sorted (grid-model-sort-by-column model 0 'ascending))
         (raw (lambda (row col)
                (grid-cell-raw-input (grid-model-get-cell sorted row col)))))
    (should (equal (funcall raw 1 0) "a"))
    ;; Each row still doubles its own amount.
    (should (equal (funcall raw 1 2) "=B2*2"))
    (should (equal (funcall raw 2 2) "=B3*2"))
    (should (equal (grid-model-get-computed-value sorted 1 2) 40))
    (should (equal (grid-model-get-computed-value sorted 2 2) 20))
    ;; Anchored parts and references into another sheet stay.
    (should (equal (funcall raw 2 3) "=B3/$B$4+Rates!B2"))
    (should (equal (funcall raw 1 3) "=SUM(B$2:B2)"))))

;;;----------------------------------------------------------------------
;;; Cell styles and conditional formats
;;;----------------------------------------------------------------------

(defun grid-table-formula-test--source ()
  "Return a 4x3 source: a header row and rows keyed c, a, b in column A."
  (make-default-data-source
   '("" "" "")
   '(("name" "n" "x") ("c" "3" "") ("a" "1" "") ("b" "2" ""))))

(ert-deftest grid-table-styles-follow-their-cells-through-structure-changes ()
  (let ((source (grid-table-formula-test--source)))
    (grid-set-style-at source 2 1 '(:bold t :color "#ff0000"))
    (should (equal (grid-get-style-at source 2 1) '(:bold t :color "#ff0000")))
    (should-not (grid-get-style-at source 1 1))
    ;; Inserting above moves the style down with its cell.
    (funcall (gethash :add-row source) source 0)
    (should (equal (grid-get-raw-value-at source 3 0) "a"))
    (should (plist-get (grid-get-style-at source 3 1) :bold))
    (should-not (grid-get-style-at source 2 1))
    (funcall (gethash :delete-row source) source 0)
    (should (plist-get (grid-get-style-at source 2 1) :bold))
    (funcall (gethash :add-column source) source 0)
    (should (plist-get (grid-get-style-at source 2 2) :bold))
    (funcall (gethash :delete-column source) source 0)
    (should (plist-get (grid-get-style-at source 2 1) :bold))
    ;; Sorting by name puts row "a" first among the data rows.
    (funcall (gethash :sort-by-column source) source 0 'ascending)
    (should (equal (grid-get-raw-value-at source 1 0) "a"))
    (should (plist-get (grid-get-style-at source 1 1) :bold))
    (should-not (grid-get-style-at source 2 1))
    ;; Deleting the styled row drops the style with it.
    (funcall (gethash :delete-row source) source 0)
    (should-not (grid-get-style-at source 1 1))
    ;; Clearing removes the entry.
    (grid-set-style-at source 0 0 '(:italic t))
    (grid-set-style-at source 0 0 nil)
    (should-not (grid-get-style-at source 0 0))))

(ert-deftest grid-table-conditional-formats-apply-by-value ()
  (let ((source (grid-table-formula-test--source)))
    (grid-set-conditional-formats
     source '((1 1 3 1 ">" "1" (:background "#ffff00"))
              (1 1 3 1 ">=" "3" (:bold t :background "#ff0000"))))
    (grid-set-style-at source 1 1 '(:italic t))
    ;; 3 satisfies both rules: the later one wins where they overlap and
    ;; the cell's own style stays.
    (should (equal (grid-table-effective-style source 1 1)
                   '(:italic t :background "#ff0000" :bold t)))
    (should-not (grid-table-effective-style source 2 1))
    (should (equal (grid-table-effective-style source 3 1)
                   '(:background "#ffff00")))
    ;; The rule follows the value, not the cell.
    (grid-set-raw-value-at source 2 1 "9")
    (should (plist-get (grid-table-effective-style source 2 1) :bold))
    ;; Rows inserted above shift the range; deleting it all drops the rule.
    (funcall (gethash :add-row source) source 0)
    (should (equal (seq-take (car (grid-get-conditional-formats source)) 4)
                   '(2 1 4 1)))
    (should (equal (grid-table-effective-style source 1 1) nil))
    (dotimes (_ 3) (funcall (gethash :delete-row source) source 1))
    (should-not (grid-get-conditional-formats source))))

(ert-deftest grid-table-conditional-operators-match ()
  (should (grid-table-conditional-match-p "=" "apple" "Apple"))
  (should-not (grid-table-conditional-match-p "=" "" ""))
  (should (grid-table-conditional-match-p "<>" "1" "2"))
  (should (grid-table-conditional-match-p "<=" "5" 5))
  (should-not (grid-table-conditional-match-p ">" "5" "text"))
  (should (grid-table-conditional-match-p "contains" "rr" "Error"))
  (should (grid-table-conditional-match-p "empty" "" nil))
  (should (grid-table-conditional-match-p "error" "" "#DIV/0!"))
  (should-not (grid-table-conditional-match-p "error" "" "fine")))

(ert-deftest grid-table-formats-round-trip-through-a-version-4-file ()
  (let ((source (grid-table-formula-test--source))
        (file (make-temp-file "grid-formats-" nil ".grid")))
    (unwind-protect
        (progn
          ;; Without formats the file stays in the oldest format.
          (grid-table-persistence--save-to-file source file)
          (should (keywordp (car (with-temp-buffer
                                   (insert-file-contents file)
                                   (read (current-buffer))))))
          (grid-set-style-at source 2 1 '(:bold t :align center))
          (grid-set-conditional-formats
           source '((1 1 3 1 ">" "1" (:background "#ffff00"))))
          (grid-merge-cells source 1 1 1 2)
          (grid-table-persistence--save-to-file source file)
          (let ((saved (with-temp-buffer
                         (insert-file-contents file)
                         (read (current-buffer)))))
            (should (eq (car saved) 'grid-table-file))
            (should (= (plist-get (cdr saved) :version) 4)))
          (let ((loaded (grid-table-persistence-create-data-source-from-file
                         file)))
            (should (equal (grid-get-style-at loaded 2 1)
                           '(:bold t :align center)))
            (should (equal (grid-get-conditional-formats loaded)
                           '((1 1 3 1 ">" "1" (:background "#ffff00")))))
            (should (equal (grid-get-merges loaded) '((1 1 1 2))))
            (should (equal (grid-get-raw-value-at loaded 3 0) "b"))))
      (delete-file file))))

(provide 'grid-table-formula-test)

;;; grid-table-formula-test.el ends here
