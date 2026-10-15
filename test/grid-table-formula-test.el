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

(ert-deftest grid-table-formula-several-values-feed-aggregates ()
  (let ((data '(("x" "10" "b") ("y" "20" "a") ("x" "5" "B") ("z" "" "c")
                ("x" "1" ""))))
    (cl-flet ((value (formula)
                (caar (grid-table-formula-test--values
                       (cons (list formula "" "") data)))))
      (should (equal (value "=SUM(UNIQUE(B2:B6))") 36))
      ;; Text compares without case; a blank is one more value, not counted.
      (should (equal (value "=COUNTA(UNIQUE(C2:C6))") 3))
      (should (equal (value "=SUM(FILTER(B2:B6,A2:A6=\"x\"))") 16))
      (should (equal (value "=SUM(FILTER(B2:B6,A2:A6=\"q\"))") "#CALC!"))
      (should (equal (value "=SUM(FILTER(B2:B6,A2:A6=\"q\",0))") 0))
      (should (equal (value "=SUM(FILTER(B2:B6,A2:A5=\"x\"))") "#VALUE!"))
      ;; Comparisons and arithmetic go element by element.
      (should (equal (value "=SUM((A2:A6=\"x\")*B2:B6)") 16))
      (should (equal (value "=MAX(SORT(B2:B6))") 20))
      ;; Only one row or one column; a cell shows one value.
      (should (equal (value "=UNIQUE(A2:B6)") "#VALUE!"))
      (should (equal (value "=B2:B4") "#VALUE!"))
      (should (equal (value "=B3:B3") "20"))
      (should (equal (value "=UNIQUE(A2:A2)") "x")))))

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
          ;; A name longer than one can type here may come from a file.
          (let ((long (make-string 40 ?x)))
            (should (grid-workbook-name-problem workbook long))
            (should-not (grid-workbook-name-problem workbook long nil t))
            (should (equal (grid-workbook-sheet-name
                            (car (grid-workbook-sheets
                                  (grid-workbook-create
                                   (list (cons long (make-default-data-source
                                                     '("") '(("1"))))
                                         (cons "Other"
                                               (make-default-data-source
                                                '("")
                                                `((,(format "=%s!A1+1" long))))))
                                   t))))
                           long)))
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

(ert-deftest grid-table-formula-rewrites-follow-whole-columns-and-rows ()
  ;; Copying moves the relative ones.
  (should (equal (grid-calc-shift-formula "=SUM(A:A)+SUM($B:C)+SUM(2:3)" 1 1)
                 "=SUM(B:B)+SUM($B:D)+SUM(3:4)"))
  (should (equal (grid-calc-shift-formula "=SUM(A:A)" 0 -1) "=SUM(#REF!:#REF!)"))
  ;; A column inserted or deleted moves them, a row leaves them alone.
  (should (equal (grid-calc-restructure-formula "=SUM(B:C)+SUM(2:5)" 'col 0 1)
                 "=SUM(C:D)+SUM(2:5)"))
  (should (equal (grid-calc-restructure-formula "=SUM(B:C)+SUM(2:5)" 'row 0 1)
                 "=SUM(B:C)+SUM(3:6)"))
  (should (equal (grid-calc-restructure-formula "=SUM(B:D)" 'col 2 -1)
                 "=SUM(B:C)"))
  (should (equal (grid-calc-restructure-formula "=SUM(B:B)+1" 'col 1 -1)
                 "=SUM(#REF!)+1"))
  ;; A renamed sheet keeps them.
  (should (equal (grid-calc-rename-sheet-in-formula
                  "=SUM(Data!A:A)+SUM('Old one'!2:2)" "Data" "New data")
                 "=SUM('New data'!A:A)+SUM('Old one'!2:2)"))
  ;; Names and text are not ranges.
  (should (equal (grid-calc-shift-formula "=LOG10(A1)&\"A:A\"" 0 1)
                 "=LOG10(B1)&\"A:A\"")))

(ert-deftest grid-table-sheet-without-a-header-row ()
  (let ((source (make-default-data-source
                 '("" "")
                 '(("pear" "3") ("apple" "=B1*2") ("fig" "1")))))
    (should (grid-table-header-row-p source))
    ;; With a header row the first row stays where it is.
    (funcall (gethash :sort-by-column source) source 0 'ascending)
    (should (equal (grid-get-raw-value-at source 0 0) "pear"))
    (should (equal (grid-get-raw-value-at source 1 0) "apple"))
    ;; Without one it is sorted like any other row.
    (grid-table-set-header-row source nil)
    (funcall (gethash :sort-by-column source) source 0 'ascending)
    (should (equal (mapcar (lambda (row) (grid-get-raw-value-at source row 0))
                           '(0 1 2))
                   '("apple" "fig" "pear")))
    ;; A formula that moved with its row still reads the same distance up.
    (should (equal (grid-get-raw-value-at source 0 1) "=#REF!*2"))
    (grid-set-raw-value-at source 0 1 "7")
    ;; A row can go above the first row, and the first row can go.
    (funcall (gethash :add-row source) source -1)
    (should (equal (grid-get-row-count source) 4))
    (should (equal (grid-get-raw-value-at source 0 0) ""))
    (should (equal (grid-get-raw-value-at source 1 0) "apple"))
    (grid-set-raw-value-at source 3 1 "=B2+1")
    (funcall (gethash :delete-row source) source -1)
    (should (equal (grid-get-raw-value-at source 0 0) "apple"))
    (should (equal (grid-get-raw-value-at source 2 1) "=B1+1"))
    (should (equal (grid-get-computed-value-at source 2 1) 8))
    ;; A merge may reach down from the first row.
    (grid-set-raw-value-at source 1 0 "")
    (grid-merge-cells source 0 0 1 0)
    (should (equal (grid-get-merges source) '((0 0 1 0))))
    ;; It keeps the header row from coming back.
    (should-error (grid-table-set-header-row source t) :type 'user-error)
    ;; The setting and the merge are saved with the grid.
    (let ((file (make-temp-file "grid-noheader-" nil ".grid")))
      (unwind-protect
          (progn
            (grid-table-persistence--save-to-file source file)
            (let ((back (grid-table-persistence-create-data-source-from-file
                         file)))
              (should-not (grid-table-header-row-p back))
              (should (equal (grid-get-merges back) '((0 0 1 0))))))
        (delete-file file)))
    (grid-unmerge-cell source 0 0)
    (grid-table-set-header-row source t)
    (should (grid-table-header-row-p source))
    (should-error (funcall (gethash :merge-cells source) source 0 0 1 0))))

;;;----------------------------------------------------------------------
;;; Number formats and dates
;;;----------------------------------------------------------------------

(ert-deftest grid-table-formats-show-numbers ()
  (should (equal (grid-format-display 1234.5 '(number 2 t)) "1,234.50"))
  (should (equal (grid-format-display -1234567 '(number 0 t)) "-1,234,567"))
  (should (equal (grid-format-display 2.5 '(number 0 nil)) "3"))
  (should (equal (grid-format-display "0.125" '(percent 1)) "12.5%"))
  (should (equal (grid-format-display -3.5 '(currency 2 "$")) "-$3.50"))
  (should (equal (grid-format-display "text" '(number 2 t)) "text"))
  ;; The general format hides the noise of binary fractions.
  (should (equal (grid-format-display (+ 0.1 0.2) nil) "0.3"))
  (should (equal (grid-format-display 3.0 nil) "3"))
  (should (equal (grid-format-display t nil) "TRUE"))
  ;; Dates are serial numbers.
  (should (= (grid-format-date-to-serial 2026 10 12) 46307))
  (should (= (grid-format-date-to-serial 2024 2 29) 45351))
  (should (equal (grid-format-display 46307 '(date "yyyy-mm-dd")) "2026-10-12"))
  (should (equal (grid-format-display 45351 '(date "m/d/yyyy")) "2/29/2024"))
  (should (equal (grid-format-display 46307.75 '(date "yyyy-mm-dd hh:mm"))
                 "2026-10-12 18:00"))
  (should (equal (grid-format-display 46307 '(date "yyyy年m月d日"))
                 "2026年10月12日"))
  ;; Spreadsheet format codes come in and go back out.
  (should (equal (grid-format-from-code "#,##0.00") '(number 2 t)))
  (should (equal (grid-format-from-code "0.0%") '(percent 1)))
  (should (equal (grid-format-from-code "\"¥\"#,##0.00") '(currency 2 "¥")))
  (should (equal (grid-format-from-code "yyyy\\-mm\\-dd") '(date "yyyy-mm-dd")))
  (should (equal (grid-format-from-code "@") '(text)))
  (should-not (grid-format-from-code "General"))
  (should (equal (grid-format-from-code "[$-409]mmmm d, yyyy")
                 '(date "yyyy-mm-dd" "[$-409]mmmm d, yyyy")))
  (should (equal (grid-format-from-code "0.00E+00") '(code "0.00E+00")))
  (dolist (format '((number 2 t) (number 0 nil) (percent 1) (date "yyyy/mm/dd")
                    (date "yyyy年m月d日") (text) (code "0.00E+00")
                    (date "yyyy-mm-dd" "[$-409]mmmm d, yyyy")))
    (should (equal (grid-format-from-code (grid-format-to-code format))
                   format))))

(ert-deftest grid-table-typed-dates-and-percentages-become-numbers ()
  (let ((source (make-default-data-source
                 '("" "") '(("h" "") ("" "") ("" "") ("" "")))))
    (grid-table-enter-value source 1 0 "2026-10-13")
    (should (equal (grid-get-raw-value-at source 1 0) "46308"))
    (should (equal (grid-get-display-value-at source 1 0) "2026-10-13"))
    (should (equal (grid-table-edit-text source 1 0) "2026-10-13"))
    (grid-set-raw-value-at source 1 1 "=A2+7")
    (should (equal (grid-get-computed-value-at source 1 1) 46315))
    ;; A date that does not exist is not one.
    (grid-table-enter-value source 2 0 "2026-02-30")
    (should (equal (grid-get-raw-value-at source 2 0) "2026-02-30"))
    (grid-table-enter-value source 2 0 "12.5%")
    (should (equal (grid-get-raw-value-at source 2 0) "0.125"))
    (should (equal (grid-get-display-value-at source 2 0) "12.5%"))
    (should (equal (grid-table-edit-text source 2 0) "12.5%"))
    ;; A cell that has a format keeps it.
    (grid-table-enter-value source 2 0 "50%")
    (should (equal (grid-get-display-value-at source 2 0) "50.0%"))
    (grid-table-enter-value source 2 1 "$1,234.5")
    (should (equal (grid-get-display-value-at source 2 1) "$1,234.5"))
    (should (equal (grid-get-raw-value-at source 2 1) "1234.5"))
    ;; Text typed after an apostrophe is never a number to a range.
    (grid-table-enter-value source 3 0 "'007")
    (should (equal (grid-get-display-value-at source 3 0) "007"))
    (grid-set-raw-value-at source 3 1 "=COUNT(A4:A4)")
    (should (equal (grid-get-computed-value-at source 3 1) 0))
    (grid-set-raw-value-at source 3 1 "=A4+1")
    (should (equal (grid-get-computed-value-at source 3 1) 8))
    ;; So is a number typed into a cell with the text format, which keeps
    ;; it after an apostrophe; a number already there stays a number.
    (grid-set-style-at source 0 1 '(:format (text)))
    (grid-set-raw-value-at source 0 1 "12")
    (grid-set-raw-value-at source 0 0 "=COUNT(B1:B1)")
    (should (equal (grid-get-computed-value-at source 0 0) 1))
    (grid-table-enter-value source 0 1 "12")
    (should (equal (grid-get-raw-value-at source 0 1) "'12"))
    (should (equal (grid-table-edit-text source 0 1) "12"))
    (should (equal (grid-get-computed-value-at source 0 0) 0))
    (grid-table-enter-value source 0 1 "twelve")
    (should (equal (grid-get-raw-value-at source 0 1) "twelve"))
    (grid-table-enter-value source 0 1 "'q")
    (should (equal (grid-get-raw-value-at source 0 1) "''q"))
    (should (equal (grid-get-display-value-at source 0 1) "'q"))
    (grid-table-enter-value source 0 1 "12")
    ;; The formats are saved with the grid and come back.
    (let ((file (make-temp-file "grid-format-" nil ".grid")))
      (unwind-protect
          (progn
            (grid-table-persistence--save-to-file source file)
            (let ((back (grid-table-persistence-create-data-source-from-file
                         file)))
              (should (equal (grid-get-display-value-at back 1 0) "2026-10-13"))
              (should (equal (grid-get-display-value-at back 2 0) "50.0%"))
              (should (equal (grid-get-display-value-at back 3 0) "007"))
              (should (equal (grid-get-computed-value-at back 0 0) 0))
              (should (equal (grid-get-computed-value-at back 3 1) 8))))
        (delete-file file)))
    ;; A source that keeps no styles stores what was typed.
    (let ((plain (copy-hash-table source)))
      (remhash :set-style-at plain)
      (should (equal (grid-table-read-entry plain 1 1 "2026-10-13")
                     '("2026-10-13" . keep))))))

(ert-deftest grid-table-date-functions-work-on-serial-numbers ()
  (let* ((source (make-default-data-source
                  '("" "") '(("46307" "2026-03-05") ("" ""))))
         (value (lambda (formula)
                  (grid-set-raw-value-at source 1 0 formula)
                  (grid-get-computed-value-at source 1 0)))
         (shown (lambda (formula)
                  (grid-set-raw-value-at source 1 0 formula)
                  (grid-get-display-value-at source 1 0))))
    (should (equal (funcall value "=DATE(2026,10,12)") 46307))
    (should (equal (funcall value "=DATE(2026,10,12)+1") 46308))
    (should (equal (funcall value "=TODAY()+1") (1+ (grid-format-today))))
    ;; A formula that starts by computing a date is shown as one.
    (should (equal (funcall shown "=DATE(2026,10,12)+1") "2026-10-13"))
    (should (equal (funcall shown "=EOMONTH(A1,1)") "2026-11-30"))
    (should (equal (funcall shown "=EDATE(DATE(2026,1,31),1)") "2026-02-28"))
    (should (equal (funcall value "=YEAR(A1)") 2026))
    (should (equal (funcall value "=MONTH(A1)") 10))
    (should (equal (funcall value "=DAY(A1)") 12))
    (should (equal (funcall value "=WEEKDAY(A1)") 2))
    ;; Dates kept as text in older sheets are still read.
    (should (equal (funcall value "=MONTH(B1)") 3))
    (should (equal (funcall value "=DATEVALUE(B1)")
                   (grid-format-date-to-serial 2026 3 5)))
    (should (equal (funcall value "=DAYS(A1,B1)") 221))
    (should (equal (funcall value "=TEXT(A1,\"yyyy/mm/dd\")") "2026/10/12"))
    (should (equal (funcall value "=TEXT(0.256,\"0.0%\")") "25.6%"))
    (should (equal (funcall value "=VALUE(\"12.5%\")") 0.125))))

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

(ert-deftest grid-table-sort-moves-references-naming-the-sorted-sheet ()
  (let* ((workbook (grid-workbook--create))
         (orders (make-default-data-source
                  '("" "" "" "")
                  '(("Key" "Amount" "Double" "Rated")
                    ("b" "10" "=Orders!B2*2" "=B2*Rates!B2")
                    ("a" "20" "='orders'!B3*2" "=B3*Rates!B3"))))
         (rates (make-default-data-source
                 '("" "") '(("Key" "Rate") ("b" "0.9") ("a" "0.5")))))
    (grid-workbook-add-sheet workbook orders "Orders")
    (grid-workbook-add-sheet workbook rates "Rates")
    (grid-workbook-recalculate workbook)
    (funcall (gethash :sort-by-column orders) orders 0 'ascending)
    ;; A reference naming the sheet itself follows its row, where a
    ;; spreadsheet that leaves it behind reads another row's amount.
    (should (equal (grid-get-raw-value-at orders 1 2) "='orders'!B2*2"))
    (should (equal (grid-get-raw-value-at orders 2 2) "=Orders!B3*2"))
    (should (equal (grid-get-computed-value-at orders 1 2) 40))
    (should (equal (grid-get-computed-value-at orders 2 2) 20))
    ;; The other sheet did not move, so references into it stay.
    (should (equal (grid-get-raw-value-at orders 1 3) "=B2*Rates!B3"))
    (should (equal (grid-get-computed-value-at orders 1 3) 10))
    (should (equal (grid-get-computed-value-at orders 2 3) 9))))

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

;; Found by an independent review: results that looked right and were not.
(ert-deftest grid-table-formula-large-numbers-round-to-themselves ()
  (should (equal (grid-format-display 5000000000 '(currency 2 "$"))
                 "$5,000,000,000.00"))
  (should (= (grid-format-round 1000000000000) 1000000000000))
  (should (= (grid-format-round 1e15) 1000000000000000))
  (should (equal (grid-format-edit-text "0.5" '(percent 0)) "50%"))
  ;; A product a hair short of a half still rounds as the half.
  (should (= (grid-format-round (* 1.005 100)) 101))
  (should (= (grid-format-round -2.5) -3))
  (should (equal (grid-table-formula-test--values
                  '(("=ROUND(1234567890.125,2)" "=ROUND(2.5,0)" "=INT(-1.5)"
                     "=INT(1.5)")))
                 '((1234567890.13 3 -2 1)))))

(ert-deftest grid-table-formula-index-stays-inside-its-range ()
  (should (equal (grid-table-formula-test--values
                  '(("1" "2" "3" "=INDEX(A1:A2,3)" "=INDEX(A1:C1,2)")
                    ("4" "5" "6" "=INDEX(A1:C3,2,3)" "=INDEX(A1:A3,3)")
                    ("7" "8" "9" "=INDEX(A1:C3,0,1)" "=INDEX(A1:C3,2,4)")))
                 '(("1" "2" "3" "#REF!" "2")
                   ("4" "5" "6" "6" "7")
                   ("7" "8" "9" "#REF!" "#REF!")))))

(ert-deftest grid-table-formula-match-compares-numbers-by-value ()
  (should (equal (grid-table-formula-test--values
                  '(("10" "20" "30" "=MATCH(20,A1:C1,0)" "=MATCH(\"B\",A2:C2,0)")
                    ("a" "b" "c" "=MATCH(99,A1:C1,0)" "")))
                 '(("10" "20" "30" 2 2)
                   ("a" "b" "c" "#N/A" "")))))

(ert-deftest grid-table-formula-text-is-no-number-in-arithmetic ()
  (should (equal (grid-table-formula-test--eval "=\"abc\"*2") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=B1*2" "abc") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=-B1" "abc") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=ROUND(B1,1)" "abc") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=IFERROR(B1*2,\"-\")" "abc")
                 "-"))
  ;; An empty cell is 0, text that reads as a number or a date is one.
  (should (equal (grid-table-formula-test--eval "=B1+1" "") 1))
  (should (equal (grid-table-formula-test--eval "=B1*2" "21") 42))
  (should (equal (grid-table-formula-test--eval "=\"3\"+4") 7))
  (should (equal (grid-table-formula-test--eval "=TRUE+1") 2))
  (should (equal (grid-table-formula-test--eval "=B1+1" "2026-10-13") 46309))
  ;; Aggregates still pass over text.
  (should (equal (grid-table-formula-test--eval "=SUM(B1:D1)" "1" "abc" "2") 3))
  (should (equal (grid-table-formula-test--eval "=SQRT(-1)") "#NUM!")))

(ert-deftest grid-table-formula-vlookup-finds-the-row-not-above ()
  (let ((rows '(("=VLOOKUP(25,B1:C3,2)" "10" "low")
                ("=VLOOKUP(5,B1:C3,2)" "20" "mid")
                ("=VLOOKUP(99,B1:C3,2,TRUE)" "30" "high")
                ("=VLOOKUP(20,B1:C3,2,FALSE)" "" "")
                ("=VLOOKUP(25,B1:C3,2,FALSE)" "" "")
                ("=VLOOKUP(20,B1:C3,3)" "" ""))))
    (should (equal (mapcar #'car (grid-table-formula-test--values rows))
                   '("mid" "#N/A" "high" "mid" "#N/A" "#REF!")))))

(ert-deftest grid-table-formula-common-functions ()
  (should (equal (grid-table-formula-test--values
                  '(("=TRUNC(1.59)" "=TRUNC(-1.59,1)" "=FLOOR(7,2)"
                     "=CEILING(7,2)" "=MROUND(7,2)" "=FLOOR(0.35,0.1)"
                     "=CEILING(7.2)" "=FLOOR(1,0)")))
                 '((1 -1.5 6 8 8 0.3 8 "#DIV/0!"))))
  (should (equal (grid-table-formula-test--values
                  '(("3" "1" "x" "4" "=MEDIAN(A1:D1)" "=MEDIAN(A1:D1,10)")
                    ("2" "5" "7" "1" "=LARGE(A1:D1,2)" "=SMALL(A1:D1,1)")
                    ("=RANK(3,A1:D1)" "=RANK(3,A1:D1,1)" "=RANK(9,A1:D1)"
                     "=LARGE(A1:D1,5)" "=SUMPRODUCT(A1:D1,A2:D2)"
                     "=SUMPRODUCT(A1:B1,A2:D2)")))
                 '(("3" "1" "x" "4" 3 3.5)
                   ("2" "5" "7" "1" 3 1)
                   (2 2 "#N/A" "#NUM!" 15 "#VALUE!")))))

(ert-deftest grid-table-typing-a-number-into-a-percentage ()
  (let ((source (make-default-data-source '("" "") '(("" "")))))
    (grid-table-enter-value source 0 0 "12.5%")
    (grid-table-enter-value source 0 0 "50")
    (should (equal (grid-get-raw-value-at source 0 0) "0.5"))
    (should (equal (grid-get-display-value-at source 0 0) "50.0%"))
    ;; A cell with another format takes the number as typed.
    (grid-table-enter-value source 0 1 "50")
    (should (equal (grid-get-raw-value-at source 0 1) "50"))))

;;;----------------------------------------------------------------------
;;; Recalculating only what an edit reaches
;;;----------------------------------------------------------------------

(defun grid-table-formula-test--all-values (model)
  "Return every computed value of MODEL, row by row."
  (cl-loop for row below (grid-data-model-rows model) collect
           (cl-loop for col below (grid-data-model-cols model) collect
                    (grid-model-get-computed-value model row col))))

(defun grid-table-formula-test--fresh-values (model)
  "Return what MODEL's cells are worth when every one is computed anew."
  (grid-table-formula-test--all-values
   (grid-model-create (grid-data-model-rows model) (grid-data-model-cols model)
                      (grid-model-get-all-raw-values model)
                      nil (grid-model--same-styles model))))

(defun grid-table-formula-test--computed (thunk)
  "Return how many cells calling THUNK computes."
  (let* ((count 0)
         (spy (lambda (&rest _) (setq count (1+ count)))))
    (advice-add 'grid-calc--parsed :before spy)
    (advice-add 'grid-calc--typed-value :before spy)
    (unwind-protect (funcall thunk)
      (advice-remove 'grid-calc--parsed spy)
      (advice-remove 'grid-calc--typed-value spy))
    count))

(ert-deftest grid-table-edit-computes-only-what-it-reaches ()
  (let ((model (grid-model-create
                4 4 '(("1" "=A1*2" "=B1+1" "9")
                      ("2" "=SUM(A1:A3)" "=B2*10" "9")
                      ("3" "=D4" "=A:A" "9")
                      ("4" "" "" "9")))))
    ;; A cell nothing reads: it alone is computed.
    (should (= (grid-table-formula-test--computed
                (lambda () (grid-model-set-raw-value model 0 3 "5")))
               1))
    ;; D4 is read by B3 only.
    (should (= (grid-table-formula-test--computed
                (lambda () (grid-model-set-raw-value model 3 3 "6")))
               2))
    (should (equal (grid-model-get-computed-value model 2 1) "6"))
    ;; A1 reaches B1, C1, the sum, what reads the sum, and the column.
    (should (= (grid-table-formula-test--computed
                (lambda () (grid-model-set-raw-value model 0 0 "10")))
               6))
    (should (equal (grid-table-formula-test--all-values model)
                   (grid-table-formula-test--fresh-values model)))
    (should (equal (grid-model-get-computed-value model 1 2) 150))))

(ert-deftest grid-table-edit-follows-a-formula-that-reads-other-cells-now ()
  (let ((model (grid-model-create 2 3 '(("1" "2" "=A1+1") ("" "" "=C1*2")))))
    (grid-model-set-raw-value model 0 2 "=B1+1")
    (should (equal (grid-model-get-computed-value model 1 2) 6))
    ;; A1 is no longer read, B1 is.
    (should (= (grid-table-formula-test--computed
                (lambda () (grid-model-set-raw-value model 0 0 "50")))
               1))
    (grid-model-set-raw-value model 0 1 "7")
    (should (equal (grid-model-get-computed-value model 1 2) 16))
    ;; A formula replaced by a number is read like any number.
    (grid-model-set-raw-value model 0 2 "100")
    (should (equal (grid-model-get-computed-value model 1 2) 200))
    (grid-model-set-raw-value model 0 1 "8")
    (should (equal (grid-model-get-computed-value model 1 2) 200))))

(ert-deftest grid-table-edit-makes-and-breaks-cycles-and-errors ()
  (let ((model (grid-model-create 2 3 '(("1" "=A1+1" "=B1+1") ("=C1" "" "")))))
    (grid-model-set-raw-value model 0 0 "=C1")
    (should (equal (grid-table-formula-test--all-values model)
                   '(("#CYCLE!" "#CYCLE!" "#CYCLE!") ("#CYCLE!" "" ""))))
    (grid-model-set-raw-value model 0 0 "5")
    (should (equal (grid-table-formula-test--all-values model)
                   '(("5" 6 7) (7 "" ""))))
    (grid-model-set-raw-value model 0 0 "=1/0")
    (should (equal (grid-model-get-computed-value model 1 0) "#DIV/0!"))
    (grid-model-set-raw-value model 0 0 "2")
    (should (equal (grid-model-get-computed-value model 1 0) 4))))

(ert-deftest grid-table-edit-recomputes-what-nothing-announces ()
  (let ((calls 0))
    (puthash "TICK" (lambda (_model) (setq calls (1+ calls)))
             grid-calc-functions)
    (unwind-protect
        (let ((model (grid-model-create
                      2 3 '(("1" "=TICK()" "=B1+0")
                            ("=NOW()" "=A2+0" "x")))))
          ;; A function that may read anything, and a clock, are
          ;; computed at every recalculation, with what reads them.
          (let ((before calls))
            (grid-model-set-raw-value model 1 2 "y")
            (should (= calls (1+ before)))
            (should (equal (grid-model-get-computed-value model 0 2) calls)))
          (should (numberp (grid-model-get-computed-value model 1 1)))
          ;; The text format leaves a number that is there a number.
          (grid-model-set-raw-value model 0 0 "5")
          (grid-model-set-raw-value model 1 2 "=SUM(A1:A1)")
          (should (equal (grid-model-get-computed-value model 1 2) 5))
          (grid-model-set-style model 0 0 '(:format (text)))
          (grid-table-calc-evaluate model)
          (should (equal (grid-model-get-computed-value model 1 2) 5)))
      (remhash "TICK" grid-calc-functions))))

(ert-deftest grid-table-edit-reaches-other-sheets ()
  (let* ((workbook (grid-workbook--create))
         (data (make-default-data-source
                '("" "") '(("1" "=A1*2") ("2" "=Sums!A1"))))
         (sums (make-default-data-source
                '("" "") '(("=SUM(Data!A1:A2)" "=A1+1") ("=Data!B1" "k")))))
    (grid-workbook-add-sheet workbook data "Data")
    (grid-workbook-add-sheet workbook sums "Sums")
    (grid-workbook-recalculate workbook t)
    (should (equal (grid-get-computed-value-at sums 0 1) 4))
    (grid-set-raw-value-at data 0 0 "10")
    (should (equal (grid-get-computed-value-at sums 0 0) 12))
    (should (equal (grid-get-computed-value-at sums 0 1) 13))
    (should (equal (grid-get-computed-value-at sums 1 0) 20))
    (should (equal (grid-get-computed-value-at data 1 1) 12))
    ;; A row put in, which makes the model anew, then an edit.
    (funcall (gethash :add-row data) data 0)
    (should (equal (grid-get-raw-value-at sums 0 0) "=SUM(Data!A1:A3)"))
    (grid-set-raw-value-at data 1 0 "100")
    (should (equal (grid-get-computed-value-at sums 0 0) 112))
    ;; A sheet renamed, then an edit.
    (grid-workbook-rename-sheet workbook data "Raw")
    (grid-set-raw-value-at data 0 0 "1")
    (should (equal (grid-get-computed-value-at sums 0 0) 103))
    (should (equal (grid-get-computed-value-at sums 1 0) 2))))

(ert-deftest grid-table-edits-give-what-computing-everything-gives ()
  (let* ((rows 12) (cols 6)
         (formulas '("=A1+1" "=SUM(A1:C4)" "=B:B" "=SUM(2:3)" "=IF(A2>5,B3,C4)"
                     "=INDEX(A1:C5,2,2)" "=VLOOKUP(3,A1:B9,2)" "=A1&B1"
                     "=D5*2" "=E7+F8" "=AVERAGE(D1:F12)" "=MAX(A:A)"
                     "=ROUND(C3/7,2)" "=A12" "=F1" "=COUNT(A1:F12)"
                     "=XLOOKUP(2,A1:A9,B1:B9,0)" "=E1+E2+E3" "=\"x\"*2"))
         (pick (lambda ()
                 (pcase (random 4)
                   (0 (number-to-string (random 20)))
                   (1 (nth (random (length formulas)) formulas))
                   (2 "")
                   (_ (nth (random 3) '("a" "7" "2026-10-13"))))))
         model)
    (random "grid-table")
    (setq model (grid-model-create
                 rows cols
                 (cl-loop repeat rows collect
                          (cl-loop repeat cols collect (funcall pick)))))
    (dotimes (_ 300)
      (grid-model-set-raw-value model (random rows) (random cols)
                                (funcall pick))
      (should (equal (grid-table-formula-test--all-values model)
                     (grid-table-formula-test--fresh-values model))))
    ;; The same with the record of who reads what switched off.
    (let ((grid-calc-incremental nil))
      (grid-model-set-raw-value model 0 0 "3")
      (should (equal (grid-table-formula-test--all-values model)
                     (grid-table-formula-test--fresh-values model))))
    (grid-model-set-raw-value model 0 0 "4")
    (should (equal (grid-table-formula-test--all-values model)
                   (grid-table-formula-test--fresh-values model)))))

;; Formulas reading another sheet are computed when what they read changes,
;; and only then.
(ert-deftest grid-table-formula-graph-spans-sheets ()
  (let* ((one (make-default-data-source
               '("" "" "") '(("1" "2" "3") ("4" "5" "6") ("7" "8" "9"))))
         (two (make-default-data-source
               '("" "" "") '(("=S1!B2*2" "=SUM(S1!A:A)" "=A1+B1")
                             ("=S2!A1+1" "" "") ("" "" ""))))
         (workbook (grid-workbook-create (list (cons "S1" one) (cons "S2" two))))
         (model (lambda (source) (gethash :model source))))
    (should (equal (grid-get-computed-value-at two 0 0) 10))
    (should (equal (grid-get-computed-value-at two 0 1) 12))
    (should (equal (grid-get-computed-value-at two 1 0) 11))
    ;; Neither the reference nor the self-named one is computed every time.
    (should (consp (aref (grid-model-get-cell (funcall model two) 0 0)
                         grid-cell--reads)))
    (should (consp (aref (grid-model-get-cell (funcall model two) 1 0)
                         grid-cell--reads)))
    ;; An edit nothing on S2 reads computes S1's cell and no more.
    (should (= (grid-table-formula-test--computed
                (lambda () (grid-set-raw-value-at one 2 2 "90")))
               1))
    ;; An edit S2 reads reaches it, and what reads it there.
    (grid-set-raw-value-at one 1 1 "50")
    (should (equal (grid-get-computed-value-at two 0 0) 100))
    (should (equal (grid-get-computed-value-at two 0 2) 112))
    (grid-set-raw-value-at one 0 0 "100")
    (should (equal (grid-get-computed-value-at two 0 1) 111))
    ;; A formula that stops reading a cell is no longer reached from it.
    (grid-set-raw-value-at two 0 0 "=S1!C1")
    (should (equal (grid-get-computed-value-at two 0 0) "3"))
    (should (= (grid-table-formula-test--computed
                (lambda () (grid-set-raw-value-at one 1 1 "60")))
               1))
    ;; A cycle across the sheets is found.
    (grid-set-raw-value-at one 0 2 "=S2!A1")
    (should (equal (grid-get-computed-value-at one 0 2) "#CYCLE!"))
    (grid-set-raw-value-at two 0 0 "=S1!C2")
    (should (equal (grid-get-computed-value-at one 0 2) "6"))
    (ignore workbook)))

(ert-deftest grid-table-formula-graph-across-sheets-matches-full-recalc ()
  (let* ((rows 8) (cols 4)
         (formulas '("=A1+1" "=S1!B2*2" "=SUM(S2!A1:B4)" "=S2!C3" "=SUM(S1!A:A)"
                     "=s1!D4+A2" "=COUNT(S2!A:D)" "=B2&S1!C1" "=MAX(S1!A1:D8)"))
         (pick (lambda ()
                 (pcase (random 4)
                   (0 (number-to-string (random 20)))
                   (1 (nth (random (length formulas)) formulas))
                   (2 "")
                   (_ "x"))))
         (table (lambda ()
                  (cl-loop repeat rows collect
                           (cl-loop repeat cols collect (funcall pick)))))
         sources)
    (random "grid-table-sheets")
    (setq sources (list (make-default-data-source (make-list cols "") (funcall table))
                        (make-default-data-source (make-list cols "") (funcall table))))
    (grid-workbook-create (list (cons "S1" (nth 0 sources))
                                (cons "S2" (nth 1 sources))))
    (cl-flet ((values (source)
                (grid-table-formula-test--all-values (gethash :model source)))
              (fresh ()
                (let ((copies
                       (mapcar (lambda (source)
                                 (make-default-data-source
                                  (make-list cols "")
                                  (cl-loop for row below rows collect
                                           (cl-loop for col below cols collect
                                                    (grid-get-raw-value-at
                                                     source row col)))))
                               sources)))
                  (grid-workbook-create (list (cons "S1" (nth 0 copies))
                                              (cons "S2" (nth 1 copies))))
                  copies)))
      (dotimes (_ 200)
        (grid-set-raw-value-at (nth (random 2) sources) (random rows) (random cols)
                               (funcall pick))
        (let ((copies (fresh)))
          (should (equal (mapcar #'values sources) (mapcar #'values copies))))))))

(ert-deftest grid-table-formula-defined-names ()
  (let* ((one (make-default-data-source
               '("" "") '(("Item" "Qty") ("a" "2") ("b" "3") ("=SUM(Qty)*Rate" ""))))
         (two (make-default-data-source
               '("") '(("=INDEX(Qty,2)+VLOOKUP(\"b\",Table,2,FALSE)"))))
         (workbook (grid-workbook-create (list (cons "Sheet1" one)
                                               (cons "Two" two))))
         (file (make-temp-file "grid-names" nil ".grid")))
    (unwind-protect
        (progn
          (should (equal (grid-get-computed-value-at one 3 0) "#NAME?"))
          (grid-workbook-define-name workbook "Qty" "=Sheet1!$B$2:$B$3")
          (grid-workbook-define-name workbook "rate" "=Sheet1!$B$2")
          (grid-workbook-define-name workbook "Table" "=Sheet1!$A$2:$B$3")
          (should (equal (grid-get-computed-value-at one 3 0) 10))
          (should (equal (grid-get-computed-value-at two 0 0) 6))
          ;; An edit inside a name reaches the formulas using it.
          (grid-set-raw-value-at one 1 1 "10")
          (should (equal (grid-get-computed-value-at one 3 0) 130))
          ;; Names follow their cells when lines move, and undo with them.
          (let ((before (gethash :model one)))
            (funcall (gethash :add-row one) one 1)
            (should (equal (grid-workbook-name-definition workbook "QTY")
                           "=Sheet1!$B$2:$B$4"))
            (grid-workbook-model-restored one (gethash :model one) before)
            (puthash :model before one)
            (should (equal (grid-workbook-name-definition workbook "Qty")
                           "=Sheet1!$B$2:$B$3")))
          ;; Renaming the sheet renames it in the definitions.
          (grid-workbook-rename-sheet workbook one "Data")
          (should (equal (grid-workbook-name-definition workbook "Rate")
                         "=Data!$B$2"))
          (should (equal (grid-get-computed-value-at two 0 0) 6))
          ;; Saved and read back.
          (grid-table-persistence--save-to-file one file)
          (let* ((loaded (grid-table-persistence-create-data-source-from-file file))
                 (other (cadr (grid-workbook-sheets (grid-workbook-of loaded)))))
            (should (equal (grid-workbook-names (grid-workbook-of loaded))
                           (grid-workbook-names workbook)))
            (should (equal (grid-get-computed-value-at other 0 0) 6)))
          ;; A name can only be one that no reference or function takes.
          (should (grid-calc-name-problem "TAX1"))
          (should (grid-calc-name-problem "Q1Sales"))
          (should (grid-calc-name-problem "Sum"))
          (should (grid-calc-name-problem "TRUE"))
          (should-not (grid-calc-name-problem "Sales_2024"))
          (grid-workbook-delete-name workbook "rate")
          (should (equal (grid-get-computed-value-at one 3 0) "#NAME?")))
      (delete-file file))))

(ert-deftest grid-table-formula-names-on-a-lone-sheet-are-saved ()
  (let* ((source (make-default-data-source
                  '("" "") '(("x" "4") ("y" "=Four*2"))))
         (workbook (grid-workbook-ensure source))
         (file (make-temp-file "grid-lone-names" nil ".grid")))
    (unwind-protect
        (progn
          (grid-workbook-define-name
           workbook "Four"
           (concat "=" (grid-calc-sheet-prefix (grid-workbook-sheet-name source))
                   "$B$1"))
          (should (equal (grid-get-computed-value-at source 1 1) 8))
          (grid-table-persistence--save-to-file source file)
          (let ((loaded (grid-table-persistence-create-data-source-from-file file)))
            (should (equal (grid-get-computed-value-at loaded 1 1) 8))))
      (delete-file file))))

(ert-deftest grid-table-validations-follow-lines-and-files ()
  (let* ((source (make-default-data-source
                  '("" "" "")
                  '(("Size" "Pick" "Sizes") ("" "" "S") ("" "" "M") ("" "" "L"))))
         (file (make-temp-file "grid-valid" nil ".grid")))
    (unwind-protect
        (progn
          (grid-table-set-validations
           source '((1 0 3 0 list ("Yes" "No")) (1 1 3 1 list "=$C$2:$C$4")))
          (should (equal (grid-table-validation-options
                          source (grid-table-validation-at source 2 1))
                         '("S" "M" "L")))
          (should (grid-table-validation-problem source 1 0 "maybe"))
          (should-not (grid-table-validation-problem source 1 0 "yes"))
          (should-not (grid-table-validation-problem source 1 1 "M"))
          (should-not (grid-table-validation-problem source 0 0 "anything"))
          ;; A row inserted above moves the ranges, and the list's cells.
          (funcall (gethash :add-row source) source 0)
          (should (equal (grid-table-validations source)
                         '((2 0 4 0 list ("Yes" "No")) (2 1 4 1 list "=$C$3:$C$5"))))
          (should (equal (grid-table-validation-options
                          source (grid-table-validation-at source 3 1))
                         '("S" "M" "L")))
          ;; Sorting leaves them where they are.
          (funcall (gethash :sort-by-column source) source 2 'ascending)
          (should (equal (car (grid-table-validations source))
                         '(2 0 4 0 list ("Yes" "No"))))
          (grid-table-persistence--save-to-file source file)
          (should (equal (grid-table-validations
                          (grid-table-persistence-create-data-source-from-file file))
                         (grid-table-validations source))))
      (delete-file file))))

;; The text table needs a data source and nothing else.
(ert-deftest grid-table-text-render-needs-no-buffer ()
  (let* ((source (make-default-data-source
                  '("" "" "")
                  '(("Item" "Qty" "Total") ("pear" "2" "=B2*3")
                    ("wide" "" ""))))
         (text (progn
                 (grid-merge-cells source 2 0 2 2)
                 (with-temp-buffer
                   (let ((grid-table--data-source nil))
                     (grid-table-text-render source))))))
    (should (equal (split-string (substring-no-properties text) "\n" t)
                   '("┌───┬───────┬───────┬───────┐"
                     "│   │ A     │ B     │ C     │"
                     "├───┼───────┼───────┼───────┤"
                     "│1  │ Item  │ Qty   │ Total │"
                     "├───┼───────┼───────┼───────┤"
                     "│2  │ pear  │ 2     │ 6     │"
                     "├───┼───────┴───────┴───────┤"
                     "│3  │ wide                  │"
                     "└───┴───────────────────────┘")))))

(ert-deftest grid-table-dates-written-month-first ()
  (should (= (grid-format-parse-date "10/13/2026") 46308))
  (should (= (grid-format-parse-date "3/4/2026")
             (grid-format-parse-date "2026-03-04")))
  (should (= (grid-format-parse-date "10/13/2026 18:30")
             (grid-format-parse-date "2026-10-13 18:30")))
  ;; There is no month 13: day first is not guessed.
  (should-not (grid-format-parse-date "13/10/2026"))
  (should-not (grid-format-parse-date "2/30/2026"))
  (should-not (grid-format-parse-date "10/13/26"))
  (let ((source (make-default-data-source '("" "") '(("" "=A1+1")))))
    (grid-table-enter-value source 0 0 "3/4/2026")
    (should (equal (grid-get-raw-value-at source 0 0) "46085"))
    (should (equal (grid-get-display-value-at source 0 0) "03/04/2026"))
    (should (equal (grid-get-computed-value-at source 0 1) 46086))))

(ert-deftest grid-table-formula-errors-and-names-read-as-in-excel ()
  ;; An error written in a formula, as a deleted reference leaves it,
  ;; is that error; a word that calls nothing is an unknown name.
  (should (equal (grid-table-formula-test--eval "=#REF!+1") "#REF!"))
  (should (equal (grid-table-formula-test--eval "=IFERROR(#N/A,7)") 7))
  (should (equal (grid-table-formula-test--eval "=IFERROR(Sheet2!#REF!,7)") 7))
  (should (equal (grid-table-formula-test--eval "=\"#REF!\"") "#REF!"))
  (should (equal (grid-table-formula-test--eval "=Rate+1") "#NAME?"))
  (let* ((model (grid-model-create 3 2 '(("1" "=A3+A2") ("2" "") ("3" ""))))
         (model (grid-model-delete-row model 1)))
    (should (equal (grid-model-get-computed-value model 0 1) "#REF!"))))

(ert-deftest grid-table-formula-criteria-take-wildcards ()
  (let ((cells '("apple" "b" "avocado" "10" "" "*" "12")))
    (cl-flet ((count (criteria)
                (apply #'grid-table-formula-test--eval
                       (format "=COUNTIF(B1:H1,%s)" criteria) cells)))
      (should (= (count "\"a*\"") 2))
      (should (= (count "\"A?PLE\"") 1))
      (should (= (count "\"<>a*\"") 5))
      (should (= (count "\"~*\"") 1))
      (should (= (count "\">=10\"") 2))
      (should (= (count "10") 1))
      (should (= (count "\"\"") 1))
      (should (= (count "\"<>\"") 6)))))

(ert-deftest grid-table-sort-keeps-frozen-rows-in-place ()
  (let ((model (grid-model-create 4 1 '(("Title") ("Header") ("b") ("a")))))
    (grid-model-set-frozen model 2 0)
    (let ((sorted (grid-model-sort-by-column model 0 'ascending t)))
      (should (equal (grid-model-get-all-raw-values sorted)
                     '(("Title") ("Header") ("a") ("b")))))))

(ert-deftest grid-table-chart-over-no-data-is-empty ()
  (require 'grid-table-chart)
  (should (equal (grid-chart-columnplot nil nil nil) ""))
  (should (stringp (grid-chart--lineplot-render nil nil nil))))

(ert-deftest grid-table-hidden-lines-follow-structure-and-files ()
  (let* ((source (make-default-data-source
                  '("K" "V" "W")
                  '(("K" "V" "W") ("c" "1" "x") ("a" "2" "y") ("b" "3" "z"))))
         (file (make-temp-file "grid-hidden" nil ".grid")))
    (unwind-protect
        (progn
          ;; Row 1 (c) and column W are hidden.
          (grid-table-set-hidden source '(1) '(2))
          (should (equal (grid-table-hidden source) '((1) 2)))
          ;; A row inserted above pushes the hidden row down.
          (funcall (gethash :add-row source) source 0)
          (should (equal (car (grid-table-hidden source)) '(2)))
          ;; The hidden row travels with a sort.
          (funcall (gethash :sort-by-column source) source 0 'ascending)
          (let ((row (car (car (grid-table-hidden source)))))
            (should (equal (grid-get-raw-value-at source row 0) "c")))
          ;; Deleting the hidden column forgets it.
          (funcall (gethash :delete-column source) source 2)
          (should (equal (cdr (grid-table-hidden source)) nil))
          (grid-table-set-hidden source (car (grid-table-hidden source)) '(1))
          (grid-table-persistence--save-to-file source file)
          (let ((loaded (grid-table-persistence-create-data-source-from-file
                         file)))
            (should (equal (grid-table-hidden loaded)
                           (grid-table-hidden source)))))
      (delete-file file))))

(ert-deftest grid-table-chart-data-reads-a-range-like-a-spreadsheet ()
  "A first row and column of text name the series and the categories."
  (let* ((cells [["" "Q1" "Q2" "Q3"] ["North" 1 2 3] ["South" 4 5 6]])
         (value (lambda (r c) (aref (aref cells r) c)))
         (label (lambda (r c) (format "%s" (aref (aref cells r) c)))))
    ;; Wider than tall: the series run along the rows.
    (let ((data (grid-chart-svg-data '(0 0 2 3) value label)))
      (should (eq (plist-get data :by) 'rows))
      (should (equal (plist-get data :categories) '("Q1" "Q2" "Q3")))
      (should (equal (plist-get data :series) '(("North" 1 2 3) ("South" 4 5 6))))
      (should (equal (plist-get data :ranges) '((1 1 1 3) (2 1 2 3))))
      (should (equal (plist-get data :category-range) '(0 1 0 3))))
    ;; Switched, the columns are the series.
    (let ((data (grid-chart-svg-data '(0 0 2 3) value label 'columns)))
      (should (equal (plist-get data :categories) '("North" "South")))
      (should (equal (plist-get data :series) '(("Q1" 1 4) ("Q2" 2 5) ("Q3" 3 6))))
      (should (equal (plist-get data :ranges) '((1 1 2 1) (1 2 2 2) (1 3 2 3))))
      (should (equal (plist-get data :category-range) '(1 0 2 0))))
    ;; Numbers alone give numbered categories and series.
    (let ((data (grid-chart-svg-data '(1 1 2 2) value label)))
      (should (equal (plist-get data :categories) '("1" "2")))
      (should (equal (plist-get data :series) '(("Series 1" 1 4) ("Series 2" 2 5))))
      (should-not (plist-get data :category-range)))
    (should (equal (grid-chart-svg-parse-range "$B$2:A1") '(0 0 1 1)))
    (should-not (grid-chart-svg-parse-range "Sheet2!A1:B2"))
    ;; Every type draws, and a view shows only part of the drawing.
    (dolist (type (mapcar #'car grid-chart-svg-types))
      (should (string-prefix-p
               "<svg" (grid-chart-svg-render
                       (list :type type :title "T")
                       (grid-chart-svg-data '(0 0 2 3) value label) 400 300))))
    (should (string-match-p
             "viewBox=\"10 20 100 50\""
             (grid-chart-svg-render '(:type column) '(:series nil) 400 300
                                    nil nil '(10 20 100 50))))))

(ert-deftest grid-table-charts-follow-their-cells ()
  "A chart's anchor and range move with the lines and stay in the file."
  (let* ((source (make-default-data-source
                  '("K" "V" "W")
                  '(("K" "V" "W") ("a" "1" "2") ("b" "3" "4") ("c" "5" "6"))))
         (file (make-temp-file "grid-chart" nil ".grid"))
         (chart (lambda () (car (grid-table-charts source)))))
    (unwind-protect
        (progn
          (grid-table-set-charts
           source (list (list :anchor '(1 . 2) :size '(30 . 10) :type 'column
                              :range "A1:C4" :by nil :title "T")))
          ;; A row added above the anchor pushes it and the range down.
          (funcall (gethash :add-row source) source 0)
          (should (equal (plist-get (funcall chart) :anchor) '(2 . 2)))
          (should (equal (plist-get (funcall chart) :range) "A1:C5"))
          (funcall (gethash :add-column source) source 0)
          (should (equal (plist-get (funcall chart) :anchor) '(2 . 3)))
          (should (equal (plist-get (funcall chart) :range) "B1:D5"))
          ;; Deleting the anchor's row leaves the chart where it was.
          (funcall (gethash :delete-row source) source 1)
          (should (equal (plist-get (funcall chart) :anchor) '(2 . 3)))
          (should (equal (plist-get (funcall chart) :range) "B1:D4"))
          ;; A sort leaves charts in place.
          (funcall (gethash :sort-by-column source) source 1 'descending)
          (should (equal (plist-get (funcall chart) :anchor) '(2 . 3)))
          (grid-table-persistence--save-to-file source file)
          (should (equal (grid-table-charts
                          (grid-table-persistence-create-data-source-from-file
                           file))
                         (grid-table-charts source)))
          ;; A malformed chart in a file is left out.
          (with-temp-file file
            (prin1 '(grid-table-file :version 4 :headers ("A") :rows (("1"))
                                     :charts ((:anchor (0 . 0) :size (9 . 9)
                                               :type nonsense :range "A1")
                                              (:anchor (0 . 0) :size (9 . 9)
                                               :type pie :range "A1")))
                   (current-buffer)))
          (should (equal (mapcar (lambda (c) (plist-get c :type))
                                 (grid-table-charts
                                  (grid-table-persistence-create-data-source-from-file
                                   file)))
                         '(pie))))
      (delete-file file))))

(provide 'grid-table-formula-test)

;;; grid-table-formula-test.el ends here
