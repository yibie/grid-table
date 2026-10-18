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

(defun grid-table-formula-test--grid (formula rows cols)
  "Return a ROWS by COLS sheet of raw values with FORMULA in its first cell."
  (cons (cons formula (make-list (1- cols) ""))
        (make-list (1- rows) (make-list cols ""))))

(defun grid-table-formula-test--should-be (case)
  "Check that the formula in CASE gives the value it is paired with."
  (let* ((model (grid-model-create 1 3 (list (list (car case) "" ""))))
         (value (grid-model-get-computed-value model 0 0))
         (want (cdr case)))
    (should (if (numberp want)
                (and (numberp value)
                     (< (abs (- value want)) (* 1e-9 (max 1 (abs want)))))
              (and (stringp value) (equal value want))))))

(ert-deftest grid-table-formula-offset-takes-empty-arguments-as-excel-does ()
  ;; An empty height or width keeps the size of the reference; an
  ;; empty row or column offset is 0.
  (dolist (case '(("=SUM(OFFSET(B1:D1,0,0,))" . 7)
                  ("=SUM(OFFSET(B1:D1,0,0,,2))" . 3)
                  ("=SUM(OFFSET(B1:D1,,1))" . 6)
                  ("=SUM(OFFSET(B1:D1,0,0,1,))" . 7)))
    (should (equal (grid-table-formula-test--eval (car case) "1" "2" "4")
                   (cdr case)))))

(ert-deftest grid-table-formula-subtotal-counts-error-values ()
  ;; As COUNT and COUNTA do; the other functions give the error.
  (dolist (case '(("=SUBTOTAL(3,B1:D1)" . 3)
                  ("=SUBTOTAL(2,B1:D1)" . 1)
                  ("=SUBTOTAL(103,B1:D1)" . 3)
                  ("=SUBTOTAL(9,B1:D1)" . "#DIV/0!")
                  ("=AGGREGATE(3,4,B1:D1)" . 3)))
    (should (equal (grid-table-formula-test--eval (car case) "1" "=1/0" "x")
                   (cdr case)))))

(ert-deftest grid-table-formula-sums-and-means-keep-every-digit ()
  ;; Excel keeps a whole double, only showing 15 digits: a formula
  ;; that reads the sum or the mean sees all of it.
  (should (equal (grid-table-formula-test--eval "=SUM(0.1,0.2)") 0.30000000000000004))
  (should (equal (grid-table-formula-test--eval "=AVERAGE(1,2,2)*3") 5))
  (should (eq (grid-table-formula-test--eval "=SUM(0.1,0.2)=0.3") t))
  (should (equal (grid-table-formula-test--eval "=SUM(0.1,0.2)&\"\"") "0.3")))

(ert-deftest grid-table-formula-unions-hold-every-area ()
  ;; A union in a union adds its areas, duplicates and all; the range
  ;; and intersection operators take in each area of a union.
  (dolist (case '(("=SUM(((B1,C1),C1))" . 5)
                  ("=SUM((B1,(B1,C1)))" . 4)
                  ("=SUM(B1:(C1,D1))" . 7)
                  ("=SUM((B1,C1):D1)" . 7)
                  ("=SUM((B1:C1,D1) C1:D1)" . 6)
                  ("=SUM(B1 (C1,D1))" . "#NULL!")
                  ("=SUM((B1,C1%))" . "#VALUE!")
                  ("=COUNT((B1,C1%))" . 0)
                  ("=AREAS((B1,(C1,D1)))" . 3)
                  ("=RANK(B1,(B1,C1,D1),1)" . 1)
                  ("=RANK.AVG(C1,(B1:C1,C1:D1))" . 2.5)
                  ("=SUM((B1,IFS(TRUE,C1)))" . 3)
                  ("=SUM((B1,SWITCH(2,1,C1,D1)))" . 5)))
    (should (equal (grid-table-formula-test--eval (car case) "1" "2" "4")
                   (cdr case)))))

(ert-deftest grid-table-formula-operators-follow-spreadsheet-rules ()
  (should (equal (grid-table-formula-test--eval "=2^3") 8))
  (should (equal (grid-table-formula-test--eval "=(-2)^3") -8))
  (should (equal (grid-table-formula-test--eval "=(-4)^-1") -0.25))
  (should (equal (grid-table-formula-test--eval "=(-8)^(1/3)") "#NUM!"))
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
      ;; Several values spill over the cells below and to the right;
      ;; here the data is in the way.
      (should (equal (value "=UNIQUE(A2:B6)") "#SPILL!"))
      (should (equal (value "=B2:B4") "#SPILL!"))
      (should (equal (value "=B3:B3") 20))
      (should (equal (value "=UNIQUE(A2:A2)") "x")))))

(ert-deftest grid-table-formula-multi-branch-functions-return ()
  (should (equal (grid-table-formula-test--eval
                  "=IFS(B1>5,\"big\",B1>1,\"mid\",TRUE,\"small\")" "3")
                 "mid"))
  (should (equal (car (car (grid-table-formula-test--values
                            '(("=VLOOKUP(\"b\",B1:C2,2,FALSE)" "a" "1")
                              ("" "b" "2")))))
                 2))
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
  ;; LibreOffice quotes each end of a reference through sheets.
  (should (equal (grid-parser-parse "'a b':'it''s'!A1")
                 '(CELL-REF "A1" "a b:it's")))
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

(ert-deftest grid-table-formula-cell-names-the-file-and-sheet ()
  ;; CELL("filename") names a workbook's file and sheet once it has a
  ;; file, as Excel does once a workbook is saved; !A1 is A1 on the
  ;; sheet the formula is worked out on, as a name may say.
  (should (equal (grid-parser-parse "CELL(\"filename\",!A1)")
                 '(FUNCTION-CALL "CELL" ((STRING "filename") (CELL-REF "A1")))))
  (let* ((one (make-default-data-source '("") '(("=CELL(\"filename\",A1)"))))
         (two (make-default-data-source
               '("") '(("=MID(CELL(\"filename\"),FIND(\"]\",CELL(\"filename\"))+1,99)"))))
         (workbook (grid-workbook-create (list (cons "One" one) (cons "Two" two)))))
    (should (equal (grid-get-computed-value-at one 0 0) ""))
    (setf (grid-workbook-file workbook) "/data/Book.xlsx")
    (grid-workbook-recalculate workbook t)
    (should (equal (grid-get-computed-value-at one 0 0) "/data/[Book.xlsx]One"))
    (should (equal (grid-get-computed-value-at two 0 0) "Two"))))

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

(ert-deftest grid-table-workbook-reference-through-sheets ()
  (cl-flet ((month (name value)
              (make-default-data-source
               (list "n" "v") `(("n" "v") (,value "x")) name)))
    (let* ((jan (month "Jan" "1"))
           (workbook (grid-workbook-ensure jan))
           (feb (grid-workbook-add-sheet workbook (month "Feb" "2") "Feb"))
           (mar (grid-workbook-add-sheet workbook (month "Mar" "3") "Mar"))
           (total (grid-workbook-new-sheet workbook "Total")))
      (grid-workbook-recalculate workbook t)
      (grid-set-raw-value-at total 0 0 "=SUM(Jan:Mar!A2)")
      (grid-set-raw-value-at total 0 1 "=COUNTA(Mar:Jan!A2:B2)")
      (grid-set-raw-value-at total 0 2 "=SUM(Jan:Nope!A2)")
      (grid-set-raw-value-at total 0 3 "=MAX('Feb:Mar'!A2)")
      (should (equal (grid-get-computed-value-at total 0 0) 6))
      (should (equal (grid-get-computed-value-at total 0 1) 6))
      (should (equal (grid-get-computed-value-at total 0 2) "#REF!"))
      (should (equal (grid-get-computed-value-at total 0 3) 3))
      ;; An edit to a sheet in between reaches the total.
      (grid-set-raw-value-at feb 1 0 "20")
      (should (equal (grid-get-computed-value-at total 0 0) 24))
      ;; A sheet moved in between is taken in.
      (let ((apr (grid-workbook-add-sheet workbook (month "Apr" "100") "Apr")))
        (grid-workbook-recalculate workbook t)
        (should (equal (grid-get-computed-value-at total 0 0) 24))
        (grid-workbook-move-sheet workbook apr -2)
        (should (equal (grid-workbook-sheet-names workbook)
                       '("Jan" "Feb" "Apr" "Mar" "Total")))
        (should (equal (grid-get-computed-value-at total 0 0) 124)))
      ;; Taking out an end draws the span in; renaming one follows it.
      (grid-workbook-remove-sheet workbook mar)
      (should (equal (grid-get-raw-value-at total 0 0) "=SUM(Jan:Apr!A2)"))
      (should (equal (grid-get-raw-value-at total 0 3) "=MAX(Feb:Apr!A2)"))
      (should (equal (grid-get-computed-value-at total 0 0) 121))
      (grid-workbook-rename-sheet workbook jan "Q1 Start")
      (should (equal (grid-get-raw-value-at total 0 0)
                     "=SUM('Q1 Start:Apr'!A2)"))
      (should (equal (grid-get-computed-value-at total 0 0) 121))))
  ;; A sheet named like a cell goes in apostrophes, so A1:Sheet2!B3 is
  ;; a range from A1.
  (should (equal (grid-parser-parse "SUM(A1:Sheet2!B3)")
                 '(FUNCTION-CALL "SUM" ((RANGE-OP (CELL-REF "A1")
                                                  (CELL-REF "B3" "Sheet2"))))))
  (should (equal (grid-calc-sheet-prefix "S1:S3") "'S1:S3'!")))

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
    (should (equal (funcall value "=XLOOKUP(4,A:A,C:C)") 6))
    (should (equal (funcall value "=XLOOKUP(9,A1:A2,B1:B2,\"none\")") "none"))
    (should (equal (funcall value "=XLOOKUP(9,A1:A2,B1:B2)") "#N/A"))
    (should (equal (funcall value "=XLOOKUP(1,A1:A2,B1:B2,1/0)") 2))))

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
    (should (equal (funcall value "=VALUE(\"12.5%\")") 0.125))
    ;; Arithmetic reads a number with spaces around it, but not a date;
    ;; DATEVALUE reads the date all the same.
    (should (equal (funcall value "=\" 10 \"+1") 11))
    (should (equal (funcall value "=\" 2024-01-10 \"+1") "#VALUE!"))
    (should (equal (funcall value "=\"2024-01-10\"+1") 45302))
    (should (equal (funcall value "=DATEVALUE(\" 2024-01-10 \")") 45301))
    ;; Text such as 1/2/2024 names the month first, or the day first
    ;; where the region writes it so.
    (should (equal (funcall value "=DATEVALUE(\"1/2/2024\")") 45293))
    (should (equal (funcall value "=DATEVALUE(\"15/1/2024\")") "#VALUE!"))
    (let ((grid-calc-date-order 'day-month-year))
      (should (equal (funcall value "=DATEVALUE(\"1/2/2024\")") 45323))
      (should (equal (funcall value "=DATEVALUE(\"15/1/2024\")") 45306))
      (should (equal (funcall value "=DATEVALUE(\"29/02/1900\")") 60))
      (should (equal (funcall value "=DATEVALUE(\"2024-01-15\")") 45306)))
    ;; CHAR reads codes past 127 as Windows-1252 does.
    (should (equal (funcall value "=CHAR(65)") "A"))
    (should (equal (funcall value "=CHAR(128)") "€"))
    (should (equal (funcall value "=CHAR(200)") "È"))
    (should (equal (funcall value "=CHAR(256)") "#VALUE!"))
    (should (equal (funcall value "=CODE(CHAR(128))") 128))
    (should (equal (funcall value "=CODE(\"È\")") 200))
    ;; The first argument that fails gives the error.
    (should (equal (funcall value "=FLOOR(\"x\",1/0)") "#VALUE!"))
    (should (equal (funcall value "=FLOOR(1/0,\"x\")") "#DIV/0!"))))

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
                 ;; A whole column of INDEX is several values, which
                 ;; would spill past the bottom of the sheet.
                 '(("1" "2" "3" "#REF!" 2)
                   ("4" "5" "6" 6 7)
                   ("7" "8" "9" "#SPILL!" "#REF!")))))

(ert-deftest grid-table-formula-index-below-1-is-value ()
  (should (equal (grid-table-formula-test--eval "=INDEX({1,2;3,4},-1)") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=INDEX({1,2;3,4},1,-1)") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=INDEX({1,2;3,4},-1,9)") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=INDEX({1,2;3,4},1,9)") "#REF!")))

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

(ert-deftest grid-table-formula-sorted-lookup-takes-a-blank-for-0 ()
  ;; A3 is blank: sought among sorted values it is 0, matched exactly
  ;; it is nothing.  An index past the table is looked at only when the
  ;; value is found.
  (should (equal (car (grid-table-formula-test--values
                       '(("0" "a" "=VLOOKUP(A3,A1:B2,2)" "=MATCH(A3,A1:A2)"
                          "=LOOKUP(A3,A1:A2,B1:B2)" "=MATCH(A3,A1:A2,0)"
                          "=VLOOKUP(A3,B1:B2,0)")
                         ("1" "b" "" "" "" "" "")
                         ("" "" "" "" "" "" ""))))
                 '("0" "a" "a" 1 "a" "#N/A" "#N/A"))))

(ert-deftest grid-table-formula-match-down-stops-at-an-equal-value ()
  ;; Values read from the first, the search stops at the first below
  ;; WANTED or after the run of those equal to it.
  (should (equal (grid-table-formula-test--eval "=MATCH(0,{0,1,2,3,4},-1)") 1))
  (should (equal (grid-table-formula-test--eval "=MATCH(-1,{0,1,2,3,4},-1)") 5))
  (should (equal (grid-table-formula-test--eval "=MATCH(2,{0,1,2,3,4},-1)") "#N/A"))
  (should (equal (grid-table-formula-test--eval "=MATCH(3,{5,3,3,1},-1)") 3)))

(ert-deftest grid-table-formula-lookup-reads-a-table-for-its-results ()
  ;; Values of several columns are looked along the first; results of
  ;; several rows and columns are #N/A.
  (should (equal (grid-table-formula-test--eval
                  "=LOOKUP(3,{1,\"a\";3,\"c\";5,\"e\"},{10;30;50})")
                 30))
  (should (equal (grid-table-formula-test--eval
                  "=LOOKUP(3,{1,\"a\";3,\"c\";5,\"e\"},{10,1;30,3;50,5})")
                 "#N/A")))

(ert-deftest grid-table-formula-sorted-lookup-halves-as-excel-does ()
  ;; In values not sorted Excel's search by halves lands where a search
  ;; from the start does not; these are what Excel gave.
  (should (equal (grid-table-formula-test--eval
                  "=LOOKUP(\"DOUBLE\",{\"DOUBLE\",\"BLANK\",\"STRING\",\"REF\",\"AREA\"},{1,2,3,4,5})")
                 1))
  (should (equal (car (nth 2 (grid-table-formula-test--values
                              '(("INTEGRAL" "DOUBLE" "BLANK" "STRING" "REF" "AREA" "")
                                ("1" "2" "3" "4" "5" "6" "")
                                ("=HLOOKUP(\"STRING\",A1:G2,2,TRUE)" "" "" "" "" "" "")))))
                 4))
  ;; Of equal values the last is taken; none before the first is #N/A.
  (should (equal (grid-table-formula-test--eval "=MATCH(5,{1,5,5,5,9},1)") 4))
  (should (equal (grid-table-formula-test--eval "=MATCH(0,{1,5,9},1)") "#N/A"))
  ;; Blanks below the values are passed over.
  (should (equal (nth 2 (grid-table-formula-test--values
                         '(("1" "3" "5" "" "")
                           ("a" "b" "c" "" "")
                           ("=HLOOKUP(4,A1:E2,2)" "=MATCH(9,A1:E1)" "" "" ""))))
                 '("b" 3 "" "" "")))
  ;; XLOOKUP's search by halves orders numbers before text.
  (should (equal (grid-table-formula-test--eval
                  "=XLOOKUP(46523,{9700;\"xyz\";\"xyz\";160726;204100;510300},{0.1;0.22;0.24;0.32;0.35;0.37},0,1,2)")
                 0.22))
  (should (equal (grid-table-formula-test--eval "=XLOOKUP(6,{9;7;5;3},{1;2;3;4},0,1,-2)") 2))
  (should (equal (grid-table-formula-test--eval "=XLOOKUP(6,{9;7;5;3},{1;2;3;4},0,-1,-2)") 3))
  (should (equal (grid-table-formula-test--eval "=XLOOKUP(6,{9;7;5;3},{1;2;3;4},\"none\",0,-2)") "none")))

(ert-deftest grid-table-formula-takes-one-value-where-excel-wants-a-number ()
  ;; MODE takes a value written directly, or one cell, as an array of
  ;; one, so text or a blank there is #VALUE!, as Excel gives.
  (dolist (case '(("=MODE(B1,\"1\",2,3)" . "#VALUE!")
                  ("=MODE(\"1\",1,1/0,2,3)" . "#VALUE!")
                  ("=MODE(B1,2,2,E1)" . "#VALUE!")
                  ("=MODE(B1:D1,1,1)" . 1)
                  ("=DEVSQ(C1:D1)" . "#NUM!")
                  ("=DEVSQ(B1)" . 0)
                  ;; So with SUMPRODUCT of single cells.
                  ("=SUMPRODUCT(C1:C1,B1:B1)" . "#VALUE!")
                  ("=SUMPRODUCT(C1:D1,B1:C1)" . 0)
                  ("=SUMPRODUCT(E1:E1,B1:B1)" . 0)
                  ;; A workbook before a plain sheet name is not quoted.
                  ("=ADDRESS(2,3,3,TRUE,\"[Book1]Sheet1\")" . "[Book1]Sheet1!$C2")
                  ("=ADDRESS(2,3,3,TRUE,\"[Book1]My Sheet\")" . "'[Book1]My Sheet'!$C2")))
    (should (equal (grid-table-formula-test--eval (car case) "1" "x" "y" "")
                   (cdr case)))))

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
    (should (equal (grid-model-get-computed-value model 2 1) 6))
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

;;;----------------------------------------------------------------------
;;; Spilling
;;;----------------------------------------------------------------------

(ert-deftest grid-table-formula-several-values-spill ()
  (let ((model (grid-model-create 6 3)))
    (cl-flet ((column (col)
                (cl-loop for row below 6
                         collect (grid-model-get-computed-value model row col))))
      ;; The values go down the cells below, which stay empty.
      (grid-model-set-raw-value model 0 0 "=SEQUENCE(3)")
      (should (equal (column 0) '(1 2 3 "" "" "")))
      (should (equal (nth 0 (nth 1 (grid-model-get-all-raw-values model))) ""))
      ;; A formula reads a spilled cell, and a spill as a whole.
      (grid-model-set-raw-value model 5 1 "=SUM(A1:A3)")
      (grid-model-set-raw-value model 4 1 "=ROWS(A1#)")
      (should (equal (column 1) '("" "" "" "" 3 6)))
      ;; Something in the way: #SPILL!, and the readers follow.
      (grid-model-set-raw-value model 2 0 "x")
      (should (equal (column 0) '("#SPILL!" "" "x" "" "" "")))
      (should (equal (column 1) '("" "" "" "" "#REF!" "#SPILL!")))
      ;; Out of the way again.
      (grid-model-set-raw-value model 2 0 "")
      (should (equal (column 0) '(1 2 3 "" "" "")))
      ;; Fewer values leave the cells they no longer reach empty.
      (grid-model-set-raw-value model 0 0 "=SEQUENCE(2)")
      (should (equal (column 0) '(1 2 "" "" "" "")))
      (should (equal (column 1) '("" "" "" "" 2 3)))
      ;; Past the bottom of the sheet.
      (grid-model-set-raw-value model 3 2 "=SEQUENCE(4)")
      (should (equal (column 2) '("" "" "" "#SPILL!" "" ""))))))

(ert-deftest grid-table-formula-array-formula-fills-its-range ()
  (let ((model (grid-model-create
                3 4 '(("1" "=A1:A2*10" "=A1:A3+1" "=7")
                      ("2" "" "" "")
                      ("x" "" "" "")))))
    (grid-model-set-arrays model '(((0 . 1) . "B1:B3") ((0 . 2) . "C1:C2")
                                   ((0 . 3) . "D1:D3")))
    (grid-model-set-raw-value model 1 2 "own")
    (cl-flet ((column (col)
                (cl-loop for row below 3
                         collect (grid-model-get-computed-value model row col))))
      ;; Past its values an array formula shows #N/A; a single value
      ;; fills every cell; a cell holding something keeps it.
      (should (equal (column 1) '(10 20 "#N/A")))
      (should (equal (column 2) '(2 "own" "")))
      (should (equal (column 3) '(7 7 7)))
      ;; A one-cell array formula that comes out FALSE shows FALSE.
      (grid-model-set-raw-value model 2 2 "=1>2")
      (grid-model-set-arrays model '(((2 . 2) . "C3")))
      (should (equal (grid-model-get-computed-value model 2 2) nil)))))

(ert-deftest grid-table-formula-empty-arguments-and-isformula-ranges ()
  (let ((model (grid-model-create 3 3 '(("=1" "" "") ("2" "" "") ("=3" "" "")))))
    (cl-flet ((value (formula)
                (grid-model-set-raw-value model 0 2 formula)
                (grid-model-get-computed-value model 0 2)))
      ;; A dynamic-array function takes its default for an empty
      ;; argument; an older one takes it as 0.
      (should (equal (value "=INDEX(SORT(A1:A3,,-1),1)") 3))
      (should (equal (value "=ROWS(SEQUENCE(2,,5))") 2))
      (should (equal (value "=ROUND(2.5,)") 3))
      ;; ISFORMULA over a range answers for each cell.
      (should (equal (value "=INDEX(ISFORMULA(A1:A3),1)") t))
      (should (equal (value "=INDEX(ISFORMULA(A1:A3),2)") nil))
      (should (equal (value "=ISFORMULA(A3)") t))
      ;; A number joined into text loses every trailing zero.
      (should (equal (value "=8.4&\"!\"") "8.4!"))
      (should (equal (value "=0.125&\"\"") "0.125"))
      ;; IFERROR over several values replaces each error on its own.
      (should (equal (value "=INDEX(IFERROR({1;2}/{0;1},{8;9}),1)") 8))
      (should (equal (value "=INDEX(IFERROR({1;2}/{0;1},{8;9}),2)") 2))
      (should (equal (value "=IFNA(1/0,3)") "#DIV/0!"))
      ;; A spill reference followed by a space intersects what comes next.
      (grid-model-set-raw-value model 0 1 "=SEQUENCE(3)")
      (should (equal (value "=B1# B2") 2)))))

(ert-deftest grid-table-formula-spill-read-before-its-formula ()
  ;; A1 comes before B4 in the sheet but reads what B4 spills.
  (let ((model (grid-model-create
                6 2 '(("=B5*10" "") ("" "") ("" "") ("" "=SEQUENCE(2)")
                      ("" "") ("" "")))))
    (should (equal (grid-model-get-computed-value model 0 0) 20))))

(ert-deftest grid-table-formula-file-formula-reads-a-spill ()
  ;; A1 and A2 are a workbook file's formulas; B4, which they come
  ;; first to compute, is not, and spills all the same.
  (let ((model (grid-model-create
                6 2 '(("=B5*10" "") ("=SUM(B4#)" "") ("" "")
                      ("" "=SEQUENCE(2)") ("" "") ("" "")))))
    (grid-model-set-legacy model '((0 . 0) (1 . 0)))
    (grid-calc-forget model)
    (grid-model-recalculate-all model)
    (should (equal (grid-calc-spill-range model 3 1) "B4:B5"))
    (should (equal (grid-model-get-computed-value model 0 0) 20))
    (should (equal (grid-model-get-computed-value model 1 0) 3))))

(ert-deftest grid-table-formula-file-formula-takes-one-value-where-excel-does ()
  ;; A workbook file's formula takes one value of a range wherever
  ;; Excel's classes for its functions say so, inside arguments too.
  ;; Each answer is Excel's for a formula in E2 that is no array formula.
  (cl-flet ((value (formula &optional (row 1) (col 4))
              (let ((model (grid-model-create
                            6 5 '(("1" "-7" ">0" "" "") ("-2" "" "<0" "" "")
                                  ("3" "" ">5" "" "") ("" "" "" "" "")
                                  ("" "" "" "" "") ("" "" "" "" "")))))
                (grid-model-set-raw-value model row col formula)
                (grid-model-set-legacy model (list (cons row col)))
                (grid-calc-forget model)
                (grid-model-recalculate-all model)
                (grid-model-get-computed-value model row col))))
    (pcase-dolist (`(,formula ,expected)
                   '(("=SUM(A1:A3*2)" -4) ("=SUMPRODUCT(A1:A3*2)" 4)
                     ("=SUM(LEN(A1:A3))" 2) ("=SUMPRODUCT(--ISNUMBER(A1:A3))" 3)
                     ("=SUM(IF(A1:A3>0,1,0))" 0) ("=SUMPRODUCT(IF(A1:A3>0,1,0))" 0)
                     ("=MAX(INDEX(A1:A3,0))" 3) ("=SUM(INDEX(A1:A3*2,0))" 4)
                     ("=SUMIF(A1:A3,C1:C3)" -2) ("=SUMPRODUCT(SUMIF(A1:A3,C1:C3))" 2)
                     ("=ABS(IF(TRUE,A1:A3))" 2) ("=TRANSPOSE(A1:A3)" -2)
                     ("=MAX(ABS(A1:A3))" 2) ("=SUMPRODUCT(ABS(A1:A3))" 6)
                     ("=SUMPRODUCT((A1:A3>0)*1)" 2) ("=COUNTIF(A1:A3,A1:A3)" 1)
                     ("=AGGREGATE(14,6,A1:A3/(A1:A3>0),1)" 3)
                     ("=ROW(A1:A3)" 1) ("=SUM({1,2,3})" 6) ("=ABS({-1,-2})" 1)
                     ;; What a function computes stays several values.
                     ("=SUM(ABS({-1,-2}))" 3) ("=TYPE(A1:A3)" 64)
                     ("=SUM(SIGN(FREQUENCY(IF(A1:A3<0,MATCH(A1:A3,A1:A3,0)),ROW(A1:A3))))"
                      1)))
      (should (equal (list formula (value formula)) (list formula expected))))
    ;; A row of cells gives the one in the formula's column.
    (should (equal (value "=ABS(A1:C1)" 4 1) 7))
    ;; A formula outside the range's rows has none of them.
    (should (equal (value "=ABS(A1:A3)" 4 4) "#VALUE!"))))

(ert-deftest grid-table-formula-text-shows-where-a-file-formula-takes-one-value ()
  ;; Excel shows an @ where a workbook file's formula takes one value
  ;; of what may be several, and none where it takes them all.
  (cl-flet ((text (formula)
              (let ((model (grid-model-create
                            3 3 '(("1" "2" "") ("3" "4" "") ("" "" "")))))
                (grid-model-set-raw-value model 0 2 formula)
                (grid-model-set-raw-value model 2 0 "=FORMULATEXT(C1)")
                (grid-model-set-legacy model '((0 . 2)))
                (grid-calc-forget model)
                (grid-model-recalculate-all model)
                (grid-model-get-computed-value model 2 0))))
    (pcase-dolist (`(,formula ,expected)
                   '(("=A1:A2+B1:B2" "=@A1:A2+@B1:B2") ("=-A1:A2" "=-@A1:A2")
                     ("=SUM(A1:A2*2)" "=SUM(@A1:A2*2)") ("=SUM(A1:B2)" "=SUM(A1:B2)")
                     ("=SUMPRODUCT(A1:A2*2)" "=SUMPRODUCT(A1:A2*2)")
                     ("=ANCHORARRAY(A1)" "=@A1#") ("=A1" "=A1") ("=A1:A1" "=A1:A1")
                     ("=UNIQUE(A1:A2)" "=@UNIQUE(A1:A2)")
                     ("=SINGLE((UNIQUE(A1:A2)))" "=@(UNIQUE(A1:A2))")
                     ("=TYPE(ANCHORARRAY(A1))" "=TYPE(A1#)")
                     ("=SUM((A1,B1))" "=SUM((A1,B1))") ("=\"A1:A2\"" "=\"A1:A2\"")))
      (should (equal (list formula (text formula)) (list formula expected))))))

(ert-deftest grid-table-formula-spill-moves-with-its-formula ()
  (let* ((model (grid-model-create
                 5 2 '(("" "=SUM(A3:A5)") ("" "") ("=SEQUENCE(3)" "")
                       ("" "") ("" ""))))
         (moved (grid-model-delete-row model 0)))
    (should (equal (cl-loop for row below 4
                            collect (grid-model-get-computed-value moved row 0))
                   '("" 1 2 3)))
    (should (equal (grid-model-get-computed-value moved 0 1) 6))))

(ert-deftest grid-table-edits-give-what-computing-everything-gives-with-spills ()
  (let* ((rows 10) (cols 5)
         (formulas '("=SEQUENCE(2)" "=SEQUENCE(3,2)" "=A1:A3" "=SORT(B1:B4)"
                     "=SUM(A1:B6)" "=A2+1" "=C4*2" "=COUNT(C1:C10)"
                     "=ROWS(A1#)" "=B3" "=TRANSPOSE(A1:A3)"))
         (pick (lambda ()
                 (pcase (random 5)
                   (0 (number-to-string (random 20)))
                   ((or 1 2) (nth (random (length formulas)) formulas))
                   (_ ""))))
         model)
    (random "grid-table-spills")
    (setq model (grid-model-create
                 rows cols
                 (cl-loop repeat rows collect
                          (cl-loop repeat cols collect (funcall pick)))))
    (dotimes (_ 300)
      (grid-model-set-raw-value model (random rows) (random cols)
                                (funcall pick))
      (should (equal (grid-table-formula-test--all-values model)
                     (grid-table-formula-test--fresh-values model))))))

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
    (should (equal (grid-get-computed-value-at two 0 0) 3))
    (should (= (grid-table-formula-test--computed
                (lambda () (grid-set-raw-value-at one 1 1 "60")))
               1))
    ;; A cycle across the sheets is found.
    (grid-set-raw-value-at one 0 2 "=S2!A1")
    (should (equal (grid-get-computed-value-at one 0 2) "#CYCLE!"))
    (grid-set-raw-value-at two 0 0 "=S1!C2")
    (should (equal (grid-get-computed-value-at one 0 2) 6))
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
          ;; A name can only be one that no reference takes.  It may
          ;; share a function's name, as Excel allows: Rate is one.
          (should (grid-calc-name-problem "TAX1"))
          (should (grid-calc-name-problem "Q1Sales"))
          ;; No sheet has a column GLOBAL, so Global1 is a name.
          (should-not (grid-calc-name-problem "Global1"))
          (should-not (grid-calc-name-problem "Sum"))
          (should (grid-calc-name-problem "TRUE"))
          (should-not (grid-calc-name-problem "Sales_2024"))
          (grid-workbook-delete-name workbook "rate")
          (should (equal (grid-get-computed-value-at one 3 0) "#NAME?")))
      (delete-file file))))

;; Tables a workbook file has, named by references such as T[Qty]
(ert-deftest grid-table-formula-table-references ()
  (let* ((one (make-default-data-source
               '("" "" "" "")
               '(("Item" "Q1" "Price" "=SUM(Sales[Q1])")
                 ("a" "2" "5" "=Sales[@Q1]*Sales[@Price]")
                 ("b" "3" "7" "=Sales[@Q1]*2")
                 ("Total" "=SUBTOTAL(109,[Q1])" "" "=ROWS(Sales[#All])")
                 ("=Sales[[#Headers],[Price]]" "=COUNTA(Sales[[#Data],[#Totals],[Item]])"
                  "=SUM(Sales[[Q1]:[Price]])" "=Sales[Nope]"))))
         (workbook (grid-workbook-create (list (cons "One" one)))))
    (setf (grid-workbook-tables workbook)
          '(("Sales" "One" 0 0 3 2 1 1 ("Item" "Q1" "Price"))))
    (grid-workbook-set-names workbook nil)
    (should (equal (grid-get-computed-value-at one 0 3) 5))
    (should (equal (grid-get-computed-value-at one 1 3) 10))
    (should (equal (grid-get-computed-value-at one 2 3) 6))
    (should (equal (grid-get-computed-value-at one 3 1) 5))
    (should (equal (grid-get-computed-value-at one 3 3) 4))
    (should (equal (grid-get-computed-value-at one 4 0) "Price"))
    (should (equal (grid-get-computed-value-at one 4 1) 3))
    (should (equal (grid-get-computed-value-at one 4 2) 17))
    (should (equal (grid-get-computed-value-at one 4 3) "#REF!"))
    ;; An edit inside the table reaches the formulas naming it.
    (grid-set-raw-value-at one 1 1 "4")
    (should (equal (grid-get-computed-value-at one 0 3) 7))
    ;; What is inside the brackets names columns, so moving lines
    ;; leaves it as it is.
    (should (equal (grid-calc-shift-formula
                    "=Sales[Q1]+B2+Sales[[#This Row],[Q1]]" 1 0)
                   "=Sales[Q1]+B3+Sales[[#This Row],[Q1]]"))))

;; A workbook file may give a name to one sheet only, define it by a
;; formula, or leave its references unanchored, read as from A1.
(ert-deftest grid-table-formula-names-as-a-workbook-file-has-them ()
  (let* ((one (make-default-data-source
               '("" "" "") '(("1" "=Rate" "=Above") ("2" "=Two!Rate" "=Above")
                             ("3" "=Twice" "=Loop"))))
         (two (make-default-data-source '("") '(("10") ("=Rate"))))
         (workbook (grid-workbook-create (list (cons "One" one)
                                               (cons "Two" two)))))
    (setf (grid-workbook-local-names workbook)
          '((("Two" . "Rate") . "=Two!$A$1")
            (("One" . "Above") . "=One!$A1048576")))
    (grid-workbook-set-names workbook '(("Rate" . "=One!$A$3")
                                        ("Twice" . "=Rate*2")
                                        ("Loop" . "=Loop+1")))
    ;; The sheet's own name comes first; elsewhere the workbook's.
    (should (equal (grid-get-computed-value-at one 0 1) 3))
    (should (equal (grid-get-computed-value-at two 1 0) 10))
    (should (equal (grid-get-computed-value-at one 1 1) 10))
    ;; A name defined by a formula, and one defined by way of itself
    (should (equal (grid-get-computed-value-at one 2 1) 6))
    (should (equal (grid-get-computed-value-at one 2 2) "#NAME?"))
    ;; $A1048576 read from row 2 is the row above it, going round
    (should (equal (grid-get-computed-value-at one 1 2) 1))
    (should (equal (grid-get-computed-value-at one 0 2) 0))
    ;; It follows the cell it reads.
    (grid-set-raw-value-at one 0 0 "5")
    (should (equal (grid-get-computed-value-at one 1 2) 5))))

;; A name read while another cell computes the same name is not read by
;; way of itself: each cell reads it afresh.
(ert-deftest grid-table-formula-name-read-down-a-chain ()
  (let* ((one (make-default-data-source
               '("") '(("=Next") ("=Next") ("=Next") ("1"))))
         (workbook (grid-workbook-create (list (cons "One" one)))))
    (grid-workbook-set-names workbook '(("Next" . "=One!$A2+1")))
    (grid-workbook-recalculate workbook t)
    (should (equal (grid-get-computed-value-at one 0 0) 4))))

;; A column each of whose cells reads the one below is computed from the
;; top down, however long, as is a cycle longer than the formulas put on
;; top of each other at a time.
(ert-deftest grid-table-formula-long-chains-are-computed ()
  (let* ((plain (make-default-data-source
                 '("") (append (cl-loop for row from 2 to 300
                                        collect (list (format "=A%d+1" row)))
                               '(("1")))))
         (named (make-default-data-source
                 '("") (append (make-list 299 '("=Next")) '(("1")))))
         (workbook (grid-workbook-create (list (cons "One" named))))
         (cycle (make-default-data-source
                 '("") (cl-loop for row from 1 to 30
                                collect (list (format "=A%d" (1+ (% row 30))))))))
    (should (equal (grid-get-computed-value-at plain 0 0) 300))
    (grid-workbook-set-names workbook '(("Next" . "=One!$A2+1")))
    (grid-workbook-recalculate workbook t)
    (should (equal (grid-get-computed-value-at named 0 0) 300))
    ;; With less room, fewer formulas fit on top of each other.
    (let ((max-lisp-eval-depth 400))
      (grid-workbook-recalculate workbook t))
    (should (equal (grid-get-computed-value-at named 0 0) 300))
    (should (equal (grid-get-computed-value-at cycle 0 0) "#CYCLE!"))
    (should (equal (grid-get-computed-value-at cycle 29 0) "#CYCLE!"))
    ;; A cycle longer than formulas may go on top of each other
    (let ((long (make-default-data-source
                 '("") (cl-loop for row from 1 to 300
                                collect (list (format "=A%d+1"
                                                      (1+ (% row 300))))))))
      (dolist (row '(0 1 150 299))
        (should (equal (grid-get-computed-value-at long row 0) "#CYCLE!"))))))

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
(ert-deftest grid-table-tables-follow-inserted-and-deleted-lines ()
  (let* ((data (make-default-data-source
                '("" "" "")
                '(("Item" "Qty" "") ("a" "2" "") ("b" "3" "")
                  ("" "" "=SUM(Table1[Qty])"))))
         (workbook (grid-workbook-create (list (cons "Data" data))))
         (table (lambda () (cdr (car (grid-workbook-tables workbook))))))
    (setf (grid-workbook-tables workbook)
          (list (list "Table1" "Data" 0 0 2 1 1 0 '("Item" "Qty"))))
    (grid-workbook-recalculate workbook t)
    (should (equal (grid-get-computed-value-at data 3 2) 5))
    ;; A row above moves it; the table's name is no cell to move.
    (let ((before (gethash :model data)))
      (funcall (gethash :add-row data) data -1)
      (should (equal (funcall table) '("Data" 1 0 3 1 1 0 ("Item" "Qty"))))
      (should (equal (grid-get-raw-value-at data 4 2) "=SUM(Table1[Qty])"))
      (should (equal (grid-get-computed-value-at data 4 2) 5))
      ;; Undone, it goes back.
      (grid-workbook-model-restored data (gethash :model data) before)
      (puthash :model before data)
      (should (equal (funcall table) '("Data" 0 0 2 1 1 0 ("Item" "Qty")))))
    ;; A row inside makes it longer, a column inside wider, with a name
    ;; in its header cell.
    (funcall (gethash :add-row data) data 0)
    (should (equal (funcall table) '("Data" 0 0 3 1 1 0 ("Item" "Qty"))))
    (funcall (gethash :add-column data) data 1)
    (should (equal (funcall table)
                   '("Data" 0 0 3 2 1 0 ("Item" "Column1" "Qty"))))
    (should (equal (grid-get-raw-value-at data 0 1) "Column1"))
    (grid-set-raw-value-at data 1 2 "4")
    (should (equal (grid-get-computed-value-at data 4 3) 9))
    ;; Deleted lines inside make it smaller again.
    (funcall (gethash :delete-column data) data 1)
    (funcall (gethash :delete-row data) data 0)
    (should (equal (funcall table) '("Data" 0 0 2 1 1 0 ("Item" "Qty"))))
    (should (equal (grid-get-computed-value-at data 3 2) 5))
    ;; Without its header row the table goes.
    (funcall (gethash :delete-row data) data -1)
    (should-not (grid-workbook-tables workbook))))

(ert-deftest grid-table-text-follows-excel-format-codes ()
  "TEXT writes numbers, fractions, times and dates as Excel does.
The cases and what Excel made of them come from Apache POI's format
test workbooks; a date's serial counts from 1904 where the workbook did."
  (dolist (case
           '(
    (1900 12.34 "$#.#" "$12.3")
    (1900 12.3 "\\a $#.00\" profit\"" "a $12.30 profit")
    (1900 1234567 "#,##" "1,234,567")
    (1900 1234567 ",0" ",1234567")
    (1900 1234567 "0,000000" "1,234,567")
    (1900 1234567 "?,?????????" "    1,234,567")
    (1900 -1234567 "#,###" "-1,234,567")
    (1900 -1234567 "0,0" "-1,234,567")
    (1900 1234567 "0,0000000" "01,234,567")
    (1900 -123456 "0,00000" "-123,456")
    (1900 -1234567 "?,????????" "-  1,234,567")
    (1900 -1234567 "?,??????" "-1,234,567")
    (1900 -1234567 "0.#," "-1234.6")
    (1900 -123.45 "0,0.00%%" "-1,234,500.00%%")
    (1900 123456.789 "|#,e-#|" "|1e5|")
    (1900 123456.789 "|0.0|e-|0|" "|1.2|e|5|")
    (1900 123456.789 "|#e+#,|" "|1e+5|")
    (1900 123456.789 "|0000.0000|e+|0|" "|0012.3457|e|+4|")
    (1900 0.000123456789 "|#######.####|e+|#|" "|1234.5679|e|-7|")
    (1900 0.00000123456789 "|####.####|e+|#|" "|123.4568|e|-8|")
    (1900 0.0000123456789 "|?.?|e+|?|" "|1.2|e|-5|")
    (1900 0 "|#|e-|#|" "|0|e|0|")
    (1900 3.14159 "[h]:m:s.000" "75:23:53.376")
    (1900 0.08546296296296296 "[mm]:ss.0" "123:04.0")
    (1900 0 "\"It was \"[h]\" [yes, \"h\"] hours and \"mm:ss" "It was 0 [yes, 0] hours and 00:00")
    (1900 -3.14159 "[s].000" "-271433.376")
    (1900 1 "[<10]#\" Wow\"" "1 Wow")
    (1900 10 "[>10]#\" Big\"" "10")
    (1900 1 "[<=10]#\" Wow\"" "1 Wow")
    (1900 10 "[>=10]#\" Big\"" "10 Big")
    (1900 1 "General" "1")
    (1904 17816.607951388887 "d-m-y" "11-10-52")
    (1904 17816.607951388887 "dddd-mmmm-yyyy" "Saturday-October-1952")
    (1904 17816.607951388887 "hh:mm:ss a/p" "02:35:27 p")
    (1904 17816.607951388887 "d-m-y h:m:s" "11-10-52 14:35:27")
    (1904 1 "yyyy-mm-dd hh:mm:ss.000" "1904-01-02 00:00:00.000")
    (1904 1.0032134017094 "yyyy-mm-dd hh:mm:ss.00" "1904-01-02 00:04:37.64")
    (1900 23.75 "|#\\:#/#|" "|23:3/4|")
    (1900 0 "|#\\:#/#|" "|0|")
    (1900 -23.75 "|#-#-#\\:#/#|" "-|-2-3:3/4|")
    (1900 1 "|#-#-#\\:#/#|" "|--1|")
    (1900 0 "|#\\:#=/=#|" "|0|")
    (1900 -23.75 "|#\\:#=/=?|" "-|23:3=/=4|")
    (1900 23.75 "|#\\:#=/=0|" "|23:3=/=4|")
    (1900 -1 "|#\\:#=/=0|" "-|1|")
    (1900 1 "|#\\:? ?=/=#|" "|:1      |")
    (1900 0 "|#\\:? ?=/=?|" "|:0      |")
    (1900 23.75 "|#\\:? ?0#/000" "|2:3  03/004")
    (1900 3.75 "|#_#/#|" "|15 /4|")))
    (pcase-let ((`(,system ,value ,code ,expected) case))
      (let ((grid-format-date-system system))
        (should (equal (list value code (grid-calc-format-code value code))
                       (list value code expected)))))))

(ert-deftest grid-table-text-follows-excel-with-elapsed-times ()
  "TEXT writes an elapsed time of any size, and refuses a negative one
that has more fields than its hours, minutes or seconds.  The cases
come from pycel's text test workbook."
  (dolist (case '((71003160 "[hh]" "1704075840")
                  (2958465 "[hh]" "71003160")
                  (2958466 "h" "#VALUE!")
                  (2958465 "h" "0")
                  (-1 "[h]" "-24")
                  (-1 "[h]:m" "#VALUE!")
                  (0 "[h]:m" "0:0")
                  (-1 "[m]:s" "#VALUE!")
                  (0 "[s].000" "0.000")))
    (pcase-let ((`(,value ,code ,expected) case))
      (let ((model (grid-model-create 1 3 (list (list (number-to-string value) code
                                                     "=TEXT(A1,B1)")))))
        (should (equal (list value code (grid-model-get-computed-value model 0 2))
                       (list value code expected)))))))

(ert-deftest grid-table-text-reads-times-written-as-text ()
  "TEXT reads a date or a time written as text before it writes it.
The cases come from pycel's and IronCalc's TEXT test workbooks."
  (dolist (case
           '(;; A date and a time together.
             ("2007-02-03 19:08:07.0123" "[h]" "938803")
             ("2007-02-03 19:08:07.0123" "[m]" "56328188")
             ("2007-02-03 19:08:07.0123" "[s]" "3379691287")
             ;; The last of the fields may be up to 9999, and a fraction
             ;; of it counts as seconds.
             ("0:59:99" "#.#####" ".04212")
             ("23:99" "#.#####" "1.02708")
             ("23:9999" "#.#####" "7.90208")
             ("0:0:9999" "#.#####" ".11573")
             ("23:59:9999" "#.#####" "1.11503")
             ("59:9999.0" "#.#####" ".1567")
             ("23:59.012345" "#.#######" ".0166552")
             ("59:9999.012345" "0.########" "0.15670153")
             ("23:59." "#.###########" ".99930555556")
             ;; Not a time, so it is left as it is.
             ("60:99" "#.#####" "60:99")
             ("24:99" "#.#####" "24:99")
             ("23:10000" "#.#####" "23:10000")
             ("23:60:9999" "#.#####" "23:60:9999")
             ("23:59.:0" "#.###########" "23:59.:0")))
    (pcase-let ((`(,value ,code ,expected) case))
      (let ((model (grid-model-create 1 3 (list (list value code "=TEXT(A1,B1)")))))
        (should (equal (list value code (grid-model-get-computed-value model 0 2))
                       (list value code expected)))))))

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

(ert-deftest grid-table-text-writes-every-digit-and-literal-comma ()
  "TEXT writes a whole number with no placeholder of its own, keeps a
comma that is not a thousands separator, and takes a date code mixed
with digits, or a quote that is never closed, for a mistake.  The cases
come from pycel's and IronCalc's TEXT test workbooks."
  (dolist (case
           '(("1234.56" ".#" "1234.6")
             ("1234.56" ".##0" "1234.560")
             ("890123.456789" "#.#####00#0" "890123.45678900")
             ("890123.456789" "0,x#.00.0" "89012x3.45.7")
             ("890123.456789" "#x,0x#.00.0" "8901x,2x3.45.7")
             ("890123.456789" "#x,0,#.00.0" "890,1x,23.45.7")
             ("890123.456789" "#.###x,##" "890123.456x,79")
             ("890123.456789" "#.###,x##" "890123.456x79")
             ("890123.456789" "#,#.##.##,#" "890,123.45.679")
             ("1234567890" "0,## ## ,##" "1,234,56 7,8 ,90")
             ("1000000.555" "0 .  # ,0.00.0,00" "1000000 .  5 ,5.50.000")
             ;; Letters that are no code of Excel's are written as they are.
             ("0" "w;x;z@" "w")
             ("-1" "w;x@" "-w")
             ("X" "##.00;-  ##.0;\"ZERO\";->@<-@" "->X<-X")
             ;; @ in a section that writes numbers is a mistake.
             ("X" ";-  ##.0;\"ZERO\"@;\"X\"" "#VALUE!")
             ;; So is a date code among digit placeholders, and a quote
             ;; that is never closed.
             ("41181" "mm###" "#VALUE!")
             ("1.23" "#\"°F\"\"" "#VALUE!")
             ;; A very small number takes an exponent.
             ("-0.0000000025" "General" "-2.5E-09")))
    (pcase-let ((`(,value ,code ,expected) case))
      (let ((model (grid-model-create 1 3 (list (list value code "=TEXT(A1,B1)")))))
        (should (equal (list value code (grid-model-get-computed-value model 0 2))
                       (list value code expected)))))))

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
  ;; Text a formula gives that spells an error stays text in its cell.
  (let ((values (grid-table-formula-test--values
                 '(("=\"#N/A\"" "=ISERROR(A1)" "=ISTEXT(A1)" "'#DIV/0!" "=UPPER(D1)"
                    "=ISERROR(E1)" "=NA()" "=ISERROR(G1)")))))
    (should (equal (nth 1 (car values)) nil))
    (should (equal (nth 2 (car values)) t))
    (should (equal (nth 5 (car values)) nil))
    (should (equal (nth 7 (car values)) t)))
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
      (should (= (count "\"<>\"") 6))))
  ;; An empty cell given as the criterion means 0, not an empty cell.
  (should (= (grid-table-formula-test--eval "=COUNTIF(B1:E1,F1)" "0" "" "x" "") 1))
  ;; "=23" takes the text "23" too, but "<>23" leaves out only the number.
  (should (= (grid-table-formula-test--eval "=COUNTIF(B1:C1,\"=23\")" "23" "'23") 2))
  (should (= (grid-table-formula-test--eval "=COUNTIF(B1:C1,\"<>23\")" "23" "'23") 1))
  ;; A whole column has every row of the sheet, and a whole row every
  ;; column, though the sheet has fewer.
  (cl-flet ((eval (formula) (grid-table-formula-test--eval formula "x" "1" "")))
    (should (= (eval "=ROWS(B:B)") 1048576))
    (should (= (eval "=COLUMNS(1:1)") 16384))
    (should (= (eval "=COUNTIF(B:B,\"<>x\")") 1048575))
    (should (= (caar (grid-table-formula-test--values
                      '(("=COUNTIF(2:2,\"<>x\")") ("x" "1"))))
               16383))
    (should (= (eval "=COUNTIFS(B:B,\"\",C:C,\"\")") 1048575))
    (should (= (eval "=COUNTBLANK(B:B)") 1048575))
    (should (= (eval "=SUMIF(B:B,\"x\",C:C)") 1))
    (should (= (eval "=INDEX(COUNTIF(B:B,{\"x\";\"<>x\"}),2)") 1048575)))
  ;; A comparison with nothing to compare with meets no cell.
  (dolist (op '(">" ">=" "<" "<="))
    (should (= (grid-table-formula-test--eval (format "=COUNTIF(B1:D1,\"%s\")" op)
                                              "1" "a" "")
               0))))

(ert-deftest grid-table-formula-countif-takes-many-criteria ()
  ;; Each criterion of an array counts on its own.
  (cl-flet ((first-value (formula)
              (car (car (grid-table-formula-test--values
                         `((,formula "x") ("" "") ("" "x") ("" "y")))))))
    (should (equal (first-value "=SUMPRODUCT((B1:B4<>\"\")/COUNTIF(B1:B4,B1:B4&\"\"))")
                   2))
    (should (equal (first-value "=INDEX(COUNTIF(B:B,{\"x\";\"\";\"z\"}),1)") 2))
    ;; B:B reaches the sheet's last row, whose cells are empty.
    (should (equal (first-value "=INDEX(COUNTIF(B:B,{\"x\";\"\";\"z\"}),2)") 1048573))
    (should (equal (first-value "=INDEX(COUNTIF(B:B,{\"x\";\"\";\"z\"}),3)") 0))
    ;; So does each of COUNTIFS's and SUMIFS's.
    (should (equal (first-value "=SUM(COUNTIFS(B1:B4,{\"x\",\"y\"}))") 3))
    (should (equal (first-value "=SUM(SUMIFS(C1:C4,B1:B4,{\"x\",\"y\"}))") 0))
    (should (equal (first-value "=SUM(COUNTIFS(B1:B4,\"x\",B1:B4,{\"x\",\"y\"}))") 2))
    ;; A reference that is lost is the answer where one is wanted.
    (should (equal (first-value "=COUNTIF(#REF!,1)") "#REF!"))
    (should (equal (first-value "=IFNA(MATCH(1,#REF!,0),0)") "#REF!"))
    (should (equal (first-value "=COUNTIF(B1:B4,#N/A)") 0))
    (should (equal (first-value "=COUNTA(#REF!)") 1))
    (should (equal (first-value "=COUNTBLANK(#REF!)") "#REF!"))
    (should (equal (first-value "=ISREF(#REF!)") nil))
    (should (equal (first-value "=IFERROR(VLOOKUP(1,#REF!,1,0),\"x\")") "x"))))

(ert-deftest grid-table-formula-whole-numbers-divide-as-numbers ()
  ;; Numbers typed whole are no whole-number arithmetic: 6/10 is 0.6.
  (cl-flet ((near (formula expected)
              (let ((value (grid-table-formula-test--eval formula)))
                (should (equal (list formula (and (numberp value)
                                                  (< (abs (- value expected)) 1e-9)))
                               (list formula t))))))
    (near "=T.DIST(2,6,TRUE)" 0.9537868442341624)
    (near "=T.DIST(2,6,FALSE)" 0.06403612261840969)
    (near "=T.DIST.2T(1,2)" 0.4226497308103734)
    (near "=F.DIST(2,6,8,TRUE)" 0.8208)
    (near "=F.DIST.RT(2,5,6)" 0.21167432749937592)
    (near "=GAMMA.DIST(1,2,3,FALSE)" 0.07961459006375433)
    (near "=EFFECT(1,2)" 1.25)
    (near "=PERCENTILE.INC({2,1,4},1)" 4)
    (near "=BITAND(6,3)" 2)
    (near "=HOUR(1)" 0))
  (should (equal (grid-table-formula-test--eval "=GAMMA(-1)") "#NUM!")))

(ert-deftest grid-table-formula-pairs-match-by-count ()
  ;; Excel pairs values in reading order, whatever shape holds them.
  (let ((rows '(("1" "4" "5" "6") ("2" "" "" "") ("3" "x" "" ""))))
    (cl-flet ((value (formula)
                (let ((model (grid-model-create 3 4 rows)))
                  (grid-model-set-raw-value model 2 3 formula)
                  (grid-model-get-computed-value model 2 3))))
      (should (= (value "=SUMX2PY2(A1:A3,B1:D1)") 91))
      (should (= (value "=CORREL(A1:A3,B1:D1)") 1))
      (should (equal (value "=SUMX2MY2(A1:A2,B1:B3)") "#N/A"))
      (should (equal (value "=SUMXMY2(B2:B3,C2:C3)") "#DIV/0!")))))

(ert-deftest grid-table-formula-forecasts-by-exponential-smoothing ()
  (cl-flet ((value (rows formula)
              (let ((model (grid-model-create (length rows) 3
                                              (mapcar (lambda (row) (append row '("")))
                                                      rows))))
                (grid-model-set-raw-value model 0 2 formula)
                (grid-model-get-computed-value model 0 2))))
    (let ((line (cl-loop for x from 1 to 10
                         collect (list (number-to-string x)
                                       (number-to-string (+ 3 (* 2 x))))))
          ;; A rise of 1 a step, and a season of four steps.
          (seasons (cl-loop for x from 1 to 16
                            collect (list (number-to-string x)
                                          (number-to-string
                                           (+ 10 x (nth (% (1- x) 4) '(0 5 -3 2))))))))
      ;; A straight line goes on straight, with no doubt.
      (should (= (value line "=FORECAST.ETS(12,B1:B10,A1:A10)") 27))
      (should (= (value line "=FORECAST.ETS(11.5,B1:B10,A1:A10)") 26))
      (should (= (value line "=FORECAST.ETS.CONFINT(12,B1:B10,A1:A10)") 0))
      (should (= (value line "=FORECAST.ETS.SEASONALITY(B1:B10,A1:A10)") 0))
      (should (= (value line "=FORECAST.ETS.STAT(B1:B10,A1:A10,8)") 1))
      ;; The season is found, and carried on.
      (should (= (value seasons "=FORECAST.ETS.SEASONALITY(B1:B16,A1:A16)") 4))
      (should (< (abs (- (value seasons "=FORECAST.ETS(17,B1:B16,A1:A16)") 27)) 1e-9))
      (should (< (abs (- (value seasons "=FORECAST.ETS(18,B1:B16,A1:A16)") 33)) 1e-9))
      (should (< (abs (- (value seasons "=FORECAST.ETS(19,B1:B16,A1:A16,4)") 26)) 1e-9))
      ;; A missing step is filled in from the points beside it.
      (should (= (value (append (cl-subseq line 0 3) (cl-subseq line 4))
                        "=FORECAST.ETS(12,B1:B9,A1:A9)")
                 27))
      (should (equal (value line "=FORECAST.ETS(12,B1:B10,A1:A9)") "#N/A"))
      (should (equal (value line "=FORECAST.ETS(5,B1:B10,A1:A10)") "#NUM!"))
      (should (equal (value line "=FORECAST.ETS(12,B1:B10,A1:A10,1.5)") "#NUM!"))
      (should (equal (value line "=FORECAST.ETS.CONFINT(12,B1:B10,A1:A10,1)") "#NUM!"))
      (should (equal (value (append (cl-subseq line 0 9) '(("10.5" "24")))
                            "=FORECAST.ETS(12,B1:B10,A1:A10)")
                     "#NUM!")))
    ;; Daily values Excel forecasts at 6696.40; this fit is LibreOffice's.
    (let ((rows (cl-loop for x from 43888
                         for y in '(403 531 615 984 1254 1520 1820 2251 2612 3420 4189 5469)
                         collect (list (number-to-string x) (number-to-string y)))))
      (should (< (abs (- (value rows "=FORECAST.ETS(43900,B1:B12,A1:A12,1,1)")
                         6642.539397076179))
                 1e-6))
      (let ((first (value rows "=FORECAST.ETS.CONFINT(43900,B1:B12,A1:A12,0.95,1,1)"))
            (second (value rows "=FORECAST.ETS.CONFINT(43901,B1:B12,A1:A12,0.95,1,1)")))
        (should (< 400 first 500))
        (should (< (* 1.8 first) second (* 2.3 first)))))))

(ert-deftest grid-table-formula-unions-spills-and-nested-totals ()
  (let ((rows '(("1" "2" "=SEQUENCE(3)" "")
                ("3" "=AGGREGATE(9,0,A1:A2)" "" "")
                ("=SUBTOTAL(9,A1:A2)" "" "" ""))))
    (cl-flet ((value (formula)
                (let ((model (grid-model-create 3 4 rows)))
                  (grid-model-set-raw-value model 1 3 formula)
                  (grid-model-get-computed-value model 1 3))))
      ;; Each area of a union counts once; a union is no single value.
      (should (= (value "=SUM((A1,B1:B2))") 7))
      (should (equal (value "=(A1,B1)") "#VALUE!"))
      ;; A# is the cells the formula in A spills over; only a formula
      ;; spills.
      (should (= (value "=ROWS(C1#)") 3))
      (should (= (value "=ROWS(C1:C2 C1#)") 2))
      (should (equal (value "=ROWS(A1#)") "#REF!"))
      (should (equal (value "=COLUMNS(#N/A)") "#N/A"))
      ;; Options 0 to 3 leave out other totals, 4 to 7 keep them.
      (should (= (value "=AGGREGATE(9,0,A1:B3)") 6))
      (should (= (value "=AGGREGATE(9,4,A1:B3)") 14))
      ;; FORMULATEXT shows the operators the file stores as functions.
      (should (equal (value "=FORMULATEXT(B2)") "=AGGREGATE(9,0,A1:A2)")))
    (should (equal (grid-calc-excel-formula-text "=SUM(ANCHORARRAY(C1))+SINGLE(A1:A3)")
                   "=SUM(C1#)+@A1:A3"))
    (should (equal (grid-calc-excel-formula-text "=\"SINGLE(A1)\"")
                   "=\"SINGLE(A1)\""))))

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

;; A row a filter left out is hidden under the filter's header.
(ert-deftest grid-table-subtotal-leaves-out-the-rows-a-filter-left-out ()
  (let* ((source (make-default-data-source
                  '("A" "B")
                  '(("V" "") ("1" "") ("2" "") ("3" "") ("4" "") ("5" "")
                    ("" "=SUBTOTAL(9,A2:A6)") ("" "=SUBTOTAL(109,A2:A6)"))))
         (file (make-temp-file "grid-filtered" nil ".grid")))
    (unwind-protect
        (progn
          ;; Rows 2 and 4 are hidden; the filter over A1:A4 hid row 2.
          (grid-table-set-hidden source '(2 4) nil)
          (grid-table-set-filters source '((0 0 3 0)))
          (should (equal (grid-model-filtered-rows (gethash :model source)) '(2)))
          (should (equal (grid-get-computed-value-at source 6 1) 13))
          (should (equal (grid-get-computed-value-at source 7 1) 9))
          ;; The range grows with a row inserted inside it.
          (funcall (gethash :add-row source) source 0)
          (should (equal (grid-table-filters source) '((0 0 4 0))))
          (should (equal (car (grid-table-hidden source)) '(3 5)))
          (grid-table-persistence--save-to-file source file)
          (let ((loaded (grid-table-persistence-create-data-source-from-file
                         file)))
            (should (equal (grid-table-filters loaded) '((0 0 4 0))))
            (should (equal (grid-get-computed-value-at loaded 7 1) 13))))
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

;;;----------------------------------------------------------------------
;;; ARRAYTOTEXT, VALUETOTEXT and BAHTTEXT
;;;----------------------------------------------------------------------

(defun grid-table-formula-test--text-row (empty-text)
  "Return the C..Q values of a row of the text sample; EMPTY-TEXT is I."
  (list "" "" "0" "1.9" "2.3" "10" "-1" "" empty-text "TRUE"
        "=#VALUE!" "=#N/A" "=#DIV/0!" "=#NAME?" "=#NULL!" "=#NUM!"
        "=#REF!"))

(defun grid-table-formula-test--text-model ()
  "Return a model with the C..Q sample values in rows 3 and 4."
  (grid-model-create
   4 17
   (list (make-list 17 "")
         (make-list 17 "")
         (grid-table-formula-test--text-row "1987-08-26")
         (grid-table-formula-test--text-row "=\"\""))))

(defun grid-table-formula-test--text-call (formula)
  "Return FORMULA's value in A1 of the C..Q sample model."
  (let ((model (grid-table-formula-test--text-model)))
    (grid-model-set-raw-value model 0 0 formula)
    (grid-model-get-computed-value model 0 0)))

(ert-deftest grid-table-arraytotext-writes-values-as-text ()
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(C3:D4,0)")
                 "0, 1.9, 0, 1.9"))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(G3,0)")
                 "-1"))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(H3:H4,0)")
                 ", "))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(J3:K3,0)")
                 "TRUE, #VALUE!"))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(C4:D4,2)")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(D3:E4,1)")
                 "{1.9,2.3;1.9,2.3}"))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(E3:E4,1)")
                 "{2.3;2.3}"))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(I4:J4,1)")
                 "{\"\",TRUE}"))
  ;; A single blank cell is not an array constant.
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(H4,1)")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(K4:L4,0.9)")
                 "#VALUE!, #N/A"))
  (should (equal (grid-table-formula-test--text-call "=ARRAYTOTEXT(C4:Q4,1)")
                 (concat "{0,1.9,2.3,10,-1,,\"\",TRUE,#VALUE!,#N/A,"
                         "#DIV/0!,#NAME?,#NULL!,#NUM!,#REF!}")))
  ;; A range as the format takes its first value, 0 here.
  (should (equal (grid-table-formula-test--text-call
                  "=ARRAYTOTEXT(C3:J3,C3:Q3)")
                 "0, 1.9, 2.3, 10, -1, , 1987-08-26, TRUE")))

(ert-deftest grid-table-valuetotext-writes-one-value-as-text ()
  (should (equal (grid-table-formula-test--text-call "=VALUETOTEXT(1.9,\"0\")")
                 "1.9"))
  (should (equal (grid-table-formula-test--text-call "=VALUETOTEXT(-1,FALSE)")
                 "-1"))
  (should (equal (grid-table-formula-test--text-call "=VALUETOTEXT(\"ciao3\",H3)")
                 "ciao3"))
  (should (equal (grid-table-formula-test--text-call
                  "=VALUETOTEXT(\"1987-08-26\",1)")
                 "\"1987-08-26\""))
  (should (equal (grid-table-formula-test--text-call "=VALUETOTEXT(TRUE,1)")
                 "TRUE"))
  (should (equal (grid-table-formula-test--text-call "=VALUETOTEXT(#N/A,0)")
                 "#N/A"))
  (should (equal (grid-table-formula-test--text-call "=VALUETOTEXT(1,-1)")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--text-call "=VALUETOTEXT(1,\"a\")")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--text-call "=VALUETOTEXT(1,2)")
                 "#VALUE!")))

(ert-deftest grid-table-bahttext-writes-thai-amounts ()
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(0)")
                 "ศูนย์บาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(157)")
                 "หนึ่งร้อยห้าสิบเจ็ดบาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(65)")
                 "หกสิบห้าบาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(65.8)")
                 "หกสิบห้าบาทแปดสิบสตางค์"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(-1)")
                 "ลบหนึ่งบาทถ้วน"))
  ;; Tens and units follow the usual Thai rules.
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(11)")
                 "สิบเอ็ดบาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(21)")
                 "ยี่สิบเอ็ดบาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(101)")
                 "หนึ่งร้อยหนึ่งบาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(1000000)")
                 "หนึ่งล้านบาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(1000001)")
                 "หนึ่งล้านหนึ่งบาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(H3)")
                 "ศูนย์บาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(TRUE)")
                 "หนึ่งบาทถ้วน"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(\"abc\")")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(#N/A)")
                 "#N/A"))
  (should (equal (grid-table-formula-test--text-call "=BAHTTEXT(C3:D4)")
                 "ศูนย์บาทถ้วน")))

(ert-deftest grid-table-formula-date-arguments-fail-as-excel-has-them ()
  ;; A date that is no date fails once all the arguments are read, so a
  ;; later argument's failure wins.  The Analysis ToolPak functions check
  ;; each as they read it, and take no TRUE or FALSE.  B1 holds "abc".
  (dolist (case '(("=WEEKDAY(-1,\"x\")" . "#VALUE!")
                  ("=WEEKDAY(-1,TRUE)" . "#NUM!")
                  ("=WEEKDAY(\"1/1/10000\",1/0)" . "#VALUE!")
                  ("=WEEKNUM(-1,\"x\")" . "#NUM!")
                  ("=WEEKNUM(TRUE)" . "#VALUE!")
                  ("=WEEKNUM(1,TRUE)" . "#VALUE!")
                  ("=EDATE(TRUE,1)" . "#VALUE!")
                  ("=DEC2BIN(TRUE)" . "#VALUE!")
                  ("=YEARFRAC(1/0,1,9)" . "#NUM!")
                  ("=YEARFRAC(2958466,1)" . "#NUM!")
                  ("=YEARFRAC(DATE(2024,2,29),DATE(2025,2,28))" . 1)
                  ("=NETWORKDAYS(1,10,B1)" . "#VALUE!")
                  ("=NETWORKDAYS(1,10,-2)" . "#NUM!")
                  ("=NETWORKDAYS.INTL(1,10,\"1111111\")" . 0)
                  ("=WORKDAY.INTL(1,10,\"1111111\")" . "#VALUE!")))
    (should (equal (grid-table-formula-test--eval (car case) "abc")
                   (cdr case)))))

(ert-deftest grid-table-formula-xlookup-finds-an-empty-cell-for-a-blank ()
  ;; XLOOKUP and XMATCH match a blank with an empty cell; MATCH does not.
  (should (equal (grid-table-formula-test--values
                  '(("=XLOOKUP(B1,C1:C3,D1:D3,\"none\")" "" "1" "a")
                    ("=XMATCH(B1,C1:C3)" "" "" "b")
                    ("=MATCH(B1,C1:C3,0)" "" "3" "c")))
                 '(("b" "" "1" "a") (2 "" "" "b") ("#N/A" "" "3" "c")))))

(ert-deftest grid-table-formula-xlookup-nearest-crosses-kinds ()
  ;; Numbers sort below text, text below FALSE and TRUE; blanks and
  ;; errors are passed over.  B1:B5 hold 2, 5, TRUE, "ala" and #DIV/0!.
  (dolist (case '(("=XLOOKUP(10,B1:B5,C1:C5,,1)" . "g")
                  ("=XLOOKUP(\"qwerty\",B1:B5,C1:C5,,1)" . "f")
                  ("=XLOOKUP(FALSE,B1:B5,C1:C5,,-1)" . "g")
                  ("=XLOOKUP(1,B1:B5,C1:C5,,-1)" . "#N/A")
                  ("=XLOOKUP(TRUE,B1:B6,C1:C6,,1)" . "f")
                  ("=XLOOKUP(\"zz\",B1:B6,C1:C6,,1)" . "f")))
    (should (equal (car (car (grid-table-formula-test--values
                              `((,(car case) "2" "a") ("" "5" "b")
                                ("" "TRUE" "f") ("" "ala" "g")
                                ("" "=1/0" "h") ("" "" "i")))))
                   (cdr case)))))

(ert-deftest grid-table-formula-xmatch-by-halves-finds-as-excel-does ()
  ;; Of equal values, the search by halves finds the first in values
  ;; sorted up and the last in values sorted down; the next smaller one
  ;; in values sorted down is the first of its run.
  (dolist (case '(("=XMATCH(5,{1,5,5,5,9},0,2)" . 2)
                  ("=XMATCH(5,{9,5,5,5,1},0,-2)" . 4)
                  ("=XMATCH(6,{1,5,5,5,9},1,2)" . 5)
                  ("=XMATCH(6,{9,5,5,5,1},-1,-2)" . 2)
                  ("=XMATCH(0,{1,5,9},-1,2)" . "#N/A")))
    (should (equal (grid-table-formula-test--eval (car case)) (cdr case)))))

(ert-deftest grid-table-formula-xmatch-nearest-takes-a-blank-above-all ()
  ;; B1 is blank: the next smaller value is the largest, as Excel sorts.
  (should (equal (grid-table-formula-test--eval
                  "=XMATCH(B1,{FALSE,2.1,TRUE,4.1},-1)" "") 3))
  (should (equal (grid-table-formula-test--eval
                  "=XMATCH(B1,{\"a\",2.1,\"c\",4.1},-1)" "") 3))
  (should (equal (grid-table-formula-test--eval
                  "=XMATCH(B1,{-1.1,2.1,3.1,4.1},1)" "") "#N/A")))

(ert-deftest grid-table-formula-xmatch-halves-values-not-sorted-as-excel-does ()
  ;; The middle of the first and last value, both searched, rounded down
  (dolist (case '(("=XMATCH(4.1,{\"\",\"a\",1.1,4.1},0,2)" . "#N/A")
                  ("=XMATCH(4.1,{\"c\",4.1,\"a\",1.1},0,2)" . 2)
                  ("=XMATCH(4.1,{\"\",4.1,\"a\",1.1},1,2)" . 2)
                  ("=XMATCH(4.1,{TRUE,4.1,FALSE,\"c\"},-1,-2)" . 2)
                  ("=XMATCH(FALSE,{\"a\",2,FALSE,4.1,3,2.1,2,FALSE,9,9},-1,2)" . 8)))
    (should (equal (grid-table-formula-test--eval (car case)) (cdr case)))))

(ert-deftest grid-table-formula-xlookup-takes-no-wildcards-by-halves ()
  (should (equal (grid-table-formula-test--eval "=XMATCH(\"b\",{\"a\",\"b\"},2,2)")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=XMATCH(\"b*\",{\"a\",\"bc\"},2)")
                 2)))

(ert-deftest grid-table-formula-xmatch-wants-one-row-or-column ()
  (should (equal (grid-table-formula-test--eval "=XMATCH(2,{1,2;3,4})") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=XLOOKUP(2,{1,2;3,4},{1,2;3,4})")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=MATCH(2,{1,2;3,4},0)") "#N/A")))

(ert-deftest grid-table-formula-vstack-and-hstack-fill-out-with-na ()
  ;; A shorter array is filled out with #N/A, as Excel fills it.
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=VSTACK({1;2},{3;4;5})" 5 1))
                 '((1) (2) (3) (4) (5))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=VSTACK({1,2},{3})" 2 2))
                 '((1 2) (3 "#N/A"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=HSTACK({1;2},{3;4;5})" 3 2))
                 '((1 3) (2 4) ("#N/A" 5)))))

(ert-deftest grid-table-formula-tocol-and-torow-read-a-line-first ()
  ;; A row after a row, or a column after a column with scan_by_column.
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TOCOL({1,2;3,4})" 4 1))
                 '((1) (2) (3) (4))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TOCOL({1,2;3,4},0,TRUE)" 4 1))
                 '((1) (3) (2) (4))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TOROW({1;2;3})" 1 3))
                 '((1 2 3))))
  ;; IGNORE leaves out 1 blanks, 2 error values, 3 both.
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TOCOL({1,NA();3,4},2)" 3 1))
                 '((1) (3) (4))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TOCOL({1,NA();3,4},3)" 3 1))
                 '((1) (3) (4))))
  (let ((sheet (grid-table-formula-test--values
                '(("=ROWS(TOCOL(B1:B2))" "=1") ("" "")))))
    (should (equal (car sheet) '(2 1)))
    (should (equal (car (grid-table-formula-test--values
                         '(("=ROWS(TOCOL(B1:B2,1))" "=1") ("" ""))))
                   '(1 1)))))

(ert-deftest grid-table-formula-wraprows-and-wrapcols-wrap-a-vector ()
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=WRAPROWS({1,2,3,4,5},2)" 3 2))
                 '((1 2) (3 4) (5 "#N/A"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=WRAPROWS({1,2,3},2,\"x\")" 2 2))
                 '((1 2) (3 "x"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=WRAPCOLS({1,2,3,4,5},2)" 2 3))
                 '((1 3 5) (2 4 "#N/A"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=WRAPCOLS({1},2,0)" 2 1))
                 '((1) (0))))
  ;; Not a vector, or no count to wrap by.
  (should (equal (grid-table-formula-test--eval "=WRAPROWS({1,2;3,4},2)")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=WRAPROWS({1,2},0)") "#NUM!")))

(ert-deftest grid-table-formula-take-and-drop-count-from-the-last ()
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TAKE({1;2;3;4;5},2)" 2 1))
                 '((1) (2))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TAKE({1;2;3;4;5},-2)" 2 1))
                 '((4) (5))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=DROP({1;2;3;4;5},2)" 3 1))
                 '((3) (4) (5))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=DROP({1;2;3;4;5},-2)" 3 1))
                 '((1) (2) (3))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TAKE({1,2,3;4,5,6},2,2)" 2 2))
                 '((1 2) (4 5))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TAKE({1,2,3;4,5,6},,-2)" 2 2))
                 '((2 3) (5 6))))
  ;; Dropping none keeps it all.
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=DROP({1;2},0)" 2 1))
                 '((1) (2))))
  ;; None taken, or all of it dropped, is no array at all.
  (should (equal (grid-table-formula-test--eval "=TAKE({1;2},0)") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=DROP({1;2},2)") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=DROP({1;2},1,1)") "#VALUE!")))

(ert-deftest grid-table-formula-choosecols-picks-columns ()
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=CHOOSECOLS({1,2,3;4,5,6},3,1)" 2 2))
                 '((3 1) (6 4))))
  (should (equal (grid-table-formula-test--eval "=CHOOSECOLS({1,2,3},-1)") 3))
  (should (equal (grid-table-formula-test--eval "=CHOOSECOLS({1,2,3},4)")
                 "#VALUE!")))

(ert-deftest grid-table-formula-choosecols-reads-places-as-values ()
  "A cell's text and TRUE are places too; a block of places is #VALUE!."
  (should (equal (grid-table-formula-test--eval "=CHOOSECOLS({1,2,3},B1)" "TRUE") 1))
  (should (equal (grid-table-formula-test--eval "=CHOOSECOLS({1,2,3},B1)" "'2") 2))
  (should (equal (grid-table-formula-test--eval "=CHOOSEROWS({1;2;3},B1)" "'3") 3))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=CHOOSECOLS({1,2,3},{-3,1})" 1 2))
                 '((1 1))))
  (should (equal (grid-table-formula-test--eval "=CHOOSECOLS({1,2,3},{1,2;2,1})")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=CHOOSEROWS({1;2;3},{1;2},{1,2;2,1})")
                 "#VALUE!")))

(ert-deftest grid-table-formula-expand-fills-out-to-its-size ()
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=EXPAND({1,2},2,3,0)" 2 3))
                 '((1 2 0) (0 0 0))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=EXPAND({1},2)" 2 1))
                 '((1) ("#N/A"))))
  ;; A size left out stays as it is; below the array is #VALUE!.
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=EXPAND({1,2},1)" 1 2))
                 '((1 2))))
  (should (equal (grid-table-formula-test--eval "=EXPAND({1,2},1,1)") "#VALUE!")))

(ert-deftest grid-table-formula-sortby-sorts-by-other-values ()
  ;; The last array sorts first; the one before it keeps what it leaves.
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=SORTBY({10;20;30;40},{2;1;2;1},1,{1;2;3;4},-1)" 4 1))
                 '((40) (20) (30) (10))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=SORTBY({1;2;3},{3;1;2},-1)" 3 1))
                 '((1) (3) (2))))
  ;; A row of values sorts the columns of the array.
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=SORTBY({1,2,3},{3,1,2})" 1 3))
                 '((2 3 1))))
  ;; Not as many values as the array has, or an order that is no order.
  (should (equal (grid-table-formula-test--eval "=SORTBY({1;2;3},{1;2})")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=SORTBY({1;2;3},{1;2;3},2)")
                 "#VALUE!")))

(ert-deftest grid-table-formula-sort-takes-several-places-and-puts-blanks-last ()
  "SORT sorts by each place in turn, and a blank goes after every value."
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=SORT({1,2;1,1;0,3},{1,2},{1,-1})" 3 2))
                 '((0 3) (1 2) (1 1))))
  ;; Not an order for each place, or a block of places.
  (should (equal (grid-table-formula-test--eval "=SORT({2,1,2,1},1,{1;-1})")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=SORT({1,2;3,4},{1,2;1,2})")
                 "#VALUE!"))
  (should (equal (mapcar #'car (grid-table-formula-test--values
                                '(("=SORT(B1:B3)" "2") ("" "") ("" "1"))))
                 '(1 2 0)))
  (should (equal (mapcar #'car (grid-table-formula-test--values
                                '(("=SORTBY({1;2;3},B1:B3,-1)" "") ("" "5") ("" "7"))))
                 '(3 2 1))))

(ert-deftest grid-table-formula-textsplit-splits-at-its-delimiters ()
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=TEXTSPLIT(\"a,b;c,d\",\",\",\";\")" 2 2))
                 '(("a" "b") ("c" "d"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=TEXTSPLIT(\"a,,b\",\",\")" 1 3))
                 '(("a" "" "b"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=TEXTSPLIT(\"a,,b\",\",\",,TRUE)" 1 2))
                 '(("a" "b"))))
  ;; Uneven rows are filled out; a delimiter may be several values.
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=TEXTSPLIT(\"a,b;c\",\",\",\";\")" 2 2))
                 '(("a" "b") ("c" "#N/A"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=TEXTSPLIT(\"a1b2c\",{\"1\",\"2\"})" 1 3))
                 '(("a" "b" "c"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=TEXTSPLIT(\"aXbxc\",\"X\",,FALSE,1)" 1 3))
                 '(("a" "b" "c"))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=TEXTSPLIT(\"a-b;c-d\",\"-\",\";\",FALSE,0,\"?\")" 2 2))
                 '(("a" "b") ("c" "d"))))
  ;; No delimiter to split at, or an empty text among them.
  (should (equal (grid-table-formula-test--eval "=TEXTSPLIT(\"abc\",)") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=TEXTSPLIT(\"abc\",\"\")")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=TEXTSPLIT(\"abc\",,\"\")")
                 "#VALUE!")))

(ert-deftest grid-table-formula-text-around-a-delimiter-as-excel-gives-it ()
  (let ((text "\"123 the 456 the 891\""))
    ;; A fraction is taken down; the end of the text may be a delimiter.
    (should (equal (grid-table-formula-test--eval
                    (format "=TEXTAFTER(%s,\"the\",-1.2)" text))
                   " 456 the 891"))
    (should (equal (grid-table-formula-test--eval
                    (format "=TEXTBEFORE(%s,\"x\",1,0,1)" text))
                   "123 the 456 the 891"))
    (should (equal (grid-table-formula-test--eval
                    (format "=TEXTAFTER(%s,\"x\",-1,0,1)" text))
                   "123 the 456 the 891"))
    (should (equal (grid-table-formula-test--eval
                    (format "=TEXTAFTER(%s,\"x\",2,0,1,\"no\")" text))
                   "no"))
    (should (equal (grid-table-formula-test--eval
                    (format "=TEXTAFTER(%s,\"x\",1,0,2)" text))
                   "#VALUE!")))
  ;; What is given when nothing is found is computed only then, and a
  ;; blank cell is empty text.
  (should (equal (grid-table-formula-test--eval "=TEXTAFTER(\"a-b\",\"-\",1,0,0,1/0)")
                 "b"))
  (should (equal (grid-table-formula-test--eval "=TEXTAFTER(\"a-b\",\"x\",1,0,0,B1)" "")
                 ""))
  ;; An instance given past where the delimiter could stand is #VALUE!.
  (should (equal (grid-table-formula-test--eval "=TEXTAFTER(\"del\",\"deli\",1)")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=TEXTAFTER(\"del\",\"deli\")")
                 "#N/A")))

(ert-deftest grid-table-formula-randarray-keeps-to-its-size-and-range ()
  (should (equal (grid-table-formula-test--eval "=ROWS(RANDARRAY(3,2))") 3))
  (should (equal (grid-table-formula-test--eval "=COLUMNS(RANDARRAY(3,2))") 2))
  (should (equal (grid-table-formula-test--eval "=ROWS(RANDARRAY())") 1))
  (should (<= 0 (grid-table-formula-test--eval "=SUM(RANDARRAY(20,20))") 400))
  ;; Whole numbers count both ends in.
  (let ((sum (grid-table-formula-test--eval "=SUM(RANDARRAY(20,20,2,3,TRUE))")))
    (should (and (<= 800 sum 1200) (= sum (truncate sum)))))
  (should (equal (grid-table-formula-test--eval "=RANDARRAY(0)") "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=RANDARRAY(1,1,3,2)") "#VALUE!")))

(ert-deftest grid-table-formula-math-functions-excel-had-missing ()
  "The math functions added from Excel's list give what Excel gives.
The cases come from IronCalc's and the formulas package's workbooks."
  (dolist (case '(("=SEC(0)" 1.0) ("=COT(0)" "#DIV/0!") ("=ACOT(0)" 1.5707963267948966)
                  ("=ACOTH(0.5)" "#NUM!") ("=SQRTPI(-1)" "#NUM!")
                  ("=FACTDOUBLE(7)" 105.0) ("=FACTDOUBLE(-1)" 1.0)
                  ("=FACTDOUBLE(-1.1)" "#NUM!") ("=SQRTPI(TRUE)" "#VALUE!")
                  ("=QUOTIENT(-7,2)" -3) ("=QUOTIENT(TRUE,1)" "#VALUE!")
                  ("=GCD(24,36)" 12) ("=LCM({4,\"8\",12})" 24)
                  ("=LCM(\"8\",2,B1)" 8) ("=LCM(B1:C1)" 0) ("=GCD(B1,C1)" "#VALUE!")
                  ("=GCD(,5)" "#N/A") ("=GCD(-0.0003,1)" "#NUM!") ("=GCD(\"\",1)" "#VALUE!")
                  ("=LCM(2^31-1,2^30-1)" "#NUM!")
                  ("=MULTINOMIAL(2,3,4)" 1260.0) ("=MULTINOMIAL({TRUE,1,2})" "#VALUE!")
                  ("=PERMUT(5,2)" 20.0) ("=PERMUTATIONA(3,2)" 9.0) ("=COMBINA(4,3)" 20)
                  ("=SERIESSUM(2,0,1,{1,2,3})" 17.0) ("=SERIESSUM(0,0,2,{1,2,3})" "#NUM!")
                  ("=ROMAN(1999)" "MCMXCIX") ("=ROMAN(1999,4)" "MIM") ("=ARABIC(\"MCMXCIX\")" 1999)))
    (let* ((model (grid-model-create 1 3 (list (list (car case) "" ""))))
           (value (grid-model-get-computed-value model 0 0)))
      (should (if (numberp (cadr case))
                  (and (numberp value) (= value (cadr case)))
                (equal (list (car case) value) case))))))

(ert-deftest grid-table-formula-engineering-units ()
  "CONVERT reads Excel's units, their prefixes and their temperatures.
The cases come from IronCalc's and the formulas package's workbooks."
  (cl-flet ((near (formula expected)
              (let ((value (grid-table-formula-test--eval formula)))
                (should (equal (list formula
                                     (and (numberp value)
                                          (< (abs (- value expected))
                                             (* 1e-9 (max 1 (abs value) (abs expected))))))
                               (list formula t))))))
    (near "=CONVERT(1,\"m\",\"m\")" 1)
    (near "=CONVERT(1,\"mi\",\"m\")" 1609.344)
    (near "=CONVERT(4,\"mi\",\"m\")" 6437.376)
    (near "=CONVERT(1,\"ang\",\"m\")" 1e-10)
    (near "=CONVERT(1,\"Mm\",\"m\")" 1000000)
    (near "=CONVERT(0.0002,\"Ml\",\"m3\")" 0.2)
    (near "=CONVERT(0.0002,\"Mm3\",\"m3\")" 2e14)
    (near "=CONVERT(0.05,\"Mm2\",\"m2\")" 5e10)
    (near "=CONVERT(0.05,\"Mar\",\"m2\")" 5000000)
    (near "=CONVERT(1,\"kibyte\",\"bit\")" 8192)
    (near "=CONVERT(1,\"Mbyte\",\"bit\")" 8000000)
    (near "=CONVERT(1,\"mbyte\",\"bit\")" 0.008)
    (near "=CONVERT(1,\"Ysec\",\"sec\")" 1e+24)
    (near "=CONVERT(1,\"min\",\"sec\")" 60)
    (near "=CONVERT(0.05,\"Matm\",\"Pa\")" 5066250000)
    (near "=CONVERT(1,\"e\",\"J\")" 1e-07)
    (near "=CONVERT(10,\"J\",\"e\")" 100000000)
    (near "=CONVERT(1,\"T\",\"ga\")" 10000)
    (near "=CONVERT(13,\"C\",\"F\")" 55.400000000000006)
    (near "=CONVERT(1,\"F\",\"K\")" 255.92777777777775)
    (near "=CONVERT(1,\"Rank\",\"K\")" 0.5555555555555556)
    (near "=CONVERT(1,\"Reau\",\"K\")" 274.4)
    (near "=CONVERT(12,\"MK\",\"K\")" 12000000)
    ;; pc is a parsec to a metre, and a picocalorie to a joule.
    (near "=CONVERT(1,\"pc\",\"J\")" 4.184e-12)
    (should (equal (grid-table-formula-test--eval "=CONVERT(1,\"C\",\"F\")") 33.8))
    (dolist (formula '("=CONVERT(12,\"MC\",\"K\")" "=CONVERT(0.05,\"Mha\",\"m2\")"
                       "=CONVERT(0.05,\"Mft2\",\"m2\")" "=CONVERT(1,\"g\",\"m\")"
                       "=CONVERT(1,\"parsnip\",\"g\")"))
      (should (equal (list formula (grid-table-formula-test--eval formula))
                     (list formula "#N/A"))))
    (should (equal (grid-table-formula-test--eval "=CONVERT(TRUE,\"g\",\"g\")")
                   "#VALUE!"))))

(ert-deftest grid-table-formula-engineering-bessel ()
  "The Bessel functions answer as Excel's do, the order cut off at the point.
The cases come from IronCalc's and the formulas package's workbooks."
  (cl-flet ((near (formula expected)
              (let ((value (grid-table-formula-test--eval formula)))
                (should (equal (list formula
                                     (and (numberp value)
                                          (< (abs (- value expected))
                                             (* 1e-9 (max 1 (abs value) (abs expected))))))
                               (list formula t))))))
    (near "=BESSELI(1,1)" 0.5651590975819435)
    (near "=BESSELI(-1,2)" 0.13574766658069928)
    (near "=BESSELI(2.3,2)" 1.0054316669125958)
    (near "=BESSELI(3.2,2)" 2.7882984611029293)
    (near "=BESSELI(1,1.999)" 0.5651590975819435)
    (near "=BESSELI(0,10000000)" 0)
    (near "=BESSELI(0.2,7)" 1.9866085100212153e-11)
    (near "=BESSELI(1e-10,5)" 2.6041666666666672e-54)
    (near "=BESSELJ(1,1)" 0.4400505856771301)
    (near "=BESSELJ(2.3,2)" 0.41391458970875217)
    (near "=BESSELJ(100000,5)" 0.001846551246313725)
    (near "=BESSELJ(10000000,2)" 8.683739567294922e-05)
    (near "=BESSELJ(2,30)" 3.650256266474097e-33)
    (near "=BESSELJ(-32,7)" 0.07621034879110145)
    (near "=BESSELK(1,1)" 0.6019072316669057)
    (near "=BESSELK(2.3,2)" 0.1617333633782021)
    (near "=BESSELK(0.2,7)" 3594005998.103765)
    (near "=BESSELK(1e-10,5)" 3.8400000000000003e+52)
    (near "=BESSELK(2,30)" 4.2711258180315126e+30)
    (near "=BESSELK(10000000,0)" 0)
    (near "=BESSELY(1,1)" -0.7812128209531197)
    (near "=BESSELY(0.2,7)" -2295654720.15733)
    (near "=BESSELY(1e-10,0)" -14.732516312945261)
    (near "=BESSELY(1.5,12)" -422217415.9345024)
    (near "=BESSELY(2,30)" -2.9132238172293094e+30)
    (dolist (formula '("=BESSELI(1,-1)" "=BESSELI(10000000,2)" "=BESSELK(0,1)"
                       "=BESSELY(-1,0)"))
      (should (equal (list formula (grid-table-formula-test--eval formula))
                     (list formula "#NUM!"))))
    (should (equal (grid-table-formula-test--eval "=BESSELJ(TRUE,1)") "#VALUE!"))
    (should (equal (grid-table-formula-test--eval "=BESSELJ(1,\"x\")") "#VALUE!"))))

(ert-deftest grid-table-formula-engineering-complex-more ()
  "The complex tangent, cotangent, secant and their kin, as text."
  (dolist (case '(("=IMTAN(\"3.1+2j\")" . "-0.00293552600273488+0.964145352213088j")
                  ("=IMCOT(\"3.1+2j\")" . "-0.00315788915435829-1.03717839601561j")
                  ("=IMCSC(\"3.1+2j\")" . "0.0118908807944521+0.275445903842597j")
                  ("=IMSEC(\"3.1+2j\")" . "-0.26560479396574+0.0106559588386141j")
                  ("=IMCSCH(\"3.1+2j\")" . "-0.0373189117262454-0.0818749558100934j")
                  ("=IMSECH(\"3.1+2j\")" . "-0.0376700432543674-0.0819771357068288j")
                  ("=IMSINH(\"3.1+2j\")" . "-4.60943006026724+10.1127515524169j")
                  ("=IMCOSH(\"3.1+2j\")" . "-4.62817714333229+10.0717884285999j")
                  ("=IMTAN(\"0.0000000000000001\")" . "0.0000000000000001")
                  ("=IMCOT(\"0.0000000000000001\")" . "1E+16")
                  ("=IMCSCH(\"0.0000000000000001\")" . "1E+16")
                  ("=IMSECH(0)" . "1")
                  ("=IMTAN(TRUE)" . "#VALUE!")))
    (should (equal (list (car case) (grid-table-formula-test--eval (car case)))
                   (cons (car case) (list (cdr case)))))))

(ert-deftest grid-table-formula-engineering-complex-parts-in-full ()
  "A complex number writes its parts out in full where the digits fit."
  (dolist (case '(("=COMPLEX(0.0000000000000001,0)" . "0.0000000000000001")
                  ("=COMPLEX(0.00000000000000001,0)" . "0.00000000000000001")
                  ("=COMPLEX(0.00000000000000000001,0)" . "1E-20")
                  ("=COMPLEX(0.0000012,0)" . "0.0000012")
                  ("=COMPLEX(1.2E-45,-3)" . "1.2E-45-3i")
                  ("=IMPRODUCT(\"7+0i\",\"\")" . "0")
                  ("=IMSUM(\"\",\"2+3i\")" . "2+3i")))
    (should (equal (list (car case) (grid-table-formula-test--eval
                                     (car case)))
                   (cons (car case) (list (cdr case))))))
  ;; An empty cell counts as zero to the product as well.
  (should (equal (grid-table-formula-test--eval "=IMPRODUCT(B1,C1)" "" "") "0"))
  (should (equal (grid-table-formula-test--eval "=IMPRODUCT(\"7+0i\",B1)" "") "0")))

(ert-deftest grid-table-formula-engineering-number-systems-places ()
  "A negative number's PLACES, and an empty one, as Excel takes them."
  (dolist (case '(("=BIN2HEX(\"1100011011\",2)" . "FFFFFFFF1B")
                  ("=BIN2HEX(\"1100011011\",0)" . "#NUM!")
                  ("=BIN2HEX(\"1100011011\",1)" . "FFFFFFFF1B")
                  ("=BIN2HEX(\"1100011011\",2.3)" . "FFFFFFFF1B")
                  ("=BIN2HEX(\"1100011011\",10)" . "FFFFFFFF1B")
                  ("=BIN2HEX(\"1100011011\",11)" . "#NUM!")
                  ("=BIN2HEX(\"1100011011\",TRUE)" . "#VALUE!")
                  ("=BIN2OCT(\"1100011011\",-2)" . "#NUM!")
                  ("=BIN2OCT(\"1111111111\",2)" . "7777777777")
                  ("=BIN2HEX(100,0)" . "#NUM!")
                  ("=BIN2OCT(B1)" . "0")
                  ("=BIN2DEC(C1)" . 0)
                  ("=DEC2HEX(12)" . "C")))
    (should (equal (list (car case) (grid-table-formula-test--eval (car case) "" ""))
                   (cons (car case) (list (cdr case)))))))
(ert-deftest grid-table-formula-database-criteria-as-excel-reads-them ()
  "DCOUNT reads names without regard to case, text criteria as the start
of the text, and gives #VALUE! for a field it cannot find.  The cases
come from IronCalc's DCOUNT workbook."
  (let ((rows '(("Region" "Units" "" "region" "Units" "Nope")
                ("East" "120" "" "ea" ">100" "x")
                ("West" "80" "" "" "" "")
                ("Eastern" "200" "" "" "" ""))))
    (dolist (case '(("=DCOUNT(A1:B4,\"units\",D1:D2)" 2)
                    ("=DCOUNT(A1:B4,2,E1:E2)" 2)
                    ("=DCOUNT(A1:B4,TRUE,D1:D2)" 0)
                    ("=DCOUNT(A1:B4,,D1:D2)" 2)
                    ("=DCOUNT(A1:B4,2,F1:F2)" 0)
                    ("=DCOUNT(A1:B4,2,F1:F3)" 3)
                    ("=DCOUNT(A1:B1,2,D1:D2)" "#VALUE!")
                    ("=DCOUNT(A1:B4,0,D1:D2)" "#VALUE!")
                    ("=DCOUNT(A1:B4,3,D1:D2)" "#VALUE!")
                    ("=DCOUNT(A1:B4,C1,D1:D2)" "#VALUE!")
                    ("=DCOUNT(A1:B4,A1:B1,D1:D2)" "#VALUE!")))
      (let ((model (grid-model-create 5 6 (append rows (list (list (car case) "" "" "" "" ""))))))
        (should (equal (list (car case) (grid-model-get-computed-value model 4 0))
                       case))))))

(ert-deftest grid-table-formula-database-functions-reduce-their-field ()
  "DSUM and its like read the numbers in the field of the rows meeting
the criteria; DCOUNTA and DGET read every value there."
  (let ((rows '(("Region" "Units" "" "Region" "Region")
                ("East" "120" "" "East" "West")
                ("West" "80" "" "" "")
                ("East" "200" "" "" "")
                ("East" "n/a" "" "" ""))))
    (dolist (case '(("=DSUM(A1:B5,2,D1:D2)" 320) ("=DAVERAGE(A1:B5,2,D1:D2)" 160)
                    ("=DMIN(A1:B5,2,D1:D2)" 120) ("=DMAX(A1:B5,2,D1:D2)" 200)
                    ("=DPRODUCT(A1:B5,2,D1:D2)" 24000) ("=DVAR(A1:B5,2,D1:D2)" 3200)
                    ("=DVARP(A1:B5,2,D1:D2)" 1600) ("=DSTDEVP(A1:B5,2,D1:D2)" 40)
                    ("=DCOUNTA(A1:B5,2,D1:D2)" 3) ("=DCOUNTA(A1:B5,,D1:D2)" 3)
                    ("=DGET(A1:B5,2,E1:E2)" 80) ("=DGET(A1:B5,2,D1:D2)" "#NUM!")
                    ("=DGET(A1:B5,2,D1:D3)" "#NUM!")
                    ("=DMIN(A1:B5,2,E1:E2)" 80) ("=DAVERAGE(A1:B1,2,D1:D2)" "#VALUE!")))
      (let* ((model (grid-model-create 6 5 (append rows (list (list (car case) "" "" "" "")))))
             (value (grid-model-get-computed-value model 5 0)))
        (should (if (numberp (cadr case))
                    (and (numberp value) (= value (cadr case)))
                  (equal (list (car case) value) case)))))))

(ert-deftest grid-table-formula-statistics-excel-had-missing ()
  "The statistics functions added from Excel's list give what Excel gives.
The cases come from the formulas package's and pycel's workbooks."
  (dolist (case '(("=SLOPE({0,0.1,0.2},{0.2,0.3,-0.1})" -0.34615384615384615)
                  ("=SLOPE({1,2},{3,3})" "#DIV/0!") ("=SLOPE({1,2},{3,4,5})" "#N/A")
                  ("=FORECAST.LINEAR(0.1,{1,7,3,0},{7,2,9,4})" 5.077586206896552)
                  ("=STEYX({1,2},{3,4})" "#DIV/0!") ("=FISHER(1)" "#NUM!")
                  ("=FISHER(0.1)" 0.10033534773107562) ("=FISHERINV(1)" 0.7615941559557649) ("=GEOMEAN(1,0)" "#NUM!")
                  ("=HARMEAN(1,2,4)" 1.7142857142857142) ("=TRIMMEAN({1,2,3,4},1)" "#NUM!")
                  ("=PROB({1,2,3,4,5},{0.1,0.2,0.3,0.3,0.1},2,3)" 0.5)
                  ("=PROB({1,2},{0.5,0.6},1)" "#NUM!")
                  ("=BETADIST(0.7,1,3,0.5,5)" 0.12749519890260627)
                  ("=BETADIST(0,,,)" "#NUM!") ("=LOGNORMDIST(0,,)" "#NUM!")))
    (let* ((model (grid-model-create 1 3 (list (list (car case) "" ""))))
           (value (grid-model-get-computed-value model 0 0)))
      (should (if (numberp (cadr case))
                  (and (numberp value) (< (abs (- value (cadr case))) 1e-12))
                (equal (list (car case) value) case))))))

(ert-deftest grid-table-formula-regression-as-excel-fits-it ()
  "LINEST, LOGEST, TREND and GROWTH fit and spill as Excel does.
A variable that adds nothing gets the slope 0, and an error among the
values is #VALUE!.  The cases come from pycel's and the formulas
package's workbooks."
  (dolist (case '(("=INDEX(LINEST({1;2;3},{2;3;4},FALSE,FALSE),1)" 0.6896551724137931)
                  ("=INDEX(LINEST({1;2;3},{2;3;4},TRUE,TRUE),3,1)" 1.0)
                  ("=INDEX(LINEST({1;2;3},{2;3;4},TRUE,TRUE),5,1)" 2.0)
                  ("=INDEX(LINEST({1;2;3},{2;3;4},TRUE,TRUE),3,2)" 0.0)
                  ("=INDEX(LINEST({1;2;3},{2,1;3,5;4,2},TRUE,TRUE),4,3)" "#N/A")
                  ("=INDEX(LINEST({1,2,4},{3,3,3}),1)" 0.0)
                  ("=INDEX(LINEST({1,2,4},{3,3,3}),2)" 2.3333333333333335)
                  ("=LINEST({1,2,4},{3,4})" "#REF!")
                  ("=LINEST({1,2,4},{3,4,#N/A})" "#VALUE!")
                  ("=LOGEST({1,2,-4})" "#NUM!")
                  ("=INDEX(TREND({1,2,4},{1,2,3},{4,5}),2)" 6.833333333333334)
                  ("=INDEX(GROWTH({1,2,4},{1,2,3},{4}),1)" 8.0)))
    (let* ((model (grid-model-create 1 3 (list (list (car case) "" ""))))
           (value (grid-model-get-computed-value model 0 0)))
      (should (if (numberp (cadr case))
                  (and (numberp value) (< (abs (- value (cadr case))) 1e-9))
                (equal (list (car case) value) case))))))
(ert-deftest grid-table-formula-lambda-is-called-as-excel-calls-it ()
  "LAMBDA is called where it is made, through a LET name or a defined
name, takes references as references, and leaves out what it is not
given.  The cases come from IronCalc's workbooks."
  (dolist (case '(("=LAMBDA(x, x+1)(5)" 6)
                  ("=LAMBDA(x,y, SQRT(x*x+y*y))(3,4)" 5)
                  ("=LAMBDA(x,LAMBDA(y,x*y))(3)(4)" 12)
                  ("=LET(area, LAMBDA(x,y,x*y), area(B1,C1))" 42)
                  ("=LAMBDA(x, ISREF(x))(B1:C1)" t)
                  ("=LAMBDA(x, ISREF(x))(B1+1)" nil)
                  ("=LET(x, B1, ISREF(x))" t)
                  ("=LAMBDA(x, SUM(x))(B1:C1*2)" 26)
                  ("=LAMBDA(x,y, IF(ISOMITTED(y),\"none\",x+y))(1,)" "none")
                  ("=LAMBDA(x,y, IF(ISOMITTED(y),x/2,x*y))(B1)" 3)
                  ("=LAMBDA(x,x)(1,2)" "#VALUE!")
                  ("=LAMBDA(x,x,x)(1,2)" "#VALUE!")
                  ("=LAMBDA(x,x+1)" "#CALC!")
                  ("=area(1)" "#NAME?")))
    (should (equal (list (car case)
                         (grid-table-formula-test--eval (car case) "6" "7"))
                   case)))
  ;; An edit reaches a LAMBDA's arguments and what its body reads.
  (let ((model (grid-model-create
                1 4 '(("=LAMBDA(x,x*2)(B1)" "5" "=LET(f,LAMBDA(x,x+D1),f(B1))" "1")))))
    (grid-model-set-raw-value model 0 1 "10")
    (should (equal (grid-model-get-computed-value model 0 0) 20))
    (should (equal (grid-model-get-computed-value model 0 2) 11))
    (grid-model-set-raw-value model 0 3 "3")
    (should (equal (grid-model-get-computed-value model 0 2) 13)))
  ;; A name defined as a LAMBDA is called by its name.
  (let* ((one (make-default-data-source '("" "") '(("=Twice(B1)" "4"))))
         (workbook (grid-workbook-create (list (cons "Sheet1" one)))))
    (grid-workbook-define-name workbook "Twice" "=LAMBDA(x,x*2)")
    (should (equal (grid-get-computed-value-at one 0 0) 8))
    (grid-set-raw-value-at one 0 1 "5")
    (should (equal (grid-get-computed-value-at one 0 0) 10))))
(ert-deftest grid-table-formula-lambda-helpers-as-excel-has-them ()
  "BYROW, BYCOL, MAP, REDUCE, SCAN and MAKEARRAY call a LAMBDA, or a
built-in function named alone, as Excel does.  The cases come from
IronCalc's and the formulas package's workbooks."
  (dolist (case '(("=INDEX(BYROW({1,2;3,4},LAMBDA(x,SUM(x))),2)" 7)
                  ("=INDEX(BYCOL({1,2;3,4},SUM),2)" 6)
                  ("=INDEX(BYROW({1,2;3,4},LAMBDA(x,x)),1)" "#CALC!")
                  ("=INDEX(MAP({1,2;3,4},{5,6;7,8},LAMBDA(x,y,x*y)),2,2)" 32)
                  ("=MAP({1,2},LAMBDA(a,a+1),LAMBDA(b,b+3))" "#VALUE!")
                  ("=REDUCE(,{3,4},LAMBDA(a,b,SQRT(a*a+b*b)))" 5)
                  ("=REDUCE(1,{1,2,3},LAMBDA(a,b,a+b^2))" 15)
                  ("=INDEX(SCAN(1,{1,2;3,4},LAMBDA(a,b,a+b)),2,1)" 7)
                  ("=INDEX(SCAN(\"x\",{1,2},LAMBDA(a,b,a&b)),2)" "x12")
                  ("=INDEX(MAKEARRAY(2,3,LAMBDA(r,c,r*10+c)),2,3)" 23)
                  ("=MAKEARRAY(TRUE,1,LAMBDA(r,c,r+c))" 2)
                  ("=MAKEARRAY(\"1\",1,LAMBDA(r,c,r+c))" "#VALUE!")
                  ("=MAKEARRAY(0,1,LAMBDA(r,c,r+c))" "#VALUE!")
                  ("=TYPE(LAMBDA(x,x))" 128)))
    (should (equal (list (car case) (grid-table-formula-test--eval (car case)))
                   case))))
(ert-deftest grid-table-formula-regex-functions-read-patterns-as-excel ()
  "REGEXTEST, REGEXEXTRACT and REGEXREPLACE read a pattern as PCRE2
does.  The cases come from IronCalc's and the formulas package's
workbooks."
  (dolist (case '(("=REGEXTEST(\"Another\",\"[A-Z]her\")" nil)
                  ("=REGEXTEST(\"There\",\"[A-Z]her\")" t)
                  ("=REGEXTEST(\"ciao\",\"^ciaO$\",1)" t)
                  ("=REGEXTEST(\"ciao\",\"^ciaO$\")" nil)
                  ("=REGEXTEST(\"10\",\"^\\d+$\",2)" "#VALUE!")
                  ("=REGEXTEST(1.9,1.9)" t)
                  ("=REGEXTEST(\"a]b\",\"[]x]\")" t)
                  ("=REGEXTEST(\"a-b\",\"a[\\-+]b\")" t)
                  ("=REGEXTEST(\"ab\",\"(?=a)\")" "#VALUE!")
                  ("=REGEXEXTRACT(\"LonelyPlanet\",\"[A-Z][a-z]+\")" "Lonely")
                  ("=INDEX(REGEXEXTRACT(\"LonelyPlanet\",\"[A-Z][a-z]+\",1),2)" "Planet")
                  ("=INDEX(REGEXEXTRACT(\"1a2b3c\",\"((\\d)[bc]+|(3c))\",2),2)" "2")
                  ("=REGEXEXTRACT(\"Rees (378) 555-4197\",\"[0-9()]+ [0-9-]+\",2)" "#VALUE!")
                  ("=REGEXEXTRACT(\"1.9\",\"^\\d+$\")" "#N/A")
                  ("=REGEXEXTRACT(\"a12\",\"\\d{2}\")" "12")
                  ("=REGEXREPLACE(\"JamesBond\",\"([A-Z][a-z]+)([A-Z][a-z]+)\",\"$2, $1 $2\")"
                   "Bond, James Bond")
                  ("=REGEXREPLACE(\"a1b2c3\",\"\\d\",\"#\")" "a#b#c#")
                  ("=REGEXREPLACE(\"a1b2c3\",\"\\d\",\"#\",-1)" "a1b2c#")
                  ("=REGEXREPLACE(10,\"^\\d+$\",TRUE)" "TRUE")))
    (should (equal (list (car case) (grid-table-formula-test--eval (car case)))
                   case))))

(ert-deftest grid-table-formula-finance-finds-coupons-and-accrual ()
  "COUP*, ACCRINT and ACCRINTM as Excel has them.
The cases are the ones the ironcalc and formulas workbooks keep
Excel's values for."
  (dolist (case '(("=COUPPCD(DATE(2020,1,1),DATE(2021,1,1),2,0)" . 43831)
                  ("=COUPNCD(DATE(2020,1,1),DATE(2021,1,1),2,0)" . 44013)
                  ("=COUPNUM(DATE(2020,1,1),DATE(2021,1,1),2,0)" . 2)
                  ("=COUPDAYBS(DATE(2020,1,1),DATE(2021,1,1),2,0)" . 0)
                  ("=COUPDAYSNC(DATE(2020,1,1),DATE(2021,1,1),2,0)" . 180)
                  ("=COUPDAYS(DATE(2020,1,1),DATE(2021,1,1),1,0)" . 360)
                  ("=COUPDAYS(DATE(2020,1,1),DATE(2021,1,1),4,3)" . 91.25)
                  ("=COUPDAYS(DATE(2020,1,1),DATE(2021,1,1),1,2)" . 360)
                  ("=COUPNUM(DATE(2020,1,1),DATE(2024,3,17),4,0)" . 17)
                  ("=COUPPCD(DATE(2020,1,1),DATE(2024,3,17),4,0)" . 43816)
                  ("=COUPNCD(DATE(2020,1,1),DATE(2024,3,17),4,0)" . 43907)
                  ("=COUPDAYBS(DATE(2020,1,1),DATE(2024,3,17),4,1)" . 15)
                  ("=COUPDAYSNC(DATE(2020,1,1),DATE(2024,3,17),4,0)" . 76)
                  ("=COUPNCD(DATE(2020,1,1),DATE(2021,1,1),3,0)" . "#NUM!")
                  ("=COUPDAYS(DATE(2021,1,1),DATE(2020,1,1),2,0)" . "#NUM!")
                  ("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2021,9,1),0.05,1000,2,0)"
                   . 12.5)
                  ("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2021,9,1),0.05,1000,2,1)"
                   . 12.568306010928962)
                  ("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2021,9,1),0.05,1000,2,2)"
                   . 12.777777777777777)
                  ("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2021,9,1),0.05,1000,2,3)"
                   . 12.602739726027398)
                  ("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2021,9,1),0.05,1000,2,4)"
                   . 12.5)
                  ("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2021,12,1),0.025,5000,2,0)"
                   . 62.5)
                  ("=ACCRINT(DATE(2008,3,1),DATE(2008,9,1),DATE(2008,5,1),0,1000,2,0)"
                   . "#NUM!")
                  ("=ACCRINT(DATE(2008,5,1),DATE(2008,9,1),DATE(2008,5,1),0.1,1000,2,0)"
                   . "#NUM!")
                  ("=ACCRINTM(DATE(2020,1,1),DATE(2021,1,1),0.1,1000,0)" . 100)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-prices-bonds-and-bills ()
  "PRICE, YIELD, the discounted securities and the bills, as Excel has them."
  (dolist (case '(("=PRICE(DATE(2020,3,1),DATE(2030,3,1),0.06,0.07,130,1,0)"
                   . 108.22689722310892)
                  ("=YIELD(DATE(2020,3,1),DATE(2030,3,1),0.06,108.2268972231089,130,1,0)"
                   . 0.07)
                  ("=DURATION(32000,43232,0.09,0.08,1)" . 11.816961230148491)
                  ("=MDURATION(32000,43232,0.09,0.08,1)" . 10.94163076865601)
                  ("=DISC(39472,39614,97.975,100,0)" . 0.05207142857142884)
                  ("=INTRATE(DATE(2027,2,15),DATE(2027,5,15),2000000,2040000,0)" . 0.08)
                  ("=RECEIVED(32000,43232,0.01,0.014,1)" . 0.017559927308391263)
                  ("=PRICEDISC(DATE(2020,1,15),DATE(2025,4,15),0.05,100,0)" . 73.75)
                  ("=YIELDDISC(DATE(2020,1,15),DATE(2025,4,15),95,100,0)"
                   . 0.010025062656641603)
                  ("=PRICEMAT(DATE(2020,1,15),DATE(2025,4,15),DATE(2019,3,1),0.05,0.06,0)"
                   . 94.962927756654)
                  ("=YIELDMAT(DATE(2020,1,15),DATE(2025,4,15),DATE(2019,3,1),0.05,98,0)"
                   . 0.05256832719519287)
                  ("=TBILLEQ(20000,20182,0.5)" . 0.6784386617100372)
                  ("=TBILLPRICE(20000,20182,0.5)" . 74.72222222222223)
                  ("=TBILLYIELD(20000,20182,200)" . -0.989010989010989)
                  ("=TBILLPRICE(20000,20182,0)" . "#NUM!")
                  ("=PRICE(DATE(2020,3,1),DATE(2030,3,1),0.06,-0.01,130,1,0)" . "#NUM!")
                  ("=ODDFPRICE(DATE(2020,3,1),DATE(2030,3,1),DATE(2020,1,15),DATE(2021,3,1),0.075,0.05,120,1,1)"
                   . 131.5377155865953)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-pays-and-depreciates ()
  "CUMIPMT, MIRR, PDURATION, RRI, DOLLARDE, DB and VDB as Excel has them."
  (dolist (case '(("=CUMIPMT(0.0083,36,8000,1,10,1)" . -520.1181191021385)
                  ("=CUMPRINC(0.0083,36,8000,1,10,1)" . -2038.518055313885)
                  ("=CUMIPMT(0.1,2,8000,1,10,1)" . "#NUM!")
                  ("=CUMIPMT(0.1,36,8000,3,10,1)" . -5676.255396537561)
                  ("=ISPMT(0.2,1,3,100)" . -13.333333333333336)
                  ("=ISPMT(0.2,1,0,100)" . "#DIV/0!")
                  ("=MIRR({-6,1},0.1,0.2)" . -0.8333333333333334)
                  ("=MIRR({5,6},0.1,0.2)" . "#DIV/0!")
                  ("=FVSCHEDULE(120,{0.2,0.4,0.6,0.8})" . 580.6080000000001)
                  ("=FVSCHEDULE(1,{0.09,0.11,0.1})" . 1.3308900000000004)
                  ("=FVSCHEDULE(1,{0.1,TRUE})" . "#VALUE!")
                  ("=PDURATION(0.2,100,200)" . 3.8017840169239268)
                  ("=PDURATION(0,100,200)" . "#NUM!")
                  ("=RRI(2,3,4)" . 0.15470053837925146)
                  ("=RRI(-1,3,4)" . "#NUM!")
                  ("=DOLLARDE(1.2345,3.2)" . 1.7816666666666663)
                  ("=DOLLARFR(1.7816666666666663,3.2)" . 1.2345)
                  ("=DOLLARDE(1.2345,0)" . "#DIV/0!")
                  ("=DB(100,10,5,3,1)" . 22.567920075)
                  ("=DB(100,10,4,3,13)" . "#NUM!")
                  ("=DB(100,10,4,3,9.2222)" . 16.529375400000003)
                  ("=VDB(2400,300,120,4.2,18,1.4,TRUE)" . 341.59973986675504)
                  ("=VDB(10000,1000,10,0,1)" . 2000)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-counts-whole-periods-once ()
  "A coupon period over before settlement counts as one, but only from the issue."
  (dolist (case '(("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2022,3,1),0.025,5000,2,2)"
                   . 93.75)
                  ("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2022,3,1),0.025,5000,2,2,FALSE)"
                   . 94.79166666666666)
                  ("=ACCRINT(DATE(2021,6,1),DATE(2021,12,1),DATE(2022,9,1),0.025,5000,2,2)"
                   . 156.94444444444443)
                  ("=ACCRINT(DATE(2005,1,15),DATE(2007,1,1),DATE(2009,4,15),1,1,4,0)"
                   . 4.25)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-finds-a-rate-below-nothing ()
  "IRR walks to a rate under zero, and XIRR refuses a rate of nothing or less."
  (dolist (case '(("=IRR({-5,1})" . -0.8)
                  ("=IRR({-2,1})" . -0.5)
                  ("=IRR({-70,12,15})" . -0.44350694133432755)
                  ("=IRR({-20,1,1,1,1,1})" . -0.3352800891362343)
                  ("=IRR({-400,1,1,1,1,1})" . -0.6737366751808405)
                  ("=IRR({-20,5,5,5,5,5},17)" . 0.07930826116414913)
                  ("=IRR({-20,5,5,5,5,5},-1)" . "#VALUE!")
                  ("=XIRR({-400,100,10,5,6,5},{44958,44986,45017,45078,45108,45139})"
                   . -0.9977533546974883)
                  ("=XIRR({\"hello\",1},{1,2})" . "#VALUE!")
                  ("=XNPV(-0.1,{-10,1,5,0.0001,4.5},{1,99,55,4,9})" . "#NUM!")
                  ("=XNPV(0,{-10,1,5,0.0001,4.5},{1,99,55,4,9})" . "#NUM!")
                  ("=XNPV(0.1,{-20,8,8,8,8,8},{-100,1,2,3,4,5})" . "#NUM!")))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-takes-a-fraction-of-a-period ()
  "DDB and IPMT take a period with a fraction, as Excel counts it."
  (dolist (case '(("=DDB(100,10.233,4.2312,3.2,1.5)" . 13.532735530775124)
                  ("=DDB(10000,1000,100,3.2,3)" . 280.55568287635316)
                  ("=DDB(100,10,4,3,2.7)" . 0.5624999999999964)
                  ("=IPMT(0.1,3.9,3,8000,10,1)" . -26.866364667656377)
                  ("=PPMT(0.1,3.9,3,8000,10,1)" . -2900.351432640776)
                  ("=IPMT(0.01,1.0001,36,8000,0,1)" . -79.2077378295619)
                  ("=IPMT(0.1,4,3,8000,10,1)" . "#NUM!")
                  ("=IPMT(0.01,1,36,8000,0,1)" . 0)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-refuses-what-excel-refuses ()
  "A rate at or below -1, and a logical value where a number is wanted."
  (dolist (case '(("=PMT(-3,5,100,300,1)" . "#NUM!")
                  ("=FV(-1,-2,1)" . "#DIV/0!")
                  ("=RATE(10,-1200,2000,0,1)" . "#NUM!")
                  ("=NOMINAL(10,TRUE)" . "#VALUE!")
                  ("=EFFECT(FALSE,2.9)" . "#VALUE!")
                  ("=NOMINAL(0.2,12)" . 0.1837136459967743)
                  ("=EFFECT(0.2,12)" . 0.21939108490523163)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-prices-a-bill-past-half-a-year ()
  "TBILLEQ past 182 days, as a bond paying twice a year would yield."
  (dolist (case '(("=TBILLEQ(DATE(2024,4,1),DATE(2025,3,4),0.3)" . 0.3883686423017574)
                  ("=TBILLEQ(2040,2400,0.8)" . 2.5066048253748145)
                  ("=TBILLEQ(2040,2223,0.8)" . 1.364497682211261)
                  ("=TBILLEQ(20000,20182,0.5)" . 0.6784386617100372)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-prices-a-bond-without-a-coupon ()
  "A bond that pays no coupon has a price and a yield."
  (dolist (case '(("=PRICE(DATE(2005,4,15),DATE(2009,7,31),0,0.065,100,4)"
                   . 75.82697729980659)
                  ("=YIELD(DATE(2005,4,15),DATE(2009,7,31),0,0.065,100,4)"
                   . 2.1336010409109454)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-keeps-coupons-to-the-month-end ()
  "A bond maturing on the last day of a month has its coupons on last days."
  (dolist (case '(("=COUPNCD(DATE(2000,1,1),DATE(2000,9,30),4)" . 36616)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-prices-an-odd-last-period ()
  "ODDLPRICE and ODDLYIELD, as Microsoft's examples give them."
  (dolist (case '(("=ODDLPRICE(DATE(2008,2,7),DATE(2008,6,15),DATE(2007,10,15),0.0375,0.0405,100,2,0)"
                   . 99.87828601472134)
                  ("=ODDLYIELD(DATE(2008,4,20),DATE(2008,6,15),DATE(2007,12,24),0.0375,99.875,100,2,0)"
                   . 0.04519223562916933)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-finance-depreciates-the-french-way ()
  "AMORLINC and AMORDEGRC, as the French accounting has them."
  (dolist (case '(("=AMORLINC(100,DATE(2005,1,15),DATE(2009,4,15),6,1,0.2,0)"
                   . 8.999999999999986)
                  ("=AMORLINC(100,DATE(2005,1,11),DATE(2009,4,15),7,1,0.2,0)"
                   . 7.7777777777777715)
                  ("=AMORLINC(100,DATE(2005,1,15),DATE(2009,4,15),12,0,0.2,0)"
                   . 85.00000000000001)
                  ("=AMORLINC(100,DATE(2005,1,15),DATE(2009,4,15),-1,1,0.2,0)" . "#NUM!")
                  ("=AMORDEGRC(100,DATE(2005,1,15),DATE(2009,4,15),6,0,0.2,0)" . 94)
                  ("=AMORDEGRC(100,DATE(2005,1,11),DATE(2009,4,15),7,1,0.2,0)" . 3)
                  ("=AMORDEGRC(100,DATE(2005,1,15),DATE(2009,4,15),4,1,0.2,0)" . 2)
                  ("=AMORDEGRC(100,DATE(2005,1,15),DATE(2009,4,15),9,1,0.2,0)" . 4)
                  ("=AMORDEGRC(100,DATE(2005,1,15),DATE(2009,4,15),0,1,0.2,0)" . 0)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-toolpak-function-takes-no-range-for-a-value ()
  ;; Excel hands the functions of the Analysis ToolPak, and some newer
  ;; ones like them, the reference itself: several cells where one value
  ;; is wanted are #VALUE!, neither intersected nor taken one at a time,
  ;; though an array is.
  (cl-flet ((value (formula legacy)
              (let ((model (grid-model-create
                            3 5 '(("1" "" "" "" "") ("-2" "" "" "" "")
                                  ("3" "" "" "" "")))))
                (grid-model-set-raw-value model 1 4 formula)
                (when legacy
                  (grid-model-set-legacy model '((1 . 4))))
                (grid-calc-forget model)
                (grid-model-recalculate-all model)
                (grid-model-get-computed-value model 1 4))))
    (pcase-dolist (`(,formula ,typed ,file)
                   '(("=DELTA(A1:A3)" "#VALUE!" "#VALUE!")
                     ("=DELTA(A2,-2)" 1 1)
                     ("=SUM(DELTA(A1:A3+2,0))" 1 1)
                     ("=SUM(GESTEP({1,-2,3}))" 2 2)
                     ("=ERF.PRECISE(A1:A3)" "#VALUE!" "#VALUE!")
                     ("=IMSINH(A1:A3)" "#VALUE!" "#VALUE!")
                     ("=ABS(A1:A3)" "#SPILL!" 2)))
      (should (equal (value formula nil) typed))
      (should (equal (value formula t) file)))))

(ert-deftest grid-table-formula-toolpak-functions-refuse-true-and-false ()
  ;; DELTA, GESTEP, ERF and COMPLEX, of the Analysis ToolPak, and the
  ;; newer error functions give #VALUE! for TRUE or FALSE, where ABS
  ;; takes FALSE as 0.
  (dolist (formula '("=DELTA(B1)" "=GESTEP(B1)" "=ERF(B1)" "=ERF(0,B1)"
                     "=ERFC(B1)" "=ERF.PRECISE(B1)" "=ERFC.PRECISE(B1)"
                     "=COMPLEX(1,B1)"))
    (should (equal (grid-table-formula-test--eval formula "FALSE") "#VALUE!")))
  (should (equal (grid-table-formula-test--eval "=ABS(B1)" "FALSE") 0))
  (should (equal (grid-table-formula-test--eval "=DELTA(B1)" "0") 1)))

(ert-deftest grid-table-formula-char-numbers-as-the-system-does ()
  ;; CHAR and CODE follow Windows-1252, or Mac Roman as Excel on a Mac.
  (dolist (case '(("=CHAR(128)" . "€") ("=CODE(\"€\")" . 128)
                  ("=CHAR(65)" . "A")))
    (grid-table-formula-test--should-be case))
  (let ((grid-calc-character-set 'mac-roman))
    (dolist (case '(("=CHAR(128)" . "Ä") ("=CHAR(160)" . "†")
                    ("=CHAR(198)" . "∆") ("=UNICODE(CHAR(240))" . #xF8FF)
                    ("=CODE(\"Ä\")" . 128) ("=CODE(\"∆\")" . 198)
                    ("=CODE(\"€\")" . 219) ("=CHAR(65)" . "A")))
      (grid-table-formula-test--should-be case))))

(ert-deftest grid-table-formula-file-arraytotext-takes-its-range-whole ()
  ;; ARRAYTOTEXT in a workbook file's formula writes out every cell of
  ;; its range, not the one in the formula's row.
  (let ((model (grid-model-create
                2 3 '(("1" "2" "=ARRAYTOTEXT(A1:B2)") ("3" "4" "")))))
    (grid-model-set-legacy model '((0 . 2)))
    (grid-calc-forget model)
    (grid-model-recalculate-all model)
    (should (equal (grid-model-get-computed-value model 0 2) "1, 2, 3, 4"))))

(ert-deftest grid-table-formula-percent-functions-as-excel ()
  "PERCENTOF, PERCENTRANK.EXC, LOGINV and ZTEST as Excel has them."
  (dolist (case '(("=PERCENTOF({0,2,2.3},{2,2.3,-0.2,-2})" . 2.047619047619048)
                  ("=PERCENTOF({1,\"a\",TRUE},{2,\"b\"})" . 0.5)
                  ("=PERCENTOF({1,#N/A},{2})" . "#NUM!")
                  ("=PERCENTOF({1},{0})" . "#DIV/0!")
                  ("=PERCENTRANK.EXC({0,0.1,0.7,0.002,4},4,4)" . 0.8333)
                  ("=PERCENTRANK.EXC({0,0.1,0.7,0.002,4},2,2)" . 0.73)
                  ("=PERCENTRANK.EXC({0,0.1,0.7,0.002,4},13)" . "#N/A")
                  ("=PERCENTRANK.EXC({0,0.1,0.7,0.002,4},2,0)" . "#NUM!")
                  ("=PERCENTRANK.INC({0,0.1,0.7,0.002,4},2,2)" . 0.84)
                  ("=LOGINV(0.4,2,2)" . 4.45178791317475)
                  ("=ZTEST({3.2,0.7},0.7,3)" . 0.2778448951413972)
                  ("=ZTEST({3.2,0.7},2,0)" . "#NUM!")
                  ("=ZTEST({4},2,2)" . "#DIV/0!")
                  ("=ZTEST({#NUM!,#REF!},1)" . "#NUM!")))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-several-values-each-give-their-first ()
  "Where one call of several gives an array, its first value stands for it."
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid "=SEQUENCE({2,3})" 1 2))
                 '((1 1))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=DROP({1,2,3;4,5,6;7,8,9},1,{1;2})" 2 1))
                 '((5) (6))))
  (should (equal (grid-table-formula-test--values
                  (grid-table-formula-test--grid
                   "=WRAPROWS({1,2,3,4,5,6},{1,2},6)" 1 2))
                 '((1 1)))))

(ert-deftest grid-table-formula-tests-of-two-ranges-give-their-errors ()
  "CHISQ.TEST, F.TEST and FREQUENCY give an error in their ranges, as Excel."
  (dolist (case '(("=CHISQ.TEST({TRUE,#N/A},{#DIV/0!,1})" . "#N/A")
                  ("=CHISQ.TEST({TRUE,1},{#VALUE!,#N/A})" . "#VALUE!")
                  ("=CHISQ.TEST(3,2)" . "#N/A")
                  ("=CHISQ.TEST({4,\"a\"},{TRUE,\"b\"})" . "#DIV/0!")
                  ("=F.TEST({TRUE,#N/A},{#DIV/0!,1})" . "#N/A")
                  ("=FREQUENCY({-1,#DIV/0!,3},1)" . "#DIV/0!")))
    (grid-table-formula-test--should-be case))
  (should (equal (grid-table-formula-test--values
                  (cons '("=FREQUENCY({1,2,3,0,-1},B1)" "") '(("" ""))))
                 '((2 "") (3 "")))))

(ert-deftest grid-table-formula-beta-inv-takes-a-chance-above-0 ()
  "BETA.INV of a chance of 0 is #NUM!, and of 1 the top of its range."
  (dolist (case '(("=BETA.INV(0,4,4)" . "#NUM!")
                  ("=BETAINV(0,3,2)" . "#NUM!")
                  ("=BETA.INV(1,2,2)" . 1)
                  ("=BETA.INV(1,2,2,3,5)" . 5)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-weekend-read-as-excel ()
  "A weekend left out is Saturday and Sunday, but not one in a blank cell."
  (should (equal (grid-table-formula-test--eval "=NETWORKDAYS.INTL(1,30,B1)" "") "#NUM!"))
  (should (equal (grid-table-formula-test--eval "=WORKDAY.INTL(1,30,B1)" "") "#NUM!"))
  (should (equal (grid-table-formula-test--eval "=NETWORKDAYS.INTL(1,30,,)") 21))
  (should (equal (grid-table-formula-test--eval "=WORKDAY.INTL(1,30,,B1)" "") 41))
  (should (equal (grid-table-formula-test--eval "=NETWORKDAYS.INTL(1,30,B1)" "TRUE") 21))
  ;; An error there is no weekend, whatever error it is.
  (should (equal (grid-table-formula-test--eval "=NETWORKDAYS.INTL(1,30,B1)" "=NA()")
                 "#VALUE!"))
  (should (equal (grid-table-formula-test--eval "=WORKDAY.INTL(1,30,B1)" "=1/0")
                 "#NUM!")))

(ert-deftest grid-table-formula-coupdaysnc-on-30-360-is-the-rest-of-the-period ()
  "On a 30/360 basis Excel gives COUPDAYS less COUPDAYBS, not DAYS360 to the next."
  (dolist (case '(("=COUPDAYSNC(7,DATE(2050,12,31),1,0)" . 353)
                  ("=COUPDAYSNC(3.2,DATE(2050,12,31),1,0)" . 357)
                  ("=COUPDAYSNC(DATE(2011,1,25),DATE(2011,11,15),2,1)" . 110)))
    (grid-table-formula-test--should-be case)))

(ert-deftest grid-table-formula-text-of-a-date-and-a-time-reads ()
  "Text with a date and a time after it is both, as Excel reads it."
  (dolist (case '(("=VALUE(\"1/1/1987 05:00 AM\")" . 31778.208333333332)
                  ("=VALUE(\"1/1/1987 13:30\")" . 31778.5625)
                  ("=WEEKNUM(\"1/1/1987 05:00 AM\",1)" . 1)))
    (grid-table-formula-test--should-be case)))

(provide 'grid-table-formula-test)

;;; grid-table-formula-test.el ends here
