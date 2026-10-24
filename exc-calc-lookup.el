;;; exc-calc-lookup.el --- Lookup, reference and array functions -*- lexical-binding: t -*-

;;; Commentary:

;; Finding values (MATCH, VLOOKUP, XLOOKUP and their kin), making
;; references (INDEX, OFFSET, INDIRECT), saying where a reference is
;; (ROW, COLUMN, ADDRESS) and reshaping several values (SORT, FILTER).

;;; Code:

(require 'exc-calc)
(require 'exc-parser)

(declare-function exc-calc--regexp "exc-calc-text" (pattern))
;; The model is in exc-data-model.el, which loads exc-calc, which loads
;; this file: requiring it here would be a circular load.
(declare-function exc-data-model-rows "exc-data-model")
(declare-function exc-data-model-cols "exc-data-model")
(declare-function exc-model-array-at "exc-data-model")

;;;----------------------------------------------------------------------
;;; Tables of values
;;;----------------------------------------------------------------------
;;
;; A lookup reads a range or an array the same way: as a number of rows
;; and columns and the value at each, without first copying a range out
;; of its sheet.

(defun exc-calc--table-size (value)
  "Return (ROWS . COLS) of VALUE, a `values' argument.
Rows of a range past its sheet's cells are not counted."
  (cond ((exc-calc-ref-p value)
         (cons (exc-calc-ref-filled-rows value) (exc-calc-ref-cols value)))
        ((exc-calc-array-p value)
         (cons (exc-calc-array-rows value) (exc-calc-array-cols value)))
        (t (cons 1 1))))

(defun exc-calc--table-full-size (value)
  "Return (ROWS . COLS) of VALUE, a range counted whole."
  (if (exc-calc-ref-p value)
      (cons (exc-calc-ref-rows value) (exc-calc-ref-cols value))
    (exc-calc--table-size value)))

(defun exc-calc--table-at (value row col)
  "Return the value at ROW and COL of VALUE, a `values' argument."
  (cond ((exc-calc-ref-p value) (exc-calc-ref-cell value row col))
        ((exc-calc-array-p value) (exc-calc-aref value row col))
        (t value)))

(defun exc-calc--line-of (value)
  "Return (LENGTH . GETTER) for VALUE, one row or one column of values.
GETTER takes a position from 0.  Several rows and columns are #N/A."
  (pcase-let ((`(,rows . ,cols) (exc-calc--table-size value))
              (`(,full-rows . ,_) (exc-calc--table-full-size value)))
    (cond ((= cols 1)
           (cons rows (lambda (i) (exc-calc--table-at value i 0))))
          ((or (= rows 1) (= full-rows 1))
           (cons cols (lambda (i) (exc-calc--table-at value 0 i))))
          ;; A range of rows the sheet does not have
          ((= rows 0) (cons 0 #'ignore))
          (t (exc-calc-raise "#N/A")))))

(defun exc-calc--part (value top left rows cols)
  "Return ROWS by COLS of VALUE from TOP and LEFT, counted from 0.
A range gives a range, anything else an array."
  (if (exc-calc-ref-p value)
      (exc-calc-ref--make (exc-calc-ref-model value)
                           (+ (exc-calc-ref-top value) top)
                           (+ (exc-calc-ref-left value) left)
                           (+ (exc-calc-ref-top value) top rows -1)
                           (+ (exc-calc-ref-left value) left cols -1))
    (let ((value (if (exc-calc-array-p value) value
                   (exc-calc-array--make 1 1 (vector value)))))
      (exc-calc-array rows cols
                       (lambda (row col)
                         (exc-calc-aref value (+ top row) (+ left col)))))))

;;;----------------------------------------------------------------------
;;; Comparing as lookups do
;;;----------------------------------------------------------------------

(defun exc-calc-wildcard-regexp (pattern)
  "Return a regexp matching the whole of text that PATTERN matches.
In PATTERN * stands for any characters, ? for any one, and ~ makes the
character after it plain; a ~ with nothing after it, as ~ alone, makes
nothing plain and stands for nothing, which Excel matches at once."
  (let ((parts '()) (i 0) (len (length pattern)))
    (while (< i len)
      (let ((char (aref pattern i)))
        (cond
         ((eq char ?~)
          (when (< (1+ i) len)
            (setq i (1+ i))
            (push (regexp-quote (string (aref pattern i))) parts)))
         ((eq char ?*) (push "\\(?:.\\|\n\\)*" parts))
         ((eq char ??) (push "\\(?:.\\|\n\\)" parts))
         (t (push (regexp-quote (string char)) parts))))
      (setq i (1+ i)))
    (concat "\\`" (apply #'concat (nreverse parts)) "\\'")))

(defun exc-calc-wildcard-p (text)
  "Return non-nil when TEXT holds a wildcard, * ? or ~."
  (string-match-p "[*?~]" text))

(defun exc-calc--lookup-order (left right)
  "Return -1, 0 or 1 as LEFT sorts before, with or after RIGHT, or nil.
Only values of one kind compare: numbers, text without regard to case,
and TRUE and FALSE.  Blanks and error values compare with nothing."
  (cond
   ((and (numberp left) (numberp right))
    (cond ((exc-calc--numbers-equal-p left right) 0) ((< left right) -1) (t 1)))
   ((and (stringp left) (stringp right))
    (let ((a (downcase left)) (b (downcase right)))
      (cond ((string= a b) 0) ((string< a b) -1) (t 1))))
   ((and (exc-calc-boolean-p left) (exc-calc-boolean-p right))
    (cond ((eq left right) 0) ((eq left :false) -1) (t 1)))))

(defun exc-calc--matcher (wanted wildcards)
  "Return a function telling whether a value equals WANTED.
With WILDCARDS, text WANTED is a pattern; see `exc-calc-wildcard-regexp'.
A blank WANTED, which only XLOOKUP and XMATCH search for, is an empty
cell."
  (cond
   ((exc-calc-blank-p wanted) #'exc-calc-blank-p)
   ((and wildcards (stringp wanted) (exc-calc-wildcard-p wanted))
    (let ((regexp (exc-calc-wildcard-regexp wanted)))
      (lambda (value)
        (and (stringp value)
             (let ((case-fold-search t)) (string-match-p regexp value))))))
   (t (lambda (value) (eql (exc-calc--lookup-order value wanted) 0)))))

(defvar exc-calc--exact-indexes (make-hash-table :test 'eq :weakness 'key)
  "Where each value stands in a range or array searched more than once.
Keyed by the range or array, as one formula reads it; a value is an
alist from how it is read: `line', `column' or `row'.  There it is the
symbol `seen' after the first search, then a table from what
`exc-calc--exact-key' makes of a value to its first and last position.")

(defun exc-calc--exact-key (value)
  "Return what VALUE is filed under for an exact search, or nil.
Text without regard to case, a number to 15 digits, TRUE and FALSE."
  (cond ((numberp value) (format "%.14e" value))
        ((stringp value) (and (not (string-empty-p value))
                              (cons 'text (downcase value))))
        ((exc-calc-boolean-p value) value)))

(defun exc-calc--exact-index (source length getter)
  "Return the table of where the LENGTH values of SOURCE stand, or nil.
SOURCE is (OBJECT . READING), as `exc-calc--find-exact' takes it.  Nil
the first time it is searched: one search reads it as quickly."
  (let* ((readings (gethash (car source) exc-calc--exact-indexes))
         (known (alist-get (cdr source) readings)))
    (cond
     ((hash-table-p known) known)
     ((not known)
      (puthash (car source) (cons (cons (cdr source) 'seen) readings)
               exc-calc--exact-indexes)
      nil)
     (t
      (let ((index (make-hash-table :test 'equal :size length)))
        (dotimes (i length)
          (when-let* ((key (exc-calc--exact-key (funcall getter i))))
            (if-let* ((place (gethash key index)))
                (setcdr place i)
              (puthash key (cons i i) index))))
        (setf (alist-get (cdr source) readings) index)
        (puthash (car source) readings exc-calc--exact-indexes)
        index)))))

(defun exc-calc--find-exact (wanted length getter &optional from-end wildcards
                                     source)
  "Return the first position from 0 whose value equals WANTED, or nil.
LENGTH values are read with GETTER; FROM-END searches from the last.
SOURCE, (OBJECT . READING) for the range or array they come from and
how GETTER reads it, lets a formula searching it for many values, as
MATCH(A1:A900,A1:A900,0) does, read it once."
  (let* ((pattern (and wildcards (stringp wanted) (exc-calc-wildcard-p wanted)))
         (key (and source (not pattern) (exc-calc--exact-key wanted)))
         (index (and key (exc-calc--exact-index source length getter)))
         (place (and index (gethash key index))))
    (cond
     (place (if from-end (cdr place) (car place)))
     ;; A number only nearly equal may still be filed under another key.
     ((and index (not (numberp wanted))) nil)
     (t
      (let ((match (exc-calc--matcher wanted wildcards)))
        (if from-end
            (cl-loop for i downfrom (1- length) to 0
                     when (funcall match (funcall getter i)) return i)
          (cl-loop for i below length
                   when (funcall match (funcall getter i)) return i)))))))

(defun exc-calc--find-matching (match length getter from-end)
  "Return the first position MATCH likes among LENGTH values read by GETTER.
FROM-END searches from the last, as XMATCH with a search mode of -1 does."
  (if from-end
      (cl-loop for i downfrom (1- length) to 0
               when (funcall match (funcall getter i)) return i)
    (cl-loop for i below length
             when (funcall match (funcall getter i)) return i)))

(defun exc-calc--regexp-matcher (wanted)
  "Return a function telling whether a value WANTED matches as a regexp.
It matches when it is found anywhere in the value, as REGEXTEST finds
it, so a pattern wanting the whole value says so with ^ and $; a number
or a truth value is read as its text, and a blank or an error matches
nothing.  The pattern is read as REGEXTEST reads one, as PCRE2 writes
it, so ^(Pu|Ro) groups and picks as it does there.  A pattern that is
no regexp is #VALUE!."
  (let ((regexp (condition-case nil
                    (let ((regexp (exc-calc--regexp (exc-calc--text wanted))))
                      (string-match-p regexp "")
                      regexp)
                  (invalid-regexp (exc-calc-raise "#VALUE!")))))
    (lambda (value)
      (and (or (stringp value) (numberp value) (memq value '(t :false)))
           (string-match-p regexp (exc-calc--text value))
           t))))

(defun exc-calc--find-sorted (wanted length getter &optional descending)
  "Return the position of WANTED among LENGTH sorted values, or nil.
That is the last value not past WANTED: not above it, or not below it
when DESCENDING.  Values of another kind than WANTED are passed over.
Values sorted up are searched by halves, as Excel does, so in values
not sorted the answer is Excel's, not the last value not past WANTED."
  (if descending
      (let ((found nil) (i 0))
        (while (< i length)
          (let ((order (exc-calc--lookup-order (funcall getter i) wanted)))
            (cond ((null order))
                  ((> order 0) (setq found i))
                  ;; One equal to it ends the search, after its run.
                  ((= order 0)
                   (setq found i)
                   (while (and (< (1+ found) length)
                               (eql (exc-calc--lookup-order
                                     (funcall getter (1+ found)) wanted)
                                    0))
                     (setq found (1+ found)))
                   (setq i length))
                  (t (setq i length))))
          (setq i (1+ i)))
        found)
    (exc-calc--find-halving wanted length getter)))

(defun exc-calc--find-halving (wanted length getter)
  "Return the last of LENGTH values not above WANTED, searched by halves.
This is the search of Apache POI's LookupUtils, which follows Excel's:
the values between LOW and HIGH, both apart, are halved until none are
left, or one equal to WANTED is met, when the last of its run is taken.
A middle value of another kind gives way to the next one that is not;
when none is, the search keeps to the values before it."
  (let ((low -1) (high length) (found nil))
    (while (and (not found) (>= (- high low) 2))
      (let* ((middle (+ low (/ (- high low) 2)))
             (order (exc-calc--lookup-order (funcall getter middle) wanted)))
        (unless order
          (let ((next (1+ middle)))
            (while (and next (< next high))
              (let ((next-order (exc-calc--lookup-order (funcall getter next) wanted)))
                (cond ((and (eql next-order 1) (= next (1- high)))
                       (setq next nil))
                      (next-order (setq middle next order next-order next nil))
                      (t (setq next (1+ next))))))))
        (cond ((null order) (setq high middle))
              ((= order 0)
               (setq found middle)
               (while (and (< (1+ found) high)
                           (eql (exc-calc--lookup-order (funcall getter (1+ found))
                                                         wanted)
                                0))
                 (setq found (1+ found))))
              ((> order 0) (setq high middle))
              (t (setq low middle)))))
    (or found (and (>= low 0) low))))

(defun exc-calc--nearest-order (left right)
  "Return -1, 0 or 1 as LEFT sorts before, with or after RIGHT, or nil.
Numbers, text and TRUE and FALSE compare, as Excel sorts them; blanks
and error values compare with nothing."
  (let ((kind-p (lambda (value) (or (numberp value) (stringp value)
                                    (exc-calc-boolean-p value)))))
    (and (funcall kind-p left) (funcall kind-p right)
         (exc-calc--sort-order left right))))

(defun exc-calc--find-nearest (wanted length getter direction &optional from-end)
  "Return the position of WANTED among LENGTH values, or of the nearest.
Values need not be sorted.  DIRECTION -1 takes the largest value below
WANTED when none equals it, 1 the smallest above it, values of another
kind being below or above as Excel sorts them.  A blank WANTED is above
every value but an empty cell, which equals it."
  (let (best best-value)
    (dotimes (n length)
      (let* ((i (if from-end (- length n 1) n))
             (value (funcall getter i))
             (order (if (exc-calc-blank-p wanted)
                        (cond ((exc-calc-blank-p value) 0)
                              ((exc-calc--nearest-order value 0) -1))
                      (exc-calc--nearest-order value wanted))))
        (when order
          (cond
           ((= order 0) (unless (and best (eql (exc-calc--nearest-order
                                                 best-value wanted) 0))
                          (setq best i best-value value)))
           ((and (= order direction)
                 (not (and best (eql (exc-calc--nearest-order best-value wanted) 0)))
                 (or (null best)
                     (eql (exc-calc--nearest-order value best-value)
                          (- direction))))
            (setq best i best-value value))))))
    best))

;;;----------------------------------------------------------------------
;;; Finding values
;;;----------------------------------------------------------------------

(exc-calc-define match ((wanted any) (values values) &optional (type integer 1))
  "The position of WANTED in VALUES, one row or column, counted from 1.
TYPE 0 finds a value equal to WANTED, where text may hold wildcards;
1 the last not above it in values sorted up; -1 the last not below it in
values sorted down.  #N/A when there is none; a blank WANTED is
sought as 0 in sorted values."
  (when (exc-calc-blank-p wanted)
    (if (= type 0) (exc-calc-raise "#N/A") (setq wanted 0)))
  (pcase-let ((`(,length . ,getter) (exc-calc--line-of values)))
    (let ((found (cond ((= type 0)
                        (exc-calc--find-exact wanted length getter nil t
                                               (cons values 'line)))
                       ((> type 0) (exc-calc--find-sorted wanted length getter))
                       (t (exc-calc--find-sorted wanted length getter t)))))
      (if found (1+ found) (exc-calc-err "#N/A")))))

(defun exc-calc--sort-order (left right)
  "Return -1, 0 or 1 as LEFT sorts before, with or after RIGHT.
Values of every kind compare, as Excel sorts them: numbers, then text,
then TRUE and FALSE, then blanks and error values."
  (let ((rank (lambda (value)
                (cond ((numberp value) 0) ((stringp value) 1)
                      ((exc-calc-boolean-p value) 2) (t 3)))))
    (or (exc-calc--lookup-order left right)
        (let ((a (funcall rank left)) (b (funcall rank right)))
          (cond ((< a b) -1) ((> a b) 1) (t 0))))))

(defun exc-calc--find-bisecting (wanted length getter mode descending)
  "Return the position XLOOKUP's search by halves finds for WANTED, or nil.
LENGTH values read with GETTER are taken as sorted up, or down when
DESCENDING; MODE is its match_mode, 0, -1 or 1.  As Excel searches, the
values from LOW to HIGH, both in, are halved at the middle rounded down,
so in values not sorted it lands where Excel does.  Of several equal to
WANTED, the first is found in values sorted up, the last in values
sorted down."
  (let ((low 0) (high (1- length)) found)
    (while (<= low high)
      (let* ((middle (/ (+ low high) 2))
             (order (exc-calc--sort-order (funcall getter middle) wanted)))
        (cond ((= order 0)
               (setq found middle)
               (if descending (setq low (1+ middle)) (setq high (1- middle))))
              ;; A value that comes before WANTED in the sorting
              ((if descending (> order 0) (< order 0)) (setq low (1+ middle)))
              (t (setq high (1- middle))))))
    ;; Unless found, WANTED would come between HIGH and LOW.
    (or found
        (let ((j (if (eq (= mode 1) (not descending)) low high)))
          (and (/= mode 0) (< -1 j length) j)))))

(defun exc-calc--xfind (wanted length getter mode search &optional source)
  "Return the position XMATCH and XLOOKUP find for WANTED, or nil.
MODE and SEARCH are their match_mode and search_mode; SOURCE is as
`exc-calc--find-exact' takes it."
  (unless (memq mode '(0 -1 1 2 3)) (exc-calc-raise "#VALUE!"))
  (unless (memq search '(1 -1 2 -2)) (exc-calc-raise "#VALUE!"))
  ;; Wildcards and regular expressions cannot be searched for by halves.
  (when (and (memq mode '(2 3)) (memq search '(2 -2))) (exc-calc-raise "#VALUE!"))
  (let ((from-end (memq search '(-1 -2))))
    (pcase mode
      ((guard (memq search '(2 -2)))
       (exc-calc--find-bisecting wanted length getter mode (= search -2)))
      ;; Their search for equal text takes ß for ss, as MATCH does not.
      ((and 0 (guard (and (stringp wanted)
                          (string-match-p "ss\\|ß" (downcase wanted)))))
       (let ((fold (lambda (value)
                     (if (stringp value)
                         (string-replace "ß" "ss" (downcase value))
                       value))))
         (exc-calc--find-exact (funcall fold wanted) length
                                (lambda (i) (funcall fold (funcall getter i)))
                                from-end)))
      (0 (exc-calc--find-exact wanted length getter from-end nil source))
      (2 (exc-calc--find-exact wanted length getter from-end t source))
      (3 (exc-calc--find-matching (exc-calc--regexp-matcher wanted)
                                   length getter from-end))
      (_ (exc-calc--find-nearest wanted length getter mode from-end)))))

(defun exc-calc--x-line-of (value)
  "Return (LENGTH . GETTER) for VALUE as XMATCH and XLOOKUP read it.
As `exc-calc--line-of', but several rows and columns are #VALUE!."
  (pcase-let ((`(,rows . ,cols) (exc-calc--table-full-size value)))
    (when (and (> rows 1) (> cols 1)) (exc-calc-raise "#VALUE!")))
  (exc-calc--line-of value))

(exc-calc-define xmatch ((wanted any) (values values)
                          &optional (mode integer 0) (search integer 1))
  "The position of WANTED in VALUES, counted from 1; #N/A when not found.
MODE 0 matches exactly, -1 takes the next smaller value too, 1 the next
larger, 2 matches text with wildcards and 3 matches it as a regular
expression.  SEARCH 1 starts at the first value, -1 at the last."
  :empty t
  (pcase-let ((`(,length . ,getter) (exc-calc--x-line-of values)))
    (let ((found (exc-calc--xfind wanted length getter mode search
                                   (cons values 'line))))
      (if found (1+ found) (exc-calc-err "#N/A")))))

(defun exc-calc--table-lookup (wanted table index approximate across)
  "Look WANTED up in the first column of TABLE and give column INDEX.
ACROSS looks along the first row and gives row INDEX instead, as HLOOKUP
does.  APPROXIMATE takes the last value not above WANTED, as 0 when
it is blank."
  (when (exc-calc-blank-p wanted)
    (if approximate (setq wanted 0) (exc-calc-raise "#N/A")))
  ;; Text is no table at all, as VLOOKUP(3,"Nothing",1) is #VALUE! in
  ;; IronCalc's HVLOOKUP workbook; a number or a truth value is a table
  ;; of one cell, which then holds nothing to find.
  (when (stringp table) (exc-calc-raise "#VALUE!"))
  (pcase-let ((`(,rows . ,cols) (exc-calc--table-size table))
              (`(,full-rows . ,full-cols) (exc-calc--table-full-size table)))
    (let* ((length (if across cols rows))
           (getter (if across (lambda (i) (exc-calc--table-at table 0 i))
                     (lambda (i) (exc-calc--table-at table i 0))))
           (found (if approximate
                      (exc-calc--find-sorted wanted length getter)
                    (exc-calc--find-exact wanted length getter nil t
                                           (cons table (if across 'row 'column))))))
      ;; Only a value found is looked for in INDEX.
      (cond ((null found) (exc-calc-err "#N/A"))
            ((not (<= 1 index (if across full-rows full-cols)))
             (exc-calc-err (if (< index 1) "#VALUE!" "#REF!")))
            (across (exc-calc--table-at table (1- index) found))
            (t (exc-calc--table-at table found (1- index)))))))

(exc-calc-define vlookup ((wanted any) (table values) (column integer)
                           &optional (approximate bool t))
  "The value in COLUMN of the row of TABLE whose first cell is WANTED.
APPROXIMATE, the default, takes the last row not above WANTED in a
first column sorted up; FALSE matches exactly, text with wildcards."
  (exc-calc--table-lookup wanted table column approximate nil))

(exc-calc-define hlookup ((wanted any) (table values) (row integer)
                           &optional (approximate bool t))
  "The value in ROW of the column of TABLE whose first cell is WANTED.
See VLOOKUP."
  (exc-calc--table-lookup wanted table row approximate t))

(exc-calc-define lookup ((wanted any) (values values) &optional (results values))
  "The value matching WANTED in sorted VALUES, or the same place of RESULTS.
Without RESULTS, VALUES wider than tall is looked along its first row
and gives its last row; otherwise along its first column, giving its
last column.  VALUES is read so with RESULTS too, which must be one row
or column.  A blank WANTED is sought as 0."
  (when (exc-calc-blank-p wanted) (setq wanted 0))
  (pcase-let ((`(,rows . ,cols) (exc-calc--table-size values)))
    (let* ((across (> cols rows))
           (length (if across cols rows))
           (getter (if across (lambda (i) (exc-calc--table-at values 0 i))
                     (lambda (i) (exc-calc--table-at values i 0))))
           (found (exc-calc--find-sorted wanted length getter)))
      (cond ((null found) (exc-calc-err "#N/A"))
            (results
             (pcase-let ((`(,r-rows . ,r-cols) (exc-calc--table-full-size results)))
               (cond ((and (> r-rows 1) (> r-cols 1)) (exc-calc-err "#N/A"))
                     ((and (= r-rows 1) (> r-cols 1))
                      (exc-calc--table-at results 0 found))
                     (t (exc-calc--table-at results found 0)))))
            (across (exc-calc--table-at values (1- rows) found))
            (t (exc-calc--table-at values found (1- cols)))))))

(exc-calc-define xlookup ((wanted any) (values values) (results values)
                           &optional (missing lazy) (mode integer 0)
                           (search integer 1))
  "The part of RESULTS at the place of WANTED in VALUES.
VALUES is one row or column; RESULTS as tall gives that row of it, as
wide that column.  MISSING is given when nothing matches, else #N/A.
MODE and SEARCH are as XMATCH's."
  :empty t
  (pcase-let ((`(,length . ,getter) (exc-calc--x-line-of values))
              (`(,r-rows . ,r-cols) (exc-calc--table-full-size results))
              (`(,v-rows . ,_) (exc-calc--table-full-size values)))
    (let ((down (> v-rows 1))
          (found (exc-calc--xfind wanted length getter mode search
                                   (cons values 'line))))
      (cond
       ((null found)
        (if (and missing (not (equal missing '(MISSING))))
            (exc-calc-arg missing 'values)
          (exc-calc-err "#N/A")))
       ((if down (/= r-rows v-rows) (/= r-cols length))
        (exc-calc-err "#VALUE!"))
       (down (exc-calc--part results found 0 1 r-cols))
       (t (exc-calc--part results 0 found r-rows 1))))))

;;;----------------------------------------------------------------------
;;; Making references
;;;----------------------------------------------------------------------

(exc-calc-define index ((source areas) (row integer)
                         &optional (column integer) (area integer 1))
  "The value at ROW and COLUMN of SOURCE, counted from 1.
SOURCE of several areas is looked in its AREA'th.  SOURCE of one row
takes a lone ROW as its column; a range of several rows and columns
needs COLUMN, an array of them gives the whole row.  ROW 0 gives the
whole column and COLUMN 0 the whole row.  A range gives a reference.
A ROW or COLUMN below 0 is #VALUE!, past SOURCE or its areas #REF!."
  (let ((areas (if (eq (car-safe source) 'union) (cdr source) (list source))))
    (unless (<= 1 area (length areas)) (exc-calc-raise "#REF!"))
    (setq source (nth (1- area) areas)))
  (pcase-let ((`(,rows . ,cols) (exc-calc--table-full-size source)))
    (when (and (null column) (= rows 1) (> cols 1))
      (setq column row row 1))
    (when (or (< row 0) (and column (< column 0)))
      (exc-calc-raise "#VALUE!"))
    (when (and (null column) (> cols 1) (exc-calc-ref-p source))
      (exc-calc-raise "#REF!"))
    (setq column (or column (if (= cols 1) 1 0)))
    (unless (and (<= 0 row rows) (<= 0 column cols))
      (exc-calc-raise "#REF!"))
    (exc-calc--part source
                     (if (= row 0) 0 (1- row)) (if (= column 0) 0 (1- column))
                     (if (= row 0) rows 1) (if (= column 0) cols 1))))

(defconst exc-calc--max-rows 1048576 "Rows a sheet may have.")
(defconst exc-calc--max-cols 16384 "Columns a sheet may have.")

(defun exc-calc--count-size (value)
  "Return (ROWS . COLS) of VALUE as a function counting its cells sees it.
Whole columns reach the sheet's last row and whole rows its last column,
as A:A has 1048576 cells, though the range stops at the sheet's cells."
  (pcase-let ((`(,rows . ,cols) (exc-calc--table-full-size value)))
    (pcase (and (exc-calc-ref-p value) (exc-calc-ref-whole value))
      ('column (cons (- exc-calc--max-rows (exc-calc-ref-top value)) cols))
      ('row (cons rows (- exc-calc--max-cols (exc-calc-ref-left value))))
      (_ (cons rows cols)))))

(defun exc-calc--checked-ref (model top left bottom right)
  "Return the reference from TOP LEFT to BOTTOM RIGHT of MODEL.
#REF! when it reaches past a sheet's edges."
  (unless (and (<= 0 top bottom) (< bottom exc-calc--max-rows)
               (<= 0 left right) (< right exc-calc--max-cols))
    (exc-calc-raise "#REF!"))
  (exc-calc-ref--make model top left bottom right))

(defun exc-calc--offset-size (size)
  "Return SIZE as a count of rows or columns, as OFFSET takes it.
A fraction is cut to a whole number, but one that leaves none of itself
keeps one: Excel reads OFFSET(A1,0,0,1,0.9) as one column wide, and
only a SIZE of 0 as none at all."
  (let ((whole (truncate size)))
    (cond ((/= whole 0) whole)
          ((zerop size) 0)
          ((> size 0) 1)
          (t -1))))

(exc-calc-define offset ((reference ref) (rows integer) (cols integer)
                          &optional (height number) (width number))
  "The reference ROWS down and COLS across from REFERENCE.
It is HEIGHT by WIDTH, by default as large as REFERENCE."
  :empty t
  (let ((height (or height (exc-calc-ref-rows reference)))
        (width (or width (exc-calc-ref-cols reference)))
        (top (+ (exc-calc-ref-top reference) rows))
        (left (+ (exc-calc-ref-left reference) cols)))
    (setq height (exc-calc--offset-size height)
          width (exc-calc--offset-size width))
    (when (or (= height 0) (= width 0)) (exc-calc-raise "#REF!"))
    ;; A negative size reaches up or left of the corner.
    (when (< height 0) (setq top (+ top height 1) height (- height)))
    (when (< width 0) (setq left (+ left width 1) width (- width)))
    (exc-calc--checked-ref (exc-calc-ref-model reference)
                            top left (+ top height -1) (+ left width -1))))

(defun exc-calc--r1c1-part (text here letter)
  "Return the row or column TEXT gives after LETTER, R or C, from 0.
TEXT is empty for HERE, [N] for N away from HERE, or N.  N away
wraps around the sheet's edge, as R[-6]C from row 2 is its last row."
  (cond ((string-empty-p text) here)
        ((string-match "\\`\\[\\([-+]?[0-9]+\\)\\]\\'" text)
         (and here (mod (+ here (string-to-number (match-string 1 text)))
                        (if (eq letter ?R) exc-calc--max-rows exc-calc--max-cols))))
        ((string-match "\\`[0-9]+\\'" text) (1- (string-to-number text)))))

(defun exc-calc--r1c1-side (text)
  "Return what one side TEXT of an R1C1 reference names, or nil.
\(cell ROW COL) for a cell, (row ROW) for a whole row, as R2, and
\(column COL) for a whole column, as C2; ROW and COL are counted from 0
or are relative to the formula's cell."
  (let* ((case-fold-search t)
         (n "\\(\\[[-+]?[0-9]+\\]\\|[0-9]*\\)")
         (here-row (nth 1 exc-calc--here))
         (here-col (nth 2 exc-calc--here))
         (parts (cond ((string-match (concat "\\`R" n "C" n "\\'") text)
                       (list 'cell (match-string 1 text) (match-string 2 text)))
                      ((string-match (concat "\\`R" n "\\'") text)
                       (list 'row (match-string 1 text)))
                      ((string-match (concat "\\`C" n "\\'") text)
                       (list 'column (match-string 1 text))))))
    ;; The parts are read once the matching is done, as reading them
    ;; matches again.
    (pcase parts
      (`(cell ,row ,col)
       (let ((row (exc-calc--r1c1-part row here-row ?R))
             (col (exc-calc--r1c1-part col here-col ?C)))
         (and row col (list 'cell row col))))
      (`(row ,row)
       (let ((row (exc-calc--r1c1-part row here-row ?R)))
         (and row (list 'row row))))
      (`(column ,col)
       (let ((col (exc-calc--r1c1-part col here-col ?C)))
         (and col (list 'column col)))))))

(defun exc-calc--r1c1-reference (text model)
  "Return the reference R1C1-style TEXT names, from MODEL's sheet, or nil.
TEXT is a cell, as R2C3, a range of cells, as R1C1:R2C3, or whole rows
or columns, as R2, C2 or R1:R3."
  (let ((sheet nil))
    (when-let* ((prefix (and (> (length text) 0)
                             (exc-parser-sheet-prefix-at text 0))))
      (setq sheet (cdr prefix) text (substring text (car prefix))))
    (let* ((sides (split-string text ":"))
           (a (and (<= 1 (length sides) 2) (exc-calc--r1c1-side (car sides))))
           (b (if (cdr sides) (exc-calc--r1c1-side (cadr sides)) a)))
      (when (and a b (eq (car a) (car b)))
        (let ((model (exc-calc--sheet-model model sheet)))
          (pcase (car a)
            ('cell
             (exc-calc--checked-ref model
                                     (min (nth 1 a) (nth 1 b)) (min (nth 2 a) (nth 2 b))
                                     (max (nth 1 a) (nth 1 b)) (max (nth 2 a) (nth 2 b))))
            ;; Whole rows and columns stop at the sheet's cells, as 2:3 does.
            ('row
             (let ((top (min (nth 1 a) (nth 1 b))) (bottom (max (nth 1 a) (nth 1 b))))
               (exc-calc--checked-ref model top 0 bottom 0)
               (exc-calc-ref--make model top 0 bottom
                                    (max 0 (1- (exc-data-model-cols model))) 'row)))
            ('column
             (let ((left (min (nth 1 a) (nth 1 b))) (right (max (nth 1 a) (nth 1 b))))
               (exc-calc--checked-ref model 0 left 0 right)
               (exc-calc-ref--make model 0 left
                                    (max 0 (1- (exc-data-model-rows model))) right
                                    'column)))))))))

(exc-calc-define indirect ((text text) &optional (a1 bool t))
  "The reference TEXT names, as \"B2\", \"Sheet2!A1:C3\" or a defined name.
With A1 false TEXT is in R1C1 style, as \"R2C3\" or \"R[-1]C\"."
  (let ((text (string-trim text)))
    (or (if a1
            (let ((ast (ignore-errors (exc-parser-parse text))))
              (pcase (car-safe ast)
                ((or 'CELL-REF 'RANGE 'NAME 'RANGE-OP)
                 ;; A reference into another workbook is #REF!: Excel
                 ;; does not read one whose file is not open.
                 (let* ((exc-calc-externals nil)
                        (value (exc-calc--eval exc-calc--model ast)))
                   (and (exc-calc-ref-p value) value)))))
          (exc-calc--r1c1-reference text exc-calc--model))
        (exc-calc-err "#REF!"))))

(defun exc-calc--array-range-reference (model row col)
  "Return the range an array formula over several cells fills, or nil.
The formula in the corner of it spills over all of them, as C2# names
every cell of the array formula written over C2:C5."
  (when-let* ((array (exc-model-array-at model row col))
              (range (ignore-errors
                       (exc-calc--reference model (exc-parser-parse array)))))
    (and (or (/= (exc-calc-ref-rows range) 1) (/= (exc-calc-ref-cols range) 1))
         range)))

(defun exc-calc--anchor-array (reference)
  "Return the cells the formula in the corner of REFERENCE spills over.
They are a reference, so a formula in a plain cell takes the one in its
own row or column, as from any range."
  (let* ((model (exc-calc-ref-model reference))
         (row (exc-calc-ref-top reference))
         (col (exc-calc-ref-left reference))
         (raw (exc-calc--raw-at reference))
         (index (+ (* row (exc-data-model-cols model)) col)))
    (or (and (= (exc-calc-ref-rows reference) 1)
             (= (exc-calc-ref-cols reference) 1)
             (exc-calc--array-range-reference model row col))
        ;; Only one cell, holding a formula, is a corner things spill from.
        ;; A formula whose values cannot spill names no cells.
        (if (not (and (= (exc-calc-ref-rows reference) 1)
                      (= (exc-calc-ref-cols reference) 1)
                      (exc-calc--formula-p raw)))
            (exc-calc-err "#REF!")
          ;; The formula is computed first, as reading its cell would:
          ;; what it spills is what the cells show, whichever of them a
          ;; formula reached first.
          (when exc-calc--pass
            (exc-calc--compute-cell model index))
          (pcase (and (gethash model exc-calc--spills)
                      (gethash index (aref (exc-calc--spill-state model) 0)))
            (`(,rows ,cols nil)
             (exc-calc-ref--make model row col (+ row rows -1) (+ col cols -1)))
            (_ (exc-calc-err "#REF!")))))))

(exc-calc-define anchorarray ((reference ref))
  "All the values the formula at REFERENCE gives, as A1# names them."
  (if (eq (car-safe reference) 'union) (exc-calc-err "#REF!")
    (exc-calc--anchor-array reference)))

;;;----------------------------------------------------------------------
;;; Where a reference is
;;;----------------------------------------------------------------------

(defun exc-calc--here-or-raise (index)
  "Return element INDEX of `exc-calc--here', or signal #VALUE!."
  (or (nth index exc-calc--here) (exc-calc-raise "#VALUE!")))

(exc-calc-define row (&optional (reference ref))
  "The row number of REFERENCE, or of the formula's own cell.
A range of several rows gives each of their numbers."
  (if (null reference)
      (1+ (exc-calc--here-or-raise 1))
    (let ((top (exc-calc-ref-top reference)))
      (if (= (exc-calc-ref-rows reference) 1) (1+ top)
        (exc-calc-array (exc-calc-ref-rows reference) 1
                         (lambda (row _) (+ top row 1)))))))

(exc-calc-define column (&optional (reference ref))
  "The column number of REFERENCE, or of the formula's own cell.
A range of several columns gives each of their numbers."
  (if (null reference)
      (1+ (exc-calc--here-or-raise 2))
    (let ((left (exc-calc-ref-left reference)))
      (if (= (exc-calc-ref-cols reference) 1) (1+ left)
        (exc-calc-array 1 (exc-calc-ref-cols reference)
                         (lambda (_ col) (+ left col 1)))))))

(exc-calc-define rows ((value areas))
  "How many rows VALUE has.  An error given for VALUE is the answer,
and several areas are #REF!, as in Excel."
  (cond ((exc-calc-err-p value) value)
        ((eq (car-safe value) 'union) (exc-calc-err "#REF!"))
        (t (car (exc-calc--count-size value)))))

(exc-calc-define columns ((value areas))
  "How many columns VALUE has.  An error given for VALUE is the answer,
and several areas are #REF!, as in Excel."
  (cond ((exc-calc-err-p value) value)
        ((eq (car-safe value) 'union) (exc-calc-err "#REF!"))
        (t (cdr (exc-calc--count-size value)))))

(exc-calc-define areas ((reference ref))
  "How many areas REFERENCE has, as (A1:B2,D4) has 2."
  (if (eq (car-safe reference) 'union) (length (cdr reference)) 1))

(exc-calc-define address ((row integer) (column integer)
                           &optional (absolute integer 1) (a1 bool t)
                           (sheet text))
  "The text of the reference to ROW and COLUMN, as $B$3.
ABSOLUTE 1 fixes both, 2 the row, 3 the column and 4 neither.  With A1
false the text is in R1C1 style; SHEET is put before it, even empty."
  (unless (and (<= 1 row exc-calc--max-rows) (<= 1 column exc-calc--max-cols)
               (<= 1 absolute 4))
    (exc-calc-raise "#VALUE!"))
  (let ((fixed-row (memq absolute '(1 2)))
        (fixed-col (memq absolute '(1 3))))
    (concat
     (and sheet
          ;; Excel quotes [Book1]Sheet1 only as it would Sheet1, and
          ;; puts a bare ! for an empty name.
          (if (or (string-empty-p sheet)
                  (and (string-match "\\`\\[[^]]+\\]\\(.+\\)\\'" sheet)
                       (not (string-prefix-p "'" (exc-calc-sheet-prefix
                                                  (match-string 1 sheet))))))
              (concat sheet "!")
            (exc-calc-sheet-prefix sheet)))
     (if a1
         (concat (and fixed-col "$") (exc-calc-col-number-to-letter (1- column))
                 (and fixed-row "$") (number-to-string row))
       (concat "R" (if fixed-row (number-to-string row) (format "[%d]" row))
               "C" (if fixed-col (number-to-string column)
                     (format "[%d]" column)))))))

(exc-calc-define cell ((info text) &optional (reference ref))
  "What INFO says of the cell REFERENCE names, or of the formula's own:
\"address\", \"row\", \"col\", \"contents\", \"type\" (b blank, l text,
v anything else) or \"filename\"."
  (let ((reference (or reference
                       (exc-calc-ref--make exc-calc--model
                                            (exc-calc--here-or-raise 1)
                                            (exc-calc--here-or-raise 2)
                                            (nth 1 exc-calc--here)
                                            (nth 2 exc-calc--here)))))
    (pcase (downcase info)
      ("address" (concat "$" (exc-calc-col-number-to-letter
                              (exc-calc-ref-left reference))
                         "$" (number-to-string (1+ (exc-calc-ref-top reference)))))
      ("row" (1+ (exc-calc-ref-top reference)))
      ("col" (1+ (exc-calc-ref-left reference)))
      ("contents" (let ((value (exc-calc-ref-cell reference 0 0)))
                    (if (exc-calc-blank-p value) 0 value)))
      ("type" (let ((value (exc-calc-ref-cell reference 0 0)))
                (cond ((exc-calc-blank-p value) "b") ((stringp value) "l")
                      (t "v"))))
      ("filename" (or (and exc-calc-file-name
                           (funcall exc-calc-file-name
                                    (exc-calc-ref-model reference)))
                      ""))
      (_ (exc-calc-err "#VALUE!")))))

(exc-calc-define hyperlink ((link any) &optional (label any))
  "LABEL, or LINK without it: what a link shows.
A LABEL that is a blank cell shows 0, as IronCalc's link_test workbook
has HYPERLINK(\"https://\"&E21,E21) with E21 empty."
  (cond ((null label) link)
        ((exc-calc-blank-p label) 0)
        (t label)))

(exc-calc-define getpivotdata (&rest (_arguments values))
  "#REF!: exc has no pivot tables."
  (exc-calc-err "#REF!"))

;;;----------------------------------------------------------------------
;;; Reshaping several values
;;;----------------------------------------------------------------------

(exc-calc-define transpose ((array array))
  "ARRAY with its rows made columns."
  (exc-calc-array (exc-calc-array-cols array) (exc-calc-array-rows array)
                   (lambda (row col) (exc-calc-aref array col row))))

(defun exc-calc--line-numbers (arguments)
  "Return the values of ARGUMENTS read as numbers, a cell's text and TRUE too.
A range or array of more than one row and more than one column is
#VALUE!."
  (let (numbers)
    (dolist (argument arguments)
      (when (or (and (exc-calc-ref-p argument)
                     (> (exc-calc-ref-rows argument) 1)
                     (> (exc-calc-ref-cols argument) 1))
                (and (exc-calc-array-p argument)
                     (> (exc-calc-array-rows argument) 1)
                     (> (exc-calc-array-cols argument) 1)))
        (exc-calc-raise "#VALUE!"))
      (exc-calc-each-value argument
                            (lambda (value _) (push (exc-calc-num value) numbers))))
    (nreverse numbers)))

(defun exc-calc--picked (arguments size)
  "Return the places, from 0, that the numbers in ARGUMENTS pick of SIZE.
A number counts from 1, or from the last when negative; they are read
as `exc-calc--line-numbers' reads them."
  (or (mapcar (lambda (n)
                (let ((n (truncate n)))
                  (cond ((<= 1 n size) (1- n))
                        ((<= 1 (- n) size) (+ size n))
                        (t (exc-calc-raise "#VALUE!")))))
              (exc-calc--line-numbers arguments))
      (exc-calc-raise "#VALUE!")))

(exc-calc-define chooserows ((array array) &rest (rows values))
  "The rows of ARRAY whose numbers are ROWS, counting from 1; a negative
number counts from the last."
  (let ((picked (exc-calc--picked rows (exc-calc-array-rows array))))
    (exc-calc-array (length picked) (exc-calc-array-cols array)
                     (lambda (row col) (exc-calc-aref array (nth row picked) col)))))

(exc-calc-define choosecols ((array array) &rest (cols values))
  "The columns of ARRAY whose numbers are COLS, counting from 1; a
negative number counts from the last."
  (let ((picked (exc-calc--picked cols (exc-calc-array-cols array))))
    (exc-calc-array (exc-calc-array-rows array) (length picked)
                     (lambda (row col)
                       (exc-calc-aref array row (nth col picked))))))

(exc-calc-define sequence ((rows integer 1) &optional (cols integer 1)
                            (start number 1) (step number 1))
  "ROWS by COLS numbers from START, each STEP more than the one before.
An argument left empty is 1."
  :empty t
  (unless (and (> rows 0) (> cols 0)) (exc-calc-raise "#CALC!"))
  (exc-calc-array rows cols
                   (lambda (row col)
                     (exc-calc--tidy-number (+ start (* step (+ (* row cols) col)))))))

(defun exc-calc--lines (array by-col)
  "Return the rows of ARRAY as lists, or its columns when BY-COL."
  (exc-calc-array-rows-list (if by-col
                                 (exc-calc-array (exc-calc-array-cols array)
                                                  (exc-calc-array-rows array)
                                                  (lambda (r c) (exc-calc-aref array c r)))
                               array)))

(defun exc-calc--from-lines (lines by-col)
  "Return LINES, lists of values, as the rows of an array, or its columns."
  (let ((array (exc-calc-array-from-rows lines)))
    (if by-col
        (exc-calc-array (exc-calc-array-cols array) (exc-calc-array-rows array)
                         (lambda (r c) (exc-calc-aref array c r)))
      array)))

(defun exc-calc--sorts-before-p (left right direction)
  "Return non-nil when LEFT goes before RIGHT, sorting up with DIRECTION
1 and down with -1.  A blank goes last either way."
  (cond ((eq left :blank) nil)
        ((eq right :blank) t)
        (t (let ((order (exc-calc-compare left right)))
             (if (= direction 1) (< order 0) (> order 0))))))

(exc-calc-define sort ((array array) &optional (index values 1)
                        (order values 1) (by-col bool))
  "ARRAY with its rows sorted by their INDEXth value, or its columns with
BY-COL.  ORDER 1 sorts up, -1 down.  INDEX may give several places, the
first sorting first, with an ORDER for each or one for all."
  :empty t
  (let* ((lines (exc-calc--lines array by-col))
         (width (length (car lines)))
         (indexes (mapcar (lambda (n)
                            (let ((n (truncate n)))
                              (unless (<= 1 n width) (exc-calc-raise "#VALUE!"))
                              (1- n)))
                          (exc-calc--line-numbers (list index))))
         (orders (mapcar (lambda (n)
                           (let ((n (truncate n)))
                             (unless (memq n '(1 -1)) (exc-calc-raise "#VALUE!"))
                             n))
                         (exc-calc--line-numbers (list order)))))
    (unless (and indexes orders) (exc-calc-raise "#VALUE!"))
    (cond ((= (length orders) 1)
           (setq orders (make-list (length indexes) (car orders))))
          ((/= (length orders) (length indexes)) (exc-calc-raise "#VALUE!")))
    ;; The last place sorts first, the ones before it keeping what it
    ;; leaves in order.
    (cl-loop for place in (reverse indexes)
             for direction in (reverse orders)
             do (setq lines (cl-stable-sort
                             (copy-sequence lines)
                             (lambda (a b)
                               (exc-calc--sorts-before-p
                                (nth place a) (nth place b) direction)))))
    (exc-calc--from-lines lines by-col)))

(exc-calc-define unique ((array array) &optional (by-col bool) (once bool))
  "The rows of ARRAY each once, in the order they come, or its columns
with BY-COL.  With ONCE only rows that come only once."
  (let* ((lines (exc-calc--lines array by-col))
         (same (lambda (a b) (cl-every #'exc-calc-equal-p a b)))
         (kept (if once
                   (cl-remove-if (lambda (line)
                                   (> (cl-count line lines :test same) 1))
                                 lines)
                 (cl-remove-duplicates lines :test same :from-end t))))
    (if kept (exc-calc--from-lines kept by-col) (exc-calc-err "#CALC!"))))

(exc-calc-define filter ((array array) (include array) &optional (empty lazy))
  "The rows of ARRAY whose value in INCLUDE is true, or its columns when
INCLUDE is a row.  EMPTY is given when none is; without it #CALC!."
  (let* ((by-col (and (= (exc-calc-array-rows include) 1)
                      ;; One value is a row for an ARRAY of one column.
                      (or (> (exc-calc-array-cols include) 1)
                          (= (exc-calc-array-cols array) 1))))
         (keep (exc-calc-array-values include)))
    (unless (= (length keep) (if by-col (exc-calc-array-cols array)
                               (exc-calc-array-rows array)))
      (exc-calc-raise "#VALUE!"))
    ;; Rows are made into lists only when they are kept: a FILTER of a
    ;; whole column keeps a few of thousands.
    (let ((kept (if by-col
                    (cl-loop for line in (exc-calc--lines array t)
                             for flag in keep
                             when (exc-calc-truth flag) collect line)
                  (cl-loop for flag in keep
                           for row from 0
                           when (exc-calc-truth flag)
                           collect (exc-calc-array-row-list array row)))))
      (cond (kept (exc-calc--from-lines kept by-col))
            ((and empty (not (equal empty '(MISSING))))
             ;; A reference given is read: FILTER gives an array, never a
             ;; reference, so ISREF(FILTER(...)) is FALSE.
             (let ((value (exc-calc-arg empty 'values)))
               (if (exc-calc-ref-p value) (exc-calc-arg-array value) value)))
            (t (exc-calc-err "#CALC!"))))))

;;;----------------------------------------------------------------------
;;; Stacking, wrapping and cutting several values
;;;----------------------------------------------------------------------

;; These take the cells they fill from a lazy argument, whose error
;; value is a value like any other: a PAD-WITH of #N/A fills cells as
;; Excel's does.

(defun exc-calc--pad-value (argument)
  "Return the value ARGUMENT, a `values' argument, gives the cells it fills."
  (or (car (exc-calc-flat-values argument)) :blank))

(defun exc-calc--fill-value (argument)
  "Return the value the lazy ARGUMENT gives the cells a function fills,
#N/A when it is left out."
  (if argument
      (exc-calc--pad-value (exc-calc-arg argument 'values))
    (exc-calc-err "#N/A")))

(defun exc-calc--line-values (array)
  "Return the values of ARRAY, one row of them or one column.
Several rows and columns are #VALUE!."
  (if (or (= 1 (exc-calc-array-rows array))
          (= 1 (exc-calc-array-cols array)))
      (append (exc-calc-array-data array) nil)
    (exc-calc-raise "#VALUE!")))

(defun exc-calc--stack (arrays across)
  "Return ARRAYS put together, side by side when ACROSS, else one under
another.  A cell past the size of one of them is #N/A, as Excel fills
it."
  (let* ((height (if across
                     (apply #'max (mapcar #'exc-calc-array-rows arrays))
                   (cl-loop for array in arrays
                            sum (exc-calc-array-rows array))))
         (width (if across
                    (cl-loop for array in arrays
                             sum (exc-calc-array-cols array))
                  (apply #'max (mapcar #'exc-calc-array-cols arrays))))
         (result (exc-calc-array height width
                                  (lambda (_row _col)
                                    (exc-calc-err "#N/A"))))
         (data (exc-calc-array-data result))
         (row 0) (col 0))
    (dolist (array arrays)
      (let ((array-data (exc-calc-array-data array))
            (cols (exc-calc-array-cols array)))
        (dotimes (r (exc-calc-array-rows array))
          (dotimes (c cols)
            (aset data (+ (* (+ row r) width) (+ col c))
                  (aref array-data (+ (* r cols) c)))))
        (if across (setq col (+ col cols))
          (setq row (+ row (exc-calc-array-rows array))))))
    result))

(exc-calc-define vstack (&rest (arrays array))
  "The values of ARRAYS running one under another, #N/A where one is
shorter than the rest."
  (if arrays (exc-calc--stack arrays nil) (exc-calc-raise "#VALUE!")))

(exc-calc-define hstack (&rest (arrays array))
  "The values of ARRAYS running side by side, #N/A where one is shorter
than the rest."
  (if arrays (exc-calc--stack arrays t) (exc-calc-raise "#VALUE!")))

(defun exc-calc--values-in-order (array ignore by-col)
  "Return the values of ARRAY one after another, row by row, or column by
column when BY-COL.  IGNORE 1 leaves out blanks, 2 error values, 3 both."
  (let ((rows (exc-calc-array-rows array))
        (cols (exc-calc-array-cols array))
        (values '()))
    (dotimes (outer (if by-col cols rows))
      (dotimes (inner (if by-col rows cols))
        (let ((value (if by-col
                         (exc-calc-aref array inner outer)
                       (exc-calc-aref array outer inner))))
          (unless (or (and (memq ignore '(1 3)) (eq value :blank))
                      (and (memq ignore '(2 3)) (exc-calc-err-p value)))
            (push value values)))))
    (nreverse values)))

(exc-calc-define tocol ((array array) &optional (ignore integer 0)
                         (scan-by-column bool))
  "The values of ARRAY in one column, a row after a row, or a column after
a column with SCAN-BY-COLUMN.  IGNORE 1 leaves out blanks, 2 error
values, 3 both."
  :empty t
  (unless (memq ignore '(0 1 2 3)) (exc-calc-raise "#VALUE!"))
  (exc-calc-array-from-list
   (exc-calc--values-in-order array ignore scan-by-column)))

(exc-calc-define torow ((array array) &optional (ignore integer 0)
                         (scan-by-column bool))
  "The values of ARRAY in one row, a row after a row, or a column after a
column with SCAN-BY-COLUMN.  IGNORE 1 leaves out blanks, 2 error values,
3 both."
  :empty t
  (unless (memq ignore '(0 1 2 3)) (exc-calc-raise "#VALUE!"))
  (exc-calc-array-from-list
   (exc-calc--values-in-order array ignore scan-by-column) t))

(defun exc-calc--wrapped (values wrap-count rows-first pad)
  "Return VALUES in lines of WRAP-COUNT, the last filled out with PAD.
Lines are rows when ROWS-FIRST, else columns.  One value alone is a
line of one, as Excel has WRAPCOLS(FALSE,2)."
  (let* ((count (length values))
         (lines (ceiling (max count 1) wrap-count))
         (across (if (<= count 1) 1 wrap-count))
         (height (if rows-first lines across))
         (width (if rows-first across lines)))
    (exc-calc-array height width
                     (lambda (row col)
                       (let ((at (if rows-first
                                     (+ (* row wrap-count) col)
                                   (+ (* col wrap-count) row))))
                         (if (< at count) (nth at values) pad))))))

(exc-calc-define wraprows ((array array) (wrap-count integer)
                            &optional (pad-with lazy))
  "The values of ARRAY wrapped into rows of WRAP-COUNT columns, the last
row filled out with PAD-WITH, #N/A without one."
  (unless (> wrap-count 0) (exc-calc-raise "#NUM!"))
  (exc-calc--wrapped (exc-calc--line-values array) wrap-count t
                      (exc-calc--fill-value pad-with)))

(exc-calc-define wrapcols ((array array) (wrap-count integer)
                            &optional (pad-with lazy))
  "The values of ARRAY wrapped into columns of WRAP-COUNT rows, the last
column filled out with PAD-WITH, #N/A without one."
  (unless (> wrap-count 0) (exc-calc-raise "#NUM!"))
  (exc-calc--wrapped (exc-calc--line-values array) wrap-count nil
                      (exc-calc--fill-value pad-with)))

(defun exc-calc--taken (count size)
  "Return (START . COUNT) of the rows or columns TAKE keeps of SIZE of
them: the first COUNT of them, or the last ones when COUNT is negative."
  (cond ((null count) (cons 0 size))
        ((zerop count) (exc-calc-raise "#VALUE!"))
        ((> count 0) (cons 0 (min count size)))
        (t (let ((count (min (- count) size)))
             (cons (- size count) count)))))

(defun exc-calc--dropped (count size)
  "Return (START . COUNT) of the rows or columns DROP keeps of SIZE of
them, COUNT of them dropped from the first, or from the last when
negative, all of them when COUNT is left out or 0.  Dropping all of
them is #VALUE!, as DROP gives it."
  (cond ((or (null count) (zerop count)) (cons 0 size))
        (t (let ((dropped (min (abs count) size)))
             (when (= dropped size) (exc-calc-raise "#VALUE!"))
             (if (> count 0)
                 (cons dropped (- size dropped))
               (cons 0 (- size dropped)))))))

(exc-calc-define take ((array array) &optional (rows integer) (cols integer))
  "The first ROWS rows of ARRAY and the first COLS columns of it; a
negative number counts from the last."
  :empty t
  (let ((height (exc-calc-array-rows array))
        (width (exc-calc-array-cols array)))
    (pcase-let ((`(,top . ,kept-rows) (exc-calc--taken rows height))
                (`(,left . ,kept-cols) (exc-calc--taken cols width)))
      (exc-calc--part array top left kept-rows kept-cols))))

(exc-calc-define drop ((array array) &optional (rows integer) (cols integer))
  "ARRAY without its first ROWS rows and its first COLS columns; a
negative number counts from the last."
  :empty t
  (let ((height (exc-calc-array-rows array))
        (width (exc-calc-array-cols array)))
    (pcase-let ((`(,top . ,kept-rows) (exc-calc--dropped rows height))
                (`(,left . ,kept-cols) (exc-calc--dropped cols width)))
      (exc-calc--part array top left kept-rows kept-cols))))

(exc-calc-define expand ((array array) &optional (rows integer) (cols integer)
                          (pad-with lazy))
  "ARRAY made ROWS by COLS cells, filled out with PAD-WITH, #N/A without
one; a size left out stays as it is."
  :empty t
  (let* ((height (or rows (exc-calc-array-rows array)))
         (width (or cols (exc-calc-array-cols array)))
         (pad (exc-calc--fill-value pad-with)))
    (when (or (< height (exc-calc-array-rows array))
              (< width (exc-calc-array-cols array)))
      (exc-calc-raise "#VALUE!"))
    (exc-calc-array height width
                     (lambda (row col)
                       (if (and (< row (exc-calc-array-rows array))
                                (< col (exc-calc-array-cols array)))
                           (exc-calc-aref array row col)
                         pad)))))

(exc-calc-define sortby ((array array) (by-array values) &rest (rest values))
  "The rows of ARRAY sorted by the values of BY-ARRAY, or its columns when
BY-ARRAY is a row of them.  Each array after BY-ARRAY sorts what the ones
before it leave in order, and a 1 after one of them sorts up, a -1 down."
  (let (pairs)
    ;; Each array to sort by, with the 1 or -1 that follows it.
    (dolist (argument (cons by-array rest))
      (cond
       ((numberp argument)
        (unless (and pairs (memq (truncate argument) '(1 -1)))
          (exc-calc-raise "#VALUE!"))
        (setcar (car pairs) (truncate argument)))
       ((exc-calc-err-p argument)
        (exc-calc-raise (exc-calc-err-code argument)))
       (t (push (cons 1 (exc-calc-arg-array argument)) pairs))))
    (setq pairs (nreverse pairs))
    (let* ((first (cdr (car pairs)))
           (by-rows (exc-calc-array-rows first))
           (by-cols (exc-calc-array-cols first))
           (rows (exc-calc-array-rows array))
           (cols (exc-calc-array-cols array))
           (by-col (cond ((and (= by-cols 1) (= by-rows rows)) nil)
                         ((and (= by-rows 1) (= by-cols cols)) t)
                         (t (exc-calc-raise "#VALUE!"))))
           (count (if by-col cols rows))
           (lines (exc-calc--lines array by-col))
           (keys (mapcar (lambda (pair)
                           (let ((values (exc-calc--line-values (cdr pair))))
                             (unless (= (length values) count)
                               (exc-calc-raise "#VALUE!"))
                             values))
                         pairs))
           (order (number-sequence 0 (1- count))))
      ;; The last pair sorts first, the ones before it keeping what it
      ;; leaves in order.
      (cl-loop for key in (reverse keys)
               for direction in (mapcar #'car (reverse pairs))
               do (setq order
                        (cl-stable-sort
                         order
                         (lambda (a b)
                           (exc-calc--sorts-before-p (nth a key) (nth b key)
                                                      direction)))))
      (exc-calc--from-lines (mapcar (lambda (index) (nth index lines)) order)
                             by-col))))

(provide 'exc-calc-lookup)

;;; exc-calc-lookup.el ends here
