;;; grid-table-calc-stats.el --- Statistical functions -*- lexical-binding: t -*-

;;; Commentary:

;; Counting, averages, the largest and smallest values, ranks and
;; percentiles, spread, and SUBTOTAL and AGGREGATE, which do these over
;; the rows that are shown.  Distributions are in grid-table-calc-dist.

;;; Code:

(require 'grid-table-calc)
(require 'grid-table-calc-math)

;;;----------------------------------------------------------------------
;;; Counting
;;;----------------------------------------------------------------------

(grid-calc-define count (&rest (values values))
  "How many of VALUES are numbers.
Written as an argument, TRUE, FALSE and text that reads as a number
count too."
  :errors t
  (let ((count 0))
    (dolist (argument values count)
      (grid-calc-each-value
       argument
       (lambda (value direct)
         (when (or (numberp value)
                   (and direct (or (grid-calc-boolean-p value)
                                   (and (stringp value)
                                        (grid-calc-text-number value)))))
           (cl-incf count)))))))

(grid-calc-define counta (&rest (values values))
  "How many of VALUES are not empty cells."
  :errors t
  (let ((count 0))
    (dolist (argument values count)
      (grid-calc-each-value argument
                            (lambda (value _)
                              (unless (eq value :blank) (cl-incf count)))))))

(grid-calc-define countblank ((range values))
  "How many cells of RANGE are empty or hold empty text."
  :errors t
  (pcase-let ((`(,rows . ,cols) (grid-calc--table-full-size range)))
    (let ((filled 0))
      (grid-calc-each-value range
                            (lambda (value _)
                              (unless (or (eq value :blank) (equal value ""))
                                (cl-incf filled))))
      (- (* rows cols) filled))))

(grid-calc-define countif ((range values) (criterion values))
  "How many cells of RANGE meet CRITERION."
  (let ((criterion (grid-calc--deref criterion)))
    (if (grid-calc-array-p criterion)
        ;; Read RANGE once, and count each criterion once, rather than
        ;; for every cell of an array as COUNTIF(B:B,B:B&"") has.
        (pcase-let* ((`(,rows . ,cols) (grid-calc--range-shape range))
                     (filled (min rows (car (grid-calc--table-size range))))
                     (values (if (grid-calc-ref-p range)
                                 (grid-calc-ref-to-array
                                  (grid-calc--part range 0 0 filled cols))
                               range))
                     (counts (make-hash-table :test #'equal)))
          (grid-calc-array-map
           (lambda (one)
             (with-memoization (gethash one counts)
               (+ (length (grid-calc--matching-cells (list (cons values one))))
                  (if (and (< filled rows)
                           (funcall (grid-calc-criterion one) :blank))
                      (* (- rows filled) cols)
                    0))))
           criterion))
      (grid-calc--matching-count (list (cons range criterion))))))

(grid-calc-define countifs (&rest (criteria values))
  "How many cells meet every criterion, beside each other in their ranges.
CRITERIA alternate a range and the criterion its cells must meet."
  (grid-calc--matching-count (grid-calc--criteria-pairs criteria)))

(grid-calc-define frequency ((data array) (bins array))
  "How many numbers of DATA fall in each bin BINS bound, as a column.
A bin holds the numbers above the bin before it and not above its own
bound; one more holds those above the last."
  (let* ((numbers (cl-remove-if-not #'numberp (grid-calc-array-values data)))
         (bounds (cl-remove-if-not #'numberp (grid-calc-array-values bins)))
         (sorted (sort (copy-sequence bounds) #'<))
         (counts (make-vector (1+ (length bounds)) 0)))
    (dolist (n numbers)
      (let ((bin (or (cl-position-if (lambda (bound) (<= n bound)) sorted)
                     (length sorted))))
        (cl-incf (aref counts bin))))
    ;; Counts go in the order the bounds were written.
    (let ((order (append (mapcar (lambda (bound) (cl-position bound sorted)) bounds)
                         (list (length sorted)))))
      (grid-calc-array (length order) 1
                       (lambda (row _) (aref counts (nth row order)))))))

;;;----------------------------------------------------------------------
;;; Averages and spread
;;;----------------------------------------------------------------------

(defun grid-calc--mean (numbers)
  "Return the mean of NUMBERS; #DIV/0! when there are none."
  (unless numbers (grid-calc-raise "#DIV/0!"))
  (grid-calc--tidy-number (grid-calc--snap (/ (float (apply #'+ numbers))
                                              (length numbers)))))

(defun grid-calc--squares (numbers)
  "Return the sum of the squares of NUMBERS from their mean."
  (let ((mean (/ (float (apply #'+ numbers)) (length numbers))))
    (cl-loop for n in numbers sum (expt (- n mean) 2))))

(defun grid-calc--variance (numbers sample)
  "Return the variance of NUMBERS, of a SAMPLE or of all there are."
  (let ((count (length numbers)))
    (when (< count (if sample 2 1)) (grid-calc-raise "#DIV/0!"))
    (/ (grid-calc--squares numbers) (if sample (1- count) count))))

(grid-calc-define average (&rest (numbers values))
  "The mean of NUMBERS."
  (grid-calc--mean (grid-calc-numbers numbers)))

(grid-calc-define averagea (&rest (values values))
  "The mean of VALUES, where text counts as 0 and TRUE as 1."
  (grid-calc--mean (grid-calc-numbers values 'a)))

(grid-calc-define averageif ((range values) (criterion any) &optional (average-range values))
  "The mean of the cells of AVERAGE-RANGE, or RANGE, beside cells of
RANGE that meet CRITERION."
  (let ((target (grid-calc--target range (or average-range range))))
    (grid-calc--mean
     (grid-calc--numbers-at target
                            (grid-calc--matching-cells (list (cons range criterion))
                                                       target)))))

(grid-calc-define averageifs ((average-range values) &rest (criteria values))
  "The mean of the cells of AVERAGE-RANGE beside cells meeting every
criterion of CRITERIA, which alternate ranges and criteria."
  (grid-calc--mean (grid-calc--numbers-at
                    average-range
                    (grid-calc--matching-cells (grid-calc--criteria-pairs criteria)
                                               average-range))))

(defmacro grid-calc--define-spread (name doc function &optional how)
  "Define NAME, DOC, as FUNCTION of the numbers of its arguments.
HOW is passed on to `grid-calc-numbers'."
  `(grid-calc-define ,name (&rest (numbers values))
     ,doc
     (grid-calc--finite (funcall ,function (grid-calc-numbers numbers ,how)))))

(grid-calc--define-spread var.s "The variance of NUMBERS, a sample."
                          (lambda (ns) (grid-calc--variance ns t)))
(grid-calc--define-spread var.p "The variance of NUMBERS, all there are."
                          (lambda (ns) (grid-calc--variance ns nil)))
(grid-calc--define-spread stdev.s "The standard deviation of NUMBERS, a sample."
                          (lambda (ns) (sqrt (grid-calc--variance ns t))))
(grid-calc--define-spread stdev.p "The standard deviation of NUMBERS, all there are."
                          (lambda (ns) (sqrt (grid-calc--variance ns nil))))
(grid-calc--define-spread vara "The variance of VALUES, a sample, text as 0."
                          (lambda (ns) (grid-calc--variance ns t)) 'a)
(grid-calc--define-spread varpa "The variance of VALUES, all, text as 0."
                          (lambda (ns) (grid-calc--variance ns nil)) 'a)
(grid-calc--define-spread stdeva "The standard deviation of VALUES, text as 0."
                          (lambda (ns) (sqrt (grid-calc--variance ns t))) 'a)
(grid-calc--define-spread stdevpa "The standard deviation of all VALUES, text as 0."
                          (lambda (ns) (sqrt (grid-calc--variance ns nil))) 'a)
(grid-calc--define-spread devsq "The sum of squares of NUMBERS from their mean."
                          (lambda (ns) (if ns (grid-calc--squares ns) 0)))
(grid-calc--define-spread avedev "The mean distance of NUMBERS from their mean."
                          (lambda (ns)
                            (unless ns (grid-calc-raise "#NUM!"))
                            (let ((mean (/ (float (apply #'+ ns)) (length ns))))
                              (/ (cl-loop for n in ns sum (abs (- n mean)))
                                 (length ns)))))
(grid-calc-alias var var.s)
(grid-calc-alias varp var.p)
(grid-calc-alias stdev stdev.s)
(grid-calc-alias stdevp stdev.p)

(defun grid-calc--paired (left right)
  "Return the pairs of numbers of LEFT and RIGHT, arrays of one size."
  (grid-calc--number-pairs left right))

(defun grid-calc--covariance (left right sample)
  "Return the covariance of arrays LEFT and RIGHT, of a SAMPLE or not."
  (let* ((pairs (grid-calc--paired left right))
         (n (length pairs)))
    (when (< n (if sample 2 1)) (grid-calc-raise "#DIV/0!"))
    (let ((mean-x (/ (float (apply #'+ (mapcar #'car pairs))) n))
          (mean-y (/ (float (apply #'+ (mapcar #'cdr pairs))) n)))
      (/ (cl-loop for (x . y) in pairs sum (* (- x mean-x) (- y mean-y)))
         (if sample (1- n) n)))))

(grid-calc-define covariance.p ((left array) (right array))
  "The covariance of the pairs of LEFT and RIGHT, all there are."
  (grid-calc--tidy-number (grid-calc--covariance left right nil)))

(grid-calc-define covariance.s ((left array) (right array))
  "The covariance of the pairs of LEFT and RIGHT, a sample."
  (grid-calc--tidy-number (grid-calc--covariance left right t)))

(grid-calc-alias covar covariance.p)

(grid-calc-define correl ((left array) (right array))
  "The correlation of the pairs of LEFT and RIGHT."
  (let* ((pairs (grid-calc--paired left right))
         (xs (mapcar #'car pairs)) (ys (mapcar #'cdr pairs)))
    (when (< (length pairs) 2) (grid-calc-raise "#DIV/0!"))
    (let ((sx (grid-calc--squares xs)) (sy (grid-calc--squares ys)))
      (when (or (zerop sx) (zerop sy)) (grid-calc-raise "#DIV/0!"))
      (/ (* (grid-calc--covariance left right nil) (length pairs))
         (sqrt (* sx sy))))))

;;;----------------------------------------------------------------------
;;; Largest, smallest and in between
;;;----------------------------------------------------------------------

(grid-calc-define min (&rest (numbers values))
  "The smallest of NUMBERS; 0 when there are none."
  (let ((numbers (grid-calc-numbers numbers)))
    (if numbers (apply #'min numbers) 0)))

(grid-calc-define max (&rest (numbers values))
  "The largest of NUMBERS; 0 when there are none."
  (let ((numbers (grid-calc-numbers numbers)))
    (if numbers (apply #'max numbers) 0)))

(grid-calc-define mina (&rest (values values))
  "The smallest of VALUES, where text counts as 0 and TRUE as 1."
  (let ((numbers (grid-calc-numbers values 'a)))
    (if numbers (apply #'min numbers) 0)))

(grid-calc-define maxa (&rest (values values))
  "The largest of VALUES, where text counts as 0 and TRUE as 1."
  (let ((numbers (grid-calc-numbers values 'a)))
    (if numbers (apply #'max numbers) 0)))

(grid-calc-define minifs ((range values) &rest (criteria values))
  "The smallest cell of RANGE beside cells meeting every criterion."
  (let ((numbers (grid-calc--numbers-at
                  range (grid-calc--matching-cells (grid-calc--criteria-pairs criteria)
                                                   range))))
    (if numbers (apply #'min numbers) 0)))

(grid-calc-define maxifs ((range values) &rest (criteria values))
  "The largest cell of RANGE beside cells meeting every criterion."
  (let ((numbers (grid-calc--numbers-at
                  range (grid-calc--matching-cells (grid-calc--criteria-pairs criteria)
                                                   range))))
    (if numbers (apply #'max numbers) 0)))

(defun grid-calc--sorted-numbers (argument)
  "Return the numbers of ARGUMENT, a `values' argument, sorted up."
  (sort (grid-calc-numbers (list argument)) #'<))

(defun grid-calc--median (numbers)
  "Return the median of NUMBERS; #NUM! when there are none."
  (unless numbers (grid-calc-raise "#NUM!"))
  (let* ((sorted (vconcat (sort (copy-sequence numbers) #'<)))
         (n (length sorted)))
    (if (cl-oddp n) (aref sorted (/ n 2))
      (grid-calc--tidy-number (/ (+ (aref sorted (1- (/ n 2))) (aref sorted (/ n 2)))
                                 2.0)))))

(grid-calc-define median (&rest (numbers values))
  "The middle of NUMBERS, or the mean of the two middle ones."
  (grid-calc--median (grid-calc-numbers numbers)))

(defun grid-calc--modes (numbers)
  "Return the numbers coming most often in NUMBERS, more than once.
They are in the order they first come; #N/A when none repeats."
  (let ((counts '()) (best 1))
    (dolist (n numbers)
      (let ((cell (assoc n counts #'=)))
        (if cell (setcdr cell (1+ (cdr cell))) (push (cons n 1) counts))))
    (setq counts (nreverse counts))
    (dolist (cell counts) (setq best (max best (cdr cell))))
    (when (= best 1) (grid-calc-raise "#N/A"))
    (mapcar #'car (cl-remove-if-not (lambda (cell) (= (cdr cell) best)) counts))))

(grid-calc-define mode.sngl (&rest (numbers values))
  "The number coming most often in NUMBERS, the first when several do."
  (car (grid-calc--modes (grid-calc-numbers numbers))))

(grid-calc-alias mode mode.sngl)

(grid-calc-define mode.mult (&rest (numbers values))
  "The numbers coming most often in NUMBERS, as a column."
  (grid-calc-array-from-list (grid-calc--modes (grid-calc-numbers numbers))))

(defun grid-calc--kth (numbers k largest)
  "Return the Kth LARGEST, or smallest, of NUMBERS, counting from 1."
  (let ((k (if (floatp k) (truncate (fceiling (grid-calc--snap k))) k)))
    (unless (and numbers (<= 1 k (length numbers))) (grid-calc-raise "#NUM!"))
    (nth (1- k) (sort (copy-sequence numbers) (if largest #'> #'<)))))

(grid-calc-define large ((numbers values) (k any))
  "The Kth largest of NUMBERS."
  (let ((numbers (grid-calc-numbers (list numbers))))
    (if (grid-calc-array-p k)
        (grid-calc-array-map (lambda (k) (grid-calc--catching
                                           (grid-calc--kth numbers (grid-calc-num k) t)))
                             k)
      (grid-calc--kth numbers (grid-calc-num k) t))))

(grid-calc-define small ((numbers values) (k any))
  "The Kth smallest of NUMBERS."
  (let ((numbers (grid-calc-numbers (list numbers))))
    (if (grid-calc-array-p k)
        (grid-calc-array-map (lambda (k) (grid-calc--catching
                                           (grid-calc--kth numbers (grid-calc-num k) nil)))
                             k)
      (grid-calc--kth numbers (grid-calc-num k) nil))))

(defun grid-calc--rank (number numbers ascending average)
  "Return the rank of NUMBER among NUMBERS, the largest first.
ASCENDING ranks the smallest first; AVERAGE gives ties their mean rank."
  (unless (cl-member number numbers :test #'=) (grid-calc-raise "#N/A"))
  (let ((before (cl-count-if (lambda (n) (if ascending (< n number) (> n number)))
                             numbers))
        (ties (cl-count number numbers :test #'=)))
    (if average (grid-calc--tidy-number (+ before (/ (1+ ties) 2.0)))
      (1+ before))))

(grid-calc-define rank.eq ((number number) (numbers values) &optional (order number 0))
  "The rank of NUMBER among NUMBERS, the largest first, or the smallest
when ORDER is not 0.  Equal numbers share the best rank."
  (grid-calc--rank number (grid-calc-numbers (list numbers)) (/= order 0) nil))

(grid-calc-alias rank rank.eq)

(grid-calc-define rank.avg ((number number) (numbers values) &optional (order number 0))
  "The rank of NUMBER among NUMBERS, equal numbers sharing their mean rank."
  (grid-calc--rank number (grid-calc-numbers (list numbers)) (/= order 0) t))

(defun grid-calc--percentile (numbers k exclusive)
  "Return the K percentile of NUMBERS, K from 0 to 1.
EXCLUSIVE leaves out the first and last as PERCENTILE.EXC does."
  (let* ((sorted (vconcat (sort (copy-sequence numbers) #'<)))
         (n (length sorted)))
    (when (or (= n 0) (< k 0) (> k 1)) (grid-calc-raise "#NUM!"))
    (let ((position (if exclusive (1- (* k (1+ n))) (* k (1- n)))))
      (when (or (< position 0) (> position (1- n))) (grid-calc-raise "#NUM!"))
      (let* ((low (truncate (ffloor (grid-calc--snap position))))
             (fraction (- position low)))
        (grid-calc--tidy-number
         (grid-calc--snap
          (if (or (zerop fraction) (>= low (1- n))) (float (aref sorted low))
            (+ (aref sorted low) (* fraction (- (aref sorted (1+ low))
                                                (aref sorted low)))))))))))

(grid-calc-define percentile.inc ((numbers values) (k number))
  "The K percentile of NUMBERS, K from 0 to 1."
  (grid-calc--percentile (grid-calc-numbers (list numbers)) k nil))

(grid-calc-alias percentile percentile.inc)

(grid-calc-define percentile.exc ((numbers values) (k number))
  "The K percentile of NUMBERS, K between 0 and 1, the ends left out."
  (grid-calc--percentile (grid-calc-numbers (list numbers)) k t))

(grid-calc-define quartile.inc ((numbers values) (quart integer))
  "The QUART quartile of NUMBERS, 0 their smallest to 4 their largest."
  (unless (<= 0 quart 4) (grid-calc-raise "#NUM!"))
  (grid-calc--percentile (grid-calc-numbers (list numbers)) (/ quart 4.0) nil))

(grid-calc-alias quartile quartile.inc)

(grid-calc-define quartile.exc ((numbers values) (quart integer))
  "The QUART quartile of NUMBERS, 1 to 3, the ends left out."
  (unless (<= 1 quart 3) (grid-calc-raise "#NUM!"))
  (grid-calc--percentile (grid-calc-numbers (list numbers)) (/ quart 4.0) t))

(grid-calc-define percentrank.inc ((numbers values) (number number)
                                   &optional (digits integer 3))
  "Where NUMBER stands among NUMBERS, from 0 to 1, to DIGITS places."
  (let* ((sorted (vconcat (sort (grid-calc-numbers (list numbers)) #'<)))
         (n (length sorted)))
    (when (or (= n 0) (< digits 1)) (grid-calc-raise "#NUM!"))
    (when (or (< number (aref sorted 0)) (> number (aref sorted (1- n))))
      (grid-calc-raise "#N/A"))
    (let* ((below (cl-count-if (lambda (x) (< x number)) sorted))
           (rank (if (= n 1) 1.0
                   (if (cl-find number sorted :test #'=)
                       (/ (float below) (1- n))
                     ;; Between two values: as far between their ranks
                     (let ((low (aref sorted (1- below)))
                           (high (aref sorted below)))
                       (/ (+ (1- below) (/ (- number low) (float (- high low))))
                          (1- n)))))))
      (grid-calc--round-to rank digits 'down))))

(grid-calc-alias percentrank percentrank.inc)

;;;----------------------------------------------------------------------
;;; SUBTOTAL and AGGREGATE
;;;----------------------------------------------------------------------

(defun grid-calc--subtotal-cell-p (model row col)
  "Return non-nil when the cell at ROW and COL of MODEL is a SUBTOTAL or
AGGREGATE, which these functions leave out to not count twice."
  (let ((raw (grid-cell-raw-input
              (aref (grid-data-model-cells model)
                    (+ (* row (grid-data-model-cols model)) col)))))
    (and (grid-calc--formula-p raw)
         (string-match-p "\\b\\(SUBTOTAL\\|AGGREGATE\\)(" (upcase raw)))))

(defun grid-calc--shown-values (arguments skip-hidden skip-errors
                                           &optional keep-nested)
  "Return the values of ARGUMENTS as SUBTOTAL and AGGREGATE see them.
Cells holding a SUBTOTAL or AGGREGATE are left out unless KEEP-NESTED,
and with SKIP-HIDDEN the hidden rows; SKIP-ERRORS leaves error values
out."
  (let (values)
    (dolist (argument arguments)
      (if (grid-calc-ref-p argument)
          (let* ((model (grid-calc-ref-model argument))
                 (hidden (and skip-hidden (car (grid-data-model-hidden model))))
                 (top (grid-calc-ref-top argument))
                 (left (grid-calc-ref-left argument))
                 (cols (max 0 (min (grid-calc-ref-cols argument)
                                   (- (grid-data-model-cols model) left)))))
            (dotimes (row (grid-calc-ref-filled-rows argument))
              (unless (memq (+ top row) hidden)
                (dotimes (col cols)
                  (unless (and (not keep-nested)
                               (grid-calc--subtotal-cell-p model (+ top row) (+ left col)))
                    (push (grid-calc--cell model (+ top row) (+ left col)) values))))))
        (grid-calc-each-value argument (lambda (value _) (push value values)))))
    (setq values (nreverse values))
    (if skip-errors (cl-remove-if #'grid-calc-err-p values)
      (let ((error (cl-find-if #'grid-calc-err-p values)))
        (when error (grid-calc-raise (grid-calc-err-code error))))
      values)))

(defun grid-calc--aggregate-by (number values &optional k)
  "Return function NUMBER of SUBTOTAL or AGGREGATE over VALUES.
K is the extra argument of AGGREGATE's functions 14 to 19."
  (let ((numbers (cl-remove-if-not #'numberp values)))
    (pcase number
      (1 (grid-calc--mean numbers))
      (2 (length numbers))
      (3 (cl-count-if-not (lambda (v) (eq v :blank)) values))
      (4 (if numbers (apply #'max numbers) 0))
      (5 (if numbers (apply #'min numbers) 0))
      (6 (if numbers (grid-calc--finite (grid-calc--snap (apply #'* numbers))) 0))
      (7 (grid-calc--finite (sqrt (grid-calc--variance numbers t))))
      (8 (grid-calc--finite (sqrt (grid-calc--variance numbers nil))))
      (9 (grid-calc--add numbers))
      (10 (grid-calc--finite (grid-calc--variance numbers t)))
      (11 (grid-calc--finite (grid-calc--variance numbers nil)))
      (12 (grid-calc--median numbers))
      (13 (car (grid-calc--modes numbers)))
      (14 (grid-calc--kth numbers (grid-calc-num k) t))
      (15 (grid-calc--kth numbers (grid-calc-num k) nil))
      (16 (grid-calc--percentile numbers (grid-calc-num k) nil))
      (17 (grid-calc--percentile numbers (/ (grid-calc-int k) 4.0) nil))
      (18 (grid-calc--percentile numbers (grid-calc-num k) t))
      (19 (grid-calc--percentile numbers (/ (grid-calc-int k) 4.0) t))
      (_ (grid-calc-raise "#VALUE!")))))

(grid-calc-define subtotal ((function integer) &rest (ranges values))
  "FUNCTION of the values of RANGES, leaving out other subtotals.
FUNCTION 1 to 11 is AVERAGE, COUNT, COUNTA, MAX, MIN, PRODUCT, STDEV,
STDEVP, SUM, VAR and VARP; 101 to 111 is the same leaving out hidden
rows."
  (unless (or (<= 1 function 11) (<= 101 function 111))
    (grid-calc-raise "#VALUE!"))
  (grid-calc--aggregate-by (% function 100)
                           (grid-calc--shown-values ranges (> function 100) nil)))

(grid-calc-define aggregate ((function integer) (options integer) (range values)
                             &rest (more values))
  "FUNCTION of the values of RANGE, leaving out what OPTIONS says.
FUNCTION 1 to 13 is as SUBTOTAL's, then MEDIAN and MODE.SNGL; 14 to 19
are LARGE, SMALL, PERCENTILE.INC, QUARTILE.INC, PERCENTILE.EXC and
QUARTILE.EXC, which take K as the next argument.  OPTIONS 0 to 7: odd
ones leave out hidden rows, 2 and 3 and 6 and 7 error values, 0 to 3
other subtotals."
  (unless (and (<= 1 function 19) (<= 0 options 7)) (grid-calc-raise "#VALUE!"))
  (let ((skip-hidden (memq options '(1 3 5 7)))
        (skip-errors (memq options '(2 3 6 7)))
        (keep-nested (> options 3)))
    (if (>= function 14)
        (grid-calc--aggregate-by
         function (grid-calc--shown-values (list range) skip-hidden skip-errors
                                           keep-nested)
         (grid-calc--first (or (car more) (grid-calc-raise "#VALUE!"))))
      (grid-calc--aggregate-by
       function (grid-calc--shown-values (cons range more) skip-hidden skip-errors
                                         keep-nested)))))

;;;----------------------------------------------------------------------
;;; Database functions
;;;----------------------------------------------------------------------

(defun grid-calc--database-rows (database criteria)
  "Return the rows of DATABASE, from 1, that meet the CRITERIA table.
Both are ranges or arrays whose first row names the columns.  A row of
CRITERIA is met when each criterion in it is; any row will do."
  (pcase-let ((`(,rows . ,cols) (grid-calc--table-size database))
              (`(,c-rows . ,c-cols) (grid-calc--table-size criteria)))
    (let ((names (cl-loop for col below cols
                          collect (grid-calc-text (grid-calc--table-at database 0 col))))
          (tests '()))
      (cl-loop for c-row from 1 below c-rows
               do (push (cl-loop for c-col below c-cols
                                 for name = (grid-calc-text
                                             (grid-calc--table-at criteria 0 c-col))
                                 for criterion = (grid-calc--table-at criteria c-row c-col)
                                 for col = (cl-position name names :test #'string-equal)
                                 unless (grid-calc-blank-p criterion)
                                 collect (cons (or col (grid-calc-raise "#VALUE!"))
                                               (grid-calc-criterion criterion)))
                        tests))
      (cl-loop for row from 1 below rows
               when (cl-some (lambda (all)
                               (cl-every (lambda (test)
                                           (funcall (cdr test)
                                                    (grid-calc--table-at database row
                                                                         (car test))))
                                         all))
                             tests)
               collect row))))

(defun grid-calc--database-field (database field)
  "Return the column of DATABASE that FIELD, a name or number from 1, is."
  (if (numberp field)
      (1- (truncate field))
    (or (cl-loop for col below (cdr (grid-calc--table-size database))
                 when (string-equal (grid-calc-text (grid-calc--table-at database 0 col))
                                    field)
                 return col)
        (grid-calc-raise "#VALUE!"))))

(grid-calc-define dcount ((database values) (field any) (criteria values))
  "How many rows of DATABASE meeting CRITERIA hold a number in FIELD.
Without FIELD every row meeting CRITERIA that holds a number counts."
  (let ((rows (grid-calc--database-rows database criteria)))
    (if (grid-calc-blank-p field)
        (length rows)
      (let ((col (grid-calc--database-field database field)))
        (cl-count-if (lambda (row) (numberp (grid-calc--table-at database row col)))
                     rows)))))

(provide 'grid-table-calc-stats)

;;; grid-table-calc-stats.el ends here
