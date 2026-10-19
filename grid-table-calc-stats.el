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
  (pcase-let ((`(,rows . ,cols) (grid-calc--count-size range)))
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
                     (`(,on-rows . ,on-cols) (grid-calc--table-size range))
                     (filled (min rows on-rows))
                     (filled-cols (min cols on-cols))
                     (past (- (* rows cols) (* filled filled-cols)))
                     (values (if (grid-calc-ref-p range)
                                 (grid-calc-ref-to-array
                                  (grid-calc--part range 0 0 filled filled-cols))
                               range))
                     (counts (make-hash-table :test #'equal)))
          (grid-calc-array-map
           (lambda (one)
             (with-memoization (gethash one counts)
               (+ (length (grid-calc--matching-cells (list (cons values one))))
                  (if (and (> past 0)
                           (funcall (grid-calc-criterion one) :blank))
                      past
                    0))))
           criterion))
      (grid-calc--matching-count (list (cons range criterion))))))

(grid-calc-define countifs (&rest (criteria values))
  "How many cells meet every criterion, beside each other in their ranges.
CRITERIA alternate a range and the criterion its cells must meet."
  (grid-calc--each-criteria criteria #'grid-calc--matching-count))

(grid-calc-define frequency ((data values) (bins array))
  "How many numbers of DATA fall in each bin BINS bound, as a column.
A bin holds the numbers above the bin before it and not above its own
bound; one more holds those above the last.  DATA is read first: one
value that is no number, as an IF of one cell gives when it is false,
is #VALUE!, and an error value is itself.  Text, a logical value and a
blank are left out of a range or an array.  BINS without a number is
the one bound 0, as in Excel."
  (let (numbers)
    ;; One value written directly must be a number; what a range or an
    ;; array holds besides numbers is left out.
    (grid-calc-each-value
     data
     (lambda (value direct)
       (cond ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
             ((numberp value) (push value numbers))
             ((and direct (not (grid-calc-blank-p value)))
              (grid-calc-raise "#VALUE!")))))
    (grid-calc--raise-first-error bins)
    (let* ((numbers (nreverse numbers))
           (bounds (or (cl-remove-if-not #'numberp (grid-calc-array-values bins))
                       (list 0)))
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
                         (lambda (row _) (aref counts (nth row order))))))))

;;;----------------------------------------------------------------------
;;; Averages and spread
;;;----------------------------------------------------------------------

(defun grid-calc--mean (numbers)
  "Return the mean of NUMBERS; #DIV/0! when there are none."
  (unless numbers (grid-calc-raise "#DIV/0!"))
  (grid-calc--tidy-number (/ (float (apply #'+ numbers)) (length numbers))))

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

(grid-calc-define averageif ((range values) (criterion values) &optional (average-range values))
  "The mean of the cells of AVERAGE-RANGE, or RANGE, beside cells of
RANGE that meet CRITERION.  An error as CRITERION meets that error."
  (let ((criterion (grid-calc--deref criterion))
        (target (grid-calc--target range (or average-range range))))
    (cl-flet ((average (one)
                (grid-calc--mean
                 (grid-calc--numbers-at
                  target (grid-calc--matching-cells (list (cons range one))
                                                    target)))))
      (if (grid-calc-array-p criterion)
          (grid-calc-array-map
           (lambda (one) (grid-calc--catching (average one)))
           criterion)
        (average criterion)))))

(grid-calc-define averageifs ((average-range values) &rest (criteria values))
  "The mean of the cells of AVERAGE-RANGE beside cells meeting every
criterion of CRITERIA, which alternate ranges and criteria."
  (grid-calc--each-criteria
   criteria
   (lambda (pairs)
     (grid-calc--mean (grid-calc--numbers-at
                       average-range
                       (grid-calc--matching-cells pairs average-range))))))

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
                          (lambda (ns)
                            (unless ns (grid-calc-raise "#NUM!"))
                            (grid-calc--squares ns)))
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

(defun grid-calc--correlation (left right)
  "Return the correlation of the pairs of numbers of LEFT and RIGHT."
  (let* ((pairs (grid-calc--paired left right))
         (xs (mapcar #'car pairs)) (ys (mapcar #'cdr pairs)))
    (when (< (length pairs) 2) (grid-calc-raise "#DIV/0!"))
    (let ((sx (grid-calc--squares xs)) (sy (grid-calc--squares ys)))
      (when (or (zerop sx) (zerop sy)) (grid-calc-raise "#DIV/0!"))
      (/ (* (grid-calc--covariance left right nil) (length pairs))
         (sqrt (* sx sy))))))

(grid-calc-define correl ((left array) (right array))
  "The correlation of the pairs of LEFT and RIGHT."
  (grid-calc--correlation left right))

(grid-calc-alias pearson correl)

(grid-calc-define rsq ((known-y array) (known-x array))
  "The square of the correlation of KNOWN-Y with KNOWN-X."
  (let ((r (grid-calc--correlation known-y known-x)))
    (* r r)))

(defun grid-calc--line (known-y known-x)
  "Return the least-squares line of KNOWN-Y on KNOWN-X.
It is a list of the slope, the intercept and the pairs (Y . X)."
  (let* ((pairs (grid-calc--paired known-y known-x))
         (ys (mapcar #'car pairs)) (xs (mapcar #'cdr pairs)))
    (unless pairs (grid-calc-raise "#DIV/0!"))
    (let ((sxx (grid-calc--squares xs)))
      (when (zerop sxx) (grid-calc-raise "#DIV/0!"))
      (let* ((n (length pairs))
             (mean-x (/ (float (apply #'+ xs)) n))
             (mean-y (/ (float (apply #'+ ys)) n))
             (slope (/ (cl-loop for (y . x) in pairs
                                sum (* (- x mean-x) (- y mean-y)))
                       sxx)))
        (list slope (- mean-y (* slope mean-x)) pairs)))))

(grid-calc-define slope ((known-y array) (known-x array))
  "The slope of the least-squares line of KNOWN-Y on KNOWN-X."
  (grid-calc--tidy-number (car (grid-calc--line known-y known-x))))

(grid-calc-define intercept ((known-y array) (known-x array))
  "Where the least-squares line of KNOWN-Y on KNOWN-X meets the y axis."
  (grid-calc--tidy-number (cadr (grid-calc--line known-y known-x))))

(grid-calc-define forecast ((x number) (known-y array) (known-x array))
  "The value at X of the least-squares line of KNOWN-Y on KNOWN-X."
  (pcase-let ((`(,slope ,intercept ,_) (grid-calc--line known-y known-x)))
    (grid-calc--tidy-number (+ intercept (* slope x)))))

(grid-calc-alias forecast.linear forecast)

(grid-calc-define steyx ((known-y array) (known-x array))
  "The standard error of the Y predicted from each X by their line."
  (pcase-let ((`(,slope ,intercept ,pairs) (grid-calc--line known-y known-x)))
    (when (< (length pairs) 3) (grid-calc-raise "#DIV/0!"))
    (sqrt (/ (cl-loop for (y . x) in pairs
                      sum (expt (- y (+ intercept (* slope x))) 2))
             (- (length pairs) 2)))))

(defun grid-calc--moment (numbers power sample)
  "Return the sum of the POWERth powers of NUMBERS in standard deviations
from their mean, the deviation of a SAMPLE or of all there are."
  (let* ((n (length numbers))
         (mean (/ (float (apply #'+ numbers)) n))
         (sd (sqrt (/ (grid-calc--squares numbers) (if sample (1- n) n)))))
    (when (zerop sd) (grid-calc-raise "#DIV/0!"))
    (cl-loop for x in numbers sum (expt (/ (- x mean) sd) power))))

(grid-calc-define kurt (&rest (numbers values))
  "The kurtosis of NUMBERS."
  (let* ((numbers (grid-calc-numbers numbers)) (n (length numbers)))
    (when (< n 4) (grid-calc-raise "#DIV/0!"))
    (- (* (/ (* n (+ n 1.0)) (* (- n 1) (- n 2) (- n 3)))
          (grid-calc--moment numbers 4 t))
       (/ (* 3.0 (expt (- n 1) 2)) (* (- n 2) (- n 3))))))

(grid-calc-define skew (&rest (numbers values))
  "The skewness of NUMBERS, a sample."
  (let* ((numbers (grid-calc-numbers numbers)) (n (length numbers)))
    (when (< n 3) (grid-calc-raise "#DIV/0!"))
    (* (/ n (* (- n 1.0) (- n 2))) (grid-calc--moment numbers 3 t))))

(grid-calc-define skew.p (&rest (numbers values))
  "The skewness of NUMBERS, all there are."
  (let* ((numbers (grid-calc-numbers numbers)) (n (length numbers)))
    (when (< n 1) (grid-calc-raise "#DIV/0!"))
    (/ (grid-calc--moment numbers 3 nil) n)))

(defun grid-calc--positive-numbers (values)
  "Return the numbers of VALUES; #NUM! when there are none or one is not
above 0."
  (let ((numbers (grid-calc-numbers values)))
    (unless (and numbers (cl-every (lambda (x) (> x 0)) numbers))
      (grid-calc-raise "#NUM!"))
    numbers))

(grid-calc-define geomean (&rest (numbers values))
  "The geometric mean of NUMBERS."
  (let ((numbers (grid-calc--positive-numbers numbers)))
    (grid-calc--tidy-number
     (exp (/ (apply #'+ (mapcar #'log numbers)) (length numbers))))))

(grid-calc-define harmean (&rest (numbers values))
  "The harmonic mean of NUMBERS."
  (let ((numbers (grid-calc--positive-numbers numbers)))
    (grid-calc--tidy-number
     (/ (length numbers) (apply #'+ (mapcar (lambda (x) (/ 1.0 x)) numbers))))))

(grid-calc-define trimmean ((numbers values) (percent number))
  "The mean of NUMBERS without the PERCENT of them at the two ends."
  (let* ((sorted (sort (grid-calc-numbers (list numbers)) #'<))
         (n (length sorted)))
    (unless (and (> n 0) (<= 0 percent) (< percent 1)) (grid-calc-raise "#NUM!"))
    (let ((cut (floor (* n percent) 2)))
      (grid-calc--mean (cl-subseq sorted cut (- n cut))))))

(grid-calc-define devsq (&rest (numbers values))
  "The sum of the squares of NUMBERS from their mean."
  (let ((numbers (grid-calc-numbers numbers)))
    (unless numbers (grid-calc-raise "#NUM!"))
    (grid-calc--tidy-number (grid-calc--squares numbers))))

(grid-calc-define avedev (&rest (numbers values))
  "The mean of the distances of NUMBERS from their mean."
  (let ((numbers (grid-calc-numbers numbers)))
    (unless numbers (grid-calc-raise "#NUM!"))
    (let ((mean (/ (float (apply #'+ numbers)) (length numbers))))
      (grid-calc--mean (mapcar (lambda (x) (abs (- x mean))) numbers)))))

(grid-calc-define fisher ((x number))
  "The Fisher transformation of X."
  (grid-calc--require (< -1 x 1))
  (* 0.5 (log (/ (+ 1.0 x) (- 1.0 x)))))

(grid-calc-define fisherinv ((y number))
  "The X whose Fisher transformation is Y."
  (if (> (abs y) 300) (if (> y 0) 1.0 -1.0)
    ;; As Excel works it out, which a Y near 0 shows
    (let ((e (exp (* 2.0 y)))) (/ (- e 1) (+ e 1)))))

(grid-calc-define prob ((xs array) (chances array) (low number) &optional (high number))
  "The chance of a value of XS from LOW to HIGH, or of LOW, where each X
comes with the chance beside it in CHANCES."
  (let* ((pairs (grid-calc--paired xs chances))
         (total (apply #'+ (mapcar #'cdr pairs))))
    (unless (and (cl-every (lambda (p) (< 0 (cdr p) 1.0000001)) pairs)
                 (< (abs (- total 1)) 1e-7))
      (grid-calc-raise "#NUM!"))
    (let ((high (or high low)))
      (grid-calc--tidy-number
       (cl-loop for (x . chance) in pairs
                when (<= low x high) sum chance)))))

;;;----------------------------------------------------------------------
;;; Regression: LINEST, LOGEST, TREND and GROWTH
;;;----------------------------------------------------------------------

(defun grid-calc--regression-data (known-y known-x)
  "Return the data of a regression of KNOWN-Y on KNOWN-X.
It is a list of the Ys, a vector; the variables, a list of vectors as
long; and how they lie in KNOWN-X: `one' for a single variable,
`columns' when each is a column, `rows' when each is a row.  Without
KNOWN-X the variable is 1, 2, 3 and on.  Sizes that do not fit are
#REF!; a value that is not a number #VALUE!."
  (let* ((rows (grid-calc-array-rows known-y)) (cols (grid-calc-array-cols known-y))
         (n (* rows cols))
         (x-rows (and known-x (grid-calc-array-rows known-x)))
         (x-cols (and known-x (grid-calc-array-cols known-x)))
         (lie (cond ((or (null known-x) (= (* x-rows x-cols) n)) 'one)
                    ((and (= cols 1) (= x-rows n)) 'columns)
                    ((and (= rows 1) (= x-cols n)) 'rows)
                    (t (grid-calc-raise "#REF!"))))
         (ys (apply #'vconcat (append (grid-calc--regression-matrix known-y) nil))))
    (list ys
          (pcase lie
            ('one (list (if known-x
                            (apply #'vconcat (append (grid-calc--regression-matrix known-x) nil))
                          (vconcat (number-sequence 1.0 n)))))
            ('columns (let ((m (grid-calc--regression-matrix known-x)))
                        (cl-loop for j below x-cols
                                 collect (cl-map 'vector (lambda (row) (aref row j)) m))))
            ('rows (append (grid-calc--regression-matrix known-x) nil)))
          lie)))

(defun grid-calc--regression-matrix (array)
  "Return ARRAY as a matrix for a regression: as `grid-calc--matrix'
does, but an error in it is #VALUE!, as Excel has it."
  (when (cl-some #'grid-calc-err-p (grid-calc-array-values array))
    (grid-calc-raise "#VALUE!"))
  (grid-calc--matrix array))

(defun grid-calc--dot (a b)
  "Return the dot product of vectors A and B."
  (cl-loop for x across a for y across b sum (* x y)))

(defun grid-calc--fit (ys vars const)
  "Fit YS by least squares to the vectors VARS, through 0 unless CONST.
A variable that is the same as a sum of those before it, or with CONST
a constant, gets the coefficient 0, as in Excel.  Return a plist of
:slopes, :intercept, :errors (the standard errors of the slopes),
:intercept-error, :r2, :sey, :f, :df, :ssreg and :ssresid; a statistic
Excel cannot give is an error value."
  (let* ((n (length ys))
         (mean (lambda (v) (if const (/ (cl-reduce #'+ v) n) 0.0)))
         (y-mean (funcall mean ys))
         (x-means (mapcar mean vars))
         (centered (cl-mapcar (lambda (v m) (cl-map 'vector (lambda (x) (- x m)) v))
                              vars x-means))
         (basis '()) (kept '()))
    ;; Gram-Schmidt: keep a variable only when it adds a direction
    (cl-loop for v in centered for j from 0
             for size = (grid-calc--dot v v)
             for rest = (copy-sequence v)
             do (dolist (q basis)
                  (let ((c (/ (grid-calc--dot rest q) (grid-calc--dot q q))))
                    (dotimes (i n) (aset rest i (- (aref rest i) (* c (aref q i)))))))
             when (> (grid-calc--dot rest rest) (* 1e-13 size))
             do (push rest basis) (push j kept))
    (setq kept (nreverse kept))
    (let* ((m (length kept))
           (xs (mapcar (lambda (j) (nth j centered)) kept))
           (yc (cl-map 'vector (lambda (y) (- y y-mean)) ys))
           (inverse (and (> m 0)
                         (grid-calc--eliminate
                          (apply #'vector
                                 (mapcar (lambda (a)
                                           (apply #'vector
                                                  (mapcar (lambda (b) (grid-calc--dot a b)) xs)))
                                         xs))
                          t)))
           (products (mapcar (lambda (x) (grid-calc--dot x yc)) xs))
           (coefs (cl-loop for i below m
                           collect (cl-loop for p in products for j from 0
                                            sum (* (aref (aref inverse i) j) p))))
           (slopes (cl-loop for j below (length vars)
                            collect (let ((at (cl-position j kept)))
                                      (if at (nth at coefs) 0.0))))
           (intercept (if const
                          (- y-mean (cl-loop for b in slopes for xm in x-means
                                             sum (* b xm)))
                        0.0))
           (fitted (cl-loop for i below n
                            collect (+ intercept
                                       (cl-loop for b in slopes for v in vars
                                                sum (* b (aref v i))))))
           (ssresid (cl-loop for y across ys for f in fitted sum (expt (- y f) 2)))
           (ssreg (cl-loop for f in fitted sum (expt (- f y-mean) 2)))
           (df (- n m (if const 1 0)))
           (sey (if (> df 0) (sqrt (/ ssresid df)) (grid-calc-err "#NUM!")))
           (error-of (lambda (variance)
                       (if (numberp sey) (* sey (sqrt variance)) sey))))
      (list :slopes slopes :intercept intercept
            :errors (cl-loop for j below (length vars)
                             collect (let ((at (cl-position j kept)))
                                       (if at (funcall error-of (aref (aref inverse at) at))
                                         0.0)))
            :intercept-error
            (if (not const) (grid-calc-err "#N/A")
              (funcall error-of
                       (+ (/ 1.0 n)
                          (cl-loop for a in kept for i from 0
                                   sum (cl-loop for b in kept for j from 0
                                                sum (* (nth a x-means) (nth b x-means)
                                                       (aref (aref inverse i) j)))))))
            :r2 (if (zerop (+ ssreg ssresid)) (grid-calc-err "#NUM!")
                  (/ ssreg (+ ssreg ssresid)))
            :sey sey
            :f (if (or (= m 0) (<= df 0) (zerop ssresid)) (grid-calc-err "#NUM!")
                 (/ (/ ssreg m) (/ ssresid df)))
            :df (if (< df 0) (grid-calc-err "#NUM!") df)
            :ssreg ssreg :ssresid ssresid))))

(defun grid-calc--flag (value default)
  "Return VALUE as true or false, DEFAULT when it is left out."
  (cond ((or (null value) (eq value :blank)) default)
        ((grid-calc-err-p value) (grid-calc-raise "#VALUE!"))
        (t (grid-calc-truth value))))

(defun grid-calc--estimate (known-y known-x const stats logs)
  "Return what LINEST gives for KNOWN-Y, KNOWN-X, CONST and STATS, or,
with LOGS, what LOGEST gives."
  (pcase-let* ((`(,ys ,vars ,_) (grid-calc--regression-data known-y known-x))
               (ys (if logs (grid-calc--logs ys) ys))
               (fit (grid-calc--fit ys vars (grid-calc--flag const t)))
               (shown (lambda (x) (if (and logs (numberp x)) (exp x) x)))
               (first (mapcar shown (append (reverse (plist-get fit :slopes))
                                            (list (plist-get fit :intercept)))))
               (width (length first))
               (pad (lambda (&rest values)
                      (append values (make-list (- width (length values))
                                                (grid-calc-err "#N/A"))))))
    (grid-calc-array-from-rows
     (if (not (grid-calc--flag stats nil)) (list first)
       (list first
             (append (reverse (plist-get fit :errors))
                     (list (plist-get fit :intercept-error)))
             (funcall pad (plist-get fit :r2) (plist-get fit :sey))
             (funcall pad (plist-get fit :f) (plist-get fit :df))
             (funcall pad (plist-get fit :ssreg) (plist-get fit :ssresid)))))))

(defun grid-calc--logs (ys)
  "Return the logs of YS; #NUM! when one is not above 0."
  (cl-map 'vector (lambda (y) (if (> y 0) (log y) (grid-calc-raise "#NUM!"))) ys))

(grid-calc-define linest ((known-y array) &optional (known-x array) (const any) (stats any))
  "The least-squares line of KNOWN-Y on KNOWN-X: its slopes, last
variable first, then its intercept.  Through 0 when CONST is false;
with STATS, four more rows of statistics."
  :errors t
  (grid-calc--estimate known-y known-x const stats nil))

(grid-calc-define logest ((known-y array) &optional (known-x array) (const any) (stats any))
  "The exponential curve fit to KNOWN-Y on KNOWN-X: its bases, last
variable first, then its constant.  CONST and STATS are as for LINEST."
  :errors t
  (grid-calc--estimate known-y known-x const stats t))

(defun grid-calc--predict (known-y known-x new-x const logs)
  "Return the Y at each NEW-X of the fit of KNOWN-Y on KNOWN-X, as TREND
gives, or as GROWTH does with LOGS."
  (pcase-let* ((`(,ys ,vars ,lie) (grid-calc--regression-data known-y known-x))
               (ys (if logs (grid-calc--logs ys) ys))
               (fit (grid-calc--fit ys vars (grid-calc--flag const t)))
               (slopes (plist-get fit :slopes))
               (intercept (plist-get fit :intercept))
               (new-x (or new-x known-x
                          (grid-calc-array (grid-calc-array-rows known-y)
                                           (grid-calc-array-cols known-y)
                                           (let ((cols (grid-calc-array-cols known-y)))
                                             (lambda (row col) (+ 1 col (* row cols)))))))
               (matrix (grid-calc--regression-matrix new-x))
               (k (length vars))
               (at (lambda (xs)
                     (let ((y (+ intercept (cl-loop for b in slopes for x in xs
                                                    sum (* b x)))))
                       (grid-calc--tidy-number (if logs (exp y) y))))))
    (pcase lie
      ('one (grid-calc-array (grid-calc-array-rows new-x) (grid-calc-array-cols new-x)
                             (lambda (row col)
                               (funcall at (list (aref (aref matrix row) col))))))
      ('columns
       (unless (= (grid-calc-array-cols new-x) k) (grid-calc-raise "#REF!"))
       (grid-calc-array (length matrix) 1
                        (lambda (row _) (funcall at (append (aref matrix row) nil)))))
      ('rows
       (unless (= (grid-calc-array-rows new-x) k) (grid-calc-raise "#REF!"))
       (grid-calc-array 1 (grid-calc-array-cols new-x)
                        (lambda (_ col)
                          (funcall at (cl-map 'list (lambda (row) (aref row col))
                                              matrix))))))))

(grid-calc-define trend ((known-y array) &optional (known-x array) (new-x array) (const any))
  "The Y at each NEW-X of the least-squares line of KNOWN-Y on KNOWN-X,
through 0 when CONST is false."
  :errors t
  (grid-calc--predict known-y known-x new-x const nil))

(grid-calc-define growth ((known-y array) &optional (known-x array) (new-x array) (const any))
  "The Y at each NEW-X of the exponential curve fit to KNOWN-Y on KNOWN-X,
through 1 when CONST is false."
  :errors t
  (grid-calc--predict known-y known-x new-x const t))

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
  (grid-calc--each-criteria
   criteria
   (lambda (pairs)
     (let ((numbers (grid-calc--numbers-at
                     range (grid-calc--matching-cells pairs range))))
       (if numbers (apply #'min numbers) 0)))))

(grid-calc-define maxifs ((range values) &rest (criteria values))
  "The largest cell of RANGE beside cells meeting every criterion."
  (grid-calc--each-criteria
   criteria
   (lambda (pairs)
     (let ((numbers (grid-calc--numbers-at
                     range (grid-calc--matching-cells pairs range))))
       (if numbers (apply #'max numbers) 0)))))

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

(defun grid-calc--mode-numbers (arguments)
  "Return the numbers of ARGUMENTS as MODE takes them.
Excel takes each argument as an array there, so one written directly,
or a reference to one cell, is #VALUE! unless it is a number."
  (dolist (argument arguments)
    (let ((value (cond ((and (grid-calc-ref-p argument)
                             (= (grid-calc-ref-rows argument) 1)
                             (= (grid-calc-ref-cols argument) 1))
                        (grid-calc--deref argument))
                       ((not (or (grid-calc-ref-p argument)
                                 (grid-calc-array-p argument)
                                 (eq (car-safe argument) 'union)))
                        argument)
                       (t 0))))
      (cond ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
            ((not (numberp value)) (grid-calc-raise "#VALUE!")))))
  (grid-calc-numbers arguments))

(grid-calc-define mode.sngl (&rest (numbers values))
  "The number coming most often in NUMBERS, the first when several do."
  (car (grid-calc--modes (grid-calc--mode-numbers numbers))))

(grid-calc-alias mode mode.sngl)

(grid-calc-define mode.mult (&rest (numbers values))
  "The numbers coming most often in NUMBERS, as a column."
  (grid-calc-array-from-list (grid-calc--modes (grid-calc--mode-numbers numbers))))

(defun grid-calc--kth (numbers k largest)
  "Return the Kth LARGEST, or smallest, of NUMBERS, counting from 1.
A fraction of K counts up, but K below 1 is #NUM!, as in Excel."
  (when (< k 1) (grid-calc-raise "#NUM!"))
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

(grid-calc-define rank.eq ((number number) (numbers areas) &optional (order number 0))
  "The rank of NUMBER among NUMBERS, the largest first, or the smallest
when ORDER is not 0.  Equal numbers share the best rank."
  (grid-calc--rank number (grid-calc-numbers (list numbers)) (/= order 0) nil))

(grid-calc-alias rank rank.eq)

(grid-calc-define rank.avg ((number number) (numbers areas) &optional (order number 0))
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
      (let* ((low (floor (grid-calc--snap position)))
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

(grid-calc-define quartile.inc ((numbers values) (quart number))
  "The QUART quartile of NUMBERS, 0 their smallest to 4 their largest.
A fraction is cut off, but one below 0 is #NUM!, as in Excel.  An error
in NUMBERS comes before QUART is checked."
  (let ((numbers (grid-calc-numbers (list numbers))))
    (unless (<= 0 quart) (grid-calc-raise "#NUM!"))
    (setq quart (truncate quart))
    (unless (<= quart 4) (grid-calc-raise "#NUM!"))
    (grid-calc--percentile numbers (/ quart 4.0) nil)))

(grid-calc-alias quartile quartile.inc)

(grid-calc-define quartile.exc ((numbers values) (quart integer))
  "The QUART quartile of NUMBERS, 1 to 3, the ends left out.
An error in NUMBERS comes before QUART is checked."
  (let ((numbers (grid-calc-numbers (list numbers))))
    (unless (<= 1 quart 3) (grid-calc-raise "#NUM!"))
    (grid-calc--percentile numbers (/ quart 4.0) t)))

(defun grid-calc--percent-rank (numbers number digits exclusive)
  "Return where NUMBER stands among NUMBERS, to DIGITS places.
From 0 to 1, or with EXCLUSIVE strictly between them."
  (let* ((sorted (vconcat (sort (grid-calc-numbers (list numbers)) #'<)))
         (n (length sorted)))
    (when (or (= n 0) (< digits 1)) (grid-calc-raise "#NUM!"))
    (when (or (< number (aref sorted 0)) (> number (aref sorted (1- n))))
      (grid-calc-raise "#N/A"))
    (let* ((below (cl-count-if (lambda (x) (< x number)) sorted))
           ;; Its place among them, counted from 1: between two values
           ;; as far between their places
           (place (if (cl-find number sorted :test #'=)
                      (1+ below)
                    (let ((low (aref sorted (1- below)))
                          (high (aref sorted below)))
                      (+ below (/ (- number low) (float (- high low)))))))
           (rank (cond (exclusive (/ place (float (1+ n))))
                       ((= n 1) 1.0)
                       (t (/ (1- place) (float (1- n)))))))
      (grid-calc--round-to rank digits 'down))))

(grid-calc-define percentrank.inc ((numbers values) (number number)
                                   &optional (digits integer 3))
  "Where NUMBER stands among NUMBERS, from 0 to 1, to DIGITS places."
  (grid-calc--percent-rank numbers number digits nil))

(grid-calc-define percentrank.exc ((numbers values) (number number)
                                   &optional (digits integer 3))
  "Where NUMBER stands among NUMBERS, between 0 and 1, to DIGITS places."
  (grid-calc--percent-rank numbers number digits t))

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
and with SKIP-HIDDEN the hidden rows, or with `filtered' only the ones
a filter left out; SKIP-ERRORS leaves error values
out, and `keep' keeps them, for COUNT and COUNTA to count.  Otherwise
an error value signals."
  (let (values)
    (dolist (argument arguments)
      (if (grid-calc-ref-p argument)
          (let* ((model (grid-calc-ref-model argument))
                 (hidden (pcase skip-hidden
                           ('nil nil)
                           ('filtered (grid-model-filtered-rows model))
                           (_ (car (grid-data-model-hidden model)))))
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
    (pcase skip-errors
      ('keep values)
      ('nil
       (let ((error (cl-find-if #'grid-calc-err-p values)))
         (when error (grid-calc-raise (grid-calc-err-code error))))
       values)
      (_ (cl-remove-if #'grid-calc-err-p values)))))

(defun grid-calc--counting-p (number)
  "Return non-nil when SUBTOTAL's function NUMBER is COUNT or COUNTA.
They count error values rather than give them, as COUNT and COUNTA do."
  (memq (% number 100) '(2 3)))

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
rows.  Either leaves out the rows a filter left out."
  (unless (or (<= 1 function 11) (<= 101 function 111))
    (grid-calc-raise "#VALUE!"))
  (grid-calc--aggregate-by (% function 100)
                           (grid-calc--shown-values
                            ranges (if (> function 100) t 'filtered)
                            (and (grid-calc--counting-p function) 'keep))))

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
        (skip-errors (or (memq options '(2 3 6 7))
                         (and (grid-calc--counting-p function) 'keep)))
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

(defun grid-calc--database-names (table)
  "Return the names atop the columns of TABLE, as text; nil for an error."
  (cl-loop for col below (cdr (grid-calc--table-size table))
           for name = (grid-calc--table-at table 0 col)
           collect (unless (grid-calc-err-p name) (grid-calc-text name))))

(defun grid-calc--same-name-p (name other)
  "Return non-nil when NAME and OTHER, column names or nil, are one."
  (and name other (string-equal-ignore-case name other)))

(defun grid-calc--database-criterion (criterion)
  "Return a function telling whether a value meets CRITERION.
Text with no operator before it is met by text that starts with it."
  (grid-calc-criterion
   (if (and (stringp criterion) (not (string-empty-p criterion))
            (not (string-match-p "\\`[<>=]" criterion))
            (not (grid-calc-text-number criterion)))
       (concat criterion "*")
     criterion)))

(defun grid-calc--database-rows (database criteria)
  "Return the rows of DATABASE, from 1, that meet the CRITERIA table.
Both are ranges or arrays whose first row names the columns, without
regard to case.  A row of CRITERIA is met when each criterion in it is,
none under a name DATABASE lacks; any row will do.  A DATABASE of no
more than its names is #VALUE!."
  (pcase-let ((`(,rows . ,_) (grid-calc--table-size database))
              (`(,c-rows . ,c-cols) (grid-calc--table-size criteria)))
    (when (< rows 2) (grid-calc-raise "#VALUE!"))
    (let ((names (grid-calc--database-names database))
          (c-names (grid-calc--database-names criteria))
          (tests '()))
      (cl-loop for c-row from 1 below c-rows
               do (push (cl-loop for c-col below c-cols
                                 for criterion = (grid-calc--table-at criteria c-row c-col)
                                 unless (grid-calc-blank-p criterion)
                                 collect (cons (cl-position (nth c-col c-names) names
                                                            :test #'grid-calc--same-name-p)
                                               (grid-calc--database-criterion criterion)))
                        tests))
      (cl-loop for row from 1 below rows
               when (cl-some (lambda (all)
                               (cl-every (lambda (test)
                                           (and (car test)
                                                (funcall (cdr test)
                                                         (grid-calc--table-at database row
                                                                              (car test)))))
                                         all))
                             tests)
               collect row))))

(defun grid-calc--database-field (database field)
  "Return the column of DATABASE that FIELD, a name or number from 1, is.
FIELD is a `values' argument; one left out is nil.  Several cells, a
blank one, or a name or number DATABASE has no column for is #VALUE!."
  (unless (eq field :blank)
    (let ((value (grid-calc--deref field))
          (cols (cdr (grid-calc--table-size database))))
      (cond
       ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
       ((or (grid-calc-array-p value) (grid-calc-blank-p value))
        (grid-calc-raise "#VALUE!"))
       ((stringp value)
        (or (cl-position value (grid-calc--database-names database)
                         :test #'grid-calc--same-name-p)
            (grid-calc-raise "#VALUE!")))
       (t (let ((col (1- (truncate (grid-calc-num value)))))
            (if (< -1 col cols) col (grid-calc-raise "#VALUE!"))))))))

(grid-calc-define dcount ((database values) (field values) (criteria values))
  "How many rows of DATABASE meeting CRITERIA hold a number in FIELD.
Without FIELD every row meeting CRITERIA that holds a number counts."
  (let ((rows (grid-calc--database-rows database criteria))
        (col (grid-calc--database-field database field)))
    (if col
        (cl-count-if (lambda (row) (numberp (grid-calc--table-at database row col)))
                     rows)
      (length rows))))

;; DCOUNTA and DGET read every value of the field, the others its numbers.

(defun grid-calc--database-values (database field criteria)
  "Return the values in FIELD of the rows of DATABASE meeting CRITERIA."
  (let ((rows (grid-calc--database-rows database criteria))
        (col (or (grid-calc--database-field database field)
                 (grid-calc-raise "#VALUE!"))))
    (mapcar (lambda (row) (grid-calc--table-at database row col)) rows)))

(defmacro grid-calc--define-database (name doc reduce)
  "Define NAME, a database function giving REDUCE of the numbers it reads.
DOC is its docstring."
  `(grid-calc-define ,name ((database values) (field values) (criteria values))
     ,doc
     (funcall ,reduce (cl-remove-if-not
                       #'numberp
                       (grid-calc--database-values database field criteria)))))

(grid-calc--define-database dsum "The sum of FIELD in the rows meeting CRITERIA."
                            (lambda (ns) (grid-calc--tidy-number (apply #'+ 0 ns))))
(grid-calc--define-database daverage "The mean of FIELD in the rows meeting CRITERIA."
                            #'grid-calc--mean)
(grid-calc--define-database dmin "The least of FIELD in the rows meeting CRITERIA."
                            (lambda (ns) (if ns (apply #'min ns) 0)))
(grid-calc--define-database dmax "The greatest of FIELD in the rows meeting CRITERIA."
                            (lambda (ns) (if ns (apply #'max ns) 0)))
(grid-calc--define-database dproduct "The product of FIELD in the rows meeting CRITERIA."
                            (lambda (ns) (if ns (grid-calc--finite (apply #'* ns)) 0)))
(grid-calc--define-database dvar "The variance of FIELD in the rows meeting CRITERIA, a sample."
                            (lambda (ns) (grid-calc--variance ns t)))
(grid-calc--define-database dvarp "The variance of FIELD in all the rows meeting CRITERIA."
                            (lambda (ns) (grid-calc--variance ns nil)))
(grid-calc--define-database dstdev "The standard deviation of FIELD in the rows meeting CRITERIA, a sample."
                            (lambda (ns) (sqrt (grid-calc--variance ns t))))
(grid-calc--define-database dstdevp "The standard deviation of FIELD in all the rows meeting CRITERIA."
                            (lambda (ns) (sqrt (grid-calc--variance ns nil))))

(grid-calc-define dcounta ((database values) (field values) (criteria values))
  "How many rows of DATABASE meeting CRITERIA hold a value in FIELD.
Without FIELD every row meeting CRITERIA counts."
  (if (eq field :blank)
      (length (grid-calc--database-rows database criteria))
    (cl-count-if-not #'grid-calc-blank-p
                     (grid-calc--database-values database field criteria))))

(grid-calc-define dget ((database values) (field values) (criteria values))
  "The one value in FIELD of the rows meeting CRITERIA.
#VALUE! when no row meets them, #NUM! when more than one does."
  (let ((values (grid-calc--database-values database field criteria)))
    (cond ((null values) (grid-calc-raise "#VALUE!"))
          ((cdr values) (grid-calc-raise "#NUM!"))
          (t (car values)))))

(provide 'grid-table-calc-stats)

;;; grid-table-calc-stats.el ends here
