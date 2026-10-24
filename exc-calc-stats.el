;;; exc-calc-stats.el --- Statistical functions -*- lexical-binding: t -*-

;;; Commentary:

;; Counting, averages, the largest and smallest values, ranks and
;; percentiles, spread, and SUBTOTAL and AGGREGATE, which do these over
;; the rows that are shown.  Distributions are in exc-calc-dist.

;;; Code:

(require 'exc-calc)
(require 'exc-calc-math)

;; The model is in exc-data-model.el, which loads exc-calc, which loads
;; this file: requiring it here would be a circular load.
(declare-function exc-data-model-cols "exc-data-model")
(declare-function exc-data-model-cells "exc-data-model")
(declare-function exc-cell-raw-input "exc-data-model")
(declare-function exc-model-filtered-rows "exc-data-model")
(declare-function exc-data-model-hidden "exc-data-model")

;;;----------------------------------------------------------------------
;;; Counting
;;;----------------------------------------------------------------------

(exc-calc-define count (&rest (values values))
  "How many of VALUES are numbers.
Written as an argument, TRUE, FALSE and text that reads as a number
count too."
  :errors t
  (let ((count 0))
    (dolist (argument values count)
      (exc-calc-each-value
       argument
       (lambda (value direct)
         (when (or (numberp value)
                   (and direct (or (exc-calc-boolean-p value)
                                   (and (stringp value)
                                        (exc-calc-text-number value)))))
           (cl-incf count)))))))

(exc-calc-define counta (&rest (values values))
  "How many of VALUES are not empty cells."
  :errors t
  (let ((count 0))
    (dolist (argument values count)
      (exc-calc-each-value argument
                            (lambda (value _)
                              (unless (eq value :blank) (cl-incf count)))))))

(exc-calc-define countblank ((range values))
  "How many cells of RANGE are empty or hold empty text."
  :errors t
  (pcase-let ((`(,rows . ,cols) (exc-calc--count-size range)))
    (let ((filled 0))
      (exc-calc-each-value range
                            (lambda (value _)
                              (unless (or (eq value :blank) (equal value ""))
                                (cl-incf filled))))
      (- (* rows cols) filled))))

(exc-calc-define countif ((range values) (criterion values))
  "How many cells of RANGE meet CRITERION."
  (let ((criterion (exc-calc--deref criterion)))
    (if (exc-calc-array-p criterion)
        ;; Read RANGE once, and count each criterion once, rather than
        ;; for every cell of an array as COUNTIF(B:B,B:B&"") has.
        (pcase-let* ((`(,rows . ,cols) (exc-calc--range-shape range))
                     (`(,on-rows . ,on-cols) (exc-calc--table-size range))
                     (filled (min rows on-rows))
                     (filled-cols (min cols on-cols))
                     (past (- (* rows cols) (* filled filled-cols)))
                     (values (if (exc-calc-ref-p range)
                                 (exc-calc-ref-to-array
                                  (exc-calc--part range 0 0 filled filled-cols))
                               range))
                     (counts (make-hash-table :test #'equal)))
          (exc-calc-array-map
           (lambda (one)
             (with-memoization (gethash one counts)
               (+ (length (exc-calc--matching-cells (list (cons values one))))
                  (if (and (> past 0)
                           (funcall (exc-calc-criterion one) :blank))
                      past
                    0))))
           criterion))
      (exc-calc--matching-count (list (cons range criterion))))))

(exc-calc-define countifs (&rest (criteria values))
  "How many cells meet every criterion, beside each other in their ranges.
CRITERIA alternate a range and the criterion its cells must meet."
  (exc-calc--each-criteria criteria #'exc-calc--matching-count))

(exc-calc-define frequency ((data values) (bins array))
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
    (exc-calc-each-value
     data
     (lambda (value direct)
       (cond ((exc-calc-err-p value) (exc-calc-raise (exc-calc-err-code value)))
             ((numberp value) (push value numbers))
             ((and direct (not (exc-calc-blank-p value)))
              (exc-calc-raise "#VALUE!")))))
    (exc-calc--raise-first-error bins)
    (let* ((numbers (nreverse numbers))
           (bounds (or (cl-remove-if-not #'numberp (exc-calc-array-values bins))
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
        (exc-calc-array (length order) 1
                         (lambda (row _) (aref counts (nth row order))))))))

;;;----------------------------------------------------------------------
;;; Averages and spread
;;;----------------------------------------------------------------------

(defun exc-calc--mean (numbers)
  "Return the mean of NUMBERS; #DIV/0! when there are none."
  (unless numbers (exc-calc-raise "#DIV/0!"))
  (exc-calc--tidy-number (/ (float (apply #'+ numbers)) (length numbers))))

(defun exc-calc--squares (numbers)
  "Return the sum of the squares of NUMBERS from their mean."
  (let ((mean (/ (float (apply #'+ numbers)) (length numbers))))
    (cl-loop for n in numbers sum (expt (- n mean) 2))))

(defun exc-calc--variance (numbers sample)
  "Return the variance of NUMBERS, of a SAMPLE or of all there are."
  (let ((count (length numbers)))
    (when (< count (if sample 2 1)) (exc-calc-raise "#DIV/0!"))
    (/ (exc-calc--squares numbers) (if sample (1- count) count))))

(exc-calc-define average (&rest (numbers values))
  "The mean of NUMBERS."
  (exc-calc--mean (exc-calc-numbers numbers)))

(exc-calc-define averagea (&rest (values values))
  "The mean of VALUES, where text counts as 0 and TRUE as 1."
  (exc-calc--mean (exc-calc-numbers values 'a)))

(exc-calc-define averageif ((range values) (criterion values) &optional (average-range values))
  "The mean of the cells of AVERAGE-RANGE, or RANGE, beside cells of
RANGE that meet CRITERION.  An error as CRITERION meets that error."
  (let ((criterion (exc-calc--deref criterion))
        (target (exc-calc--target range (or average-range range))))
    (cl-flet ((average (one)
                (exc-calc--mean
                 (exc-calc--numbers-at
                  target (exc-calc--matching-cells (list (cons range one))
                                                    target)))))
      (if (exc-calc-array-p criterion)
          (exc-calc-array-map
           (lambda (one) (exc-calc--catching (average one)))
           criterion)
        (average criterion)))))

(exc-calc-define averageifs ((average-range values) &rest (criteria values))
  "The mean of the cells of AVERAGE-RANGE beside cells meeting every
criterion of CRITERIA, which alternate ranges and criteria."
  (exc-calc--each-criteria
   criteria
   (lambda (pairs)
     (exc-calc--mean (exc-calc--numbers-at
                       average-range
                       (exc-calc--matching-cells pairs average-range))))))

(defmacro exc-calc--define-spread (name doc function &optional how)
  "Define NAME, DOC, as FUNCTION of the numbers of its arguments.
HOW is passed on to `exc-calc-numbers'."
  `(exc-calc-define ,name (&rest (numbers values))
     ,doc
     (exc-calc--finite (funcall ,function (exc-calc-numbers numbers ,how)))))

(exc-calc--define-spread var.s "The variance of NUMBERS, a sample."
                          (lambda (ns) (exc-calc--variance ns t)))
(exc-calc--define-spread var.p "The variance of NUMBERS, all there are."
                          (lambda (ns) (exc-calc--variance ns nil)))
(exc-calc--define-spread stdev.s "The standard deviation of NUMBERS, a sample."
                          (lambda (ns) (sqrt (exc-calc--variance ns t))))
(exc-calc--define-spread stdev.p "The standard deviation of NUMBERS, all there are."
                          (lambda (ns) (sqrt (exc-calc--variance ns nil))))
(exc-calc--define-spread vara "The variance of VALUES, a sample, text as 0."
                          (lambda (ns) (exc-calc--variance ns t)) 'a)
(exc-calc--define-spread varpa "The variance of VALUES, all, text as 0."
                          (lambda (ns) (exc-calc--variance ns nil)) 'a)
(exc-calc--define-spread stdeva "The standard deviation of VALUES, text as 0."
                          (lambda (ns) (sqrt (exc-calc--variance ns t))) 'a)
(exc-calc--define-spread stdevpa "The standard deviation of all VALUES, text as 0."
                          (lambda (ns) (sqrt (exc-calc--variance ns nil))) 'a)
(exc-calc--define-spread devsq "The sum of squares of NUMBERS from their mean."
                          (lambda (ns)
                            (unless ns (exc-calc-raise "#NUM!"))
                            (exc-calc--squares ns)))
(exc-calc--define-spread avedev "The mean distance of NUMBERS from their mean."
                          (lambda (ns)
                            (unless ns (exc-calc-raise "#NUM!"))
                            (let ((mean (/ (float (apply #'+ ns)) (length ns))))
                              (/ (cl-loop for n in ns sum (abs (- n mean)))
                                 (length ns)))))
(exc-calc-alias var var.s)
(exc-calc-alias varp var.p)
(exc-calc-alias stdev stdev.s)
(exc-calc-alias stdevp stdev.p)

(defun exc-calc--paired (left right)
  "Return the pairs of numbers of LEFT and RIGHT, arrays of one size."
  (exc-calc--number-pairs left right))

(defun exc-calc--covariance (left right sample)
  "Return the covariance of arrays LEFT and RIGHT, of a SAMPLE or not."
  (let* ((pairs (exc-calc--paired left right))
         (n (length pairs)))
    (when (< n (if sample 2 1)) (exc-calc-raise "#DIV/0!"))
    (let ((mean-x (/ (float (apply #'+ (mapcar #'car pairs))) n))
          (mean-y (/ (float (apply #'+ (mapcar #'cdr pairs))) n)))
      (/ (cl-loop for (x . y) in pairs sum (* (- x mean-x) (- y mean-y)))
         (if sample (1- n) n)))))

(exc-calc-define covariance.p ((left array) (right array))
  "The covariance of the pairs of LEFT and RIGHT, all there are."
  (exc-calc--tidy-number (exc-calc--covariance left right nil)))

(exc-calc-define covariance.s ((left array) (right array))
  "The covariance of the pairs of LEFT and RIGHT, a sample."
  (exc-calc--tidy-number (exc-calc--covariance left right t)))

(exc-calc-alias covar covariance.p)

(defun exc-calc--correlation (left right)
  "Return the correlation of the pairs of numbers of LEFT and RIGHT."
  (let* ((pairs (exc-calc--paired left right))
         (xs (mapcar #'car pairs)) (ys (mapcar #'cdr pairs)))
    (when (< (length pairs) 2) (exc-calc-raise "#DIV/0!"))
    (let ((sx (exc-calc--squares xs)) (sy (exc-calc--squares ys)))
      (when (or (zerop sx) (zerop sy)) (exc-calc-raise "#DIV/0!"))
      (/ (* (exc-calc--covariance left right nil) (length pairs))
         (sqrt (* sx sy))))))

(exc-calc-define correl ((left array) (right array))
  "The correlation of the pairs of LEFT and RIGHT."
  (exc-calc--correlation left right))

(exc-calc-alias pearson correl)

(exc-calc-define rsq ((known-y array) (known-x array))
  "The square of the correlation of KNOWN-Y with KNOWN-X."
  (let ((r (exc-calc--correlation known-y known-x)))
    (* r r)))

(defun exc-calc--line (known-y known-x)
  "Return the least-squares line of KNOWN-Y on KNOWN-X.
It is a list of the slope, the intercept and the pairs (Y . X)."
  (let* ((pairs (exc-calc--paired known-y known-x))
         (ys (mapcar #'car pairs)) (xs (mapcar #'cdr pairs)))
    (unless pairs (exc-calc-raise "#DIV/0!"))
    (let ((sxx (exc-calc--squares xs)))
      (when (zerop sxx) (exc-calc-raise "#DIV/0!"))
      (let* ((n (length pairs))
             (mean-x (/ (float (apply #'+ xs)) n))
             (mean-y (/ (float (apply #'+ ys)) n))
             (slope (/ (cl-loop for (y . x) in pairs
                                sum (* (- x mean-x) (- y mean-y)))
                       sxx)))
        (list slope (- mean-y (* slope mean-x)) pairs)))))

(exc-calc-define slope ((known-y array) (known-x array))
  "The slope of the least-squares line of KNOWN-Y on KNOWN-X."
  (exc-calc--tidy-number (car (exc-calc--line known-y known-x))))

(exc-calc-define intercept ((known-y array) (known-x array))
  "Where the least-squares line of KNOWN-Y on KNOWN-X meets the y axis."
  (exc-calc--tidy-number (cadr (exc-calc--line known-y known-x))))

(exc-calc-define forecast ((x number) (known-y array) (known-x array))
  "The value at X of the least-squares line of KNOWN-Y on KNOWN-X."
  (pcase-let ((`(,slope ,intercept ,_) (exc-calc--line known-y known-x)))
    (exc-calc--tidy-number (+ intercept (* slope x)))))

(exc-calc-alias forecast.linear forecast)

(exc-calc-define steyx ((known-y array) (known-x array))
  "The standard error of the Y predicted from each X by their line."
  (pcase-let ((`(,slope ,intercept ,pairs) (exc-calc--line known-y known-x)))
    (when (< (length pairs) 3) (exc-calc-raise "#DIV/0!"))
    (sqrt (/ (cl-loop for (y . x) in pairs
                      sum (expt (- y (+ intercept (* slope x))) 2))
             (- (length pairs) 2)))))

(defun exc-calc--moment (numbers power sample)
  "Return the sum of the POWERth powers of NUMBERS in standard deviations
from their mean, the deviation of a SAMPLE or of all there are."
  (let* ((n (length numbers))
         (mean (/ (float (apply #'+ numbers)) n))
         (sd (sqrt (/ (exc-calc--squares numbers) (if sample (1- n) n)))))
    (when (zerop sd) (exc-calc-raise "#DIV/0!"))
    (cl-loop for x in numbers sum (expt (/ (- x mean) sd) power))))

(exc-calc-define kurt (&rest (numbers values))
  "The kurtosis of NUMBERS."
  (let* ((numbers (exc-calc-numbers numbers)) (n (length numbers)))
    (when (< n 4) (exc-calc-raise "#DIV/0!"))
    (- (* (/ (* n (+ n 1.0)) (* (- n 1) (- n 2) (- n 3)))
          (exc-calc--moment numbers 4 t))
       (/ (* 3.0 (expt (- n 1) 2)) (* (- n 2) (- n 3))))))

(exc-calc-define skew (&rest (numbers values))
  "The skewness of NUMBERS, a sample."
  (let* ((numbers (exc-calc-numbers numbers)) (n (length numbers)))
    (when (< n 3) (exc-calc-raise "#DIV/0!"))
    (* (/ n (* (- n 1.0) (- n 2))) (exc-calc--moment numbers 3 t))))

(exc-calc-define skew.p (&rest (numbers values))
  "The skewness of NUMBERS, all there are."
  (let* ((numbers (exc-calc-numbers numbers)) (n (length numbers)))
    (when (< n 1) (exc-calc-raise "#DIV/0!"))
    (/ (exc-calc--moment numbers 3 nil) n)))

(defun exc-calc--positive-numbers (values)
  "Return the numbers of VALUES; #NUM! when there are none or one is not
above 0."
  (let ((numbers (exc-calc-numbers values)))
    (unless (and numbers (cl-every (lambda (x) (> x 0)) numbers))
      (exc-calc-raise "#NUM!"))
    numbers))

(exc-calc-define geomean (&rest (numbers values))
  "The geometric mean of NUMBERS."
  (let ((numbers (exc-calc--positive-numbers numbers)))
    (exc-calc--tidy-number
     (exp (/ (apply #'+ (mapcar #'log numbers)) (length numbers))))))

(exc-calc-define harmean (&rest (numbers values))
  "The harmonic mean of NUMBERS."
  (let ((numbers (exc-calc--positive-numbers numbers)))
    (exc-calc--tidy-number
     (/ (length numbers) (apply #'+ (mapcar (lambda (x) (/ 1.0 x)) numbers))))))

(exc-calc-define trimmean ((numbers values) (percent number))
  "The mean of NUMBERS without the PERCENT of them at the two ends."
  (let* ((sorted (sort (exc-calc-numbers (list numbers)) #'<))
         (n (length sorted)))
    (unless (and (> n 0) (<= 0 percent) (< percent 1)) (exc-calc-raise "#NUM!"))
    (let ((cut (floor (* n percent) 2)))
      (exc-calc--mean (cl-subseq sorted cut (- n cut))))))

(exc-calc-define devsq (&rest (numbers values))
  "The sum of the squares of NUMBERS from their mean."
  (let ((numbers (exc-calc-numbers numbers)))
    (unless numbers (exc-calc-raise "#NUM!"))
    (exc-calc--tidy-number (exc-calc--squares numbers))))

(exc-calc-define avedev (&rest (numbers values))
  "The mean of the distances of NUMBERS from their mean."
  (let ((numbers (exc-calc-numbers numbers)))
    (unless numbers (exc-calc-raise "#NUM!"))
    (let ((mean (/ (float (apply #'+ numbers)) (length numbers))))
      (exc-calc--mean (mapcar (lambda (x) (abs (- x mean))) numbers)))))

(exc-calc-define fisher ((x number))
  "The Fisher transformation of X."
  (exc-calc--require (< -1 x 1))
  (* 0.5 (log (/ (+ 1.0 x) (- 1.0 x)))))

(exc-calc-define fisherinv ((y number))
  "The X whose Fisher transformation is Y."
  (if (> (abs y) 300) (if (> y 0) 1.0 -1.0)
    ;; As Excel works it out, which a Y near 0 shows
    (let ((e (exp (* 2.0 y)))) (/ (- e 1) (+ e 1)))))

(exc-calc-define prob ((xs array) (chances array) (low number) &optional (high number))
  "The chance of a value of XS from LOW to HIGH, or of LOW, where each X
comes with the chance beside it in CHANCES."
  (let* ((pairs (exc-calc--paired xs chances))
         (total (apply #'+ (mapcar #'cdr pairs))))
    ;; Excel shares the chances out of their sum, so none of them at all
    ;; is #DIV/0! rather than the #NUM! of chances that come to no 1.
    (when (zerop total) (exc-calc-raise "#DIV/0!"))
    (unless (and (cl-every (lambda (p) (< 0 (cdr p) 1.0000001)) pairs)
                 (< (abs (- total 1)) 1e-7))
      (exc-calc-raise "#NUM!"))
    (let ((high (or high low)))
      (exc-calc--tidy-number
       (cl-loop for (x . chance) in pairs
                when (<= low x high) sum chance)))))

;;;----------------------------------------------------------------------
;;; Regression: LINEST, LOGEST, TREND and GROWTH
;;;----------------------------------------------------------------------

(defun exc-calc--regression-data (known-y known-x)
  "Return the data of a regression of KNOWN-Y on KNOWN-X.
It is a list of the Ys, a vector; the variables, a list of vectors as
long; and how they lie in KNOWN-X: `one' for a single variable,
`columns' when each is a column, `rows' when each is a row.  Without
KNOWN-X the variable is 1, 2, 3 and on.  Sizes that do not fit are
#REF!; a value that is not a number #VALUE!."
  (let* ((rows (exc-calc-array-rows known-y)) (cols (exc-calc-array-cols known-y))
         (n (* rows cols))
         (x-rows (and known-x (exc-calc-array-rows known-x)))
         (x-cols (and known-x (exc-calc-array-cols known-x)))
         (lie (cond ((or (null known-x) (= (* x-rows x-cols) n)) 'one)
                    ((and (= cols 1) (= x-rows n)) 'columns)
                    ((and (= rows 1) (= x-cols n)) 'rows)
                    (t (exc-calc-raise "#REF!"))))
         (ys (apply #'vconcat (append (exc-calc--regression-matrix known-y) nil))))
    (list ys
          (pcase lie
            ('one (list (if known-x
                            (apply #'vconcat (append (exc-calc--regression-matrix known-x) nil))
                          (vconcat (number-sequence 1.0 n)))))
            ('columns (let ((m (exc-calc--regression-matrix known-x)))
                        (cl-loop for j below x-cols
                                 collect (cl-map 'vector (lambda (row) (aref row j)) m))))
            ('rows (append (exc-calc--regression-matrix known-x) nil)))
          lie)))

(defun exc-calc--regression-matrix (array)
  "Return ARRAY as a matrix for a regression: as `exc-calc--matrix'
does, but an error in it is #VALUE!, as Excel has it."
  (when (cl-some #'exc-calc-err-p (exc-calc-array-values array))
    (exc-calc-raise "#VALUE!"))
  (exc-calc--matrix array))

(defun exc-calc--dot (a b)
  "Return the dot product of vectors A and B."
  (cl-loop for x across a for y across b sum (* x y)))

(defun exc-calc--fit (ys vars const)
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
             for size = (exc-calc--dot v v)
             for rest = (copy-sequence v)
             do (dolist (q basis)
                  (let ((c (/ (exc-calc--dot rest q) (exc-calc--dot q q))))
                    (dotimes (i n) (aset rest i (- (aref rest i) (* c (aref q i)))))))
             when (> (exc-calc--dot rest rest) (* 1e-13 size))
             do (push rest basis) (push j kept))
    (setq kept (nreverse kept))
    (let* ((m (length kept))
           (xs (mapcar (lambda (j) (nth j centered)) kept))
           (yc (cl-map 'vector (lambda (y) (- y y-mean)) ys))
           (inverse (and (> m 0)
                         (exc-calc--eliminate
                          (apply #'vector
                                 (mapcar (lambda (a)
                                           (apply #'vector
                                                  (mapcar (lambda (b) (exc-calc--dot a b)) xs)))
                                         xs))
                          t)))
           (products (mapcar (lambda (x) (exc-calc--dot x yc)) xs))
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
           (sey (if (> df 0) (sqrt (/ ssresid df)) (exc-calc-err "#NUM!")))
           (error-of (lambda (variance)
                       (if (numberp sey) (* sey (sqrt variance)) sey))))
      (list :slopes slopes :intercept intercept
            :errors (cl-loop for j below (length vars)
                             collect (let ((at (cl-position j kept)))
                                       (if at (funcall error-of (aref (aref inverse at) at))
                                         0.0)))
            :intercept-error
            (if (not const) (exc-calc-err "#N/A")
              (funcall error-of
                       (+ (/ 1.0 n)
                          (cl-loop for a in kept for i from 0
                                   sum (cl-loop for b in kept for j from 0
                                                sum (* (nth a x-means) (nth b x-means)
                                                       (aref (aref inverse i) j)))))))
            :r2 (if (zerop (+ ssreg ssresid)) (exc-calc-err "#NUM!")
                  (/ ssreg (+ ssreg ssresid)))
            :sey sey
            :f (if (or (= m 0) (<= df 0) (zerop ssresid)) (exc-calc-err "#NUM!")
                 (/ (/ ssreg m) (/ ssresid df)))
            :df (if (< df 0) (exc-calc-err "#NUM!") df)
            :ssreg ssreg :ssresid ssresid))))

(defun exc-calc--flag (value default)
  "Return VALUE as true or false, DEFAULT when it is left out."
  (cond ((or (null value) (eq value :blank)) default)
        ((exc-calc-err-p value) (exc-calc-raise "#VALUE!"))
        (t (exc-calc-truth value))))

(defun exc-calc--estimate (known-y known-x const stats logs)
  "Return what LINEST gives for KNOWN-Y, KNOWN-X, CONST and STATS, or,
with LOGS, what LOGEST gives."
  (pcase-let* ((`(,ys ,vars ,_) (exc-calc--regression-data known-y known-x))
               (ys (if logs (exc-calc--logs ys) ys))
               (fit (exc-calc--fit ys vars (exc-calc--flag const t)))
               (shown (lambda (x) (if (and logs (numberp x)) (exp x) x)))
               (first (mapcar shown (append (reverse (plist-get fit :slopes))
                                            (list (plist-get fit :intercept)))))
               (width (length first))
               (pad (lambda (&rest values)
                      (append values (make-list (- width (length values))
                                                (exc-calc-err "#N/A"))))))
    (exc-calc-array-from-rows
     (if (not (exc-calc--flag stats nil)) (list first)
       (list first
             (append (reverse (plist-get fit :errors))
                     (list (plist-get fit :intercept-error)))
             (funcall pad (plist-get fit :r2) (plist-get fit :sey))
             (funcall pad (plist-get fit :f) (plist-get fit :df))
             (funcall pad (plist-get fit :ssreg) (plist-get fit :ssresid)))))))

(defun exc-calc--logs (ys)
  "Return the logs of YS; #NUM! when one is not above 0."
  (cl-map 'vector (lambda (y) (if (> y 0) (log y) (exc-calc-raise "#NUM!"))) ys))

(exc-calc-define linest ((known-y array) &optional (known-x array) (const any) (stats any))
  "The least-squares line of KNOWN-Y on KNOWN-X: its slopes, last
variable first, then its intercept.  Through 0 when CONST is false;
with STATS, four more rows of statistics."
  :errors t
  (exc-calc--estimate known-y known-x const stats nil))

(exc-calc-define logest ((known-y array) &optional (known-x array) (const any) (stats any))
  "The exponential curve fit to KNOWN-Y on KNOWN-X: its bases, last
variable first, then its constant.  CONST and STATS are as for LINEST."
  :errors t
  (exc-calc--estimate known-y known-x const stats t))

(defun exc-calc--predict (known-y known-x new-x const logs)
  "Return the Y at each NEW-X of the fit of KNOWN-Y on KNOWN-X, as TREND
gives, or as GROWTH does with LOGS."
  (pcase-let* ((`(,ys ,vars ,lie) (exc-calc--regression-data known-y known-x))
               (ys (if logs (exc-calc--logs ys) ys))
               (fit (exc-calc--fit ys vars (exc-calc--flag const t)))
               (slopes (plist-get fit :slopes))
               (intercept (plist-get fit :intercept))
               (new-x (or new-x known-x
                          (exc-calc-array (exc-calc-array-rows known-y)
                                           (exc-calc-array-cols known-y)
                                           (let ((cols (exc-calc-array-cols known-y)))
                                             (lambda (row col) (+ 1 col (* row cols)))))))
               (matrix (exc-calc--regression-matrix new-x))
               (k (length vars))
               (at (lambda (xs)
                     (let ((y (+ intercept (cl-loop for b in slopes for x in xs
                                                    sum (* b x)))))
                       (exc-calc--tidy-number (if logs (exp y) y))))))
    (pcase lie
      ('one (exc-calc-array (exc-calc-array-rows new-x) (exc-calc-array-cols new-x)
                             (lambda (row col)
                               (funcall at (list (aref (aref matrix row) col))))))
      ('columns
       (unless (= (exc-calc-array-cols new-x) k) (exc-calc-raise "#REF!"))
       (exc-calc-array (length matrix) 1
                        (lambda (row _) (funcall at (append (aref matrix row) nil)))))
      ('rows
       (unless (= (exc-calc-array-rows new-x) k) (exc-calc-raise "#REF!"))
       (exc-calc-array 1 (exc-calc-array-cols new-x)
                        (lambda (_ col)
                          (funcall at (cl-map 'list (lambda (row) (aref row col))
                                              matrix))))))))

(exc-calc-define trend ((known-y array) &optional (known-x array) (new-x array) (const any))
  "The Y at each NEW-X of the least-squares line of KNOWN-Y on KNOWN-X,
through 0 when CONST is false."
  :errors t
  (exc-calc--predict known-y known-x new-x const nil))

(exc-calc-define growth ((known-y array) &optional (known-x array) (new-x array) (const any))
  "The Y at each NEW-X of the exponential curve fit to KNOWN-Y on KNOWN-X,
through 1 when CONST is false."
  :errors t
  (exc-calc--predict known-y known-x new-x const t))

;;;----------------------------------------------------------------------
;;; Largest, smallest and in between
;;;----------------------------------------------------------------------

(exc-calc-define min (&rest (numbers values))
  "The smallest of NUMBERS; 0 when there are none.
An error among them is that error, and text that is no number is
#VALUE!, whichever comes first, as MAX reads them."
  :errors t
  (let ((numbers (exc-calc-numbers numbers)))
    (if numbers (apply #'min numbers) 0)))

(exc-calc-define max (&rest (numbers values))
  "The largest of NUMBERS; 0 when there are none.
An error among them is that error, and text that is no number is
#VALUE!, whichever comes first as Excel reads them: MAX(\"abc\",NA())
is #VALUE! and MAX(NA(),\"abc\") is #N/A."
  :errors t
  (let ((numbers (exc-calc-numbers numbers)))
    (if numbers (apply #'max numbers) 0)))

(exc-calc-define mina (&rest (values values))
  "The smallest of VALUES, where text counts as 0 and TRUE as 1."
  (let ((numbers (exc-calc-numbers values 'a)))
    (if numbers (apply #'min numbers) 0)))

(exc-calc-define maxa (&rest (values values))
  "The largest of VALUES, where text counts as 0 and TRUE as 1."
  (let ((numbers (exc-calc-numbers values 'a)))
    (if numbers (apply #'max numbers) 0)))

(exc-calc-define minifs ((range values) &rest (criteria values))
  "The smallest cell of RANGE beside cells meeting every criterion."
  (exc-calc--each-criteria
   criteria
   (lambda (pairs)
     (let ((numbers (exc-calc--numbers-at
                     range (exc-calc--matching-cells pairs range))))
       (if numbers (apply #'min numbers) 0)))))

(exc-calc-define maxifs ((range values) &rest (criteria values))
  "The largest cell of RANGE beside cells meeting every criterion."
  (exc-calc--each-criteria
   criteria
   (lambda (pairs)
     (let ((numbers (exc-calc--numbers-at
                     range (exc-calc--matching-cells pairs range))))
       (if numbers (apply #'max numbers) 0)))))

(defun exc-calc--sorted-numbers (argument)
  "Return the numbers of ARGUMENT, a `values' argument, sorted up."
  (sort (exc-calc-numbers (list argument)) #'<))

(defun exc-calc--median (numbers)
  "Return the median of NUMBERS; #NUM! when there are none."
  (unless numbers (exc-calc-raise "#NUM!"))
  (let* ((sorted (vconcat (sort (copy-sequence numbers) #'<)))
         (n (length sorted)))
    (if (cl-oddp n) (aref sorted (/ n 2))
      (exc-calc--tidy-number (/ (+ (aref sorted (1- (/ n 2))) (aref sorted (/ n 2)))
                                 2.0)))))

(exc-calc-define median (&rest (numbers values))
  "The middle of NUMBERS, or the mean of the two middle ones."
  (exc-calc--median (exc-calc-numbers numbers)))

(defun exc-calc--modes (numbers)
  "Return the numbers coming most often in NUMBERS, more than once.
They are in the order they first come; #N/A when none repeats."
  (let ((counts '()) (best 1))
    (dolist (n numbers)
      (let ((cell (assoc n counts #'=)))
        (if cell (setcdr cell (1+ (cdr cell))) (push (cons n 1) counts))))
    (setq counts (nreverse counts))
    (dolist (cell counts) (setq best (max best (cdr cell))))
    (when (= best 1) (exc-calc-raise "#N/A"))
    (mapcar #'car (cl-remove-if-not (lambda (cell) (= (cdr cell) best)) counts))))

(defun exc-calc--mode-numbers (arguments)
  "Return the numbers of ARGUMENTS as MODE takes them.
Excel takes each argument as an array there, so one written directly,
or a reference to one cell, is #VALUE! unless it is a number."
  (dolist (argument arguments)
    (let ((value (cond ((and (exc-calc-ref-p argument)
                             (= (exc-calc-ref-rows argument) 1)
                             (= (exc-calc-ref-cols argument) 1))
                        (exc-calc--deref argument))
                       ((not (or (exc-calc-ref-p argument)
                                 (exc-calc-array-p argument)
                                 (eq (car-safe argument) 'union)))
                        argument)
                       (t 0))))
      (cond ((exc-calc-err-p value) (exc-calc-raise (exc-calc-err-code value)))
            ((not (numberp value)) (exc-calc-raise "#VALUE!")))))
  (exc-calc-numbers arguments))

(exc-calc-define mode.sngl (&rest (numbers values))
  "The number coming most often in NUMBERS, the first when several do."
  (car (exc-calc--modes (exc-calc--mode-numbers numbers))))

(exc-calc-alias mode mode.sngl)

(exc-calc-define mode.mult (&rest (numbers values))
  "The numbers coming most often in NUMBERS, as a column."
  (exc-calc-array-from-list (exc-calc--modes (exc-calc--mode-numbers numbers))))

(defun exc-calc--kth (numbers k largest)
  "Return the Kth LARGEST, or smallest, of NUMBERS, counting from 1.
A fraction of K counts up, but K below 1 is #NUM!, as in Excel."
  (when (< k 1) (exc-calc-raise "#NUM!"))
  (let ((k (if (floatp k) (truncate (fceiling (exc-calc--snap k))) k)))
    (unless (and numbers (<= 1 k (length numbers))) (exc-calc-raise "#NUM!"))
    (nth (1- k) (sort (copy-sequence numbers) (if largest #'> #'<)))))

(exc-calc-define large ((numbers values) (k any))
  "The Kth largest of NUMBERS."
  (let ((numbers (exc-calc-numbers (list numbers))))
    (if (exc-calc-array-p k)
        (exc-calc-array-map (lambda (k) (exc-calc--catching
                                           (exc-calc--kth numbers (exc-calc-num k) t)))
                             k)
      (exc-calc--kth numbers (exc-calc-num k) t))))

(exc-calc-define small ((numbers values) (k any))
  "The Kth smallest of NUMBERS."
  (let ((numbers (exc-calc-numbers (list numbers))))
    (if (exc-calc-array-p k)
        (exc-calc-array-map (lambda (k) (exc-calc--catching
                                           (exc-calc--kth numbers (exc-calc-num k) nil)))
                             k)
      (exc-calc--kth numbers (exc-calc-num k) nil))))

(defun exc-calc--rank (number numbers ascending average)
  "Return the rank of NUMBER among NUMBERS, the largest first.
ASCENDING ranks the smallest first; AVERAGE gives ties their mean rank."
  (unless (cl-member number numbers :test #'=) (exc-calc-raise "#N/A"))
  (let ((before (cl-count-if (lambda (n) (if ascending (< n number) (> n number)))
                             numbers))
        (ties (cl-count number numbers :test #'=)))
    (if average (exc-calc--tidy-number (+ before (/ (1+ ties) 2.0)))
      (1+ before))))

(exc-calc-define rank.eq ((number number) (numbers areas) &optional (order number 0))
  "The rank of NUMBER among NUMBERS, the largest first, or the smallest
when ORDER is not 0.  Equal numbers share the best rank."
  (exc-calc--rank number (exc-calc-numbers (list numbers)) (/= order 0) nil))

(exc-calc-alias rank rank.eq)

(exc-calc-define rank.avg ((number number) (numbers areas) &optional (order number 0))
  "The rank of NUMBER among NUMBERS, equal numbers sharing their mean rank."
  (exc-calc--rank number (exc-calc-numbers (list numbers)) (/= order 0) t))

(defun exc-calc--percentile (numbers k exclusive)
  "Return the K percentile of NUMBERS, K from 0 to 1.
EXCLUSIVE leaves out the first and last as PERCENTILE.EXC does."
  (let* ((sorted (vconcat (sort (copy-sequence numbers) #'<)))
         (n (length sorted)))
    (when (or (= n 0) (< k 0) (> k 1)) (exc-calc-raise "#NUM!"))
    (let ((position (if exclusive (1- (* k (1+ n))) (* k (1- n)))))
      (when (or (< position 0) (> position (1- n))) (exc-calc-raise "#NUM!"))
      (let* ((low (floor (exc-calc--snap position)))
             (fraction (- position low)))
        (exc-calc--tidy-number
         (exc-calc--snap
          (if (or (zerop fraction) (>= low (1- n))) (float (aref sorted low))
            (+ (aref sorted low) (* fraction (- (aref sorted (1+ low))
                                                (aref sorted low)))))))))))

(exc-calc-define percentile.inc ((numbers values) (k number))
  "The K percentile of NUMBERS, K from 0 to 1."
  (exc-calc--percentile (exc-calc-numbers (list numbers)) k nil))

(exc-calc-alias percentile percentile.inc)

(exc-calc-define percentile.exc ((numbers values) (k number))
  "The K percentile of NUMBERS, K between 0 and 1, the ends left out."
  (exc-calc--percentile (exc-calc-numbers (list numbers)) k t))

(exc-calc-define quartile.inc ((numbers values) (quart number))
  "The QUART quartile of NUMBERS, 0 their smallest to 4 their largest.
A fraction is cut off, but one below 0 is #NUM!, as in Excel.  An error
in NUMBERS comes before QUART is checked."
  (let ((numbers (exc-calc-numbers (list numbers))))
    (unless (<= 0 quart) (exc-calc-raise "#NUM!"))
    (setq quart (truncate quart))
    (unless (<= quart 4) (exc-calc-raise "#NUM!"))
    (exc-calc--percentile numbers (/ quart 4.0) nil)))

(exc-calc-alias quartile quartile.inc)

(exc-calc-define quartile.exc ((numbers values) (quart integer))
  "The QUART quartile of NUMBERS, 1 to 3, the ends left out.
An error in NUMBERS comes before QUART is checked."
  (let ((numbers (exc-calc-numbers (list numbers))))
    (unless (<= 1 quart 3) (exc-calc-raise "#NUM!"))
    (exc-calc--percentile numbers (/ quart 4.0) t)))

(defun exc-calc--percent-rank (numbers number digits exclusive)
  "Return where NUMBER stands among NUMBERS, to DIGITS places.
From 0 to 1, or with EXCLUSIVE strictly between them."
  (let* ((sorted (vconcat (sort (exc-calc-numbers (list numbers)) #'<)))
         (n (length sorted)))
    (when (or (= n 0) (< digits 1)) (exc-calc-raise "#NUM!"))
    (when (or (< number (aref sorted 0)) (> number (aref sorted (1- n))))
      (exc-calc-raise "#N/A"))
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
      (exc-calc--round-to rank digits 'down))))

(exc-calc-define percentrank.inc ((numbers values) (number number)
                                   &optional (digits integer 3))
  "Where NUMBER stands among NUMBERS, from 0 to 1, to DIGITS places."
  (exc-calc--percent-rank numbers number digits nil))

(exc-calc-define percentrank.exc ((numbers values) (number number)
                                   &optional (digits integer 3))
  "Where NUMBER stands among NUMBERS, between 0 and 1, to DIGITS places."
  (exc-calc--percent-rank numbers number digits t))

(exc-calc-alias percentrank percentrank.inc)

;;;----------------------------------------------------------------------
;;; SUBTOTAL and AGGREGATE
;;;----------------------------------------------------------------------

(defun exc-calc--subtotal-cell-p (model row col)
  "Return non-nil when the cell at ROW and COL of MODEL is a SUBTOTAL or
AGGREGATE, which these functions leave out to not count twice."
  (let ((raw (exc-cell-raw-input
              (aref (exc-data-model-cells model)
                    (+ (* row (exc-data-model-cols model)) col)))))
    (and (exc-calc--formula-p raw)
         (string-match-p "\\b\\(SUBTOTAL\\|AGGREGATE\\)(" (upcase raw)))))

(defun exc-calc--shown-values (arguments skip-hidden skip-errors
                                           &optional keep-nested)
  "Return the values of ARGUMENTS as SUBTOTAL and AGGREGATE see them.
Cells holding a SUBTOTAL or AGGREGATE are left out unless KEEP-NESTED,
and with SKIP-HIDDEN the hidden rows, or with `filtered' only the ones
a filter left out; SKIP-ERRORS leaves error values
out, and `keep' keeps them, for COUNT and COUNTA to count.  Otherwise
an error value signals."
  (let (values)
    (dolist (argument arguments)
      (if (exc-calc-ref-p argument)
          (let* ((model (exc-calc-ref-model argument))
                 (hidden (pcase skip-hidden
                           ('nil nil)
                           ('filtered (exc-model-filtered-rows model))
                           (_ (car (exc-data-model-hidden model)))))
                 (top (exc-calc-ref-top argument))
                 (left (exc-calc-ref-left argument))
                 (cols (max 0 (min (exc-calc-ref-cols argument)
                                   (- (exc-data-model-cols model) left)))))
            (dotimes (row (exc-calc-ref-filled-rows argument))
              (unless (memq (+ top row) hidden)
                (dotimes (col cols)
                  (unless (and (not keep-nested)
                               (exc-calc--subtotal-cell-p model (+ top row) (+ left col)))
                    (push (exc-calc--cell model (+ top row) (+ left col)) values))))))
        (exc-calc-each-value argument (lambda (value _) (push value values)))))
    (setq values (nreverse values))
    (pcase skip-errors
      ('keep values)
      ('nil
       (let ((error (cl-find-if #'exc-calc-err-p values)))
         (when error (exc-calc-raise (exc-calc-err-code error))))
       values)
      (_ (cl-remove-if #'exc-calc-err-p values)))))

(defun exc-calc--counting-p (number)
  "Return non-nil when SUBTOTAL's function NUMBER is COUNT or COUNTA.
They count error values rather than give them, as COUNT and COUNTA do."
  (memq (% number 100) '(2 3)))

(defun exc-calc--aggregate-by (number values &optional k)
  "Return function NUMBER of SUBTOTAL or AGGREGATE over VALUES.
K is the extra argument of AGGREGATE's functions 14 to 19."
  (let ((numbers (cl-remove-if-not #'numberp values)))
    (pcase number
      (1 (exc-calc--mean numbers))
      (2 (length numbers))
      (3 (cl-count-if-not (lambda (v) (eq v :blank)) values))
      (4 (if numbers (apply #'max numbers) 0))
      (5 (if numbers (apply #'min numbers) 0))
      (6 (if numbers (exc-calc--finite (exc-calc--snap (apply #'* numbers))) 0))
      (7 (exc-calc--finite (sqrt (exc-calc--variance numbers t))))
      (8 (exc-calc--finite (sqrt (exc-calc--variance numbers nil))))
      (9 (exc-calc--add numbers))
      (10 (exc-calc--finite (exc-calc--variance numbers t)))
      (11 (exc-calc--finite (exc-calc--variance numbers nil)))
      (12 (exc-calc--median numbers))
      (13 (car (exc-calc--modes numbers)))
      (14 (exc-calc--kth numbers (exc-calc-num k) t))
      (15 (exc-calc--kth numbers (exc-calc-num k) nil))
      (16 (exc-calc--percentile numbers (exc-calc-num k) nil))
      (17 (exc-calc--percentile numbers (/ (exc-calc-int k) 4.0) nil))
      (18 (exc-calc--percentile numbers (exc-calc-num k) t))
      (19 (exc-calc--percentile numbers (/ (exc-calc-int k) 4.0) t))
      (_ (exc-calc-raise "#VALUE!")))))

(exc-calc-define subtotal ((function integer) &rest (ranges values))
  "FUNCTION of the values of RANGES, leaving out other subtotals.
FUNCTION 1 to 11 is AVERAGE, COUNT, COUNTA, MAX, MIN, PRODUCT, STDEV,
STDEVP, SUM, VAR and VARP; 101 to 111 is the same leaving out hidden
rows.  Either leaves out the rows a filter left out."
  (unless (or (<= 1 function 11) (<= 101 function 111))
    (exc-calc-raise "#VALUE!"))
  (exc-calc--aggregate-by (% function 100)
                           (exc-calc--shown-values
                            ranges (if (> function 100) t 'filtered)
                            (and (exc-calc--counting-p function) 'keep))))

(exc-calc-define aggregate ((function integer) (options integer) (range values)
                             &rest (more values))
  "FUNCTION of the values of RANGE, leaving out what OPTIONS says.
FUNCTION 1 to 13 is as SUBTOTAL's, then MEDIAN and MODE.SNGL; 14 to 19
are LARGE, SMALL, PERCENTILE.INC, QUARTILE.INC, PERCENTILE.EXC and
QUARTILE.EXC, which take K as the next argument.  OPTIONS 0 to 7: odd
ones leave out hidden rows, 2 and 3 and 6 and 7 error values, 0 to 3
other subtotals."
  (unless (and (<= 1 function 19) (<= 0 options 7)) (exc-calc-raise "#VALUE!"))
  (let ((skip-hidden (memq options '(1 3 5 7)))
        (skip-errors (or (memq options '(2 3 6 7))
                         (and (exc-calc--counting-p function) 'keep)))
        (keep-nested (> options 3)))
    (if (>= function 14)
        (exc-calc--aggregate-by
         function (exc-calc--shown-values (list range) skip-hidden skip-errors
                                           keep-nested)
         (exc-calc--first (or (car more) (exc-calc-raise "#VALUE!"))))
      (exc-calc--aggregate-by
       function (exc-calc--shown-values (cons range more) skip-hidden skip-errors
                                         keep-nested)))))

;;;----------------------------------------------------------------------
;;; Database functions
;;;----------------------------------------------------------------------

(defun exc-calc--database-names (table)
  "Return the names atop the columns of TABLE, as text; nil for an error."
  (cl-loop for col below (cdr (exc-calc--table-size table))
           for name = (exc-calc--table-at table 0 col)
           collect (unless (exc-calc-err-p name) (exc-calc--text name))))

(defun exc-calc--same-name-p (name other)
  "Return non-nil when NAME and OTHER, column names or nil, are one."
  (and name other (string-equal-ignore-case name other)))

(defun exc-calc--database-criterion (criterion)
  "Return a function telling whether a value meets CRITERION.
Text with no operator before it is met by text that starts with it."
  (exc-calc-criterion
   (if (and (stringp criterion) (not (string-empty-p criterion))
            (not (string-match-p "\\`[<>=]" criterion))
            (not (exc-calc-text-number criterion)))
       (concat criterion "*")
     criterion)))

(defun exc-calc--database-rows (database criteria)
  "Return the rows of DATABASE, from 1, that meet the CRITERIA table.
Both are ranges or arrays whose first row names the columns, without
regard to case.  A row of CRITERIA is met when each criterion in it is,
none under a name DATABASE lacks; any row will do.  A DATABASE of no
more than its names is #VALUE!."
  (pcase-let ((`(,rows . ,_) (exc-calc--table-size database))
              (`(,c-rows . ,c-cols) (exc-calc--table-size criteria)))
    (when (< rows 2) (exc-calc-raise "#VALUE!"))
    (let ((names (exc-calc--database-names database))
          (c-names (exc-calc--database-names criteria))
          (tests '()))
      (cl-loop for c-row from 1 below c-rows
               do (push (cl-loop for c-col below c-cols
                                 for criterion = (exc-calc--table-at criteria c-row c-col)
                                 unless (exc-calc-blank-p criterion)
                                 collect (cons (cl-position (nth c-col c-names) names
                                                            :test #'exc-calc--same-name-p)
                                               (exc-calc--database-criterion criterion)))
                        tests))
      (cl-loop for row from 1 below rows
               when (cl-some (lambda (all)
                               (cl-every (lambda (test)
                                           (and (car test)
                                                (funcall (cdr test)
                                                         (exc-calc--table-at database row
                                                                              (car test)))))
                                         all))
                             tests)
               collect row))))

(defun exc-calc--database-field (database field)
  "Return the column of DATABASE that FIELD, a name or number from 1, is.
FIELD is a `values' argument; one left out is nil.  Several cells, a
blank one, or a name or number DATABASE has no column for is #VALUE!."
  (unless (eq field :blank)
    (let ((value (exc-calc--deref field))
          (cols (cdr (exc-calc--table-size database))))
      (cond
       ((exc-calc-err-p value) (exc-calc-raise (exc-calc-err-code value)))
       ((or (exc-calc-array-p value) (exc-calc-blank-p value))
        (exc-calc-raise "#VALUE!"))
       ((stringp value)
        (or (cl-position value (exc-calc--database-names database)
                         :test #'exc-calc--same-name-p)
            (exc-calc-raise "#VALUE!")))
       (t (let ((col (1- (truncate (exc-calc-num value)))))
            (if (< -1 col cols) col (exc-calc-raise "#VALUE!"))))))))

(exc-calc-define dcount ((database values) (field values) (criteria values))
  "How many rows of DATABASE meeting CRITERIA hold a number in FIELD.
Without FIELD every row meeting CRITERIA that holds a number counts."
  (let ((rows (exc-calc--database-rows database criteria))
        (col (exc-calc--database-field database field)))
    (if col
        (cl-count-if (lambda (row) (numberp (exc-calc--table-at database row col)))
                     rows)
      (length rows))))

;; DCOUNTA and DGET read every value of the field, the others its numbers.

(defun exc-calc--database-values (database field criteria)
  "Return the values in FIELD of the rows of DATABASE meeting CRITERIA."
  (let ((rows (exc-calc--database-rows database criteria))
        (col (or (exc-calc--database-field database field)
                 (exc-calc-raise "#VALUE!"))))
    (mapcar (lambda (row) (exc-calc--table-at database row col)) rows)))

(defmacro exc-calc--define-database (name doc reduce)
  "Define NAME, a database function giving REDUCE of the numbers it reads.
DOC is its docstring."
  `(exc-calc-define ,name ((database values) (field values) (criteria values))
     ,doc
     (funcall ,reduce (cl-remove-if-not
                       #'numberp
                       (exc-calc--database-values database field criteria)))))

(exc-calc--define-database dsum "The sum of FIELD in the rows meeting CRITERIA."
                            (lambda (ns) (exc-calc--tidy-number (apply #'+ 0 ns))))
(exc-calc--define-database daverage "The mean of FIELD in the rows meeting CRITERIA."
                            #'exc-calc--mean)
(exc-calc--define-database dmin "The least of FIELD in the rows meeting CRITERIA."
                            (lambda (ns) (if ns (apply #'min ns) 0)))
(exc-calc--define-database dmax "The greatest of FIELD in the rows meeting CRITERIA."
                            (lambda (ns) (if ns (apply #'max ns) 0)))
(exc-calc--define-database dproduct "The product of FIELD in the rows meeting CRITERIA."
                            (lambda (ns) (if ns (exc-calc--finite (apply #'* ns)) 0)))
(exc-calc--define-database dvar "The variance of FIELD in the rows meeting CRITERIA, a sample."
                            (lambda (ns) (exc-calc--variance ns t)))
(exc-calc--define-database dvarp "The variance of FIELD in all the rows meeting CRITERIA."
                            (lambda (ns) (exc-calc--variance ns nil)))
(exc-calc--define-database dstdev "The standard deviation of FIELD in the rows meeting CRITERIA, a sample."
                            (lambda (ns) (sqrt (exc-calc--variance ns t))))
(exc-calc--define-database dstdevp "The standard deviation of FIELD in all the rows meeting CRITERIA."
                            (lambda (ns) (sqrt (exc-calc--variance ns nil))))

(exc-calc-define dcounta ((database values) (field values) (criteria values))
  "How many rows of DATABASE meeting CRITERIA hold a value in FIELD.
Without FIELD every row meeting CRITERIA counts."
  (if (eq field :blank)
      (length (exc-calc--database-rows database criteria))
    (cl-count-if-not #'exc-calc-blank-p
                     (exc-calc--database-values database field criteria))))

(exc-calc-define dget ((database values) (field values) (criteria values))
  "The one value in FIELD of the rows meeting CRITERIA.
#VALUE! when no row meets them, #NUM! when more than one does."
  (let ((values (exc-calc--database-values database field criteria)))
    (cond ((null values) (exc-calc-raise "#VALUE!"))
          ((cdr values) (exc-calc-raise "#NUM!"))
          (t (car values)))))

(provide 'exc-calc-stats)

;;; exc-calc-stats.el ends here
