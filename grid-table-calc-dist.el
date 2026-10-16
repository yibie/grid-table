;;; grid-table-calc-dist.el --- Probability distributions -*- lexical-binding: t -*-

;;; Commentary:

;; The distributions Excel's statistical functions use: normal and
;; log-normal, t, chi-squared, F, gamma, beta, exponential and Weibull,
;; binomial and its kin, and Poisson; and the tests built on them.
;;
;; They rest on a few special functions computed here: the log of the
;; gamma function (Lanczos), the regularized incomplete gamma and beta
;; functions (series and continued fractions), and the inverse of the
;; standard normal distribution (Acklam, refined by Halley's method).
;; Other inverses are found by bisection.

;;; Code:

(require 'grid-table-calc)
(require 'grid-table-calc-math)
(require 'grid-table-calc-stats)

;;;----------------------------------------------------------------------
;;; Special functions
;;;----------------------------------------------------------------------

(defconst grid-calc--lanczos
  [0.99999999999980993 676.5203681218851 -1259.1392167224028
   771.32342877765313 -176.61502916214059 12.507343278686905
   -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7]
  "Coefficients of the Lanczos approximation, g = 7.")

(defun grid-calc--log-gamma (x)
  "Return the log of the absolute value of the gamma function at X."
  (if (< x 0.5)
      ;; Reflection: gamma(x) gamma(1-x) = pi / sin(pi x)
      (- (log (/ float-pi (abs (sin (* float-pi x)))))
         (grid-calc--log-gamma (- 1 x)))
    (let* ((x (- x 1.0))
           (sum (aref grid-calc--lanczos 0))
           (tt (+ x 7.5)))
      (cl-loop for i from 1 below 9
               do (setq sum (+ sum (/ (aref grid-calc--lanczos i) (+ x i)))))
      (+ (* 0.5 (log (* 2 float-pi))) (* (+ x 0.5) (log tt)) (- tt) (log sum)))))

(defconst grid-calc--epsilon 1e-15 "When a series or fraction has converged.")

(defun grid-calc--gamma-series (a x)
  "Return P(A, X) by its series, for X below A + 1."
  (let ((sum (/ 1.0 a)) (term (/ 1.0 a)) (n a))
    (catch 'done
      (dotimes (_ 1000)
        (setq n (1+ n) term (* term (/ x n)) sum (+ sum term))
        (when (< (abs term) (* (abs sum) grid-calc--epsilon)) (throw 'done nil))))
    (* sum (exp (- (* a (log x)) x (grid-calc--log-gamma a))))))

(defun grid-calc--gamma-fraction (a x)
  "Return Q(A, X) by its continued fraction, for X from A + 1."
  (let* ((tiny 1e-300)
         (b (+ x 1.0 (- a))) (c (/ 1.0 tiny)) (d (/ 1.0 b)) (h d))
    (catch 'done
      (cl-loop for i from 1 to 1000
               do (let* ((an (* (- i) (- i a))))
                    (setq b (+ b 2.0)
                          d (+ (* an d) b))
                    (when (< (abs d) tiny) (setq d tiny))
                    (setq c (+ b (/ an c)))
                    (when (< (abs c) tiny) (setq c tiny))
                    (setq d (/ 1.0 d))
                    (let ((delta (* d c)))
                      (setq h (* h delta))
                      (when (< (abs (- delta 1.0)) grid-calc--epsilon)
                        (throw 'done nil))))))
    (* h (exp (- (* a (log x)) x (grid-calc--log-gamma a))))))

(defun grid-calc--gamma-p (a x)
  "Return the regularized lower incomplete gamma function P(A, X)."
  (cond ((<= x 0) 0.0)
        ((< x (+ a 1)) (grid-calc--gamma-series a x))
        (t (- 1.0 (grid-calc--gamma-fraction a x)))))

(defun grid-calc--gamma-q (a x)
  "Return the regularized upper incomplete gamma function Q(A, X)."
  (cond ((<= x 0) 1.0)
        ((< x (+ a 1)) (- 1.0 (grid-calc--gamma-series a x)))
        (t (grid-calc--gamma-fraction a x))))

(defun grid-calc--beta-fraction (a b x)
  "Return the continued fraction of the incomplete beta function."
  (let* ((tiny 1e-300)
         (qab (+ a b)) (qap (+ a 1.0)) (qam (- a 1.0))
         (c 1.0) (d (- 1.0 (/ (* qab x) qap))))
    (when (< (abs d) tiny) (setq d tiny))
    (setq d (/ 1.0 d))
    (let ((h d))
      (catch 'done
        (cl-loop for m from 1 to 1000
                 do (let* ((m2 (* 2 m))
                           (aa (/ (* m (- b m) x) (* (+ qam m2) (+ a m2)))))
                      (setq d (+ 1.0 (* aa d)))
                      (when (< (abs d) tiny) (setq d tiny))
                      (setq c (+ 1.0 (/ aa c)))
                      (when (< (abs c) tiny) (setq c tiny))
                      (setq d (/ 1.0 d) h (* h d c))
                      (setq aa (/ (- (* (+ a m) (+ qab m) x))
                                  (* (+ a m2) (+ qap m2))))
                      (setq d (+ 1.0 (* aa d)))
                      (when (< (abs d) tiny) (setq d tiny))
                      (setq c (+ 1.0 (/ aa c)))
                      (when (< (abs c) tiny) (setq c tiny))
                      (setq d (/ 1.0 d))
                      (let ((delta (* d c)))
                        (setq h (* h delta))
                        (when (< (abs (- delta 1.0)) grid-calc--epsilon)
                          (throw 'done nil))))))
      h)))

(defun grid-calc--beta-i (x a b)
  "Return the regularized incomplete beta function I_X(A, B)."
  (cond ((<= x 0) 0.0)
        ((>= x 1) 1.0)
        (t
         (let ((front (exp (+ (- (grid-calc--log-gamma (+ a b))
                                 (grid-calc--log-gamma a)
                                 (grid-calc--log-gamma b))
                              (* a (log x)) (* b (log (- 1 x)))))))
           (if (< x (/ (+ a 1.0) (+ a b 2.0)))
               (/ (* front (grid-calc--beta-fraction a b x)) a)
             (- 1.0 (/ (* front (grid-calc--beta-fraction b a (- 1 x))) b)))))))

(defun grid-calc--erf (x)
  "Return the error function at X."
  (if (< x 0) (- (grid-calc--gamma-p 0.5 (* x x)))
    (grid-calc--gamma-p 0.5 (* x x))))

(defun grid-calc--erfc (x)
  "Return the complementary error function at X, precise when it is small."
  (if (< x 0) (+ 1.0 (grid-calc--gamma-p 0.5 (* x x)))
    (grid-calc--gamma-q 0.5 (* x x))))

(defun grid-calc--normal-cdf (z)
  "Return the standard normal distribution at Z."
  (* 0.5 (grid-calc--erfc (/ (- z) (sqrt 2.0)))))

(defun grid-calc--normal-pdf (z)
  "Return the standard normal density at Z."
  (/ (exp (* -0.5 z z)) (sqrt (* 2 float-pi))))

(defun grid-calc--normal-inverse (p)
  "Return Z where the standard normal distribution is P."
  (unless (< 0 p 1) (grid-calc-raise "#NUM!"))
  (let* ((a [-3.969683028665376e+01 2.209460984245205e+02 -2.759285104469687e+02
             1.383577518672690e+02 -3.066479806614716e+01 2.506628277459239e+00])
         (b [-5.447609879822406e+01 1.615858368580409e+02 -1.556989798598866e+02
             6.680131188771972e+01 -1.328068155288572e+01])
         (c [-7.784894002430293e-03 -3.223964580411365e-01 -2.400758277161838e+00
             -2.549732539343734e+00 4.374664141464968e+00 2.938163982698783e+00])
         (d [7.784695709041462e-03 3.224671290700398e-01 2.445134137142996e+00
             3.754408661907416e+00])
         (low 0.02425)
         (z (cond
             ((< p low)
              (let ((q (sqrt (* -2 (log p)))))
                (/ (+ (* (+ (* (+ (* (+ (* (+ (* (aref c 0) q) (aref c 1)) q)
                                          (aref c 2)) q) (aref c 3)) q) (aref c 4)) q)
                      (aref c 5))
                   (+ (* (+ (* (+ (* (+ (* (aref d 0) q) (aref d 1)) q) (aref d 2)) q)
                            (aref d 3)) q) 1))))
             ((> p (- 1 low))
              (let ((q (sqrt (* -2 (log (- 1 p))))))
                (- (/ (+ (* (+ (* (+ (* (+ (* (+ (* (aref c 0) q) (aref c 1)) q)
                                             (aref c 2)) q) (aref c 3)) q) (aref c 4)) q)
                         (aref c 5))
                      (+ (* (+ (* (+ (* (+ (* (aref d 0) q) (aref d 1)) q) (aref d 2)) q)
                               (aref d 3)) q) 1)))))
             (t
              (let* ((q (- p 0.5)) (r (* q q)))
                (/ (* (+ (* (+ (* (+ (* (+ (* (+ (* (aref a 0) r) (aref a 1)) r)
                                             (aref a 2)) r) (aref a 3)) r) (aref a 4)) r)
                         (aref a 5))
                      q)
                   (+ (* (+ (* (+ (* (+ (* (+ (* (aref b 0) r) (aref b 1)) r)
                                          (aref b 2)) r) (aref b 3)) r) (aref b 4)) r)
                      1)))))))
    ;; Two steps of Halley's method take it to full precision.
    (dotimes (_ 2)
      (let* ((e (- (grid-calc--normal-cdf z) p))
             (u (* e (sqrt (* 2 float-pi)) (exp (/ (* z z) 2)))))
        (setq z (- z (/ u (+ 1 (/ (* z u) 2)))))))
    z))

(defun grid-calc--invert (cdf p low high)
  "Return X between LOW and HIGH where the rising CDF is P, by bisection.
HIGH is pushed up until CDF there passes P."
  (unless (<= 0 p 1) (grid-calc-raise "#NUM!"))
  (let ((tries 0))
    (while (and (< (funcall cdf high) p) (< tries 200))
      (setq low high high (* 2 high) tries (1+ tries))))
  (dotimes (_ 200)
    (let ((mid (/ (+ low high) 2.0)))
      (if (< (funcall cdf mid) p) (setq low mid) (setq high mid))))
  (/ (+ low high) 2.0))

;;;----------------------------------------------------------------------
;;; Checking arguments
;;;----------------------------------------------------------------------

(defmacro grid-calc--require (&rest conditions)
  "Signal #NUM! unless every one of CONDITIONS holds."
  `(unless (and ,@conditions) (grid-calc-raise "#NUM!")))

(defun grid-calc--probability (p)
  "Return P, which must be a probability from 0 to 1."
  (grid-calc--require (<= 0 p 1))
  p)

(defun grid-calc--log-choose (n k)
  "Return the log of N choose K."
  (- (grid-calc--log-gamma (+ n 1.0)) (grid-calc--log-gamma (+ k 1.0))
     (grid-calc--log-gamma (+ (- n k) 1.0))))

;;;----------------------------------------------------------------------
;;; Normal and log-normal
;;;----------------------------------------------------------------------

(grid-calc-define norm.dist ((x number) (mean number) (sd number) (cumulative bool))
  "The normal distribution of MEAN and SD at X, or its density."
  (grid-calc--require (> sd 0))
  (let ((z (/ (- x mean) (float sd))))
    (if cumulative (grid-calc--normal-cdf z) (/ (grid-calc--normal-pdf z) sd))))

(grid-calc-define normdist ((x number) (mean number) (sd number) (cumulative bool))
  "The normal distribution of MEAN and SD at X, or its density."
  (grid-calc--require (> sd 0))
  (let ((z (/ (- x mean) (float sd))))
    (if cumulative (grid-calc--normal-cdf z) (/ (grid-calc--normal-pdf z) sd))))

(grid-calc-define norm.s.dist ((z number) &optional (cumulative bool t))
  "The standard normal distribution at Z, or its density."
  (if cumulative (grid-calc--normal-cdf z) (grid-calc--normal-pdf z)))

(grid-calc-define normsdist ((z number))
  "The standard normal distribution at Z."
  (grid-calc--normal-cdf z))

(grid-calc-define norm.inv ((p number) (mean number) (sd number))
  "X where the normal distribution of MEAN and SD is P."
  (grid-calc--require (> sd 0))
  (+ mean (* sd (grid-calc--normal-inverse p))))

(grid-calc-alias norminv norm.inv)

(grid-calc-define norm.s.inv ((p number))
  "Z where the standard normal distribution is P."
  (grid-calc--normal-inverse p))

(grid-calc-alias normsinv norm.s.inv)

(grid-calc-define standardize ((x number) (mean number) (sd number))
  "X as standard deviations SD from MEAN."
  (grid-calc--require (> sd 0))
  (/ (- x mean) (float sd)))

(grid-calc-define lognorm.dist ((x number) (mean number) (sd number) (cumulative bool))
  "The log-normal distribution at X, whose log has MEAN and SD; or its density."
  (grid-calc--require (> x 0) (> sd 0))
  (let ((z (/ (- (log x) mean) (float sd))))
    (if cumulative (grid-calc--normal-cdf z)
      (/ (grid-calc--normal-pdf z) (* x sd)))))

(grid-calc-define lognorm.inv ((p number) (mean number) (sd number))
  "X where the log-normal distribution of MEAN and SD is P."
  (grid-calc--require (> sd 0))
  (exp (+ mean (* sd (grid-calc--normal-inverse p)))))

(grid-calc-define phi ((x number))
  "The standard normal density at X."
  (grid-calc--normal-pdf x))

(grid-calc-define gauss ((z number))
  "The chance a standard normal value is between 0 and Z."
  (- (grid-calc--normal-cdf z) 0.5))

(grid-calc-define confidence.norm ((alpha number) (sd number) (size integer))
  "Half the width of a confidence interval for a mean, by the normal
distribution: ALPHA the chance of being outside, SD and SIZE those of
the sample."
  (grid-calc--require (< 0 alpha 1) (> sd 0) (>= size 1))
  (/ (* (grid-calc--normal-inverse (- 1 (/ alpha 2.0))) sd) (sqrt size)))

(grid-calc-alias confidence confidence.norm)

(grid-calc-define z.test ((array array) (x number) &optional (sigma number))
  "The one-tailed chance of a mean above that of ARRAY, were it X.
SIGMA is the standard deviation, by default that of the sample."
  (let* ((numbers (cl-remove-if-not #'numberp (grid-calc-array-values array)))
         (n (length numbers)))
    (when (= n 0) (grid-calc-raise "#N/A"))
    (let ((sigma (or sigma (sqrt (grid-calc--variance numbers t))))
          (mean (/ (float (apply #'+ numbers)) n)))
      (when (zerop sigma) (grid-calc-raise "#DIV/0!"))
      (- 1 (grid-calc--normal-cdf (/ (- mean x) (/ sigma (sqrt n))))))))

;;;----------------------------------------------------------------------
;;; Error function
;;;----------------------------------------------------------------------

(grid-calc-define erf ((lower number) &optional (upper number))
  "The error function at LOWER, or between LOWER and UPPER."
  (if upper (- (grid-calc--erf upper) (grid-calc--erf lower))
    (grid-calc--erf lower)))

(grid-calc-define erf.precise ((x number))
  "The error function at X."
  (grid-calc--erf x))

(grid-calc-define erfc ((x number))
  "The complementary error function at X."
  (grid-calc--erfc x))

(grid-calc-alias erfc.precise erfc)

;;;----------------------------------------------------------------------
;;; Gamma, chi-squared, exponential and Weibull
;;;----------------------------------------------------------------------

(grid-calc-define gammaln ((x number))
  "The natural log of the gamma function at X."
  (grid-calc--require (> x 0))
  (grid-calc--log-gamma x))

(grid-calc-alias gammaln.precise gammaln)

(grid-calc-define gamma ((x number))
  "The gamma function at X."
  (grid-calc--require (not (and (<= x 0) (= x (ffloor x)))))
  (let ((value (exp (grid-calc--log-gamma x))))
    (grid-calc--finite
     (grid-calc--snap (if (and (< x 0) (cl-oddp (truncate (ffloor x)))) (- value)
                        value)))))

(defun grid-calc--gamma-density (x alpha beta)
  "Return the gamma density of ALPHA and BETA at X."
  (cond ((> x 0)
         (exp (- (* (- alpha 1) (log x)) (/ x beta) (grid-calc--log-gamma alpha)
                 (* alpha (log beta)))))
        ((and (= x 0) (= alpha 1)) (/ 1.0 beta))
        (t 0.0)))

(grid-calc-define gamma.dist ((x number) (alpha number) (beta number) (cumulative bool))
  "The gamma distribution of ALPHA and BETA at X, or its density."
  (grid-calc--require (>= x 0) (> alpha 0) (> beta 0))
  (if cumulative (grid-calc--gamma-p alpha (/ x (float beta)))
    (grid-calc--gamma-density x alpha beta)))

(grid-calc-alias gammadist gamma.dist)

(grid-calc-define gamma.inv ((p number) (alpha number) (beta number))
  "X where the gamma distribution of ALPHA and BETA is P."
  (grid-calc--require (<= 0 p) (< p 1) (> alpha 0) (> beta 0))
  (* beta (grid-calc--invert (lambda (x) (grid-calc--gamma-p alpha x))
                             p 0.0 (max 1.0 (* 2 alpha)))))

(grid-calc-alias gammainv gamma.inv)

(grid-calc-define chisq.dist ((x number) (df integer) (cumulative bool))
  "The chi-squared distribution of DF degrees of freedom at X, or its
density."
  (grid-calc--require (>= x 0) (<= 1 df 10000000000))
  (if cumulative (grid-calc--gamma-p (/ df 2.0) (/ x 2.0))
    (grid-calc--gamma-density x (/ df 2.0) 2.0)))

(grid-calc-define chisq.dist.rt ((x number) (df integer))
  "The chance a chi-squared value of DF degrees of freedom is above X."
  (grid-calc--require (>= x 0) (<= 1 df 10000000000))
  (grid-calc--gamma-q (/ df 2.0) (/ x 2.0)))

(grid-calc-alias chidist chisq.dist.rt)

(defun grid-calc--chisq-inverse (p df)
  "Return X where the chi-squared distribution of DF is P."
  (grid-calc--require (<= 0 p) (< p 1) (<= 1 df 10000000000))
  (* 2 (grid-calc--invert (lambda (x) (grid-calc--gamma-p (/ df 2.0) x))
                          p 0.0 (max 1.0 df))))

(grid-calc-define chisq.inv ((p number) (df integer))
  "X where the chi-squared distribution of DF degrees of freedom is P."
  (grid-calc--chisq-inverse p df))

(grid-calc-define chisq.inv.rt ((p number) (df integer))
  "X above which a chi-squared value of DF degrees of freedom has chance P."
  (grid-calc--require (< 0 p) (<= p 1))
  (grid-calc--chisq-inverse (- 1 p) df))

(grid-calc-alias chiinv chisq.inv.rt)

(grid-calc-define chisq.test ((actual array) (expected array))
  "The chance of a chi-squared value as large as ACTUAL against EXPECTED."
  (let ((pairs (grid-calc--pairs actual expected))
        (rows (grid-calc-array-rows actual))
        (cols (grid-calc-array-cols actual))
        (statistic 0.0))
    (dolist (pair pairs)
      (when (and (numberp (car pair)) (numberp (cdr pair)))
        (when (zerop (cdr pair)) (grid-calc-raise "#DIV/0!"))
        (setq statistic (+ statistic (/ (expt (- (car pair) (cdr pair)) 2)
                                        (float (cdr pair)))))))
    (let ((df (if (and (> rows 1) (> cols 1)) (* (1- rows) (1- cols))
                (1- (* rows cols)))))
      (grid-calc--require (>= df 1))
      (grid-calc--gamma-q (/ df 2.0) (/ statistic 2)))))

(grid-calc-alias chitest chisq.test)

(grid-calc-define expon.dist ((x number) (rate number) (cumulative bool))
  "The exponential distribution of RATE at X, or its density."
  (grid-calc--require (>= x 0) (> rate 0))
  (if cumulative (- 1 (exp (- (* rate x))))
    (* rate (exp (- (* rate x))))))

(grid-calc-alias expondist expon.dist)

(grid-calc-define weibull.dist ((x number) (alpha number) (beta number) (cumulative bool))
  "The Weibull distribution of ALPHA and BETA at X, or its density."
  (grid-calc--require (>= x 0) (> alpha 0) (> beta 0))
  (let ((power (expt (/ x (float beta)) alpha)))
    (if cumulative (- 1 (exp (- power)))
      (* (/ alpha (float beta)) (expt (/ x (float beta)) (- alpha 1)) (exp (- power))))))

(grid-calc-alias weibull weibull.dist)

;;;----------------------------------------------------------------------
;;; Beta, t and F
;;;----------------------------------------------------------------------

(grid-calc-define beta.dist ((x number) (alpha number) (beta number) (cumulative bool)
                             &optional (low number 0) (high number 1))
  "The beta distribution of ALPHA and BETA at X, between LOW and HIGH; or
its density."
  (grid-calc--require (> alpha 0) (> beta 0) (< low high) (<= low x high))
  (let ((y (/ (- x low) (float (- high low)))))
    (if cumulative (grid-calc--beta-i y alpha beta)
      (/ (exp (- (+ (* (- alpha 1) (log y)) (* (- beta 1) (log (- 1 y))))
                 (- (+ (grid-calc--log-gamma alpha) (grid-calc--log-gamma beta))
                    (grid-calc--log-gamma (+ alpha beta)))))
         (- high low)))))

(grid-calc-define beta.inv ((p number) (alpha number) (beta number)
                            &optional (low number 0) (high number 1))
  "X where the beta distribution of ALPHA and BETA, between LOW and
HIGH, is P."
  (grid-calc--require (<= 0 p 1) (> alpha 0) (> beta 0) (< low high))
  (+ low (* (- high low)
            (grid-calc--invert (lambda (y) (grid-calc--beta-i y alpha beta))
                               p 0.0 1.0))))

(grid-calc-alias betainv beta.inv)

(defun grid-calc--t-cdf (x df)
  "Return the t distribution of DF degrees of freedom at X."
  (let ((tail (* 0.5 (grid-calc--beta-i (/ df (+ df (* x x))) (/ df 2.0) 0.5))))
    (if (> x 0) (- 1 tail) tail)))

(grid-calc-define t.dist ((x number) (df integer) (cumulative bool))
  "The t distribution of DF degrees of freedom at X, or its density."
  (grid-calc--require (>= df 1))
  (if cumulative (grid-calc--t-cdf x df)
    (exp (- (+ (grid-calc--log-gamma (/ (+ df 1) 2.0))
               (* (- (/ (+ df 1) 2.0)) (log (+ 1 (/ (* x x) df)))))
            (+ (* 0.5 (log (* df float-pi))) (grid-calc--log-gamma (/ df 2.0)))))))

(grid-calc-define t.dist.rt ((x number) (df integer))
  "The chance a t value of DF degrees of freedom is above X."
  (grid-calc--require (>= df 1))
  (- 1 (grid-calc--t-cdf x df)))

(grid-calc-define t.dist.2t ((x number) (df integer))
  "The chance a t value of DF degrees of freedom is further from 0 than X."
  (grid-calc--require (>= x 0) (>= df 1))
  (* 2 (- 1 (grid-calc--t-cdf x df))))

(grid-calc-define tdist ((x number) (df integer) (tails integer))
  "The chance of a t value above X, in one or both TAILS."
  (grid-calc--require (>= x 0) (>= df 1) (memq tails '(1 2)))
  (* tails (- 1 (grid-calc--t-cdf x df))))

(defun grid-calc--t-inverse (p df)
  "Return X where the t distribution of DF degrees of freedom is P."
  (grid-calc--require (< 0 p 1) (>= df 1))
  (if (< p 0.5)
      (- (grid-calc--t-inverse (- 1 p) df))
    (grid-calc--invert (lambda (x) (grid-calc--t-cdf x df)) p 0.0 4.0)))

(grid-calc-define t.inv ((p number) (df integer))
  "X where the t distribution of DF degrees of freedom is P."
  (grid-calc--t-inverse p df))

(grid-calc-define t.inv.2t ((p number) (df integer))
  "X where the chance of a t value further from 0 is P."
  (grid-calc--require (< 0 p) (<= p 1))
  (abs (grid-calc--t-inverse (/ p 2.0) df)))

(grid-calc-alias tinv t.inv.2t)

(grid-calc-define confidence.t ((alpha number) (sd number) (size integer))
  "Half the width of a confidence interval for a mean, by the t
distribution."
  (grid-calc--require (< 0 alpha 1) (> sd 0) (>= size 1))
  (when (= size 1) (grid-calc-raise "#DIV/0!"))
  (/ (* (abs (grid-calc--t-inverse (/ alpha 2.0) (1- size))) sd) (sqrt size)))

(grid-calc-define t.test ((left array) (right array) (tails integer) (type integer))
  "The chance of the means of LEFT and RIGHT differing as much, by
Student's t test: TYPE 1 paired, 2 of equal variance, 3 of unequal."
  (grid-calc--require (memq tails '(1 2)) (memq type '(1 2 3)))
  (let (statistic df)
    (if (= type 1)
        (let* ((pairs (grid-calc--number-pairs left right))
               (diffs (mapcar (lambda (p) (- (car p) (cdr p))) pairs))
               (n (length diffs)))
          (when (< n 2) (grid-calc-raise "#DIV/0!"))
          (setq statistic (/ (/ (float (apply #'+ diffs)) n)
                             (sqrt (/ (grid-calc--variance diffs t) n)))
                df (1- n)))
      (let* ((xs (cl-remove-if-not #'numberp (grid-calc-array-values left)))
             (ys (cl-remove-if-not #'numberp (grid-calc-array-values right)))
             (nx (length xs)) (ny (length ys)))
        (when (or (< nx 2) (< ny 2)) (grid-calc-raise "#DIV/0!"))
        (let ((mx (/ (float (apply #'+ xs)) nx)) (my (/ (float (apply #'+ ys)) ny))
              (vx (grid-calc--variance xs t)) (vy (grid-calc--variance ys t)))
          (if (= type 2)
              (let ((pooled (/ (+ (* (1- nx) vx) (* (1- ny) vy)) (+ nx ny -2))))
                (setq statistic (/ (- mx my) (sqrt (* pooled (+ (/ 1.0 nx) (/ 1.0 ny)))))
                      df (+ nx ny -2)))
            (let ((sx (/ vx nx)) (sy (/ vy ny)))
              (setq statistic (/ (- mx my) (sqrt (+ sx sy)))
                    df (/ (expt (+ sx sy) 2)
                          (+ (/ (* sx sx) (1- nx)) (/ (* sy sy) (1- ny))))))))))
    (* tails (- 1 (grid-calc--t-cdf (abs statistic) df)))))

(grid-calc-alias ttest t.test)

(defun grid-calc--f-cdf (x d1 d2)
  "Return the F distribution of D1 and D2 degrees of freedom at X."
  (if (<= x 0) 0.0
    (grid-calc--beta-i (/ (* d1 x) (+ (* d1 x) d2)) (/ d1 2.0) (/ d2 2.0))))

(grid-calc-define f.dist ((x number) (d1 integer) (d2 integer) (cumulative bool))
  "The F distribution of D1 and D2 degrees of freedom at X, or its density."
  (grid-calc--require (>= x 0) (>= d1 1) (>= d2 1))
  (if cumulative (grid-calc--f-cdf x d1 d2)
    (if (zerop x) (if (= d1 2) 1.0 0.0)
      (exp (- (+ (* (/ d1 2.0) (log d1)) (* (/ d2 2.0) (log d2))
                 (* (- (/ d1 2.0) 1) (log x)))
              (+ (* (/ (+ d1 d2) 2.0) (log (+ d2 (* d1 x))))
                 (- (+ (grid-calc--log-gamma (/ d1 2.0)) (grid-calc--log-gamma (/ d2 2.0)))
                    (grid-calc--log-gamma (/ (+ d1 d2) 2.0)))))))))

(grid-calc-define f.dist.rt ((x number) (d1 integer) (d2 integer))
  "The chance an F value of D1 and D2 degrees of freedom is above X."
  (grid-calc--require (>= x 0) (>= d1 1) (>= d2 1))
  (- 1 (grid-calc--f-cdf x d1 d2)))

(grid-calc-alias fdist f.dist.rt)

(grid-calc-define f.inv ((p number) (d1 integer) (d2 integer))
  "X where the F distribution of D1 and D2 degrees of freedom is P."
  (grid-calc--require (<= 0 p) (< p 1) (>= d1 1) (>= d2 1))
  (grid-calc--invert (lambda (x) (grid-calc--f-cdf x d1 d2)) p 0.0 4.0))

(grid-calc-define f.inv.rt ((p number) (d1 integer) (d2 integer))
  "X above which an F value of D1 and D2 degrees of freedom has chance P."
  (grid-calc--require (< 0 p) (<= p 1) (>= d1 1) (>= d2 1))
  (grid-calc--invert (lambda (x) (grid-calc--f-cdf x d1 d2)) (- 1 p) 0.0 4.0))

(grid-calc-alias finv f.inv.rt)

(grid-calc-define f.test ((left array) (right array))
  "The two-tailed chance of the variances of LEFT and RIGHT differing as
much, by the F test."
  (let ((xs (cl-remove-if-not #'numberp (grid-calc-array-values left)))
        (ys (cl-remove-if-not #'numberp (grid-calc-array-values right))))
    (when (or (< (length xs) 2) (< (length ys) 2)) (grid-calc-raise "#DIV/0!"))
    (let ((vx (grid-calc--variance xs t)) (vy (grid-calc--variance ys t)))
      (when (or (zerop vx) (zerop vy)) (grid-calc-raise "#DIV/0!"))
      (let ((cdf (grid-calc--f-cdf (/ vx vy) (1- (length xs)) (1- (length ys)))))
        (* 2 (min cdf (- 1 cdf)))))))

(grid-calc-alias ftest f.test)

;;;----------------------------------------------------------------------
;;; Counts: binomial and its kin, and Poisson
;;;----------------------------------------------------------------------

(defun grid-calc--binomial-pmf (k n p)
  "Return the chance of exactly K successes in N tries of chance P."
  (cond ((= p 0) (if (= k 0) 1.0 0.0))
        ((= p 1) (if (= k n) 1.0 0.0))
        (t (exp (+ (grid-calc--log-choose n k) (* k (log p))
                   (* (- n k) (log (- 1 p))))))))

(defun grid-calc--binomial-cdf (k n p)
  "Return the chance of at most K successes in N tries of chance P."
  (cond ((>= k n) 1.0)
        ((< k 0) 0.0)
        (t (min 1.0 (cl-loop for i from 0 to k sum (grid-calc--binomial-pmf i n p))))))

(grid-calc-define binom.dist ((successes integer) (trials integer) (p number)
                              (cumulative bool))
  "The chance of SUCCESSES in TRIALS tries of chance P, or of at most
that many."
  (grid-calc--require (<= 0 successes trials) (<= 0 p 1))
  (if cumulative (grid-calc--binomial-cdf successes trials p)
    (grid-calc--binomial-pmf successes trials p)))

(grid-calc-alias binomdist binom.dist)

(grid-calc-define binom.dist.range ((trials integer) (p number) (low integer)
                                    &optional (high integer))
  "The chance of LOW to HIGH successes in TRIALS tries of chance P."
  (let ((high (or high low)))
    (grid-calc--require (<= 0 low high trials) (<= 0 p 1))
    (cl-loop for k from low to high sum (grid-calc--binomial-pmf k trials p))))

(grid-calc-define binom.inv ((trials integer) (p number) (alpha number))
  "The fewest successes in TRIALS tries of chance P whose chance of at
most that many is ALPHA or more."
  (grid-calc--require (>= trials 0) (<= 0 p 1) (< 0 alpha 1))
  (let ((sum 0.0) (k 0))
    (while (and (<= k trials)
                (< (setq sum (+ sum (grid-calc--binomial-pmf k trials p)))
                   (- alpha 1e-15)))
      (setq k (1+ k)))
    (min k trials)))

(grid-calc-alias critbinom binom.inv)

(grid-calc-define negbinom.dist ((failures integer) (successes integer) (p number)
                                 &optional (cumulative bool))
  "The chance of FAILURES failures before the SUCCESSESth success, each
of chance P; or of at most that many."
  (grid-calc--require (>= failures 0) (>= successes 1) (<= 0 p 1))
  (cl-flet ((pmf (f) (exp (+ (grid-calc--log-choose (+ f successes -1) (1- successes))
                             (* successes (log p))
                             (if (= p 1) (if (= f 0) 0 -1.0e+INF)
                               (* f (log (- 1 p))))))))
    (if cumulative (cl-loop for f from 0 to failures sum (pmf f))
      (pmf failures))))

(grid-calc-define negbinomdist ((failures integer) (successes integer) (p number))
  "The chance of FAILURES failures before the SUCCESSESth success."
  (grid-calc--require (>= failures 0) (>= successes 1) (<= 0 p 1))
  (exp (+ (grid-calc--log-choose (+ failures successes -1) (1- successes))
          (* successes (log p)) (* failures (log (- 1 p))))))

(defun grid-calc--hypergeometric (k n found total)
  "Return the chance of K of N drawn being of the FOUND among TOTAL."
  (if (or (> k found) (> (- n k) (- total found))) 0.0
    (exp (- (+ (grid-calc--log-choose found k)
               (grid-calc--log-choose (- total found) (- n k)))
            (grid-calc--log-choose total n)))))

(grid-calc-define hypgeom.dist ((k integer) (n integer) (found integer) (total integer)
                                &optional (cumulative bool))
  "The chance of K of N drawn without putting back being among the FOUND
of TOTAL; or of at most K."
  (grid-calc--require (>= k 0) (<= k n) (<= k found) (< 0 n total)
                      (< 0 found total))
  (if cumulative
      (min 1.0 (cl-loop for i from 0 to k sum (grid-calc--hypergeometric i n found total)))
    (grid-calc--hypergeometric k n found total)))

(grid-calc-define hypgeomdist ((k integer) (n integer) (found integer) (total integer))
  "The chance of K of N drawn being among the FOUND of TOTAL."
  (grid-calc--require (>= k 0) (<= k n) (<= k found) (< 0 n total) (< 0 found total))
  (grid-calc--hypergeometric k n found total))

(grid-calc-define poisson.dist ((x integer) (mean number) (cumulative bool))
  "The chance of X events where MEAN are expected, or of at most X."
  (grid-calc--require (>= x 0) (>= mean 0))
  (cond ((zerop mean) 1.0)
        (cumulative (grid-calc--gamma-q (+ x 1.0) mean))
        (t (exp (- (* x (log mean)) mean (grid-calc--log-gamma (+ x 1.0)))))))

(grid-calc-alias poisson poisson.dist)

(provide 'grid-table-calc-dist)

;;; grid-table-calc-dist.el ends here
