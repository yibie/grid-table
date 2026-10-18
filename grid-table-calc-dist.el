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

(grid-calc-define lognormdist ((x number) (mean number) (sd number))
  "The log-normal distribution at X, whose log has MEAN and SD."
  (grid-calc--require (> x 0) (> sd 0))
  (grid-calc--normal-cdf (/ (- (log x) mean) (float sd))))

(grid-calc-define lognorm.inv ((p number) (mean number) (sd number))
  "X where the log-normal distribution of MEAN and SD is P."
  (grid-calc--require (> sd 0))
  (exp (+ mean (* sd (grid-calc--normal-inverse p)))))

(grid-calc-alias loginv lognorm.inv)

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
  (let* ((values (grid-calc-array-values array))
         (numbers (cl-remove-if-not #'numberp values))
         (n (length numbers)))
    (when-let* ((error (cl-find-if #'grid-calc-err-p values)))
      (grid-calc-raise (grid-calc-err-code error)))
    (when (= n 0) (grid-calc-raise "#N/A"))
    (when (= n 1) (grid-calc-raise "#DIV/0!"))
    (when (and sigma (<= sigma 0)) (grid-calc-raise "#NUM!"))
    (let ((sigma (or sigma (sqrt (grid-calc--variance numbers t))))
          (mean (/ (float (apply #'+ numbers)) n)))
      (when (zerop sigma) (grid-calc-raise "#DIV/0!"))
      (- 1 (grid-calc--normal-cdf (/ (- mean x) (/ sigma (sqrt n))))))))

(grid-calc-alias ztest z.test)

;;;----------------------------------------------------------------------
;;; Error function
;;;----------------------------------------------------------------------

(grid-calc-define erf ((lower number) &optional (upper number))
  "The error function at LOWER, or between LOWER and UPPER."
  :toolpak t
  (if upper (- (grid-calc--erf upper) (grid-calc--erf lower))
    (grid-calc--erf lower)))

(grid-calc-define erf.precise ((x number))
  "The error function at X."
  :toolpak t
  (grid-calc--erf x))

(grid-calc-define erfc ((x number))
  "The complementary error function at X."
  :toolpak t
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
  (grid-calc--require (not (and (<= x 0) (= x (ffloor (float x))))))
  (let ((value (exp (grid-calc--log-gamma x))))
    (grid-calc--finite
     (grid-calc--snap (if (and (< x 0) (cl-oddp (floor x))) (- value)
                        value)))))

(defun grid-calc--gamma-density (x alpha beta)
  "Return the gamma density of ALPHA and BETA at X."
  (cond ((> x 0)
         (exp (- (* (- alpha 1) (log x)) (/ x (float beta)) (grid-calc--log-gamma alpha)
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
  "The chance of a chi-squared value as large as ACTUAL against EXPECTED.
An error in either is given, those of ACTUAL first; a single value is
#N/A, and no pair of numbers #DIV/0!, as in Excel."
  (grid-calc--raise-pair-error actual expected t)
  (let ((pairs (grid-calc--number-pairs actual expected))
        (rows (grid-calc-array-rows actual))
        (cols (grid-calc-array-cols actual))
        (statistic 0.0))
    (let ((df (if (and (> rows 1) (> cols 1)) (* (1- rows) (1- cols))
                (1- (* rows cols)))))
      (when (< df 1) (grid-calc-raise "#N/A"))
      (unless pairs (grid-calc-raise "#DIV/0!"))
      (dolist (pair pairs)
        (when (zerop (cdr pair)) (grid-calc-raise "#DIV/0!"))
        (setq statistic (+ statistic (/ (expt (- (car pair) (cdr pair)) 2)
                                        (float (cdr pair))))))
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

(grid-calc-define betadist ((x number) (alpha number) (beta number)
                           &optional (low number 0) (high number 1))
  "The beta distribution of ALPHA and BETA at X, between LOW and HIGH."
  (grid-calc--require (> alpha 0) (> beta 0) (< low high) (<= low x high))
  (grid-calc--beta-i (/ (- x low) (float (- high low))) alpha beta))

(grid-calc-define beta.inv ((p number) (alpha number) (beta number)
                            &optional (low number 0) (high number 1))
  "X where the beta distribution of ALPHA and BETA, between LOW and
HIGH, is P."
  (grid-calc--require (< 0 p) (<= p 1) (> alpha 0) (> beta 0) (< low high))
  (if (= p 1) high
    (+ low (* (- high low)
              (grid-calc--invert (lambda (y) (grid-calc--beta-i y alpha beta))
                                 p 0.0 1.0)))))

(grid-calc-alias betainv beta.inv)

(defun grid-calc--t-cdf (x df)
  "Return the t distribution of DF degrees of freedom at X."
  (let ((tail (* 0.5 (grid-calc--beta-i (/ df (+ df (* x x 1.0))) (/ df 2.0) 0.5))))
    (if (> x 0) (- 1 tail) tail)))

(grid-calc-define t.dist ((x number) (df integer) (cumulative bool))
  "The t distribution of DF degrees of freedom at X, or its density.
The density of DF 0 is #DIV/0!, as in Excel."
  (when (and (= df 0) (not cumulative)) (grid-calc-raise "#DIV/0!"))
  (grid-calc--require (>= df 1))
  (if cumulative (grid-calc--t-cdf x df)
    (exp (- (+ (grid-calc--log-gamma (/ (+ df 1) 2.0))
               (* (- (/ (+ df 1) 2.0)) (log (+ 1 (/ (* x x 1.0) df)))))
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
    (grid-calc--beta-i (/ (* d1 x 1.0) (+ (* d1 x) d2)) (/ d1 2.0) (/ d2 2.0))))

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
much, by the F test.  An error in either is given, those of LEFT first."
  (grid-calc--raise-pair-error left right t)
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
  (grid-calc--require (>= failures 0) (>= successes 1) (< 0 p) (<= p 1))
  (cl-flet ((pmf (f) (exp (+ (grid-calc--log-choose (+ f successes -1) (1- successes))
                             (* successes (log p))
                             (if (= p 1) (if (= f 0) 0 -1.0e+INF)
                               (* f (log (- 1 p))))))))
    (if cumulative (cl-loop for f from 0 to failures sum (pmf f))
      (pmf failures))))

(grid-calc-define negbinomdist ((failures integer) (successes integer) (p number))
  "The chance of FAILURES failures before the SUCCESSESth success."
  (grid-calc--require (>= failures 0) (>= successes 1) (< 0 p) (<= p 1))
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
  (grid-calc--require (>= k 0) (<= k n) (<= k found) (<= 0 n total)
                      (<= 0 found total))
  (if cumulative
      (min 1.0 (cl-loop for i from 0 to k sum (grid-calc--hypergeometric i n found total)))
    (grid-calc--hypergeometric k n found total)))

(grid-calc-define hypgeomdist ((k integer) (n integer) (found integer) (total integer))
  "The chance of K of N drawn being among the FOUND of TOTAL."
  (grid-calc--require (>= k 0) (<= k n) (<= k found) (<= 0 n total) (<= 0 found total))
  (grid-calc--hypergeometric k n found total))

(grid-calc-define poisson.dist ((x integer) (mean number) (cumulative bool))
  "The chance of X events where MEAN are expected, or of at most X."
  (grid-calc--require (>= x 0) (>= mean 0))
  (cond ((zerop mean) 1.0)
        (cumulative (grid-calc--gamma-q (+ x 1.0) mean))
        (t (exp (- (* x (log mean)) mean (grid-calc--log-gamma (+ x 1.0)))))))

(grid-calc-alias poisson poisson.dist)

;;;----------------------------------------------------------------------
;;; Forecasting by exponential smoothing
;;;----------------------------------------------------------------------

;; FORECAST.ETS and its kin fit additive Holt-Winters smoothing to
;; values along a timeline of constant step: a level, a trend and,
;; when the values repeat in a pattern, a season.  The equations are
;; the NIST Engineering Statistics Handbook's, 6.4.3.5.  How the step,
;; gaps, repeated points and the season's length are found, how the
;; three smoothing constants are searched for, and how the level, trend
;; and season start, follow LibreOffice Calc's FORECAST.ETS.  The
;; prediction interval is that of the ETS(A,A,A) model in Hyndman et
;; al., Forecasting with Exponential Smoothing (2008), 6.3.
;;
;; Excel does not publish how it fits the constants, so its values
;; differ a little from these, as LibreOffice's do.

(cl-defstruct (grid-calc--ets (:constructor grid-calc--ets-make))
  "Values smoothed along a timeline.
YS are the values a STEP apart, FIRST the first one's place on the
timeline and LAST the last one's; MONTH-DAY, when non-zero, says the
timeline is dates on that day of the month, a month apart, places
then counted in months.  PERIOD is the season's length, 0 for none.
ALPHA, BETA and GAMMA smooth the level, trend and season; LEVEL,
TREND and SEASON hold each step's state, SEASON PERIOD places ahead,
and FORECAST each value as the step before foretold it."
  ys step first last month-day period alpha beta gamma
  level trend season forecast)

(defun grid-calc--ets-month (serial)
  "Return SERIAL's month counted from year 0, and its day: (MONTHS DAY)."
  (pcase-let ((`(,year ,month ,day) (grid-calc-serial-date serial)))
    (list (+ (* 12 year) month) day)))

(defun grid-calc--ets-place (ets x)
  "Return where on ETS's timeline X falls, in months when it counts them."
  (let ((day (grid-calc--ets-month-day ets)))
    (if (zerop day)
        (float x)
      (pcase-let* ((`(,year ,month ,on) (grid-calc-serial-date x))
                   (length (cond ((memq month '(4 6 9 11)) 30)
                                 ((/= month 2) 31)
                                 ((grid-calc--leap-year-p year) 29)
                                 (t 28))))
        (+ (* 12.0 year) month (/ (- on day) (float length)))))))

(defun grid-calc--leap-year-p (year)
  "Return non-nil when YEAR has a 29 February."
  (and (zerop (% year 4)) (or (/= 0 (% year 100)) (zerop (% year 400)))))

(defun grid-calc--ets-aggregate (ys aggregation)
  "Return the one value of YS, values at one place, AGGREGATION says.
It is 1 AVERAGE, 2 COUNT, 3 COUNTA, 4 MAX, 5 MEDIAN, 6 MIN or 7 SUM."
  (pcase aggregation
    (1 (/ (apply #'+ ys) (float (length ys))))
    ((or 2 3) (length ys))
    (4 (apply #'max ys))
    (5 (grid-calc--median ys))
    (6 (apply #'min ys))
    (7 (apply #'+ ys))))

(defun grid-calc--ets-points (values timeline completion aggregation)
  "Return an ETS of the VALUES along TIMELINE, arrays of one size.
Points at one place become one by AGGREGATION, and a gap of whole steps
is filled with the mean of the points beside it, or with 0 when
COMPLETION is 0.  The step must be constant, gaps aside, and at most
three points in ten missing."
  (let* ((pairs (grid-calc--pairs timeline values))
         points)
    (pcase-dolist (`(,x . ,y) pairs)
      (dolist (v (list x y))
        (when (grid-calc-err-p v) (grid-calc-raise (grid-calc-err-code v))))
      (unless (grid-calc-blank-p y)
        (unless (and (numberp x) (numberp y)) (grid-calc-raise "#VALUE!"))
        (push (cons (float x) (float y)) points)))
    (when (< (length points) 3) (grid-calc-raise "#VALUE!"))
    (setq points (sort points (lambda (a b) (< (car a) (car b)))))
    (let* ((first (caar points))
           (month-day
            ;; Dates on one day of the month are a month apart, however
            ;; many days that is.
            (let ((day (cadr (grid-calc--ets-month first))))
              (if (cl-every (lambda (point)
                              (= (cadr (grid-calc--ets-month (car point))) day))
                            points)
                  day 0)))
           (points (if (zerop month-day) points
                     (mapcar (lambda (point)
                               (cons (float (car (grid-calc--ets-month (car point))))
                                     (cdr point)))
                             points)))
           (merged nil))
      ;; Points at one place.
      (while points
        (let* ((x (caar points))
               (same (cl-loop while (and points (= (caar points) x))
                              collect (cdr (pop points)))))
          (push (cons x (if (cdr same)
                            (grid-calc--ets-aggregate same aggregation)
                          (car same)))
                merged)))
      (setq merged (nreverse merged))
      (when (< (length merged) 3) (grid-calc-raise "#VALUE!"))
      (let* ((step (cl-loop for (a b) on merged while b
                            minimize (- (car b) (car a))))
             (count (length merged))
             (ys nil))
        ;; The step is constant, or gaps are whole steps.
        (cl-loop for (a b) on merged while b
                 for steps = (/ (- (car b) (car a)) (float step))
                 do (unless (= steps (ffloor steps)) (grid-calc-raise "#NUM!"))
                 do (push (cdr a) ys)
                 do (dotimes (_ (1- (truncate steps)))
                      (push (if (eql completion 0) 0.0 (/ (+ (cdr a) (cdr b)) 2))
                            ys)))
        (push (cdar (last merged)) ys)
        (setq ys (vconcat (nreverse ys)))
        (when (> (- (length ys) count) (* 0.3 count)) (grid-calc-raise "#NUM!"))
        (grid-calc--ets-make :ys ys :step step :first (caar merged)
                             :last (car (car (last merged)))
                             :month-day month-day)))))

(defun grid-calc--ets-period-length (ys)
  "Return how many of YS a season most likely lasts, 1 for none.
Each length from half the values down to 1 is tried, setting the rise
from each value to the next beside that a season later; the length
whose rises differ least wins, the shorter on a tie."
  (let ((n (length ys)) (best 1) (best-error nil))
    (cl-loop for length from (/ n 2) downto 1
             for periods = (/ n length)
             for divisor = (1- (* (1- periods) length))
             when (> divisor 0)
             do (let ((error
                       (/ (cl-loop for i from (1+ (- n (* periods length)))
                                   below (- n length)
                                   sum (abs (- (- (aref ys i) (aref ys (1- i)))
                                               (- (aref ys (+ i length))
                                                  (aref ys (+ i length -1))))))
                          (float divisor))))
                  (when (or (null best-error) (<= error best-error))
                    (setq best length best-error error))))
    best))

(defun grid-calc--ets-start (ets)
  "Give ETS its level, trend and season at the first value."
  (let* ((ys (grid-calc--ets-ys ets))
         (n (length ys))
         (period (grid-calc--ets-period ets))
         (season (make-vector (+ n period) 0.0))
         (trend (if (zerop period)
                    (/ (- (aref ys (1- n)) (aref ys 0)) (float (1- n)))
                  (/ (cl-loop for i below period
                              sum (- (aref ys (+ i period)) (aref ys i)))
                     (float (* period period))))))
    (unless (zerop period)
      ;; Each place's mean distance from its season's mean, the trend
      ;; taken out.
      (let* ((periods (/ n period))
             (means (cl-loop for p below periods
                             collect (/ (cl-loop for j below period
                                                 sum (aref ys (+ (* p period) j)))
                                        (float period)))))
        (dotimes (j period)
          (aset season j
                (/ (cl-loop for p below periods
                            for mean in means
                            sum (- (aref ys (+ (* p period) j))
                                   (+ mean (* (- j (* 0.5 (1- period))) trend))))
                   (float periods))))))
    (setf (grid-calc--ets-season ets) season
          (grid-calc--ets-level ets) (make-vector n 0.0)
          (grid-calc--ets-trend ets) (make-vector n 0.0)
          (grid-calc--ets-forecast ets) (make-vector n 0.0))
    (aset (grid-calc--ets-level ets) 0 (- (aref ys 0) (aref season 0)))
    (aset (grid-calc--ets-trend ets) 0 trend)
    (aset season period (aref season 0))
    (aset (grid-calc--ets-forecast ets) 0 (aref ys 0))))

(defun grid-calc--ets-smooth (ets)
  "Smooth ETS's values with its constants, and return the mean squared error."
  (let* ((ys (grid-calc--ets-ys ets)) (n (length ys))
         (period (grid-calc--ets-period ets))
         (alpha (grid-calc--ets-alpha ets))
         (beta (grid-calc--ets-beta ets))
         (gamma (grid-calc--ets-gamma ets))
         (level (grid-calc--ets-level ets))
         (trend (grid-calc--ets-trend ets))
         (season (grid-calc--ets-season ets))
         (forecast (grid-calc--ets-forecast ets))
         (squares 0.0))
    (cl-loop for i from 1 below n
             for y = (aref ys i)
             for ahead = (+ (aref level (1- i)) (aref trend (1- i)))
             for seasonal = (aref season i)
             do (aset forecast i (+ ahead seasonal))
             do (aset level i (+ (* alpha (- y seasonal)) (* (- 1 alpha) ahead)))
             do (aset trend i (+ (* beta (- (aref level i) (aref level (1- i))))
                                 (* (- 1 beta) (aref trend (1- i)))))
             do (unless (zerop period)
                  (aset season (+ i period)
                        (+ (* gamma (- y (aref level i))) (* (- 1 gamma) seasonal))))
             do (setq squares (+ squares (expt (- (aref forecast i) y) 2))))
    (/ squares (1- n))))

(defun grid-calc--ets-search (ets setter inner)
  "Search for the constant SETTER sets of ETS that fits it best.
It halves 0 to 1 towards the end with the smaller error, to 0.001,
calling INNER after each try to fit the constants it leaves to it.
Return the error."
  (let ((try (lambda (value)
               (funcall setter value)
               (funcall inner)
               (grid-calc--ets-smooth ets))))
    (let* ((low 0.0) (high 1.0) (middle 0.5)
           (low-error (funcall try low))
           (high-error (funcall try high))
           (error (funcall try middle)))
      (if (= low-error error high-error)
          (funcall try 0.0)
        (while (> (- high middle) 0.001)
          (if (> high-error low-error)
              (setq high middle high-error error middle (/ (+ low middle) 2))
            (setq low middle low-error error middle (/ (+ middle high) 2)))
          (setq error (funcall try middle)))
        (cond ((and (> high-error low-error) (< low-error error))
               (funcall try low))
              ((and (<= high-error low-error) (< high-error error))
               (funcall try high))
              (t error))))))

(defun grid-calc--ets-fit (ets)
  "Fit ETS's constants, smoothing level, then season, then trend."
  (grid-calc--ets-start ets)
  (let* ((trend (lambda ()
                  (grid-calc--ets-search
                   ets (lambda (v) (setf (grid-calc--ets-beta ets) v)) #'ignore)))
         (season (if (zerop (grid-calc--ets-period ets))
                     (lambda ()
                       (setf (grid-calc--ets-gamma ets) 0.0)
                       (funcall trend))
                   (lambda ()
                     (grid-calc--ets-search
                      ets (lambda (v) (setf (grid-calc--ets-gamma ets) v)) trend)))))
    (grid-calc--ets-search
     ets (lambda (v) (setf (grid-calc--ets-alpha ets) v)) season))
  ets)

(defvar grid-calc--ets-last nil
  "The arguments and the fit of the last values smoothed, (KEY . ETS).
Formulas filled down one column smooth the same values again and again.")

(defun grid-calc--ets (values timeline seasonality completion aggregation)
  "Return an ETS fit to VALUES along TIMELINE.
SEASONALITY is 1 to find the season's length, 0 for no season, or the
length; COMPLETION and AGGREGATION are as `grid-calc--ets-points' takes."
  (unless (and (= seasonality (floor seasonality)) (<= 0 seasonality 8760))
    (grid-calc-raise "#NUM!"))
  (unless (memql completion '(0 1 0.0 1.0)) (grid-calc-raise "#NUM!"))
  (unless (and (= aggregation (floor aggregation)) (<= 1 aggregation 7))
    (grid-calc-raise "#NUM!"))
  (let ((key (list (grid-calc-array-values values)
                   (grid-calc-array-values timeline)
                   seasonality completion aggregation
                   (grid-format-date-offset))))
    (if (equal key (car grid-calc--ets-last))
        (cdr grid-calc--ets-last)
      (let* ((ets (grid-calc--ets-points values timeline completion
                                         (truncate aggregation)))
             (ys (grid-calc--ets-ys ets))
             (period (pcase (truncate seasonality)
                       (1 (grid-calc--ets-period-length ys))
                       (length length))))
        (when (> (* 2 period) (length ys)) (grid-calc-raise "#NUM!"))
        (setf (grid-calc--ets-period ets) (if (<= period 1) 0 period))
        (grid-calc--ets-fit ets)
        (setq grid-calc--ets-last (cons key ets))
        ets))))

(defun grid-calc--ets-steps (ets target)
  "Return how many steps past ETS's last value TARGET is, or #NUM!."
  (let ((steps (/ (- (grid-calc--ets-place ets target) (grid-calc--ets-last ets))
                  (grid-calc--ets-step ets))))
    (when (< steps 0) (grid-calc-raise "#NUM!"))
    steps))

(defun grid-calc--ets-interpolate (function steps)
  "Return FUNCTION at STEPS, a line between the whole steps beside it."
  (let* ((whole (floor steps))
         (at (funcall function whole))
         (fraction (- steps whole)))
    (if (< fraction 0.001) at
      (+ at (* fraction (- (funcall function (1+ whole)) at))))))

(defun grid-calc--ets-ahead (ets steps)
  "Return the value ETS foretells STEPS whole steps past its last value."
  (let* ((n (length (grid-calc--ets-ys ets)))
         (period (grid-calc--ets-period ets)))
    (+ (aref (grid-calc--ets-level ets) (1- n))
       (* steps (aref (grid-calc--ets-trend ets) (1- n)))
       (if (zerop period) 0.0
         (aref (grid-calc--ets-season ets) (+ n (mod (1- steps) period)))))))

(defun grid-calc--ets-errors (ets)
  "Return the forecast errors of ETS's values past the first."
  (let ((ys (grid-calc--ets-ys ets)) (forecast (grid-calc--ets-forecast ets)))
    (cl-loop for i from 1 below (length ys)
             collect (- (aref forecast i) (aref ys i)))))

(defun grid-calc--ets-rmse (ets)
  "Return the root mean squared error of ETS's forecasts."
  (let ((errors (grid-calc--ets-errors ets)))
    (sqrt (/ (cl-loop for e in errors sum (* e e)) (length errors)))))

(grid-calc-define forecast.ets ((target number) (values array) (timeline array)
                                &optional (seasonality number 1)
                                (completion number 1) (aggregation number 1))
  "The value at TARGET of VALUES along TIMELINE, smoothed.
See `grid-calc--ets' for SEASONALITY, COMPLETION and AGGREGATION."
  :empty t
  (let ((ets (grid-calc--ets values timeline seasonality completion aggregation)))
    (grid-calc--ets-interpolate (lambda (steps) (grid-calc--ets-ahead ets steps))
                                (grid-calc--ets-steps ets target))))

(grid-calc-define forecast.ets.confint ((target number) (values array)
                                        (timeline array)
                                        &optional (level number 0.95)
                                        (seasonality number 1)
                                        (completion number 1)
                                        (aggregation number 1))
  "Half the width of the interval FORECAST.ETS's value at TARGET falls in.
It does so with the chance LEVEL, between 0 and 1."
  :empty t
  (unless (< 0 level 1) (grid-calc-raise "#NUM!"))
  (let* ((ets (grid-calc--ets values timeline seasonality completion aggregation))
         (alpha (grid-calc--ets-alpha ets))
         (beta (grid-calc--ets-beta ets))
         (gamma (grid-calc--ets-gamma ets))
         (period (grid-calc--ets-period ets))
         (width (* (grid-calc--normal-inverse (/ (+ 1 level) 2))
                   (grid-calc--ets-rmse ets))))
    (grid-calc--ets-interpolate
     (lambda (steps)
       (if (< steps 1) 0.0
         ;; How much each step's error carries into one J steps on.
         (* width
            (sqrt (1+ (cl-loop for j from 1 below steps
                               sum (expt (+ (* alpha (1+ (* beta j)))
                                            (if (and (> period 0) (zerop (% j period)))
                                                (* gamma (- 1 alpha))
                                              0))
                                         2)))))))
     (grid-calc--ets-steps ets target))))

(grid-calc-define forecast.ets.seasonality ((values array) (timeline array)
                                            &optional (completion number 1)
                                            (aggregation number 1))
  "How many steps the season FORECAST.ETS finds in VALUES lasts, 0 for none."
  :empty t
  (grid-calc--ets-period (grid-calc--ets values timeline 1 completion aggregation)))

(grid-calc-define forecast.ets.stat ((values array) (timeline array) (statistic number)
                                     &optional (seasonality number 1)
                                     (completion number 1)
                                     (aggregation number 1))
  "A measure of how FORECAST.ETS smooths VALUES, STATISTIC saying which.
It is 1 alpha, 2 beta, 3 gamma, the constants smoothing the level, trend
and season; 4 MASE, 5 SMAPE, 6 MAE and 7 RMSE, measures of the
forecasts' errors; or 8 the timeline's step."
  :empty t
  (let* ((ets (grid-calc--ets values timeline seasonality completion aggregation))
         (ys (grid-calc--ets-ys ets))
         (forecast (grid-calc--ets-forecast ets))
         (errors (grid-calc--ets-errors ets))
         (count (length errors))
         (mae (/ (cl-loop for e in errors sum (abs e)) count)))
    (pcase (truncate statistic)
      (1 (grid-calc--ets-alpha ets))
      (2 (grid-calc--ets-beta ets))
      (3 (grid-calc--ets-gamma ets))
      (4 (let ((rises (cl-loop for i from 2 below (length ys)
                               sum (abs (- (aref ys i) (aref ys (1- i)))))))
           (if (zerop rises) (grid-calc-raise "#DIV/0!")
             (/ mae (/ rises (1- count))))))
      (5 (* (/ 2.0 count)
            (cl-loop for i from 1 below (length ys)
                     for e in errors
                     for sum = (+ (abs (aref forecast i)) (abs (aref ys i)))
                     unless (zerop sum) sum (/ (abs e) sum))))
      (6 mae)
      (7 (grid-calc--ets-rmse ets))
      (8 (grid-calc--ets-step ets))
      (_ (grid-calc-raise "#NUM!")))))

(provide 'grid-table-calc-dist)

;;; grid-table-calc-dist.el ends here
