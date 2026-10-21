;;; exc-calc-dist.el --- Probability distributions -*- lexical-binding: t -*-

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

(require 'exc-calc)
(require 'exc-calc-math)
(require 'exc-calc-stats)

;;;----------------------------------------------------------------------
;;; Special functions
;;;----------------------------------------------------------------------

(defconst exc-calc--lanczos
  [0.99999999999980993 676.5203681218851 -1259.1392167224028
   771.32342877765313 -176.61502916214059 12.507343278686905
   -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7]
  "Coefficients of the Lanczos approximation, g = 7.")

(defun exc-calc--log-gamma (x)
  "Return the log of the absolute value of the gamma function at X."
  (if (< x 0.5)
      ;; Reflection: gamma(x) gamma(1-x) = pi / sin(pi x)
      (- (log (/ float-pi (abs (sin (* float-pi x)))))
         (exc-calc--log-gamma (- 1 x)))
    (let* ((x (- x 1.0))
           (sum (aref exc-calc--lanczos 0))
           (tt (+ x 7.5)))
      (cl-loop for i from 1 below 9
               do (setq sum (+ sum (/ (aref exc-calc--lanczos i) (+ x i)))))
      (+ (* 0.5 (log (* 2 float-pi))) (* (+ x 0.5) (log tt)) (- tt) (log sum)))))

(defconst exc-calc--epsilon 1e-15 "When a series or fraction has converged.")

(defun exc-calc--term-limit (a)
  "Return how many terms a series or fraction of A may take.
The terms of the incomplete gamma functions are down to nothing after
about twice the square root of A, but the sum takes more to settle at
the last digit: ten times that, and no more than it is worth spending
on a number that big."
  (min 200000 (max 1000 (ceiling (* 10.0 (sqrt (max a 1.0)))))))

(defun exc-calc--gamma-series (a x)
  "Return P(A, X) by its series, for X below A + 1."
  (let ((sum (/ 1.0 a)) (term (/ 1.0 a)) (n a))
    (catch 'done
      (dotimes (_ (exc-calc--term-limit a))
        (setq n (1+ n) term (* term (/ x n)) sum (+ sum term))
        (when (< (abs term) (* (abs sum) exc-calc--epsilon)) (throw 'done nil))))
    (* sum (exp (- (* a (log x)) x (exc-calc--log-gamma a))))))

(defun exc-calc--gamma-fraction (a x)
  "Return Q(A, X) by its continued fraction, for X from A + 1."
  (let* ((tiny 1e-300)
         (b (+ x 1.0 (- a))) (c (/ 1.0 tiny)) (d (/ 1.0 b)) (h d))
    (catch 'done
      (cl-loop for i from 1 to (exc-calc--term-limit a)
               do (let* ((an (* (- i) (- i a))))
                    (setq b (+ b 2.0)
                          d (+ (* an d) b))
                    (when (< (abs d) tiny) (setq d tiny))
                    (setq c (+ b (/ an c)))
                    (when (< (abs c) tiny) (setq c tiny))
                    (setq d (/ 1.0 d))
                    (let ((delta (* d c)))
                      (setq h (* h delta))
                      (when (< (abs (- delta 1.0)) exc-calc--epsilon)
                        (throw 'done nil))))))
    (* h (exp (- (* a (log x)) x (exc-calc--log-gamma a))))))

(defun exc-calc--gamma-p (a x)
  "Return the regularized lower incomplete gamma function P(A, X)."
  (cond ((<= x 0) 0.0)
        ((< x (+ a 1)) (exc-calc--gamma-series a x))
        (t (- 1.0 (exc-calc--gamma-fraction a x)))))

(defun exc-calc--gamma-q (a x)
  "Return the regularized upper incomplete gamma function Q(A, X)."
  (cond ((<= x 0) 1.0)
        ((< x (+ a 1)) (- 1.0 (exc-calc--gamma-series a x)))
        (t (exc-calc--gamma-fraction a x))))

(defun exc-calc--beta-fraction (a b x)
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
                        (when (< (abs (- delta 1.0)) exc-calc--epsilon)
                          (throw 'done nil))))))
      h)))

(defun exc-calc--beta-i (x a b)
  "Return the regularized incomplete beta function I_X(A, B)."
  (cond ((<= x 0) 0.0)
        ((>= x 1) 1.0)
        (t
         (let ((front (exp (+ (- (exc-calc--log-gamma (+ a b))
                                 (exc-calc--log-gamma a)
                                 (exc-calc--log-gamma b))
                              (* a (log x)) (* b (log (- 1 x)))))))
           (if (< x (/ (+ a 1.0) (+ a b 2.0)))
               (/ (* front (exc-calc--beta-fraction a b x)) a)
             (- 1.0 (/ (* front (exc-calc--beta-fraction b a (- 1 x))) b)))))))

(defun exc-calc--erf (x)
  "Return the error function at X."
  (if (< x 0) (- (exc-calc--gamma-p 0.5 (* x x)))
    (exc-calc--gamma-p 0.5 (* x x))))

(defun exc-calc--erfc (x)
  "Return the complementary error function at X, precise when it is small."
  (if (< x 0) (+ 1.0 (exc-calc--gamma-p 0.5 (* x x)))
    (exc-calc--gamma-q 0.5 (* x x))))

(defun exc-calc--normal-cdf (z)
  "Return the standard normal distribution at Z."
  (* 0.5 (exc-calc--erfc (/ (- z) (sqrt 2.0)))))

(defun exc-calc--normal-pdf (z)
  "Return the standard normal density at Z."
  (/ (exp (* -0.5 z z)) (sqrt (* 2 float-pi))))

(defun exc-calc--normal-inverse (p)
  "Return Z where the standard normal distribution is P."
  (unless (< 0 p 1) (exc-calc-raise "#NUM!"))
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
      (let* ((e (- (exc-calc--normal-cdf z) p))
             (u (* e (sqrt (* 2 float-pi)) (exp (/ (* z z) 2)))))
        (setq z (- z (/ u (+ 1 (/ (* z u) 2)))))))
    z))

(defun exc-calc--invert (cdf p low high)
  "Return X between LOW and HIGH where the rising CDF is P, by bisection.
HIGH is pushed up until CDF there passes P."
  (unless (<= 0 p 1) (exc-calc-raise "#NUM!"))
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

(defmacro exc-calc--require (&rest conditions)
  "Signal #NUM! unless every one of CONDITIONS holds."
  `(unless (and ,@conditions) (exc-calc-raise "#NUM!")))

(defun exc-calc--probability (p)
  "Return P, which must be a probability from 0 to 1."
  (exc-calc--require (<= 0 p 1))
  p)

(defun exc-calc--log-choose (n k)
  "Return the log of N choose K."
  (- (exc-calc--log-gamma (+ n 1.0)) (exc-calc--log-gamma (+ k 1.0))
     (exc-calc--log-gamma (+ (- n k) 1.0))))

;;;----------------------------------------------------------------------
;;; Normal and log-normal
;;;----------------------------------------------------------------------

(exc-calc-define norm.dist ((x number) (mean number) (sd number) (cumulative bool))
  "The normal distribution of MEAN and SD at X, or its density."
  (exc-calc--require (> sd 0))
  (let ((z (/ (- x mean) (float sd))))
    (if cumulative (exc-calc--normal-cdf z) (/ (exc-calc--normal-pdf z) sd))))

(exc-calc-define normdist ((x number) (mean number) (sd number) (cumulative bool))
  "The normal distribution of MEAN and SD at X, or its density."
  (exc-calc--require (> sd 0))
  (let ((z (/ (- x mean) (float sd))))
    (if cumulative (exc-calc--normal-cdf z) (/ (exc-calc--normal-pdf z) sd))))

(exc-calc-define norm.s.dist ((z number) &optional (cumulative bool t))
  "The standard normal distribution at Z, or its density."
  (if cumulative (exc-calc--normal-cdf z) (exc-calc--normal-pdf z)))

(exc-calc-define normsdist ((z number))
  "The standard normal distribution at Z."
  (exc-calc--normal-cdf z))

(exc-calc-define norm.inv ((p number) (mean number) (sd number))
  "X where the normal distribution of MEAN and SD is P."
  (exc-calc--require (> sd 0))
  (+ mean (* sd (exc-calc--normal-inverse p))))

(exc-calc-alias norminv norm.inv)

(exc-calc-define norm.s.inv ((p number))
  "Z where the standard normal distribution is P."
  (exc-calc--normal-inverse p))

(exc-calc-alias normsinv norm.s.inv)

(exc-calc-define standardize ((x number) (mean number) (sd number))
  "X as standard deviations SD from MEAN."
  (exc-calc--require (> sd 0))
  (/ (- x mean) (float sd)))

(exc-calc-define lognorm.dist ((x number) (mean number) (sd number) (cumulative bool))
  "The log-normal distribution at X, whose log has MEAN and SD; or its density."
  (exc-calc--require (> x 0) (> sd 0))
  (let ((z (/ (- (log x) mean) (float sd))))
    (if cumulative (exc-calc--normal-cdf z)
      (/ (exc-calc--normal-pdf z) (* x sd)))))

(exc-calc-define lognormdist ((x number) (mean number) (sd number))
  "The log-normal distribution at X, whose log has MEAN and SD."
  (exc-calc--require (> x 0) (> sd 0))
  (exc-calc--normal-cdf (/ (- (log x) mean) (float sd))))

(exc-calc-define lognorm.inv ((p number) (mean number) (sd number))
  "X where the log-normal distribution of MEAN and SD is P."
  (exc-calc--require (> sd 0))
  (exp (+ mean (* sd (exc-calc--normal-inverse p)))))

(exc-calc-alias loginv lognorm.inv)

(exc-calc-define phi ((x number))
  "The standard normal density at X."
  (exc-calc--normal-pdf x))

(exc-calc-define gauss ((z number))
  "The chance a standard normal value is between 0 and Z."
  (- (exc-calc--normal-cdf z) 0.5))

(exc-calc-define confidence.norm ((alpha number) (sd number) (size integer))
  "Half the width of a confidence interval for a mean, by the normal
distribution: ALPHA the chance of being outside, SD and SIZE those of
the sample."
  (exc-calc--require (< 0 alpha 1) (> sd 0) (>= size 1))
  (/ (* (exc-calc--normal-inverse (- 1 (/ alpha 2.0))) sd) (sqrt size)))

(exc-calc-alias confidence confidence.norm)

(exc-calc-define z.test ((array array) (x number) &optional (sigma number))
  "The one-tailed chance of a mean above that of ARRAY, were it X.
SIGMA is the standard deviation, by default that of the sample."
  (let* ((values (exc-calc-array-values array))
         (numbers (cl-remove-if-not #'numberp values))
         (n (length numbers)))
    (when-let* ((error (cl-find-if #'exc-calc-err-p values)))
      (exc-calc-raise (exc-calc-err-code error)))
    (when (= n 0) (exc-calc-raise "#N/A"))
    (when (= n 1) (exc-calc-raise "#DIV/0!"))
    (when (and sigma (<= sigma 0)) (exc-calc-raise "#NUM!"))
    (let ((sigma (or sigma (sqrt (exc-calc--variance numbers t))))
          (mean (/ (float (apply #'+ numbers)) n)))
      (when (zerop sigma) (exc-calc-raise "#DIV/0!"))
      (- 1 (exc-calc--normal-cdf (/ (- mean x) (/ sigma (sqrt n))))))))

(exc-calc-alias ztest z.test)

;;;----------------------------------------------------------------------
;;; Error function
;;;----------------------------------------------------------------------

(exc-calc-define erf ((lower number) &optional (upper number))
  "The error function at LOWER, or between LOWER and UPPER."
  :toolpak t
  (if upper (- (exc-calc--erf upper) (exc-calc--erf lower))
    (exc-calc--erf lower)))

(exc-calc-define erf.precise ((x number))
  "The error function at X."
  :toolpak t
  (exc-calc--erf x))

(exc-calc-define erfc ((x number))
  "The complementary error function at X."
  :toolpak t
  (exc-calc--erfc x))

(exc-calc-alias erfc.precise erfc)

;;;----------------------------------------------------------------------
;;; Gamma, chi-squared, exponential and Weibull
;;;----------------------------------------------------------------------

(exc-calc-define gammaln ((x number))
  "The natural log of the gamma function at X."
  (exc-calc--require (> x 0))
  (exc-calc--log-gamma x))

(exc-calc-alias gammaln.precise gammaln)

(exc-calc-define gamma ((x number))
  "The gamma function at X."
  (exc-calc--require (not (and (<= x 0) (= x (ffloor (float x))))))
  (let ((value (exp (exc-calc--log-gamma x))))
    (exc-calc--finite
     (exc-calc--snap (if (and (< x 0) (cl-oddp (floor x))) (- value)
                        value)))))

(defun exc-calc--gamma-density (x alpha beta)
  "Return the gamma density of ALPHA and BETA at X.
At X 0 it is endless when ALPHA is below 1, which Excel gives as #NUM!."
  (cond ((> x 0)
         (exp (- (* (- alpha 1) (log x)) (/ x (float beta)) (exc-calc--log-gamma alpha)
                 (* alpha (log beta)))))
        ((and (= x 0) (< alpha 1)) (exc-calc-raise "#NUM!"))
        ((and (= x 0) (= alpha 1)) (/ 1.0 beta))
        (t 0.0)))

(exc-calc-define gamma.dist ((x number) (alpha number) (beta number) (cumulative bool))
  "The gamma distribution of ALPHA and BETA at X, or its density."
  (exc-calc--require (>= x 0) (> alpha 0) (> beta 0))
  (if cumulative (exc-calc--gamma-p alpha (/ x (float beta)))
    (exc-calc--gamma-density x alpha beta)))

(exc-calc-alias gammadist gamma.dist)

(exc-calc-define gamma.inv ((p number) (alpha number) (beta number))
  "X where the gamma distribution of ALPHA and BETA is P."
  (exc-calc--require (<= 0 p) (< p 1) (> alpha 0) (> beta 0))
  (* beta (exc-calc--invert (lambda (x) (exc-calc--gamma-p alpha x))
                             p 0.0 (max 1.0 (* 2 alpha)))))

(exc-calc-alias gammainv gamma.inv)

(exc-calc-define chisq.dist ((x number) (df integer) (cumulative bool))
  "The chi-squared distribution of DF degrees of freedom at X, or its
density."
  (exc-calc--require (>= x 0) (<= 1 df 10000000000))
  (if cumulative (exc-calc--gamma-p (/ df 2.0) (/ x 2.0))
    (exc-calc--gamma-density x (/ df 2.0) 2.0)))

(exc-calc-define chisq.dist.rt ((x number) (df integer))
  "The chance a chi-squared value of DF degrees of freedom is above X."
  (exc-calc--require (>= x 0) (<= 1 df 10000000000))
  (exc-calc--gamma-q (/ df 2.0) (/ x 2.0)))

(exc-calc-alias chidist chisq.dist.rt)

(defun exc-calc--chisq-inverse (p df)
  "Return X where the chi-squared distribution of DF is P."
  (exc-calc--require (<= 0 p) (< p 1) (<= 1 df 10000000000))
  (* 2 (exc-calc--invert (lambda (x) (exc-calc--gamma-p (/ df 2.0) x))
                          p 0.0 (max 1.0 df))))

(exc-calc-define chisq.inv ((p number) (df integer))
  "X where the chi-squared distribution of DF degrees of freedom is P."
  (exc-calc--chisq-inverse p df))

(exc-calc-define chisq.inv.rt ((p number) (df integer))
  "X above which a chi-squared value of DF degrees of freedom has chance P."
  (exc-calc--require (< 0 p) (<= p 1))
  (exc-calc--chisq-inverse (- 1 p) df))

(exc-calc-alias chiinv chisq.inv.rt)

(exc-calc-define chisq.test ((actual array) (expected array))
  "The chance of a chi-squared value as large as ACTUAL against EXPECTED.
An error in either is given, those of ACTUAL first; a single value is
#N/A, and no pair of numbers #DIV/0!, as in Excel."
  (exc-calc--raise-pair-error actual expected t)
  (let ((pairs (exc-calc--number-pairs actual expected))
        (rows (exc-calc-array-rows actual))
        (cols (exc-calc-array-cols actual))
        (statistic 0.0))
    (let ((df (if (and (> rows 1) (> cols 1)) (* (1- rows) (1- cols))
                (1- (* rows cols)))))
      (when (< df 1) (exc-calc-raise "#N/A"))
      (unless pairs (exc-calc-raise "#DIV/0!"))
      (dolist (pair pairs)
        (when (zerop (cdr pair)) (exc-calc-raise "#DIV/0!"))
        (setq statistic (+ statistic (/ (expt (- (car pair) (cdr pair)) 2)
                                        (float (cdr pair))))))
      (exc-calc--gamma-q (/ df 2.0) (/ statistic 2)))))

(exc-calc-alias chitest chisq.test)

(exc-calc-define expon.dist ((x number) (rate number) (cumulative bool))
  "The exponential distribution of RATE at X, or its density."
  (exc-calc--require (>= x 0) (> rate 0))
  (if cumulative (- 1 (exp (- (* rate x))))
    (* rate (exp (- (* rate x))))))

(exc-calc-alias expondist expon.dist)

(exc-calc-define weibull.dist ((x number) (alpha number) (beta number) (cumulative bool))
  "The Weibull distribution of ALPHA and BETA at X, or its density."
  (exc-calc--require (>= x 0) (> alpha 0) (> beta 0))
  (let ((power (expt (/ x (float beta)) alpha)))
    (if cumulative (- 1 (exp (- power)))
      (* (/ alpha (float beta)) (expt (/ x (float beta)) (- alpha 1)) (exp (- power))))))

(exc-calc-alias weibull weibull.dist)

;;;----------------------------------------------------------------------
;;; Beta, t and F
;;;----------------------------------------------------------------------

(exc-calc-define beta.dist ((x number) (alpha number) (beta number) (cumulative bool)
                             &optional (low number 0) (high number 1))
  "The beta distribution of ALPHA and BETA at X, between LOW and HIGH; or
its density."
  (exc-calc--require (> alpha 0) (> beta 0) (< low high) (<= low x high))
  (let ((y (/ (- x low) (float (- high low)))))
    (if cumulative (exc-calc--beta-i y alpha beta)
      (/ (exp (- (+ (* (- alpha 1) (log y)) (* (- beta 1) (log (- 1 y))))
                 (- (+ (exc-calc--log-gamma alpha) (exc-calc--log-gamma beta))
                    (exc-calc--log-gamma (+ alpha beta)))))
         (- high low)))))

(exc-calc-define betadist ((x number) (alpha number) (beta number)
                           &optional (low number 0) (high number 1))
  "The beta distribution of ALPHA and BETA at X, between LOW and HIGH."
  (exc-calc--require (> alpha 0) (> beta 0) (< low high) (<= low x high))
  (exc-calc--beta-i (/ (- x low) (float (- high low))) alpha beta))

(exc-calc-define beta.inv ((p number) (alpha number) (beta number)
                            &optional (low number 0) (high number 1))
  "X where the beta distribution of ALPHA and BETA, between LOW and
HIGH, is P."
  (exc-calc--require (< 0 p) (< p 1) (> alpha 0) (> beta 0) (< low high))
  (if (< (- 1.0 p) 1e-6)
      ;; A chance that near 1 leaves too little of the beta function to
      ;; tell where it is: it is read as the little left of it, from the
      ;; other end of the distribution, where its digits are.
      (- high (* (- high low)
                 (exc-calc--invert (lambda (y) (exc-calc--beta-i y beta alpha))
                                    (- 1.0 p) 0.0 1.0)))
    (+ low (* (- high low)
              (exc-calc--invert (lambda (y) (exc-calc--beta-i y alpha beta))
                                 p 0.0 1.0)))))

(exc-calc-alias betainv beta.inv)

(defun exc-calc--t-cdf (x df)
  "Return the t distribution of DF degrees of freedom at X."
  (let ((tail (* 0.5 (exc-calc--beta-i (/ df (+ df (* x x 1.0))) (/ df 2.0) 0.5))))
    (if (> x 0) (- 1 tail) tail)))

(exc-calc-define t.dist ((x number) (df integer) (cumulative bool))
  "The t distribution of DF degrees of freedom at X, or its density.
The density of DF 0 is #DIV/0!, as in Excel."
  (when (and (= df 0) (not cumulative)) (exc-calc-raise "#DIV/0!"))
  (exc-calc--require (>= df 1))
  (if cumulative (exc-calc--t-cdf x df)
    (exp (- (+ (exc-calc--log-gamma (/ (+ df 1) 2.0))
               (* (- (/ (+ df 1) 2.0)) (log (+ 1 (/ (* x x 1.0) df)))))
            (+ (* 0.5 (log (* df float-pi))) (exc-calc--log-gamma (/ df 2.0)))))))

(exc-calc-define t.dist.rt ((x number) (df integer))
  "The chance a t value of DF degrees of freedom is above X."
  (exc-calc--require (>= df 1))
  (- 1 (exc-calc--t-cdf x df)))

(exc-calc-define t.dist.2t ((x number) (df integer))
  "The chance a t value of DF degrees of freedom is further from 0 than X."
  (exc-calc--require (>= x 0) (>= df 1))
  (* 2 (- 1 (exc-calc--t-cdf x df))))

(exc-calc-define tdist ((x number) (df integer) (tails integer))
  "The chance of a t value above X, in one or both TAILS."
  (exc-calc--require (>= x 0) (>= df 1) (memq tails '(1 2)))
  (* tails (- 1 (exc-calc--t-cdf x df))))

(defun exc-calc--invert-below (cdf p high)
  "Return X below HIGH where the rising CDF is P, the way down pushed out.
A chance far below a half needs an X far below 0, past where 1 - P
would have told it from a half."
  (let ((low (if (zerop high) -1.0 (- (abs high)))) (tries 0))
    (while (and (> (funcall cdf low) p) (< tries 200))
      (setq low (* 2.0 low) tries (1+ tries)))
    (exc-calc--invert cdf p low high)))

(defun exc-calc--t-inverse (p df)
  "Return X where the t distribution of DF degrees of freedom is P."
  (exc-calc--require (< 0 p 1) (>= df 1))
  (if (< p 0.5)
      ;; A chance so far below a half that 1 - P is a whole is read as it
      ;; stands, from the far side of 0, where its digits still are.
      (let ((upper (- 1.0 p)))
        (if (< upper 1.0)
            (- (exc-calc--t-inverse upper df))
          (exc-calc--invert-below (lambda (x) (exc-calc--t-cdf x df)) p 0.0)))
    (exc-calc--invert (lambda (x) (exc-calc--t-cdf x df)) p 0.0 4.0)))

(exc-calc-define t.inv ((p number) (df integer))
  "X where the t distribution of DF degrees of freedom is P."
  (exc-calc--t-inverse p df))

(exc-calc-define t.inv.2t ((p number) (df integer))
  "X where the chance of a t value further from 0 is P."
  (exc-calc--require (< 0 p) (<= p 1))
  (abs (exc-calc--t-inverse (/ p 2.0) df)))

(exc-calc-alias tinv t.inv.2t)

(exc-calc-define confidence.t ((alpha number) (sd number) (size integer))
  "Half the width of a confidence interval for a mean, by the t
distribution."
  (exc-calc--require (< 0 alpha 1) (> sd 0) (>= size 1))
  (when (= size 1) (exc-calc-raise "#DIV/0!"))
  (/ (* (abs (exc-calc--t-inverse (/ alpha 2.0) (1- size))) sd) (sqrt size)))

(exc-calc-define t.test ((left array) (right array) (tails integer) (type integer))
  "The chance of the means of LEFT and RIGHT differing as much, by
Student's t test: TYPE 1 paired, 2 of equal variance, 3 of unequal."
  (exc-calc--require (memq tails '(1 2)) (memq type '(1 2 3)))
  (let (statistic df)
    (if (= type 1)
        (let* ((pairs (exc-calc--number-pairs left right))
               (diffs (mapcar (lambda (p) (- (car p) (cdr p))) pairs))
               (n (length diffs)))
          (when (< n 2) (exc-calc-raise "#DIV/0!"))
          (setq statistic (/ (/ (float (apply #'+ diffs)) n)
                             (sqrt (/ (exc-calc--variance diffs t) n)))
                df (1- n)))
      (let* ((xs (cl-remove-if-not #'numberp (exc-calc-array-values left)))
             (ys (cl-remove-if-not #'numberp (exc-calc-array-values right)))
             (nx (length xs)) (ny (length ys)))
        (when (or (< nx 2) (< ny 2)) (exc-calc-raise "#DIV/0!"))
        (let ((mx (/ (float (apply #'+ xs)) nx)) (my (/ (float (apply #'+ ys)) ny))
              (vx (exc-calc--variance xs t)) (vy (exc-calc--variance ys t)))
          (if (= type 2)
              (let ((pooled (/ (+ (* (1- nx) vx) (* (1- ny) vy)) (+ nx ny -2))))
                (setq statistic (/ (- mx my) (sqrt (* pooled (+ (/ 1.0 nx) (/ 1.0 ny)))))
                      df (+ nx ny -2)))
            (let ((sx (/ vx nx)) (sy (/ vy ny)))
              (setq statistic (/ (- mx my) (sqrt (+ sx sy)))
                    df (/ (expt (+ sx sy) 2)
                          (+ (/ (* sx sx) (1- nx)) (/ (* sy sy) (1- ny))))))))))
    (* tails (- 1 (exc-calc--t-cdf (abs statistic) df)))))

(exc-calc-alias ttest t.test)

(defun exc-calc--f-cdf (x d1 d2)
  "Return the F distribution of D1 and D2 degrees of freedom at X."
  (if (<= x 0) 0.0
    (exc-calc--beta-i (/ (* d1 x 1.0) (+ (* d1 x) d2)) (/ d1 2.0) (/ d2 2.0))))

(exc-calc-define f.dist ((x number) (d1 integer) (d2 integer) (cumulative bool))
  "The F distribution of D1 and D2 degrees of freedom at X, or its density."
  (exc-calc--require (>= x 0) (>= d1 1) (>= d2 1))
  (if cumulative (exc-calc--f-cdf x d1 d2)
    (if (zerop x) (if (= d1 2) 1.0 0.0)
      (exp (- (+ (* (/ d1 2.0) (log d1)) (* (/ d2 2.0) (log d2))
                 (* (- (/ d1 2.0) 1) (log x)))
              (+ (* (/ (+ d1 d2) 2.0) (log (+ d2 (* d1 x))))
                 (- (+ (exc-calc--log-gamma (/ d1 2.0)) (exc-calc--log-gamma (/ d2 2.0)))
                    (exc-calc--log-gamma (/ (+ d1 d2) 2.0)))))))))

(exc-calc-define f.dist.rt ((x number) (d1 integer) (d2 integer))
  "The chance an F value of D1 and D2 degrees of freedom is above X."
  (exc-calc--require (>= x 0) (>= d1 1) (>= d2 1))
  (- 1 (exc-calc--f-cdf x d1 d2)))

(exc-calc-alias fdist f.dist.rt)

(exc-calc-define f.inv ((p number) (d1 integer) (d2 integer))
  "X where the F distribution of D1 and D2 degrees of freedom is P."
  (exc-calc--require (<= 0 p) (< p 1) (>= d1 1) (>= d2 1))
  (exc-calc--invert (lambda (x) (exc-calc--f-cdf x d1 d2)) p 0.0 4.0))

(exc-calc-define f.inv.rt ((p number) (d1 integer) (d2 integer))
  "X above which an F value of D1 and D2 degrees of freedom has chance P."
  (exc-calc--require (< 0 p) (<= p 1) (>= d1 1) (>= d2 1))
  (exc-calc--invert (lambda (x) (exc-calc--f-cdf x d1 d2)) (- 1 p) 0.0 4.0))

(exc-calc-alias finv f.inv.rt)

(exc-calc-define f.test ((left array) (right array))
  "The two-tailed chance of the variances of LEFT and RIGHT differing as
much, by the F test.  An error in either is given, those of LEFT first."
  (exc-calc--raise-pair-error left right t)
  (let ((xs (cl-remove-if-not #'numberp (exc-calc-array-values left)))
        (ys (cl-remove-if-not #'numberp (exc-calc-array-values right))))
    (when (or (< (length xs) 2) (< (length ys) 2)) (exc-calc-raise "#DIV/0!"))
    (let ((vx (exc-calc--variance xs t)) (vy (exc-calc--variance ys t)))
      (when (or (zerop vx) (zerop vy)) (exc-calc-raise "#DIV/0!"))
      (let ((cdf (exc-calc--f-cdf (/ vx vy) (1- (length xs)) (1- (length ys)))))
        (* 2 (min cdf (- 1 cdf)))))))

(exc-calc-alias ftest f.test)

;;;----------------------------------------------------------------------
;;; Counts: binomial and its kin, and Poisson
;;;----------------------------------------------------------------------

(defun exc-calc--binomial-pmf (k n p)
  "Return the chance of exactly K successes in N tries of chance P."
  (cond ((= p 0) (if (= k 0) 1.0 0.0))
        ((= p 1) (if (= k n) 1.0 0.0))
        (t (exp (+ (exc-calc--log-choose n k) (* k (log p))
                   (* (- n k) (log (- 1 p))))))))

(defun exc-calc--binomial-cdf (k n p)
  "Return the chance of at most K successes in N tries of chance P.
The chances of 0 to K successes are the regularized incomplete beta
function I_{1-P}(N-K, K+1), the same sum closed.  Its front is the chance
of exactly K successes times P, which is what it is worked out from: a
difference of log gamma functions of N would lose digits where N is large."
  (cond ((>= k n) 1.0)
        ((< k 0) 0.0)
        ((= p 0) 1.0)
        ;; A thousand chances or fewer are summed, as exact as before;
        ;; the closed form is for more, which summing takes too long over.
        ((<= k 1000)
         (min 1.0 (cl-loop for i from 0 to k sum (exc-calc--binomial-pmf i n p))))
        (t (let* ((a (- n k)) (b (1+ k)) (x (- 1.0 p))
                  (pmf (exc-calc--binomial-pmf k n p)))
             (if (< x (/ (+ a 1.0) (+ a b 2.0)))
                 (min 1.0 (* pmf p (exc-calc--beta-fraction a b x)))
               (max 0.0 (- 1.0 (/ (* pmf p a (exc-calc--beta-fraction b a p))
                                   b))))))))

(exc-calc-define binom.dist ((successes integer) (trials integer) (p number)
                              (cumulative bool))
  "The chance of SUCCESSES in TRIALS tries of chance P, or of at most
that many."
  (exc-calc--require (<= 0 successes trials) (<= 0 p 1))
  (if cumulative (exc-calc--binomial-cdf successes trials p)
    (exc-calc--binomial-pmf successes trials p)))

(exc-calc-alias binomdist binom.dist)

(exc-calc-define binom.dist.range ((trials integer) (p number) (low integer)
                                    &optional (high integer))
  "The chance of LOW to HIGH successes in TRIALS tries of chance P."
  (let ((high (or high low)))
    (exc-calc--require (<= 0 low high trials) (<= 0 p 1))
    ;; The chances of up to HIGH successes less those of up to LOW-1 are
    ;; the chances of LOW to HIGH.  A few chances, or a range that runs
    ;; into either end, are summed instead: the two then stand so close
    ;; that taking one from the other loses digits.
    (if (or (<= (- high low) 1000) (< low 32) (>= high (- trials 32)))
        (cl-loop for k from low to high sum (exc-calc--binomial-pmf k trials p))
      (- (exc-calc--binomial-cdf high trials p)
         (exc-calc--binomial-cdf (1- low) trials p)))))

(exc-calc-define binom.inv ((trials integer) (p number) (alpha number))
  "The fewest successes in TRIALS tries of chance P whose chance of at
most that many is ALPHA or more."
  (exc-calc--require (>= trials 0) (<= 0 p) (< p 1) (< 0 alpha 1))
  ;; The chance of at most K successes only grows with K, so the fewest K
  ;; whose chance is ALPHA or more is what bisecting finds, in as many
  ;; steps as TRIALS has digits.
  (let ((low 0) (high trials))
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (>= (exc-calc--binomial-cdf middle trials p) (- alpha 1e-15))
            (setq high middle)
          (setq low (1+ middle)))))
    low))

(exc-calc-alias critbinom binom.inv)

(exc-calc-define negbinom.dist ((failures integer) (successes integer) (p number)
                                 &optional (cumulative bool))
  "The chance of FAILURES failures before the SUCCESSESth success, each
of chance P; or of at most that many."
  (exc-calc--require (>= failures 0) (>= successes 1) (< 0 p) (< p 1))
  (cl-flet ((pmf (f) (exp (+ (exc-calc--log-choose (+ f successes -1) (1- successes))
                             (* successes (log p)) (* f (log (- 1 p)))))))
    ;; The chances of 0 to FAILURES failures are the regularized incomplete
    ;; beta function I_P(SUCCESSES, FAILURES+1), the same sum closed.  A
    ;; thousand or fewer are summed, as exact as before.
    (if cumulative (if (<= failures 1000)
                       (cl-loop for f from 0 to failures sum (pmf f))
                     (exc-calc--beta-i p successes (1+ failures)))
      (pmf failures))))

(exc-calc-define negbinomdist ((failures integer) (successes integer) (p number))
  "The chance of FAILURES failures before the SUCCESSESth success."
  (exc-calc--require (>= failures 0) (>= successes 1) (< 0 p) (< p 1))
  (exp (+ (exc-calc--log-choose (+ failures successes -1) (1- successes))
          (* successes (log p)) (* failures (log (- 1 p))))))

(defun exc-calc--hypergeometric (k n found total)
  "Return the chance of K of N drawn being of the FOUND among TOTAL."
  (if (or (> k found) (> (- n k) (- total found))) 0.0
    (exp (- (+ (exc-calc--log-choose found k)
               (exc-calc--log-choose (- total found) (- n k)))
            (exc-calc--log-choose total n)))))

(exc-calc-define hypgeom.dist ((k integer) (n integer) (found integer) (total integer)
                                &optional (cumulative bool))
  "The chance of K of N drawn without putting back being among the FOUND
of TOTAL; or of at most K."
  (exc-calc--require (>= k 0) (<= k n) (<= k found) (<= 0 n total)
                      (<= 0 found total))
  (if cumulative
      (min 1.0 (cl-loop for i from 0 to k sum (exc-calc--hypergeometric i n found total)))
    (exc-calc--hypergeometric k n found total)))

(exc-calc-define hypgeomdist ((k integer) (n integer) (found integer) (total integer))
  "The chance of K of N drawn being among the FOUND of TOTAL."
  (exc-calc--require (>= k 0) (<= k n) (<= k found) (<= 0 n total) (<= 0 found total))
  (exc-calc--hypergeometric k n found total))

(exc-calc-define poisson.dist ((value number) (mean number) (cumulative bool))
  "The chance of VALUE events where MEAN are expected, or of at most that.
VALUE is cut to a whole number, and one below 0 is #NUM! before it is."
  (exc-calc--require (>= value 0) (>= mean 0))
  (let ((x (truncate value)))
    (cond ((zerop mean) (if (or cumulative (zerop x)) 1.0 0.0))
          (cumulative (exc-calc--gamma-q (+ x 1.0) mean))
          (t (exp (- (* x (log mean)) mean (exc-calc--log-gamma (+ x 1.0))))))))

(exc-calc-alias poisson poisson.dist)

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

(cl-defstruct (exc-calc--ets (:constructor exc-calc--ets-make))
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

(defun exc-calc--ets-month (serial)
  "Return SERIAL's month counted from year 0, and its day: (MONTHS DAY)."
  (pcase-let ((`(,year ,month ,day) (exc-calc-serial-date serial)))
    (list (+ (* 12 year) month) day)))

(defun exc-calc--ets-place (ets x)
  "Return where on ETS's timeline X falls, in months when it counts them."
  (let ((day (exc-calc--ets-month-day ets)))
    (if (zerop day)
        (float x)
      (pcase-let* ((`(,year ,month ,on) (exc-calc-serial-date x))
                   (length (cond ((memq month '(4 6 9 11)) 30)
                                 ((/= month 2) 31)
                                 ((exc-calc--leap-year-p year) 29)
                                 (t 28))))
        (+ (* 12.0 year) month (/ (- on day) (float length)))))))

(defun exc-calc--leap-year-p (year)
  "Return non-nil when YEAR has a 29 February."
  (and (zerop (% year 4)) (or (/= 0 (% year 100)) (zerop (% year 400)))))

(defun exc-calc--ets-aggregate (ys aggregation)
  "Return the one value of YS, values at one place, AGGREGATION says.
It is 1 AVERAGE, 2 COUNT, 3 COUNTA, 4 MAX, 5 MEDIAN, 6 MIN or 7 SUM."
  (pcase aggregation
    (1 (/ (apply #'+ ys) (float (length ys))))
    ((or 2 3) (length ys))
    (4 (apply #'max ys))
    (5 (exc-calc--median ys))
    (6 (apply #'min ys))
    (7 (apply #'+ ys))))

(defun exc-calc--ets-points (values timeline completion aggregation)
  "Return an ETS of the VALUES along TIMELINE, arrays of one size.
Points at one place become one by AGGREGATION, and a gap of whole steps
is filled with the mean of the points beside it, or with 0 when
COMPLETION is 0.  The step must be constant, gaps aside, and at most
three points in ten missing."
  (let* ((pairs (exc-calc--pairs timeline values))
         points)
    (pcase-dolist (`(,x . ,y) pairs)
      (dolist (v (list x y))
        (when (exc-calc-err-p v) (exc-calc-raise (exc-calc-err-code v))))
      (unless (exc-calc-blank-p y)
        (unless (and (numberp x) (numberp y)) (exc-calc-raise "#VALUE!"))
        (push (cons (float x) (float y)) points)))
    (when (< (length points) 3) (exc-calc-raise "#VALUE!"))
    (setq points (sort points (lambda (a b) (< (car a) (car b)))))
    (let* ((first (caar points))
           (month-day
            ;; Dates on one day of the month are a month apart, however
            ;; many days that is.
            (let ((day (cadr (exc-calc--ets-month first))))
              (if (cl-every (lambda (point)
                              (= (cadr (exc-calc--ets-month (car point))) day))
                            points)
                  day 0)))
           (points (if (zerop month-day) points
                     (mapcar (lambda (point)
                               (cons (float (car (exc-calc--ets-month (car point))))
                                     (cdr point)))
                             points)))
           (merged nil))
      ;; Points at one place.
      (while points
        (let* ((x (caar points))
               (same (cl-loop while (and points (= (caar points) x))
                              collect (cdr (pop points)))))
          (push (cons x (if (cdr same)
                            (exc-calc--ets-aggregate same aggregation)
                          (car same)))
                merged)))
      (setq merged (nreverse merged))
      (when (< (length merged) 3) (exc-calc-raise "#VALUE!"))
      (let* ((step (cl-loop for (a b) on merged while b
                            minimize (- (car b) (car a))))
             (count (length merged))
             (ys nil))
        ;; The step is constant, or gaps are whole steps.
        (cl-loop for (a b) on merged while b
                 for steps = (/ (- (car b) (car a)) (float step))
                 do (unless (= steps (ffloor steps)) (exc-calc-raise "#NUM!"))
                 do (push (cdr a) ys)
                 do (dotimes (_ (1- (truncate steps)))
                      (push (if (eql completion 0) 0.0 (/ (+ (cdr a) (cdr b)) 2))
                            ys)))
        (push (cdar (last merged)) ys)
        (setq ys (vconcat (nreverse ys)))
        (when (> (- (length ys) count) (* 0.3 count)) (exc-calc-raise "#NUM!"))
        (exc-calc--ets-make :ys ys :step step :first (caar merged)
                             :last (car (car (last merged)))
                             :month-day month-day)))))

(defun exc-calc--ets-period-length (ys)
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

(defun exc-calc--ets-start (ets)
  "Give ETS its level, trend and season at the first value."
  (let* ((ys (exc-calc--ets-ys ets))
         (n (length ys))
         (period (exc-calc--ets-period ets))
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
    (setf (exc-calc--ets-season ets) season
          (exc-calc--ets-level ets) (make-vector n 0.0)
          (exc-calc--ets-trend ets) (make-vector n 0.0)
          (exc-calc--ets-forecast ets) (make-vector n 0.0))
    (aset (exc-calc--ets-level ets) 0 (- (aref ys 0) (aref season 0)))
    (aset (exc-calc--ets-trend ets) 0 trend)
    (aset season period (aref season 0))
    (aset (exc-calc--ets-forecast ets) 0 (aref ys 0))))

(defun exc-calc--ets-smooth (ets)
  "Smooth ETS's values with its constants, and return the mean squared error."
  (let* ((ys (exc-calc--ets-ys ets)) (n (length ys))
         (period (exc-calc--ets-period ets))
         (alpha (exc-calc--ets-alpha ets))
         (beta (exc-calc--ets-beta ets))
         (gamma (exc-calc--ets-gamma ets))
         (level (exc-calc--ets-level ets))
         (trend (exc-calc--ets-trend ets))
         (season (exc-calc--ets-season ets))
         (forecast (exc-calc--ets-forecast ets))
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

(defun exc-calc--ets-search (ets setter inner)
  "Search for the constant SETTER sets of ETS that fits it best.
It halves 0 to 1 towards the end with the smaller error, to 0.001,
calling INNER after each try to fit the constants it leaves to it.
Return the error."
  (let ((try (lambda (value)
               (funcall setter value)
               (funcall inner)
               (exc-calc--ets-smooth ets))))
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

(defun exc-calc--ets-fit (ets)
  "Fit ETS's constants, smoothing level, then season, then trend."
  (exc-calc--ets-start ets)
  (let* ((trend (lambda ()
                  (exc-calc--ets-search
                   ets (lambda (v) (setf (exc-calc--ets-beta ets) v)) #'ignore)))
         (season (if (zerop (exc-calc--ets-period ets))
                     (lambda ()
                       (setf (exc-calc--ets-gamma ets) 0.0)
                       (funcall trend))
                   (lambda ()
                     (exc-calc--ets-search
                      ets (lambda (v) (setf (exc-calc--ets-gamma ets) v)) trend)))))
    (exc-calc--ets-search
     ets (lambda (v) (setf (exc-calc--ets-alpha ets) v)) season))
  ets)

(defvar exc-calc--ets-last nil
  "The arguments and the fit of the last values smoothed, (KEY . ETS).
Formulas filled down one column smooth the same values again and again.")

(defun exc-calc--ets (values timeline seasonality completion aggregation)
  "Return an ETS fit to VALUES along TIMELINE.
SEASONALITY is 1 to find the season's length, 0 for no season, or the
length; COMPLETION and AGGREGATION are as `exc-calc--ets-points' takes."
  (unless (and (= seasonality (floor seasonality)) (<= 0 seasonality 8760))
    (exc-calc-raise "#NUM!"))
  (unless (memql completion '(0 1 0.0 1.0)) (exc-calc-raise "#NUM!"))
  (unless (and (= aggregation (floor aggregation)) (<= 1 aggregation 7))
    (exc-calc-raise "#NUM!"))
  (let ((key (list (exc-calc-array-values values)
                   (exc-calc-array-values timeline)
                   seasonality completion aggregation
                   (exc-format-date-offset))))
    (if (equal key (car exc-calc--ets-last))
        (cdr exc-calc--ets-last)
      (let* ((ets (exc-calc--ets-points values timeline completion
                                         (truncate aggregation)))
             (ys (exc-calc--ets-ys ets))
             (period (pcase (truncate seasonality)
                       (1 (exc-calc--ets-period-length ys))
                       (length length))))
        (when (> (* 2 period) (length ys)) (exc-calc-raise "#NUM!"))
        (setf (exc-calc--ets-period ets) (if (<= period 1) 0 period))
        (exc-calc--ets-fit ets)
        (setq exc-calc--ets-last (cons key ets))
        ets))))

(defun exc-calc--ets-steps (ets target)
  "Return how many steps past ETS's last value TARGET is, or #NUM!."
  (let ((steps (/ (- (exc-calc--ets-place ets target) (exc-calc--ets-last ets))
                  (exc-calc--ets-step ets))))
    (when (< steps 0) (exc-calc-raise "#NUM!"))
    steps))

(defun exc-calc--ets-interpolate (function steps)
  "Return FUNCTION at STEPS, a line between the whole steps beside it."
  (let* ((whole (floor steps))
         (at (funcall function whole))
         (fraction (- steps whole)))
    (if (< fraction 0.001) at
      (+ at (* fraction (- (funcall function (1+ whole)) at))))))

(defun exc-calc--ets-ahead (ets steps)
  "Return the value ETS foretells STEPS whole steps past its last value."
  (let* ((n (length (exc-calc--ets-ys ets)))
         (period (exc-calc--ets-period ets)))
    (+ (aref (exc-calc--ets-level ets) (1- n))
       (* steps (aref (exc-calc--ets-trend ets) (1- n)))
       (if (zerop period) 0.0
         (aref (exc-calc--ets-season ets) (+ n (mod (1- steps) period)))))))

(defun exc-calc--ets-errors (ets)
  "Return the forecast errors of ETS's values past the first."
  (let ((ys (exc-calc--ets-ys ets)) (forecast (exc-calc--ets-forecast ets)))
    (cl-loop for i from 1 below (length ys)
             collect (- (aref forecast i) (aref ys i)))))

(defun exc-calc--ets-rmse (ets)
  "Return the root mean squared error of ETS's forecasts."
  (let ((errors (exc-calc--ets-errors ets)))
    (sqrt (/ (cl-loop for e in errors sum (* e e)) (length errors)))))

(exc-calc-define forecast.ets ((target number) (values array) (timeline array)
                                &optional (seasonality number 1)
                                (completion number 1) (aggregation number 1))
  "The value at TARGET of VALUES along TIMELINE, smoothed.
See `exc-calc--ets' for SEASONALITY, COMPLETION and AGGREGATION."
  :empty t
  (let ((ets (exc-calc--ets values timeline seasonality completion aggregation)))
    (exc-calc--ets-interpolate (lambda (steps) (exc-calc--ets-ahead ets steps))
                                (exc-calc--ets-steps ets target))))

(exc-calc-define forecast.ets.confint ((target number) (values array)
                                        (timeline array)
                                        &optional (level number 0.95)
                                        (seasonality number 1)
                                        (completion number 1)
                                        (aggregation number 1))
  "Half the width of the interval FORECAST.ETS's value at TARGET falls in.
It does so with the chance LEVEL, between 0 and 1."
  :empty t
  (unless (< 0 level 1) (exc-calc-raise "#NUM!"))
  (let* ((ets (exc-calc--ets values timeline seasonality completion aggregation))
         (alpha (exc-calc--ets-alpha ets))
         (beta (exc-calc--ets-beta ets))
         (gamma (exc-calc--ets-gamma ets))
         (period (exc-calc--ets-period ets))
         (width (* (exc-calc--normal-inverse (/ (+ 1 level) 2))
                   (exc-calc--ets-rmse ets))))
    (exc-calc--ets-interpolate
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
     (exc-calc--ets-steps ets target))))

(exc-calc-define forecast.ets.seasonality ((values array) (timeline array)
                                            &optional (completion number 1)
                                            (aggregation number 1))
  "How many steps the season FORECAST.ETS finds in VALUES lasts, 0 for none."
  :empty t
  (exc-calc--ets-period (exc-calc--ets values timeline 1 completion aggregation)))

(exc-calc-define forecast.ets.stat ((values array) (timeline array) (statistic number)
                                     &optional (seasonality number 1)
                                     (completion number 1)
                                     (aggregation number 1))
  "A measure of how FORECAST.ETS smooths VALUES, STATISTIC saying which.
It is 1 alpha, 2 beta, 3 gamma, the constants smoothing the level, trend
and season; 4 MASE, 5 SMAPE, 6 MAE and 7 RMSE, measures of the
forecasts' errors; or 8 the timeline's step."
  :empty t
  (let* ((ets (exc-calc--ets values timeline seasonality completion aggregation))
         (ys (exc-calc--ets-ys ets))
         (forecast (exc-calc--ets-forecast ets))
         (errors (exc-calc--ets-errors ets))
         (count (length errors))
         (mae (/ (cl-loop for e in errors sum (abs e)) count)))
    (pcase (truncate statistic)
      (1 (exc-calc--ets-alpha ets))
      (2 (exc-calc--ets-beta ets))
      (3 (exc-calc--ets-gamma ets))
      (4 (let ((rises (cl-loop for i from 2 below (length ys)
                               sum (abs (- (aref ys i) (aref ys (1- i)))))))
           (if (zerop rises) (exc-calc-raise "#DIV/0!")
             (/ mae (/ rises (1- count))))))
      (5 (* (/ 2.0 count)
            (cl-loop for i from 1 below (length ys)
                     for e in errors
                     for sum = (+ (abs (aref forecast i)) (abs (aref ys i)))
                     unless (zerop sum) sum (/ (abs e) sum))))
      (6 mae)
      (7 (exc-calc--ets-rmse ets))
      (8 (exc-calc--ets-step ets))
      (_ (exc-calc-raise "#NUM!")))))

(provide 'exc-calc-dist)

;;; exc-calc-dist.el ends here
