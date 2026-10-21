;;; exc-calc-math.el --- Math and trigonometry functions -*- lexical-binding: t -*-

;;; Commentary:

;; Sums and products, rounding, powers and logarithms, trigonometry,
;; matrices, and the criteria SUMIF and COUNTIF and their kin match.

;;; Code:

(require 'exc-calc)
(require 'exc-calc-lookup)

;;;----------------------------------------------------------------------
;;; Numbers as Excel keeps them
;;;----------------------------------------------------------------------

(defun exc-calc--snap (number)
  "Return NUMBER kept to the 15 significant digits Excel keeps.
So 0.1*3 is 0.3, and rounding 2.675 or taking the ceiling of 0.3*10
sees what was written rather than the nearest binary fraction."
  (if (and (floatp number) (not (zerop number))
           (not (isnan number)) (/= (abs number) 1.0e+INF))
      (float (string-to-number (format "%.15g" number)))
    number))

(defun exc-calc--finite (number)
  "Return NUMBER, or signal #NUM! when it is too large or not a number."
  (if (and (floatp number)
           (or (isnan number) (> (abs number) 1.7976931348623157e308)))
      (exc-calc-raise "#NUM!")
    (exc-calc--tidy-number number)))

(defun exc-calc--round-to (number digits how)
  "Return NUMBER rounded to DIGITS places after the point, by HOW.
HOW is `nearest', halves going away from zero, `up', away from zero, or
`down', toward zero.  Negative DIGITS rounds left of the point."
  (let* ((scale (expt 10.0 (abs digits)))
         (scaled (if (>= digits 0) (* number scale) (/ number scale))))
    (cond
     ;; DIGITS past what a float holds: to the right of the point the
     ;; number has no digits to cut off, to the left it has none left.
     ((not (< (abs scale) 1.0e+INF)) (if (< digits 0) 0 number))
     ;; The same for a number too big to scale: it has no digits at the
     ;; places wanted either.
     ((not (< (abs scaled) 1.0e+INF)) number)
     (t (let* ((scaled (exc-calc--snap scaled))
               (magnitude (abs scaled))
               (rounded (pcase how
                          ('nearest (ffloor (+ magnitude 0.5)))
                          ('up (fceiling magnitude))
                          ('down (ffloor magnitude))))
               (signed (if (< scaled 0) (- rounded) rounded)))
          (exc-calc--tidy-number
           (exc-calc--snap (if (>= digits 0) (/ signed scale)
                              (* signed scale)))))))))

(exc-calc-define round ((number number) &optional (digits integer 0))
  "NUMBER rounded to DIGITS places, halves away from zero."
  (exc-calc--round-to number digits 'nearest))

(exc-calc-define roundup ((number number) &optional (digits integer 0))
  "NUMBER rounded away from zero to DIGITS places."
  (exc-calc--round-to number digits 'up))

(exc-calc-define rounddown ((number number) &optional (digits integer 0))
  "NUMBER rounded toward zero to DIGITS places."
  (exc-calc--round-to number digits 'down))

(exc-calc-define trunc ((number number) &optional (digits number 0))
  "NUMBER with its digits past DIGITS places cut off.
DIGITS is cut to a whole number, as ROUND cuts it, and one too big for
a float to count leaves the number as it is."
  (exc-calc--round-to number (truncate digits) 'down))

(exc-calc-define int ((number number))
  "NUMBER rounded down to a whole number."
  (exc-calc--tidy-number (ffloor (exc-calc--snap (float number)))))

(defun exc-calc--to-multiple (number step how)
  "Return NUMBER rounded to a multiple of STEP, by HOW.
HOW is `nearest', `up' or `down'; up and down go along the number line,
toward plus and minus infinity."
  (if (zerop step) 0
    (let* ((quotient (exc-calc--snap (/ (float number) step)))
           (whole (pcase how
                    ('nearest (if (< quotient 0) (- (ffloor (+ (- quotient) 0.5)))
                                (ffloor (+ quotient 0.5))))
                    ('up (fceiling quotient))
                    ('down (ffloor quotient)))))
      (exc-calc--tidy-number (exc-calc--snap (* whole step))))))

(exc-calc-define mround ((number number) (multiple number))
  "NUMBER rounded to the nearest multiple of MULTIPLE."
  :toolpak t
  (when (and (/= number 0) (/= multiple 0) (/= (cl-signum number) (cl-signum multiple)))
    (exc-calc-raise "#NUM!"))
  (exc-calc--to-multiple number multiple 'nearest))

(exc-calc-define ceiling ((number number) &optional (significance number 1))
  "NUMBER rounded away from zero to a multiple of SIGNIFICANCE.
A negative NUMBER with a positive SIGNIFICANCE rounds toward zero."
  (cond ((zerop significance) 0)
        ((and (> number 0) (< significance 0)) (exc-calc-raise "#NUM!"))
        ((< significance 0)
         (- (exc-calc--to-multiple (- number) (- significance) 'up)))
        (t (exc-calc--to-multiple number significance 'up))))

(exc-calc-define floor ((number number) &optional (significance number 1))
  "NUMBER rounded toward zero to a multiple of SIGNIFICANCE.
A negative NUMBER with a positive SIGNIFICANCE rounds away from zero."
  (cond ((and (zerop significance) (zerop number)) 0)
        ((zerop significance) (exc-calc-raise "#DIV/0!"))
        ((and (> number 0) (< significance 0)) (exc-calc-raise "#NUM!"))
        ((< significance 0)
         (- (exc-calc--to-multiple (- number) (- significance) 'down)))
        (t (exc-calc--to-multiple number significance 'down))))

(exc-calc-define ceiling.math ((number number) &optional (significance number 1)
                                (mode number 0))
  "NUMBER rounded up to a multiple of SIGNIFICANCE.
A negative NUMBER rounds toward zero, or away from it when MODE is not 0."
  (let ((step (abs significance)))
    (if (and (< number 0) (/= mode 0))
        (- (exc-calc--to-multiple (- number) step 'up))
      (exc-calc--to-multiple number step 'up))))

(exc-calc-define floor.math ((number number) &optional (significance number 1)
                              (mode number 0))
  "NUMBER rounded down to a multiple of SIGNIFICANCE.
A negative NUMBER rounds away from zero, or toward it when MODE is not 0."
  (let ((step (abs significance)))
    (if (and (< number 0) (/= mode 0))
        (- (exc-calc--to-multiple (- number) step 'down))
      (exc-calc--to-multiple number step 'down))))

(exc-calc-define ceiling.precise ((number number) &optional (significance number 1))
  "NUMBER rounded up to a multiple of SIGNIFICANCE, whatever its sign."
  (exc-calc--to-multiple number (abs significance) 'up))

(exc-calc-alias iso.ceiling ceiling.precise)

(exc-calc-define floor.precise ((number number) &optional (significance number 1))
  "NUMBER rounded down to a multiple of SIGNIFICANCE, whatever its sign."
  (exc-calc--to-multiple number (abs significance) 'down))

(defun exc-calc--round-to-parity (number odd)
  "Return NUMBER rounded away from zero to an even integer, or ODD one."
  (let* ((magnitude (fceiling (exc-calc--snap (abs (float number)))))
         (whole (truncate magnitude))
         (whole (if (eq (cl-oddp whole) odd) whole (1+ whole))))
    (if (< number 0) (- whole) whole)))

(exc-calc-define even ((number number))
  "NUMBER rounded away from zero to an even whole number."
  (exc-calc--round-to-parity number nil))

(exc-calc-define odd ((number number))
  "NUMBER rounded away from zero to an odd whole number."
  (exc-calc--round-to-parity number t))

;;;----------------------------------------------------------------------
;;; Arithmetic
;;;----------------------------------------------------------------------

(exc-calc-define abs ((number number))
  "NUMBER without its sign."
  (abs number))

(exc-calc-define sign ((number number))
  "1 for a positive NUMBER, -1 for a negative one, 0 for 0."
  (cond ((> number 0) 1) ((< number 0) -1) (t 0)))

(exc-calc-define mod ((number number) (divisor number))
  "The remainder of NUMBER divided by DIVISOR, with the sign of DIVISOR."
  (when (zerop divisor) (exc-calc-raise "#DIV/0!"))
  (exc-calc--tidy-number
   (exc-calc--snap (- number (* divisor (ffloor (exc-calc--snap
                                                   (/ (float number) divisor))))))))

(exc-calc-define power ((base number) (exponent number))
  "BASE to the power EXPONENT."
  (exc-calc--power base exponent))

(exc-calc-define sqrt ((number number))
  "The square root of NUMBER."
  (when (< number 0) (exc-calc-raise "#NUM!"))
  (exc-calc--tidy-number (sqrt number)))

(exc-calc-define exp ((number number))
  "e to the power NUMBER."
  (exc-calc--finite (exp number)))

(exc-calc-define ln ((number number))
  "The natural logarithm of NUMBER."
  (when (<= number 0) (exc-calc-raise "#NUM!"))
  (exc-calc--tidy-number (log number)))

(exc-calc-define log ((number number) &optional (base number 10))
  "The logarithm of NUMBER to BASE."
  (when (or (<= number 0) (<= base 0)) (exc-calc-raise "#NUM!"))
  (when (= base 1) (exc-calc-raise "#DIV/0!"))
  (exc-calc--tidy-number (exc-calc--snap (/ (log number) (log base)))))

(exc-calc-define log10 ((number number))
  "The logarithm of NUMBER to base 10."
  (when (<= number 0) (exc-calc-raise "#NUM!"))
  (exc-calc--tidy-number (exc-calc--snap (log number 10))))

(exc-calc-define pi ()
  "The number pi."
  float-pi)

(exc-calc-define fact ((number number))
  "The product of the whole numbers from 1 to NUMBER."
  (when (< number 0) (exc-calc-raise "#NUM!"))
  (let ((product 1.0))
    (cl-loop for i from 2 to (truncate number) do (setq product (* product i)))
    (exc-calc--finite product)))

(exc-calc-define combin ((number integer) (chosen integer))
  "How many ways there are to choose CHOSEN of NUMBER things."
  (when (or (< number 0) (< chosen 0) (< number chosen))
    (exc-calc-raise "#NUM!"))
  (let ((result 1.0))
    (cl-loop for i from 1 to (min chosen (- number chosen))
             do (setq result (/ (* result (- number (- i 1))) i)))
    (exc-calc--tidy-number (fround result))))

(exc-calc-define rand ()
  "A random number from 0 up to 1."
  (/ (random most-positive-fixnum) (float most-positive-fixnum)))

(defun exc-calc--random (min max whole)
  "Return a random number from MIN up to MAX, a whole one with WHOLE."
  (if whole
      (+ (truncate min)
         (random (max 1 (1+ (- (truncate max) (truncate min))))))
    (+ min (* (- max min) (/ (random most-positive-fixnum)
                             (float most-positive-fixnum))))))

(exc-calc-define randarray (&optional (rows integer 1) (cols integer 1)
                            (min number 0) (max number 1) (whole bool))
  "ROWS by COLS random numbers from MIN up to MAX, whole ones with WHOLE."
  :empty t
  (when (or (< rows 1) (< cols 1) (> min max)) (exc-calc-raise "#VALUE!"))
  (exc-calc-array rows cols
                   (lambda (_row _col) (exc-calc--random min max whole))))

(exc-calc-define base ((number integer) (radix integer) &optional (length integer 0))
  "NUMBER written in base RADIX, padded with zeros to LENGTH digits."
  (unless (and (>= number 0) (<= 2 radix 36) (>= length 0))
    (exc-calc-raise "#NUM!"))
  (let ((digits "") (n number))
    (while (> n 0)
      (setq digits (concat (string (aref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                         (% n radix)))
                           digits)
            n (/ n radix)))
    (string-pad (if (string-empty-p digits) "0" digits) length ?0 t)))

(exc-calc-define sqrtpi ((number number))
  "The square root of NUMBER times pi."
  :toolpak t
  (when (< number 0) (exc-calc-raise "#NUM!"))
  (sqrt (* number float-pi)))

(exc-calc-define factdouble ((number number))
  "The product of every other whole number from NUMBER down to 1."
  :toolpak t
  (when (< number -1) (exc-calc-raise "#NUM!"))
  (let ((n (truncate number)) (product 1.0))
    (while (> n 1) (setq product (* product n) n (- n 2)))
    (exc-calc--finite product)))

(exc-calc-define quotient ((number number) (divisor number))
  "The whole part of NUMBER divided by DIVISOR."
  :toolpak t
  (when (zerop divisor) (exc-calc-raise "#DIV/0!"))
  (exc-calc--tidy-number (ftruncate (/ (float number) divisor))))

(exc-calc-define randbetween ((low number) (high number))
  "A random whole number from LOW to HIGH."
  (let ((low (ceiling low)) (high (floor high)))
    (when (> low high) (exc-calc-raise "#NUM!"))
    (+ low (random (1+ (- high low))))))

(defun exc-calc--toolpak-numbers (arguments)
  "Return the numbers of ARGUMENTS, `values' arguments, as GCD reads them.
Text that reads as a number counts in a range or array too; other text,
TRUE and FALSE are #VALUE! wherever they are.  A blank cell is 0 in a
range and left out as an argument; an argument left out is #N/A.  With
no number at all, a range of nothing but cells past the sheet's filled
ones gives 0, and arguments that are none #VALUE!."
  (let (numbers ranges)
    (dolist (argument arguments)
      (when (eq argument :blank) (exc-calc-raise "#N/A"))
      ;; One cell is read as written directly.
      (let ((one (exc-calc--deref argument)))
        (unless (exc-calc-array-p one) (setq argument one)))
      (when (or (exc-calc-ref-p argument) (exc-calc-array-p argument)
                (consp argument))
        (setq ranges t))
      (exc-calc-each-value
       argument
       (lambda (value direct)
         (cond
          ((numberp value) (push value numbers))
          ((exc-calc-err-p value) (exc-calc-raise (exc-calc-err-code value)))
          ((eq value :blank) (unless direct (push 0 numbers)))
          ((and (stringp value) (exc-calc-text-number value))
           (push (exc-calc-text-number value) numbers))
          (t (exc-calc-raise "#VALUE!"))))))
    (cond (numbers (nreverse numbers))
          (ranges '(0))
          (t (exc-calc-raise "#VALUE!")))))

(defun exc-calc--whole-numbers (arguments)
  "Return the numbers of ARGUMENTS with their fractions cut off.
#NUM! for one below 0 or from 2^53."
  (mapcar (lambda (n)
            (unless (and (<= 0 n) (< n (expt 2.0 53))) (exc-calc-raise "#NUM!"))
            (truncate n))
          (exc-calc--toolpak-numbers arguments)))

(exc-calc-define gcd (&rest (numbers values))
  "The greatest whole number that divides each of NUMBERS."
  (let ((result 0))
    (dolist (n (exc-calc--whole-numbers numbers) result)
      (setq result (cl-gcd result n)))))

(exc-calc-define lcm (&rest (numbers values))
  "The least whole number each of NUMBERS divides; 0 when one of them is."
  (let ((result 1))
    (dolist (n (exc-calc--whole-numbers numbers) result)
      (setq result (if (or (zerop n) (zerop result)) 0 (cl-lcm result n)))
      ;; Past 2^53 a double cannot hold it.
      (when (>= result (expt 2 53)) (exc-calc-raise "#NUM!")))))

(exc-calc-define multinomial (&rest (numbers values))
  "The factorial of the sum of NUMBERS over the product of their factorials."
  (let ((result 1.0) (sum 0))
    (dolist (n (exc-calc--whole-numbers numbers))
      ;; Each step multiplies by the binomial coefficient of SUM + N over N.
      (cl-loop for i from 1 to n
               do (setq result (/ (* result (+ sum i)) i)))
      (setq sum (+ sum n)))
    (exc-calc--finite (fround result))))

(exc-calc-define permut ((number integer) (chosen integer))
  "How many ways there are to arrange CHOSEN of NUMBER things."
  (when (or (< number 0) (< chosen 0) (< number chosen))
    (exc-calc-raise "#NUM!"))
  (let ((result 1.0))
    (cl-loop for i from 0 below chosen do (setq result (* result (- number i))))
    (exc-calc--finite result)))

(exc-calc-define permutationa ((number integer) (chosen integer))
  "How many ways there are to arrange CHOSEN of NUMBER things, repeating."
  (when (or (< number 0) (< chosen 0)) (exc-calc-raise "#NUM!"))
  (exc-calc--finite (expt (float number) chosen)))

(exc-calc-define combina ((number integer) (chosen integer))
  "How many ways there are to choose CHOSEN of NUMBER things, repeating."
  (when (or (< number 0) (< chosen 0) (and (zerop number) (> chosen 0)))
    (exc-calc-raise "#NUM!"))
  (let ((result 1.0) (n (+ number chosen -1)))
    (cl-loop for i from 1 to chosen
             do (setq result (/ (* result (- n (- i 1))) i)))
    (exc-calc--tidy-number (fround result))))

(exc-calc-define seriessum ((x number) (n number) (m number) (coefficients values))
  "The sum of each of COEFFICIENTS times X to N, N + M, N + 2M and so on."
  :toolpak t
  (let ((power n) (sum 0.0))
    (dolist (a (exc-calc-numbers (list coefficients)))
      (when (and (zerop x) (<= power 0))
        (exc-calc-raise (if (zerop power) "#NUM!" "#DIV/0!")))
      (setq sum (+ sum (* a (expt (float x) power))) power (+ power m)))
    (exc-calc--finite sum)))

(exc-calc-define munit ((size integer))
  "The SIZE by SIZE unit matrix."
  (when (< size 1) (exc-calc-raise "#VALUE!"))
  (exc-calc-array size size (lambda (row col) (if (= row col) 1 0))))

(defconst exc-calc--roman-digits
  '((?M . 1000) (?D . 500) (?C . 100) (?L . 50) (?X . 10) (?V . 5) (?I . 1))
  "The Roman digits, largest first, with their values.")

(exc-calc-define roman ((number number) &optional (form any 0))
  "NUMBER in Roman digits.  FORM 0, the default, is the classic form;
1 to 4 are ever shorter, as FORM TRUE is 0 and FALSE 4."
  (let ((form (cond ((eq form t) 0) ((eq form :false) 4)
                    ((exc-calc-blank-p form) 0)
                    (t (truncate (exc-calc-num form)))))
        (value (truncate number))
        (chars (vconcat (mapcar #'car exc-calc--roman-digits)))
        (values (vconcat (mapcar #'cdr exc-calc--roman-digits)))
        (digits '()))
    (unless (and (<= 0 value 3999) (<= 0 form 4)) (exc-calc-raise "#VALUE!"))
    ;; As LibreOffice's ROMAN: each of M, C, X and I in turn, a 4 or 9 of
    ;; it written by taking from a larger digit, as far as FORM allows.
    (dotimes (i 4)
      (let* ((index (* 2 i))
             (digit (/ value (aref values index))))
        (if (= (% digit 5) 4)
            (let ((index2 (if (= digit 4) (1- index) (- index 2)))
                  (steps 0))
              (while (and (< steps form) (< index 6))
                (setq steps (1+ steps))
                (if (<= (- (aref values index2) (aref values (1+ index))) value)
                    (setq index (1+ index))
                  (setq steps form)))
              (push (aref chars index) digits)
              (push (aref chars index2) digits)
              (setq value (- (+ value (aref values index)) (aref values index2))))
          (when (> digit 4) (push (aref chars (1- index)) digits))
          (dotimes (_ (% digit 5)) (push (aref chars index) digits))
          (setq value (% value (aref values index))))))
    (concat (nreverse digits))))

(exc-calc-define arabic ((text text))
  "The number TEXT writes in Roman digits, which may start with -."
  (let* ((text (string-trim text))
         (negative (string-prefix-p "-" text))
         (text (upcase (if negative (substring text 1) text)))
         (total 0) (previous 0))
    (when (> (length text) 255) (exc-calc-raise "#VALUE!"))
    (cl-loop for i downfrom (1- (length text)) to 0
             do (let ((value (alist-get (aref text i) exc-calc--roman-digits)))
                  (unless value (exc-calc-raise "#VALUE!"))
                  (if (< value previous) (setq total (- total value))
                    (setq total (+ total value) previous value))))
    (if negative (- total) total)))

;;;----------------------------------------------------------------------
;;; Trigonometry
;;;----------------------------------------------------------------------

(defmacro exc-calc--define-math (name doc function &optional domain)
  "Define NAME, DOC, giving FUNCTION of its one number.
DOMAIN, a function of the number, makes it #NUM! when it does not hold."
  `(exc-calc-define ,name ((number number))
     ,doc
     ,@(and domain `((unless (funcall ,domain number) (exc-calc-raise "#NUM!"))))
     (exc-calc--finite (funcall ,function (float number)))))

(exc-calc--define-math sin "The sine of NUMBER, an angle in radians." #'sin)
(exc-calc--define-math cos "The cosine of NUMBER, an angle in radians." #'cos)
(exc-calc--define-math tan "The tangent of NUMBER, an angle in radians." #'tan)
(exc-calc--define-math asin "The angle whose sine is NUMBER." #'asin
                        (lambda (x) (<= -1 x 1)))
(exc-calc--define-math acos "The angle whose cosine is NUMBER." #'acos
                        (lambda (x) (<= -1 x 1)))
(exc-calc--define-math atan "The angle whose tangent is NUMBER." #'atan)
(exc-calc--define-math sinh "The hyperbolic sine of NUMBER."
                        (lambda (x) (exc-calc--sinh x)))
(exc-calc--define-math cosh "The hyperbolic cosine of NUMBER."
                        (lambda (x) (/ (+ (exp x) (exp (- x))) 2)))
(exc-calc--define-math tanh "The hyperbolic tangent of NUMBER."
                        (lambda (x) (if (> (abs x) 20) (cl-signum x)
                                      (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1)))))
(exc-calc--define-math asinh "The number whose hyperbolic sine is NUMBER."
                        (lambda (x) (log (+ x (sqrt (1+ (* x x)))))))
(exc-calc--define-math acosh "The number whose hyperbolic cosine is NUMBER."
                        (lambda (x) (log (+ x (sqrt (1- (* x x))))))
                        (lambda (x) (>= x 1)))
(exc-calc--define-math atanh "The number whose hyperbolic tangent is NUMBER."
                        (lambda (x) (/ (log (/ (+ 1 x) (- 1 x))) 2))
                        (lambda (x) (< -1 x 1)))
(exc-calc--define-math radians "NUMBER degrees in radians."
                        (lambda (x) (/ (* x float-pi) 180)))
(exc-calc--define-math degrees "NUMBER radians in degrees."
                        (lambda (x) (/ (* x 180) float-pi)))

(defun exc-calc--within-trig-p (x)
  "Return non-nil when X is small enough for SEC, CSC and COT."
  (< (abs x) (expt 2.0 27)))

(defun exc-calc--reciprocal (x)
  "Return 1 / X, #DIV/0! for 0."
  (if (zerop x) (exc-calc-raise "#DIV/0!") (/ 1.0 x)))

(exc-calc--define-math sec "The secant of NUMBER, an angle in radians."
                        (lambda (x) (/ 1.0 (cos x))) #'exc-calc--within-trig-p)
(exc-calc--define-math csc "The cosecant of NUMBER, an angle in radians."
                        (lambda (x) (exc-calc--reciprocal (sin x)))
                        #'exc-calc--within-trig-p)
(exc-calc--define-math cot "The cotangent of NUMBER, an angle in radians."
                        (lambda (x) (exc-calc--reciprocal (tan x)))
                        #'exc-calc--within-trig-p)
(exc-calc--define-math sech "The hyperbolic secant of NUMBER."
                        (lambda (x) (if (> (abs x) 710) 0.0
                                      (/ 2 (+ (exp x) (exp (- x)))))))

(defun exc-calc--sinh (x)
  "Return the hyperbolic sine of X with all its digits.
Below 1 it sums the series, where the two exponentials would cancel."
  (if (>= (abs x) 1) (/ (- (exp x) (exp (- x))) 2)
    (let ((sum (float x)) (term (float x)) (k 1))
      (while (/= (+ sum (setq term (/ (* term x x) (* (1+ k) (+ k 2))))) sum)
        (setq sum (+ sum term) k (+ k 2)))
      sum)))

(exc-calc--define-math csch "The hyperbolic cosecant of NUMBER."
                        (lambda (x) (if (> (abs x) 710) 0.0
                                      (exc-calc--reciprocal (exc-calc--sinh x)))))
(exc-calc--define-math coth "The hyperbolic cotangent of NUMBER."
                        (lambda (x)
                          (cond ((> (abs x) 20) (cl-signum x))
                                ((< (abs x) 1)
                                 (* (/ (+ (exp x) (exp (- x))) 2)
                                    (exc-calc--reciprocal (exc-calc--sinh x))))
                                (t (exc-calc--reciprocal
                                    (/ (- (exp (* 2 x)) 1)
                                       (+ (exp (* 2 x)) 1)))))))
(exc-calc--define-math acot "The angle whose cotangent is NUMBER."
                        (lambda (x) (- (/ float-pi 2) (atan x))))
(exc-calc--define-math acoth "The number whose hyperbolic cotangent is NUMBER."
                        (lambda (x) (/ (log (/ (+ x 1) (- x 1))) 2))
                        (lambda (x) (> (abs x) 1)))

(exc-calc-define atan2 ((x number) (y number))
  "The angle of the point X, Y from the x axis."
  (when (and (zerop x) (zerop y)) (exc-calc-raise "#DIV/0!"))
  (atan (float y) x))

;;;----------------------------------------------------------------------
;;; Sums and products
;;;----------------------------------------------------------------------

(defun exc-calc--add (numbers)
  "Return the sum of NUMBERS, kept to Excel's 15 digits."
  (exc-calc--tidy-number (apply #'+ 0 numbers)))

(exc-calc-define sum (&rest (numbers values))
  "The sum of NUMBERS."
  (exc-calc--add (exc-calc-numbers numbers)))

(exc-calc-define percentof ((part values) (whole values))
  "The sum of PART as a fraction of the sum of WHOLE.
Only numbers count, and an error value in either is #NUM!."
  :errors t
  (when (cl-some #'exc-calc-err-p
                 (append (exc-calc-flat-values part)
                         (exc-calc-flat-values whole)))
    (exc-calc-raise "#NUM!"))
  (let ((whole (apply #'+ 0 (exc-calc-numbers (list whole)))))
    (when (zerop whole) (exc-calc-raise "#DIV/0!"))
    (/ (apply #'+ 0.0 (exc-calc-numbers (list part))) whole)))

(exc-calc-define product (&rest (numbers values))
  "The product of NUMBERS; 0 when there are none."
  (let ((numbers (exc-calc-numbers numbers)))
    (if numbers (exc-calc--finite (exc-calc--snap (apply #'* 1 numbers))) 0)))

(exc-calc-define sumsq (&rest (numbers values))
  "The sum of the squares of NUMBERS.
A sum too big for a float to hold is #NUM!, as IronCalc's SUMSQ
workbook has the square of 2E+154."
  (let ((total (exc-calc--add (mapcar (lambda (n) (* n n))
                                       (exc-calc-numbers numbers)))))
    (if (> (abs total) 1.7976931348623157e308)
        (exc-calc-raise "#NUM!")
      total)))

(defun exc-calc--pairs (left right)
  "Return the values of arrays LEFT and RIGHT, which must hold as many.
They are a list of (L . R) pairs, in reading order; the shapes may
differ, as a column of three pairs with a row of three."
  (let ((lefts (exc-calc-array-values left))
        (rights (exc-calc-array-values right)))
    (unless (= (length lefts) (length rights))
      (exc-calc-raise "#N/A"))
    (cl-mapcar #'cons lefts rights)))

(defun exc-calc--sum-pairs (fn xs ys)
  "Return the sum of FN over the pairs of numbers of XS and YS.
With no such pair it is #DIV/0!, as in Excel."
  (let ((pairs (exc-calc--number-pairs xs ys)))
    (unless pairs (exc-calc-raise "#DIV/0!"))
    (exc-calc--add (mapcar (lambda (p) (funcall fn (car p) (cdr p))) pairs))))

(defun exc-calc--raise-first-error (&rest arrays)
  "Raise the first error in ARRAYS, all of the first before the next."
  (dolist (array arrays)
    (when-let* ((error (cl-find-if #'exc-calc-err-p
                                   (exc-calc-array-values array))))
      (exc-calc-raise (exc-calc-err-code error)))))

(defun exc-calc--raise-pair-error (left right &optional apart)
  "Raise the first error in arrays LEFT and RIGHT.
They are gone through a place at a time, an error of LEFT there before
one of RIGHT; APART, or arrays of different sizes, all of LEFT first.
#NULL! is passed over while there is another error, as Excel does in
functions of pairs."
  (let* ((lefts (exc-calc-array-values left))
         (rights (exc-calc-array-values right))
         (errors (cl-remove-if-not
                  #'exc-calc-err-p
                  (if (or apart (/= (length lefts) (length rights)))
                      (append lefts rights)
                    (cl-mapcar (lambda (l r) (if (exc-calc-err-p l) l r))
                               lefts rights)))))
    (when errors
      (exc-calc-raise
       (exc-calc-err-code
        (or (cl-find-if-not (lambda (e) (equal (exc-calc-err-code e) "#NULL!"))
                            errors)
            (car errors)))))))

(defun exc-calc--number-pairs (left right)
  "Return the pairs of LEFT and RIGHT that are both numbers, as (L . R).
Arrays of different sizes are #N/A before an error in them is raised,
as `exc-calc--raise-pair-error' does."
  (let ((pairs (exc-calc--pairs left right)))
    (exc-calc--raise-pair-error left right)
    (cl-remove-if-not (lambda (pair)
                        (and (numberp (car pair)) (numberp (cdr pair))))
                      pairs)))

(exc-calc-define sumproduct ((first array) &rest (others array))
  "The sum of the products of the values in one place of each array.
Values that are not numbers count as 0, but text is #VALUE! when every
array is one value, as Excel takes them then.  Arrays of different
shapes give #VALUE!, after the first error in them."
  (let* ((arrays (cons first others))
         (rows (exc-calc-array-rows first))
         (cols (exc-calc-array-cols first))
         (single (and (= rows 1) (= cols 1))))
    (dolist (array others)
      (unless (and (= (exc-calc-array-rows array) rows)
                   (= (exc-calc-array-cols array) cols))
        (apply #'exc-calc--raise-first-error arrays)
        (exc-calc-raise "#VALUE!")))
    (let ((columns (mapcar #'exc-calc-array-values arrays)))
      (exc-calc--add
       (apply #'cl-mapcar
              (lambda (&rest values)
                (let ((product 1))
                  (dolist (value values product)
                    (cond ((exc-calc-err-p value)
                           (exc-calc-raise (exc-calc-err-code value)))
                          ((numberp value) (setq product (* product value)))
                          ((and single (stringp value)) (exc-calc-raise "#VALUE!"))
                          (t (setq product 0))))))
              columns)))))

(exc-calc-define sumx2my2 ((xs array) (ys array))
  "The sum of the differences of the squares of pairs of XS and YS."
  (exc-calc--sum-pairs (lambda (x y) (- (* x x) (* y y))) xs ys))

(exc-calc-define sumx2py2 ((xs array) (ys array))
  "The sum of the sums of the squares of pairs of XS and YS."
  (exc-calc--sum-pairs (lambda (x y) (+ (* x x) (* y y))) xs ys))

(exc-calc-define sumxmy2 ((xs array) (ys array))
  "The sum of the squares of the differences of pairs of XS and YS."
  (exc-calc--sum-pairs (lambda (x y) (expt (- x y) 2)) xs ys))

;;;----------------------------------------------------------------------
;;; Criteria
;;;----------------------------------------------------------------------
;;
;; A criterion as SUMIF and COUNTIF take it: a value the cell must
;; equal, or text such as ">=5", "<>done" or "a*".  Text compares
;; without regard to case and may hold wildcards.

(defun exc-calc-criterion (criterion)
  "Return a function telling whether a cell's value meets CRITERION."
  (cond
   ((exc-calc-err-p criterion) (lambda (value) (eq value criterion)))
   ((numberp criterion)
    (lambda (value)
      (or (and (numberp value) (exc-calc--numbers-equal-p value criterion))
          (and (stringp value)
               (let ((number (exc-calc-text-number value)))
                 (and number (exc-calc--numbers-equal-p number criterion)))))))
   ((exc-calc-boolean-p criterion) (lambda (value) (eq value criterion)))
   ;; An empty cell given as the criterion means 0, as Excel takes it.
   ((exc-calc-blank-p criterion) (exc-calc-criterion 0))
   (t
    (let* ((text criterion)
           (op (cond ((string-prefix-p ">=" text) ">=")
                     ((string-prefix-p "<=" text) "<=")
                     ((string-prefix-p "<>" text) "<>")
                     ((string-prefix-p ">" text) ">")
                     ((string-prefix-p "<" text) "<")
                     ((string-prefix-p "=" text) "=")))
           (operand (if op (substring text (length op)) text))
           (wanted (exc-calc--criterion-operand operand)))
      (pcase op
        ((or 'nil "=")
         (cond ((and (string-empty-p operand) op)
                (lambda (value) (eq value :blank)))
               ((string-empty-p operand)
                (lambda (value) (or (eq value :blank) (equal value ""))))
               ((stringp wanted)
                (let ((regexp (exc-calc-wildcard-regexp wanted)))
                  (lambda (value)
                    (and (stringp value)
                         (let ((case-fold-search t))
                           (string-match-p regexp value))))))
               (t (exc-calc-criterion wanted))))
        ("<>"
         (cond
          ((string-empty-p operand)
           (lambda (value) (not (eq value :blank))))
          ;; "=23" takes the text "23" too, but "<>23" leaves out only
          ;; the number.
          ((numberp wanted)
           (lambda (value)
             (not (and (numberp value) (exc-calc--numbers-equal-p value wanted)))))
          (t
           (let ((match (exc-calc-criterion
                         (if (stringp wanted) (concat "=" operand) wanted))))
             (lambda (value) (not (funcall match value)))))))
        ;; ">" or "<=" with nothing after it, as ">"&A1 of an empty A1,
        ;; meets no cell.
        ((guard (string-empty-p operand)) #'ignore)
        (_
         (let ((test (pcase op
                       (">" (lambda (order) (> order 0)))
                       (">=" (lambda (order) (>= order 0)))
                       ("<" (lambda (order) (< order 0)))
                       ("<=" (lambda (order) (<= order 0))))))
           (lambda (value)
             (let ((order (exc-calc--lookup-order value wanted)))
               (and order (funcall test order)))))))))))

(defun exc-calc--criterion-operand (text)
  "Return TEXT, what follows a criterion's operator, as the value it means."
  (cond ((string-empty-p text) text)
        ((exc-calc-text-number text))
        ((member (upcase text) '("TRUE" "FALSE"))
         (exc-calc-bool (string= (upcase text) "TRUE")))
        ((assoc (upcase text) exc-calc--errors)
         (exc-calc-err (upcase text)))
        (t text)))

(defun exc-calc--range-shape (value)
  "Return (ROWS . COLS) of VALUE, a range or array, counted whole.
A whole column counts every row of the sheet, and a whole row every
column."
  (exc-calc--count-size value))

(defun exc-calc--criteria-tests (pairs)
  "Return PAIRS, (RANGE . CRITERION), as (RANGE . TEST) and their shape.
The ranges must be one size."
  (let ((shape (exc-calc--range-shape (caar pairs))))
    (cons (mapcar (lambda (pair)
                    (unless (equal (exc-calc--range-shape (car pair)) shape)
                      (exc-calc-raise "#VALUE!"))
                    (cons (car pair) (exc-calc-criterion (cdr pair))))
                  pairs)
          shape)))

(defun exc-calc--criteria-extent (tests shape &optional target)
  "Return (ROWS . COLS) of SHAPE the ranges of TESTS have on their sheets.
TARGET, a range the matching cells are read from, counts too."
  (let ((sizes (mapcar #'exc-calc--table-size
                       (cons (or target (caar tests)) (mapcar #'car tests)))))
    (cons (min (car shape) (apply #'max (mapcar #'car sizes)))
          (min (cdr shape) (apply #'max (mapcar #'cdr sizes))))))

(defun exc-calc--matching-cells (pairs &optional target)
  "Return the positions, (ROW . COL), meeting every criterion of PAIRS.
PAIRS is a list of (RANGE . CRITERION); the ranges must be one size.
Only cells a sheet of theirs or of TARGET, the range the positions are
read from, has are looked at; past them every cell is blank, which
`exc-calc--matching-count' counts."
  (pcase-let* ((`(,tests . ,shape) (exc-calc--criteria-tests pairs))
               (`(,rows . ,cols) (exc-calc--criteria-extent tests shape target))
               (found nil))
    (dotimes (row rows)
      (dotimes (col cols)
        (when (cl-every (lambda (test)
                          (funcall (cdr test)
                                   (exc-calc--table-at (car test) row col)))
                        tests)
          (push (cons row col) found))))
    (nreverse found)))

(defun exc-calc--matching-count (pairs)
  "Return how many cells meet every criterion of PAIRS, as COUNTIFS counts.
The cells past every range's sheet are blank, and count when every
criterion takes a blank cell."
  (pcase-let* ((`(,tests . ,shape) (exc-calc--criteria-tests pairs))
               (`(,rows . ,cols) (exc-calc--criteria-extent tests shape))
               (past (- (* (car shape) (cdr shape)) (* rows cols))))
    (+ (length (exc-calc--matching-cells pairs))
       (if (and (> past 0)
                (cl-every (lambda (test) (funcall (cdr test) :blank)) tests))
           past
         0))))

(defun exc-calc--each-criteria (arguments function)
  "Return FUNCTION of ARGUMENTS made (RANGE . CRITERION) pairs.
A criterion of several values, as {\"a\",\"b\"}, makes the result an
array, FUNCTION of the pairs with each of them in turn."
  (when (or (null arguments) (cl-oddp (length arguments)))
    (exc-calc-raise "#VALUE!"))
  (let ((ranges (cl-loop for (range) on arguments by #'cddr collect range))
        (criteria (cl-loop for (_ criterion) on arguments by #'cddr
                           collect (exc-calc--scalar-or-array criterion))))
    (if (cl-some #'exc-calc-array-p criteria)
        (exc-calc--broadcast
         (lambda (&rest ones)
           (exc-calc--catching (funcall function (cl-mapcar #'cons ranges ones))))
         criteria)
      (funcall function (cl-mapcar #'cons ranges criteria)))))

(defun exc-calc--numbers-at (range positions)
  "Return the numbers of RANGE at POSITIONS; an error value signals."
  (cl-loop for (row . col) in positions
           for value = (exc-calc--table-at range row col)
           when (exc-calc-err-p value)
           do (exc-calc-raise (exc-calc-err-code value))
           when (numberp value) collect value))

(defun exc-calc--target (range target)
  "Return TARGET, a range, made as large as RANGE, as SUMIF reads it.
It takes whole columns or rows where RANGE does."
  (if (and (exc-calc-ref-p target) (exc-calc-ref-p range))
      (pcase-let ((`(,rows . ,cols) (exc-calc--table-full-size range)))
        (exc-calc-ref--make (exc-calc-ref-model target)
                             (exc-calc-ref-top target) (exc-calc-ref-left target)
                             (+ (exc-calc-ref-top target) rows -1)
                             (+ (exc-calc-ref-left target) cols -1)
                             (exc-calc-ref-whole range)))
    target))

(exc-calc-define sumif ((range values) (criterion values) &optional (sum-range values))
  "The sum of the cells of SUM-RANGE, or RANGE, beside cells of RANGE
that meet CRITERION."
  (let ((criterion (exc-calc--deref criterion)))
    (if (exc-calc-array-p criterion)
        (exc-calc-array-map
         (lambda (one)
           (exc-calc--catching
             (exc-calc--sum-if range one sum-range)))
         criterion)
      (exc-calc--sum-if range criterion sum-range))))

(defun exc-calc--sum-if (range criterion sum-range)
  "Return SUMIF of RANGE, one CRITERION and SUM-RANGE."
  (let* ((target (exc-calc--target range (or sum-range range)))
         (positions (exc-calc--matching-cells (list (cons range criterion))
                                               target)))
    (exc-calc--add (exc-calc--numbers-at target positions))))

(exc-calc-define sumifs ((sum-range values) &rest (criteria values))
  "The sum of the cells of SUM-RANGE beside cells meeting every criterion.
CRITERIA alternate a range and the criterion its cells must meet."
  (exc-calc--each-criteria
   criteria
   (lambda (pairs)
     (unless (equal (exc-calc--range-shape sum-range)
                    (exc-calc--range-shape (caar pairs)))
       (exc-calc-raise "#VALUE!"))
     (exc-calc--add (exc-calc--numbers-at sum-range
                                            (exc-calc--matching-cells
                                             pairs sum-range))))))

;;;----------------------------------------------------------------------
;;; Matrices
;;;----------------------------------------------------------------------

(defun exc-calc--matrix (array)
  "Return ARRAY's values as a vector of row vectors of numbers.
A value that is not a number is #VALUE!."
  (let ((rows (exc-calc-array-rows array)) (cols (exc-calc-array-cols array)))
    (apply #'vector
           (cl-loop for row below rows
                    collect (apply #'vector
                                   (cl-loop for col below cols
                                            for value = (exc-calc-aref array row col)
                                            collect (if (numberp value) (float value)
                                                      (exc-calc-raise
                                                       (if (exc-calc-err-p value)
                                                           (exc-calc-err-code value)
                                                         "#VALUE!")))))))))

(exc-calc-define mmult ((left array) (right array))
  "The matrix product of LEFT and RIGHT."
  (let ((a (exc-calc--matrix left)) (b (exc-calc--matrix right)))
    (unless (= (exc-calc-array-cols left) (exc-calc-array-rows right))
      (exc-calc-raise "#VALUE!"))
    (exc-calc-array (exc-calc-array-rows left) (exc-calc-array-cols right)
                     (lambda (row col)
                       (exc-calc--tidy-number
                        (exc-calc--snap
                         (cl-loop for k below (exc-calc-array-cols left)
                                  sum (* (aref (aref a row) k)
                                         (aref (aref b k) col)))))))))

(defun exc-calc--square (array)
  "Return ARRAY as a matrix, which must be square."
  (unless (= (exc-calc-array-rows array) (exc-calc-array-cols array))
    (exc-calc-raise "#VALUE!"))
  (exc-calc--matrix array))

(defun exc-calc--eliminate (matrix &optional inverse)
  "Reduce MATRIX by Gaussian elimination and return its determinant.
With INVERSE, return its inverse instead, or nil when it has none."
  (let* ((n (length matrix))
         (a (apply #'vector (mapcar #'copy-sequence matrix)))
         (inv (apply #'vector
                     (cl-loop for i below n
                              collect (apply #'vector
                                             (cl-loop for j below n
                                                      collect (if (= i j) 1.0 0.0))))))
         (det 1.0))
    (catch 'singular
      (dotimes (col n)
        (let ((pivot (cl-loop with best = col
                              for row from col below n
                              when (> (abs (aref (aref a row) col))
                                      (abs (aref (aref a best) col)))
                              do (setq best row)
                              finally return best)))
          (when (zerop (aref (aref a pivot) col))
            (setq det 0.0)
            (throw 'singular nil))
          (unless (= pivot col)
            (cl-rotatef (aref a pivot) (aref a col))
            (cl-rotatef (aref inv pivot) (aref inv col))
            (setq det (- det)))
          (let ((p (aref (aref a col) col)))
            (setq det (* det p))
            (dotimes (j n)
              (aset (aref a col) j (/ (aref (aref a col) j) p))
              (aset (aref inv col) j (/ (aref (aref inv col) j) p))))
          (dotimes (row n)
            (unless (= row col)
              (let ((factor (aref (aref a row) col)))
                (dotimes (j n)
                  (aset (aref a row) j (- (aref (aref a row) j)
                                          (* factor (aref (aref a col) j))))
                  (aset (aref inv row) j (- (aref (aref inv row) j)
                                            (* factor (aref (aref inv col) j))))))))))
      (if inverse inv det))))

(exc-calc-define mdeterm ((array array))
  "The determinant of the square matrix ARRAY."
  (let ((det (or (exc-calc--eliminate (exc-calc--square array)) 0.0)))
    (exc-calc--tidy-number (exc-calc--snap det))))

(exc-calc-define minverse ((array array))
  "The inverse of the square matrix ARRAY; #NUM! when it has none."
  (let ((inverse (exc-calc--eliminate (exc-calc--square array) t))
        (n (exc-calc-array-rows array)))
    (unless inverse (exc-calc-raise "#NUM!"))
    (exc-calc-array n n (lambda (row col)
                           (exc-calc--tidy-number
                            (exc-calc--snap (aref (aref inverse row) col)))))))

(provide 'exc-calc-math)

;;; exc-calc-math.el ends here
