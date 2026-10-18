;;; grid-table-calc-math.el --- Math and trigonometry functions -*- lexical-binding: t -*-

;;; Commentary:

;; Sums and products, rounding, powers and logarithms, trigonometry,
;; matrices, and the criteria SUMIF and COUNTIF and their kin match.

;;; Code:

(require 'grid-table-calc)
(require 'grid-table-calc-lookup)

;;;----------------------------------------------------------------------
;;; Numbers as Excel keeps them
;;;----------------------------------------------------------------------

(defun grid-calc--snap (number)
  "Return NUMBER kept to the 15 significant digits Excel keeps.
So 0.1*3 is 0.3, and rounding 2.675 or taking the ceiling of 0.3*10
sees what was written rather than the nearest binary fraction."
  (if (and (floatp number) (not (zerop number))
           (not (isnan number)) (/= (abs number) 1.0e+INF))
      (float (string-to-number (format "%.15g" number)))
    number))

(defun grid-calc--finite (number)
  "Return NUMBER, or signal #NUM! when it is too large or not a number."
  (if (and (floatp number)
           (or (isnan number) (> (abs number) 1.7976931348623157e308)))
      (grid-calc-raise "#NUM!")
    (grid-calc--tidy-number number)))

(defun grid-calc--round-to (number digits how)
  "Return NUMBER rounded to DIGITS places after the point, by HOW.
HOW is `nearest', halves going away from zero, `up', away from zero, or
`down', toward zero.  Negative DIGITS rounds left of the point."
  (let* ((scale (expt 10.0 (abs digits)))
         (scaled (grid-calc--snap (if (>= digits 0) (* number scale)
                                     (/ number scale))))
         (magnitude (abs scaled))
         (rounded (pcase how
                    ('nearest (ffloor (+ magnitude 0.5)))
                    ('up (fceiling magnitude))
                    ('down (ffloor magnitude))))
         (signed (if (< scaled 0) (- rounded) rounded)))
    (grid-calc--tidy-number
     (grid-calc--snap (if (>= digits 0) (/ signed scale) (* signed scale))))))

(grid-calc-define round ((number number) &optional (digits integer 0))
  "NUMBER rounded to DIGITS places, halves away from zero."
  (grid-calc--round-to number digits 'nearest))

(grid-calc-define roundup ((number number) &optional (digits integer 0))
  "NUMBER rounded away from zero to DIGITS places."
  (grid-calc--round-to number digits 'up))

(grid-calc-define rounddown ((number number) &optional (digits integer 0))
  "NUMBER rounded toward zero to DIGITS places."
  (grid-calc--round-to number digits 'down))

(grid-calc-define trunc ((number number) &optional (digits integer 0))
  "NUMBER with its digits past DIGITS places cut off."
  (grid-calc--round-to number digits 'down))

(grid-calc-define int ((number number))
  "NUMBER rounded down to a whole number."
  (grid-calc--tidy-number (ffloor (grid-calc--snap (float number)))))

(defun grid-calc--to-multiple (number step how)
  "Return NUMBER rounded to a multiple of STEP, by HOW.
HOW is `nearest', `up' or `down'; up and down go along the number line,
toward plus and minus infinity."
  (if (zerop step) 0
    (let* ((quotient (grid-calc--snap (/ (float number) step)))
           (whole (pcase how
                    ('nearest (if (< quotient 0) (- (ffloor (+ (- quotient) 0.5)))
                                (ffloor (+ quotient 0.5))))
                    ('up (fceiling quotient))
                    ('down (ffloor quotient)))))
      (grid-calc--tidy-number (grid-calc--snap (* whole step))))))

(grid-calc-define mround ((number any) (multiple any))
  "NUMBER rounded to the nearest multiple of MULTIPLE; TRUE or FALSE is #VALUE!."
  (setq number (grid-calc--strict-num number)
        multiple (grid-calc--strict-num multiple))
  (when (and (/= number 0) (/= multiple 0) (/= (cl-signum number) (cl-signum multiple)))
    (grid-calc-raise "#NUM!"))
  (grid-calc--to-multiple number multiple 'nearest))

(grid-calc-define ceiling ((number number) &optional (significance number 1))
  "NUMBER rounded away from zero to a multiple of SIGNIFICANCE.
A negative NUMBER with a positive SIGNIFICANCE rounds toward zero."
  (cond ((zerop significance) 0)
        ((and (> number 0) (< significance 0)) (grid-calc-raise "#NUM!"))
        ((< significance 0)
         (- (grid-calc--to-multiple (- number) (- significance) 'up)))
        (t (grid-calc--to-multiple number significance 'up))))

(grid-calc-define floor ((number number) &optional (significance number 1))
  "NUMBER rounded toward zero to a multiple of SIGNIFICANCE.
A negative NUMBER with a positive SIGNIFICANCE rounds away from zero."
  (cond ((and (zerop significance) (zerop number)) 0)
        ((zerop significance) (grid-calc-raise "#DIV/0!"))
        ((and (> number 0) (< significance 0)) (grid-calc-raise "#NUM!"))
        ((< significance 0)
         (- (grid-calc--to-multiple (- number) (- significance) 'down)))
        (t (grid-calc--to-multiple number significance 'down))))

(grid-calc-define ceiling.math ((number number) &optional (significance number 1)
                                (mode number 0))
  "NUMBER rounded up to a multiple of SIGNIFICANCE.
A negative NUMBER rounds toward zero, or away from it when MODE is not 0."
  (let ((step (abs significance)))
    (if (and (< number 0) (/= mode 0))
        (- (grid-calc--to-multiple (- number) step 'up))
      (grid-calc--to-multiple number step 'up))))

(grid-calc-define floor.math ((number number) &optional (significance number 1)
                              (mode number 0))
  "NUMBER rounded down to a multiple of SIGNIFICANCE.
A negative NUMBER rounds away from zero, or toward it when MODE is not 0."
  (let ((step (abs significance)))
    (if (and (< number 0) (/= mode 0))
        (- (grid-calc--to-multiple (- number) step 'down))
      (grid-calc--to-multiple number step 'down))))

(grid-calc-define ceiling.precise ((number number) &optional (significance number 1))
  "NUMBER rounded up to a multiple of SIGNIFICANCE, whatever its sign."
  (grid-calc--to-multiple number (abs significance) 'up))

(grid-calc-alias iso.ceiling ceiling.precise)

(grid-calc-define floor.precise ((number number) &optional (significance number 1))
  "NUMBER rounded down to a multiple of SIGNIFICANCE, whatever its sign."
  (grid-calc--to-multiple number (abs significance) 'down))

(defun grid-calc--round-to-parity (number odd)
  "Return NUMBER rounded away from zero to an even integer, or ODD one."
  (let* ((magnitude (fceiling (grid-calc--snap (abs (float number)))))
         (whole (truncate magnitude))
         (whole (if (eq (cl-oddp whole) odd) whole (1+ whole))))
    (if (< number 0) (- whole) whole)))

(grid-calc-define even ((number number))
  "NUMBER rounded away from zero to an even whole number."
  (grid-calc--round-to-parity number nil))

(grid-calc-define odd ((number number))
  "NUMBER rounded away from zero to an odd whole number."
  (grid-calc--round-to-parity number t))

;;;----------------------------------------------------------------------
;;; Arithmetic
;;;----------------------------------------------------------------------

(grid-calc-define abs ((number number))
  "NUMBER without its sign."
  (abs number))

(grid-calc-define sign ((number number))
  "1 for a positive NUMBER, -1 for a negative one, 0 for 0."
  (cond ((> number 0) 1) ((< number 0) -1) (t 0)))

(grid-calc-define mod ((number number) (divisor number))
  "The remainder of NUMBER divided by DIVISOR, with the sign of DIVISOR."
  (when (zerop divisor) (grid-calc-raise "#DIV/0!"))
  (grid-calc--tidy-number
   (grid-calc--snap (- number (* divisor (ffloor (grid-calc--snap
                                                   (/ (float number) divisor))))))))

(grid-calc-define power ((base number) (exponent number))
  "BASE to the power EXPONENT."
  (grid-calc--power base exponent))

(grid-calc-define sqrt ((number number))
  "The square root of NUMBER."
  (when (< number 0) (grid-calc-raise "#NUM!"))
  (grid-calc--tidy-number (sqrt number)))

(grid-calc-define exp ((number number))
  "e to the power NUMBER."
  (grid-calc--finite (exp number)))

(grid-calc-define ln ((number number))
  "The natural logarithm of NUMBER."
  (when (<= number 0) (grid-calc-raise "#NUM!"))
  (grid-calc--tidy-number (log number)))

(grid-calc-define log ((number number) &optional (base number 10))
  "The logarithm of NUMBER to BASE."
  (when (or (<= number 0) (<= base 0)) (grid-calc-raise "#NUM!"))
  (when (= base 1) (grid-calc-raise "#DIV/0!"))
  (grid-calc--tidy-number (grid-calc--snap (/ (log number) (log base)))))

(grid-calc-define log10 ((number number))
  "The logarithm of NUMBER to base 10."
  (when (<= number 0) (grid-calc-raise "#NUM!"))
  (grid-calc--tidy-number (grid-calc--snap (log number 10))))

(grid-calc-define pi ()
  "The number pi."
  float-pi)

(grid-calc-define fact ((number number))
  "The product of the whole numbers from 1 to NUMBER."
  (when (< number 0) (grid-calc-raise "#NUM!"))
  (let ((product 1.0))
    (cl-loop for i from 2 to (truncate number) do (setq product (* product i)))
    (grid-calc--finite product)))

(grid-calc-define combin ((number integer) (chosen integer))
  "How many ways there are to choose CHOSEN of NUMBER things."
  (when (or (< number 0) (< chosen 0) (< number chosen))
    (grid-calc-raise "#NUM!"))
  (let ((result 1.0))
    (cl-loop for i from 1 to (min chosen (- number chosen))
             do (setq result (/ (* result (- number (- i 1))) i)))
    (grid-calc--tidy-number (fround result))))

(grid-calc-define rand ()
  "A random number from 0 up to 1."
  (/ (random most-positive-fixnum) (float most-positive-fixnum)))

(defun grid-calc--random (min max whole)
  "Return a random number from MIN up to MAX, a whole one with WHOLE."
  (if whole
      (+ (truncate min)
         (random (max 1 (1+ (- (truncate max) (truncate min))))))
    (+ min (* (- max min) (/ (random most-positive-fixnum)
                             (float most-positive-fixnum))))))

(grid-calc-define randarray (&optional (rows integer 1) (cols integer 1)
                            (min number 0) (max number 1) (whole bool))
  "ROWS by COLS random numbers from MIN up to MAX, whole ones with WHOLE."
  :empty t
  (when (or (< rows 1) (< cols 1) (> min max)) (grid-calc-raise "#VALUE!"))
  (grid-calc-array rows cols
                   (lambda (_row _col) (grid-calc--random min max whole))))

(grid-calc-define base ((number integer) (radix integer) &optional (length integer 0))
  "NUMBER written in base RADIX, padded with zeros to LENGTH digits."
  (unless (and (>= number 0) (<= 2 radix 36) (>= length 0))
    (grid-calc-raise "#NUM!"))
  (let ((digits "") (n number))
    (while (> n 0)
      (setq digits (concat (string (aref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                         (% n radix)))
                           digits)
            n (/ n radix)))
    (string-pad (if (string-empty-p digits) "0" digits) length ?0 t)))

(grid-calc-define sqrtpi ((number number))
  "The square root of NUMBER times pi."
  :toolpak t
  (when (< number 0) (grid-calc-raise "#NUM!"))
  (sqrt (* number float-pi)))

(grid-calc-define factdouble ((number number))
  "The product of every other whole number from NUMBER down to 1."
  :toolpak t
  (when (< number -1) (grid-calc-raise "#NUM!"))
  (let ((n (truncate number)) (product 1.0))
    (while (> n 1) (setq product (* product n) n (- n 2)))
    (grid-calc--finite product)))

(grid-calc-define quotient ((number number) (divisor number))
  "The whole part of NUMBER divided by DIVISOR."
  :toolpak t
  (when (zerop divisor) (grid-calc-raise "#DIV/0!"))
  (grid-calc--tidy-number (ftruncate (/ (float number) divisor))))

(grid-calc-define randbetween ((low number) (high number))
  "A random whole number from LOW to HIGH."
  (let ((low (ceiling low)) (high (floor high)))
    (when (> low high) (grid-calc-raise "#NUM!"))
    (+ low (random (1+ (- high low))))))

(defun grid-calc--toolpak-numbers (arguments)
  "Return the numbers of ARGUMENTS, `values' arguments, as GCD reads them.
Text that reads as a number counts in a range or array too; other text,
TRUE and FALSE are #VALUE! wherever they are.  A blank cell is 0 in a
range and left out as an argument; an argument left out is #N/A.  With
no number at all, a range of nothing but cells past the sheet's filled
ones gives 0, and arguments that are none #VALUE!."
  (let (numbers ranges)
    (dolist (argument arguments)
      (when (eq argument :blank) (grid-calc-raise "#N/A"))
      ;; One cell is read as written directly.
      (let ((one (grid-calc--deref argument)))
        (unless (grid-calc-array-p one) (setq argument one)))
      (when (or (grid-calc-ref-p argument) (grid-calc-array-p argument)
                (consp argument))
        (setq ranges t))
      (grid-calc-each-value
       argument
       (lambda (value direct)
         (cond
          ((numberp value) (push value numbers))
          ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
          ((eq value :blank) (unless direct (push 0 numbers)))
          ((and (stringp value) (grid-calc-text-number value))
           (push (grid-calc-text-number value) numbers))
          (t (grid-calc-raise "#VALUE!"))))))
    (cond (numbers (nreverse numbers))
          (ranges '(0))
          (t (grid-calc-raise "#VALUE!")))))

(defun grid-calc--whole-numbers (arguments)
  "Return the numbers of ARGUMENTS with their fractions cut off.
#NUM! for one below 0 or from 2^53."
  (mapcar (lambda (n)
            (unless (and (<= 0 n) (< n (expt 2.0 53))) (grid-calc-raise "#NUM!"))
            (truncate n))
          (grid-calc--toolpak-numbers arguments)))

(grid-calc-define gcd (&rest (numbers values))
  "The greatest whole number that divides each of NUMBERS."
  (let ((result 0))
    (dolist (n (grid-calc--whole-numbers numbers) result)
      (setq result (cl-gcd result n)))))

(grid-calc-define lcm (&rest (numbers values))
  "The least whole number each of NUMBERS divides; 0 when one of them is."
  (let ((result 1))
    (dolist (n (grid-calc--whole-numbers numbers) result)
      (setq result (if (or (zerop n) (zerop result)) 0 (cl-lcm result n)))
      ;; Past 2^53 a double cannot hold it.
      (when (>= result (expt 2 53)) (grid-calc-raise "#NUM!")))))

(grid-calc-define multinomial (&rest (numbers values))
  "The factorial of the sum of NUMBERS over the product of their factorials."
  (let ((result 1.0) (sum 0))
    (dolist (n (grid-calc--whole-numbers numbers))
      ;; Each step multiplies by the binomial coefficient of SUM + N over N.
      (cl-loop for i from 1 to n
               do (setq result (/ (* result (+ sum i)) i)))
      (setq sum (+ sum n)))
    (grid-calc--finite (fround result))))

(grid-calc-define permut ((number integer) (chosen integer))
  "How many ways there are to arrange CHOSEN of NUMBER things."
  (when (or (< number 0) (< chosen 0) (< number chosen))
    (grid-calc-raise "#NUM!"))
  (let ((result 1.0))
    (cl-loop for i from 0 below chosen do (setq result (* result (- number i))))
    (grid-calc--finite result)))

(grid-calc-define permutationa ((number integer) (chosen integer))
  "How many ways there are to arrange CHOSEN of NUMBER things, repeating."
  (when (or (< number 0) (< chosen 0)) (grid-calc-raise "#NUM!"))
  (grid-calc--finite (expt (float number) chosen)))

(grid-calc-define combina ((number integer) (chosen integer))
  "How many ways there are to choose CHOSEN of NUMBER things, repeating."
  (when (or (< number 0) (< chosen 0) (and (zerop number) (> chosen 0)))
    (grid-calc-raise "#NUM!"))
  (let ((result 1.0) (n (+ number chosen -1)))
    (cl-loop for i from 1 to chosen
             do (setq result (/ (* result (- n (- i 1))) i)))
    (grid-calc--tidy-number (fround result))))

(grid-calc-define seriessum ((x number) (n number) (m number) (coefficients values))
  "The sum of each of COEFFICIENTS times X to N, N + M, N + 2M and so on."
  :toolpak t
  (let ((power n) (sum 0.0))
    (dolist (a (grid-calc-numbers (list coefficients)))
      (when (and (zerop x) (<= power 0))
        (grid-calc-raise (if (zerop power) "#NUM!" "#DIV/0!")))
      (setq sum (+ sum (* a (expt (float x) power))) power (+ power m)))
    (grid-calc--finite sum)))

(grid-calc-define munit ((size integer))
  "The SIZE by SIZE unit matrix."
  (when (< size 1) (grid-calc-raise "#VALUE!"))
  (grid-calc-array size size (lambda (row col) (if (= row col) 1 0))))

(defconst grid-calc--roman-digits
  '((?M . 1000) (?D . 500) (?C . 100) (?L . 50) (?X . 10) (?V . 5) (?I . 1))
  "The Roman digits, largest first, with their values.")

(grid-calc-define roman ((number number) &optional (form any 0))
  "NUMBER in Roman digits.  FORM 0, the default, is the classic form;
1 to 4 are ever shorter, as FORM TRUE is 0 and FALSE 4."
  (let ((form (cond ((eq form t) 0) ((eq form :false) 4)
                    ((grid-calc-blank-p form) 0)
                    (t (truncate (grid-calc-num form)))))
        (value (truncate number))
        (chars (vconcat (mapcar #'car grid-calc--roman-digits)))
        (values (vconcat (mapcar #'cdr grid-calc--roman-digits)))
        (digits '()))
    (unless (and (<= 0 value 3999) (<= 0 form 4)) (grid-calc-raise "#VALUE!"))
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

(grid-calc-define arabic ((text text))
  "The number TEXT writes in Roman digits, which may start with -."
  (let* ((text (string-trim text))
         (negative (string-prefix-p "-" text))
         (text (upcase (if negative (substring text 1) text)))
         (total 0) (previous 0))
    (when (> (length text) 255) (grid-calc-raise "#VALUE!"))
    (cl-loop for i downfrom (1- (length text)) to 0
             do (let ((value (alist-get (aref text i) grid-calc--roman-digits)))
                  (unless value (grid-calc-raise "#VALUE!"))
                  (if (< value previous) (setq total (- total value))
                    (setq total (+ total value) previous value))))
    (if negative (- total) total)))

;;;----------------------------------------------------------------------
;;; Trigonometry
;;;----------------------------------------------------------------------

(defmacro grid-calc--define-math (name doc function &optional domain)
  "Define NAME, DOC, giving FUNCTION of its one number.
DOMAIN, a function of the number, makes it #NUM! when it does not hold."
  `(grid-calc-define ,name ((number number))
     ,doc
     ,@(and domain `((unless (funcall ,domain number) (grid-calc-raise "#NUM!"))))
     (grid-calc--finite (funcall ,function (float number)))))

(grid-calc--define-math sin "The sine of NUMBER, an angle in radians." #'sin)
(grid-calc--define-math cos "The cosine of NUMBER, an angle in radians." #'cos)
(grid-calc--define-math tan "The tangent of NUMBER, an angle in radians." #'tan)
(grid-calc--define-math asin "The angle whose sine is NUMBER." #'asin
                        (lambda (x) (<= -1 x 1)))
(grid-calc--define-math acos "The angle whose cosine is NUMBER." #'acos
                        (lambda (x) (<= -1 x 1)))
(grid-calc--define-math atan "The angle whose tangent is NUMBER." #'atan)
(grid-calc--define-math sinh "The hyperbolic sine of NUMBER."
                        (lambda (x) (/ (- (exp x) (exp (- x))) 2)))
(grid-calc--define-math cosh "The hyperbolic cosine of NUMBER."
                        (lambda (x) (/ (+ (exp x) (exp (- x))) 2)))
(grid-calc--define-math tanh "The hyperbolic tangent of NUMBER."
                        (lambda (x) (if (> (abs x) 20) (cl-signum x)
                                      (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1)))))
(grid-calc--define-math asinh "The number whose hyperbolic sine is NUMBER."
                        (lambda (x) (log (+ x (sqrt (1+ (* x x)))))))
(grid-calc--define-math acosh "The number whose hyperbolic cosine is NUMBER."
                        (lambda (x) (log (+ x (sqrt (1- (* x x))))))
                        (lambda (x) (>= x 1)))
(grid-calc--define-math atanh "The number whose hyperbolic tangent is NUMBER."
                        (lambda (x) (/ (log (/ (+ 1 x) (- 1 x))) 2))
                        (lambda (x) (< -1 x 1)))
(grid-calc--define-math radians "NUMBER degrees in radians."
                        (lambda (x) (/ (* x float-pi) 180)))
(grid-calc--define-math degrees "NUMBER radians in degrees."
                        (lambda (x) (/ (* x 180) float-pi)))

(defun grid-calc--within-trig-p (x)
  "Return non-nil when X is small enough for SEC, CSC and COT."
  (< (abs x) (expt 2.0 27)))

(defun grid-calc--reciprocal (x)
  "Return 1 / X, #DIV/0! for 0."
  (if (zerop x) (grid-calc-raise "#DIV/0!") (/ 1.0 x)))

(grid-calc--define-math sec "The secant of NUMBER, an angle in radians."
                        (lambda (x) (/ 1.0 (cos x))) #'grid-calc--within-trig-p)
(grid-calc--define-math csc "The cosecant of NUMBER, an angle in radians."
                        (lambda (x) (grid-calc--reciprocal (sin x)))
                        #'grid-calc--within-trig-p)
(grid-calc--define-math cot "The cotangent of NUMBER, an angle in radians."
                        (lambda (x) (grid-calc--reciprocal (tan x)))
                        #'grid-calc--within-trig-p)
(grid-calc--define-math sech "The hyperbolic secant of NUMBER."
                        (lambda (x) (if (> (abs x) 710) 0.0
                                      (/ 2 (+ (exp x) (exp (- x)))))))
(grid-calc--define-math csch "The hyperbolic cosecant of NUMBER."
                        (lambda (x) (if (> (abs x) 710) 0.0
                                      (grid-calc--reciprocal
                                       (/ (- (exp x) (exp (- x))) 2)))))
(grid-calc--define-math coth "The hyperbolic cotangent of NUMBER."
                        (lambda (x) (if (> (abs x) 20) (cl-signum x)
                                      (grid-calc--reciprocal
                                       (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1))))))
(grid-calc--define-math acot "The angle whose cotangent is NUMBER."
                        (lambda (x) (- (/ float-pi 2) (atan x))))
(grid-calc--define-math acoth "The number whose hyperbolic cotangent is NUMBER."
                        (lambda (x) (/ (log (/ (+ x 1) (- x 1))) 2))
                        (lambda (x) (> (abs x) 1)))

(grid-calc-define atan2 ((x number) (y number))
  "The angle of the point X, Y from the x axis."
  (when (and (zerop x) (zerop y)) (grid-calc-raise "#DIV/0!"))
  (atan (float y) x))

;;;----------------------------------------------------------------------
;;; Sums and products
;;;----------------------------------------------------------------------

(defun grid-calc--add (numbers)
  "Return the sum of NUMBERS, kept to Excel's 15 digits."
  (grid-calc--tidy-number (apply #'+ 0 numbers)))

(grid-calc-define sum (&rest (numbers values))
  "The sum of NUMBERS."
  (grid-calc--add (grid-calc-numbers numbers)))

(grid-calc-define percentof ((part values) (whole values))
  "The sum of PART as a fraction of the sum of WHOLE.
Only numbers count, and an error value in either is #NUM!."
  :errors t
  (when (cl-some #'grid-calc-err-p
                 (append (grid-calc-flat-values part)
                         (grid-calc-flat-values whole)))
    (grid-calc-raise "#NUM!"))
  (let ((whole (apply #'+ 0 (grid-calc-numbers (list whole)))))
    (when (zerop whole) (grid-calc-raise "#DIV/0!"))
    (/ (apply #'+ 0.0 (grid-calc-numbers (list part))) whole)))

(grid-calc-define product (&rest (numbers values))
  "The product of NUMBERS; 0 when there are none."
  (let ((numbers (grid-calc-numbers numbers)))
    (if numbers (grid-calc--finite (grid-calc--snap (apply #'* 1 numbers))) 0)))

(grid-calc-define sumsq (&rest (numbers values))
  "The sum of the squares of NUMBERS."
  (grid-calc--add (mapcar (lambda (n) (* n n)) (grid-calc-numbers numbers))))

(defun grid-calc--pairs (left right)
  "Return the values of arrays LEFT and RIGHT, which must hold as many.
They are a list of (L . R) pairs, in reading order; the shapes may
differ, as a column of three pairs with a row of three."
  (let ((lefts (grid-calc-array-values left))
        (rights (grid-calc-array-values right)))
    (unless (= (length lefts) (length rights))
      (grid-calc-raise "#N/A"))
    (cl-mapcar #'cons lefts rights)))

(defun grid-calc--sum-pairs (fn xs ys)
  "Return the sum of FN over the pairs of numbers of XS and YS.
With no such pair it is #DIV/0!, as in Excel."
  (let ((pairs (grid-calc--number-pairs xs ys)))
    (unless pairs (grid-calc-raise "#DIV/0!"))
    (grid-calc--add (mapcar (lambda (p) (funcall fn (car p) (cdr p))) pairs))))

(defun grid-calc--raise-first-error (&rest arrays)
  "Raise the first error in ARRAYS, all of the first before the next."
  (dolist (array arrays)
    (when-let* ((error (cl-find-if #'grid-calc-err-p
                                   (grid-calc-array-values array))))
      (grid-calc-raise (grid-calc-err-code error)))))

(defun grid-calc--raise-pair-error (left right)
  "Raise the first error in arrays LEFT and RIGHT, all of LEFT first.
#NULL! is passed over while there is another error, as Excel does in
functions of pairs."
  (let ((errors (cl-remove-if-not #'grid-calc-err-p
                                  (append (grid-calc-array-values left)
                                          (grid-calc-array-values right)))))
    (when errors
      (grid-calc-raise
       (grid-calc-err-code
        (or (cl-find-if-not (lambda (e) (equal (grid-calc-err-code e) "#NULL!"))
                            errors)
            (car errors)))))))

(defun grid-calc--number-pairs (left right)
  "Return the pairs of LEFT and RIGHT that are both numbers, as (L . R).
Arrays of different sizes are #N/A before an error in them is raised,
as `grid-calc--raise-pair-error' does."
  (let ((pairs (grid-calc--pairs left right)))
    (grid-calc--raise-pair-error left right)
    (cl-remove-if-not (lambda (pair)
                        (and (numberp (car pair)) (numberp (cdr pair))))
                      pairs)))

(grid-calc-define sumproduct ((first array) &rest (others array))
  "The sum of the products of the values in one place of each array.
Values that are not numbers count as 0, but text is #VALUE! when every
array is one value, as Excel takes them then.  Arrays of different
shapes give #VALUE!, after the first error in them."
  (let* ((arrays (cons first others))
         (rows (grid-calc-array-rows first))
         (cols (grid-calc-array-cols first))
         (single (and (= rows 1) (= cols 1))))
    (dolist (array others)
      (unless (and (= (grid-calc-array-rows array) rows)
                   (= (grid-calc-array-cols array) cols))
        (apply #'grid-calc--raise-first-error arrays)
        (grid-calc-raise "#VALUE!")))
    (let ((columns (mapcar #'grid-calc-array-values arrays)))
      (grid-calc--add
       (apply #'cl-mapcar
              (lambda (&rest values)
                (let ((product 1))
                  (dolist (value values product)
                    (cond ((grid-calc-err-p value)
                           (grid-calc-raise (grid-calc-err-code value)))
                          ((numberp value) (setq product (* product value)))
                          ((and single (stringp value)) (grid-calc-raise "#VALUE!"))
                          (t (setq product 0))))))
              columns)))))

(grid-calc-define sumx2my2 ((xs array) (ys array))
  "The sum of the differences of the squares of pairs of XS and YS."
  (grid-calc--sum-pairs (lambda (x y) (- (* x x) (* y y))) xs ys))

(grid-calc-define sumx2py2 ((xs array) (ys array))
  "The sum of the sums of the squares of pairs of XS and YS."
  (grid-calc--sum-pairs (lambda (x y) (+ (* x x) (* y y))) xs ys))

(grid-calc-define sumxmy2 ((xs array) (ys array))
  "The sum of the squares of the differences of pairs of XS and YS."
  (grid-calc--sum-pairs (lambda (x y) (expt (- x y) 2)) xs ys))

;;;----------------------------------------------------------------------
;;; Criteria
;;;----------------------------------------------------------------------
;;
;; A criterion as SUMIF and COUNTIF take it: a value the cell must
;; equal, or text such as ">=5", "<>done" or "a*".  Text compares
;; without regard to case and may hold wildcards.

(defun grid-calc-criterion (criterion)
  "Return a function telling whether a cell's value meets CRITERION."
  (cond
   ((grid-calc-err-p criterion) (lambda (value) (eq value criterion)))
   ((numberp criterion)
    (lambda (value)
      (or (and (numberp value) (grid-calc--numbers-equal-p value criterion))
          (and (stringp value)
               (let ((number (grid-calc-text-number value)))
                 (and number (grid-calc--numbers-equal-p number criterion)))))))
   ((grid-calc-boolean-p criterion) (lambda (value) (eq value criterion)))
   ;; An empty cell given as the criterion means 0, as Excel takes it.
   ((grid-calc-blank-p criterion) (grid-calc-criterion 0))
   (t
    (let* ((text criterion)
           (op (cond ((string-prefix-p ">=" text) ">=")
                     ((string-prefix-p "<=" text) "<=")
                     ((string-prefix-p "<>" text) "<>")
                     ((string-prefix-p ">" text) ">")
                     ((string-prefix-p "<" text) "<")
                     ((string-prefix-p "=" text) "=")))
           (operand (if op (substring text (length op)) text))
           (wanted (grid-calc--criterion-operand operand)))
      (pcase op
        ((or 'nil "=")
         (cond ((and (string-empty-p operand) op)
                (lambda (value) (eq value :blank)))
               ((string-empty-p operand)
                (lambda (value) (or (eq value :blank) (equal value ""))))
               ((stringp wanted)
                (let ((regexp (grid-calc-wildcard-regexp wanted)))
                  (lambda (value)
                    (and (stringp value)
                         (let ((case-fold-search t))
                           (string-match-p regexp value))))))
               (t (grid-calc-criterion wanted))))
        ("<>"
         (cond
          ((string-empty-p operand)
           (lambda (value) (not (eq value :blank))))
          ;; "=23" takes the text "23" too, but "<>23" leaves out only
          ;; the number.
          ((numberp wanted)
           (lambda (value)
             (not (and (numberp value) (grid-calc--numbers-equal-p value wanted)))))
          (t
           (let ((match (grid-calc-criterion
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
             (let ((order (grid-calc--lookup-order value wanted)))
               (and order (funcall test order)))))))))))

(defun grid-calc--criterion-operand (text)
  "Return TEXT, what follows a criterion's operator, as the value it means."
  (cond ((string-empty-p text) text)
        ((grid-calc-text-number text))
        ((member (upcase text) '("TRUE" "FALSE"))
         (grid-calc-bool (string= (upcase text) "TRUE")))
        ((assoc (upcase text) grid-calc--errors)
         (grid-calc-err (upcase text)))
        (t text)))

(defun grid-calc--range-shape (value)
  "Return (ROWS . COLS) of VALUE, a range or array, counted whole.
A whole column counts every row of the sheet, and a whole row every
column."
  (grid-calc--count-size value))

(defun grid-calc--criteria-tests (pairs)
  "Return PAIRS, (RANGE . CRITERION), as (RANGE . TEST) and their shape.
The ranges must be one size."
  (let ((shape (grid-calc--range-shape (caar pairs))))
    (cons (mapcar (lambda (pair)
                    (unless (equal (grid-calc--range-shape (car pair)) shape)
                      (grid-calc-raise "#VALUE!"))
                    (cons (car pair) (grid-calc-criterion (cdr pair))))
                  pairs)
          shape)))

(defun grid-calc--criteria-extent (tests shape &optional target)
  "Return (ROWS . COLS) of SHAPE the ranges of TESTS have on their sheets.
TARGET, a range the matching cells are read from, counts too."
  (let ((sizes (mapcar #'grid-calc--table-size
                       (cons (or target (caar tests)) (mapcar #'car tests)))))
    (cons (min (car shape) (apply #'max (mapcar #'car sizes)))
          (min (cdr shape) (apply #'max (mapcar #'cdr sizes))))))

(defun grid-calc--matching-cells (pairs &optional target)
  "Return the positions, (ROW . COL), meeting every criterion of PAIRS.
PAIRS is a list of (RANGE . CRITERION); the ranges must be one size.
Only cells a sheet of theirs or of TARGET, the range the positions are
read from, has are looked at; past them every cell is blank, which
`grid-calc--matching-count' counts."
  (pcase-let* ((`(,tests . ,shape) (grid-calc--criteria-tests pairs))
               (`(,rows . ,cols) (grid-calc--criteria-extent tests shape target))
               (found nil))
    (dotimes (row rows)
      (dotimes (col cols)
        (when (cl-every (lambda (test)
                          (funcall (cdr test)
                                   (grid-calc--table-at (car test) row col)))
                        tests)
          (push (cons row col) found))))
    (nreverse found)))

(defun grid-calc--matching-count (pairs)
  "Return how many cells meet every criterion of PAIRS, as COUNTIFS counts.
The cells past every range's sheet are blank, and count when every
criterion takes a blank cell."
  (pcase-let* ((`(,tests . ,shape) (grid-calc--criteria-tests pairs))
               (`(,rows . ,cols) (grid-calc--criteria-extent tests shape))
               (past (- (* (car shape) (cdr shape)) (* rows cols))))
    (+ (length (grid-calc--matching-cells pairs))
       (if (and (> past 0)
                (cl-every (lambda (test) (funcall (cdr test) :blank)) tests))
           past
         0))))

(defun grid-calc--each-criteria (arguments function)
  "Return FUNCTION of ARGUMENTS made (RANGE . CRITERION) pairs.
A criterion of several values, as {\"a\",\"b\"}, makes the result an
array, FUNCTION of the pairs with each of them in turn."
  (when (or (null arguments) (cl-oddp (length arguments)))
    (grid-calc-raise "#VALUE!"))
  (let ((ranges (cl-loop for (range) on arguments by #'cddr collect range))
        (criteria (cl-loop for (_ criterion) on arguments by #'cddr
                           collect (grid-calc--scalar-or-array criterion))))
    (if (cl-some #'grid-calc-array-p criteria)
        (grid-calc--broadcast
         (lambda (&rest ones)
           (grid-calc--catching (funcall function (cl-mapcar #'cons ranges ones))))
         criteria)
      (funcall function (cl-mapcar #'cons ranges criteria)))))

(defun grid-calc--numbers-at (range positions)
  "Return the numbers of RANGE at POSITIONS; an error value signals."
  (cl-loop for (row . col) in positions
           for value = (grid-calc--table-at range row col)
           when (grid-calc-err-p value)
           do (grid-calc-raise (grid-calc-err-code value))
           when (numberp value) collect value))

(defun grid-calc--target (range target)
  "Return TARGET, a range, made as large as RANGE, as SUMIF reads it.
It takes whole columns or rows where RANGE does."
  (if (and (grid-calc-ref-p target) (grid-calc-ref-p range))
      (pcase-let ((`(,rows . ,cols) (grid-calc--table-full-size range)))
        (grid-calc-ref--make (grid-calc-ref-model target)
                             (grid-calc-ref-top target) (grid-calc-ref-left target)
                             (+ (grid-calc-ref-top target) rows -1)
                             (+ (grid-calc-ref-left target) cols -1)
                             (grid-calc-ref-whole range)))
    target))

(grid-calc-define sumif ((range values) (criterion values) &optional (sum-range values))
  "The sum of the cells of SUM-RANGE, or RANGE, beside cells of RANGE
that meet CRITERION."
  (let ((criterion (grid-calc--deref criterion)))
    (if (grid-calc-array-p criterion)
        (grid-calc-array-map
         (lambda (one)
           (grid-calc--catching
             (grid-calc--sum-if range one sum-range)))
         criterion)
      (grid-calc--sum-if range criterion sum-range))))

(defun grid-calc--sum-if (range criterion sum-range)
  "Return SUMIF of RANGE, one CRITERION and SUM-RANGE."
  (let* ((target (grid-calc--target range (or sum-range range)))
         (positions (grid-calc--matching-cells (list (cons range criterion))
                                               target)))
    (grid-calc--add (grid-calc--numbers-at target positions))))

(grid-calc-define sumifs ((sum-range values) &rest (criteria values))
  "The sum of the cells of SUM-RANGE beside cells meeting every criterion.
CRITERIA alternate a range and the criterion its cells must meet."
  (grid-calc--each-criteria
   criteria
   (lambda (pairs)
     (unless (equal (grid-calc--range-shape sum-range)
                    (grid-calc--range-shape (caar pairs)))
       (grid-calc-raise "#VALUE!"))
     (grid-calc--add (grid-calc--numbers-at sum-range
                                            (grid-calc--matching-cells
                                             pairs sum-range))))))

;;;----------------------------------------------------------------------
;;; Matrices
;;;----------------------------------------------------------------------

(defun grid-calc--matrix (array)
  "Return ARRAY's values as a vector of row vectors of numbers.
A value that is not a number is #VALUE!."
  (let ((rows (grid-calc-array-rows array)) (cols (grid-calc-array-cols array)))
    (apply #'vector
           (cl-loop for row below rows
                    collect (apply #'vector
                                   (cl-loop for col below cols
                                            for value = (grid-calc-aref array row col)
                                            collect (if (numberp value) (float value)
                                                      (grid-calc-raise
                                                       (if (grid-calc-err-p value)
                                                           (grid-calc-err-code value)
                                                         "#VALUE!")))))))))

(grid-calc-define mmult ((left array) (right array))
  "The matrix product of LEFT and RIGHT."
  (let ((a (grid-calc--matrix left)) (b (grid-calc--matrix right)))
    (unless (= (grid-calc-array-cols left) (grid-calc-array-rows right))
      (grid-calc-raise "#VALUE!"))
    (grid-calc-array (grid-calc-array-rows left) (grid-calc-array-cols right)
                     (lambda (row col)
                       (grid-calc--tidy-number
                        (grid-calc--snap
                         (cl-loop for k below (grid-calc-array-cols left)
                                  sum (* (aref (aref a row) k)
                                         (aref (aref b k) col)))))))))

(defun grid-calc--square (array)
  "Return ARRAY as a matrix, which must be square."
  (unless (= (grid-calc-array-rows array) (grid-calc-array-cols array))
    (grid-calc-raise "#VALUE!"))
  (grid-calc--matrix array))

(defun grid-calc--eliminate (matrix &optional inverse)
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

(grid-calc-define mdeterm ((array array))
  "The determinant of the square matrix ARRAY."
  (let ((det (or (grid-calc--eliminate (grid-calc--square array)) 0.0)))
    (grid-calc--tidy-number (grid-calc--snap det))))

(grid-calc-define minverse ((array array))
  "The inverse of the square matrix ARRAY; #NUM! when it has none."
  (let ((inverse (grid-calc--eliminate (grid-calc--square array) t))
        (n (grid-calc-array-rows array)))
    (unless inverse (grid-calc-raise "#NUM!"))
    (grid-calc-array n n (lambda (row col)
                           (grid-calc--tidy-number
                            (grid-calc--snap (aref (aref inverse row) col)))))))

(provide 'grid-table-calc-math)

;;; grid-table-calc-math.el ends here
