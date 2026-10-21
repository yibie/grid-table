;;; exc-calc-engineering.el --- Engineering functions -*- lexical-binding: t -*-

;;; Commentary:

;; Numbers in binary, octal and hexadecimal, tests of numbers against
;; each other, conversion between units, complex numbers and the Bessel
;; functions.  A number in another base is text of up to ten digits, its
;; top bit the sign, as Excel keeps them.  A complex number is text too,
;; as "3+4i" or "-2j".

;;; Code:

(require 'exc-calc)
(require 'exc-calc-math)

;;;----------------------------------------------------------------------
;;; Bases
;;;----------------------------------------------------------------------

(defconst exc-calc--bases
  '((2 . 10) (8 . 30) (16 . 40))
  "For each base, the bits its ten digits hold.")

(defun exc-calc--from-base (text base)
  "Return the number TEXT, up to ten digits in BASE, stands for."
  (let* ((text (upcase (cond ((eq text :blank) "")
                             ((numberp text) (exc-calc-number-text text))
                             (t text))))
         (bits (cdr (assq base exc-calc--bases)))
         (digits (pcase base (2 "01") (8 "0-7") (_ "0-9A-F"))))
    (unless (and (<= (length text) 10)
                 (string-match-p (format "\\`[%s]*\\'" digits) text))
      (exc-calc-raise "#NUM!"))
    (let ((number (if (string-empty-p text) 0 (string-to-number text base))))
      ;; Ten digits whose top bit is set are a negative number.
      (if (>= number (ash 1 (1- bits))) (- number (ash 1 bits)) number))))

(defun exc-calc--to-base (number base places)
  "Return NUMBER written in BASE, at least PLACES digits when given.
PLACES is read before NUMBER: Excel gives #NUM! for one below one or
above ten whatever NUMBER reads, as in \"E\" written in base 2."
  (let ((bits (cdr (assq base exc-calc--bases))))
    (when (and places (or (> places 10) (< places 1)))
      (exc-calc-raise "#NUM!"))
    (let ((number (truncate (exc-calc-num number))))
      (unless (and (>= number (- (ash 1 (1- bits)))) (< number (ash 1 (1- bits))))
        (exc-calc-raise "#NUM!"))
      (if (< number 0)
          ;; A negative number is always ten digits, PLACES or not.
          (exc-calc--base-digits (+ number (ash 1 bits)) base)
        (let ((text (exc-calc--base-digits number base)))
          (cond ((null places) text)
                ((< places (length text)) (exc-calc-raise "#NUM!"))
                (t (concat (make-string (- places (length text)) ?0) text))))))))

(defun exc-calc--base-digits (number base)
  "Return NUMBER, not negative, as digits in BASE."
  (if (zerop number) "0"
    (let (digits)
      (while (> number 0)
        (push (aref "0123456789ABCDEF" (% number base)) digits)
        (setq number (/ number base)))
      (apply #'string digits))))

(defmacro exc-calc--define-bases (&rest pairs)
  "Define the functions converting between each of PAIRS of bases.
Each pair is (NAME BASE), as (bin 2)."
  `(progn
     ,@(cl-loop
        for (from from-base) in pairs
        append
        (append
         (list `(exc-calc-define ,(intern (format "%s2dec" from)) ((text any))
                  ,(format "The number TEXT, in base %d, stands for." from-base)
                  :toolpak t
                  (exc-calc--from-base text ,from-base))
               ;; NUMBER is read after PLACES: Excel gives #NUM! for a
               ;; PLACES out of range whatever NUMBER reads.
               `(exc-calc-define ,(intern (format "dec2%s" from))
                    ((number any) &optional (places integer))
                  ,(format "NUMBER in base %d, at least PLACES digits long." from-base)
                  :toolpak t
                  (exc-calc--to-base number ,from-base places)))
         (cl-loop for (to to-base) in pairs
                  unless (eq from to)
                  collect
                  `(exc-calc-define ,(intern (format "%s2%s" from to))
                       ((text any) &optional (places integer))
                     ,(format "TEXT, in base %d, in base %d." from-base to-base)
                     :toolpak t
                     (exc-calc--to-base (exc-calc--from-base text ,from-base)
                                         ,to-base places)))))))

(exc-calc--define-bases (bin 2) (oct 8) (hex 16))

(exc-calc-define decimal ((text text) (base integer))
  "The number TEXT stands for in BASE, 2 to 36."
  (unless (<= 2 base 36) (exc-calc-raise "#NUM!"))
  (let ((number 0))
    (dolist (char (string-to-list (upcase (string-trim text))) number)
      (let ((digit (cond ((<= ?0 char ?9) (- char ?0))
                         ((<= ?A char ?Z) (+ 10 (- char ?A))))))
        (unless (and digit (< digit base)) (exc-calc-raise "#NUM!"))
        (setq number (+ (* number base) digit))))))

(exc-calc-define base ((number integer) (base integer) &optional (places integer 0))
  "NUMBER written in BASE, 2 to 36, at least PLACES digits long."
  (unless (and (<= 2 base 36) (>= number 0) (<= 0 places 255))
    (exc-calc-raise "#NUM!"))
  (let ((digits (if (zerop number) "0"
                  (let (digits)
                    (while (> number 0)
                      (push (aref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (% number base))
                            digits)
                      (setq number (/ number base)))
                    (apply #'string digits)))))
    (if (< (length digits) places)
        (concat (make-string (- places (length digits)) ?0) digits)
      digits)))

;;;----------------------------------------------------------------------
;;; Tests and bits
;;;----------------------------------------------------------------------

(exc-calc-define delta ((first number) &optional (second number 0))
  "1 when FIRST and SECOND are equal, else 0."
  :toolpak t
  (if (= first second) 1 0))

(exc-calc-define gestep ((number number) &optional (step number 0))
  "1 when NUMBER is at least STEP, else 0."
  :toolpak t
  (if (>= number step) 1 0))

(defun exc-calc--bit-operand (number)
  "Return NUMBER as a bit operand, a whole number below 2^48."
  (unless (and (>= number 0) (< number (expt 2.0 48)) (= number (ftruncate (float number))))
    (exc-calc-raise "#NUM!"))
  (truncate number))

(exc-calc-define bitand ((a number) (b number))
  "The bits A and B both have."
  (logand (exc-calc--bit-operand a) (exc-calc--bit-operand b)))

(exc-calc-define bitor ((a number) (b number))
  "The bits either A or B has."
  (logior (exc-calc--bit-operand a) (exc-calc--bit-operand b)))

(exc-calc-define bitxor ((a number) (b number))
  "The bits one of A and B has, but not both."
  (logxor (exc-calc--bit-operand a) (exc-calc--bit-operand b)))

(defun exc-calc--bit-shift (number shift)
  "Return NUMBER's bits moved SHIFT places, to the left when it is positive.
A SHIFT past 53 either way is #NUM!, as is a result past 2^48-1."
  (when (> (abs shift) 53) (exc-calc-raise "#NUM!"))
  (let ((result (ash (exc-calc--bit-operand number) shift)))
    (when (>= result (expt 2 48)) (exc-calc-raise "#NUM!"))
    result))

(exc-calc-define bitlshift ((number number) (shift integer))
  "NUMBER's bits moved SHIFT places to the left."
  (exc-calc--bit-shift number shift))

(exc-calc-define bitrshift ((number number) (shift integer))
  "NUMBER's bits moved SHIFT places to the right."
  (exc-calc--bit-shift number (- shift)))

(defun exc-calc--polynomial (variable coefficients)
  "Return the value at VARIABLE of a polynomial, COEFFICIENTS constant first.
The series of the hyperbolic sine below and the fits of the Bessel
functions after it are polynomials in one variable."
  (let ((value 0.0))
    (dolist (coefficient (reverse coefficients) value)
      (setq value (+ coefficient (* variable value))))))

;;;----------------------------------------------------------------------
;;; Complex numbers
;;;----------------------------------------------------------------------

(defconst exc-calc--complex-number
  "\\([-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\)"
  "A real number, as complex numbers write their parts.")

(defun exc-calc--complex (value)
  "Return VALUE, a complex number as text or a number, as (REAL IMAG SUFFIX)."
  (cond
   ((numberp value) (list (float value) 0.0 nil))
   ((memq value '(:blank)) (list 0.0 0.0 nil))
   ((not (stringp value)) (exc-calc-raise "#VALUE!"))
   (t
    (let ((text value) (n exc-calc--complex-number) (case-fold-search nil))
      (cond
       ((string-empty-p text) (list 0.0 0.0 nil))
       ((string-match (concat "\\`" n "\\'") text)
        (list (float (string-to-number (match-string 1 text))) 0.0 nil))
       ((string-match (concat "\\`" n "?\\([-+]\\)?\\([0-9.eE+-]*\\)\\([ij]\\)\\'") text)
        (let* ((first (match-string 1 text))
               (sign (match-string 2 text))
               (digits (match-string 3 text))
               (suffix (match-string 4 text)))
          (cond
           ;; "4i", "-4i", "i": one part, the imaginary.
           ((and first (null sign) (string-empty-p digits))
            (list 0.0 (float (string-to-number first)) suffix))
           ((and (null first) (null sign) (string-empty-p digits))
            (list 0.0 1.0 suffix))
           ((and (null first) sign (string-empty-p digits))
            (list 0.0 (if (equal sign "-") -1.0 1.0) suffix))
           ((and first sign
                 (or (string-empty-p digits)
                     (string-match-p (concat "\\`" n "\\'") digits))
                 (not (string-match-p "\\`[-+]" digits)))
            (let ((imag (if (string-empty-p digits) 1.0
                          (float (string-to-number digits)))))
              (list (float (string-to-number first))
                    (if (equal sign "-") (- imag) imag)
                    suffix)))
           (t (exc-calc-raise "#NUM!")))))
       (t (exc-calc-raise "#NUM!")))))))

(defun exc-calc--complex-part (number)
  "Return NUMBER as the complex numbers write it, out in full if it fits.
Excel writes a part of a complex number in full wherever the digits
reach, where the text of a number alone would have turned to an
exponent much sooner: 1e-16 is 0.0000000000000001, 1e-20 is 1E-20."
  (let ((text (exc-calc-number-text number)))
    (if (and (string-match
              "\\`\\(-?\\)\\([0-9]\\)\\(?:\\.\\([0-9]*\\)\\)?E-\\([0-9]+\\)\\'"
              text)
             (<= (+ (length (concat (match-string 2 text)
                                    (or (match-string 3 text) "")))
                    (string-to-number (match-string 4 text)))
                 20))
        (let ((digits (concat (match-string 2 text) (or (match-string 3 text) "")))
              (exponent (string-to-number (match-string 4 text))))
          (concat (match-string 1 text) "0."
                  (make-string (1- exponent) ?0) digits))
      text)))

(defun exc-calc--complex-text (real imag &optional suffix)
  "Return the complex number REAL + IMAG SUFFIX as text, as Excel writes it."
  (let* ((suffix (or suffix "i"))
         (real (exc-calc--tidy-number (exc-calc--snap real)))
         (imag (exc-calc--tidy-number (exc-calc--snap imag)))
         (real-text (unless (zerop real) (exc-calc--complex-part real)))
         (imag-text (unless (zerop imag)
                      (concat (cond ((< imag 0) "-") (real-text "+") (t ""))
                              (if (= (abs imag) 1) "" (exc-calc--complex-part (abs imag)))
                              suffix))))
    (if (or real-text imag-text) (concat real-text imag-text) "0")))

(defun exc-calc--complex-suffix (&rest numbers)
  "Return the suffix NUMBERS, parsed complex numbers, share, or #VALUE!."
  (let ((suffixes (delete-dups (delq nil (mapcar (lambda (c) (nth 2 c)) numbers)))))
    (when (cdr suffixes) (exc-calc-raise "#VALUE!"))
    (car suffixes)))

(defun exc-calc--complex-suffix-pair (x y)
  "Return the suffix X and Y share, or #NUM!, as IMSUB and IMDIV do."
  (condition-case nil (exc-calc--complex-suffix x y)
    (exc-calc-error (exc-calc-raise "#NUM!"))))

(exc-calc-define complex ((real number) (imag number) &optional (suffix text "i"))
  "The complex number REAL + IMAG SUFFIX, SUFFIX being i or j."
  :toolpak t
  (unless (member suffix '("i" "j" "")) (exc-calc-raise "#VALUE!"))
  (exc-calc--complex-text real imag (if (string-empty-p suffix) "i" suffix)))

(exc-calc-define imreal ((number any))
  "The real part of the complex NUMBER."
  (exc-calc--tidy-number (nth 0 (exc-calc--complex number))))

(exc-calc-define imaginary ((number any))
  "The imaginary part of the complex NUMBER."
  (exc-calc--tidy-number (nth 1 (exc-calc--complex number))))

(exc-calc-define imabs ((number any))
  "How far the complex NUMBER is from 0."
  (pcase-let ((`(,a ,b ,_) (exc-calc--complex number)))
    (sqrt (+ (* a a) (* b b)))))

(exc-calc-define imargument ((number any))
  "The angle of the complex NUMBER, in radians."
  (pcase-let ((`(,a ,b ,_) (exc-calc--complex number)))
    (when (and (zerop a) (zerop b)) (exc-calc-raise "#DIV/0!"))
    (atan b a)))

(exc-calc-define imconjugate ((number any))
  "The complex NUMBER with its imaginary part negated."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-text a (- b) s)))

(defun exc-calc--complex-args (arguments)
  "Return the complex numbers of ARGUMENTS, `values' arguments.
An empty cell is zero here, so that IMPRODUCT of it is 0."
  (let (numbers)
    (dolist (argument arguments)
      (exc-calc-each-value argument
                            (lambda (value _)
                              (when (exc-calc-err-p value)
                                (exc-calc-raise (exc-calc-err-code value)))
                              (push (exc-calc--complex value) numbers))))
    (nreverse numbers)))

(exc-calc-define imsum (&rest (numbers values))
  "The sum of the complex NUMBERS."
  (let* ((all (exc-calc--complex-args numbers))
         (suffix (apply #'exc-calc--complex-suffix all)))
    (exc-calc--complex-text (apply #'+ 0.0 (mapcar #'car all))
                             (apply #'+ 0.0 (mapcar #'cadr all))
                             suffix)))

(exc-calc-define imsub ((a any) (b any))
  "The complex number A less B."
  (let* ((x (exc-calc--complex a)) (y (exc-calc--complex b)))
    (exc-calc--complex-text (- (nth 0 x) (nth 0 y)) (- (nth 1 x) (nth 1 y))
                             (exc-calc--complex-suffix-pair x y))))

(defun exc-calc--complex-times (x y)
  "Return the product of the complex numbers X and Y, as (REAL IMAG)."
  (list (- (* (nth 0 x) (nth 0 y)) (* (nth 1 x) (nth 1 y)))
        (+ (* (nth 0 x) (nth 1 y)) (* (nth 1 x) (nth 0 y)))))

(exc-calc-define improduct (&rest (numbers values))
  "The product of the complex NUMBERS."
  (let* ((all (exc-calc--complex-args numbers))
         (suffix (apply #'exc-calc--complex-suffix all))
         (product (list 1.0 0.0)))
    (dolist (number all)
      (setq product (exc-calc--complex-times product number)))
    (exc-calc--complex-text (nth 0 product) (nth 1 product) suffix)))

(exc-calc-define imdiv ((a any) (b any))
  "The complex number A divided by B."
  (let* ((x (exc-calc--complex a)) (y (exc-calc--complex b))
         (den (+ (* (nth 0 y) (nth 0 y)) (* (nth 1 y) (nth 1 y)))))
    (when (zerop den) (exc-calc-raise "#NUM!"))
    (exc-calc--complex-text
     (/ (+ (* (nth 0 x) (nth 0 y)) (* (nth 1 x) (nth 1 y))) den)
     (/ (- (* (nth 1 x) (nth 0 y)) (* (nth 0 x) (nth 1 y))) den)
     (exc-calc--complex-suffix-pair x y))))

(defun exc-calc--complex-polar (number)
  "Return NUMBER, a parsed complex number, as (MODULUS ANGLE SUFFIX)."
  (pcase-let ((`(,a ,b ,s) number))
    (list (sqrt (+ (* a a) (* b b))) (atan b a) s)))

(exc-calc-define imln ((number any))
  "The natural logarithm of the complex NUMBER."
  (pcase-let ((`(,r ,theta ,s) (exc-calc--complex-polar (exc-calc--complex number))))
    (when (zerop r) (exc-calc-raise "#NUM!"))
    (exc-calc--complex-text (log r) theta s)))

(exc-calc-define imlog10 ((number any))
  "The base 10 logarithm of the complex NUMBER."
  (pcase-let ((`(,r ,theta ,s) (exc-calc--complex-polar (exc-calc--complex number))))
    (when (zerop r) (exc-calc-raise "#NUM!"))
    (exc-calc--complex-text (/ (log r) (log 10)) (/ theta (log 10)) s)))

(exc-calc-define imlog2 ((number any))
  "The base 2 logarithm of the complex NUMBER."
  (pcase-let ((`(,r ,theta ,s) (exc-calc--complex-polar (exc-calc--complex number))))
    (when (zerop r) (exc-calc-raise "#NUM!"))
    (exc-calc--complex-text (/ (log r) (log 2)) (/ theta (log 2)) s)))

(exc-calc-define imexp ((number any))
  "e to the power of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-text (* (exp a) (cos b)) (* (exp a) (sin b)) s)))

(exc-calc-define impower ((number any) (power number))
  "The complex NUMBER to the real POWER."
  (pcase-let ((`(,r ,theta ,s) (exc-calc--complex-polar (exc-calc--complex number))))
    (when (and (zerop r) (<= power 0)) (exc-calc-raise "#NUM!"))
    (let ((modulus (expt r power)) (angle (* theta power)))
      (exc-calc--complex-text (* modulus (cos angle)) (* modulus (sin angle)) s))))

(exc-calc-define imsqrt ((number any))
  "The square root of the complex NUMBER."
  (pcase-let ((`(,r ,theta ,s) (exc-calc--complex-polar (exc-calc--complex number))))
    (let ((modulus (sqrt r)) (angle (/ theta 2)))
      (exc-calc--complex-text (* modulus (cos angle)) (* modulus (sin angle)) s))))

(defun exc-calc--sinh (x)
  "Return the hyperbolic sine of X."
  (if (< (abs x) 2.5)
      ;; Its series is of one sign throughout, so it keeps every digit,
      ;; where the difference of two exponentials rounds the small ones
      ;; away; the terms are summed as Kahan sums them, to that end.
      (let ((square (* x x)) (term x) (sum x) (carry 0.0) (k 0))
        (while (and (< k 100) (> (abs term) (* 1e-19 (abs sum))))
          (setq k (1+ k)
                term (/ (* term square) (* 2 k (1+ (* 2 k)))))
          (let* ((part (- term carry))
                 (total (+ sum part)))
            (setq carry (- (- total sum) part)
                  sum total)))
        sum)
    (/ (- (exp x) (exp (- x))) 2)))

(defun exc-calc--cosh (x)
  "Return the hyperbolic cosine of X."
  (/ (+ (exp x) (exp (- x))) 2))

(exc-calc-define imsin ((number any))
  "The sine of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-text (* (sin a) (exc-calc--cosh b)) (* (cos a) (exc-calc--sinh b)) s)))

(exc-calc-define imcos ((number any))
  "The cosine of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-text (* (cos a) (exc-calc--cosh b)) (- (* (sin a) (exc-calc--sinh b))) s)))

(exc-calc-define imsinh ((number any))
  "The hyperbolic sine of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-text (* (exc-calc--sinh a) (cos b))
                             (* (exc-calc--cosh a) (sin b)) s)))

(exc-calc-define imcosh ((number any))
  "The hyperbolic cosine of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-text (* (exc-calc--cosh a) (cos b))
                             (* (exc-calc--sinh a) (sin b)) s)))

(exc-calc-define imtan ((number any))
  "The tangent of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (let ((den (+ (cos (* 2 a)) (exc-calc--cosh (* 2 b)))))
      (when (zerop den) (exc-calc-raise "#NUM!"))
      (exc-calc--complex-text (/ (sin (* 2 a)) den)
                               (/ (exc-calc--sinh (* 2 b)) den) s))))

(defun exc-calc--complex-reciprocal (real imag suffix)
  "Return 1 over the complex number REAL + IMAG SUFFIX, as text."
  (let ((den (+ (* real real) (* imag imag))))
    (when (zerop den) (exc-calc-raise "#NUM!"))
    (exc-calc--complex-text (/ real den) (/ (- imag) den) suffix)))

(exc-calc-define imcot ((number any))
  "The cotangent of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (let ((den (+ (cos (* 2 a)) (exc-calc--cosh (* 2 b)))))
      (when (zerop den) (exc-calc-raise "#NUM!"))
      (exc-calc--complex-reciprocal (/ (sin (* 2 a)) den)
                                     (/ (exc-calc--sinh (* 2 b)) den) s))))

(exc-calc-define imsec ((number any))
  "The secant of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-reciprocal (* (cos a) (exc-calc--cosh b))
                                   (- (* (sin a) (exc-calc--sinh b))) s)))

(exc-calc-define imsech ((number any))
  "The hyperbolic secant of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-reciprocal (* (exc-calc--cosh a) (cos b))
                                   (* (exc-calc--sinh a) (sin b)) s)))

(exc-calc-define imcsc ((number any))
  "The cosecant of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-reciprocal (* (sin a) (exc-calc--cosh b))
                                   (* (cos a) (exc-calc--sinh b)) s)))

(exc-calc-define imcsch ((number any))
  "The hyperbolic cosecant of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (exc-calc--complex number)))
    (exc-calc--complex-reciprocal (* (exc-calc--sinh a) (cos b))
                                   (* (exc-calc--cosh a) (sin b)) s)))

;;;----------------------------------------------------------------------
;;; Units
;;;----------------------------------------------------------------------

(defconst exc-calc--unit-prefixes
  '(("Y" . 1e24) ("Z" . 1e21) ("E" . 1e18) ("P" . 1e15) ("T" . 1e12)
    ("G" . 1e9) ("M" . 1e6) ("k" . 1e3) ("h" . 1e2) ("da" . 1e1)
    ("e" . 1e1) ("d" . 1e-1) ("c" . 1e-2) ("m" . 1e-3) ("u" . 1e-6)
    ("n" . 1e-9) ("p" . 1e-12) ("f" . 1e-15) ("a" . 1e-18)
    ("z" . 1e-21) ("y" . 1e-24))
  "The prefixes CONVERT may put before a unit, and what they multiply it by.")

(defconst exc-calc--unit-binary-prefixes
  '(("Yi" . 1.2089258196146292e24) ("Zi" . 1.1805916207174113e21)
    ("Ei" . 1.152921504606847e18) ("Pi" . 1.125899906842624e15)
    ("Ti" . 1.099511627776e12) ("Gi" . 1.073741824e9)
    ("Mi" . 1048576.0) ("ki" . 1024.0))
  "The binary prefixes, which only bits and bytes take.")

(defconst exc-calc--units
  ;; UNIT, what it measures, how many of the measure's own unit it is, and
  ;; whether a prefix may go before it, as Excel's workbooks have them.  A
  ;; degree's scale and offset are in `exc-calc--degrees' instead.
  '(("g" "mass" 1.0 t)
    ("sg" "mass" 14593.902937206363 nil)
    ("lbm" "mass" 453.59237 nil)
    ("u" "mass" 1.660538782e-24 t)
    ("ozm" "mass" 28.349523125 nil)
    ("grain" "mass" 0.06479891 nil)
    ("cwt" "mass" 45359.237 nil)
    ("shweight" "mass" 45359.237 nil)
    ("uk_cwt" "mass" 50802.34544 nil)
    ("lcwt" "mass" 50802.34544 nil)
    ("hweight" "mass" 50802.34544 nil)
    ("stone" "mass" 6350.29318 nil)
    ("ton" "mass" 907184.74 nil)
    ("brton" "mass" 1016046.9088 nil)
    ("LTON" "mass" 1016046.9088 nil)
    ("uk_ton" "mass" 1016046.9088 nil)
    ("m" "distance" 1.0 t)
    ("mi" "distance" 1609.344 nil)
    ("Nmi" "distance" 1852.0 nil)
    ("in" "distance" 0.0254 nil)
    ("ft" "distance" 0.3048 nil)
    ("yd" "distance" 0.9144 nil)
    ("ang" "distance" 1e-10 t)
    ("ell" "distance" 1.143 nil)
    ("ly" "distance" 9460730472580800.0 t)
    ("parsec" "distance" 3.0856775812815532e+16 t)
    ("pc" "distance" 3.0856775812815532e+16 t)
    ("Picapt" "distance" 0.0003527777777777778 nil)
    ("Pica" "distance" 0.0003527777777777778 nil)
    ("pica" "distance" 0.004233333333333334 nil)
    ("survey_mi" "distance" 1609.3472186944373 nil)
    ("yr" "time" 31557600.0 nil)
    ("day" "time" 86400.0 nil)
    ("d" "time" 86400.0 nil)
    ("hr" "time" 3600.0 nil)
    ("mn" "time" 60.0 nil)
    ("min" "time" 60.0 nil)
    ("sec" "time" 1.0 t)
    ("s" "time" 1.0 t)
    ("Pa" "pressure" 1.0 t)
    ("p" "pressure" 1.0 t)
    ("atm" "pressure" 101325.0 t)
    ("at" "pressure" 101325.0 t)
    ("mmHg" "pressure" 133.322 t)
    ("psi" "pressure" 6894.757293168362 nil)
    ("Torr" "pressure" 133.32236842105263 nil)
    ("N" "force" 1.0 t)
    ("dyn" "force" 1e-05 t)
    ("dy" "force" 1e-05 t)
    ("lbf" "force" 4.4482216152605005 nil)
    ("pond" "force" 0.00980665 t)
    ("J" "energy" 1.0 t)
    ("e" "energy" 1e-07 t)
    ("c" "energy" 4.184 t)
    ("cal" "energy" 4.1868 t)
    ("eV" "energy" 1.602176487e-19 t)
    ("ev" "energy" 1.602176487e-19 t)
    ("HPh" "energy" 2684519.537696173 nil)
    ("hh" "energy" 2684519.537696173 nil)
    ("Wh" "energy" 3600.0 t)
    ("wh" "energy" 3600.0 t)
    ("flb" "energy" 1.3558179483314003 nil)
    ("BTU" "energy" 1055.05585262 nil)
    ("btu" "energy" 1055.05585262 nil)
    ("HP" "power" 745.6998715822702 nil)
    ("h" "power" 745.6998715822702 nil)
    ("PS" "power" 735.49875 nil)
    ("W" "power" 1.0 t)
    ("w" "power" 1.0 t)
    ("T" "magnetism" 1.0 t)
    ("ga" "magnetism" 0.0001 t)
    ("bit" "information" 1.0 t)
    ("byte" "information" 8.0 t)
    ("admkn" "speed" 0.5147733333333333 nil)
    ("kn" "speed" 0.5144444444444445 nil)
    ("m/h" "speed" 0.0002777777777777778 t)
    ("m/hr" "speed" 0.0002777777777777778 t)
    ("m/s" "speed" 1.0 t)
    ("m/sec" "speed" 1.0 t)
    ("mph" "speed" 0.44704 t)
    ("C" "temperature" 1.0 nil)
    ("cel" "temperature" 1.0 nil)
    ("F" "temperature" 0.5555555555555556 nil)
    ("fah" "temperature" 0.5555555555555556 nil)
    ("K" "temperature" 1.0 t)
    ("kel" "temperature" 1.0 t)
    ("Rank" "temperature" 0.5555555555555556 nil)
    ("Reau" "temperature" 1.25 nil)
    ("tsp" "volume" 4.92892159375e-06 nil)
    ("tspm" "volume" 5e-06 nil)
    ("tbs" "volume" 1.4786764781249999e-05 nil)
    ("oz" "volume" 2.9573529562499998e-05 nil)
    ("cup" "volume" 0.00023658823649999998 nil)
    ("pt" "volume" 0.00047317647299999996 nil)
    ("us_pt" "volume" 0.00047317647299999996 nil)
    ("uk_pt" "volume" 0.00056826125 t)
    ("qt" "volume" 0.0009463529459999999 nil)
    ("uk_qt" "volume" 0.0011365225 nil)
    ("gal" "volume" 0.0037854117839999997 nil)
    ("uk_gal" "volume" 0.00454609 nil)
    ("l" "volume" 0.001 t)
    ("L" "volume" 0.001 t)
    ("lt" "volume" 0.001 t)
    ("ang3" "volume" 1e-30 t)
    ("ang^3" "volume" 1e-30 t)
    ("barrel" "volume" 0.158987294928 nil)
    ("bushel" "volume" 0.03523907016688 nil)
    ("ft3" "volume" 0.028316846592 nil)
    ("ft^3" "volume" 0.028316846592 nil)
    ("in3" "volume" 1.6387064e-05 nil)
    ("in^3" "volume" 1.6387064e-05 nil)
    ("ly3" "volume" 8.467866646237152e+47 nil)
    ("ly^3" "volume" 8.467866646237152e+47 nil)
    ("m3" "volume" 1.0 t)
    ("m^3" "volume" 1.0 t)
    ("mi3" "volume" 4168181825.4405794 nil)
    ("mi^3" "volume" 4168181825.4405794 nil)
    ("yd3" "volume" 0.764554857984 nil)
    ("yd^3" "volume" 0.764554857984 nil)
    ("Nmi3" "volume" 6352182208.0 nil)
    ("Nmi^3" "volume" 6352182208.0 nil)
    ("Picapt3" "volume" 4.3903956618655696e-11 nil)
    ("Picapt^3" "volume" 4.3903956618655696e-11 nil)
    ("Pica3" "volume" 4.3903956618655696e-11 nil)
    ("Pica^3" "volume" 4.3903956618655696e-11 nil)
    ("GRT" "volume" 2.8316846592 nil)
    ("regton" "volume" 2.8316846592 nil)
    ("MTON" "volume" 1.13267386368 nil)
    ("uk_acre" "area" 4046.8564224 nil)
    ("us_acre" "area" 4046.8726098742522 nil)
    ("ang2" "area" 1e-20 t)
    ("ang^2" "area" 1e-20 t)
    ("ar" "area" 100.0 t)
    ("ft2" "area" 0.09290304 nil)
    ("ft^2" "area" 0.09290304 nil)
    ("ha" "area" 10000.0 nil)
    ("in2" "area" 0.00064516 nil)
    ("in^2" "area" 0.00064516 nil)
    ("ly2" "area" 8.95054210748189e+31 nil)
    ("ly^2" "area" 8.95054210748189e+31 nil)
    ("m2" "area" 1.0 t)
    ("m^2" "area" 1.0 t)
    ("Morgen" "area" 2500.0 nil)
    ("mi2" "area" 2589988.110336 nil)
    ("mi^2" "area" 2589988.110336 nil)
    ("Nmi2" "area" 3429904.0 nil)
    ("Nmi^2" "area" 3429904.0 nil)
    ("Picapt2" "area" 1.2445216049382715e-07 nil)
    ("Pica2" "area" 1.2445216049382715e-07 nil)
    ("Pica^2" "area" 1.2445216049382715e-07 nil)
    ("Picapt^2" "area" 1.2445216049382715e-07 nil)
    ("yd2" "area" 0.83612736 nil)
    ("yd^2" "area" 0.83612736 nil))
  "The units CONVERT knows, by their names in Excel's documentation.")

(defun exc-calc--unit-power (unit)
  "Return how many lengths of the unit UNIT a prefix scales: two if it squares."
  (cond ((or (string-suffix-p "2" unit) (string-suffix-p "^2" unit)) 2)
        ((or (string-suffix-p "3" unit) (string-suffix-p "^3" unit)) 3)
        (t 1)))

(defun exc-calc--unit-options (name)
  "Return ((ROW . MULTIPLIER) ...) for each reading of the unit NAME.
The unit itself comes first, then those a prefix before it makes, since
a name can be both: pc is a parsec, and a picocalorie."
  (let ((options nil)
        (row (assoc name exc-calc--units))
        (prefixes (append exc-calc--unit-prefixes exc-calc--unit-binary-prefixes)))
    (when row (push (cons row 1.0) options))
    (dolist (pair prefixes)
      (let* ((length (length (car pair)))
             (rest (and (> (length name) length)
                        (equal (substring name 0 length) (car pair))
                        (substring name length)))
             (row (and rest (assoc rest exc-calc--units))))
        (when (and row (nth 3 row)
                   (or (not (assoc (car pair) exc-calc--unit-binary-prefixes))
                       (member rest '("bit" "byte"))))
          (push (cons row (expt (cdr pair) (exc-calc--unit-power (nth 0 row))))
                options))))
    (nreverse options)))

(defun exc-calc--unit-pair (from to)
  "Return (FROM-UNIT . TO-UNIT), readings of FROM and TO of one measure, or nil."
  (let ((to-units (exc-calc--unit-options to))
        (pair nil))
    (dolist (from-unit (exc-calc--unit-options from))
      (dolist (to-unit to-units)
        (when (and (null pair)
                   (equal (nth 1 (car from-unit)) (nth 1 (car to-unit))))
          (setq pair (cons from-unit to-unit)))))
    pair))

(defconst exc-calc--degrees
  ;; A degree's value, in Celsius, is the number times the first of these
  ;; and then the second added to it.
  '(("C" 1.0 0.0) ("cel" 1.0 0.0)
    ("F" 0.5555555555555556 -17.77777777777778)
    ("fah" 0.5555555555555556 -17.77777777777778)
    ("K" 1.0 -273.15) ("kel" 1.0 -273.15)
    ("Rank" 0.5555555555555556 -273.15)
    ("Reau" 1.25 0.0))
  "The scale and the offset of each degree CONVERT knows.")

(exc-calc-define convert ((number number) (from text) (to text))
  "NUMBER in the unit FROM written in the unit TO."
  :toolpak t
  (let* ((pair (exc-calc--unit-pair from to))
         (from-unit (car pair))
         (to-unit (cdr pair)))
    (unless pair (exc-calc-raise "#N/A"))
    (if (equal "temperature" (nth 1 (car from-unit)))
        ;; Degrees of one scale go through Celsius, which leaves the
        ;; plain ones, as 13 Celsius is 55.4 Fahrenheit, exact.
        (let ((from-degrees (cdr (assoc (nth 0 (car from-unit)) exc-calc--degrees)))
              (to-degrees (cdr (assoc (nth 0 (car to-unit)) exc-calc--degrees))))
          (exc-calc--finite
           (/ (+ (* number (cdr from-unit) (nth 0 from-degrees))
                 (- (nth 1 from-degrees) (nth 1 to-degrees)))
              (* (cdr to-unit) (nth 0 to-degrees)))))
      (exc-calc--finite
       (/ (* number (cdr from-unit) (nth 2 (car from-unit)))
          (* (cdr to-unit) (nth 2 (car to-unit))))))))

;;;----------------------------------------------------------------------
;;; Bessel functions
;;;----------------------------------------------------------------------
;;
;; A workbook's cached values are Excel's, and Excel computed them with
;; the polynomial approximations of "Numerical Recipes in C" and the
;; recurrences beside them; so they are the ones here, to the digits
;; those fits keep.

(defconst exc-calc--bessel-rescale 1.0e10
  "Above this, the recurrences that run backwards are scaled down.")

(defun exc-calc--bessel-j0 (x)
  "Return the Bessel function J0 of X."
  (let ((ax (abs x)))
    (if (< ax 8.0)
        (let ((y (* x x)))
          (/ (exc-calc--polynomial
              y '(57568490574.0 -13362590354.0 651619640.7 -11214424.18
                  77392.33017 -184.9052456))
             (exc-calc--polynomial
              y '(57568490411.0 1029532985.0 9494680.718 59272.64853
                  267.8532712 1.0))))
      (let* ((z (/ 8.0 ax))
             (y (* z z))
             (phase (- ax 0.785398164))
             (ans1 (exc-calc--polynomial
                    y '(1.0 -0.1098628627e-2 0.2734510407e-4
                         -0.2073370639e-5 0.2093887211e-6)))
             (ans2 (exc-calc--polynomial
                    y '(-0.1562499995e-1 0.1430488765e-3 -0.6911147651e-5
                         0.7621095161e-6 -0.934935152e-7))))
        (* (sqrt (/ 0.636619772 ax))
           (- (* (cos phase) ans1) (* z (sin phase) ans2)))))))

(defun exc-calc--bessel-j1 (x)
  "Return the Bessel function J1 of X."
  (let ((ax (abs x)))
    (if (< ax 8.0)
        (let ((y (* x x)))
          (/ (* x (exc-calc--polynomial
                   y '(72362614232.0 -7895059235.0 242396853.1 -2972611.439
                       15704.48260 -30.16036606)))
             (exc-calc--polynomial
              y '(144725228442.0 2300535178.0 18583304.74 99447.43394
                  376.9991397 1.0))))
      (let* ((z (/ 8.0 ax))
             (y (* z z))
             (phase (- ax 2.356194491))
             (ans1 (exc-calc--polynomial
                    y '(1.0 0.183105e-2 -0.3516396496e-4 0.2457520174e-5
                         -0.240337019e-6)))
             (ans2 (exc-calc--polynomial
                    y '(0.04687499995 -0.2002690873e-3 0.8449199096e-5
                         -0.88228987e-6 0.105787412e-6)))
             (ans (* (sqrt (/ 0.636619772 ax))
                     (- (* (cos phase) ans1) (* z (sin phase) ans2)))))
        (if (< x 0.0) (- ans) ans)))))

(defun exc-calc--bessel-y0 (x)
  "Return the Bessel function Y0 of X."
  (if (< x 8.0)
      (let ((y (* x x)))
        (+ (/ (exc-calc--polynomial
               y '(-2957821389.0 7062834065.0 -512359803.6 10879881.29
                   -86327.92757 228.4622733))
              (exc-calc--polynomial
               y '(40076544269.0 745249964.8 7189466.438 47447.26470
                   226.1030244 1.0)))
           (* 0.636619772 (exc-calc--bessel-j0 x) (log x))))
    (let* ((z (/ 8.0 x))
           (y (* z z))
           (phase (- x 0.785398164))
           (ans1 (exc-calc--polynomial
                  y '(1.0 -0.1098628627e-2 0.2734510407e-4
                       -0.2073370639e-5 0.2093887211e-6)))
           (ans2 (exc-calc--polynomial
                  y '(-0.1562499995e-1 0.1430488765e-3 -0.6911147651e-5
                       0.7621095161e-6 -0.934935152e-7))))
      (* (sqrt (/ 0.636619772 x))
         (+ (* (sin phase) ans1) (* z (cos phase) ans2))))))

(defun exc-calc--bessel-y1 (x)
  "Return the Bessel function Y1 of X."
  (if (< x 8.0)
      (let ((y (* x x)))
        (+ (/ (* x (exc-calc--polynomial
                    y '(-0.4900604943e13 0.1275274390e13 -0.5153438139e11
                        0.7349264551e9 -0.4237922726e7 0.8511937935e4)))
              (exc-calc--polynomial
               y '(0.2499580570e14 0.4244419664e12 0.3733650367e10
                   0.2245904002e8 0.1020426050e6 0.3549632885e3 1.0)))
           (* 0.636619772 (- (* (exc-calc--bessel-j1 x) (log x)) (/ 1.0 x)))))
    (let* ((z (/ 8.0 x))
           (y (* z z))
           (phase (- x 2.356194491))
           (ans1 (exc-calc--polynomial
                  y '(1.0 0.183105e-2 -0.3516396496e-4 0.2457520174e-5
                       -0.240337019e-6)))
           (ans2 (exc-calc--polynomial
                  y '(0.04687499995 -0.2002690873e-3 0.8449199096e-5
                       -0.88228987e-6 0.105787412e-6))))
      (* (sqrt (/ 0.636619772 x))
         (+ (* (sin phase) ans1) (* z (cos phase) ans2))))))

(defun exc-calc--bessel-i0 (x)
  "Return the modified Bessel function I0 of X."
  (let ((ax (abs x)))
    (if (< ax 3.75)
        (let ((y (/ x 3.75)))
          (exc-calc--polynomial
           (* y y) '(1.0 3.5156229 3.0899424 1.2067492 0.2659732 0.360768e-1
                          0.45813e-2)))
      (let ((y (/ 3.75 ax)))
        (* (/ (exp ax) (sqrt ax))
           (exc-calc--polynomial
            y '(0.39894228 0.1328592e-1 0.225319e-2 -0.157565e-2 0.916281e-2
                           -0.2057706e-1 0.2635537e-1 -0.1647633e-1
                           0.392377e-2)))))))

(defun exc-calc--bessel-i1 (x)
  "Return the modified Bessel function I1 of X."
  (let* ((ax (abs x))
         (ans (if (< ax 3.75)
                  (let ((y (/ x 3.75)))
                    (* ax (exc-calc--polynomial
                           (* y y) '(0.5 0.87890594 0.51498869 0.15084934
                                         0.2658733e-1 0.301532e-2 0.32411e-3))))
                (let* ((y (/ 3.75 ax))
                       (inner (exc-calc--polynomial
                               y '(0.2282967e-1 -0.2895312e-1 0.1787654e-1
                                     -0.420059e-2))))
                  (* (/ (exp ax) (sqrt ax))
                     (exc-calc--polynomial
                      y (list 0.39894228 -0.3988024e-1 -0.362018e-2
                              0.163801e-2 -0.1031555e-1 inner)))))))
    (if (< x 0.0) (- ans) ans)))

(defun exc-calc--bessel-k0 (x)
  "Return the modified Bessel function K0 of X."
  (if (<= x 2.0)
      (let ((y (/ (* x x) 4.0)))
        (+ (- (* (log (/ x 2.0)) (exc-calc--bessel-i0 x)))
           (exc-calc--polynomial
            y '(-0.57721566 0.42278420 0.23069756 0.3488590e-1 0.262698e-2
                 0.10750e-3 0.74e-5))))
    (let ((y (/ 2.0 x)))
      (* (/ (exp (- x)) (sqrt x))
         (exc-calc--polynomial
          y '(1.25331414 -0.7832358e-1 0.2189568e-1 -0.1062446e-1 0.587872e-2
                         -0.251540e-2 0.53208e-3))))))

(defun exc-calc--bessel-k1 (x)
  "Return the modified Bessel function K1 of X."
  (if (<= x 2.0)
      (let ((y (/ (* x x) 4.0)))
        (+ (* (log (/ x 2.0)) (exc-calc--bessel-i1 x))
           (* (/ 1.0 x)
              (exc-calc--polynomial
               y '(1.0 0.15443144 -0.67278579 -0.18156897 -0.1919402e-1
                     -0.110404e-2 -0.4686e-4)))))
    (let ((y (/ 2.0 x)))
      (* (/ (exp (- x)) (sqrt x))
         (exc-calc--polynomial
          y '(1.25331414 0.23498619 -0.3655620e-1 0.1504268e-1 -0.780353e-2
                         0.325614e-2 -0.68245e-3))))))

(defun exc-calc--bessel-j (n x)
  "Return the Bessel function J of order N at X."
  ;; An even order is the same at -X and at X, an odd one the negation.
  (let ((value (cond ((= n 0) (exc-calc--bessel-j0 x))
                     ((zerop x) 0.0)
                     ((= n 1) (exc-calc--bessel-j1 (abs x)))
                     ((> (abs x) (float n))
                      (let ((step (/ 2.0 (abs x)))
                            (below (exc-calc--bessel-j0 (abs x)))
                            (value (exc-calc--bessel-j1 (abs x)))
                            (order 1))
                        (while (< order n)
                          (let ((next (- (* (float order) step value) below)))
                            (setq below value
                                  value next))
                          (setq order (1+ order)))
                        value))
                     (t
                      ;; Down from an order past N, where the values have
                      ;; died away to nothing, then scaled so that J0 is
                      ;; what it should be.
                      (let* ((step (/ 2.0 (abs x)))
                             (start (* 2 (truncate (+ n (sqrt (* 40.0 n))) 2)))
                             (below 0.0)
                             (value 1.0)
                             (answer 0.0))
                        (dotimes (i start)
                          (let* ((order (- start i))
                                 (next (- (* order step value) below)))
                            (setq below value
                                  value next)
                            (when (> (abs value) exc-calc--bessel-rescale)
                              (setq value (/ value exc-calc--bessel-rescale)
                                    below (/ below exc-calc--bessel-rescale)
                                    answer (/ answer exc-calc--bessel-rescale)))
                            (when (= order n) (setq answer below))))
                        (* answer (/ (exc-calc--bessel-j0 (abs x)) value)))))))
    (if (and (< x 0.0) (= 1 (logand n 1))) (- value) value)))

(defun exc-calc--bessel-y (n x)
  "Return the Bessel function Y of order N at X."
  (cond
   ((= n 0) (exc-calc--bessel-y0 x))
   ((= n 1) (exc-calc--bessel-y1 x))
   (t
    (let ((step (/ 2.0 x))
          (below (exc-calc--bessel-y0 x))
          (value (exc-calc--bessel-y1 x))
          (order 1))
      (while (< order n)
        (let ((next (- (* (float order) step value) below)))
          (setq below value
                value next))
        (setq order (1+ order)))
      value))))

(defun exc-calc--bessel-i (n x)
  "Return the modified Bessel function I of order N at X."
  (cond
   ((= n 0) (exc-calc--bessel-i0 x))
   ((= n 1) (exc-calc--bessel-i1 x))
   ((zerop x) 0.0)
   (t
    (let* ((step (/ 2.0 (abs x)))
           (start (* 2 (+ n (truncate (sqrt (* 40.0 n))))))
           (below 0.0)
           (value 1.0)
           (answer 0.0))
      (dotimes (i start)
        (let* ((order (- start i))
               (next (+ below (* order step value))))
          (setq below value
                value next)
          (when (> (abs value) exc-calc--bessel-rescale)
            (setq value (/ value exc-calc--bessel-rescale)
                  below (/ below exc-calc--bessel-rescale)
                  answer (/ answer exc-calc--bessel-rescale)))
          (when (= order n) (setq answer below))))
      (let ((result (* answer (/ (exc-calc--bessel-i0 x) value))))
        (if (and (< x 0.0) (= 1 (logand n 1))) (- result) result))))))

(defun exc-calc--bessel-k (n x)
  "Return the modified Bessel function K of order N at X."
  (cond
   ((= n 0) (exc-calc--bessel-k0 x))
   ((= n 1) (exc-calc--bessel-k1 x))
   (t
    (let ((step (/ 2.0 x))
          (below (exc-calc--bessel-k0 x))
          (value (exc-calc--bessel-k1 x))
          (order 1))
      (while (< order n)
        (let ((next (+ (* (float order) step value) below)))
          (setq below value
                value next))
        (setq order (1+ order)))
      value))))

(exc-calc-define besseli ((x number) (n integer))
  "The modified Bessel function I of order N at X."
  :toolpak t
  (when (< n 0) (exc-calc-raise "#NUM!"))
  (exc-calc--finite (exc-calc--bessel-i n x)))

(exc-calc-define besselj ((x number) (n integer))
  "The Bessel function J of order N at X."
  :toolpak t
  (when (< n 0) (exc-calc-raise "#NUM!"))
  (exc-calc--finite (exc-calc--bessel-j n x)))

(exc-calc-define besselk ((x number) (n integer))
  "The modified Bessel function K of order N at X."
  :toolpak t
  (when (or (< n 0) (<= x 0)) (exc-calc-raise "#NUM!"))
  (exc-calc--finite (exc-calc--bessel-k n x)))

(exc-calc-define bessely ((x number) (n integer))
  "The Bessel function Y of order N at X."
  :toolpak t
  (when (or (< n 0) (<= x 0)) (exc-calc-raise "#NUM!"))
  (exc-calc--finite (exc-calc--bessel-y n x)))

(provide 'exc-calc-engineering)

;;; exc-calc-engineering.el ends here
