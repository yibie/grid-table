;;; grid-table-calc-engineering.el --- Engineering functions -*- lexical-binding: t -*-

;;; Commentary:

;; Numbers in binary, octal and hexadecimal, tests of numbers against
;; each other, and complex numbers.  A number in another base is text of
;; up to ten digits, its top bit the sign, as Excel keeps them.  A
;; complex number is text too, as "3+4i" or "-2j".

;;; Code:

(require 'grid-table-calc)
(require 'grid-table-calc-math)

;;;----------------------------------------------------------------------
;;; Bases
;;;----------------------------------------------------------------------

(defconst grid-calc--bases
  '((2 . 10) (8 . 30) (16 . 40))
  "For each base, the bits its ten digits hold.")

(defun grid-calc--from-base (text base)
  "Return the number TEXT, up to ten digits in BASE, stands for."
  (let* ((text (upcase (if (numberp text) (grid-calc-number-text text) text)))
         (bits (cdr (assq base grid-calc--bases)))
         (digits (pcase base (2 "01") (8 "0-7") (_ "0-9A-F"))))
    (unless (and (<= (length text) 10)
                 (string-match-p (format "\\`[%s]*\\'" digits) text))
      (grid-calc-raise "#NUM!"))
    (let ((number (if (string-empty-p text) 0 (string-to-number text base))))
      ;; Ten digits whose top bit is set are a negative number.
      (if (>= number (ash 1 (1- bits))) (- number (ash 1 bits)) number))))

(defun grid-calc--to-base (number base places)
  "Return NUMBER written in BASE, at least PLACES digits when given."
  (let* ((bits (cdr (assq base grid-calc--bases)))
         (number (truncate number)))
    (unless (and (>= number (- (ash 1 (1- bits)))) (< number (ash 1 (1- bits))))
      (grid-calc-raise "#NUM!"))
    (if (< number 0)
        ;; A negative number is always ten digits, PLACES or not.
        (grid-calc--base-digits (+ number (ash 1 bits)) base)
      (let ((text (grid-calc--base-digits number base)))
        (cond ((null places) text)
              ((or (< places 0) (> places 10) (< places (length text)))
               (grid-calc-raise "#NUM!"))
              (t (concat (make-string (- places (length text)) ?0) text)))))))

(defun grid-calc--base-digits (number base)
  "Return NUMBER, not negative, as digits in BASE."
  (if (zerop number) "0"
    (let (digits)
      (while (> number 0)
        (push (aref "0123456789ABCDEF" (% number base)) digits)
        (setq number (/ number base)))
      (apply #'string digits))))

(defmacro grid-calc--define-bases (&rest pairs)
  "Define the functions converting between each of PAIRS of bases.
Each pair is (NAME BASE), as (bin 2)."
  `(progn
     ,@(cl-loop
        for (from from-base) in pairs
        append
        (append
         (list `(grid-calc-define ,(intern (format "%s2dec" from)) ((text any))
                  ,(format "The number TEXT, in base %d, stands for." from-base)
                  (grid-calc--from-base text ,from-base))
               `(grid-calc-define ,(intern (format "dec2%s" from))
                    ((number number) &optional (places integer))
                  ,(format "NUMBER in base %d, at least PLACES digits long." from-base)
                  (grid-calc--to-base number ,from-base places)))
         (cl-loop for (to to-base) in pairs
                  unless (eq from to)
                  collect
                  `(grid-calc-define ,(intern (format "%s2%s" from to))
                       ((text any) &optional (places integer))
                     ,(format "TEXT, in base %d, in base %d." from-base to-base)
                     (grid-calc--to-base (grid-calc--from-base text ,from-base)
                                         ,to-base places)))))))

(grid-calc--define-bases (bin 2) (oct 8) (hex 16))

(grid-calc-define decimal ((text text) (base integer))
  "The number TEXT stands for in BASE, 2 to 36."
  (unless (<= 2 base 36) (grid-calc-raise "#NUM!"))
  (let ((number 0))
    (dolist (char (string-to-list (upcase (string-trim text))) number)
      (let ((digit (cond ((<= ?0 char ?9) (- char ?0))
                         ((<= ?A char ?Z) (+ 10 (- char ?A))))))
        (unless (and digit (< digit base)) (grid-calc-raise "#NUM!"))
        (setq number (+ (* number base) digit))))))

(grid-calc-define base ((number integer) (base integer) &optional (places integer 0))
  "NUMBER written in BASE, 2 to 36, at least PLACES digits long."
  (unless (and (<= 2 base 36) (>= number 0) (<= 0 places 255))
    (grid-calc-raise "#NUM!"))
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

(grid-calc-define delta ((first number) &optional (second number 0))
  "1 when FIRST and SECOND are equal, else 0."
  (if (= first second) 1 0))

(grid-calc-define gestep ((number number) &optional (step number 0))
  "1 when NUMBER is at least STEP, else 0."
  (if (>= number step) 1 0))

(defun grid-calc--bit-operand (number)
  "Return NUMBER as a bit operand, a whole number below 2^48."
  (unless (and (>= number 0) (< number (expt 2.0 48)) (= number (ftruncate (float number))))
    (grid-calc-raise "#NUM!"))
  (truncate number))

(grid-calc-define bitand ((a number) (b number))
  "The bits A and B both have."
  (logand (grid-calc--bit-operand a) (grid-calc--bit-operand b)))

(grid-calc-define bitor ((a number) (b number))
  "The bits either A or B has."
  (logior (grid-calc--bit-operand a) (grid-calc--bit-operand b)))

(grid-calc-define bitxor ((a number) (b number))
  "The bits one of A and B has, but not both."
  (logxor (grid-calc--bit-operand a) (grid-calc--bit-operand b)))

(grid-calc-define bitlshift ((number number) (shift integer))
  "NUMBER's bits moved SHIFT places to the left."
  (let ((result (ash (grid-calc--bit-operand number) shift)))
    (when (>= result (expt 2 48)) (grid-calc-raise "#NUM!"))
    result))

(grid-calc-define bitrshift ((number number) (shift integer))
  "NUMBER's bits moved SHIFT places to the right."
  (ash (grid-calc--bit-operand number) (- shift)))

;;;----------------------------------------------------------------------
;;; Complex numbers
;;;----------------------------------------------------------------------

(defconst grid-calc--complex-number
  "\\([-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\)"
  "A real number, as complex numbers write their parts.")

(defun grid-calc--complex (value)
  "Return VALUE, a complex number as text or a number, as (REAL IMAG SUFFIX)."
  (cond
   ((numberp value) (list (float value) 0.0 nil))
   ((memq value '(:blank)) (list 0.0 0.0 nil))
   ((not (stringp value)) (grid-calc-raise "#VALUE!"))
   (t
    (let ((text value) (n grid-calc--complex-number) (case-fold-search nil))
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
           (t (grid-calc-raise "#NUM!")))))
       (t (grid-calc-raise "#NUM!")))))))

(defun grid-calc--complex-text (real imag &optional suffix)
  "Return the complex number REAL + IMAG SUFFIX as text, as Excel writes it."
  (let* ((suffix (or suffix "i"))
         (real (grid-calc--tidy-number (grid-calc--snap real)))
         (imag (grid-calc--tidy-number (grid-calc--snap imag)))
         (real-text (unless (zerop real) (grid-calc-number-text real)))
         (imag-text (unless (zerop imag)
                      (concat (cond ((< imag 0) "-") (real-text "+") (t ""))
                              (if (= (abs imag) 1) "" (grid-calc-number-text (abs imag)))
                              suffix))))
    (if (or real-text imag-text) (concat real-text imag-text) "0")))

(defun grid-calc--complex-suffix (&rest numbers)
  "Return the suffix NUMBERS, parsed complex numbers, share, or #VALUE!."
  (let ((suffixes (delete-dups (delq nil (mapcar (lambda (c) (nth 2 c)) numbers)))))
    (when (cdr suffixes) (grid-calc-raise "#VALUE!"))
    (car suffixes)))

(grid-calc-define complex ((real number) (imag number) &optional (suffix text "i"))
  "The complex number REAL + IMAG SUFFIX, SUFFIX being i or j."
  (unless (member suffix '("i" "j" "")) (grid-calc-raise "#VALUE!"))
  (grid-calc--complex-text real imag (if (string-empty-p suffix) "i" suffix)))

(grid-calc-define imreal ((number any))
  "The real part of the complex NUMBER."
  (grid-calc--tidy-number (nth 0 (grid-calc--complex number))))

(grid-calc-define imaginary ((number any))
  "The imaginary part of the complex NUMBER."
  (grid-calc--tidy-number (nth 1 (grid-calc--complex number))))

(grid-calc-define imabs ((number any))
  "How far the complex NUMBER is from 0."
  (pcase-let ((`(,a ,b ,_) (grid-calc--complex number)))
    (sqrt (+ (* a a) (* b b)))))

(grid-calc-define imargument ((number any))
  "The angle of the complex NUMBER, in radians."
  (pcase-let ((`(,a ,b ,_) (grid-calc--complex number)))
    (when (and (zerop a) (zerop b)) (grid-calc-raise "#DIV/0!"))
    (atan b a)))

(grid-calc-define imconjugate ((number any))
  "The complex NUMBER with its imaginary part negated."
  (pcase-let ((`(,a ,b ,s) (grid-calc--complex number)))
    (grid-calc--complex-text a (- b) s)))

(defun grid-calc--complex-args (arguments)
  "Return the complex numbers of ARGUMENTS, `values' arguments."
  (let (numbers)
    (dolist (argument arguments)
      (grid-calc-each-value argument
                            (lambda (value _)
                              (when (grid-calc-err-p value)
                                (grid-calc-raise (grid-calc-err-code value)))
                              (unless (eq value :blank)
                                (push (grid-calc--complex value) numbers)))))
    (nreverse numbers)))

(grid-calc-define imsum (&rest (numbers values))
  "The sum of the complex NUMBERS."
  (let* ((all (grid-calc--complex-args numbers))
         (suffix (apply #'grid-calc--complex-suffix all)))
    (grid-calc--complex-text (apply #'+ 0.0 (mapcar #'car all))
                             (apply #'+ 0.0 (mapcar #'cadr all))
                             suffix)))

(grid-calc-define imsub ((a any) (b any))
  "The complex number A less B."
  (let* ((x (grid-calc--complex a)) (y (grid-calc--complex b)))
    (grid-calc--complex-text (- (nth 0 x) (nth 0 y)) (- (nth 1 x) (nth 1 y))
                             (grid-calc--complex-suffix x y))))

(defun grid-calc--complex-times (x y)
  "Return the product of the complex numbers X and Y, as (REAL IMAG)."
  (list (- (* (nth 0 x) (nth 0 y)) (* (nth 1 x) (nth 1 y)))
        (+ (* (nth 0 x) (nth 1 y)) (* (nth 1 x) (nth 0 y)))))

(grid-calc-define improduct (&rest (numbers values))
  "The product of the complex NUMBERS."
  (let* ((all (grid-calc--complex-args numbers))
         (suffix (apply #'grid-calc--complex-suffix all))
         (product (list 1.0 0.0)))
    (dolist (number all)
      (setq product (grid-calc--complex-times product number)))
    (grid-calc--complex-text (nth 0 product) (nth 1 product) suffix)))

(grid-calc-define imdiv ((a any) (b any))
  "The complex number A divided by B."
  (let* ((x (grid-calc--complex a)) (y (grid-calc--complex b))
         (den (+ (* (nth 0 y) (nth 0 y)) (* (nth 1 y) (nth 1 y)))))
    (when (zerop den) (grid-calc-raise "#NUM!"))
    (grid-calc--complex-text
     (/ (+ (* (nth 0 x) (nth 0 y)) (* (nth 1 x) (nth 1 y))) den)
     (/ (- (* (nth 1 x) (nth 0 y)) (* (nth 0 x) (nth 1 y))) den)
     (grid-calc--complex-suffix x y))))

(defun grid-calc--complex-polar (number)
  "Return NUMBER, a parsed complex number, as (MODULUS ANGLE SUFFIX)."
  (pcase-let ((`(,a ,b ,s) number))
    (list (sqrt (+ (* a a) (* b b))) (atan b a) s)))

(grid-calc-define imln ((number any))
  "The natural logarithm of the complex NUMBER."
  (pcase-let ((`(,r ,theta ,s) (grid-calc--complex-polar (grid-calc--complex number))))
    (when (zerop r) (grid-calc-raise "#NUM!"))
    (grid-calc--complex-text (log r) theta s)))

(grid-calc-define imlog10 ((number any))
  "The base 10 logarithm of the complex NUMBER."
  (pcase-let ((`(,r ,theta ,s) (grid-calc--complex-polar (grid-calc--complex number))))
    (when (zerop r) (grid-calc-raise "#NUM!"))
    (grid-calc--complex-text (/ (log r) (log 10)) (/ theta (log 10)) s)))

(grid-calc-define imlog2 ((number any))
  "The base 2 logarithm of the complex NUMBER."
  (pcase-let ((`(,r ,theta ,s) (grid-calc--complex-polar (grid-calc--complex number))))
    (when (zerop r) (grid-calc-raise "#NUM!"))
    (grid-calc--complex-text (/ (log r) (log 2)) (/ theta (log 2)) s)))

(grid-calc-define imexp ((number any))
  "e to the power of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (grid-calc--complex number)))
    (grid-calc--complex-text (* (exp a) (cos b)) (* (exp a) (sin b)) s)))

(grid-calc-define impower ((number any) (power number))
  "The complex NUMBER to the real POWER."
  (pcase-let ((`(,r ,theta ,s) (grid-calc--complex-polar (grid-calc--complex number))))
    (when (and (zerop r) (<= power 0)) (grid-calc-raise "#NUM!"))
    (let ((modulus (expt r power)) (angle (* theta power)))
      (grid-calc--complex-text (* modulus (cos angle)) (* modulus (sin angle)) s))))

(grid-calc-define imsqrt ((number any))
  "The square root of the complex NUMBER."
  (pcase-let ((`(,r ,theta ,s) (grid-calc--complex-polar (grid-calc--complex number))))
    (let ((modulus (sqrt r)) (angle (/ theta 2)))
      (grid-calc--complex-text (* modulus (cos angle)) (* modulus (sin angle)) s))))

(defun grid-calc--sinh (x)
  "Return the hyperbolic sine of X."
  (/ (- (exp x) (exp (- x))) 2))

(defun grid-calc--cosh (x)
  "Return the hyperbolic cosine of X."
  (/ (+ (exp x) (exp (- x))) 2))

(grid-calc-define imsin ((number any))
  "The sine of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (grid-calc--complex number)))
    (grid-calc--complex-text (* (sin a) (grid-calc--cosh b)) (* (cos a) (grid-calc--sinh b)) s)))

(grid-calc-define imcos ((number any))
  "The cosine of the complex NUMBER."
  (pcase-let ((`(,a ,b ,s) (grid-calc--complex number)))
    (grid-calc--complex-text (* (cos a) (grid-calc--cosh b)) (- (* (sin a) (grid-calc--sinh b))) s)))

(provide 'grid-table-calc-engineering)

;;; grid-table-calc-engineering.el ends here
