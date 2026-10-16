;;; grid-table-calc-finance.el --- Financial functions -*- lexical-binding: t -*-

;;; Commentary:

;; Loans, annuities and investments, as Excel computes them.  Money paid
;; out is negative and money received positive; TYPE 1 makes payments
;; at the start of each period rather than its end.  The functions that
;; find a rate, RATE, IRR and XIRR, search for it by Newton's method from
;; a guess, and give #NUM! when the search does not settle.

;;; Code:

(require 'grid-table-calc)

(defun grid-calc--growth (rate periods)
  "Return (1 + RATE) to the power PERIODS, or #NUM! where it has none."
  (let ((base (+ 1.0 rate)))
    (when (and (< base 0) (/= periods (ftruncate periods)))
      (grid-calc-raise "#NUM!"))
    (expt base periods)))

(defun grid-calc--annuity-fv (rate periods payment present type)
  "Return the future value of PRESENT and PERIODS of PAYMENT at RATE.
TYPE non-zero pays at the start of each period."
  (if (zerop rate)
      (- (+ present (* payment periods)))
    (let ((growth (grid-calc--growth rate periods)))
      (- (+ (* present growth)
            (/ (* payment (+ 1 (* rate type)) (- growth 1)) rate))))))

(grid-calc-define pv ((rate number) (periods number) (payment number)
                      &optional (future number 0) (type integer 0))
  "The value now of PERIODS payments of PAYMENT at RATE, and of FUTURE."
  (let ((type (if (zerop type) 0 1)))
    (if (zerop rate)
        (- (+ future (* payment periods)))
      (let ((growth (grid-calc--growth rate periods)))
        (/ (- (+ future (/ (* payment (+ 1 (* rate type)) (- growth 1)) rate)))
           growth)))))

(grid-calc-define fv ((rate number) (periods number) &optional (payment number 0)
                      (present number 0) (type integer 0))
  "The value after PERIODS of PRESENT and payments of PAYMENT at RATE."
  (grid-calc--annuity-fv rate periods payment present (if (zerop type) 0 1)))

(defun grid-calc--payment (rate periods present future type)
  "Return the payment per period paying PRESENT off to FUTURE."
  (cond
   ((zerop periods) (grid-calc-raise "#NUM!"))
   ((zerop rate) (- (/ (+ present future) (float periods))))
   (t (let ((growth (grid-calc--growth rate periods)))
        (- (/ (* rate (+ future (* present growth)))
              (* (+ 1 (* rate type)) (- growth 1))))))))

(grid-calc-define pmt ((rate number) (periods number) (present number)
                       &optional (future number 0) (type integer 0))
  "The payment per period for a loan of PRESENT over PERIODS at RATE."
  (grid-calc--payment rate periods present future (if (zerop type) 0 1)))

(defun grid-calc--interest-part (rate period periods present future type)
  "Return the interest paid in PERIOD of a loan, as IPMT gives it."
  (unless (and (>= period 1) (<= period periods)) (grid-calc-raise "#NUM!"))
  (let ((payment (grid-calc--payment rate periods present future type)))
    (if (and (= type 1) (< period 2)) 0
      (let ((interest (* (grid-calc--annuity-fv rate (1- period) payment present type)
                         rate)))
        (if (= type 1) (/ interest (+ 1 rate)) interest)))))

(grid-calc-define ipmt ((rate number) (period number) (periods number) (present number)
                        &optional (future number 0) (type integer 0))
  "The interest in payment PERIOD of PERIODS on a loan of PRESENT."
  (grid-calc--interest-part rate period periods present future (if (zerop type) 0 1)))

(grid-calc-define ppmt ((rate number) (period number) (periods number) (present number)
                        &optional (future number 0) (type integer 0))
  "The principal in payment PERIOD of PERIODS on a loan of PRESENT."
  (let ((type (if (zerop type) 0 1)))
    (- (grid-calc--payment rate periods present future type)
       (grid-calc--interest-part rate period periods present future type))))

(grid-calc-define nper ((rate number) (payment number) (present number)
                        &optional (future number 0) (type integer 0))
  "How many payments of PAYMENT at RATE pay PRESENT off to FUTURE."
  (let ((type (if (zerop type) 0 1)))
    (if (zerop rate)
        (if (zerop payment) (grid-calc-raise "#NUM!")
          (- (/ (+ present future) (float payment))))
      (let* ((adjusted (* payment (+ 1 (* rate type))))
             (top (- adjusted (* future rate)))
             (bottom (+ adjusted (* present rate))))
        (if (or (zerop bottom) (<= (/ top bottom) 0) (<= (+ 1 rate) 0))
            (grid-calc-raise "#NUM!")
          (/ (log (/ top bottom)) (log (+ 1.0 rate))))))))

(defun grid-calc--newton (function guess)
  "Return where FUNCTION is zero, searching from GUESS, or #NUM!.
FUNCTION returns (VALUE . SLOPE) at a point."
  (let ((x guess) (found nil))
    (dotimes (_ 100)
      (unless found
        (pcase-let ((`(,value . ,slope) (funcall function x)))
          (when (or (zerop slope) (isnan value) (isnan slope))
            (grid-calc-raise "#NUM!"))
          (let ((next (- x (/ value slope))))
            (if (< (abs (- next x)) 1e-10)
                (setq found next)
              (setq x next))))))
    (if (and found (not (isnan found)) (/= (abs found) 1.0e+INF)) found
      (grid-calc-raise "#NUM!"))))

(grid-calc-define rate ((periods number) (payment number) (present number)
                        &optional (future number 0) (type integer 0) (guess number 0.1))
  "The rate per period at which PERIODS payments of PAYMENT pay PRESENT
off to FUTURE."
  (let ((type (if (zerop type) 0 1)))
    (when (<= periods 0) (grid-calc-raise "#NUM!"))
    (grid-calc--newton
     (lambda (rate)
       (if (< (abs rate) 1e-12)
           ;; At no interest: value and slope as the limit gives them.
           (cons (+ present (* payment periods) future)
                 (+ (* present periods)
                    (* payment (/ (* periods (- periods 1)) 2.0))
                    (* payment type periods)))
         (when (<= (+ 1 rate) 0) (grid-calc-raise "#NUM!"))
         (let* ((growth (expt (+ 1.0 rate) periods))
                (growth-slope (* periods (expt (+ 1.0 rate) (1- periods))))
                (annuity (/ (- growth 1) rate))
                (annuity-slope (/ (- (* growth-slope rate) (- growth 1)) (* rate rate)))
                (factor (+ 1 (* rate type))))
           (cons (+ (* present growth) (* payment factor annuity) future)
                 (+ (* present growth-slope)
                    (* payment type annuity)
                    (* payment factor annuity-slope))))))
     guess)))

(grid-calc-define npv ((rate number) &rest (values values))
  "The value now of VALUES, one at the end of each period, at RATE."
  (let ((sum 0.0) (i 0))
    (dolist (value (grid-calc-numbers values))
      (setq i (1+ i) sum (+ sum (/ value (grid-calc--growth rate i)))))
    sum))

(defun grid-calc--range-numbers (values)
  "Return the numbers of VALUES, a range or array, other values left out."
  (let (numbers)
    (grid-calc-each-value values (lambda (value _)
                                   (when (grid-calc-err-p value)
                                     (grid-calc-raise (grid-calc-err-code value)))
                                   (when (numberp value) (push value numbers))))
    (nreverse numbers)))

(grid-calc-define irr ((values values) &optional (guess number 0.1))
  "The rate at which the cash flows VALUES, one per period, are worth 0."
  (let ((flows (grid-calc--range-numbers values)))
    (unless (and (cl-some (lambda (n) (> n 0)) flows)
                 (cl-some (lambda (n) (< n 0)) flows))
      (grid-calc-raise "#NUM!"))
    (grid-calc--newton
     (lambda (rate)
       (when (<= (+ 1 rate) 0) (grid-calc-raise "#NUM!"))
       (let ((value 0.0) (slope 0.0) (i 0))
         (dolist (flow flows)
           (setq value (+ value (/ flow (expt (+ 1.0 rate) i)))
                 slope (- slope (/ (* i flow) (expt (+ 1.0 rate) (1+ i))))
                 i (1+ i)))
         (cons value slope)))
     guess)))

(defun grid-calc--dated-flows (values dates)
  "Return ((DAYS . FLOW) ...) for VALUES on DATES, days from the first."
  (let ((flows (grid-calc--range-numbers values))
        (days (mapcar #'ffloor (mapcar #'float (grid-calc--range-numbers dates)))))
    (unless (and flows (= (length flows) (length days))) (grid-calc-raise "#NUM!"))
    (let ((first (car days)))
      (when (cl-some (lambda (day) (< day first)) days) (grid-calc-raise "#NUM!"))
      (cl-mapcar (lambda (day flow) (cons (/ (- day first) 365.0) flow)) days flows))))

(grid-calc-define xnpv ((rate number) (values values) (dates values))
  "The value at the first of DATES of the cash flows VALUES on DATES."
  (when (<= rate -1) (grid-calc-raise "#NUM!"))
  (let ((sum 0.0))
    (pcase-dolist (`(,years . ,flow) (grid-calc--dated-flows values dates))
      (setq sum (+ sum (/ flow (expt (+ 1.0 rate) years)))))
    sum))

(grid-calc-define xirr ((values values) (dates values) &optional (guess number 0.1))
  "The yearly rate at which the cash flows VALUES on DATES are worth 0."
  (let ((flows (grid-calc--dated-flows values dates)))
    (unless (and (cl-some (lambda (f) (> (cdr f) 0)) flows)
                 (cl-some (lambda (f) (< (cdr f) 0)) flows))
      (grid-calc-raise "#NUM!"))
    (grid-calc--newton
     (lambda (rate)
       (when (<= (+ 1 rate) 0) (grid-calc-raise "#NUM!"))
       (let ((value 0.0) (slope 0.0))
         (pcase-dolist (`(,years . ,flow) flows)
           (setq value (+ value (/ flow (expt (+ 1.0 rate) years)))
                 slope (- slope (/ (* years flow) (expt (+ 1.0 rate) (1+ years))))))
         (cons value slope)))
     guess)))

(grid-calc-define effect ((nominal number) (periods integer))
  "The yearly rate NOMINAL comes to, compounded PERIODS times a year."
  (when (or (<= nominal 0) (< periods 1)) (grid-calc-raise "#NUM!"))
  (- (expt (+ 1.0 (/ nominal periods)) periods) 1))

(grid-calc-define nominal ((effective number) (periods integer))
  "The yearly rate that, compounded PERIODS times a year, comes to EFFECTIVE."
  (when (or (<= effective 0) (< periods 1)) (grid-calc-raise "#NUM!"))
  (* periods (- (expt (+ 1.0 effective) (/ 1.0 periods)) 1)))

(grid-calc-define sln ((cost number) (salvage number) (life number))
  "The depreciation per period of COST down to SALVAGE over LIFE."
  (when (zerop life) (grid-calc-raise "#DIV/0!"))
  (/ (- cost salvage) (float life)))

(grid-calc-define syd ((cost number) (salvage number) (life number) (period number))
  "The depreciation in PERIOD by the sum of the years' digits."
  (when (or (<= life 0) (<= period 0) (> period life) (< salvage 0))
    (grid-calc-raise "#NUM!"))
  (/ (* (- cost salvage) (+ (- life period) 1))
     (/ (* life (+ life 1)) 2.0)))

(grid-calc-define ddb ((cost number) (salvage number) (life number) (period number)
                       &optional (factor number 2))
  "The depreciation in PERIOD by declining balance at FACTOR."
  (when (or (< cost 0) (< salvage 0) (<= life 0) (<= period 0) (> period life)
            (<= factor 0))
    (grid-calc-raise "#NUM!"))
  (let ((value cost) (depreciation 0.0) (rate (/ factor (float life))))
    (dotimes (_ (ceiling period))
      (setq depreciation (max 0 (min (* value (min rate 1)) (- value salvage)))
            value (- value depreciation)))
    depreciation))

(provide 'grid-table-calc-finance)

;;; grid-table-calc-finance.el ends here
