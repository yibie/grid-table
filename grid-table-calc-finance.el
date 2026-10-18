;;; grid-table-calc-finance.el --- Financial functions -*- lexical-binding: t -*-

;;; Commentary:

;; Loans, annuities and investments, as Excel computes them.  Money paid
;; out is negative and money received positive; TYPE 1 makes payments
;; at the start of each period rather than its end.  The functions that
;; find a rate, RATE, IRR and XIRR, search for it by Newton's method from
;; a guess, and give #NUM! when the search does not settle.
;;
;; The bonds - the COUP functions, PRICE and YIELD, the discounted
;; securities, the bills and the odd first and last periods - count
;; their days by a BASIS from 0 to 4, as `grid-calc--year-fraction'
;; does, and put their coupons on the day of MATURITY, shortened to the
;; last day of a month that has no such day.

;;; Code:

(require 'grid-table-calc)
(require 'grid-table-calc-date)

(defun grid-calc--growth (rate periods)
  "Return (1 + RATE) to the power PERIODS, or #NUM! where it has none."
  (let ((base (+ 1.0 rate)))
    (when (and (< base 0) (/= periods (ftruncate (float periods))))
      (grid-calc-raise "#NUM!"))
    (when (and (= base 0) (< periods 0)) (grid-calc-raise "#DIV/0!"))
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
   ((<= (+ 1.0 rate) 0) (grid-calc-raise "#NUM!"))
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
  "Return the interest paid in PERIOD of a loan, as IPMT gives it.
PERIOD may carry a fraction, and run past the last payment by less
than one, as Excel allows; the first payment at the start of a period
pays no interest."
  (unless (and (>= period 1) (< (1- period) periods)) (grid-calc-raise "#NUM!"))
  (let ((payment (grid-calc--payment rate periods present future type)))
    (if (and (= type 1) (<= period 1)) 0
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
        (if (or (zerop bottom) (<= (/ top (float bottom)) 0) (<= (+ 1 rate) 0))
            (grid-calc-raise "#NUM!")
          (/ (log (/ top (float bottom))) (log (+ 1.0 rate))))))))

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

(defun grid-calc--finance-root (function guess)
  "Return where FUNCTION is zero, searching from GUESS, or #NUM!.
FUNCTION returns (VALUE . SLOPE) at a point.  A step that would pass
-1, where a rate has no meaning, is halved towards it, so that a root
between -1 and GUESS can still be found."
  (let ((x guess) (found nil) (tries 0))
    (while (and (not found) (< tries 500))
      (setq tries (1+ tries))
      (pcase-let ((`(,value . ,slope) (funcall function x)))
        (cond
         ((or (zerop slope) (isnan value) (isnan slope)) (setq tries 500))
         (t
          (let ((next (- x (/ value slope))))
            (when (<= next -1) (setq next (/ (1- x) 2.0)))
            (if (< (abs (- next x)) 1e-12)
                (setq found next)
              (setq x next)))))))
    (if (and found (not (isnan found)) (/= (abs found) 1.0e+INF))
        found
      (grid-calc-raise "#NUM!"))))

(grid-calc-define rate ((periods number) (payment number) (present number)
                        &optional (future number 0) (type integer 0) (guess number 0.1))
  "The rate per period at which PERIODS payments of PAYMENT pay PRESENT
off to FUTURE."
  (let ((type (if (zerop type) 0 1))
        found)
    (when (<= periods 0) (grid-calc-raise "#NUM!"))
    (setq found
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
                      (annuity-slope (/ (- (* growth-slope rate) (- growth 1))
                                        (* rate rate)))
                      (factor (+ 1 (* rate type))))
                 (cons (+ (* present growth) (* payment factor annuity) future)
                       (+ (* present growth-slope)
                          (* payment type annuity)
                          (* payment factor annuity-slope))))))
           guess))
    (if (<= (+ 1 found) 0) (grid-calc-raise "#NUM!") found)))

(grid-calc-define npv ((rate number) &rest (values values))
  "The value now of VALUES, one at the end of each period, at RATE."
  (let ((sum 0.0) (i 0))
    (dolist (value (grid-calc-numbers values))
      (setq i (1+ i) sum (+ sum (/ value (grid-calc--growth rate i)))))
    sum))

(defun grid-calc--range-numbers (values &optional plain-error)
  "Return the numbers of VALUES, a range or array, other values left out.
An error value signals itself, unless PLAIN-ERROR, when it is #VALUE!."
  (let (numbers)
    (grid-calc-each-value
     values
     (lambda (value _)
       (when (grid-calc-err-p value)
         (grid-calc-raise (if plain-error "#VALUE!" (grid-calc-err-code value))))
       (when (numberp value) (push value numbers))))
    (nreverse numbers)))

(defun grid-calc--finance-flow-numbers (values)
  "Return the numbers of VALUES, or #VALUE! for text or a logical value.
Text that is no number is an error and not nothing, as XIRR has it;
a cell left empty is worth nothing."
  (let (numbers)
    (grid-calc-each-value
     values
     (lambda (value _)
       (cond
        ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
        ((stringp value)
         (push (or (grid-calc-text-number value) (grid-calc-raise "#VALUE!"))
               numbers))
        ((memq value '(t :false)) (grid-calc-raise "#VALUE!"))
        ((eq value :blank) (push 0 numbers))
        ((numberp value) (push value numbers)))))
    (nreverse numbers)))

(grid-calc-define irr ((values values) &optional (guess number 0.1))
  "The rate at which the cash flows VALUES, one per period, are worth 0."
  (let ((flows (grid-calc--range-numbers values t)))
    (when (<= guess -1) (grid-calc-raise "#VALUE!"))
    (unless (and (cl-some (lambda (n) (> n 0)) flows)
                 (cl-some (lambda (n) (< n 0)) flows))
      (grid-calc-raise "#NUM!"))
    (grid-calc--finance-root
     (lambda (rate)
       (let ((value 0.0) (slope 0.0) (i 0))
         (dolist (flow flows)
           (setq value (+ value (/ flow (expt (+ 1.0 rate) i)))
                 slope (- slope (/ (* i flow) (expt (+ 1.0 rate) (1+ i))))
                 i (1+ i)))
         (cons value slope)))
     guess)))

(defun grid-calc--values-list (argument)
  "Return the values of ARGUMENT, in the order `grid-calc-each-value' has."
  (let (values)
    (grid-calc-each-value argument (lambda (value _) (push value values)))
    (nreverse values)))

(defun grid-calc--drop-unvalued (values dates)
  "Return (VALUES . DATES) without the places where both cells are empty,
as XIRR leaves them out."
  (if (not (grid-calc-ref-p values))
      (cons values dates)
    (let ((vs (grid-calc--values-list values))
          (ds (grid-calc--values-list dates))
          kept-values kept-dates)
      (if (/= (length vs) (length ds))
          (cons values dates)
        (cl-mapc (lambda (value date)
                   (unless (and (eq value :blank) (eq date :blank))
                     (push value kept-values)
                     (push date kept-dates)))
                 vs ds)
        (cons (grid-calc-array--make 1 (length kept-values)
                                     (vconcat (nreverse kept-values)))
              (grid-calc-array--make 1 (length kept-dates)
                                     (vconcat (nreverse kept-dates))))))))

(defun grid-calc--dated-flows (values dates &optional strict)
  "Return ((DAYS . FLOW) ...) for VALUES on DATES, days from the first.
DATES is read first, as Excel reads it.  With STRICT, text that is no
number and a logical value are #VALUE!, where otherwise they are left
out and the counts no longer agree; but a place where both are empty
cells is left out."
  (when strict
    (pcase-setq `(,values . ,dates) (grid-calc--drop-unvalued values dates)))
  (let ((days (mapcar #'ffloor (mapcar #'float (grid-calc--range-numbers dates))))
        (flows (if strict
                   (grid-calc--finance-flow-numbers values)
                 (grid-calc--range-numbers values))))
    (unless (and flows (= (length flows) (length days))) (grid-calc-raise "#NUM!"))
    (when (cl-some (lambda (day) (or (< day 0) (> day grid-calc--last-serial)))
                   days)
      (grid-calc-raise "#NUM!"))
    (let ((first (car days)))
      (when (cl-some (lambda (day) (< day first)) days) (grid-calc-raise "#NUM!"))
      (cl-mapcar (lambda (day flow) (cons (/ (- day first) 365.0) flow)) days flows))))

(grid-calc-define xnpv ((rate any) (values values) (dates values))
  "The value at the first of DATES of the cash flows VALUES on DATES."
  (let ((rate (grid-calc--strict-num rate)))
    (when (<= rate 0) (grid-calc-raise "#NUM!"))
    (let ((sum 0.0))
      (pcase-dolist (`(,years . ,flow) (grid-calc--dated-flows values dates))
        (setq sum (+ sum (/ flow (expt (+ 1.0 rate) years)))))
      sum)))

(grid-calc-define xirr ((values values) (dates values) &optional (guess any 0.1))
  "The yearly rate at which the cash flows VALUES on DATES are worth 0."
  (let ((flows (grid-calc--dated-flows values dates t))
        (guess (grid-calc--strict-num guess)))
    (unless (and (cl-some (lambda (f) (> (cdr f) 0)) flows)
                 (cl-some (lambda (f) (< (cdr f) 0)) flows))
      (grid-calc-raise "#NUM!"))
    (let ((rate (grid-calc--finance-root
                 (lambda (rate)
                   (let ((value 0.0) (slope 0.0))
                     (pcase-dolist (`(,years . ,flow) flows)
                       (setq value (+ value (/ flow (expt (+ 1.0 rate) years)))
                             slope (- slope (/ (* years flow)
                                               (expt (+ 1.0 rate) (1+ years))))))
                     (cons value slope)))
                 guess)))
      ;; Excel gives up on a rate that comes within nothing of -1.
      (when (< (+ 1 rate) 1e-9) (grid-calc-raise "#NUM!"))
      rate)))

(grid-calc-define effect ((nominal number) (periods integer))
  "The yearly rate NOMINAL comes to, compounded PERIODS times a year."
  :toolpak t
  (when (or (<= nominal 0) (< periods 1)) (grid-calc-raise "#NUM!"))
  (- (expt (+ 1.0 (/ nominal (float periods))) periods) 1))

(grid-calc-define nominal ((effective number) (periods integer))
  "The yearly rate that, compounded PERIODS times a year, comes to EFFECTIVE."
  :toolpak t
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

(defun grid-calc--finance-amorlinc (cost start first salvage period rate basis)
  "Return the AMORLINC depreciation of COST in PERIOD.
The first period, from START to FIRST, is a fraction of a full one."
  (let* ((one (* cost rate))
         (first-rate (* (grid-calc--finance-years start first basis) one))
         (rest (- cost salvage first-rate))
         (full (truncate (/ rest one))))
    (cond
     ((zerop period) first-rate)
     ((<= 1 period full) one)
     ((= period (1+ full)) (- rest (* full one)))
     (t 0.0))))

(grid-calc-define amorlinc ((cost any) (start date) (first date) (salvage any)
                            (period any) (rate any) &optional (basis integer 0))
  "The depreciation in PERIOD of COST bought on START, by the French method."
  :toolpak t
  (grid-calc--finance-basis basis)
  (let ((cost (grid-calc--strict-num cost))
        (salvage (grid-calc--strict-num salvage))
        (period (grid-calc--strict-num period))
        (rate (grid-calc--strict-num rate)))
    (when (or (<= cost 0) (<= rate 0) (< salvage 0) (< period 0) (> start first))
      (grid-calc-raise "#NUM!"))
    (grid-calc--finance-amorlinc cost start first salvage (truncate period)
                                 rate basis)))

(defun grid-calc--finance-amordegrc (cost start first salvage period rate basis)
  "Return the AMORDEGRC depreciation of COST in PERIOD.
The depreciation takes a coefficient by the life RATE gives, and the
last of it is half of what is left."
  (let* ((life (ceiling (/ 1.0 rate)))
         (coefficient (cond ((memq life '(3 4)) 1.5)
                            ((memq life '(5 6)) 2.0)
                            ((> life 6) 2.5)
                            (t (grid-calc-raise "#NUM!"))))
         (room (- cost salvage))
         (value (grid-calc--round-to
                 (min (* (grid-calc--finance-years start first basis)
                         coefficient rate cost)
                      room)
                 0 'nearest))
         (total 0.0)
         (i 3))
    (while (<= i (+ period 2))
      (setq total (+ total value))
      (cond
       ((> total room) (setq value 0.0 i (+ period 3)))
       ((< i life) (setq value (* coefficient rate (- cost total))))
       ((= i life) (setq value (/ (- cost total) 2.0))))
      (setq i (1+ i)))
    (grid-calc--round-to value 0 'nearest)))

(grid-calc-define amordegrc ((cost any) (start date) (first date) (salvage any)
                             (period any) (rate any) &optional (basis integer 0))
  "The depreciation in PERIOD of COST bought on START, the French way."
  :toolpak t
  (grid-calc--finance-basis basis)
  (let ((cost (grid-calc--strict-num cost))
        (salvage (grid-calc--strict-num salvage))
        (period (grid-calc--strict-num period))
        (rate (grid-calc--strict-num rate)))
    (when (or (<= cost salvage) (<= rate 0) (< salvage 0) (< period 0)
              (> start first))
      (grid-calc-raise "#NUM!"))
    (grid-calc--finance-amordegrc cost start first salvage (truncate period)
                                  rate basis)))

(grid-calc-define ddb ((cost number) (salvage number) (life number) (period number)
                       &optional (factor number 2))
  "The depreciation in PERIOD by declining balance at FACTOR."
  (when (or (< cost 0) (< salvage 0) (<= life 0) (<= period 0) (> period life)
            (<= factor 0))
    (grid-calc-raise "#NUM!"))
  (let* ((rate (/ factor (float life)))
         (value cost))
    ;; The value a whole period before PERIOD takes it, never past SALVAGE.
    (dotimes (_ (max 0 (1- (floor period))))
      (setq value (- value
                     (max 0 (min (* value (min rate 1)) (- value salvage))))))
    (min (* cost (grid-calc--growth (- rate) (1- period)) rate)
         (max 0 (- value salvage)))))

;;;----------------------------------------------------------------------
;;; Days, coupon dates and the periods they belong to
;;;----------------------------------------------------------------------

(defun grid-calc--finance-basis (basis)
  "Return BASIS, or #NUM! when it is not one of 0 to 4."
  (unless (memq basis '(0 1 2 3 4)) (grid-calc-raise "#NUM!"))
  basis)

(defun grid-calc--finance-frequency (frequency)
  "Return FREQUENCY, or #NUM! when it is not one of 1, 2 and 4."
  (unless (memq frequency '(1 2 4)) (grid-calc-raise "#NUM!"))
  frequency)

(defun grid-calc--finance-days (start end basis)
  "The days from START to END, counted by BASIS."
  (pcase basis
    ((or 0 4) (grid-calc--days-360 start end (= basis 4) t))
    (_ (- end start))))

(defun grid-calc--finance-step (frequency)
  "The months from one coupon to the next at FREQUENCY."
  (pcase frequency (1 12) (2 6) (4 3)))

(defun grid-calc--finance-month-days (year month)
  "The days of MONTH in YEAR."
  (nth 2 (grid-calc-serial-date (grid-calc-date-serial year (1+ month) 0))))

(defun grid-calc--finance-coupon-date (maturity months)
  "The day MONTHS months before MATURITY, its day kept where it fits.
A MATURITY on the last day of its month keeps to the last day of a month."
  (pcase-let* ((`(,year ,month ,day) (grid-calc-serial-date maturity))
               (index (- (+ (* year 12) (1- month)) months))
               (target-year (floor (/ index 12)))
               (target-month (1+ (% index 12)))
               (end (grid-calc--finance-month-days target-year target-month)))
    ;; A coupon before the workbook's first day is that day, as Excel has it.
    (max 0 (grid-calc-date-serial
            target-year target-month
            (if (= day (grid-calc--finance-month-days year month))
                end
              (min day end))))))

(defun grid-calc--finance-months (start end)
  "The months from START to END, the days of the month left out."
  (pcase-let ((`(,y1 ,m1 ,_) (grid-calc-serial-date start))
              (`(,y2 ,m2 ,_) (grid-calc-serial-date end)))
    (- (+ (* y2 12) m2) (+ (* y1 12) m1))))

(defun grid-calc--finance-coupons (settlement maturity frequency)
  "Return (PCD NCD MONTHS) for the coupons of MATURITY about SETTLEMENT.
PCD is the coupon on or before SETTLEMENT, NCD the one after it, and
MONTHS how many months before MATURITY PCD stands."
  (let* ((step (grid-calc--finance-step frequency))
         (months (* step
                    (max 1 (ceiling (/ (float (grid-calc--finance-months
                                               settlement maturity))
                                       step)))))
         pcd ncd)
    (setq pcd (grid-calc--finance-coupon-date maturity months)
          ncd (grid-calc--finance-coupon-date maturity (- months step)))
    (while (> pcd settlement)
      (setq months (+ months step)
            pcd (grid-calc--finance-coupon-date maturity months)
            ncd (grid-calc--finance-coupon-date maturity (- months step))))
    (while (<= ncd settlement)
      (setq months (- months step)
            pcd (grid-calc--finance-coupon-date maturity months)
            ncd (grid-calc--finance-coupon-date maturity (- months step))))
    (list pcd ncd months)))

(defun grid-calc--finance-period-days (pcd ncd frequency basis)
  "The days of the coupon period PCD to NCD, as BASIS and FREQUENCY count."
  (pcase basis
    ((or 0 2 4) (/ 360.0 frequency))
    (3 (/ 365.0 frequency))
    ;; Counted in the calendar, without the 29 February 1900 of serials.
    (_ (- ncd pcd (if (and (< pcd 60) (> ncd 60)) 1 0)))))

(defun grid-calc--finance-coupon-parts (settlement maturity frequency basis)
  "Return (N PCD NCD A DSC E) of the coupons about SETTLEMENT.
N is how many coupons are still to come, A the days from the coupon
before SETTLEMENT, DSC the days to the one after, and E the days of
the coupon period.  On a 30/360 BASIS, DSC is E less A, as Excel has
it, not the days counted that way to the next coupon."
  (pcase-let* ((`(,pcd ,ncd ,months)
                (grid-calc--finance-coupons settlement maturity frequency))
               (step (grid-calc--finance-step frequency))
               (a (grid-calc--finance-days pcd settlement basis))
               (e (grid-calc--finance-period-days pcd ncd frequency basis)))
    (list (/ months step) pcd ncd a
          (if (memq basis '(0 4)) (- e a)
            (grid-calc--finance-days settlement ncd basis))
          e)))

(defun grid-calc--finance-bond-period (pcd ncd frequency basis)
  "The days of the coupon period PCD to NCD, as a bond counts them."
  (if (memq basis '(0 4))
      (/ 360.0 frequency)
    (- ncd pcd)))

(defun grid-calc--finance-coupon-setup (settlement maturity frequency basis)
  "Give #NUM! unless the coupons about SETTLEMENT make sense, and return PARTS.
E is the length a bond discounts a period by, which is the days of the
coupon period unless BASIS counts a year as 360 days even of them."
  (grid-calc--finance-basis basis)
  (grid-calc--finance-frequency frequency)
  (when (>= settlement maturity) (grid-calc-raise "#NUM!"))
  (pcase-let ((`(,n ,pcd ,ncd ,a ,dsc ,_)
               (grid-calc--finance-coupon-parts
                settlement maturity frequency basis)))
    (list n pcd ncd a dsc
          (grid-calc--finance-bond-period pcd ncd frequency basis))))

;;;----------------------------------------------------------------------
;;; Coupon dates and the days between them
;;;----------------------------------------------------------------------

(grid-calc-define couppcd ((settlement date) (maturity date)
                           (frequency integer) &optional (basis integer 0))
  "The day of the coupon before SETTLEMENT."
  :toolpak t
  (nth 1 (grid-calc--finance-coupon-setup settlement maturity frequency basis)))

(grid-calc-define coupncd ((settlement date) (maturity date)
                           (frequency integer) &optional (basis integer 0))
  "The day of the coupon after SETTLEMENT."
  :toolpak t
  (nth 2 (grid-calc--finance-coupon-setup settlement maturity frequency basis)))

(grid-calc-define coupnum ((settlement date) (maturity date)
                           (frequency integer) &optional (basis integer 0))
  "How many coupons MATURITY still pays after SETTLEMENT."
  :toolpak t
  (nth 0 (grid-calc--finance-coupon-setup settlement maturity frequency basis)))

(grid-calc-define coupdaybs ((settlement date) (maturity date)
                             (frequency integer) &optional (basis integer 0))
  "The days from the coupon before SETTLEMENT to SETTLEMENT."
  :toolpak t
  (nth 3 (grid-calc--finance-coupon-setup settlement maturity frequency basis)))

(grid-calc-define coupdaysnc ((settlement date) (maturity date)
                              (frequency integer) &optional (basis integer 0))
  "The days from SETTLEMENT to the coupon after it."
  :toolpak t
  (nth 4 (grid-calc--finance-coupon-setup settlement maturity frequency basis)))

(grid-calc-define coupdays ((settlement date) (maturity date)
                            (frequency integer) &optional (basis integer 0))
  "The days of the coupon period SETTLEMENT falls in."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-frequency frequency)
  (when (>= settlement maturity) (grid-calc-raise "#NUM!"))
  (nth 5 (grid-calc--finance-coupon-parts settlement maturity frequency basis)))

;;;----------------------------------------------------------------------
;;; Interest accrued
;;;----------------------------------------------------------------------

(defun grid-calc--finance-normal-length (first frequency basis)
  "The days of a coupon period ending at FIRST, as BASIS counts them."
  (pcase basis
    ((or 0 2 4) (/ 360.0 frequency))
    (3 (/ 365.0 frequency))
    (_ (float (- first (grid-calc--finance-coupon-date
                        first (grid-calc--finance-step frequency)))))))

(defun grid-calc--finance-accrued (start settle first frequency basis whole)
  "The coupon periods accrued from START to SETTLE, on FIRST's coupons.
A coupon period that ends before FIRST counts as its normal length,
whatever its own, as ACCRINT counts an odd first period.  With WHOLE, a
period that is over before SETTLE counts as one, whatever its days."
  (let ((sign 1.0) (total 0.0)
        (normal (grid-calc--finance-normal-length first frequency basis))
        (step (grid-calc--finance-step frequency)))
    (when (> start settle)
      (cl-rotatef start settle)
      (setq sign -1.0))
    (when (< start settle)
      (let ((months (nth 2 (grid-calc--finance-coupons start first frequency)))
            pcd ncd)
        (setq pcd (grid-calc--finance-coupon-date first months)
              ncd (grid-calc--finance-coupon-date first (- months step)))
        (while (< pcd settle)
          (let* ((low (max pcd start))
                 (high (min ncd settle))
                 (length (grid-calc--finance-days pcd ncd basis))
                 (days (grid-calc--finance-days low high basis)))
            (when (and (= low pcd) (< ncd first))
              (setq days (+ days (- normal length))))
            (setq total (+ total (if (and whole (= low pcd) (< ncd settle))
                                     1.0
                                   (/ days (float normal))))))
          (setq months (- months step)
                pcd ncd
                ncd (grid-calc--finance-coupon-date first (- months step))))))
    (* sign total)))

(defun grid-calc--finance-rate-check (rate par)
  "Give #NUM! unless RATE and PAR are both above nothing."
  (when (or (<= rate 0) (<= par 0)) (grid-calc-raise "#NUM!")))

(grid-calc-define accrint ((issue date) (first date) (settlement date)
                           (rate number) (par number) (frequency integer)
                           &optional (basis integer 0) (method bool t))
  "The interest accrued on PAR from ISSUE to SETTLEMENT, at RATE.
Up to FIRST, Excel counts the part of the coupon period ISSUE falls
in, the whole periods after it and the days from the coupon before
FIRST to SETTLEMENT.  METHOD nil leaves out those whole periods, as
Excel does whatever its help says, which can come to less than
nothing; METHOD non-nil past FIRST counts from ISSUE period by period."
  (grid-calc--finance-basis basis)
  (grid-calc--finance-frequency frequency)
  (grid-calc--finance-rate-check rate par)
  (when (>= issue settlement) (grid-calc-raise "#NUM!"))
  (* par (/ (float rate) frequency)
     (if (and method (> settlement first))
         (grid-calc--finance-accrued issue settlement first frequency basis t)
       (pcase-let* ((step (grid-calc--finance-step frequency))
                    (pcd (grid-calc--finance-coupon-date first step))
                    (`(,low ,high ,months)
                     (grid-calc--finance-coupons issue first frequency))
                    (between (- (/ months step) (if (= low issue) 1 2))))
         (+ (if (or (>= issue pcd) (= low issue))
                0
              (/ (float (grid-calc--finance-days issue high basis))
                 (grid-calc--finance-period-days low high frequency basis)))
            (if (and method (< issue pcd)) between 0)
            (/ (float (grid-calc--finance-days (max issue pcd) settlement
                                               basis))
               (grid-calc--finance-period-days pcd first frequency
                                               basis)))))))

(grid-calc-define accrintm ((issue date) (settlement date) (rate number)
                            (par number) &optional (basis integer 0))
  "The interest accrued on PAR from ISSUE to SETTLEMENT, at RATE."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-rate-check rate par)
  (when (>= issue settlement) (grid-calc-raise "#NUM!"))
  (* par rate (grid-calc--year-fraction issue settlement basis)))

;;;----------------------------------------------------------------------
;;; Discounted securities
;;;----------------------------------------------------------------------

(defun grid-calc--finance-dates-check (settlement maturity)
  "Give #NUM! unless SETTLEMENT is before MATURITY."
  (when (>= settlement maturity) (grid-calc-raise "#NUM!")))

(defun grid-calc--finance-years (settlement maturity basis)
  "The years from SETTLEMENT to MATURITY, counted by BASIS."
  (grid-calc--year-fraction settlement maturity basis))

(grid-calc-define disc ((settlement date) (maturity date) (price number)
                        (redemption number) &optional (basis integer 0))
  "The rate at which a security bought at PRICE is discounted."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-dates-check settlement maturity)
  (when (or (<= price 0) (<= redemption 0)) (grid-calc-raise "#NUM!"))
  (/ (- redemption price) redemption
     (grid-calc--finance-years settlement maturity basis)))

(grid-calc-define intrate ((settlement date) (maturity date) (investment number)
                           (redemption number) &optional (basis integer 0))
  "The rate INVESTMENT comes to at REDEMPTION."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-dates-check settlement maturity)
  (when (or (<= investment 0) (<= redemption 0)) (grid-calc-raise "#NUM!"))
  (/ (- redemption investment) investment
     (grid-calc--finance-years settlement maturity basis)))

(grid-calc-define received ((settlement date) (maturity date)
                            (investment number) (discount number)
                            &optional (basis integer 0))
  "What to invest at DISCOUNT now to have INVESTMENT at MATURITY."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-dates-check settlement maturity)
  (when (or (<= investment 0) (<= discount 0)) (grid-calc-raise "#NUM!"))
  (let ((years (grid-calc--finance-years settlement maturity basis)))
    (when (>= (* discount years) 1) (grid-calc-raise "#NUM!"))
    (/ investment (- 1 (* discount years)))))

(grid-calc-define pricedisc ((settlement date) (maturity date)
                             (discount number) (redemption number)
                             &optional (basis integer 0))
  "The price of a discounted security worth REDEMPTION."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-dates-check settlement maturity)
  (when (or (<= discount 0) (<= redemption 0)) (grid-calc-raise "#NUM!"))
  (- redemption
     (* redemption discount
        (grid-calc--finance-years settlement maturity basis))))

(grid-calc-define yielddisc ((settlement date) (maturity date) (price number)
                             (redemption number) &optional (basis integer 0))
  "The yield of a discounted security bought at PRICE."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-dates-check settlement maturity)
  (when (or (<= price 0) (<= redemption 0)) (grid-calc-raise "#NUM!"))
  (/ (- redemption price) price
     (grid-calc--finance-years settlement maturity basis)))

(grid-calc-define pricemat ((settlement date) (maturity date) (issue date)
                            (rate number) &optional (yield number 0)
                            (basis integer 0))
  "The price of a security paying RATE from ISSUE to MATURITY."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-dates-check settlement maturity)
  (when (or (< rate 0) (< yield 0) (>= issue settlement))
    (grid-calc-raise "#NUM!"))
  (let* ((dim (grid-calc--finance-years issue maturity basis))
         (a (grid-calc--finance-years issue settlement basis))
         (dsm (grid-calc--finance-years settlement maturity basis))
         (coupon (* 100.0 rate)))
    (- (/ (+ 100 (* dim coupon)) (+ 1 (* dsm yield)))
       (* a coupon))))

(grid-calc-define yieldmat ((settlement date) (maturity date) (issue date)
                            (rate number) &optional (price number 0)
                            (basis integer 0))
  "The yield of a security paying RATE from ISSUE to MATURITY."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-dates-check settlement maturity)
  (when (or (< rate 0) (<= price 0) (>= issue settlement))
    (grid-calc-raise "#NUM!"))
  (let* ((dim (grid-calc--finance-years issue maturity basis))
         (a (grid-calc--finance-years issue settlement basis))
         (dsm (grid-calc--finance-years settlement maturity basis))
         (coupon (* 100.0 rate))
         (paid (+ price (* a coupon))))
    (/ (- (+ 100 (* dim coupon)) paid) paid dsm)))

;;;----------------------------------------------------------------------
;;; Treasury bills
;;;----------------------------------------------------------------------

(defun grid-calc--finance-bill-days (settlement maturity)
  "The days from SETTLEMENT to MATURITY, which must be within the year."
  (grid-calc--finance-dates-check settlement maturity)
  (when (> (grid-calc--days-360 settlement maturity nil t) 360)
    (grid-calc-raise "#NUM!"))
  (- maturity settlement))

(grid-calc-define tbillprice ((settlement date) (maturity date)
                              (discount number))
  "The price of a bill bought at DISCOUNT."
  :toolpak t
  (when (<= discount 0) (grid-calc-raise "#NUM!"))
  (let* ((days (grid-calc--finance-bill-days settlement maturity))
         (price (* 100 (- 1 (/ (* discount days) 360.0)))))
    (when (<= price 0) (grid-calc-raise "#NUM!"))
    price))

(grid-calc-define tbillyield ((settlement date) (maturity date) (price number))
  "The yield of a bill bought at PRICE."
  :toolpak t
  (when (<= price 0) (grid-calc-raise "#NUM!"))
  (/ (* (- 100 price) 360.0)
     (* price (grid-calc--finance-bill-days settlement maturity))))

(grid-calc-define tbilleq ((settlement date) (maturity date) (discount number))
  "The bond equivalent yield of a bill bought at DISCOUNT."
  :toolpak t
  (when (<= discount 0) (grid-calc-raise "#NUM!"))
  (let* ((days (grid-calc--finance-bill-days settlement maturity))
         (price (* 100 (- 1 (/ (* discount days) 360.0)))))
    (when (<= price 0) (grid-calc-raise "#NUM!"))
    (cond
     ((<= days 182) (/ (* 365.0 discount) (- 360 (* discount days))))
     ;; A year or less: the rate a bond paying twice a year would pay.
     ((<= days 365)
      (let* ((years (/ days 365.0))
             (squared (- (* years years)
                         (* (- (* 2 years) 1) (- 1 (/ 100.0 price))))))
        (when (< squared 0) (grid-calc-raise "#NUM!"))
        (/ (+ (* -2 years) (* 2 (sqrt squared)))
           (- (* 2 years) 1))))
     (t (* 2 (- (sqrt (/ 100.0 price)) 1))))))

;;;----------------------------------------------------------------------
;;; Bonds
;;;----------------------------------------------------------------------

(defun grid-calc--finance-base (yield frequency)
  "Return 1 + YIELD/FREQUENCY, or #NUM! when that is nothing."
  (let ((base (+ 1 (/ yield (float frequency)))))
    (when (<= base 0) (grid-calc-raise "#NUM!"))
    base))

(defun grid-calc--finance-bond-value (n a dsc e rate yield redemption frequency)
  "Return (DIRTY PRICE) of a bond per 100, from N A DSC E.
DIRTY is what the coupons still to come are worth, PRICE what is paid
for it, the interest accrued since the last coupon taken off."
  (let* ((base (grid-calc--finance-base yield frequency))
         (coupon (/ (* 100.0 rate) frequency))
         (fraction (/ dsc (float e)))
         (accrued (* coupon (/ a (float e)))))
    (if (= n 1)
        (let ((dirty (/ (+ redemption coupon)
                        (+ 1 (* fraction (/ yield (float frequency)))))))
          (list dirty (- dirty accrued)))
      (let ((dirty (+ (* redemption (expt base (- (+ (1- n) fraction))))
                      (cl-loop for k below n
                               sum (* coupon (expt base (- (+ k fraction))))))))
        (list dirty (- dirty accrued))))))

(defun grid-calc--finance-solve (function guess)
  "Return X where FUNCTION is zero, searching from GUESS, or #NUM!."
  (let ((x guess) (found nil))
    (dotimes (_ 100)
      (unless found
        (let* ((value (funcall function x))
               (step (* 1e-7 (max 1.0 (abs x))))
               (slope (/ (- (funcall function (+ x step)) value) step)))
          (when (or (zerop slope) (isnan value) (isnan slope))
            (grid-calc-raise "#NUM!"))
          (let ((next (- x (/ value slope))))
            (cond
             ((< (abs (- next x)) 1e-12) (setq found next))
             ((not (and (< next 1e6) (> next -0.999))) (grid-calc-raise "#NUM!"))
             (t (setq x next)))))))
    (if (and found (not (isnan found)) (/= (abs found) 1.0e+INF)) found
      (grid-calc-raise "#NUM!"))))

(grid-calc-define price ((settlement date) (maturity date) (rate number)
                         (yield number) (redemption number)
                         (frequency integer) &optional (basis integer 0))
  "The price per 100 of a bond paying RATE and worth REDEMPTION."
  :toolpak t
  (pcase-let ((`(,n ,_ ,_ ,a ,dsc ,e)
               (grid-calc--finance-coupon-setup
                settlement maturity frequency basis)))
    (when (or (< rate 0) (< yield 0) (<= redemption 0))
      (grid-calc-raise "#NUM!"))
    (nth 1 (grid-calc--finance-bond-value
            n a dsc e rate yield redemption frequency))))

(grid-calc-define yield ((settlement date) (maturity date) (rate number)
                         (price number) (redemption number)
                         (frequency integer) &optional (basis integer 0))
  "The yield of a bond bought at PRICE, paying RATE to REDEMPTION."
  :toolpak t
  (pcase-let ((`(,n ,_ ,_ ,a ,dsc ,e)
               (grid-calc--finance-coupon-setup
                settlement maturity frequency basis)))
    (when (or (< rate 0) (<= price 0) (<= redemption 0))
      (grid-calc-raise "#NUM!"))
    (let* ((fraction (/ dsc (float e)))
           (coupon (/ (* 100.0 rate) frequency)))
      (if (= n 1)
          (/ (* (float frequency)
                (- (/ (+ redemption coupon)
                      (+ price (* coupon (/ a (float e)))))
                   1))
             fraction)
        (grid-calc--finance-solve
         (lambda (guess)
           (- (nth 1 (grid-calc--finance-bond-value
                      n a dsc e rate guess redemption frequency))
              price))
         0.1)))))

(defun grid-calc--finance-duration-parts (coupon yield frequency n dsc e)
  "Return (DIRTY WEIGHTED) of a bond paying COUPON, for its duration."
  (let* ((base (grid-calc--finance-base yield frequency))
         (payment (/ (* 100.0 coupon) frequency))
         (fraction (/ dsc (float e)))
         (dirty 0.0) (weighted 0.0))
    (dotimes (k n)
      (let* ((periods (+ k fraction))
             (value (* (if (= k (1- n)) (+ 100 payment) payment)
                       (expt base (- periods)))))
        (setq dirty (+ dirty value)
              weighted (+ weighted (* periods value)))))
    (list dirty weighted)))

(grid-calc-define duration ((settlement date) (maturity date) (coupon number)
                            (yield number) (frequency integer)
                            &optional (basis integer 0))
  "The Macaulay duration, in years, of a bond paying COUPON."
  :toolpak t
  (pcase-let ((`(,n ,_ ,_ ,_ ,dsc ,e)
               (grid-calc--finance-coupon-setup
                settlement maturity frequency basis)))
    (when (or (<= coupon 0) (< yield 0)) (grid-calc-raise "#NUM!"))
    (let* ((parts (grid-calc--finance-duration-parts
                   coupon yield frequency n dsc e)))
      (when (zerop (nth 0 parts)) (grid-calc-raise "#NUM!"))
      (/ (nth 1 parts) (nth 0 parts) (float frequency)))))

(grid-calc-define mduration ((settlement date) (maturity date) (coupon number)
                             (yield number) (frequency integer)
                             &optional (basis integer 0))
  "The modified duration, in years, of a bond paying COUPON."
  :toolpak t
  (pcase-let ((`(,n ,_ ,_ ,_ ,dsc ,e)
               (grid-calc--finance-coupon-setup
                settlement maturity frequency basis)))
    (when (or (<= coupon 0) (< yield 0)) (grid-calc-raise "#NUM!"))
    (let* ((parts (grid-calc--finance-duration-parts
                   coupon yield frequency n dsc e)))
      (when (zerop (nth 0 parts)) (grid-calc-raise "#NUM!"))
      (/ (nth 1 parts) (nth 0 parts) (float frequency)
         (grid-calc--finance-base yield frequency)))))

;;;----------------------------------------------------------------------
;;; Odd first and last periods
;;;----------------------------------------------------------------------

(defun grid-calc--finance-odd-units (start end first frequency basis)
  "The coupon periods from START to END, counted at the quasi coupons of FIRST.
The first period it reaches counts its days over the length BASIS gives
a period, as 360 days a year for basis 2; each whole one after it
counts as one."
  (let ((step (grid-calc--finance-step frequency))
        (counted nil)
        (total 0.0))
    (when (< start end)
      (let ((months (nth 2 (grid-calc--finance-coupons start first frequency)))
            pcd ncd)
        (setq pcd (grid-calc--finance-coupon-date first months)
              ncd (grid-calc--finance-coupon-date first (- months step)))
        (while (< pcd end)
          (let ((low (max pcd start))
                (high (min ncd end)))
            (when (< low high)
              (setq total
                    (+ total
                       (if (and counted (= low pcd) (= high ncd))
                           1.0
                         (/ (grid-calc--finance-days low high basis)
                            (float (grid-calc--finance-period-days
                                    pcd ncd frequency basis)))))
                    counted t)))
          (setq months (- months step)
                pcd ncd
                ncd (grid-calc--finance-coupon-date first (- months step))))))
    total))

(defun grid-calc--finance-odd-price (settlement maturity issue first rate
                                                yield redemption frequency basis)
  "The price per 100 of a bond with an odd first period."
  (let* ((base (grid-calc--finance-base yield frequency))
         (coupon (/ (* 100.0 rate) frequency))
         (fraction (grid-calc--finance-odd-units
                    settlement first first frequency basis))
         (paid (* coupon (grid-calc--finance-odd-units
                          issue first first frequency basis)))
         (accrued (* coupon (grid-calc--finance-odd-units
                             issue settlement first frequency basis)))
         (count (1+ (/ (grid-calc--finance-months first maturity)
                       (grid-calc--finance-step frequency)))))
    (- (+ (* redemption (expt base (- (+ (1- count) fraction))))
          (/ paid (expt base fraction))
          (cl-loop for k from 2 to count
                   sum (/ coupon (expt base (+ (- k 1) fraction)))))
       accrued)))

(grid-calc-define oddfprice ((settlement date) (maturity date) (issue date)
                             (first date) (rate number) (yield number)
                             (redemption number) (frequency integer)
                             &optional (basis integer 0))
  "The price of a bond whose first coupon comes FIRST."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-frequency frequency)
  (when (or (< rate 0) (< yield 0) (<= redemption 0))
    (grid-calc-raise "#NUM!"))
  (when (or (>= issue settlement) (>= settlement first) (>= first maturity))
    (grid-calc-raise "#NUM!"))
  (grid-calc--finance-odd-price settlement maturity issue first rate
                                yield redemption frequency basis))

(grid-calc-define oddfyield ((settlement date) (maturity date) (issue date)
                             (first date) (rate number) (price number)
                             (redemption number) (frequency integer)
                             &optional (basis integer 0))
  "The yield of a bond whose first coupon comes FIRST."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-frequency frequency)
  (when (or (< rate 0) (<= price 0) (<= redemption 0))
    (grid-calc-raise "#NUM!"))
  (when (or (>= issue settlement) (>= settlement first) (>= first maturity))
    (grid-calc-raise "#NUM!"))
  (grid-calc--finance-solve
   (lambda (guess)
     (- (grid-calc--finance-odd-price settlement maturity issue first rate
                                      guess redemption frequency basis)
        price))
   0.1))

(defun grid-calc--finance-odd-last-units (start end last frequency basis)
  "The coupon periods from START to END, counted at the quasi coupons after LAST.
Each quasi coupon period counts its days from START to END over its own
length, as the odd last period of ODDLPRICE counts them."
  (let* ((step (grid-calc--finance-step frequency))
         (k 1)
         (pcd last)
         (ncd (grid-calc--finance-coupon-date last (- step)))
         (total 0.0))
    (while (< pcd end)
      (let ((low (max pcd start))
            (high (min ncd end)))
        (when (< low high)
          (setq total
                (+ total
                   (/ (grid-calc--finance-days low high basis)
                      (float (grid-calc--finance-bond-period
                              pcd ncd frequency basis)))))))
      (setq k (1+ k)
            pcd ncd
            ncd (grid-calc--finance-coupon-date last (- (* k step)))))
    total))

(defun grid-calc--finance-odd-last-price (settlement maturity last-interest
                                                     rate yield redemption
                                                     frequency basis)
  "The price per 100 of a bond with an odd last period.
The last coupon covers LAST-INTEREST to MATURITY, counted in the quasi
coupon periods after LAST-INTEREST, and the interest still to come from
SETTLEMENT the same way."
  (let* ((coupon (/ (* 100.0 rate) frequency))
         (last (grid-calc--finance-odd-last-units
                last-interest maturity last-interest frequency basis))
         (left (grid-calc--finance-odd-last-units
                settlement maturity last-interest frequency basis))
         (accrued (grid-calc--finance-odd-last-units
                   last-interest settlement last-interest frequency basis)))
    (- (/ (+ redemption (* last coupon))
          (+ 1 (* left (/ yield frequency))))
       (* accrued coupon))))

(grid-calc-define oddlprice ((settlement date) (maturity date)
                             (last-interest date) (rate number) (yield number)
                             (redemption number) (frequency integer)
                             &optional (basis integer 0))
  "The price of a bond whose last coupon came at LAST-INTEREST."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-frequency frequency)
  (when (or (< rate 0) (< yield 0) (<= redemption 0))
    (grid-calc-raise "#NUM!"))
  (when (or (<= settlement last-interest) (>= last-interest maturity))
    (grid-calc-raise "#NUM!"))
  (grid-calc--finance-odd-last-price settlement maturity last-interest rate
                                     yield redemption frequency basis))

(grid-calc-define oddlyield ((settlement date) (maturity date)
                             (last-interest date) (rate number) (price number)
                             (redemption number) (frequency integer)
                             &optional (basis integer 0))
  "The yield of a bond whose last coupon came at LAST-INTEREST."
  :toolpak t
  (grid-calc--finance-basis basis)
  (grid-calc--finance-frequency frequency)
  (when (or (< rate 0) (<= price 0) (<= redemption 0))
    (grid-calc-raise "#NUM!"))
  (when (or (<= settlement last-interest) (>= last-interest maturity))
    (grid-calc-raise "#NUM!"))
  (grid-calc--finance-solve
   (lambda (guess)
     (- (grid-calc--finance-odd-last-price
         settlement maturity last-interest rate guess redemption
         frequency basis)
        price))
   0.1))

;;;----------------------------------------------------------------------
;;; Payments over several periods
;;;----------------------------------------------------------------------

(defun grid-calc--finance-periods-check (rate periods present start end type)
  "Give #NUM! unless the periods of CUMIPMT make sense."
  (when (or (<= rate 0) (<= periods 0) (<= present 0)
            (< start 1) (< end start) (> end periods)
            (not (memq type '(0 1))))
    (grid-calc-raise "#NUM!"))
  (list (ceiling start) (truncate end)))

(grid-calc-define cumipmt ((rate number) (periods number) (present number)
                           (start number) (end number) (type number))
  "The interest paid from payment START to END of PERIODS on PRESENT."
  :toolpak t
  (let* ((bounds (grid-calc--finance-periods-check
                  rate periods present start end type))
         (start (nth 0 bounds))
         (end (nth 1 bounds))
         (sum 0.0))
    (dotimes (i (1+ (- end start)))
      (setq sum (+ sum (grid-calc--interest-part
                        rate (+ start i) periods present 0 type))))
    sum))

(grid-calc-define cumprinc ((rate number) (periods number) (present number)
                            (start number) (end number) (type number))
  "The principal paid from payment START to END of PERIODS on PRESENT.
It is what the loan has fallen by over those payments, from
the balance left after N of them, PRESENT times (G - (1 + RATE)^N) over
\(G - 1), G being (1 + RATE)^PERIODS: summed payment by payment, large
interests nearly cancel and Excel's last digits are lost.  Paid at the
start of each period, all of the first payment is principal and the
others are less by 1 + RATE."
  :toolpak t
  (let* ((bounds (grid-calc--finance-periods-check
                  rate periods present start end type))
         (start (nth 0 bounds))
         (end (nth 1 bounds))
         (payment (grid-calc--payment rate periods present 0 type)))
    (let* ((base (+ 1.0 rate))
           (growth (grid-calc--growth rate periods))
           (fallen (lambda (from to)
                     (if (> from to)
                         0
                       (- (/ (* present (- (expt base to)
                                           (expt base (1- from))))
                             (- growth 1)))))))
      (cond ((= type 0) (funcall fallen start end))
            ((= start 1) (+ payment (/ (funcall fallen 2 end) base)))
            (t (/ (funcall fallen start end) base))))))

(grid-calc-define ispmt ((rate number) (period number) (periods number)
                         (present number))
  "The interest paid in PERIOD of a loan of PRESENT paying no principal."
  (when (zerop periods) (grid-calc-raise "#DIV/0!"))
  (* present rate (- (/ (float period) periods) 1)))

;;;----------------------------------------------------------------------
;;; Investments
;;;----------------------------------------------------------------------

(grid-calc-define mirr ((values values) (finance number) (reinvest number))
  "The rate at which VALUES, one per period, are worth nothing."
  (let ((flows (grid-calc--range-numbers values)))
    (unless (<= 2 (length flows)) (grid-calc-raise "#DIV/0!"))
    (let ((positive 0.0) (negative 0.0) (i 0)
          (last (length flows)))
      (dolist (flow flows)
        (cond ((> flow 0)
               (setq positive (+ positive
                                 (* flow (expt (+ 1.0 reinvest)
                                               (- last i 1))))))
              ((< flow 0)
               (setq negative (+ negative
                                 (/ flow (expt (+ 1.0 finance) i))))))
        (setq i (1+ i)))
      (when (>= negative 0) (grid-calc-raise "#DIV/0!"))
      (- (expt (/ (- positive) negative) (/ 1.0 (1- last))) 1))))

(grid-calc-define fvschedule ((principal number) (schedule values))
  "PRINCIPAL grown by each of the rates in SCHEDULE, one after another."
  :toolpak t
  (let ((total principal))
    (grid-calc-each-value
     schedule
     (lambda (value _)
       (cond
        ((grid-calc-err-p value) (grid-calc-raise (grid-calc-err-code value)))
        ((grid-calc-blank-p value))
        ((numberp value) (setq total (* total (+ 1 value))))
        (t (grid-calc-raise "#VALUE!")))))
    total))

(grid-calc-define pduration ((rate number) (present number) (future number))
  "How many periods PRESENT takes to become FUTURE at RATE."
  (when (or (<= rate 0) (<= present 0) (<= future 0))
    (grid-calc-raise "#NUM!"))
  (/ (log (/ (float future) present)) (log (+ 1.0 rate))))

(grid-calc-define rri ((periods number) (present number) (future number))
  "The rate per period at which PRESENT becomes FUTURE."
  (when (or (<= periods 0) (<= present 0) (< future 0))
    (grid-calc-raise "#NUM!"))
  (- (expt (/ (float future) present) (/ 1.0 periods)) 1))

(defun grid-calc--finance-dollar-scale (fraction)
  "The power of ten FRACTION is written over, as DOLLARDE takes it."
  (let ((scale 1))
    (while (> fraction scale) (setq scale (* scale 10)))
    scale))

(grid-calc-define dollarde ((fractional number) (fraction number))
  "FRACTIONAL read as a fraction of FRACTION, as a decimal."
  :toolpak t
  (let* ((fraction (grid-calc-int fraction))
         (sign (if (< fractional 0) -1 1))
         (whole (truncate (abs fractional)))
         (part (- (abs fractional) whole)))
    (when (zerop fraction) (grid-calc-raise "#DIV/0!"))
    (* sign (+ whole (/ (* part (grid-calc--finance-dollar-scale fraction))
                        (float fraction))))))

(grid-calc-define dollarfr ((fractional number) (fraction number))
  "FRACTIONAL as a decimal read back as a fraction of FRACTION."
  :toolpak t
  (let* ((fraction (grid-calc-int fraction))
         (sign (if (< fractional 0) -1 1))
         (whole (truncate (abs fractional)))
         (part (- (abs fractional) whole)))
    (when (zerop fraction) (grid-calc-raise "#DIV/0!"))
    (* sign (+ whole (/ (* part (float fraction))
                        (grid-calc--finance-dollar-scale fraction))))))

;;;----------------------------------------------------------------------
;;; Depreciation
;;;----------------------------------------------------------------------

(grid-calc-define db ((cost number) (salvage number) (life number)
                      (period number) &optional (month number 12))
  "The depreciation in PERIOD by the fixed declining balance."
  (let ((period (truncate period))
        (month (truncate month)))
    (when (or (< cost 0) (< salvage 0) (<= life 0) (< period 1)
              (< month 1) (> month 12))
      (grid-calc-raise "#NUM!"))
    (if (or (zerop cost) (> period (+ life 1)))
        0.0
      (let* ((rate (/ (round (* 1000
                              (- 1 (expt (/ salvage (float cost))
                                         (/ 1.0 life)))))
                      1000.0))
             (value (float cost))
             (depreciation 0.0))
        (dotimes (i period)
          (setq depreciation
                (* value rate
                   (cond ((and (= i 0) (< month 12)) (/ month 12.0))
                         ((>= i life) (/ (- 12 month) 12.0))
                         (t 1.0)))
                value (- value depreciation)))
        depreciation))))

(grid-calc-define vdb ((cost number) (salvage number) (life number)
                       (start number) (end number)
                       &optional (factor number 2) (no-switch bool))
  "The depreciation from START to END by the declining balance."
  (when (or (< cost 0) (< salvage 0) (<= life 0) (< start 0) (< end start)
            (> end life) (<= factor 0))
    (grid-calc-raise "#NUM!"))
  (let ((rate (/ factor (float life)))
        (value (float cost))
        (total 0.0)
        (i 0))
    (while (< i (ceiling end))
      (let* ((room (max 0.0 (- value salvage)))
             (balance (min (* value rate) room))
             (straight (if (> (- life i) 0) (/ room (- life i)) 0.0))
             (depreciation (if no-switch
                               balance
                             (max balance straight)))
             (from (max start i))
             (to (min end (1+ i))))
        (when (< from to)
          (setq total (+ total (* depreciation (- to from)))))
        (setq value (- value depreciation)
              i (1+ i))))
    total))

(provide 'grid-table-calc-finance)

;;; grid-table-calc-finance.el ends here
