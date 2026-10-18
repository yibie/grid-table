;;; grid-table-calc-date.el --- Date and time functions -*- lexical-binding: t -*-

;;; Commentary:

;; Dates are Excel's serial numbers: days from 1900-01-01, which is 1,
;; with the time as the fraction of a day.  See `grid-calc-date-serial'.

;;; Code:

(require 'grid-table-calc)
(require 'grid-table-calc-math)

(defconst grid-calc--last-serial 2958465
  "The serial number of 9999-12-31, the last date there is.")

(defun grid-calc--serial (value)
  "Return VALUE as a serial number of a date, a number or text read as one.
Text that is not a date or a number, and a negative number, are errors."
  (let ((serial (grid-calc-num value)))
    (unless (<= 0 serial (1+ grid-calc--last-serial)) (grid-calc-raise "#NUM!"))
    serial))

(defun grid-calc--day-serial (value)
  "Return the day VALUE is a date of, its serial number without the time."
  (floor (grid-calc--snap (float (grid-calc--serial value)))))

(defun grid-calc--day-number (value)
  "Return VALUE as a date argument takes it: the day, not yet checked.
`grid-calc--day-checked' checks it once all the arguments are read."
  (floor (grid-calc--snap (float (grid-calc-num value)))))

(defun grid-calc--day-checked (day &optional strict)
  "Return DAY, or #NUM! when it is no date, as -1 is not.
The day after 9999-12-31 passes, unless STRICT."
  (unless (<= 0 day (if strict grid-calc--last-serial (1+ grid-calc--last-serial)))
    (grid-calc-raise "#NUM!"))
  day)

(defun grid-calc--checked-serial (serial)
  "Return SERIAL, or #NUM! when it is before 1900 or past 9999."
  (unless (<= 0 serial grid-calc--last-serial) (grid-calc-raise "#NUM!"))
  serial)

(defun grid-calc--time-parts (serial)
  "Return (HOUR MINUTE SECOND) of SERIAL, rounded to the second."
  (let* ((seconds (round (* (- serial (ffloor (float serial))) 86400.0)))
         (seconds (min seconds 86399)))
    (list (/ seconds 3600) (% (/ seconds 60) 60) (% seconds 60))))

;;;----------------------------------------------------------------------
;;; Making dates and times
;;;----------------------------------------------------------------------

(grid-calc-define today ()
  "The serial number of today."
  (let ((now (decode-time)))
    (grid-calc-date-serial (nth 5 now) (nth 4 now) (nth 3 now))))

(grid-calc-define now ()
  "The serial number of this moment."
  (let ((now (decode-time)))
    (+ (grid-calc-date-serial (nth 5 now) (nth 4 now) (nth 3 now))
       (/ (+ (* (nth 2 now) 3600) (* (nth 1 now) 60) (nth 0 now)) 86400.0))))

(grid-calc-define date ((year integer) (month integer) (day integer))
  "The serial number of YEAR, MONTH and DAY.
A YEAR below 1900 has 1900 added; a month or day outside its range
carries over into the years and months around it."
  (let ((year (if (< year 1900) (+ year 1900) year)))
    (unless (<= 1900 year 9999) (grid-calc-raise "#NUM!"))
    (grid-calc--checked-serial (grid-calc-date-serial year month day))))

(grid-calc-define time ((hour integer) (minute integer) (second integer))
  "The fraction of a day HOUR, MINUTE and SECOND are, past days left out."
  (let ((seconds (+ (* hour 3600) (* minute 60) second)))
    (when (< seconds 0) (grid-calc-raise "#NUM!"))
    (grid-calc--tidy-number (/ (mod seconds 86400) 86400.0))))

(grid-calc-define datevalue ((text text))
  "The serial number of the date TEXT names."
  (let ((serial (grid-calc--parse-date-time text)))
    (unless serial (grid-calc-raise "#VALUE!"))
    (floor serial)))

(grid-calc-define timevalue ((text text))
  "The fraction of a day the time TEXT names."
  (let ((serial (grid-calc--parse-date-time text)))
    (unless serial (grid-calc-raise "#VALUE!"))
    (grid-calc--tidy-number (- serial (ffloor (float serial))))))

;;;----------------------------------------------------------------------
;;; Parts of a date
;;;----------------------------------------------------------------------

(grid-calc-define year ((date date))
  "The year of DATE."
  (nth 0 (grid-calc-serial-date date)))

(grid-calc-define month ((date date))
  "The month of DATE, 1 to 12."
  (nth 1 (grid-calc-serial-date date)))

(grid-calc-define day ((date date))
  "The day of the month of DATE."
  (nth 2 (grid-calc-serial-date date)))

(grid-calc-define hour ((time any))
  "The hour of TIME, 0 to 23."
  (nth 0 (grid-calc--time-parts (grid-calc--serial time))))

(grid-calc-define minute ((time any))
  "The minute of TIME, 0 to 59."
  (nth 1 (grid-calc--time-parts (grid-calc--serial time))))

(grid-calc-define second ((time any))
  "The second of TIME, 0 to 59."
  (nth 2 (grid-calc--time-parts (grid-calc--serial time))))

(defun grid-calc--weekday (serial)
  "Return the day of the week of SERIAL, 0 for Sunday.
Excel's day 1 is a Sunday, and counting on from there holds for every
later day, its 29 February 1900 included."
  (mod (1- (+ (floor serial) (grid-format-date-offset))) 7))

(defconst grid-calc--weekday-starts
  '((1 . 0) (2 . 1) (11 . 1) (12 . 2) (13 . 3) (14 . 4) (15 . 5) (16 . 6) (17 . 0))
  "The day of the week that is 1, 0 for Sunday, for each WEEKDAY type.")

(grid-calc-define weekday ((date date) &optional (type integer 1))
  "The day of the week of DATE.
TYPE 1 counts Sunday as 1, 2 Monday as 1, 3 Monday as 0, and 11 to 17
count from Monday to Sunday as 1."
  (let ((day (grid-calc--weekday date)))
    (if (= type 3) (mod (- day 1) 7)
      (let ((start (cdr (assq type grid-calc--weekday-starts))))
        (unless start (grid-calc-raise "#NUM!"))
        (1+ (mod (- day start) 7))))))

(defun grid-calc--iso-week (serial)
  "Return the ISO week number of the day SERIAL."
  (let* ((weekday (mod (+ (grid-calc--weekday serial) 6) 7)) ; Monday 0
         (thursday (+ serial (- 3 weekday)))
         (year (nth 0 (grid-calc-serial-date thursday)))
         (first (grid-calc-date-serial year 1 1)))
    (1+ (/ (- thursday first) 7))))

(grid-calc-define isoweeknum ((date date))
  "The ISO week number of DATE: weeks start on Monday, and the first week
holds the year's first Thursday."
  (grid-calc--iso-week date))

(grid-calc-define weeknum ((date date) &optional (type integer 1))
  "The week of the year of DATE, the first week holding 1 January.
TYPE 1 starts weeks on Sunday, 2 and 11 on Monday, 12 to 17 on Tuesday
to Sunday; 21 counts ISO weeks."
  :toolpak t
  (if (= type 21) (grid-calc--iso-week date)
    (let* ((start (or (cdr (assq type grid-calc--weekday-starts))
                      (grid-calc-raise "#NUM!")))
           (first (grid-calc-date-serial (nth 0 (grid-calc-serial-date date)) 1 1))
           (offset (mod (- (grid-calc--weekday first) start) 7)))
      (1+ (/ (+ (- date first) offset) 7)))))

;;;----------------------------------------------------------------------
;;; Moving by months and counting days
;;;----------------------------------------------------------------------

(defun grid-calc--add-months (serial months end)
  "Return SERIAL moved by MONTHS whole months.
END gives the last day of that month; otherwise a day it does not have
becomes its last day."
  (pcase-let* ((`(,year ,month ,day) (grid-calc-serial-date serial))
               (last (grid-calc-date-serial year (+ month months 1) 0)))
    (grid-calc--checked-serial
     (if end last (min last (grid-calc-date-serial year (+ month months) day))))))

(grid-calc-define edate ((start date) (months integer))
  "The date MONTHS months after START, on the same day of the month."
  :toolpak t
  (grid-calc--add-months start months nil))

(grid-calc-define eomonth ((start date) (months integer))
  "The last day of the month MONTHS months after START."
  :toolpak t
  (grid-calc--add-months start months t))

(grid-calc-define days ((end date) (start date))
  "The days from START to END."
  (- end start))

(defun grid-calc--days-360 (start end european &optional both-februaries)
  "Return the days from START to END in a year of twelve 30-day months.
EUROPEAN takes a 31st as the 30th; otherwise the US rules apply, and
BOTH-FEBRUARIES, as YEARFRAC has them, makes END the 30th too when both
are the last day of February."
  (pcase-let ((`(,y1 ,m1 ,d1) (grid-calc-serial-date start))
              (`(,y2 ,m2 ,d2) (grid-calc-serial-date end)))
    (if european
        (setq d1 (min d1 30) d2 (min d2 30))
      (let ((last-of-feb (lambda (y m d)
                           (and (= m 2)
                                (= d (nth 2 (grid-calc-serial-date
                                             (grid-calc-date-serial y 3 0))))))))
        (when (and both-februaries (funcall last-of-feb y1 m1 d1)
                   (funcall last-of-feb y2 m2 d2))
          (setq d2 30))
        (when (= d1 31) (setq d1 30))
        (when (funcall last-of-feb y1 m1 d1) (setq d1 30))
        (when (and (= d2 31) (>= d1 30)) (setq d2 30))))
    (+ (* 360 (- y2 y1)) (* 30 (- m2 m1)) (- d2 d1))))

(grid-calc-define days360 ((start date) (end date) &optional (european bool))
  "The days from START to END in a year of twelve 30-day months.
The day after 9999-12-31 is #NUM! here, as in Excel."
  (grid-calc--days-360 (grid-calc--day-checked start t)
                       (grid-calc--day-checked end t)
                       european))

(defun grid-calc--leap-year-p (year)
  "Return non-nil when YEAR has 29 February."
  (and (= (% year 4) 0) (or (/= (% year 100) 0) (= (% year 400) 0))))

(defun grid-calc--year-fraction (start end basis)
  "Return the years from START to END, serial days, counted by BASIS.
BASIS 0 is US 30/360, 1 actual days by actual years, 2 actual/360,
3 actual/365 and 4 European 30/360."
  (when (> start end) (cl-rotatef start end))
  (pcase basis
    (0 (/ (grid-calc--days-360 start end nil t) 360.0))
    (4 (/ (grid-calc--days-360 start end t) 360.0))
    (2 (/ (- end start) 360.0))
    (3 (/ (- end start) 365.0))
    (1
     (pcase-let ((`(,y1 ,m1 ,d1) (grid-calc-serial-date start))
                 (`(,y2 ,_ ,_) (grid-calc-serial-date end)))
       (let ((days (- end start)))
         (if (or (= y1 y2)
                 (and (= (1+ y1) y2)
                      (<= end (grid-calc-date-serial y2 m1 d1))))
             ;; Within a year: by its days, 366 when a 29 February falls in
             (/ days (if (or (and (= y1 y2) (grid-calc--leap-year-p y1))
                             (and (/= y1 y2)
                                  (cl-some (lambda (y)
                                             (and (grid-calc--leap-year-p y)
                                                  (<= start (grid-calc-date-serial y 2 29) end)))
                                           (list y1 y2))))
                         366.0 365.0))
           (/ days (/ (float (- (grid-calc-date-serial (1+ y2) 1 1)
                                (grid-calc-date-serial y1 1 1)))
                      (1+ (- y2 y1))))))))
    (_ (grid-calc-raise "#NUM!"))))

(grid-calc-define yearfrac ((start any) (end any) &optional (basis any 0))
  "The years from START to END, the days counted by BASIS."
  :errors t
  :toolpak t
  ;; Excel looks at BASIS before the dates, so YEARFRAC(#N/A,1,9) is #NUM!.
  (let ((basis (grid-calc-int basis)))
    (unless (<= 0 basis 4) (grid-calc-raise "#NUM!"))
    (grid-calc--year-fraction (grid-calc--checked-serial (grid-calc--day-serial start))
                              (grid-calc--checked-serial (grid-calc--day-serial end))
                              basis)))

(grid-calc-define datedif ((start date) (end date) (unit text))
  "The whole years \"Y\", months \"M\" or days \"D\" from START to END;
\"YM\" the months past whole years, \"YD\" the days past whole years and
\"MD\" the days past whole months."
  (when (> start end) (grid-calc-raise "#NUM!"))
  (pcase-let ((`(,y1 ,m1 ,d1) (grid-calc-serial-date start))
              (`(,y2 ,m2 ,d2) (grid-calc-serial-date end)))
    (let ((months (- (+ (* 12 y2) m2) (+ (* 12 y1) m1) (if (< d2 d1) 1 0))))
      (pcase (upcase unit)
        ("Y" (/ months 12))
        ("M" months)
        ("D" (- end start))
        ("YM" (% months 12))
        ("MD" (if (>= d2 d1) (- d2 d1)
                (- end (grid-calc-date-serial y2 (1- m2) d1))))
        ("YD" (let ((anniversary (grid-calc-date-serial
                                  (if (or (> m2 m1) (and (= m2 m1) (>= d2 d1))) y2 (1- y2))
                                  m1 d1)))
                (- end anniversary)))
        (_ (grid-calc-raise "#NUM!"))))))

;;;----------------------------------------------------------------------
;;; Working days
;;;----------------------------------------------------------------------

(defconst grid-calc--weekend-numbers
  '((1 . (6 0)) (2 . (0 1)) (3 . (1 2)) (4 . (2 3)) (5 . (3 4)) (6 . (4 5))
    (7 . (5 6)) (11 . (0)) (12 . (1)) (13 . (2)) (14 . (3)) (15 . (4))
    (16 . (5)) (17 . (6)))
  "The days of the weekend, 0 for Sunday, for each weekend number.")

(defun grid-calc--weekend-value (ast)
  "Return the weekend the lazy argument AST gives, nil when left out.
A range gives its first value, and an error is a value, not raised."
  (when (and ast (not (equal ast '(MISSING))))
    (let ((value (grid-calc-arg ast 'values)))
      (when (grid-calc-ref-p value) (setq value (grid-calc--deref value)))
      (if (grid-calc-array-p value) (car (grid-calc-array-values value)) value))))

(defun grid-calc--weekend (weekend &optional all-off)
  "Return the days of the week WEEKEND names, as a list, 0 for Sunday.
WEEKEND is a weekend number, or seven 0s and 1s from Monday, 1 a day off;
seven 1s are #VALUE!, unless ALL-OFF.  Left out it is Saturday and
Sunday, but a blank cell is #NUM!, TRUE is 1, and an error #VALUE! when
ALL-OFF and else #NUM!, as Excel gives."
  (cond
   ((null weekend) '(6 0))
   ((grid-calc-blank-p weekend) (grid-calc-raise "#NUM!"))
   ((grid-calc-err-p weekend) (grid-calc-raise (if all-off "#VALUE!" "#NUM!")))
   ((grid-calc-boolean-p weekend)
    (grid-calc--weekend (if (eq weekend :false) 0 1)))
   ((stringp weekend)
    (unless (string-match-p "\\`[01]\\{7\\}\\'" weekend) (grid-calc-raise "#VALUE!"))
    (when (and (not all-off) (string= weekend "1111111")) (grid-calc-raise "#VALUE!"))
    (cl-loop for i below 7 when (eq (aref weekend i) ?1) collect (mod (1+ i) 7)))
   ((cdr (assq (truncate (grid-calc-num weekend)) grid-calc--weekend-numbers)))
   (t (grid-calc-raise "#NUM!"))))

(defun grid-calc--holidays (holidays)
  "Return the day serial numbers of HOLIDAYS, a `values' argument.
Blanks are none; TRUE, FALSE and text that is no date are #VALUE!, and a
number that is no date #NUM!."
  (let (days)
    (when holidays
      (grid-calc-each-value
       holidays
       (lambda (value _direct)
         (cond ((grid-calc-blank-p value))
               ((grid-calc-boolean-p value) (grid-calc-raise "#VALUE!"))
               (t (push (grid-calc--day-checked (grid-calc--day-number value) t)
                        days))))))
    (nreverse days)))

(defun grid-calc--work-day-p (serial weekend holidays)
  "Return non-nil when SERIAL is not in WEEKEND nor among HOLIDAYS."
  (not (or (memq (grid-calc--weekday serial) weekend) (memql serial holidays))))

(defun grid-calc--work-day (start days weekend holidays)
  "Return the day DAYS working days from START."
  (let ((serial start) (step (if (< days 0) -1 1)) (left (abs days)))
    (while (> left 0)
      (setq serial (+ serial step))
      (when (grid-calc--work-day-p serial weekend holidays) (setq left (1- left))))
    (grid-calc--checked-serial serial)))

(defun grid-calc--network-days (start end weekend holidays)
  "Return the working days from START to END, both counted."
  (let ((sign 1))
    (when (> start end) (cl-rotatef start end) (setq sign -1))
    (* sign (cl-loop for serial from start to end
                     count (grid-calc--work-day-p serial weekend holidays)))))

(grid-calc-define workday ((start date) (days integer) &optional (holidays values))
  "The date DAYS working days from START, weekends and HOLIDAYS left out."
  :toolpak t
  (grid-calc--work-day start days '(6 0)
                       (grid-calc--holidays holidays)))

(grid-calc-define workday.intl ((start date) (days integer) &optional (weekend lazy)
                                (holidays values))
  "The date DAYS working days from START, WEEKEND days and HOLIDAYS left out."
  :toolpak t
  (grid-calc--work-day start days
                       (grid-calc--weekend (grid-calc--weekend-value weekend))
                       (grid-calc--holidays holidays)))

(grid-calc-define networkdays ((start date) (end date) &optional (holidays values))
  "The working days from START to END, weekends and HOLIDAYS left out."
  :toolpak t
  (grid-calc--network-days start end
                           '(6 0) (grid-calc--holidays holidays)))

(grid-calc-define networkdays.intl ((start date) (end date) &optional (weekend lazy)
                                    (holidays values))
  "The working days from START to END, WEEKEND days and HOLIDAYS left out."
  :toolpak t
  (grid-calc--network-days start end
                           (grid-calc--weekend (grid-calc--weekend-value weekend) t)
                           (grid-calc--holidays holidays)))

(provide 'grid-table-calc-date)

;;; grid-table-calc-date.el ends here
