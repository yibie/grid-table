;;; grid-table-format.el --- Number and date formats for grid cells -*- lexical-binding: t -*-

;;; Commentary:

;; A cell keeps a number and is shown through a format.  A format is the
;; :format property of the cell's style, nil for the general format or
;; one of
;;
;;   (number DECIMALS THOUSANDS)   1234.5 as 1,234.50
;;   (percent DECIMALS)            0.125 as 12.5%
;;   (currency DECIMALS SYMBOL)    1234.5 as $1,234.50
;;   (date PATTERN [EXCEL-CODE])   46308 as 2026-10-13; the code is the
;;                                 workbook's own when PATTERN only
;;                                 approximates it
;;   (text)                        what was typed, never a number
;;   (code EXCEL-CODE)             a format of the workbook the cell came
;;                                 from that has no counterpart here; it
;;                                 is shown as general and written back
;;
;; A date is the number of days since 1899-12-30, as in a spreadsheet,
;; with the time of day as its fraction.

;;; Code:

(require 'cl-lib)

(defconst grid-format-date-patterns
  '("yyyy-mm-dd" "yyyy/mm/dd" "mm/dd/yyyy" "dd/mm/yyyy" "yyyy年m月d日"
    "yyyy-mm-dd hh:mm" "yyyy-mm-dd hh:mm:ss" "hh:mm" "hh:mm:ss")
  "Date patterns offered when a cell is given a date format.")

(defconst grid-format--unix-epoch-serial 25569
  "The serial number of 1970-01-01.")

(defvar grid-format-date-system nil
  "1904 when serial numbers count days from 1904-01-01, 1900 when from 1900.
A workbook made on a Mac may count from 1904, and its serial numbers are
then 1462 days less for the same date.  Bound to its workbook's while a
formula is computed; nil leaves it to `grid-format-date-system-function'.")

(defvar grid-format-date-system-function nil
  "Function of no arguments giving the date system when nothing bound one.
It returns 1900, 1904 or nil, which is 1900.")

(defun grid-format-date-offset ()
  "Return how many days a serial number is short of the 1900 system's."
  (if (eql (or grid-format-date-system
               (and grid-format-date-system-function
                    (funcall grid-format-date-system-function)))
           1904)
      1462 0))

(defun grid-format-text-p (value)
  "Return non-nil when VALUE is text that must not be read as a number."
  (and (stringp value) (> (length value) 0)
       (get-text-property 0 'grid-text value)))

(defun grid-format-number (value)
  "Return VALUE as a number when it is one or reads as one, else nil."
  (cond ((numberp value) value)
        ((and (stringp value)
              (not (grid-format-text-p value))
              (string-match-p
               "\\`[ \t]*[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?[ \t]*\\'"
               value))
         (string-to-number value))))

(defun grid-format-round (number)
  "Round NUMBER to the nearest integer, a half going away from zero.
A product that falls a hair short of a half, as 1.005 * 100 does, still
counts as the half it stands for.  The allowance for that is a few
steps of what a float of that size can tell apart, so it never reaches a
number that is whole already, however large."
  (let* ((size (abs number))
         (rounded (cond ((integerp size) size)
                        ;; Floats this large have no half to round.
                        ((>= size 1e15) (truncate size))
                        (t (floor (+ size 0.5 (* size 1e-15)))))))
    (if (< number 0) (- rounded) rounded)))

;;;----------------------------------------------------------------------
;;; Dates
;;;----------------------------------------------------------------------

(defun grid-format-date-to-serial (year month day &optional hour minute second)
  "Return the serial number of YEAR, MONTH and DAY, with the time if given.
A month or a day outside its range carries over, so month 13 is January
of the next year and day 0 the last day of the month before."
  (let ((seconds (float-time
                  (encode-time (list (or second 0) (or minute 0) (or hour 0)
                                     day month year nil nil t)))))
    (- (if (or hour minute second)
           (+ grid-format--unix-epoch-serial (/ seconds 86400.0))
         (+ grid-format--unix-epoch-serial (floor seconds 86400)))
       (grid-format-date-offset))))

(defun grid-format-serial-to-date (serial)
  "Return (YEAR MONTH DAY HOUR MINUTE SECOND WEEKDAY) for SERIAL.
WEEKDAY is 0 for Sunday."
  (let ((time (decode-time
               (round (* (- (+ serial (grid-format-date-offset))
                            grid-format--unix-epoch-serial)
                         86400.0))
               t)))
    (list (nth 5 time) (nth 4 time) (nth 3 time)
          (nth 2 time) (nth 1 time) (nth 0 time) (nth 6 time))))

(defun grid-format-today ()
  "Return the serial number of today in the local time zone."
  (let ((now (decode-time)))
    (grid-format-date-to-serial (nth 5 now) (nth 4 now) (nth 3 now))))

(defun grid-format-now ()
  "Return the serial number of this moment in the local time zone."
  (let ((now (decode-time)))
    (grid-format-date-to-serial (nth 5 now) (nth 4 now) (nth 3 now)
                                (nth 2 now) (nth 1 now) (nth 0 now))))

(defun grid-format-date (serial pattern)
  "Return SERIAL written after PATTERN.
PATTERN is made of yyyy, yy, mm, m, dd, d, hh, h and ss; an mm or m that
follows the hour stands for minutes.  Everything else is copied."
  (pcase-let ((`(,year ,month ,day ,hour ,minute ,second)
               (grid-format-serial-to-date serial))
              (after-hour nil)
              (case-fold-search t))
    (replace-regexp-in-string
     "yyyy\\|yy\\|mm\\|m\\|dd\\|d\\|hh\\|h\\|ss"
     (lambda (token)
       (pcase (downcase token)
         ("yyyy" (format "%04d" year))
         ("yy" (format "%02d" (mod year 100)))
         ("dd" (format "%02d" day))
         ("d" (number-to-string day))
         ("hh" (setq after-hour t) (format "%02d" hour))
         ("h" (setq after-hour t) (number-to-string hour))
         ("ss" (format "%02d" second))
         ("mm" (prog1 (format "%02d" (if after-hour minute month))
                 (setq after-hour nil)))
         ("m" (prog1 (number-to-string (if after-hour minute month))
                (setq after-hour nil)))))
     pattern t t)))

(defun grid-format-parse-date (text)
  "Return the serial number of the date TEXT, or nil.
TEXT is a date written year first, such as 2026-10-13 or 2026/10/13, or
month first with slashes as in the United States, such as 10/13/2026,
optionally followed by a time such as 18:30 or 18:30:05."
  (when (and (stringp text)
             (or (string-match
                  (concat "\\`[ \t]*\\(?1:[0-9]\\{4\\}\\)\\(?2:[-/]\\)"
                          "\\(?3:[0-9]\\{1,2\\}\\)\\2\\(?4:[0-9]\\{1,2\\}\\)"
                          "\\(?:[ T]\\(?5:[0-9]\\{1,2\\}\\):\\(?6:[0-9]\\{2\\}\\)"
                          "\\(?::\\(?7:[0-9]\\{2\\}\\)\\)?\\)?[ \t]*\\'")
                  text)
                 (string-match
                  (concat "\\`[ \t]*\\(?3:[0-9]\\{1,2\\}\\)/\\(?4:[0-9]\\{1,2\\}\\)/"
                          "\\(?1:[0-9]\\{4\\}\\)"
                          "\\(?: \\(?5:[0-9]\\{1,2\\}\\):\\(?6:[0-9]\\{2\\}\\)"
                          "\\(?::\\(?7:[0-9]\\{2\\}\\)\\)?\\)?[ \t]*\\'")
                  text)))
    (let ((year (string-to-number (match-string 1 text)))
          (month (string-to-number (match-string 3 text)))
          (day (string-to-number (match-string 4 text)))
          (hour (and (match-string 5 text)
                     (string-to-number (match-string 5 text))))
          (minute (and (match-string 6 text)
                       (string-to-number (match-string 6 text))))
          (second (and (match-string 7 text)
                       (string-to-number (match-string 7 text)))))
      (when (and (>= year 1900) (<= 1 month 12) (<= 1 day 31)
                 (or (null hour) (and (< hour 24) (< minute 60)))
                 (or (null second) (< second 60)))
        (let ((serial (grid-format-date-to-serial year month day
                                                  hour minute second)))
          ;; February 30 would carry over into March: not a date.
          (and (= (nth 2 (grid-format-serial-to-date serial)) day)
               serial))))))

;;;----------------------------------------------------------------------
;;; Showing a value
;;;----------------------------------------------------------------------

(defun grid-format--fixed (number decimals thousands)
  "Return NUMBER with DECIMALS decimal places.
With THOUSANDS, its whole part is grouped by commas."
  (let* ((scale (expt 10.0 decimals))
         (rounded (/ (grid-format-round (* number scale)) scale))
         (text (format (format "%%.%df" decimals) (abs rounded)))
         (point (string-search "." text))
         (whole (if point (substring text 0 point) text))
         (fraction (if point (substring text point) "")))
    (when thousands
      (while (string-match "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\(,\\|\\'\\)" whole)
        (setq whole (replace-match "\\1,\\2\\3" t nil whole))))
    (concat (if (and (< rounded 0) (string-match-p "[1-9]" text)) "-" "")
            whole fraction)))

(defun grid-format-general (value)
  "Return the text of VALUE in the general format.
A number is written without the noise of binary fractions, so the sum
of 0.1 and 0.2 reads 0.3."
  (cond ((null value) "")
        ((eq value t) "TRUE")
        ((stringp value) value)
        ((integerp value) (number-to-string value))
        ((floatp value)
         (cond ((or (isnan value) (> (abs value) 1e300)) (format "%s" value))
               ((and (= value (ftruncate value)) (< (abs value) 1e15))
                (format "%d" (truncate value)))
               (t (format "%.15g" value))))
        (t (format "%s" value))))

(defun grid-format-value (value format)
  "Return the text of VALUE under FORMAT, or nil when FORMAT says nothing.
Only numbers are formatted; text keeps its own form."
  (let ((number (grid-format-number value)))
    (when number
      (pcase format
        (`(number ,decimals ,thousands)
         (grid-format--fixed number decimals thousands))
        (`(percent ,decimals)
         (concat (grid-format--fixed (* number 100) decimals nil) "%"))
        (`(currency ,decimals ,symbol)
         (let ((text (grid-format--fixed (abs number) decimals t)))
           (concat (if (and (< number 0) (string-match-p "[1-9]" text)) "-" "")
                   symbol text)))
        (`(date ,pattern . ,_)
         (and (>= number 0) (grid-format-date number pattern)))))))

(defun grid-format-display (value format)
  "Return the text a cell shows for computed VALUE under FORMAT."
  (or (and format (grid-format-value value format))
      (grid-format-general value)))

(defun grid-format-kind (format)
  "Return what FORMAT is a kind of: a symbol, or nil for general."
  (car-safe format))

(defun grid-format-decimals (format)
  "Return the number of decimal places FORMAT shows, or nil."
  (and (memq (car-safe format) '(number percent currency))
       (nth 1 format)))

(defun grid-format-with-decimals (format decimals)
  "Return FORMAT showing DECIMALS decimal places.
A format without decimal places becomes a plain number format."
  (let ((decimals (max 0 (min 12 decimals))))
    (pcase format
      (`(percent ,_) (list 'percent decimals))
      (`(currency ,_ ,symbol) (list 'currency decimals symbol))
      (`(number ,_ ,thousands) (list 'number decimals thousands))
      (_ (list 'number decimals nil)))))

;;;----------------------------------------------------------------------
;;; Reading what was typed
;;;----------------------------------------------------------------------

(defun grid-format--raw-number (number)
  "Return the text NUMBER is stored as."
  (grid-format-general number))

(defun grid-format--typed-decimals (text)
  "Return how many decimal places the number in TEXT was typed with."
  (if (string-match "\\.\\([0-9]+\\)" text)
      (length (match-string 1 text))
    0))

(defun grid-format-parse-entry (text)
  "Read TEXT as a user types it into a cell.
Return (RAW . FORMAT) when TEXT is a date, a percentage, an amount of
money or a number with thousands separators: RAW is the text of the
number to store and FORMAT the format that shows it as typed.  Return
nil for anything else, which is stored as it is."
  (when (stringp text)
    (let ((serial (grid-format-parse-date text))
          (case-fold-search nil))
      (cond
       (serial
        (cons (grid-format--raw-number serial)
              (list 'date
                    (concat (cond ((string-match-p
                                    "\\`[ \t]*[0-9]\\{1,2\\}/" text)
                                   "mm/dd/yyyy")
                                  ((string-search "/" text) "yyyy/mm/dd")
                                  (t "yyyy-mm-dd"))
                            (cond ((string-match-p ":[0-9]+:" text) " hh:mm:ss")
                                  ((string-search ":" text) " hh:mm")
                                  (t ""))))))
       ((string-match
         "\\`[ \t]*\\([-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\)[ \t]*%[ \t]*\\'"
         text)
        (let ((typed (match-string 1 text)))
          (cons (grid-format--raw-number
                 (/ (grid-format-round
                     (* (string-to-number typed)
                        (expt 10.0 (+ 8 (grid-format--typed-decimals typed)))))
                    (expt 10.0 (+ 10 (grid-format--typed-decimals typed)))))
                (list 'percent (grid-format--typed-decimals typed)))))
       ((string-match
         (concat "\\`[ \t]*\\(-\\)?\\([$¥€£]\\)\\(-\\)?"
                 "\\([0-9][0-9,]*\\(?:\\.[0-9]+\\)?\\)[ \t]*\\'")
         text)
        (let ((negative (or (match-string 1 text) (match-string 3 text)))
              (symbol (match-string 2 text))
              (typed (string-replace "," "" (match-string 4 text))))
          (cons (concat (if negative "-" "") typed)
                (list 'currency (grid-format--typed-decimals typed) symbol))))
       ((string-match
         "\\`[ \t]*\\([-+]?[0-9]\\{1,3\\}\\(?:,[0-9]\\{3\\}\\)+\\(?:\\.[0-9]+\\)?\\)[ \t]*\\'"
         text)
        (let ((typed (string-replace "," "" (match-string 1 text))))
          (cons typed
                (list 'number (grid-format--typed-decimals typed) t))))))))

(defun grid-format-edit-text (raw format)
  "Return the text an edit of a cell starts from.
RAW is what the cell stores and FORMAT its format.  A date and a
percentage are edited as they are typed, not as the number behind them,
and text in a cell with the text format without the apostrophe that
`grid-table-read-entry' puts back."
  (let ((number (and (stringp raw) (not (string-prefix-p "=" raw))
                     (grid-format-number raw))))
    (pcase (and number format)
      (`(date . ,_)
       (if (< number 0)
           raw
         (grid-format-date
          number
          (if (= number (floor number))
              "yyyy-mm-dd"
            "yyyy-mm-dd hh:mm:ss"))))
      (`(percent ,_)
       (concat (grid-format-general
                (/ (grid-format-round (* number 1e12)) 1e10))
               "%"))
      (_ (if (and (equal format '(text)) (stringp raw)
                  (string-prefix-p "'" raw))
             (substring raw 1)
           raw)))))

;;;----------------------------------------------------------------------
;;; Spreadsheet format codes
;;;----------------------------------------------------------------------

(defun grid-format--code-decimals (zeros)
  "Return the number of decimal places ZEROS, such as \".00\", asks for."
  (if zeros (1- (length zeros)) 0))

(defun grid-format-from-code (code)
  "Return the format that spreadsheet format CODE stands for.
A code with no counterpart is kept as (code CODE)."
  (let ((case-fold-search t)
        (plain (and (stringp code)
                    ;; Colors, locales and literal marks say nothing here.
                    (replace-regexp-in-string
                     "\\[[^]$][^]]*\\]\\|\\[\\$-[^]]*\\]\\|\\\\\\|;@\\'" "" code))))
    (cond
     ((or (null plain) (string-empty-p plain)
          (string= (downcase plain) "general"))
      nil)
     ((string= plain "@") (list 'text))
     ((string-match "\\`\\(#,##\\)?0\\(\\.0+\\)?\\(?:_)\\|_ \\)?\\(?:;.*\\)?\\'" plain)
      (list 'number (grid-format--code-decimals (match-string 2 plain))
            (and (match-string 1 plain) t)))
     ((string-match "\\`0\\(\\.0+\\)?%\\'" plain)
      (list 'percent (grid-format--code-decimals (match-string 1 plain))))
     ((string-match
       (concat "\\`\\(?:\"\\([^\"]+\\)\"\\|\\([$¥€£]\\)\\|\\[\\$\\([^]-]*\\)[^]]*\\]\\)"
               "#,##0\\(\\.0+\\)?")
       plain)
      (list 'currency (grid-format--code-decimals (match-string 4 plain))
            (or (match-string 1 plain) (match-string 2 plain)
                (match-string 3 plain))))
     ((let ((bare (replace-regexp-in-string "\"[^\"]*\"" "" plain)))
        (and (string-match-p "[ymdhs]" bare)
             (not (string-match-p "[0#?]" bare))))
      (let ((pattern (downcase (string-replace "\"" "" plain))))
        (if (and (string-match-p "\\`\\(?:yyyy\\|yy\\|mm\\|m\\|dd\\|d\\|hh\\|h\\|ss\\|[-/:. 年月日时分秒]\\)+\\'"
                                 pattern))
            (list 'date pattern)
          ;; Month names, AM/PM and the like: show the nearest plain date
          ;; and keep the code for the way back.
          (list 'date
                (cond ((not (string-match-p "[yd]" pattern)) "hh:mm")
                      ((string-match-p "h" pattern) "yyyy-mm-dd hh:mm")
                      (t "yyyy-mm-dd"))
                code))))
     (t (list 'code code)))))

(defun grid-format-to-code (format)
  "Return the spreadsheet format code of FORMAT, or nil for general."
  (let ((zeros (lambda (decimals)
                 (if (> decimals 0) (concat "." (make-string decimals ?0)) ""))))
    (pcase format
      (`(number ,decimals ,thousands)
       (concat (if thousands "#,##0" "0") (funcall zeros decimals)))
      (`(percent ,decimals) (concat "0" (funcall zeros decimals) "%"))
      (`(currency ,decimals ,symbol)
       (concat "\"" symbol "\"#,##0" (funcall zeros decimals)))
      (`(date ,_ ,code) code)
      (`(date ,pattern)
       (replace-regexp-in-string "[年月日时分秒]+" "\"\\&\"" pattern t))
      (`(text) "@")
      (`(code ,code) code))))

(provide 'grid-table-format)

;;; grid-table-format.el ends here
