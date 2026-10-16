;;; grid-table-calc-text.el --- Text functions -*- lexical-binding: t -*-

;;; Commentary:

;; Taking text apart and putting it together, finding and replacing in
;; it, reading numbers from it, and writing numbers into it after a
;; format code as TEXT does.  The format codes are Excel's: up to four
;; sections for positive, negative, zero and text; the digit
;; placeholders 0 # and ?; commas for thousands; percent, scientific and
;; fractions; and the date and time codes.

;;; Code:

(require 'grid-table-calc)
(require 'grid-table-calc-math)
(require 'grid-table-calc-lookup)

;;;----------------------------------------------------------------------
;;; Taking text apart
;;;----------------------------------------------------------------------

(defun grid-calc--count (count)
  "Return COUNT of characters, which may not be negative."
  (when (< count 0) (grid-calc-raise "#VALUE!"))
  count)

(grid-calc-define left ((text text) &optional (count integer 1))
  "The first COUNT characters of TEXT."
  (substring text 0 (min (length text) (grid-calc--count count))))

(grid-calc-define right ((text text) &optional (count integer 1))
  "The last COUNT characters of TEXT."
  (substring text (max 0 (- (length text) (grid-calc--count count)))))

(grid-calc-define mid ((text text) (start integer) (count integer))
  "COUNT characters of TEXT from the STARTth, counting from 1."
  (when (< start 1) (grid-calc-raise "#VALUE!"))
  (let ((from (min (length text) (1- start))))
    (substring text from (min (length text) (+ from (grid-calc--count count))))))

(grid-calc-define len ((text text))
  "How many characters TEXT has."
  (length text))

(grid-calc-alias leftb left)
(grid-calc-alias rightb right)
(grid-calc-alias midb mid)
(grid-calc-alias lenb len)

(grid-calc-define upper ((text text))
  "TEXT in capitals."
  (upcase text))

(grid-calc-define lower ((text text))
  "TEXT in small letters."
  (downcase text))

(grid-calc-define proper ((text text))
  "TEXT with the first letter of each word a capital, the rest small."
  (let ((case-fold-search nil) (start t))
    (apply #'string
           (mapcar (lambda (char)
                     (prog1 (if start (upcase char) (downcase char))
                       (setq start (not (string-match-p "[[:alpha:]]" (string char))))))
                   text))))

(grid-calc-define trim ((text text))
  "TEXT with spaces at its ends gone and runs of spaces made one."
  (string-trim (replace-regexp-in-string " +" " " text) " +" " +"))

(grid-calc-define clean ((text text))
  "TEXT with its control characters gone."
  (replace-regexp-in-string "[\x00-\x1f]" "" text))

(grid-calc-define exact ((left text) (right text))
  "TRUE when LEFT and RIGHT are the same text, case and all."
  (grid-calc-bool (string= left right)))

(grid-calc-define char ((code integer))
  "The character whose code is CODE, 1 to 255."
  (unless (<= 1 code 255) (grid-calc-raise "#VALUE!"))
  (string (decode-char 'ucs (if (< code 128) code
                              ;; Windows-1252, as Excel on Windows has it
                              (or (aref [#x20AC nil #x201A #x0192 #x201E #x2026 #x2020
                                         #x2021 #x02C6 #x2030 #x0160 #x2039 #x0152 nil
                                         #x017D nil nil #x2018 #x2019 #x201C #x201D
                                         #x2022 #x2013 #x2014 #x02DC #x2122 #x0161
                                         #x203A #x0153 nil #x017E #x0178]
                                        (- code 128))
                                  code)))))

(grid-calc-define code ((text text))
  "The code of the first character of TEXT."
  (when (string-empty-p text) (grid-calc-raise "#VALUE!"))
  (aref text 0))

(grid-calc-define unichar ((code integer))
  "The character whose Unicode code point is CODE."
  (unless (and (<= 1 code #x10FFFF) (not (<= #xD800 code #xDFFF)))
    (grid-calc-raise "#VALUE!"))
  (string code))

(grid-calc-define unicode ((text text))
  "The Unicode code point of the first character of TEXT."
  (when (string-empty-p text) (grid-calc-raise "#VALUE!"))
  (aref text 0))

(grid-calc-define rept ((text text) (times integer))
  "TEXT written TIMES times."
  (when (< times 0) (grid-calc-raise "#VALUE!"))
  (when (> (* times (length text)) 32767) (grid-calc-raise "#VALUE!"))
  (apply #'concat (make-list times text)))

;;;----------------------------------------------------------------------
;;; Putting text together
;;;----------------------------------------------------------------------

(defun grid-calc--checked-length (text)
  "Return TEXT, or #VALUE! when it is longer than a cell can hold."
  (when (> (length text) 32767) (grid-calc-raise "#VALUE!"))
  text)

(grid-calc-define concatenate (&rest (texts text))
  "TEXTS put together."
  (grid-calc--checked-length (apply #'concat texts)))

(defun grid-calc--texts (arguments)
  "Return the text of each value of ARGUMENTS, `values' arguments."
  (let (texts)
    (dolist (argument arguments)
      (grid-calc-each-value argument (lambda (value _) (push (grid-calc-text value) texts))))
    (nreverse texts)))

(grid-calc-define concat (&rest (texts values))
  "The values of TEXTS, ranges among them, put together."
  (grid-calc--checked-length (apply #'concat (grid-calc--texts texts))))

(grid-calc-define textjoin ((delimiter values) (skip-empty bool) &rest (texts values))
  "The values of TEXTS put together with DELIMITER between them.
SKIP-EMPTY leaves out empty values.  DELIMITER of several values uses
them in turn."
  (let* ((delimiters (or (grid-calc--texts (list delimiter)) '("")))
         (texts (grid-calc--texts texts))
         (texts (if skip-empty (cl-remove "" texts :test #'string=) texts))
         (parts '()) (i 0))
    (while texts
      (push (pop texts) parts)
      (when texts
        (push (nth (% i (length delimiters)) delimiters) parts)
        (setq i (1+ i))))
    (grid-calc--checked-length (apply #'concat (nreverse parts)))))

;;;----------------------------------------------------------------------
;;; Finding and replacing
;;;----------------------------------------------------------------------

(defun grid-calc--find-text (wanted text start case-fold wildcards)
  "Return the position, from 1, of WANTED in TEXT from START, or #VALUE!."
  (when (or (< start 1) (> start (1+ (length text)))) (grid-calc-raise "#VALUE!"))
  (if (string-empty-p wanted) start
    (let* ((case-fold-search case-fold)
           (regexp (if (and wildcards (grid-calc-wildcard-p wanted))
                       (substring (grid-calc-wildcard-regexp wanted) 2 -2)
                     (regexp-quote wanted)))
           ;; A pattern beginning with * stands for text from START itself.
           (found (string-match regexp text (1- start))))
      (if found (1+ found) (grid-calc-raise "#VALUE!")))))

(grid-calc-define find ((wanted text) (text text) &optional (start integer 1))
  "The position of WANTED in TEXT, from START, matching case."
  (grid-calc--find-text wanted text start nil nil))

(grid-calc-define search ((wanted text) (text text) &optional (start integer 1))
  "The position of WANTED in TEXT, from START, regardless of case.
WANTED may hold wildcards: * for any characters and ? for any one."
  (grid-calc--find-text wanted text start t t))

(grid-calc-alias findb find)
(grid-calc-alias searchb search)

(grid-calc-define substitute ((text text) (old text) (new text) &optional (instance integer))
  "TEXT with OLD replaced by NEW, or only its INSTANCEth occurrence."
  (cond
   ((string-empty-p old) text)
   ((null instance) (string-replace old new text))
   ((< instance 1) (grid-calc-raise "#VALUE!"))
   (t (let ((pos 0) (n 0) found)
        (while (and (not found) (setq pos (string-search old text pos)))
          (setq n (1+ n))
          (if (= n instance) (setq found pos) (setq pos (+ pos (length old)))))
        (if found
            (concat (substring text 0 found) new (substring text (+ found (length old))))
          text)))))

(grid-calc-define replace ((text text) (start integer) (count integer) (new text))
  "TEXT with COUNT characters from the STARTth replaced by NEW."
  (when (or (< start 1) (< count 0)) (grid-calc-raise "#VALUE!"))
  (let* ((from (min (length text) (1- start)))
         (to (min (length text) (+ from count))))
    (concat (substring text 0 from) new (substring text to))))

(grid-calc-alias replaceb replace)

(defun grid-calc--delimiter-at (text delimiters from case-fold)
  "Return (START . END) of the first of DELIMITERS in TEXT from FROM."
  (let ((case-fold-search case-fold) best)
    (dolist (delimiter delimiters best)
      (let ((pos (if case-fold
                     (string-match (regexp-quote delimiter) text from)
                   (string-search delimiter text from))))
        (when (and pos (or (null best) (< pos (car best))))
          (setq best (cons pos (+ pos (length delimiter)))))))))

(defun grid-calc--text-around (text delimiter instance case-fold missing after)
  "Return TEXT before the INSTANCEth DELIMITER, or AFTER it.
A negative INSTANCE counts from the end; MISSING is given when there is
no such delimiter."
  (let* ((delimiters (grid-calc--texts (list delimiter)))
         (positions '()) (from 0))
    (when (or (= instance 0) (> (abs instance) (max 1 (length text))))
      (grid-calc-raise "#VALUE!"))
    (if (cl-some #'string-empty-p delimiters)
        (setq positions (list (cons 0 0) (cons (length text) (length text))))
      (while-let ((found (grid-calc--delimiter-at text delimiters from case-fold)))
        (push found positions)
        (setq from (max (1+ (car found)) (cdr found))))
      (setq positions (nreverse positions)))
    (let ((found (if (> instance 0) (nth (1- instance) positions)
                   (nth (+ (length positions) instance) positions))))
      (cond ((and found (>= (+ (length positions) instance) 0))
             (if after (substring text (cdr found)) (substring text 0 (car found))))
            (missing missing)
            (t (grid-calc-err "#N/A"))))))

(grid-calc-define textbefore ((text text) (delimiter values) &optional
                              (instance integer 1) (mode integer 0) (_end integer 0)
                              (missing any))
  "TEXT before the INSTANCEth DELIMITER; MODE 1 ignores case."
  :empty t
  (grid-calc--text-around text delimiter instance (= mode 1) missing nil))

(grid-calc-define textafter ((text text) (delimiter values) &optional
                             (instance integer 1) (mode integer 0) (_end integer 0)
                             (missing any))
  "TEXT after the INSTANCEth DELIMITER; MODE 1 ignores case."
  :empty t
  (grid-calc--text-around text delimiter instance (= mode 1) missing t))

;;;----------------------------------------------------------------------
;;; Reading numbers
;;;----------------------------------------------------------------------

(grid-calc-define value ((text any))
  "The number TEXT reads as: a number, a percentage, an amount, a date
or a time."
  (cond ((numberp text) text)
        ((stringp text) (or (grid-calc-text-number text) (grid-calc-raise "#VALUE!")))
        ((eq text :blank) 0)
        (t (grid-calc-raise "#VALUE!"))))

(grid-calc-define numbervalue ((text text) &optional (decimal text ".") (group text ","))
  "The number TEXT reads as, its decimal point DECIMAL and its thousands
separator GROUP."
  (let* ((decimal (if (string-empty-p decimal) "." (substring decimal 0 1)))
         (group (if (string-empty-p group) "," (substring group 0 1)))
         (body (replace-regexp-in-string "[ \t\n]" "" text))
         (percents 0))
    (while (string-suffix-p "%" body)
      (setq body (substring body 0 -1) percents (1+ percents)))
    (let* ((point (string-search decimal body))
           (whole (if point (substring body 0 point) body))
           (fraction (if point (substring body (1+ point)) "")))
      (when (or (string-search decimal fraction) (string-search group fraction))
        (grid-calc-raise "#VALUE!"))
      (let ((plain (concat (string-replace group "" whole)
                           (if point "." "") fraction)))
        (if (string-empty-p plain) 0
          (unless (string-match-p "\\`[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
                                  plain)
            (grid-calc-raise "#VALUE!"))
          (grid-calc--tidy-number (/ (string-to-number plain) (expt 100.0 percents))))))))

;;;----------------------------------------------------------------------
;;; Format codes
;;;----------------------------------------------------------------------

(defun grid-calc--code-sections (code)
  "Return the sections of format CODE, split at semicolons outside quotes."
  (let ((sections '()) (start 0) (i 0) (len (length code)) (quoted nil))
    (while (< i len)
      (let ((char (aref code i)))
        (cond (quoted (when (eq char ?\") (setq quoted nil)))
              ((eq char ?\") (setq quoted t))
              ((memq char '(?\\ ?_ ?*)) (setq i (1+ i)))
              ((eq char ?\[) (setq i (or (cl-position ?\] code :start i) i)))
              ((eq char ?\;) (push (substring code start i) sections)
               (setq start (1+ i)))))
      (setq i (1+ i)))
    (nreverse (cons (substring code start) sections))))

(defun grid-calc--code-tokens (section)
  "Return SECTION, one section of a format code, as a list of tokens.
A token is (lit TEXT), (digit CHAR), (point), (comma), (percent),
(exp SIGN), (slash), (at), (general), (date TEXT), (elapsed TEXT),
(ampm TEXT) or (cond OP NUMBER)."
  (let ((tokens '()) (i 0) (len (length section)) (case-fold-search t))
    (cl-flet ((lit (text) (push (list 'lit text) tokens)))
      (while (< i len)
        (let ((char (aref section i)) (rest (substring section i)))
          (cond
           ((eq char ?\")
            (let ((end (or (cl-position ?\" section :start (1+ i)) len)))
              (lit (substring section (1+ i) end))
              (setq i end)))
           ((eq char ?\\) (when (< (1+ i) len) (lit (string (aref section (1+ i)))))
            (setq i (1+ i)))
           ((eq char ?_) (lit " ") (setq i (1+ i)))
           ((eq char ?*) (setq i (1+ i)))
           ((eq char ?\[)
            (let* ((end (or (cl-position ?\] section :start i) (1- len)))
                   (inside (substring section (1+ i) end)))
              (cond
               ((string-match "\\`\\(h+\\|m+\\|s+\\)\\'" inside)
                (push (list 'elapsed (downcase inside)) tokens))
               ((string-match "\\`\\(<=\\|>=\\|<>\\|<\\|>\\|=\\)\\(-?[0-9.]+\\)\\'" inside)
                (push (list 'cond (match-string 1 inside)
                            (string-to-number (match-string 2 inside)))
                      tokens))
               ((string-match "\\`\\$\\([^-]*\\)" inside) (lit (match-string 1 inside))))
              (setq i end)))
           ((string-match "\\`general" rest)
            (push (list 'general) tokens) (setq i (+ i 6)))
           ((string-match "\\`\\(am/pm\\|a/p\\)" rest)
            (push (list 'ampm (match-string 0 rest)) tokens)
            (setq i (+ i (length (match-string 0 rest)) -1)))
           ((string-match "\\`\\(y+\\|m+\\|d+\\|h+\\|s+\\|e+\\)" rest)
            (let ((run (downcase (match-string 0 rest))))
              (if (and (eq (aref run 0) ?e) (< (length run) 2)
                       (< (1+ i) len) (memq (aref section (1+ i)) '(?+ ?-)))
                  (progn (push (list 'exp (string (aref section (1+ i)))) tokens)
                         (setq i (1+ i)))
                (push (list 'date (if (eq (aref run 0) ?e) "yyyy" run)) tokens)
                (setq i (+ i (length run) -1)))))
           ((memq char '(?0 ?# ??)) (push (list 'digit char) tokens))
           ((eq char ?.) (push (list 'point) tokens))
           ((eq char ?,) (push (list 'comma) tokens))
           ((eq char ?%) (push (list 'percent) tokens) (lit "%"))
           ((eq char ?/) (push (list 'slash) tokens))
           ((eq char ?@) (push (list 'at) tokens))
           (t (lit (string char)))))
        (setq i (1+ i))))
    (nreverse tokens)))

(defun grid-calc--date-code-p (tokens)
  "Return non-nil when TOKENS, a section's, are a date or time code."
  (and (cl-some (lambda (token) (memq (car token) '(date elapsed ampm))) tokens)
       (not (cl-some (lambda (token) (and (eq (car token) 'digit)
                                          (not (eq (cadr token) ?0))))
                     tokens))))

(defconst grid-calc--month-full-names
  ["January" "February" "March" "April" "May" "June" "July" "August"
   "September" "October" "November" "December"]
  "The months' names.")

(defconst grid-calc--day-full-names
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "The days' names, from Sunday.")

(defun grid-calc--format-date (serial tokens)
  "Return SERIAL written after TOKENS, a date or time code's."
  (let* ((fraction-digits
          ;; Zeros after the point after seconds give fractions of one.
          (cl-loop for (token next) on tokens
                   when (and (eq (car token) 'point) (eq (car next) 'digit))
                   return (cl-loop for tok in (cdr (memq token tokens))
                                   while (eq (car tok) 'digit) count t)))
         (step (/ 1.0 (* 86400 (expt 10 (or fraction-digits 0)))))
         (serial (* step (fround (/ (float serial) step))))
         (day (floor serial))
         (seconds (* (- serial day) 86400))
         (whole-seconds (floor (+ seconds 1e-9)))
         (date (grid-calc-serial-date day))
         (hour (/ whole-seconds 3600))
         (minute (% (/ whole-seconds 60) 60))
         (second (% whole-seconds 60))
         (twelve (cl-some (lambda (token) (eq (car token) 'ampm)) tokens))
         (out '())
         (skip-digits nil))
    (cl-loop
     for (token . rest) on tokens
     for previous = nil then last-date
     for last-date = (if (memq (car token) '(date elapsed)) (cadr token) previous)
     do
     (pcase token
       (`(lit ,text) (push text out))
       ;; In a date a slash is only a slash.
       ('(slash) (push "/" out))
       (`(point)
        (if fraction-digits
            (progn
              (push "." out)
              (push (substring (format (format "%%.%df" fraction-digits)
                                       (- seconds whole-seconds))
                               2)
                    out)
              (setq skip-digits t))
          (push "." out)))
       (`(digit ,char) (unless skip-digits (push (string char) out)))
       (`(ampm ,text)
        (let ((pm (>= hour 12)))
          (push (if (string= (downcase text) "a/p")
                    (let ((letter (if pm "p" "a")))
                      (if (eq (aref text 0) ?A) (upcase letter) letter))
                  (if pm "PM" "AM"))
                out)))
       (`(elapsed ,code)
        (let ((total (pcase (aref code 0)
                       (?h (+ (* day 24) hour))
                       (?m (+ (* day 1440) (* hour 60) minute))
                       (_ (+ (* day 86400) whole-seconds)))))
          (push (format (format "%%0%dd" (length code)) total) out)))
       (`(date ,code)
        (push
         (pcase (aref code 0)
           (?y (if (<= (length code) 2) (format "%02d" (% (nth 0 date) 100))
                 (format "%04d" (nth 0 date))))
           (?d (pcase (length code)
                 (1 (number-to-string (nth 2 date)))
                 (2 (format "%02d" (nth 2 date)))
                 (3 (substring (aref grid-calc--day-full-names
                                     (grid-calc--weekday-of day))
                               0 3))
                 (_ (aref grid-calc--day-full-names (grid-calc--weekday-of day)))))
           (?h (let ((h (if twelve (let ((h (% hour 12))) (if (= h 0) 12 h)) hour)))
                 (if (= (length code) 1) (number-to-string h) (format "%02d" h))))
           (?s (if (= (length code) 1) (number-to-string second)
                 (format "%02d" second)))
           (?m
            ;; Minutes after an hour or before seconds; else the month.
            (if (and (<= (length code) 2)
                     (or (and previous (memq (aref previous 0) '(?h)))
                         (cl-loop for tok in rest
                                  when (memq (car tok) '(date elapsed))
                                  return (eq (aref (cadr tok) 0) ?s))))
                (if (= (length code) 1) (number-to-string minute)
                  (format "%02d" minute))
              (let ((month (nth 1 date)))
                (pcase (length code)
                  (1 (number-to-string month))
                  (2 (format "%02d" month))
                  (3 (substring (aref grid-calc--month-full-names (1- month)) 0 3))
                  (5 (substring (aref grid-calc--month-full-names (1- month)) 0 1))
                  (_ (aref grid-calc--month-full-names (1- month))))))))
         out))
       (_ (setq skip-digits nil))))
    (apply #'concat (nreverse out))))

(defun grid-calc--weekday-of (serial)
  "Return the day of the week of SERIAL, 0 for Sunday."
  (mod (1- (floor serial)) 7))

(defun grid-calc--group-thousands (digits)
  "Return DIGITS, a string of them, with commas between thousands."
  (let ((out "") (n (length digits)))
    (dotimes (i n)
      (setq out (concat out (string (aref digits i))))
      (when (and (< i (1- n)) (= (% (- n i 1) 3) 0)) (setq out (concat out ","))))
    out))

(defun grid-calc--fill-integer (digits placeholders grouping)
  "Return DIGITS, the whole part, put into PLACEHOLDERS, characters 0 # ?.
Digits beyond the placeholders go before the first; GROUPING puts
commas between thousands."
  (let* ((digits (if (equal digits "0") "" digits))
         (needed (length placeholders))
         (padded (if (< (length digits) needed)
                     (concat (make-string (- needed (length digits)) ?\s) digits)
                   digits))
         (lead (- (length padded) needed))
         (out (substring padded 0 lead)))
    (cl-loop for i from 0 below needed
             for char = (aref padded (+ lead i))
             for placeholder = (aref placeholders i)
             do (setq out (concat out
                                  (if (eq char ?\s)
                                      (pcase placeholder (?0 "0") (?? " ") (_ ""))
                                    (string char)))))
    (if grouping
        (let ((spaces (and (string-match "\\`\\( *\\)" out) (match-string 1 out))))
          (concat spaces (grid-calc--group-thousands (substring out (length spaces)))))
      out)))

(defun grid-calc--fill-fraction (digits placeholders)
  "Return DIGITS after the point put into PLACEHOLDERS, characters 0 # ?."
  (let ((out (cl-loop for i below (length placeholders)
                      concat (string (aref digits i)))))
    ;; Trailing zeros at # go, at ? become spaces.
    (let ((i (1- (length placeholders))))
      (while (and (>= i 0) (eq (aref out i) ?0) (not (eq (aref placeholders i) ?0)))
        (aset out i (if (eq (aref placeholders i) ??) ?\s ?\0))
        (setq i (1- i))))
    (remove ?\0 out)))

(defun grid-calc--format-fraction (number tokens)
  "Return NUMBER written as a fraction after TOKENS, as # ?/? or ?/8."
  (let* ((slash (cl-position '(slash) tokens :test #'equal))
         (before (cl-subseq tokens 0 slash))
         (after (nthcdr (1+ slash) tokens))
         (denominator-digits (cl-loop for tok in after
                                      while (eq (car tok) 'digit) collect (cadr tok)))
         (fixed (let ((text (cl-loop for tok in after
                                     while (memq (car tok) '(digit lit))
                                     concat (if (eq (car tok) 'lit) (cadr tok) ""))))
                  (and (string-match "\\`[1-9][0-9]*\\'" text)
                       (null denominator-digits)
                       (string-to-number text))))
         (whole-tokens (let ((gap (cl-position-if (lambda (tok) (eq (car tok) 'lit))
                                                  before :from-end t)))
                         (and gap (cl-some (lambda (tok) (eq (car tok) 'digit))
                                           (cl-subseq before 0 gap))
                              (cl-subseq before 0 gap))))
         (sign (if (< number 0) "-" ""))
         (number (abs number))
         (whole (if whole-tokens (floor number) 0))
         (part (- number whole))
         (max-den (if fixed fixed (1- (expt 10 (max 1 (length denominator-digits))))))
         (best-num 0) (best-den 1))
    (if fixed
        (setq best-den fixed best-num (round (* part fixed)))
      (let ((best-err 2.0))
        (cl-loop for den from 1 to max-den
                 for num = (round (* part den))
                 for err = (abs (- part (/ (float num) den)))
                 when (< err (- best-err 1e-12))
                 do (setq best-err err best-num num best-den den))))
    (when (and whole-tokens (= best-num best-den)) (setq whole (1+ whole) best-num 0))
    (concat sign
            (if whole-tokens
                (concat (if (and (= whole 0) (> best-num 0)) "" (number-to-string whole))
                        (if (> best-num 0) " " ""))
              "")
            (if (and whole-tokens (= best-num 0)) ""
              (format "%s/%s" (if whole-tokens best-num
                                (+ best-num (* whole best-den)))
                      best-den)))))

(defun grid-calc--format-number (number tokens negative-shown)
  "Return NUMBER written after TOKENS, a number code's.
NEGATIVE-SHOWN says the section shows a minus sign for a negative."
  (cond
   ((cl-some (lambda (tok) (eq (car tok) 'slash)) tokens)
    (grid-calc--format-fraction (if negative-shown number (abs number)) tokens))
   (t
    (let* ((digit-index (lambda (pred)
                          (cl-position-if pred tokens)))
           (point-at (funcall digit-index (lambda (tok) (eq (car tok) 'point))))
           (exp-at (funcall digit-index (lambda (tok) (eq (car tok) 'exp))))
           (last-digit (cl-position-if (lambda (tok) (eq (car tok) 'digit)) tokens
                                       :end (or point-at exp-at (length tokens))
                                       :from-end t))
           (percents (cl-count-if (lambda (tok) (eq (car tok) 'percent)) tokens))
           ;; Commas right after the last whole digit divide by 1000 each.
           (scale-commas (if last-digit
                             (cl-loop for tok in (nthcdr (1+ last-digit) tokens)
                                      while (eq (car tok) 'comma) count t)
                           0))
           (grouping (and last-digit
                          (cl-loop for tok in (cl-subseq tokens 0 last-digit)
                                   thereis (eq (car tok) 'comma))))
           (int-places (concat (cl-loop for tok in (cl-subseq tokens 0 (or point-at exp-at
                                                                           (length tokens)))
                                        when (eq (car tok) 'digit) collect (cadr tok))))
           (frac-places (if point-at
                            (concat (cl-loop for tok in (cl-subseq tokens (1+ point-at)
                                                                   (or exp-at (length tokens)))
                                             when (eq (car tok) 'digit) collect (cadr tok)))
                          ""))
           (exp-places (if exp-at
                           (concat (cl-loop for tok in (nthcdr (1+ exp-at) tokens)
                                            when (eq (car tok) 'digit) collect (cadr tok)))
                         ""))
           (value (* (abs number) (expt 100 percents) (expt 1000.0 (- scale-commas))))
           (exponent 0))
      (when exp-at
        (if (zerop value) (setq exponent 0)
          ;; As many whole digits as placeholders, the exponent a multiple
          ;; of their count when # leads, as engineering notation.
          (let* ((places (max 1 (length int-places)))
                 (power (floor (log value 10))))
            (setq exponent (if (and (> (length int-places) 1)
                                    (eq (aref int-places 0) ?#))
                               (* places (floor power places))
                             (- power (1- places))))
            (setq value (/ value (expt 10.0 exponent)))
            (when (>= (grid-calc--round-to value (length frac-places) 'nearest)
                      (expt 10.0 (if (and (> (length int-places) 1)
                                          (eq (aref int-places 0) ?#))
                                     places
                                   places)))
              (setq value (/ value 10) exponent (1+ exponent))))))
      (let* ((rounded (grid-calc--round-to value (length frac-places) 'nearest))
             (text (format (format "%%.%df" (length frac-places)) rounded))
             (point (string-search "." text))
             (whole (if point (substring text 0 point) text))
             (fraction (if point (substring text (1+ point)) ""))
             (whole-text (grid-calc--fill-integer whole int-places grouping))
             (fraction-text (grid-calc--fill-fraction fraction frac-places))
             (sign (if (and negative-shown (< number 0)
                            (string-match-p "[1-9]" text))
                       "-" ""))
             (out '()) (whole-done nil) (fraction-done nil) (exp-done nil))
        (dolist (tok tokens)
          (pcase (car tok)
            ('lit (push (cadr tok) out))
            ('digit
             (cond ((and exp-at (> (cl-position tok tokens :test #'eq) exp-at))
                    (unless exp-done
                      (setq exp-done t)
                      (push (format (format "%%0%dd" (length exp-places)) (abs exponent)) out)))
                   ((and point-at (> (cl-position tok tokens :test #'eq) point-at))
                    (unless fraction-done (setq fraction-done t) (push fraction-text out)))
                   (t (unless whole-done (setq whole-done t) (push whole-text out)))))
            ('point (unless whole-done (setq whole-done t) (push whole-text out))
                    (push "." out))
            ('exp (unless whole-done (setq whole-done t) (push whole-text out))
                  (push (concat "E" (cond ((< exponent 0) "-")
                                          ((equal (cadr tok) "+") "+")
                                          (t "")))
                        out))
            ('general (push (grid-calc--general-text (abs number)) out))))
        (unless (or whole-done (not (cl-some (lambda (tok) (eq (car tok) 'digit)) tokens)))
          (push whole-text out))
        (concat sign (apply #'concat (nreverse out))))))))

(defun grid-calc--general-text (number)
  "Return NUMBER as the General format writes it, in 11 characters."
  (let ((text (grid-calc-number-text number)))
    (if (<= (length text) 11) text
      (let ((precision 10) result)
        (while (and (> precision 0)
                    (> (length (setq result (grid-calc-number-text
                                             (string-to-number
                                              (format (format "%%.%dg" precision) number)))))
                       11))
          (setq precision (1- precision)))
        (if (<= (length result) 11) result
          (let ((sci (upcase (format "%.5e" number))))
            (replace-regexp-in-string "\\.?0*E" "E" sci)))))))

(defun grid-calc--section-matches (tokens number)
  "Return non-nil when TOKENS have no condition or NUMBER meets it."
  (let ((condition (cl-find 'cond tokens :key #'car)))
    (or (null condition)
        (let ((bound (nth 2 condition)))
          (pcase (nth 1 condition)
            ("<" (< number bound)) ("<=" (<= number bound))
            (">" (> number bound)) (">=" (>= number bound))
            ("=" (= number bound)) ("<>" (/= number bound)))))))

(defun grid-calc-format-code (value code)
  "Return VALUE written after the Excel format CODE, as TEXT does."
  (let* ((sections (mapcar #'grid-calc--code-tokens (grid-calc--code-sections code)))
         (count (length sections)))
    (if (stringp value)
        (let ((text-section (or (nth 3 sections)
                                (cl-find-if (lambda (tokens) (assq 'at tokens)) sections))))
          (if text-section
              (mapconcat (lambda (tok)
                           (pcase (car tok)
                             ('lit (cadr tok)) ('at value) (_ "")))
                         text-section "")
            value))
      (let* ((conditional (cl-some (lambda (tokens) (assq 'cond tokens)) sections))
             (section-index
              (cond (conditional
                     (or (cl-position-if (lambda (tokens)
                                           (and (assq 'cond tokens)
                                                (grid-calc--section-matches tokens value)))
                                         sections)
                         (cl-position-if-not (lambda (tokens) (assq 'cond tokens))
                                             sections)
                         0))
                    ((and (< value 0) (>= count 2)) 1)
                    ((and (= value 0) (>= count 3)) 2)
                    (t 0)))
             (tokens (nth section-index sections))
             ;; Only the first section, and conditional ones, write the
             ;; minus sign; the others are written as a negative's own.
             (negative-shown (or (= section-index 0) conditional)))
        (cond
         ((assq 'at tokens) (grid-calc-raise "#VALUE!"))
         ((grid-calc--date-code-p tokens)
          (unless (<= 0 value grid-calc--last-serial*) (grid-calc-raise "#VALUE!"))
          (grid-calc--format-date value tokens))
         ((and (assq 'general tokens)
               (not (cl-some (lambda (tok) (eq (car tok) 'digit)) tokens)))
          (concat (if (and negative-shown (< value 0)) "-" "")
                  (mapconcat (lambda (tok)
                               (pcase (car tok)
                                 ('lit (cadr tok))
                                 ('general (grid-calc--general-text (abs value)))
                                 (_ "")))
                             tokens "")))
         ((not (cl-some (lambda (tok) (eq (car tok) 'digit)) tokens))
          (concat (if (and negative-shown (< value 0) (= count 1)) "-" "")
                  (mapconcat (lambda (tok) (if (eq (car tok) 'lit) (cadr tok) ""))
                             tokens "")))
         (t (grid-calc--format-number value tokens negative-shown)))))))

(defconst grid-calc--last-serial* 2958465.99999
  "The last moment a date and time format writes.")

(grid-calc-define text ((value any) (code text))
  "VALUE written after the format CODE, as \"0.00\" or \"yyyy-mm-dd\"."
  (let ((value (cond ((eq value :blank) 0)
                     ((eq value t) "TRUE")
                     ((eq value :false) "FALSE")
                     ((stringp value) (or (grid-calc-text-number value) value))
                     (t value))))
    (grid-calc--checked-length (grid-calc-format-code value code))))

(grid-calc-define dollar ((number number) &optional (decimals integer 2))
  "NUMBER as an amount of dollars, to DECIMALS places."
  (let* ((rounded (grid-calc--round-to number decimals 'nearest))
         (places (if (> decimals 0) (concat "." (make-string decimals ?0)) "")))
    (grid-calc-format-code rounded (format "$#,##0%s;($#,##0%s)" places places))))

(grid-calc-define fixed ((number number) &optional (decimals integer 2) (no-commas bool))
  "NUMBER rounded to DECIMALS places, with commas unless NO-COMMAS."
  (let* ((rounded (grid-calc--round-to number decimals 'nearest))
         (places (if (> decimals 0) (concat "." (make-string decimals ?0)) "")))
    (grid-calc-format-code rounded (concat (if no-commas "0" "#,##0") places))))

(provide 'grid-table-calc-text)

;;; grid-table-calc-text.el ends here
