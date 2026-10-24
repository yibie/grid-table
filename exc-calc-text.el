;;; exc-calc-text.el --- Text functions -*- lexical-binding: t -*-

;;; Commentary:

;; Taking text apart and putting it together, finding and replacing in
;; it, reading numbers from it, and writing numbers into it after a
;; format code as TEXT does.  The format codes are Excel's: up to four
;; sections for positive, negative, zero and text; the digit
;; placeholders 0 # and ?; commas for thousands; percent, scientific and
;; fractions; and the date and time codes.

;;; Code:

(require 'exc-calc)
(require 'exc-calc-math)
(require 'exc-calc-lookup)
(require 'exc-obsolete)

;;;----------------------------------------------------------------------
;;; Taking text apart
;;;----------------------------------------------------------------------

(defun exc-calc--count (count)
  "Return COUNT of characters, which may not be negative."
  (when (< count 0) (exc-calc-raise "#VALUE!"))
  count)

(exc-calc-define left ((text text) &optional (count integer 1))
  "The first COUNT characters of TEXT."
  (substring text 0 (min (length text) (exc-calc--count count))))

(exc-calc-define right ((text text) &optional (count integer 1))
  "The last COUNT characters of TEXT."
  (substring text (max 0 (- (length text) (exc-calc--count count)))))

(exc-calc-define mid ((text text) (start integer) (count integer))
  "COUNT characters of TEXT from the STARTth, counting from 1."
  (when (< start 1) (exc-calc-raise "#VALUE!"))
  (let ((from (min (length text) (1- start))))
    (substring text from (min (length text) (+ from (exc-calc--count count))))))

(exc-calc-define len ((text text))
  "How many characters TEXT has."
  (length text))

(exc-calc-alias leftb left)
(exc-calc-alias rightb right)
(exc-calc-alias midb mid)
(exc-calc-alias lenb len)

(exc-calc-define upper ((text text))
  "TEXT in capitals."
  (upcase text))

(exc-calc-define lower ((text text))
  "TEXT in small letters."
  (downcase text))

(exc-calc-define proper ((text text))
  "TEXT with the first letter of each word a capital, the rest small."
  (let ((case-fold-search nil) (start t))
    (apply #'string
           (mapcar (lambda (char)
                     (prog1 (if start (upcase char) (downcase char))
                       (setq start (not (string-match-p "[[:alpha:]]" (string char))))))
                   text))))

(exc-calc-define trim ((text text))
  "TEXT with spaces at its ends gone and runs of spaces made one."
  (string-trim (replace-regexp-in-string " +" " " text) " +" " +"))

(exc-calc-define clean ((text text))
  "TEXT with its control characters gone."
  (replace-regexp-in-string "[\x00-\x1f]" "" text))

(exc-calc-define asc ((text text))
  "TEXT with its full-width letters, digits, signs and katakana made
half-width."
  (require 'japan-util)
  (japanese-hankaku text))

(exc-calc-define dbcs ((text text))
  "TEXT with its half-width letters, digits, signs and katakana made
full-width."
  (require 'japan-util)
  (japanese-zenkaku
   (apply #'string
          (mapcar (lambda (char)
                    (cond ((= char ?\s) #x3000)
                          ((<= #x21 char #x7e) (+ char #xfee0))
                          (t char)))
                  text))))

(exc-calc-define exact ((left text) (right text))
  "TRUE when LEFT and RIGHT are the same text, case and all."
  (exc-calc-bool (string= left right)))

(defconst exc-calc--windows-1252
  [#x20AC nil #x201A #x0192 #x201E #x2026 #x2020 #x2021 #x02C6 #x2030 #x0160
   #x2039 #x0152 nil #x017D nil nil #x2018 #x2019 #x201C #x201D #x2022 #x2013
   #x2014 #x02DC #x2122 #x0161 #x203A #x0153 nil #x017E #x0178]
  "The characters of codes 128 to 159 in Windows-1252, as Excel on
Windows has them; nil where it has none, and the code stands for itself.")

(defcustom exc-calc-character-set 'windows-1252
  "The character set CHAR and CODE number characters by.
As Excel on the system it runs on: `windows-1252' as on Windows, and
`mac-roman' as on a Mac, where CHAR(128) is Ä rather than €."
  :type '(choice (const windows-1252) (const mac-roman))
  :group 'exc)

(defconst exc-calc--mac-roman-apple '((#x0394 . #x2206) (#xE01E . #xF8FF))
  "Where Excel on a Mac has Apple's character for a Mac Roman code
rather than Emacs's: the increment sign, and the Apple logo.")

(exc-calc-define char ((code integer))
  "The character whose code is CODE, 1 to 255."
  (unless (<= 1 code 255) (exc-calc-raise "#VALUE!"))
  (cond ((< code 128) (string code))
        ((eq exc-calc-character-set 'mac-roman)
         (let ((char (aref (decode-coding-string (unibyte-string code)
                                                 'mac-roman)
                           0)))
           (string (or (cdr (assq char exc-calc--mac-roman-apple)) char))))
        (t (string (or (and (<= code 159)
                            (aref exc-calc--windows-1252 (- code 128)))
                       code)))))

(exc-calc-define code ((text text))
  "The code of the first character of TEXT, as CHAR has it."
  (when (string-empty-p text) (exc-calc-raise "#VALUE!"))
  (let ((char (aref text 0)))
    (if (eq exc-calc-character-set 'mac-roman)
        (let* ((char (or (car (rassq char exc-calc--mac-roman-apple)) char))
               (bytes (and (>= char 128)
                           (encode-coding-string (string char) 'mac-roman))))
          (if (and bytes (= (length bytes) 1) (>= (aref bytes 0) 128))
              (aref bytes 0)
            char))
      (let ((at (seq-position exc-calc--windows-1252 char)))
        (if at (+ 128 at) char)))))

(exc-calc-define unichar ((code integer))
  "The character whose Unicode code point is CODE."
  (unless (and (<= 1 code #x10FFFF) (not (<= #xD800 code #xDFFF)))
    (exc-calc-raise "#VALUE!"))
  (string code))

(exc-calc-define unicode ((text text))
  "The Unicode code point of the first character of TEXT."
  (when (string-empty-p text) (exc-calc-raise "#VALUE!"))
  (aref text 0))

(exc-calc-define rept ((text text) (times integer))
  "TEXT written TIMES times."
  (when (< times 0) (exc-calc-raise "#VALUE!"))
  (when (> (* times (length text)) 32767) (exc-calc-raise "#VALUE!"))
  (apply #'concat (make-list times text)))

;;;----------------------------------------------------------------------
;;; Splitting text
;;;----------------------------------------------------------------------

(defun exc-calc--split-delimiters (argument)
  "Return the delimiters of ARGUMENT, a `values' argument, as a list of
text, the longest first so that it cuts before the shorter it starts.
A cell left empty is no delimiter, and an empty text is #VALUE!."
  (let (delimiters)
    (dolist (value (exc-calc-flat-values argument))
      (cond ((exc-calc-err-p value)
             (exc-calc-raise (exc-calc-err-code value)))
            ((eq value :blank))
            (t (let ((text (exc-calc--text value)))
                 (when (string-empty-p text) (exc-calc-raise "#VALUE!"))
                 (push text delimiters)))))
    (cl-stable-sort delimiters (lambda (a b) (> (length a) (length b))))))

(defun exc-calc--splitting (text delimiters case-fold)
  "Return TEXT cut at each of DELIMITERS, empty pieces and all.
CASE-FOLD ignores case."
  (let ((regexp (mapconcat #'regexp-quote delimiters "\\|"))
        (case-fold-search case-fold)
        (start 0) (pieces '()))
    (while (string-match regexp text start)
      (push (substring text start (match-beginning 0)) pieces)
      (setq start (match-end 0)))
    (nreverse (cons (substring text start) pieces))))

(exc-calc-define textsplit ((text text) (columns values)
                             &optional (rows values) (ignore-empty bool)
                             (match-mode integer 0) (pad-with lazy))
  "TEXT cut at its COLUMNS delimiters into the cells of a row, and at its
ROWS delimiters into rows.  IGNORE-EMPTY leaves out empty pieces,
MATCH-MODE 1 ignores case, and PAD-WITH fills out rows, #N/A without it."
  :empty t
  (let ((columns (exc-calc--split-delimiters columns))
        (rows (exc-calc--split-delimiters rows))
        (pad (exc-calc--fill-value pad-with)))
    (unless (memq match-mode '(0 1)) (exc-calc-raise "#VALUE!"))
    ;; Neither delimiter is no way to split at all.
    (when (and (null columns) (null rows)) (exc-calc-raise "#VALUE!"))
    (let* ((case-fold (= match-mode 1))
           (lines (if rows (exc-calc--splitting text rows case-fold)
                    (list text)))
           (pieces (mapcar (lambda (line)
                             (let ((pieces (if columns
                                               (exc-calc--splitting
                                                line columns case-fold)
                                             (list line))))
                               (if ignore-empty
                                   (cl-remove "" pieces :test #'string=)
                                 pieces)))
                           lines))
           (pieces (if ignore-empty
                       (or (cl-remove-if #'null pieces) '(("")))
                     pieces))
           (width (apply #'max (mapcar #'length pieces))))
      (exc-calc-array (length pieces) width
                       (lambda (row col)
                         (or (nth col (nth row pieces)) pad))))))

;;;----------------------------------------------------------------------
;;; Putting text together
;;;----------------------------------------------------------------------

(defun exc-calc--checked-length (text)
  "Return TEXT, or #VALUE! when it is longer than a cell can hold."
  (when (> (length text) 32767) (exc-calc-raise "#VALUE!"))
  text)

(exc-calc-define concatenate (&rest (texts text))
  "TEXTS put together."
  (exc-calc--checked-length (apply #'concat texts)))

(defun exc-calc--texts (arguments)
  "Return the text of each value of ARGUMENTS, `values' arguments."
  (let (texts)
    (dolist (argument arguments)
      (exc-calc-each-value argument (lambda (value _) (push (exc-calc--text value) texts))))
    (nreverse texts)))

(exc-calc-define concat (&rest (texts values))
  "The values of TEXTS, ranges among them, put together."
  (exc-calc--checked-length (apply #'concat (exc-calc--texts texts))))

(exc-calc-define textjoin ((delimiter values) (skip-empty bool) &rest (texts values))
  "The values of TEXTS put together with DELIMITER between them.
SKIP-EMPTY leaves out empty values.  DELIMITER of several values uses
them in turn."
  (let* ((delimiters (or (exc-calc--texts (list delimiter)) '("")))
         (texts (exc-calc--texts texts))
         (texts (if skip-empty (cl-remove "" texts :test #'string=) texts))
         (parts '()) (i 0))
    (while texts
      (push (pop texts) parts)
      (when texts
        (push (nth (% i (length delimiters)) delimiters) parts)
        (setq i (1+ i))))
    (exc-calc--checked-length (apply #'concat (nreverse parts)))))

;;;----------------------------------------------------------------------
;;; Finding and replacing
;;;----------------------------------------------------------------------

(defun exc-calc--find-text (wanted text start case-fold wildcards)
  "Return the position, from 1, of WANTED in TEXT from START, or #VALUE!."
  (when (or (< start 1) (> start (1+ (length text)))) (exc-calc-raise "#VALUE!"))
  (if (string-empty-p wanted) start
    (let* ((case-fold-search case-fold)
           (regexp (if (and wildcards (exc-calc-wildcard-p wanted))
                       (substring (exc-calc-wildcard-regexp wanted) 2 -2)
                     (regexp-quote wanted)))
           ;; A pattern beginning with * stands for text from START itself.
           (found (string-match regexp text (1- start))))
      (if found (1+ found) (exc-calc-raise "#VALUE!")))))

(exc-calc-define find ((wanted text) (text text) &optional (start integer 1))
  "The position of WANTED in TEXT, from START, matching case."
  (exc-calc--find-text wanted text start nil nil))

(exc-calc-define search ((wanted text) (text text) &optional (start integer 1))
  "The position of WANTED in TEXT, from START, regardless of case.
WANTED may hold wildcards: * for any characters and ? for any one."
  (exc-calc--find-text wanted text start t t))

(exc-calc-alias findb find)
(exc-calc-alias searchb search)

(exc-calc-define substitute ((text text) (old text) (new text) &optional (instance integer))
  "TEXT with OLD replaced by NEW, or only its INSTANCEth occurrence."
  (cond
   ((string-empty-p old) text)
   ((null instance) (string-replace old new text))
   ((< instance 1) (exc-calc-raise "#VALUE!"))
   (t (let ((pos 0) (n 0) found)
        (while (and (not found) (setq pos (string-search old text pos)))
          (setq n (1+ n))
          (if (= n instance) (setq found pos) (setq pos (+ pos (length old)))))
        (if found
            (concat (substring text 0 found) new (substring text (+ found (length old))))
          text)))))

(exc-calc-define replace ((text text) (start integer) (count integer) (new text))
  "TEXT with COUNT characters from the STARTth replaced by NEW."
  (when (or (< start 1) (< count 0)) (exc-calc-raise "#VALUE!"))
  (let* ((from (min (length text) (1- start)))
         (to (min (length text) (+ from count))))
    (concat (substring text 0 from) new (substring text to))))

(exc-calc-alias replaceb replace)

(defun exc-calc--delimiter-at (text delimiters from case-fold)
  "Return (START . END) of the first of DELIMITERS in TEXT from FROM."
  (let ((case-fold-search case-fold) best)
    (dolist (delimiter delimiters best)
      (let ((pos (if case-fold
                     (string-match (regexp-quote delimiter) text from)
                   (string-search delimiter text from))))
        (when (and pos (or (null best) (< pos (car best))))
          (setq best (cons pos (+ pos (length delimiter)))))))))

(defun exc-calc--text-around (text delimiter instance mode end missing
                                     after)
  "Return TEXT before the INSTANCEth DELIMITER, or AFTER it.
MODE 1 ignores case; MODE other than 0 or 1 is #VALUE!.
INSTANCE is 1 when nil; a negative one counts from the end, and one with
a fraction is taken down to the whole number below it.  One given that
counts past where the shortest DELIMITER could stand in TEXT is
#VALUE!.  END 1 takes the end of TEXT for a delimiter after the last,
or its start before the first when INSTANCE is negative.  MISSING, a
lazy argument, is given when there is no such delimiter, an empty one as
empty text; #N/A without it."
  (let* ((delimiters (exc-calc--texts (list delimiter)))
         (given instance)
         (instance (if instance (floor instance) 1))
         (case-fold (= mode 1))
         (positions '()) (from 0))
    (when (or (= instance 0) (not (memq end '(0 1))) (not (memq mode '(0 1)))
              (and given
                   (> (abs instance)
                      (- (length text) (apply #'min (mapcar #'length delimiters))
                         -1))))
      (exc-calc-raise "#VALUE!"))
    (if (cl-some #'string-empty-p delimiters)
        (setq positions (list (cons 0 0) (cons (length text) (length text))))
      (while-let ((found (exc-calc--delimiter-at text delimiters from case-fold)))
        (push found positions)
        (setq from (max (1+ (car found)) (cdr found))))
      (setq positions (nreverse positions))
      (when (= end 1)
        (setq positions (if (> instance 0)
                            (append positions (list (cons (length text) (length text))))
                          (cons (cons 0 0) positions)))))
    (let ((found (if (> instance 0) (nth (1- instance) positions)
                   (nth (+ (length positions) instance) positions))))
      (cond ((and found (>= (+ (length positions) instance) 0))
             (if after (substring text (cdr found)) (substring text 0 (car found))))
            ((and missing (not (equal missing '(MISSING))))
             (let ((value (exc-calc-arg missing)))
               (if (exc-calc-blank-p value) "" value)))
            (t (exc-calc-err "#N/A"))))))

(exc-calc-define textbefore ((text text) (delimiter values) &optional
                              (instance number) (mode integer 0) (end integer 0)
                              (missing lazy))
  "TEXT before the INSTANCEth DELIMITER; MODE 1 ignores case.
See `exc-calc--text-around' for END and MISSING."
  :empty t
  (exc-calc--text-around text delimiter instance mode end missing nil))

(exc-calc-define textafter ((text text) (delimiter values) &optional
                             (instance number) (mode integer 0) (end integer 0)
                             (missing lazy))
  "TEXT after the INSTANCEth DELIMITER; MODE 1 ignores case.
See `exc-calc--text-around' for END and MISSING."
  :empty t
  (exc-calc--text-around text delimiter instance mode end missing t))

;;;----------------------------------------------------------------------
;;; Regular expressions
;;;----------------------------------------------------------------------
;;
;; REGEXTEST and its like read a pattern as PCRE2 does, which
;; `exc-calc--regexp' rewrites as an Emacs regexp.  ^ and $ are the
;; ends of the text, as PCRE2 has them without its multiline flag.

(defconst exc-calc--regexp-escapes
  '((?d "[0-9]" "0-9") (?D "[^0-9]") (?w "[[:alnum:]_]" "[:alnum:]_")
    (?W "[^[:alnum:]_]") (?s "[[:space:]]" "[:space:]") (?S "[^[:space:]]")
    (?b "\\b") (?B "\\B") (?A "\\`") (?z "\\'") (?Z "\\'")
    (?n "\n" "\n") (?t "\t" "\t") (?r "\r" "\r") (?f "\f" "\f") (?v "\v" "\v"))
  "What a backslash and a letter stand for in a pattern: as an Emacs
regexp, and inside a [ ] class.")

(defun exc-calc--regexp-class (pattern start)
  "Return (REGEXP . END) for the [ ] class of PATTERN opening at START.
END is the index past its ]."
  (let ((i (1+ start)) (n (length pattern)) negated items has-bracket has-caret
        has-dash)
    (when (and (< i n) (eq (aref pattern i) ?^)) (setq negated t) (cl-incf i))
    (cl-labels
        ((one-char ()
           ;; The next character of the class, a backslash read off;
           ;; a string where the escape stands for a set.
           (let ((c (aref pattern i)))
             (cl-incf i)
             (if (and (eq c ?\\) (< i n))
                 (let* ((e (aref pattern i))
                        (known (assq e exc-calc--regexp-escapes)))
                   (cl-incf i)
                   (cond ((null known) e)
                         ((nth 2 known)
                          (if (= (length (nth 2 known)) 1)
                              (aref (nth 2 known) 0)
                            (nth 2 known)))
                         (t (exc-calc-raise "#VALUE!"))))
               c))))
      (while (and (< i n) (or (not (eq (aref pattern i) ?\]))
                              ;; ] first is a ]
                              (= i (+ start 1 (if negated 1 0)))))
        (if (and (eq (aref pattern i) ?\[) (< (1+ i) n)
                 (eq (aref pattern (1+ i)) ?:))
            ;; [:alpha:] and its like
            (let ((end (string-search ":]" pattern (+ i 2))))
              (unless end (exc-calc-raise "#VALUE!"))
              (push (substring pattern i (+ end 2)) items)
              (setq i (+ end 2)))
          (let ((c (one-char)))
            (if (and (characterp c) (< (1+ i) n) (eq (aref pattern i) ?-)
                     (not (eq (aref pattern (1+ i)) ?\])))
                (progn
                  (cl-incf i)
                  (let ((last (one-char)))
                    (unless (and (characterp last) (<= c last))
                      (exc-calc-raise "#VALUE!"))
                    (push (format "%c-%c" c last) items)))
              (pcase c
                (?\] (setq has-bracket t))
                (?^ (setq has-caret t))
                (?- (setq has-dash t))
                ((pred characterp) (push (string c) items))
                (_ (push c items)))))))
      (unless (< i n) (exc-calc-raise "#VALUE!"))
      ;; Emacs reads ] only first, ^ anywhere but first and - only last.
      (let ((body (concat (if has-bracket "]" "")
                          (apply #'concat (nreverse items))
                          (if has-caret "^" "")
                          (if has-dash "-" ""))))
        (when (string-prefix-p "^" body)
          (setq body (concat (substring body 1) "^")))
        (when (equal body "")
          (exc-calc-raise "#VALUE!"))
        (cons (concat "[" (if negated "^" "") body "]") (1+ i))))))

(defun exc-calc--regexp (pattern)
  "Return PATTERN, a regular expression as PCRE2 reads one, as an Emacs
regexp.  What Emacs has nothing for, as a lookahead, is #VALUE!."
  (let ((i 0) (n (length pattern)) parts)
    (while (< i n)
      (let ((c (aref pattern i)))
        (cl-incf i)
        (push
         (pcase c
           (?\\
            (unless (< i n) (exc-calc-raise "#VALUE!"))
            (let ((e (aref pattern i)))
              (cl-incf i)
              (cond ((assq e exc-calc--regexp-escapes)
                     (nth 1 (assq e exc-calc--regexp-escapes)))
                    ((<= ?1 e ?9) (format "\\%c" e))
                    ((or (<= ?a e ?z) (<= ?A e ?Z) (<= ?0 e ?9))
                     (exc-calc-raise "#VALUE!"))
                    (t (regexp-quote (string e))))))
           (?\[
            (let ((class (exc-calc--regexp-class pattern (1- i))))
              (setq i (cdr class))
              (car class)))
           (?\(
            (cond ((not (eq (and (< i n) (aref pattern i)) ??)) "\\(")
                  ((string-prefix-p "?:" (substring pattern i))
                   (cl-incf i 2) "\\(?:")
                  ;; A named group counts as any other.
                  ((string-match "\\`?P?<[[:alpha:]_][[:alnum:]_]*>"
                                 (substring pattern i))
                   (cl-incf i (match-end 0)) "\\(")
                  (t (exc-calc-raise "#VALUE!"))))
           (?\) "\\)")
           (?| "\\|")
           (?^ "\\`")
           (?$ "\\'")
           (?{
            (if (string-match "\\`\\([0-9]+\\)\\(,[0-9]*\\)?}" (substring pattern i))
                (prog1 (concat "\\{" (substring pattern i (+ i (1- (match-end 0))))
                               "\\}")
                  (cl-incf i (match-end 0)))
              "{"))
           (_ (string c)))
         parts)))
    (apply #'concat (nreverse parts))))

(defun exc-calc--regexp-matches (pattern text case)
  "Return the match data of each match of PATTERN in TEXT, from the left.
CASE is 0 to match case or 1 to ignore it; any other is #VALUE!."
  (unless (memq case '(0 1)) (exc-calc-raise "#VALUE!"))
  (let ((regexp (exc-calc--regexp pattern))
        (case-fold-search (= case 1))
        (start 0) matches)
    (condition-case nil
        (while (and (<= start (length text)) (string-match regexp text start))
          (push (match-data) matches)
          (setq start (max (match-end 0) (1+ (match-beginning 0)))))
      (invalid-regexp (exc-calc-raise "#VALUE!")))
    (nreverse matches)))

(defun exc-calc--match-part (text match group)
  "Return the GROUPth part of MATCH, match data in TEXT, or \"\"."
  (let ((start (nth (* 2 group) match)))
    (if start (substring text start (nth (1+ (* 2 group)) match)) "")))

(exc-calc-define regextest ((text text) (pattern text) &optional (case integer 0))
  "TRUE when PATTERN matches some of TEXT; CASE 1 ignores case."
  :empty t
  (exc-calc-bool (exc-calc--regexp-matches pattern text case)))

(exc-calc-define regexextract ((text text) (pattern text) &optional
                                (mode integer 0) (case integer 0))
  "What PATTERN matches in TEXT: with MODE 0 the first match, 1 all of
them and 2 the groups of the first.  No match is #N/A."
  :empty t
  (unless (memq mode '(0 1 2)) (exc-calc-raise "#VALUE!"))
  (let* ((matches (exc-calc--regexp-matches pattern text case))
         (parts (pcase mode
                  (0 (list (exc-calc--match-part text (car matches) 0)))
                  (1 (mapcar (lambda (match) (exc-calc--match-part text match 0))
                             matches))
                  (2 (let ((groups (regexp-opt-depth (exc-calc--regexp pattern))))
                       (when (zerop groups) (exc-calc-raise "#VALUE!"))
                       (cl-loop for group from 1 to groups
                                collect (exc-calc--match-part
                                         text (car matches) group)))))))
    (cond ((null matches) (exc-calc-err "#N/A"))
          ((cdr parts) (exc-calc-array 1 (length parts)
                                        (lambda (_ col) (nth col parts))))
          (t (car parts)))))

(exc-calc-define regexreplace ((text text) (pattern text) (replacement text)
                                &optional (occurrence integer 0) (case integer 0))
  "TEXT with what PATTERN matches written as REPLACEMENT, where $1 is the
first group matched.  OCCURRENCE 0 replaces every match, another number
that one, counting from the end when it is less than 0."
  :empty t
  (let* ((matches (exc-calc--regexp-matches pattern text case))
         (count (length matches))
         (chosen (cond ((zerop occurrence) matches)
                       ((<= 1 occurrence count) (list (nth (1- occurrence) matches)))
                       ((<= 1 (- occurrence) count)
                        (list (nth (+ count occurrence) matches)))))
         (start 0) parts)
    (dolist (match chosen)
      (push (substring text start (car match)) parts)
      (push (replace-regexp-in-string
             "\\$\\(?:\\([0-9]+\\)\\|{\\([0-9]+\\)}\\|\\$\\)"
             (lambda (found)
               (let ((group (or (match-string 1 found) (match-string 2 found))))
                 (if group
                     (exc-calc--match-part text match (string-to-number group))
                   "$")))
             replacement t t)
            parts)
      (setq start (cadr match)))
    (push (substring text start) parts)
    (apply #'concat (nreverse parts))))

;;;----------------------------------------------------------------------
;;; Reading numbers
;;;----------------------------------------------------------------------

(exc-calc-define value ((text any))
  "The number TEXT reads as: a number, a percentage, an amount, a date
or a time."
  (cond ((numberp text) text)
        ((stringp text) (or (exc-calc-text-number text) (exc-calc-raise "#VALUE!")))
        ((eq text :blank) 0)
        (t (exc-calc-raise "#VALUE!"))))

(exc-calc-define numbervalue ((text text) &optional (decimal text ".") (group text))
  "The number TEXT reads as, its decimal point DECIMAL and its thousands
separator GROUP.  Parentheses about it make it negative, as Excel has
it, and percent signs within them count as those after them do; an
empty DECIMAL or GROUP, or the two alike, are #VALUE!.  GROUP left out
is a comma, or none when DECIMAL is one."
  (when (or (string-empty-p decimal) (equal group ""))
    (exc-calc-raise "#VALUE!"))
  (let* ((decimal (substring decimal 0 1))
         (group (cond (group (substring group 0 1))
                      ((equal decimal ",") nil)
                      (t ",")))
         (body (replace-regexp-in-string "[ \t\n]" "" text))
         (percents 0)
         (sign 1))
    (when (equal decimal group) (exc-calc-raise "#VALUE!"))
    (cl-flet ((strip-percents ()
                (while (string-suffix-p "%" body)
                  (setq body (substring body 0 -1) percents (1+ percents)))))
      (strip-percents)
      (when (string-match "\\`(\\(.*\\))\\'" body)
        (setq body (match-string 1 body) sign -1)
        (when (string-match-p "\\`[-+]" body) (exc-calc-raise "#VALUE!"))
        (strip-percents)))
    (let* ((point (string-search decimal body))
           (whole (if point (substring body 0 point) body))
           (fraction (if point (substring body (1+ point)) "")))
      (when (or (string-search decimal fraction)
                (and group (string-search group fraction)))
        (exc-calc-raise "#VALUE!"))
      (let ((plain (concat (if group (string-replace group "" whole) whole)
                           (if point "." "") fraction)))
        (if (string-empty-p plain) 0
          (unless (string-match-p "\\`[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
                                  plain)
            (exc-calc-raise "#VALUE!"))
          (exc-calc--tidy-number (/ (* sign (string-to-number plain))
                                     (expt 100.0 percents))))))))

;;;----------------------------------------------------------------------
;;; Format codes
;;;----------------------------------------------------------------------

(defun exc-calc--code-sections (code)
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

(defun exc-calc--code-tokens (section)
  "Return SECTION, one section of a format code, as a list of tokens.
A token is (lit TEXT), (digit CHAR), (point), (comma), (percent),
(exp SIGN LETTER), (slash), (at), (general), (date TEXT), (elapsed TEXT),
(ampm TEXT) or (cond OP NUMBER)."
  (let ((tokens '()) (i 0) (len (length section)) (case-fold-search t))
    (cl-flet ((lit (text) (push (list 'lit text) tokens)))
      (while (< i len)
        (let ((char (aref section i)) (rest (substring section i)))
          (cond
           ((eq char ?\")
            ;; A quote that is never closed is not a format code.
            (let ((end (cl-position ?\" section :start (1+ i))))
              (unless end (exc-calc-raise "#VALUE!"))
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
               ((string-match "\\` *\\(<=\\|>=\\|<>\\|<\\|>\\|=\\) *\\(-?[0-9.]+\\) *\\'" inside)
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
                  (progn (push (list 'exp (string (aref section (1+ i))) (string char))
                               tokens)
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

(defun exc-calc--date-code-p (tokens)
  "Return non-nil when TOKENS, a section's, are a date or time code."
  (and (cl-some (lambda (token) (memq (car token) '(date elapsed ampm))) tokens)
       (not (cl-some (lambda (token) (and (eq (car token) 'digit)
                                          (not (eq (cadr token) ?0))))
                     tokens))))

(defconst exc-calc--month-full-names
  ["January" "February" "March" "April" "May" "June" "July" "August"
   "September" "October" "November" "December"]
  "The months' names.")

(defconst exc-calc--day-full-names
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "The days' names, from Sunday.")

(defun exc-calc--format-date (serial tokens)
  "Return SERIAL written after TOKENS, a date or time code's."
  (let* ((fraction-digits
          ;; Zeros after the point after seconds give fractions of one.
          (cl-loop for (token next) on tokens
                   when (and (eq (car token) 'point) (eq (car next) 'digit))
                   return (cl-loop for tok in (cdr (memq token tokens))
                                   while (eq (car tok) 'digit) count t)))
         ;; An elapsed time may be negative; it is written after a minus.
         (sign (if (< serial 0) "-" ""))
         (per-second (expt 10 (or fraction-digits 0)))
         ;; The moment rounded to what the code shows, in whole units.
         (units (round (* (abs (float serial)) 86400 per-second)))
         (day (/ units (* 86400 per-second)))
         (whole-seconds (/ (% units (* 86400 per-second)) per-second))
         (seconds (+ whole-seconds (/ (float (% units per-second)) per-second)))
         (date (exc-calc-serial-date day))
         (elapsed-units (cl-loop for token in tokens
                                 when (eq (car token) 'elapsed)
                                 collect (aref (cadr token) 0)))
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
       ;; In a date a slash is only a slash, and a comma only a comma.
       ('(slash) (push "/" out))
       ('(comma) (push "," out))
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
       ;; Beside [h] or [s], h or s are the elapsed hours or seconds too.
       ((and `(date ,code)
             (guard (and (memq (aref code 0) '(?h ?s)) (memq (aref code 0) elapsed-units))))
        (push (format (format "%%0%dd" (min 2 (length code)))
                      (if (eq (aref code 0) ?h) (+ (* day 24) hour)
                        (+ (* day 86400) whole-seconds)))
              out))
       (`(date ,code)
        (push
         (pcase (aref code 0)
           (?y (if (<= (length code) 2) (format "%02d" (% (nth 0 date) 100))
                 (format "%04d" (nth 0 date))))
           (?d (pcase (length code)
                 (1 (number-to-string (nth 2 date)))
                 (2 (format "%02d" (nth 2 date)))
                 (3 (substring (aref exc-calc--day-full-names
                                     (exc-calc--weekday-of day))
                               0 3))
                 (_ (aref exc-calc--day-full-names (exc-calc--weekday-of day)))))
           (?h (let ((h (if twelve (let ((h (% hour 12))) (if (= h 0) 12 h)) hour)))
                 (if (= (length code) 1) (number-to-string h) (format "%02d" h))))
           (?s (if (= (length code) 1) (number-to-string second)
                 (format "%02d" second)))
           (?m
            ;; Minutes after an hour or a second or before seconds;
            ;; else the month.
            (if (and (<= (length code) 2)
                     (or (and previous (memq (aref previous 0) '(?h ?s)))
                         (cl-loop for tok in rest
                                  when (memq (car tok) '(date elapsed))
                                  return (eq (aref (cadr tok) 0) ?s))))
                (if (= (length code) 1) (number-to-string minute)
                  (format "%02d" minute))
              (let ((month (nth 1 date)))
                (pcase (length code)
                  (1 (number-to-string month))
                  (2 (format "%02d" month))
                  (3 (substring (aref exc-calc--month-full-names (1- month)) 0 3))
                  (5 (substring (aref exc-calc--month-full-names (1- month)) 0 1))
                  (_ (aref exc-calc--month-full-names (1- month))))))))
         out))
       (_ (setq skip-digits nil))))
    (concat sign (apply #'concat (nreverse out)))))

(defun exc-calc--weekday-of (serial)
  "Return the day of the week of SERIAL, 0 for Sunday."
  (mod (1- (+ (floor serial) (exc-format-date-offset))) 7))

(defun exc-calc--digit-p (token)
  "Return non-nil when TOKEN is a digit placeholder."
  (eq (car token) 'digit))

(defun exc-calc--pad (placeholder)
  "Return what PLACEHOLDER, 0 # or ?, shows where it has no digit."
  (pcase placeholder (?0 "0") (?? " ") (_ "")))

(defun exc-calc--fill-whole (digits placeholders &optional grouping pad)
  "Return DIGITS, a whole number's, put into PLACEHOLDERS, one string each.
The digits fill from the right; those beyond the placeholders go into
the first.  Where a placeholder has no digit it shows (funcall PAD
PLACEHOLDER), `exc-calc--pad' by default.  GROUPING puts a comma
between thousands, a space where the digit left of it is a space."
  (let* ((pad (or pad #'exc-calc--pad))
         (places (length placeholders))
         (count (max places (length digits)))
         ;; Characters from the right, the Kth for the Kth position.
         (shown (cl-loop for k from 1 to count
                         collect (if (<= k (length digits))
                                     (string (aref digits (- (length digits) k)))
                                   (funcall pad (aref placeholders (- places k))))))
         (slots (make-vector (max places 1) "")))
    (cl-loop for k from 1 to count
             for text = (nth (1- k) shown)
             for left = (nth k shown)
             for separator = (if (and grouping (= (% k 3) 0) left)
                                 (cond ((string= left " ") " ")
                                       ((string= left "") "")
                                       (t ","))
                               "")
             for slot = (max 0 (- places k))
             do (aset slots slot (concat separator text (aref slots slot))))
    (append slots nil)))

(defun exc-calc--fill-fraction (digits placeholders)
  "Return DIGITS after the point put into PLACEHOLDERS, one string each."
  (let* ((out (cl-loop for i below (length placeholders)
                       collect (string (aref digits i))))
         ;; A zero after the last digit that is not one shows what its own
         ;; placeholder says, the 0 placeholders further on included.
         (last (cl-loop for i downfrom (1- (length out)) to 0
                        unless (equal (nth i out) "0") return i)))
    (cl-loop for i from (1+ (or last -1)) below (length out)
             do (setf (nth i out) (exc-calc--pad (aref placeholders i))))
    out))

(defun exc-calc--placeholders (tokens)
  "Return the digit placeholders among TOKENS as a string."
  (concat (cl-loop for tok in tokens when (exc-calc--digit-p tok) collect (cadr tok))))

(defun exc-calc--token-width (tokens)
  "Return how many characters TOKENS take when every placeholder shows one."
  (cl-loop for tok in tokens
           sum (pcase (car tok) ('lit (length (cadr tok))) ((or 'digit 'slash) 1) (_ 0))))

(defun exc-calc--write-tokens (tokens slots)
  "Return TOKENS written, each digit placeholder by the next of SLOTS."
  (mapconcat (lambda (tok)
               (pcase (car tok)
                 ('lit (cadr tok))
                 ('digit (or (pop slots) ""))
                 ('slash "/")
                 (_ "")))
             tokens ""))

(defun exc-calc--best-fraction (part max-den)
  "Return (NUMERATOR . DENOMINATOR) nearest PART, the denominator at most MAX-DEN."
  (let ((best-num 0) (best-den 1) (best-err 2.0))
    (cl-loop for den from 1 to max-den
             for num = (round (* part den))
             for err = (abs (- part (/ (float num) den)))
             when (< err (- best-err 1e-12))
             do (setq best-err err best-num num best-den den))
    (cons best-num best-den)))

(defun exc-calc--format-fraction (number tokens)
  "Return NUMBER written as a fraction after TOKENS, as # ?/? or ?/8.
The placeholders right before the slash take the numerator, those
after it the denominator, and any before them, past the literal text
between, the whole part; without them the fraction is improper."
  (let* ((slash (cl-position 'slash tokens :key #'car))
         (before (cl-subseq tokens 0 slash))
         (after (nthcdr (1+ slash) tokens))
         ;; The numerator: the run of placeholders nearest the slash.
         (num-end (1+ (or (cl-position-if #'exc-calc--digit-p before :from-end t) -1)))
         (num-start (let ((i num-end))
                      (while (and (> i 0) (exc-calc--digit-p (nth (1- i) before)))
                        (setq i (1- i)))
                      i))
         (whole-end (1+ (or (cl-position-if #'exc-calc--digit-p before
                                            :end num-start :from-end t)
                            -1)))
         (whole-start (or (cl-position-if #'exc-calc--digit-p before) 0))
         (improper (= whole-end 0))
         (prefix (cl-subseq before 0 (if improper num-start whole-start)))
         (whole-tokens (unless improper (cl-subseq before whole-start whole-end)))
         (separator (unless improper (cl-subseq before whole-end num-start)))
         (num-tokens (cl-subseq before num-start num-end))
         (to-slash (nthcdr num-end before))
         ;; A denominator written as a number, as in ?/8 or ?/10.
         (fixed-tokens (cl-loop for tok in after
                                while (or (and (eq (car tok) 'lit)
                                               (string-match-p "\\`[0-9]+\\'" (cadr tok)))
                                          (equal tok '(digit ?0)))
                                collect tok))
         (fixed-text (mapconcat (lambda (tok) (if (eq (car tok) 'lit) (cadr tok) "0"))
                                fixed-tokens ""))
         (fixed (and (string-match-p "\\`[1-9]" fixed-text) (string-to-number fixed-text)))
         (den-start (if fixed 0 (or (cl-position-if #'exc-calc--digit-p after)
                                    (length after))))
         (den-end (if fixed (length fixed-tokens)
                    (let ((i den-start))
                      (while (and (< i (length after)) (exc-calc--digit-p (nth i after)))
                        (setq i (1+ i)))
                      i)))
         (from-slash (cl-subseq after 0 den-start))
         (den-tokens (cl-subseq after den-start den-end))
         (suffix (nthcdr den-end after))
         (whole-places (exc-calc--placeholders whole-tokens))
         (num-places (exc-calc--placeholders num-tokens))
         (den-places (if fixed "" (exc-calc--placeholders den-tokens)))
         (sign (if (< number 0) "-" ""))
         (value (abs number))
         (whole (if improper 0 (floor value)))
         (best (if fixed (cons (round (* (- value whole) fixed)) fixed)
                 (exc-calc--best-fraction
                  (- value whole) (1- (expt 10 (max 1 (length den-places)))))))
         (num (car best)) (den (cdr best)))
    (when (and (not improper) (= num den)) (setq whole (1+ whole) num 0))
    (let* ((hidden (and (not improper) (= num 0) (not (string-search "0" num-places))))
           (any-blank (string-search "?" (concat whole-places num-places den-places)))
           (whole-slots
            (unless improper
              (exc-calc--fill-whole
               (cond ((> whole 0) (number-to-string whole)) (hidden "0") (t ""))
               whole-places nil
               (lambda (placeholder)
                 (pcase placeholder
                   (?0 "0")
                   (?? (if (and (= num 0) (= value 0)) "0" " "))
                   (_ ""))))))
           (whole-text (exc-calc--write-tokens whole-tokens whole-slots))
           (whole-shown (string-match-p "[0-9]" (apply #'concat whole-slots)))
           (num-text (exc-calc--write-tokens
                      num-tokens (exc-calc--fill-whole (number-to-string num) num-places)))
           (den-text
            (cond (fixed (number-to-string den))
                  ((string-search "0" den-places)
                   (exc-calc--write-tokens
                    den-tokens (exc-calc--fill-whole (number-to-string den) den-places)))
                  (t (let ((digits (number-to-string den)))
                       (exc-calc--write-tokens
                        den-tokens
                        (cl-loop for i below (length den-places)
                                 collect (if (< i (length digits)) (string (aref digits i))
                                           (exc-calc--pad (aref den-places i)))))))))
           (fraction-text
            (cond (hidden
                   (if any-blank
                       (make-string (exc-calc--token-width
                                     (append separator num-tokens to-slash '((slash))
                                             from-slash den-tokens))
                                    ?\s)
                     ""))
                  (t (concat
                      (cond (improper "")
                            (whole-shown (exc-calc--write-tokens separator nil))
                            ((string-search "?" (concat whole-places num-places))
                             (make-string (exc-calc--token-width separator) ?\s))
                            (t ""))
                      num-text
                      (exc-calc--write-tokens to-slash nil) "/"
                      (exc-calc--write-tokens from-slash nil)
                      den-text)))))
      (concat sign (exc-calc--write-tokens prefix nil) whole-text fraction-text
              (exc-calc--write-tokens suffix nil)))))

(defun exc-calc--comma-roles (tokens)
  "Return what each comma among TOKENS does, a list as long as TOKENS.
A comma right after a placeholder and right before another one groups
thousands; one with a placeholder before it and one after it in the
whole number, but not right after it, is not written; one at the end of
the whole number divides by a thousand; any other is written as one."
  (let* ((point-at (cl-position 'point tokens :key #'car))
         (exp-at (cl-position 'exp tokens :key #'car))
         (whole-end (or point-at exp-at (length tokens)))
         (last-place (or (cl-position-if #'exc-calc--digit-p
                                         (cl-subseq tokens 0 whole-end)
                                         :from-end t)
                         -1)))
    (cl-loop for tok in tokens
             for i from 0
             for role = nil then this
             for this =
             (when (eq (car tok) 'comma)
               (let ((before (and (> i 0) (nth (1- i) tokens)))
                     (after (nth (1+ i) tokens))
                     (digit-after-in-part
                      (cl-some #'exc-calc--digit-p
                               (cl-subseq tokens (1+ i)
                                          (cond ((< i whole-end) whole-end)
                                                ((and exp-at (< i exp-at)) exp-at)
                                                (t (length tokens)))))))
                 (cond
                  ;; Commas after the last whole placeholder divide by a
                  ;; thousand, one after another too, as #,##0,, does.
                  ((and (< i whole-end) (> i last-place)
                        (cl-some #'exc-calc--digit-p (cl-subseq tokens 0 i)))
                   'scale)
                  ;; So does one right after such a comma past the point,
                  ;; as 0.0,, does.
                  ((and (eq role 'scale) (>= i whole-end) (not digit-after-in-part))
                   'scale)
                  ((not (and before (exc-calc--digit-p before))) 'literal)
                  ((and (< i whole-end) (exc-calc--digit-p after)) 'group)
                  ((and (>= i whole-end) (not digit-after-in-part)) 'scale)
                  (t 'ignore))))
             collect this)))

(defun exc-calc--fifteen-digits (number decimals)
  "Return NUMBER written with DECIMALS places, as Excel writes it.
It is read to 15 significant digits, and the places past them are 0."
  (if (zerop number)
      (format (format "%%.%df" decimals) 0)
    (let* ((sci (format "%.14e" (abs number)))
           (digits (concat (substring sci 0 1) (substring sci 2 16)))
           (exponent (string-to-number (substring sci 17)))
           (whole (if (< exponent 0) "0"
                    (let ((count (1+ exponent)))
                      (if (<= count 15) (substring digits 0 count)
                        (concat digits (make-string (- count 15) ?0))))))
           (fraction (if (< exponent 0)
                         (concat (make-string (- -1 exponent) ?0) digits)
                       (if (< (1+ exponent) 15) (substring digits (1+ exponent)) "")))
           (fraction (if (> (length fraction) decimals)
                         (substring fraction 0 decimals)
                       (concat fraction (make-string (- decimals (length fraction)) ?0)))))
      (concat (if (< number 0) "-" "") whole
              (if (> decimals 0) (concat "." fraction) "")))))

(defun exc-calc--format-number (number tokens negative-shown)
  "Return NUMBER written after TOKENS, a number code's.
NEGATIVE-SHOWN says the section shows a minus sign for a negative."
  (if (and (assq 'slash tokens)
           (not (assq 'point tokens)) (not (assq 'exp tokens)))
      (exc-calc--format-fraction (if negative-shown number (abs number)) tokens)
    (let* ((roles (exc-calc--comma-roles tokens))
           (point-at (cl-position 'point tokens :key #'car))
           (exp-at (cl-position 'exp tokens :key #'car))
           (whole-end (or point-at exp-at (length tokens)))
           (frac-end (or exp-at (length tokens)))
           (int-places (exc-calc--placeholders (cl-subseq tokens 0 whole-end)))
           (frac-places (if point-at
                            (exc-calc--placeholders (cl-subseq tokens (1+ point-at) frac-end))
                          ""))
           (exp-places (if exp-at (exc-calc--placeholders (nthcdr (1+ exp-at) tokens)) ""))
           (grouping (memq 'group roles))
           ;; Scientific notation neither scales by % nor by commas.
           (value (if exp-at (abs number)
                    (* (abs number)
                       (expt 100 (cl-count 'percent tokens :key #'car))
                       (expt 1000.0 (- (cl-count 'scale roles))))))
           (decimals (length frac-places))
           (exponent 0))
      (when exp-at
        (unless (zerop value)
          ;; The exponent a multiple of the whole part's placeholders.
          (let* ((places (max 1 (length int-places)))
                 (power (floor (log value 10))))
            (when (>= value (expt 10.0 (1+ power))) (setq power (1+ power)))
            (when (< value (expt 10.0 power)) (setq power (1- power)))
            (setq exponent (* places (floor power places)))
            (when (>= (exc-calc--round-to (/ value (expt 10.0 exponent)) decimals 'nearest)
                      (expt 10.0 places))
              (setq exponent (+ exponent places)))
            (setq value (/ value (expt 10.0 exponent))))))
      (let* ((rounded (exc-calc--round-to value decimals 'nearest))
             (text (exc-calc--fifteen-digits rounded decimals))
             (point (string-search "." text))
             (whole (if point (substring text 0 point) text))
             (whole-slots (exc-calc--fill-whole
                           (if (equal whole "0") "" whole)
                           ;; A zero in scientific notation fills every place.
                           (if (and exp-at (zerop value))
                               (make-string (length int-places) ?0)
                             int-places)
                           grouping))
             (frac-slots (exc-calc--fill-fraction (if point (substring text (1+ point)) "")
                                                   frac-places))
             (exp-text (let ((slots (exc-calc--fill-whole
                                     (if (= exponent 0) "" (number-to-string (abs exponent)))
                                     exp-places)))
                         (if (string-match-p "[0-9]" (apply #'concat slots))
                             (apply #'concat slots)
                           ;; The exponent shows at least its last digit.
                           (concat (apply #'concat (butlast slots))
                                   (let ((last (car (last slots))))
                                     (if (string= last "") "0"
                                       (concat (substring last 0 -1) "0")))))))
             (sign (if (and negative-shown (< number 0) (string-match-p "[1-9]" text)) "-" ""))
             ;; A whole number with no placeholder to sit in is written whole.
             (whole-text (if (and (null exp-at) (string-empty-p int-places)
                                  (not (equal whole "0")))
                             whole ""))
             (exp-sign (and exp-at
                            (cond ((< exponent 0) "-")
                                  ((equal (nth 1 (nth exp-at tokens)) "+") "+")
                                  (t ""))))
             (exp-done nil)
             (out '()))
        (cl-loop
         for tok in tokens
         for role in roles
         for i from 0
         do (pcase (car tok)
              ('lit (push (cadr tok) out))
              ('comma (when (eq role 'literal) (push "," out)))
              ('digit
               (cond ((and exp-at (> i exp-at))
                      (unless exp-done (setq exp-done t) (push (concat exp-sign exp-text) out)))
                     ((and point-at (> i point-at)) (push (or (pop frac-slots) "") out))
                     (t (push (or (pop whole-slots) "") out))))
              ('point (push "." out))
              ('exp (push (nth 2 tok) out))
              ('general (push (exc-calc--general-text (abs number)) out))))
        (concat sign whole-text (apply #'concat (nreverse out)))))))

(defun exc-calc--general-text (number)
  "Return NUMBER as the General format writes it, in 11 characters."
  (let ((text (exc-calc-number-text number)))
    (cond
     ((<= (length text) 11) text)
     ;; A very small number takes an exponent rather than lose its digits.
     ((and (/= number 0) (< (abs number) 1e-4))
      (replace-regexp-in-string
       "\\.?0*E" "E" (upcase (format "%.5e" number))))
     (t
      (let ((precision 10) result)
        (while (and (> precision 0)
                    (> (length (setq result (exc-calc-number-text
                                             (string-to-number
                                              (format (format "%%.%dg" precision) number)))))
                       11))
          (setq precision (1- precision)))
        (if (<= (length result) 11) result
          (let ((sci (upcase (format "%.5e" number))))
            (replace-regexp-in-string "\\.?0*E" "E" sci))))))))

(defun exc-calc--section-matches (tokens number)
  "Return non-nil when TOKENS have no condition or NUMBER meets it."
  (let ((condition (cl-find 'cond tokens :key #'car)))
    (or (null condition)
        (let ((bound (nth 2 condition)))
          (pcase (nth 1 condition)
            ("<" (< number bound)) ("<=" (<= number bound))
            (">" (> number bound)) (">=" (>= number bound))
            ("=" (= number bound)) ("<>" (/= number bound)))))))

(defun exc-calc--text-time (text)
  "Return the fraction of a day TEXT names as a time, or nil.
As 13:30, 0:59:99 or 23:59.5: the last of up to three fields may be
up to 9999, and a last field with a fraction counts as seconds, so
23:59.5 is 23 minutes and 59.5 seconds."
  (let ((fields (split-string text ":")))
    (when (and (<= 2 (length fields) 3)
               (cl-every (lambda (field) (string-match-p "\\`[0-9]+\\.?[0-9]*\\'" field))
                         fields)
               (cl-every (lambda (field) (string-match-p "\\`[0-9]+\\'" field))
                         (butlast fields)))
      (let* ((numbers (mapcar #'string-to-number fields))
             (units (cond ((= (length fields) 3) '(3600 60 1))
                          ((string-match-p "\\.[0-9]" (car (last fields))) '(60 1))
                          (t '(3600 60))))
             (limits (if (= (car units) 3600) '(24 60) '(60))))
        (when (and (< (car (last numbers)) 10000)
                   (cl-every (lambda (number limit) (< number limit))
                             (butlast numbers) limits))
          (/ (cl-loop for number in numbers
                      for unit in units sum (* number unit))
             86400.0))))))

(defun exc-calc--text-date-time (text)
  "Return the serial number TEXT names as a date, a time or both, or nil."
  (let ((text (string-trim text)))
    (or (exc-calc--parse-day text)
        (exc-calc--text-time text)
        (exc-calc--parse-time text)
        (and (string-match "\\`\\(.*[0-9a-z]\\)[ T]+\\([0-9]\\{1,2\\}:.*\\)\\'" text)
             (let ((day (match-string 1 text))
                   (time (match-string 2 text)))
               (when-let* ((day (exc-calc--parse-day day))
                           (time (or (exc-calc--text-time time)
                                     (exc-calc--parse-time time))))
                 (+ day time)))))))

(defun exc-calc--number-sections (sections)
  "Return SECTIONS without the one that writes a text value."
  (let ((count (length sections)))
    (cond ((= count 4) (butlast sections))
          ((and (> count 1) (assq 'at (car (last sections)))) (butlast sections))
          (t sections))))

(defun exc-calc-format-code (value code)
  "Return VALUE written after the Excel format CODE, as TEXT does."
  (let* ((sections (mapcar #'exc-calc--code-tokens (exc-calc--code-sections code)))
         ;; The fourth section writes text; without one, so does a section
         ;; that has @ in it.
         (text-section (or (nth 3 sections)
                           (cl-find-if (lambda (tokens) (assq 'at tokens)) sections))))
    (if (stringp value)
        (cond
         ((not text-section) value)
         ;; @ belongs in the text section only.
         ((and (nth 3 sections)
               (cl-some (lambda (tokens) (assq 'at tokens)) (butlast sections 1)))
          (exc-calc-raise "#VALUE!"))
         (t (mapconcat (lambda (tok)
                         (pcase (car tok)
                           ('lit (cadr tok)) ('at value) (_ "")))
                       text-section "")))
      (let* ((number-sections (exc-calc--number-sections sections))
             (number-count (length number-sections))
             (conditional (cl-some (lambda (tokens) (assq 'cond tokens)) number-sections))
             (section-index
              (cond (conditional
                     (or (cl-position-if (lambda (tokens)
                                           (and (assq 'cond tokens)
                                                (exc-calc--section-matches tokens value)))
                                         number-sections)
                         (cl-position-if-not (lambda (tokens) (assq 'cond tokens))
                                             number-sections)))
                    ((and (< value 0) (>= number-count 2)) 1)
                    ((and (= value 0) (>= number-count 3)) 2)
                    (t 0)))
             ;; A value no section takes is written as General writes it.
             (tokens (if section-index (nth section-index number-sections)
                       '((general))))
             ;; Only the first section, and conditional ones, write the
             ;; minus sign; the others are written as a negative's own.
             (negative-shown (or (eql section-index 0) conditional))
             (exp-at (cl-position 'exp tokens :key #'car))
             (elapsed (assq 'elapsed tokens))
             ;; A date or a time of day beside the elapsed hours or seconds,
             ;; which a negative serial cannot have.
             (dated (cl-some (lambda (tok) (eq (car tok) 'date)) tokens)))
        (cond
         ((assq 'at tokens) (exc-calc-raise "#VALUE!"))
         ;; A point after an exponent is not a number code.
         ((and exp-at (cl-position 'point tokens :key #'car :start (1+ exp-at)))
          (exc-calc-raise "#VALUE!"))
         ;; A date code and a digit placeholder do not go together.
         ((and (cl-some (lambda (tok) (memq (car tok) '(date elapsed ampm))) tokens)
               (cl-some (lambda (tok) (and (eq (car tok) 'digit) (not (eq (cadr tok) ?0))))
                        tokens))
          (exc-calc-raise "#VALUE!"))
         ((exc-calc--date-code-p tokens)
          (if (< value 0)
              ;; A minus is written for an elapsed time only, and only one
              ;; standing alone: [s] writes -86400, where [s].000 is
              ;; #VALUE!, as pycel's text workbook has them.
              (unless (and elapsed (null (cdr tokens)))
                (exc-calc-raise "#VALUE!"))
            (unless (or (and elapsed (not dated)) (<= value exc-calc--last-serial*))
              (exc-calc-raise "#VALUE!")))
          (exc-calc--format-date value tokens))
         ((and (assq 'general tokens)
               (not (cl-some (lambda (tok) (eq (car tok) 'digit)) tokens)))
          (concat (if (and negative-shown (< value 0)) "-" "")
                  (mapconcat (lambda (tok)
                               (pcase (car tok)
                                 ('lit (cadr tok))
                                 ('general (exc-calc--general-text (abs value)))
                                 (_ "")))
                             tokens "")))
         ((not (cl-some (lambda (tok) (eq (car tok) 'digit)) tokens))
          (concat (if (and negative-shown (< value 0) (= number-count 1)) "-" "")
                  (mapconcat (lambda (tok) (if (eq (car tok) 'lit) (cadr tok) ""))
                             tokens "")))
         (t (exc-calc--format-number value tokens negative-shown)))))))

(defconst exc-calc--last-serial* 2958465.99999
  "The last moment a date and time format writes.")

(exc-calc-define text ((value any) (code text))
  "VALUE written after the format CODE, as \"0.00\" or \"yyyy-mm-dd\"."
  (let ((value (cond ((eq value :blank) 0)
                     ((eq value t) "TRUE")
                     ((eq value :false) "FALSE")
                     ((stringp value)
                      (or (exc-calc-text-number value)
                          (exc-calc--text-date-time value)
                          value))
                     (t value))))
    (exc-calc--checked-length (exc-calc-format-code value code))))

(exc-calc-define dollar ((number number) &optional (decimals integer 2))
  "NUMBER as an amount of dollars, to DECIMALS places."
  (let* ((rounded (exc-calc--round-to number decimals 'nearest))
         (places (if (> decimals 0) (concat "." (make-string decimals ?0)) "")))
    (exc-calc-format-code rounded (format "$#,##0%s;($#,##0%s)" places places))))

(exc-calc-define fixed ((number number) &optional (decimals integer 2) (no-commas bool))
  "NUMBER rounded to DECIMALS places, with commas unless NO-COMMAS.
DECIMALS past 127 are #VALUE!, as in Excel."
  (when (> decimals 127) (exc-calc-raise "#VALUE!"))
  (let* ((rounded (if (< decimals -308) 0
                    (exc-calc--round-to number decimals 'nearest)))
         (places (if (> decimals 0) (concat "." (make-string decimals ?0)) "")))
    (exc-calc-format-code rounded (concat (if no-commas "0" "#,##0") places))))

;;;----------------------------------------------------------------------
;;; Values as text, and Thai amounts
;;;----------------------------------------------------------------------

(defun exc-calc--optional-ast (ast)
  "Return AST, or nil when the argument was left out or left empty."
  (and ast (not (equal ast '(MISSING))) ast))

(defun exc-calc--argument-value (ast)
  "Return the value AST gives, a range read whole, errors left as values."
  (let ((value (exc-calc-arg ast 'values)))
    (if (exc-calc-ref-p value) (exc-calc--deref value) value)))

(defun exc-calc--as-array (value)
  "Return VALUE as an array, one value being an array of one."
  (if (exc-calc-array-p value) value
    (exc-calc-array--make 1 1 (vector value))))

(defun exc-calc--text-format (ast)
  "Return 0 or 1, the format ARRAYTOTEXT and VALUETOTEXT take from AST."
  (let ((value (if (exc-calc--optional-ast ast)
                   (exc-calc--first-of (exc-calc--argument-value ast))
                 0)))
    (let ((number (truncate (exc-calc-num value))))
      (if (memq number '(0 1)) number (exc-calc-raise "#VALUE!")))))

(defun exc-calc--text-value (value strict)
  "Return VALUE as ARRAYTOTEXT writes it; STRICT quotes text."
  (cond
   ((eq value :blank) "")
   ((exc-calc-err-p value) (exc-calc-err-code value))
   ((eq value t) "TRUE")
   ((eq value :false) "FALSE")
   ((numberp value) (exc-calc-number-text value))
   ((stringp value)
    (if strict
        (concat "\"" (string-replace "\"" "\"\"" value) "\"")
      value))
   (t (exc-calc--text value))))

(exc-calc-define arraytotext ((value lazy) &optional (format lazy))
  "The values of VALUE as text, joined or, with FORMAT 1, as an array constant."
  (let ((array (exc-calc--as-array (exc-calc--argument-value value)))
        (strict (eql (exc-calc--text-format format) 1)))
    (let ((rows (exc-calc-array-rows array))
          (cols (exc-calc-array-cols array))
          (data (exc-calc-array-data array)))
      (when (and strict (= rows 1) (= cols 1) (eq (aref data 0) :blank))
        (exc-calc-raise "#VALUE!"))
      (if strict
          (concat "{"
                  (mapconcat
                   (lambda (row)
                     (mapconcat
                      (lambda (col)
                        (exc-calc--text-value
                         (aref data (+ (* row cols) col)) t))
                      (number-sequence 0 (1- cols)) ","))
                   (number-sequence 0 (1- rows)) ";")
                  "}")
        (mapconcat (lambda (item) (exc-calc--text-value item nil))
                   (append data nil) ", ")))))

(exc-calc-define valuetotext ((value lazy) &optional (format lazy))
  "VALUE as text: numbers generally, text as it is, quoted with FORMAT 1."
  (let ((value (exc-calc--first-of (exc-calc--argument-value value)))
        (strict (eql (exc-calc--text-format format) 1)))
    (exc-calc--text-value value strict)))

(defconst exc-calc--thai-digits
  ["ศูนย์" "หนึ่ง" "สอง" "สาม" "สี่" "ห้า" "หก" "เจ็ด" "แปด" "เก้า"]
  "The Thai words for the digits 0 to 9.")

(defun exc-calc--thai-number (number)
  "Return NUMBER, a whole number from zero, in Thai words."
  (if (zerop number)
      (aref exc-calc--thai-digits 0)
    (let ((text "") (left number))
      (when (>= left 1000000)
        (setq text (concat text
                           (exc-calc--thai-number (/ left 1000000))
                           "ล้าน")
              left (% left 1000000)))
      (dolist (place '((100000 . "แสน") (10000 . "หมื่น")
                       (1000 . "พัน") (100 . "ร้อย")))
        (let ((digit (/ left (car place))))
          (when (> digit 0)
            (setq text (concat text (aref exc-calc--thai-digits digit)
                               (cdr place))))
          (setq left (% left (car place)))))
      (let ((tens (/ left 10)) (unit (% left 10)))
        (cond ((= tens 1) (setq text (concat text "สิบ")))
              ((= tens 2) (setq text (concat text "ยี่สิบ")))
              ((> tens 2)
               (setq text (concat text (aref exc-calc--thai-digits tens)
                                  "สิบ"))))
        (cond ((= unit 1)
               (setq text (concat text (if (> tens 0) "เอ็ด" "หนึ่ง"))))
              ((> unit 1)
               (setq text (concat text (aref exc-calc--thai-digits unit))))))
      text)))

(exc-calc-define bahttext ((value lazy))
  "VALUE as an amount of baht, in Thai words."
  (let* ((value (exc-calc--first-of (exc-calc--argument-value value)))
         (number (exc-calc-num value))
         (negative (< number 0))
         (satang (round (* (abs number) 100)))
         (baht (/ satang 100))
         (rest (% satang 100)))
    (concat (if negative "ลบ" "")
            (exc-calc--thai-number baht)
            "บาท"
            (if (zerop rest)
                "ถ้วน"
              (concat (exc-calc--thai-number rest) "สตางค์")))))

(provide 'exc-calc-text)

;;; exc-calc-text.el ends here
