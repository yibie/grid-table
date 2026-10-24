;;; exc-parser.el --- AST parser for grid formulas -*- lexical-binding: t -*-

(require 'eieio)
(require 'cl-lib)
(require 'exc-obsolete)

;; The formula evaluator is in exc-calc.el, which loads this file:
;; requiring it here would be a circular load.
(declare-function exc-calc-eval-ast "exc-calc")

;;;----------------------------------------------------------------------
;;; Token Definitions
;;;----------------------------------------------------------------------

;; Token types
(defconst exc-parser-token-type-number 'NUMBER)
(defconst exc-parser-token-type-string 'STRING)
(defconst exc-parser-token-type-cell-ref 'CELL-REF)
(defconst exc-parser-token-type-function 'FUNCTION)
(defconst exc-parser-token-type-operator 'OPERATOR)
(defconst exc-parser-token-type-identifier 'IDENTIFIER)
(defconst exc-parser-token-type-lparen 'LPAREN)
(defconst exc-parser-token-type-rparen 'RPAREN)
(defconst exc-parser-token-type-comma 'COMMA)
(defconst exc-parser-token-type-boolean 'BOOLEAN)
(defconst exc-parser-token-type-eof 'EOF)
(defconst exc-parser-token-type-error 'ERROR)

(defconst exc-parser-error-literals
  '("#DIV/0!" "#NAME?" "#N/A" "#REF!" "#VALUE!" "#NUM!" "#NULL!" "#CALC!"
    "#SPILL!" "#GETTING_DATA")
  "Error values a formula may hold as written text, as `#REF!' left by a
deleted cell.")

(defconst exc-parser-sheet-prefix-regexp
  (concat "\\(?:'\\(\\(?:[^']\\|''\\)+\\)'"
          "\\(?::'\\(\\(?:[^']\\|''\\)+\\)'\\)?"
          "\\|\\(\\(?:\\[[0-9]+\\]\\)?[[:alpha:]_][[:alnum:]_.]*"
          "\\(?::\\([[:alpha:]_][[:alnum:]_.]*\\)\\)?\\)\\)!")
  "Matches the sheet a reference names, up to its exclamation mark.
Group 1 is a name written in apostrophes, where a doubled apostrophe
stands for one; group 3 is a name written bare.  Either may be two
names joined by a colon, a reference through the sheets from the one to
the other (Sheet1:Sheet3!A1); group 4 is then the second bare name.
A bare name may follow the number of another workbook, as [1]Sheet4!A1.
Two names each in apostrophes, as LibreOffice writes \\='a b\\=':\\='c d\\='!A1,
put the second in group 2.")

(defconst exc-parser--cells-regexp
  (concat "\\(?:\\$?[A-Za-z]+\\$?[0-9]+\\(?::\\$?[A-Za-z]+\\$?[0-9]+\\)?"
          "\\|\\$?[A-Za-z]+:\\$?[A-Za-z]+"
          "\\|\\$?[0-9]+:\\$?[0-9]+\\)")
  "Matches a cell, a range of cells, whole columns (A:C) or whole rows (2:5).")

(defconst exc-parser--whole-rows-regexp "\\$?[0-9]+:\\$?[0-9]+"
  "Matches a range of whole rows, such as 2:5.")

(defconst exc-parser--external-prefix-regexp "\\[\\([0-9]+\\)\\]!"
  "Matches the number of another workbook, up to its exclamation mark.
[1]!Name is a name of that workbook, one of no single sheet.")

(defun exc-parser-external-sheet (sheet)
  "Return (LINK . SHEET) when SHEET names another workbook, or nil.
[1]Sheet1 is the sheet Sheet1 of the first workbook a file links to, and
[1] with no sheet is a name of that workbook that no single sheet holds."
  (when (and (stringp sheet)
             (string-match "\\`\\[\\([0-9]+\\)\\]\\(.*\\)\\'" sheet))
    (cons (string-to-number (match-string 1 sheet)) (match-string 2 sheet))))

(defun exc-parser-sheet-prefix-at (text pos)
  "Return (END . NAME) for the sheet prefix that starts at POS in TEXT.
END is the position after the exclamation mark.  Return nil when no
prefix starts there.  [1]!Name is such a prefix: a name of another
workbook, one that no single sheet of it holds, names it [1]."
  (let* ((char (aref text pos))
         (external (and (eq char ?\[)
                        (string-match exc-parser--external-prefix-regexp text pos)
                        (= (match-beginning 0) pos))))
    (if external
        (cons (match-end 0) (concat "[" (match-string 1 text) "]"))
      ;; Looking for a match that starts further on would be wasted work.
      (when (and (or (eq char ?') (eq char ?_) (eq char ?\[)
                     (eq (get-char-code-property char 'general-category) 'Lo)
                     (memq (get-char-code-property char 'general-category)
                           '(Lu Ll Lt Lm)))
                 (eq (string-match exc-parser-sheet-prefix-regexp text pos) pos)
                 ;; A1:Sheet2!B3 is a range from A1, as no sheet named
                 ;; like a cell goes without apostrophes.
                 (not (and (match-beginning 4)
                           (save-match-data
                             (let ((case-fold-search nil))
                               (exc-parser--cell-text-p
                                (upcase (substring text pos
                                                   (1- (match-beginning 4))))))))))
        (cons (match-end 0)
              (cond ((match-beginning 2)
                     (concat (string-replace "''" "'" (match-string 1 text)) ":"
                             (string-replace "''" "'" (match-string 2 text))))
                    ((match-beginning 1)
                     (string-replace "''" "'" (match-string 1 text)))
                    (t (match-string 3 text))))))))

;; Helper to create a token
(defun exc-parser-make-token (type value)
  "Create a token with TYPE and VALUE."
  (list type value))

;; Helper to check token type
(defun exc-parser-token-is-p (token type &optional value)
  "Checks if TOKEN matches TYPE and optionally VALUE."
  (and (eq (car token) type)
       (or (null value) (equal (cadr token) value))))

;; Helper to get token value
(defun exc-parser-token-value (token)
  "Get the value from a token."
  (cadr token))

;; Helper to get token type
(defun exc-parser-token-type (token)
  "Get the type from a token."
  (car token))

;; Helper to get operator value as string
(defun exc-parser-get-operator (token)
  "Gets the operator value from an OPERATOR token as string."
  (when (eq (car token) exc-parser-token-type-operator)
    (cadr token)))

;;;----------------------------------------------------------------------
;;; Lexical Analyzer (Tokenizer)
;;;----------------------------------------------------------------------

(defun exc-parser--name-start-p (char)
  "Return non-nil when CHAR may start a name, a function or a reference.
A quotation mark may, as Excel takes the curly ones for names."
  (or (and (>= char ?A) (<= char ?Z))
      (and (>= char ?a) (<= char ?z))
      (memq char '(?$ ?_ ?\\))
      (and (> char 127)
           (memq (get-char-code-property char 'general-category)
                 '(Lu Ll Lt Lm Lo Pi Pf)))))

(defun exc-parser--name-char-p (char)
  "Return non-nil when CHAR may stand inside a name."
  (or (exc-parser--name-start-p char)
      (and (>= char ?0) (<= char ?9))
      (memq char '(?. ??))
      (and (> char 127)
           (memq (get-char-code-property char 'general-category)
                 '(Nd Mn Mc Pi Pf)))))

(defconst exc-parser--cell-regexp "\\`\\$?\\([A-Z]\\{1,3\\}\\)\\$?\\([0-9]\\{1,7\\}\\)\\'"
  "Matches the text of one cell, such as $B$2, in capitals.")

(defun exc-parser--cell-text-p (text)
  "Return non-nil when TEXT, in capitals, is a cell on a sheet's grid.
LOG10 is a cell, as XFD1048576 is; TAX2020 and A0 are not."
  (and (string-match exc-parser--cell-regexp text)
       (let ((column (match-string 1 text))
             (row (string-to-number (match-string 2 text))))
         (and (<= 1 row 1048576)
              (or (< (length column) 3) (not (string< "XFD" column)))))))

(defun exc-parser--column-text-p (text)
  "Return non-nil when TEXT, in capitals, is a whole column such as $C."
  (and (string-match "\\`\\$?\\([A-Z]\\{1,3\\}\\)\\'" text)
       (let ((column (match-string 1 text)))
         (or (< (length column) 3) (not (string< "XFD" column))))))

(defconst exc-parser--function-prefixes
  '("_XLFN._XLWS." "_XLFN." "_XLWS." "_XLPM.")
  "Prefixes a workbook file writes before newer functions and LET names.")

(defun exc-parser--strip-prefix (name)
  "Return NAME, in capitals, without a workbook file's function prefix."
  (let ((prefix (cl-find-if (lambda (prefix) (string-prefix-p prefix name))
                            exc-parser--function-prefixes)))
    (if prefix (substring name (length prefix)) name)))

(defun exc-parser--bracket-end (text pos)
  "Return the position after the bracket that closes the one at POS in TEXT.
Brackets nest; an apostrophe makes the character after it plain."
  (let ((depth 0) (len (length text)) end)
    (while (and (not end) (< pos len))
      (pcase (aref text pos)
        (?' (setq pos (1+ pos)))
        (?\[ (setq depth (1+ depth)))
        (?\] (setq depth (1- depth))
             (when (= depth 0) (setq end (1+ pos)))))
      (setq pos (1+ pos)))
    (or end (error "Unclosed ["))))

(defun exc-parser--quoted-at (text pos)
  "Return (END . NAME) for the name in apostrophes that starts at POS in TEXT.
END is the position after the closing apostrophe, and NAME what it holds,
a doubled apostrophe standing for one."
  (let ((end (1+ pos)) (len (length text)) found)
    (while (and (not found) (< end len))
      (if (eq (aref text end) ?')
          (if (and (< (1+ end) len) (eq (aref text (1+ end)) ?'))
              (setq end (+ end 2))
            (setq found end))
        (setq end (1+ end))))
    (when found
      (cons (1+ found)
            (string-replace "''" "'" (substring text (1+ pos) found))))))

(defun exc-parser--word-at (text pos)
  "Return (END . WORD) for the name that starts at POS in TEXT."
  (let ((end pos) (len (length text)))
    (while (and (< end len) (exc-parser--name-char-p (aref text end)))
      (setq end (1+ end)))
    (cons end (substring text pos end))))

(defun exc-parser--reference-end (text pos)
  "Return (END . TEXT) for a reference that starts at POS in TEXT, or nil.
That is a cell such as $A$1, a range such as A1:B5, whole columns such
as A:C or whole rows such as 2:5, in capitals.  A name followed by an
opening parenthesis is a function, not a reference."
  (let ((case-fold-search t)
        (len (length text)))
    (cond
     ((eq (string-match exc-parser--whole-rows-regexp text pos) pos)
      (cons (match-end 0) (match-string 0 text)))
     ((eq (string-match "\\$?[A-Za-z]+\\$?[0-9]*" text pos) pos)
      (let* ((first-end (match-end 0))
             (first (upcase (match-string 0 text))))
        (unless (or (and (< first-end len)
                         (or (exc-parser--name-char-p (aref text first-end))
                             (memq (aref text first-end) '(?\( ?\[))))
                    (not (or (exc-parser--cell-text-p first)
                             (exc-parser--column-text-p first))))
          (let ((range (and (< first-end len) (eq (aref text first-end) ?:)
                            (eq (string-match "\\$?[A-Za-z]+\\$?[0-9]*"
                                              text (1+ first-end))
                                (1+ first-end))
                            (let ((second-end (match-end 0))
                                  (second (upcase (match-string 0 text))))
                              (and (not (and (< second-end len)
                                             (or (exc-parser--name-char-p
                                                  (aref text second-end))
                                                 (eq (aref text second-end) ?\())))
                                   (if (exc-parser--cell-text-p first)
                                       (exc-parser--cell-text-p second)
                                     (exc-parser--column-text-p second))
                                   (cons second-end
                                         (concat first ":" second)))))))
            (cond (range range)
                  ;; A column alone is a name, as a function's is
                  ((exc-parser--cell-text-p first) (cons first-end first))))))))))

(defvar exc-parser--starts nil
  "Nil, or a table where parsing keeps where each token and node starts.
Keys are tokens and the AST nodes made of them, compared with `eq';
values are positions in the formula text.  See `exc-parser-parse-starts'.")

(defun exc-parser-tokenize (formula-str)
  "Return the tokens of FORMULA-STR, a formula without its equals sign.
Whitespace between two references, where it intersects them, is a SPACE
token; elsewhere it separates tokens and is dropped."
  (let ((tokens '())
        (pos 0)
        (len (length formula-str)))
    (while (< pos len)
      (let ((char (aref formula-str pos)) (before tokens) (start pos))
        (cond
         ((memq char '(?\s ?\t ?\n ?\r))
          (let ((start pos))
            (while (and (< pos len) (memq (aref formula-str pos) '(?\s ?\t ?\n ?\r)))
              (setq pos (1+ pos)))
            ;; A1:B5 C3:D4 is where the two ranges meet, as is A1# C3:D4
            (when (and tokens (< pos len)
                       (or (memq (caar tokens) '(CELL-REF RPAREN STRUCT NAME-REF))
                           (equal (car tokens) '(OPERATOR "#")))
                       (or (exc-parser--name-start-p (aref formula-str pos))
                           (memq (aref formula-str pos) '(?' ?\())))
              (push (list 'SPACE (substring formula-str start pos)) tokens))))

         ;; A reference into another sheet (e.g., Sheet2!A1, 'My Sheet'!A1:B5)
         ((when-let* ((prefix (exc-parser-sheet-prefix-at formula-str pos)))
            (let ((reference (exc-parser--reference-end formula-str (car prefix))))
              (cond
               (reference
                (push (list exc-parser-token-type-cell-ref (cdr reference)
                            (cdr prefix))
                      tokens)
                (setq pos (car reference)))
               ;; A deleted reference keeps its sheet: Sheet2!#REF!
               ((exc-parser--error-literal-at formula-str (car prefix))
                (let ((text (exc-parser--error-literal-at
                             formula-str (car prefix))))
                  (push (exc-parser-make-token exc-parser-token-type-error text)
                        tokens)
                  (setq pos (+ (car prefix) (length text)))))
               ;; A name the sheet keeps: Sheet2!Total
               ((and (< (car prefix) len)
                     (exc-parser--name-start-p (aref formula-str (car prefix))))
                (let ((word (exc-parser--word-at formula-str (car prefix))))
                  (if (and (exc-parser-external-sheet (cdr prefix))
                           (< (car word) len)
                           (eq (aref formula-str (car word)) ?\[))
                      ;; A table of another workbook is #REF!: only the
                      ;; workbook that holds a table reads one.
                      (progn
                        (push (exc-parser-make-token exc-parser-token-type-error
                                                      "#REF!")
                              tokens)
                        (setq pos (exc-parser--bracket-end formula-str (car word))))
                    (push (list 'NAME-REF (upcase (cdr word)) (cdr prefix)) tokens)
                    (setq pos (car word)))))
               ;; A name of another workbook in apostrophes: [1]!'ES #F'
               ((and (exc-parser-external-sheet (cdr prefix))
                     (eq (aref formula-str (car prefix)) ?'))
                (let ((quoted (exc-parser--quoted-at formula-str (car prefix))))
                  (push (list 'NAME-REF (upcase (cdr quoted)) (cdr prefix)) tokens)
                  (setq pos (car quoted))))))))

         ;; !A1 is A1 on the sheet the formula is worked out on, as a
         ;; name may say: CELL("filename",!A1)
         ((and (eq char ?!) (< (1+ pos) len)
               (when-let* ((reference (exc-parser--reference-end
                                       formula-str (1+ pos))))
                 (push (exc-parser-make-token exc-parser-token-type-cell-ref
                                               (cdr reference))
                       tokens)
                 (setq pos (car reference)))))

         ;; A reference: A1, $A$1, A1:B5, A:C or 2:5
         ((and (or (exc-parser--name-start-p char) (and (>= char ?0) (<= char ?9)))
               (when-let* ((reference (exc-parser--reference-end formula-str pos)))
                 (push (exc-parser-make-token exc-parser-token-type-cell-ref
                                               (cdr reference))
                       tokens)
                 (setq pos (car reference)))))

         ;; Numbers: 12, 1.5, .5, 1E+3
         ((or (and (>= char ?0) (<= char ?9))
              (and (eq char ?.) (< (1+ pos) len)
                   (<= ?0 (aref formula-str (1+ pos)) ?9)))
          (string-match "[0-9]*\\.?[0-9]*\\(?:[eE][-+]?[0-9]+\\)?" formula-str pos)
          (push (exc-parser-make-token exc-parser-token-type-number
                                        (string-to-number (match-string 0 formula-str)))
                tokens)
          (setq pos (match-end 0)))

         ;; Strings, where "" stands for one quotation mark
         ((eq char ?\")
          (let ((start (1+ pos)) (parts '()) done)
            (setq pos start)
            (while (not done)
              (cond
               ((>= pos len) (error "Unclosed string"))
               ((and (eq (aref formula-str pos) ?\")
                     (< (1+ pos) len) (eq (aref formula-str (1+ pos)) ?\"))
                (push (substring formula-str start (1+ pos)) parts)
                (setq pos (+ pos 2) start pos))
               ((eq (aref formula-str pos) ?\")
                (push (substring formula-str start pos) parts)
                (setq pos (1+ pos) done t))
               (t (setq pos (1+ pos)))))
            (push (exc-parser-make-token exc-parser-token-type-string
                                          (apply #'concat (nreverse parts)))
                  tokens)))

         ;; Error values written in the formula (e.g., #REF!)
         ((and (eq char ?#) (exc-parser--error-literal-at formula-str pos))
          (let ((text (exc-parser--error-literal-at formula-str pos)))
            (push (exc-parser-make-token exc-parser-token-type-error text) tokens)
            (setq pos (+ pos (length text)))))

         ;; A1# is the cells the formula in A1 spills over
         ((and (eq char ?#) (memq (caar tokens) '(CELL-REF NAME-REF)))
          (push (exc-parser-make-token exc-parser-token-type-operator "#") tokens)
          (setq pos (1+ pos)))

         ;; A table's columns: Table1[Price], [@Price], Table1[[#Totals],[Price]]
         ((eq char ?\[)
          (let ((end (exc-parser--bracket-end formula-str pos)))
            (push (list 'STRUCT nil (substring formula-str pos end)) tokens)
            (setq pos end)))

         ((memq char '(?\( ?\) ?, ?\; ?{ ?}))
          (push (exc-parser-make-token
                 (pcase char (?\( 'LPAREN) (?\) 'RPAREN) (?, 'COMMA)
                        (?\; 'SEMICOLON) (?{ 'LBRACE) (?} 'RBRACE))
                 char)
                tokens)
          (setq pos (1+ pos)))

         ;; Function names, defined names, TRUE and FALSE
         ((exc-parser--name-start-p char)
          (pcase-let* ((`(,end . ,word) (exc-parser--word-at formula-str pos))
                       (name (exc-parser--strip-prefix (upcase word))))
            (setq pos end)
            (cond
             ((and (< pos len) (eq (aref formula-str pos) ?\[))
              (let ((end (exc-parser--bracket-end formula-str pos)))
                (push (list 'STRUCT word (substring formula-str pos end)) tokens)
                (setq pos end)))
             ((and (member name '("TRUE" "FALSE"))
                   (not (and (< pos len) (eq (aref formula-str pos) ?\())))
              (push (exc-parser-make-token exc-parser-token-type-boolean
                                            (equal name "TRUE"))
                    tokens))
             (t (push (exc-parser-make-token exc-parser-token-type-function name)
                      tokens)))))

         ;; Two-character comparison operators
         ((and (< (1+ pos) len)
               (member (substring formula-str pos (+ pos 2)) '("<=" ">=" "<>")))
          (push (exc-parser-make-token exc-parser-token-type-operator
                                        (substring formula-str pos (+ pos 2)))
                tokens)
          (setq pos (+ pos 2)))

         ;; Single-character operators
         (t
          (push (exc-parser-make-token exc-parser-token-type-operator
                                        (char-to-string char))
                tokens)
          (setq pos (1+ pos))))
        (when (and exc-parser--starts (not (eq tokens before)))
          (puthash (car tokens) start exc-parser--starts))))
    ;; Whitespace kept before something that turned out no reference
    (setq tokens (nreverse tokens))
    (let ((kept '()))
      (while tokens
        (let ((token (pop tokens)))
          ;; A function's reference may meet one too: A1:C3 INDEX(...).
          (unless (and (eq (car token) 'SPACE)
                       (not (memq (caar tokens)
                                  '(CELL-REF LPAREN STRUCT NAME-REF FUNCTION))))
            (push token kept))))
      (nreverse (cons (exc-parser-make-token exc-parser-token-type-eof nil)
                      kept)))))

(defun exc-parser--error-literal-at (formula pos)
  "Return the error value written in FORMULA at POS, or nil."
  (and (< pos (length formula))
       (eq (aref formula pos) ?#)
       (cl-loop with length = (length formula)
                for text in exc-parser-error-literals
                for end = (+ pos (length text))
                when (and (<= end length)
                          (eq t (compare-strings formula pos end
                                                 text nil nil t)))
                return text)))

;;;----------------------------------------------------------------------
;;; AST Node Definitions
;;;----------------------------------------------------------------------

;; AST node types
(defconst exc-ast-type-number 'NUMBER)
(defconst exc-ast-type-string 'STRING)
(defconst exc-ast-type-cell-ref 'CELL-REF)
(defconst exc-ast-type-binary-op 'BINARY-OP)
(defconst exc-ast-type-function-call 'FUNCTION-CALL)
(defconst exc-ast-type-unary-op 'UNARY-OP)

;; Helper to create AST nodes
(defun exc-ast-make-number (value)
  "Create a number AST node."
  (list exc-ast-type-number value))

(defun exc-ast-make-string (value)
  "Create a string AST node."
  (list exc-ast-type-string value))

(defun exc-ast-make-cell-ref (ref &optional sheet)
  "Create a cell reference AST node for REF, in SHEET when it names one."
  (if sheet
      (list exc-ast-type-cell-ref ref sheet)
    (list exc-ast-type-cell-ref ref)))

(defun exc-ast-make-binary-op (operator left right)
  "Create a binary operation AST node."
  (list exc-ast-type-binary-op operator left right))

(defun exc-ast-make-unary-op (operator operand)
  "Create a unary operation AST node."
  (list exc-ast-type-unary-op operator operand))

(defun exc-ast-make-function-call (function-name arguments)
  "Create a function call AST node."
  (list exc-ast-type-function-call function-name arguments))

;; Helper to check AST node type
(defun exc-ast-node-type (node)
  "Get the type of an AST node."
  (car node))

(defun exc-ast-node-value (node)
  "Get the value of an AST node."
  (cadr node))

(defun exc-ast-node-left (node)
  "Get the left operand of a binary operation node."
  (caddr node))

(defun exc-ast-node-right (node)
  "Get the right operand of a binary operation node."
  (cadddr node))

(defun exc-ast-node-operand (node)
  "Get the operand of a unary operation node."
  (caddr node))

(defun exc-ast-node-function-name (node)
  "Get the function name of a function call node."
  (cadr node))

(defun exc-ast-node-arguments (node)
  "Get the arguments of a function call node."
  (caddr node))

;;;----------------------------------------------------------------------
;;; Parser Implementation
;;;----------------------------------------------------------------------

(defclass exc-parser-state ()
  ((tokens :initarg :tokens :accessor exc-parser-state-tokens)
   (current :initform 0 :accessor exc-parser-state-current)))

(defun exc-parser-create (tokens)
  "Create a new parser instance."
  (make-instance 'exc-parser-state :tokens tokens))

(defun exc-parser-current-token (parser)
  "Get the current token."
  (nth (exc-parser-state-current parser) (exc-parser-state-tokens parser)))

(defun exc-parser-peek-token (parser)
  "Peek at the next token without consuming it."
  (nth (1+ (exc-parser-state-current parser)) (exc-parser-state-tokens parser)))

(defun exc-parser-advance (parser)
  "Advance to the next token."
  (setf (exc-parser-state-current parser) (1+ (exc-parser-state-current parser))))

(defun exc-parser-match (parser expected-type)
  "Match and consume a token of the expected type."
  (let ((token (exc-parser-current-token parser)))
    (if (exc-parser-token-is-p token expected-type)
        (progn
          (exc-parser-advance parser)
          token)
      (error "Expected %s, got %s" expected-type (exc-parser-token-type token)))))

(defun exc-parser-check (parser type)
  "Check if the current token is of the given type."
  (exc-parser-token-is-p (exc-parser-current-token parser) type))

(defun exc-parser-check-any (parser &rest types)
  "Check if the current token is any of the given types."
  (let ((token (exc-parser-current-token parser)))
    (cl-some (lambda (type) (exc-parser-token-is-p token type)) types)))

;;;----------------------------------------------------------------------
;;; Grammar Rules (Recursive Descent Parser)
;;;----------------------------------------------------------------------

(defun exc-parser-parse-expression (tokens)
  "Parses a list of tokens into an AST. Returns (ast . remaining-tokens)."
  (exc-parser-parse-or-expression tokens))

(defun exc-parser-parse-or-expression (tokens)
  "Parses OR expressions (lowest precedence). Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-and-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens (exc-parser-token-is-p (car remaining-tokens) exc-parser-token-type-operator "OR"))
      (pop remaining-tokens) ; Consume OR token
      (let* ((right-result (exc-parser-parse-and-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (exc-ast-make-binary-op 'OR left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun exc-parser-parse-and-expression (tokens)
  "Parses AND expressions. Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-equality-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens (exc-parser-token-is-p (car remaining-tokens) exc-parser-token-type-operator "AND"))
      (pop remaining-tokens) ; Consume AND token
      (let* ((right-result (exc-parser-parse-equality-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (exc-ast-make-binary-op 'AND left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun exc-parser-parse-equality-expression (tokens)
  "Parses equality expressions (=, <>, <, >, <=, >=).
Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-comparison-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (let ((op (exc-parser-get-operator (car remaining-tokens))))
                  (member op '("=" "<>" "<" ">" "<=" ">="))))
      (let* ((op (exc-parser-get-operator (pop remaining-tokens)))
             (right-result (exc-parser-parse-comparison-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (exc-ast-make-binary-op (intern op) left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun exc-parser-parse-comparison-expression (tokens)
  "Parses string concatenation (&), which binds tighter than comparison.
Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-additive-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (equal (exc-parser-get-operator (car remaining-tokens)) "&"))
      (pop remaining-tokens)
      (let* ((right-result (exc-parser-parse-additive-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (exc-ast-make-binary-op '& left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun exc-parser-parse-additive-expression (tokens)
  "Parses additive expressions (+, -). Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-multiplicative-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (let ((op (exc-parser-get-operator (car remaining-tokens))))
                  (member op '("+" "-"))))
      (let* ((op (exc-parser-get-operator (pop remaining-tokens)))
             (right-result (exc-parser-parse-multiplicative-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (exc-ast-make-binary-op (intern op) left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun exc-parser-parse-multiplicative-expression (tokens)
  "Parses multiplicative expressions (*, /). Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-power-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (let ((op (exc-parser-get-operator (car remaining-tokens))))
                  (member op '("*" "/"))))
      (let* ((op (exc-parser-get-operator (pop remaining-tokens)))
             (right-result (exc-parser-parse-power-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (exc-ast-make-binary-op (intern op) left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun exc-parser-parse-percent-expression (tokens)
  "Parses a value followed by percent signs, each dividing it by 100.
Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-unary-expression tokens))
         (operand (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (exc-parser-token-is-p (car remaining-tokens)
                                        exc-parser-token-type-operator "%"))
      (pop remaining-tokens)
      (setq operand (exc-ast-make-unary-op '% operand)))
    (cons operand remaining-tokens)))

(defun exc-parser-parse-power-expression (tokens)
  "Parses power expressions (^). Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-percent-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens (exc-parser-token-is-p (car remaining-tokens) exc-parser-token-type-operator "^"))
      (pop remaining-tokens) ; Consume ^ token
      (let* ((right-result (exc-parser-parse-percent-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (exc-ast-make-binary-op '^ left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun exc-parser-parse-unary-expression (tokens)
  "Parses unary expressions (+, -, NOT). Returns (ast . remaining-tokens)."
  (cond
   ((and tokens (exc-parser-token-is-p (car tokens) exc-parser-token-type-operator "+"))
    (pop tokens)
    (let* ((result (exc-parser-parse-unary-expression tokens))
           (operand (car result))
           (remaining (cdr result)))
      (cons (exc-ast-make-unary-op '+ operand) remaining)))
   ((and tokens (exc-parser-token-is-p (car tokens) exc-parser-token-type-operator "-"))
    (pop tokens)
    (let* ((result (exc-parser-parse-unary-expression tokens))
           (operand (car result))
           (remaining (cdr result)))
      (cons (exc-ast-make-unary-op '- operand) remaining)))
   ((and tokens (exc-parser-token-is-p (car tokens) exc-parser-token-type-operator "NOT"))
    (pop tokens)
    (let* ((result (exc-parser-parse-unary-expression tokens))
           (operand (car result))
           (remaining (cdr result)))
      (cons (exc-ast-make-unary-op 'NOT operand) remaining)))
   ;; @A1:A9, the cell of the range in the formula's own row or column
   ((and tokens (exc-parser-token-is-p (car tokens) exc-parser-token-type-operator "@"))
    (pop tokens)
    (let ((result (exc-parser-parse-unary-expression tokens)))
      (cons (exc-ast-make-unary-op '@ (car result)) (cdr result))))
   (t
    (exc-parser-parse-intersection-expression tokens))))

(defun exc-parser-parse-intersection-expression (tokens)
  "Parse references joined by spaces, the cells they have in common.
Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser-parse-range-expression tokens))
         (left (car result))
         (remaining (cdr result)))
    (while (eq (caar remaining) 'SPACE)
      (pop remaining)
      (let ((right (exc-parser-parse-range-expression remaining)))
        (setq left (list 'INTERSECT left (car right))
              remaining (cdr right))))
    (cons left remaining)))

(defun exc-parser-parse-range-expression (tokens)
  "Parse references joined by colons, as A1:INDEX(B1:B9,3).
Returns (ast . remaining-tokens)."
  (let* ((result (exc-parser--parse-spilled tokens))
         (left (car result))
         (remaining (cdr result)))
    (while (exc-parser-token-is-p (car remaining)
                                   exc-parser-token-type-operator ":")
      (pop remaining)
      (let ((right (exc-parser--parse-spilled remaining)))
        (setq left (exc-parser--starting (list 'RANGE-OP left (car right)) left)
              remaining (cdr right))))
    (cons left remaining)))

(defun exc-parser--parse-spilled (tokens)
  "Parse a primary expression, and a # after it as ANCHORARRAY of it.
Workbooks store A1# so.  Returns (ast . remaining-tokens)."
  (let ((result (exc-parser-parse-primary-expression tokens)))
    (if (exc-parser-token-is-p (cadr result) exc-parser-token-type-operator "#")
        (cons (exc-parser--starting
               (list 'FUNCTION-CALL "ANCHORARRAY" (list (car result)))
               (car result))
              (cddr result))
      result)))

(defun exc-parser--parse-arguments (tokens close)
  "Parse expressions separated by commas up to the token type CLOSE.
An argument left empty, as in IF(A1,,2), is (MISSING).  Returns
\(ARGUMENTS . TOKENS-AFTER-CLOSE)."
  (let ((args '()) done)
    (if (eq (caar tokens) close)
        (setq done t)
      (while (not done)
        (if (memq (caar tokens) (list 'COMMA close))
            (push (list 'MISSING) args)
          (let ((result (exc-parser-parse-expression tokens)))
            (push (car result) args)
            (setq tokens (cdr result))))
        (if (eq (caar tokens) 'COMMA)
            (pop tokens)
          (setq done t))))
    (unless (eq (caar tokens) close)
      (error "Expected %s" (if (eq close 'RPAREN) "')'" close)))
    (cons (nreverse args) (cdr tokens))))

(defun exc-parser--parse-array (tokens)
  "Parse an array constant after its opening brace, as {1,2;3,4}.
Returns ((ARRAY ROWS) . remaining-tokens), each row a list of nodes."
  (let ((rows '()) (row '()) done)
    (while (not done)
      (let ((result (exc-parser-parse-unary-expression tokens)))
        (push (car result) row)
        (setq tokens (cdr result)))
      (pcase (caar tokens)
        ('COMMA (pop tokens))
        ('SEMICOLON (pop tokens) (push (nreverse row) rows) (setq row nil))
        ('RBRACE (pop tokens) (push (nreverse row) rows) (setq done t))
        (_ (error "Expected '}'"))))
    (setq rows (nreverse rows))
    (unless (cl-every (lambda (r) (= (length r) (length (car rows)))) rows)
      (error "Rows of an array must be as long as each other"))
    (cons (list 'ARRAY rows) tokens)))

(defun exc-parser--starting (node from)
  "Return NODE, kept as starting where the token or node FROM starts.
See `exc-parser--starts'."
  (when exc-parser--starts
    (let ((start (gethash from exc-parser--starts)))
      (when start (puthash node start exc-parser--starts))))
  node)

(defun exc-parser-parse-primary-expression (tokens)
  "Parse one primary expression, keeping where it starts.
Returns (ast . remaining-tokens).  See `exc-parser--starts'."
  (let ((result (exc-parser--parse-primary tokens)))
    (exc-parser--starting (car result) (car tokens))
    result))

(defun exc-parser--parse-primary (tokens)
  "Parses primary expressions (numbers, strings, cell references,
function calls, parenthesized expressions)."
  (when (null tokens)
    (error "Unexpected end of expression"))

  (let ((token (car tokens)))
    (cond
     ;; Numbers
     ((eq (car token) exc-parser-token-type-number)
      (pop tokens)
      (cons (exc-ast-make-number (cadr token)) tokens))

     ;; Strings
     ((eq (car token) exc-parser-token-type-string)
      (pop tokens)
      (cons (exc-ast-make-string (cadr token)) tokens))

     ;; Error values
     ((eq (car token) exc-parser-token-type-error)
      (pop tokens)
      (cons (list 'ERROR (cadr token)) tokens))

     ;; TRUE and FALSE
     ((eq (car token) exc-parser-token-type-boolean)
      (pop tokens)
      (cons (list 'LITERAL (cadr token)) tokens))

     ;; Cell references or ranges
     ((eq (car token) exc-parser-token-type-cell-ref)
      (pop tokens)
      (let ((ref-str (cadr token))
            (sheet (caddr token)))
        (if (string-match ":" ref-str)
            ;; Range reference A1:B5; a sheet applies to both ends
            (cons (list 'RANGE
                        (exc-ast-make-cell-ref (car (split-string ref-str ":"))
                                                sheet)
                        (exc-ast-make-cell-ref (cadr (split-string ref-str ":"))
                                                sheet))
                  tokens)
          ;; Single cell reference
          (cons (exc-ast-make-cell-ref ref-str sheet) tokens))))

     ;; Function calls
     ((eq (car token) exc-parser-token-type-function)
      (let ((func-name (cadr token)))
        (pop tokens) ; Consume function name
        (if (not (and tokens (exc-parser-token-is-p (car tokens) exc-parser-token-type-lparen)))
            ;; A word that calls nothing names something, as Excel's
            ;; defined names do.
            (cons (list 'NAME func-name) tokens)
        (pop tokens) ; Consume '('
        (let* ((result (exc-parser--parse-arguments tokens 'RPAREN))
               (call (exc-ast-make-function-call func-name (car result)))
               (tokens (cdr result)))
          ;; What a call gives may be called in turn, as a LAMBDA is:
          ;; LAMBDA(x,x+1)(5) applies it to 5.
          (while (and tokens (exc-parser-token-is-p
                              (car tokens) exc-parser-token-type-lparen))
            (let ((result (exc-parser--parse-arguments (cdr tokens) 'RPAREN)))
              (setq call (exc-ast-make-function-call
                          "(APPLY)" (cons call (car result)))
                    tokens (cdr result))))
          (cons call tokens)))))

     ;; A name a sheet keeps: Sheet2!Total
     ((eq (car token) 'NAME-REF)
      (pop tokens)
      (cons (list 'NAME (nth 1 token) (nth 2 token)) tokens))

     ;; A table's columns: Table1[Price]
     ((eq (car token) 'STRUCT)
      (pop tokens)
      (cons (list 'STRUCT-REF (nth 1 token) (nth 2 token)) tokens))

     ;; An array constant: {1,2;3,4}
     ((eq (car token) 'LBRACE)
      (exc-parser--parse-array (cdr tokens)))

     ;; Parenthesized expressions; (A1:A3,C1:C3) is both ranges
     ((exc-parser-token-is-p token exc-parser-token-type-lparen)
      (pop tokens) ; Consume '('
      (let* ((expr-result (exc-parser-parse-expression tokens))
             (parts (list (car expr-result)))
             (remaining (cdr expr-result)))
        (while (eq (caar remaining) 'COMMA)
          (pop remaining)
          (let ((next (exc-parser-parse-expression remaining)))
            (push (car next) parts)
            (setq remaining (cdr next))))
        (unless (and remaining (exc-parser-token-is-p (car remaining) exc-parser-token-type-rparen))
          (error "Expected ')' after expression"))
        (pop remaining) ; Consume ')'
        (cons (if (cdr parts) (cons 'UNION (nreverse parts)) (car parts))
              remaining)))

     (t
      (error "Unexpected token: %s" token)))))

;;;----------------------------------------------------------------------
;;; Main Parser Function
;;;----------------------------------------------------------------------

(defconst exc-ast-type-elisp-code 'ELISP-CODE)

(defun exc-parser-parse (formula-str)
  "Parse a formula string into an AST."
  (if (string-prefix-p "elisp:" formula-str t)
      (list exc-ast-type-elisp-code (substring formula-str 6))
    (let* ((tokens (exc-parser-tokenize formula-str))
           (result (exc-parser-parse-expression tokens))
           (rest (cdr result)))
      (unless (or (null rest)
                  (exc-parser-token-is-p (car rest) exc-parser-token-type-eof))
        (error "Unexpected %s" (exc-parser-token-value (car rest))))
      (car result))))

(defun exc-parser-parse-starts (formula-str)
  "Return (AST . STARTS) for FORMULA-STR, as `exc-parser-parse' parses it.
STARTS is a table of where each node of AST starts in FORMULA-STR, for
the nodes a token starts; see `exc-parser--starts'."
  (let* ((exc-parser--starts (make-hash-table :test #'eq))
         (ast (exc-parser-parse formula-str)))
    (cons ast exc-parser--starts)))

;;;----------------------------------------------------------------------
;;; AST Evaluator (Moved to exc-calc.el)
;;;----------------------------------------------------------------------

;; AST evaluation is now handled by exc-calc.el
;; Use exc-calc-eval-ast function for evaluation

;;;----------------------------------------------------------------------
;;; Test Functions
;;;----------------------------------------------------------------------

(defun exc-parser-test-tokenizer ()
  "Tests the tokenizer function."
  (interactive)
  (let ((test-cases '(
                       "1+2*3"
                       "SUM(A1:A10)"
                       "IF(A1>0, \"Positive\", \"Negative\")"
                       "A1+B1*C1"
                       "(1+2)*(3+4)"
                       "=$A$1+B2"
                       "COUNT(C1:C5)"
                       "\"Hello World\" & \"!\""
                       "100 / 5%"
                       "NOT(TRUE)"
                       "2^3^4"
                       "A1=B1"
                       "XOR(TRUE, FALSE)"
                       )))
    (dolist (formula test-cases)
      (message "Tokenizing: %s" formula)
      (let ((tokens (exc-parser-tokenize formula)))
        (message "  Tokens: %s" tokens)
        (message "")))))

(defun exc-parser-test-parser ()
  "Tests the parser function."
  (interactive)
  (let ((test-cases '(
                       "1+2*3"
                       "SUM(A1:A10)"
                       "IF(A1>0, \"Positive\", \"Negative\")"
                       "A1+B1*C1"
                       "(1+2)*(3+4)"
                       )))
    (dolist (formula test-cases)
      (message "Parsing: %s" formula)
      (condition-case err
          (let ((ast (exc-parser-parse formula)))
            (message "  AST: %s" ast))
        (error
         (message "  Error: %s" (error-message-string err))))
      (message ""))))

(defun exc-parser-test-evaluator ()
  "Tests the AST evaluator function."
  (interactive)
  (let ((test-cases '(
                       ("1+2*3" 7)
                       ("(1+2)*(3+4)" 21)
                       ("SUM(1,2,3)" 6)
                       ("IF(1>0, 10, 20)" 10)
                       )))
    (dolist (test-case test-cases)
      (let ((formula (car test-case))
            (expected (cadr test-case)))
        (message "Evaluating: %s (expected: %s)" formula expected)
        (condition-case err
            (let* ((ast (exc-parser-parse formula))
                   (result (exc-calc-eval-ast nil ast)))
              (message "  Result: %s" result))
          (error
           (message "  Error: %s" (error-message-string err))))
        (message "")))))

(provide 'exc-parser)
