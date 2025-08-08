;;; grid-table-parser.el --- AST parser for grid formulas -*- lexical-binding: t -*-

;;;----------------------------------------------------------------------
;;; Token Definitions
;;;----------------------------------------------------------------------

;; Token types
(defconst grid-parser-token-type-number 'NUMBER)
(defconst grid-parser-token-type-string 'STRING)
(defconst grid-parser-token-type-cell-ref 'CELL-REF)
(defconst grid-parser-token-type-function 'FUNCTION)
(defconst grid-parser-token-type-operator 'OPERATOR)
(defconst grid-parser-token-type-identifier 'IDENTIFIER)
(defconst grid-parser-token-type-lparen 'LPAREN)
(defconst grid-parser-token-type-rparen 'RPAREN)
(defconst grid-parser-token-type-comma 'COMMA)
(defconst grid-parser-token-type-eof 'EOF)

;; Helper to create a token
(defun grid-parser-make-token (type value)
  "Create a token with TYPE and VALUE."
  (list type value))

;; Helper to check token type
(defun grid-parser-token-is-p (token type &optional value)
  "Checks if TOKEN matches TYPE and optionally VALUE."
  (and (eq (car token) type)
       (or (null value) (equal (cadr token) value))))

;; Helper to get token value
(defun grid-parser-token-value (token)
  "Get the value from a token."
  (cadr token))

;; Helper to get token type
(defun grid-parser-token-type (token)
  "Get the type from a token."
  (car token))

;; Helper to get operator value as string
(defun grid-parser-get-operator (token)
  "Gets the operator value from an OPERATOR token as string."
  (when (eq (car token) grid-parser-token-type-operator)
    (char-to-string (cadr token))))

;;;----------------------------------------------------------------------
;;; Lexical Analyzer (Tokenizer)
;;;----------------------------------------------------------------------

(defun grid-parser-tokenize (formula-str)
  "Tokenizes a formula string into a list of tokens."
  (let ((tokens '())
        (pos 0)
        (len (length formula-str)))
    (while (< pos len)
      (let ((char (aref formula-str pos)))
        (cond
         ;; Skip whitespace
         ((member char '(?\s ?\t ?\n))
          (setq pos (1+ pos)))

         ;; Numbers (integers and floats)
         ((or (and (>= char ?0) (<= char ?9)) (eq char ?.))
          (let ((start pos))
            (while (and (< pos len)
                        (or (and (>= (aref formula-str pos) ?0)
                                 (<= (aref formula-str pos) ?9))
                            (eq (aref formula-str pos) ?.)))
              (setq pos (1+ pos)))
            (push (grid-parser-make-token grid-parser-token-type-number
                                          (string-to-number (substring formula-str start pos)))
                  tokens)))

         ;; Strings (e.g., "hello world")
         ((eq char ?\")
          (setq pos (1+ pos)) ; Consume opening quote
          (let ((start pos))
            (while (and (< pos len) (not (eq (aref formula-str pos) ?\")))
              (setq pos (1+ pos)))
            (push (grid-parser-make-token grid-parser-token-type-string
                                          (substring formula-str start pos))
                  tokens)
            (setq pos (1+ pos)))) ; Consume closing quote

         ;; Parentheses and comma
         ((eq char ?\()
          (push (grid-parser-make-token grid-parser-token-type-lparen char) tokens)
          (setq pos (1+ pos)))
         ((eq char ?\))
          (push (grid-parser-make-token grid-parser-token-type-rparen char) tokens)
          (setq pos (1+ pos)))
         ((eq char ?,)
          (push (grid-parser-make-token grid-parser-token-type-comma char) tokens)
          (setq pos (1+ pos)))

         ;; Cell references (e.g., A1, $A$1, A1:B5) or Function names (e.g., SUM)
         ((and (>= char ?A) (<= char ?Z))
          (let ((start pos))
            (while (and (< pos len)
                        (or (and (>= (aref formula-str pos) ?A)
                                 (<= (aref formula-str pos) ?Z))
                            (and (>= (aref formula-str pos) ?0)
                                 (<= (aref formula-str pos) ?9))
                            (eq (aref formula-str pos) ?$)
                            (eq (aref formula-str pos) ?:)))
              (setq pos (1+ pos)))
            (let* ((text (substring formula-str start pos))
                   (token-type (cond
                               ;; Check for cell reference pattern (e.g., A1, $A$1, A1:B5)
                               ((string-match "^\\(\\$?[A-Z]+\\$?[0-9]+\\)\\(:\\$?[A-Z]+\\$?[0-9]+\\)?$" text)
                                grid-parser-token-type-cell-ref)
                               ;; Otherwise, assume it's a function name
                               (t grid-parser-token-type-function))))
              (push (grid-parser-make-token token-type text) tokens))))

         ;; Operators and delimiters
         (t
          (push (grid-parser-make-token grid-parser-token-type-operator char) tokens)
          (setq pos (1+ pos))))))
    ;; Add EOF token
    (push (grid-parser-make-token grid-parser-token-type-eof nil) tokens)
    (nreverse tokens)))

;;;----------------------------------------------------------------------
;;; AST Node Definitions
;;;----------------------------------------------------------------------

;; AST node types
(defconst grid-ast-type-number 'NUMBER)
(defconst grid-ast-type-string 'STRING)
(defconst grid-ast-type-cell-ref 'CELL-REF)
(defconst grid-ast-type-binary-op 'BINARY-OP)
(defconst grid-ast-type-function-call 'FUNCTION-CALL)
(defconst grid-ast-type-unary-op 'UNARY-OP)

;; Helper to create AST nodes
(defun grid-ast-make-number (value)
  "Create a number AST node."
  (list grid-ast-type-number value))

(defun grid-ast-make-string (value)
  "Create a string AST node."
  (list grid-ast-type-string value))

(defun grid-ast-make-cell-ref (ref)
  "Create a cell reference AST node."
  (list grid-ast-type-cell-ref ref))

(defun grid-ast-make-binary-op (operator left right)
  "Create a binary operation AST node."
  (list grid-ast-type-binary-op operator left right))

(defun grid-ast-make-unary-op (operator operand)
  "Create a unary operation AST node."
  (list grid-ast-type-unary-op operator operand))

(defun grid-ast-make-function-call (function-name arguments)
  "Create a function call AST node."
  (list grid-ast-type-function-call function-name arguments))

;; Helper to check AST node type
(defun grid-ast-node-type (node)
  "Get the type of an AST node."
  (car node))

(defun grid-ast-node-value (node)
  "Get the value of an AST node."
  (cadr node))

(defun grid-ast-node-left (node)
  "Get the left operand of a binary operation node."
  (caddr node))

(defun grid-ast-node-right (node)
  "Get the right operand of a binary operation node."
  (cadddr node))

(defun grid-ast-node-operand (node)
  "Get the operand of a unary operation node."
  (caddr node))

(defun grid-ast-node-function-name (node)
  "Get the function name of a function call node."
  (cadr node))

(defun grid-ast-node-arguments (node)
  "Get the arguments of a function call node."
  (caddr node))

;;;----------------------------------------------------------------------
;;; Parser Implementation
;;;----------------------------------------------------------------------

(defclass grid-parser ()
  ((tokens :initarg :tokens :accessor grid-parser-tokens)
   (current :initform 0 :accessor grid-parser-current)))

(defun grid-parser-create (tokens)
  "Create a new parser instance."
  (make-instance 'grid-parser :tokens tokens))

(defun grid-parser-current-token (parser)
  "Get the current token."
  (nth (grid-parser-current parser) (grid-parser-tokens parser)))

(defun grid-parser-peek-token (parser)
  "Peek at the next token without consuming it."
  (nth (1+ (grid-parser-current parser)) (grid-parser-tokens parser)))

(defun grid-parser-advance (parser)
  "Advance to the next token."
  (setf (grid-parser-current parser) (1+ (grid-parser-current parser))))

(defun grid-parser-match (parser expected-type)
  "Match and consume a token of the expected type."
  (let ((token (grid-parser-current-token parser)))
    (if (grid-parser-token-is-p token expected-type)
        (progn
          (grid-parser-advance parser)
          token)
      (error "Expected %s, got %s" expected-type (grid-parser-token-type token)))))

(defun grid-parser-check (parser type)
  "Check if the current token is of the given type."
  (grid-parser-token-is-p (grid-parser-current-token parser) type))

(defun grid-parser-check-any (parser &rest types)
  "Check if the current token is any of the given types."
  (let ((token (grid-parser-current-token parser)))
    (cl-some (lambda (type) (grid-parser-token-is-p token type)) types)))

;;;----------------------------------------------------------------------
;;; Grammar Rules (Recursive Descent Parser)
;;;----------------------------------------------------------------------

(defun grid-parser-parse-expression (tokens)
  "Parses a list of tokens into an AST. Returns (ast . remaining-tokens)."
  (grid-parser-parse-or-expression tokens))

(defun grid-parser-parse-or-expression (tokens)
  "Parses OR expressions (lowest precedence). Returns (ast . remaining-tokens)."
  (let* ((result (grid-parser-parse-and-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens (grid-parser-token-is-p (car remaining-tokens) grid-parser-token-type-operator "OR"))
      (pop remaining-tokens) ; Consume OR token
      (let* ((right-result (grid-parser-parse-and-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (grid-ast-make-binary-op 'OR left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun grid-parser-parse-and-expression (tokens)
  "Parses AND expressions. Returns (ast . remaining-tokens)."
  (let* ((result (grid-parser-parse-equality-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens (grid-parser-token-is-p (car remaining-tokens) grid-parser-token-type-operator "AND"))
      (pop remaining-tokens) ; Consume AND token
      (let* ((right-result (grid-parser-parse-equality-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (grid-ast-make-binary-op 'AND left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun grid-parser-parse-equality-expression (tokens)
  "Parses equality expressions (=, <>, <, >, <=, >=). Returns (ast . remaining-tokens)."
  (let* ((result (grid-parser-parse-comparison-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (let ((op (grid-parser-get-operator (car remaining-tokens))))
                  (member op '("=" "<>" "<" ">" "<=" ">="))))
      (let* ((op (grid-parser-get-operator (pop remaining-tokens)))
             (right-result (grid-parser-parse-comparison-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (grid-ast-make-binary-op (intern op) left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun grid-parser-parse-comparison-expression (tokens)
  "Parses comparison expressions. Returns (ast . remaining-tokens)."
  (let* ((result (grid-parser-parse-additive-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (let ((op (grid-parser-get-operator (car remaining-tokens))))
                  (member op '("<" ">" "<=" ">="))))
      (let* ((op (grid-parser-get-operator (pop remaining-tokens)))
             (right-result (grid-parser-parse-additive-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (grid-ast-make-binary-op (intern op) left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun grid-parser-parse-additive-expression (tokens)
  "Parses additive expressions (+, -). Returns (ast . remaining-tokens)."
  (let* ((result (grid-parser-parse-multiplicative-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (let ((op (grid-parser-get-operator (car remaining-tokens))))
                  (member op '("+" "-"))))
      (let* ((op (grid-parser-get-operator (pop remaining-tokens)))
             (right-result (grid-parser-parse-multiplicative-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (grid-ast-make-binary-op (intern op) left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun grid-parser-parse-multiplicative-expression (tokens)
  "Parses multiplicative expressions (*, /, %). Returns (ast . remaining-tokens)."
  (let* ((result (grid-parser-parse-power-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens
                (let ((op (grid-parser-get-operator (car remaining-tokens))))
                  (member op '("*" "/" "%"))))
      (let* ((op (grid-parser-get-operator (pop remaining-tokens)))
             (right-result (grid-parser-parse-power-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (grid-ast-make-binary-op (intern op) left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun grid-parser-parse-power-expression (tokens)
  "Parses power expressions (^). Returns (ast . remaining-tokens)."
  (let* ((result (grid-parser-parse-unary-expression tokens))
         (left (car result))
         (remaining-tokens (cdr result)))
    (while (and remaining-tokens (grid-parser-token-is-p (car remaining-tokens) grid-parser-token-type-operator "^"))
      (pop remaining-tokens) ; Consume ^ token
      (let* ((right-result (grid-parser-parse-unary-expression remaining-tokens))
             (right (car right-result))
             (new-remaining (cdr right-result)))
        (setq left (grid-ast-make-binary-op '^ left right))
        (setq remaining-tokens new-remaining)))
    (cons left remaining-tokens)))

(defun grid-parser-parse-unary-expression (tokens)
  "Parses unary expressions (+, -, NOT). Returns (ast . remaining-tokens)."
  (cond
   ((and tokens (grid-parser-token-is-p (car tokens) grid-parser-token-type-operator "+"))
    (pop tokens)
    (let* ((result (grid-parser-parse-primary-expression tokens))
           (operand (car result))
           (remaining (cdr result)))
      (cons (grid-ast-make-unary-op '+ operand) remaining)))
   ((and tokens (grid-parser-token-is-p (car tokens) grid-parser-token-type-operator "-"))
    (pop tokens)
    (let* ((result (grid-parser-parse-primary-expression tokens))
           (operand (car result))
           (remaining (cdr result)))
      (cons (grid-ast-make-unary-op '- operand) remaining)))
   ((and tokens (grid-parser-token-is-p (car tokens) grid-parser-token-type-operator "NOT"))
    (pop tokens)
    (let* ((result (grid-parser-parse-primary-expression tokens))
           (operand (car result))
           (remaining (cdr result)))
      (cons (grid-ast-make-unary-op 'NOT operand) remaining)))
   (t
    (grid-parser-parse-primary-expression tokens))))

(defun grid-parser-parse-primary-expression (tokens)
  "Parses primary expressions (numbers, strings, cell references, function calls, parenthesized expressions)."
  (when (null tokens)
    (error "Unexpected end of expression"))

  (let ((token (car tokens)))
    (cond
     ;; Numbers
     ((eq (car token) grid-parser-token-type-number)
      (pop tokens)
      (cons (grid-ast-make-number (cadr token)) tokens))

     ;; Strings
     ((eq (car token) grid-parser-token-type-string)
      (pop tokens)
      (cons (grid-ast-make-string (cadr token)) tokens))

     ;; Cell references or ranges
     ((eq (car token) grid-parser-token-type-cell-ref)
      (pop tokens)
      (let ((ref-str (cadr token)))
        (if (string-match ":" ref-str)
            ;; Range reference A1:B5
            (cons (list 'RANGE
                        (grid-ast-make-cell-ref (car (split-string ref-str ":")))
                        (grid-ast-make-cell-ref (cadr (split-string ref-str ":"))))
                  tokens)
          ;; Single cell reference
          (cons (grid-ast-make-cell-ref ref-str) tokens))))

     ;; Function calls
     ((eq (car token) grid-parser-token-type-function)
      (let ((func-name (cadr token)))
        (pop tokens) ; Consume function name
        (unless (and tokens (grid-parser-token-is-p (car tokens) grid-parser-token-type-lparen))
          (error "Expected '(' after function name"))
        (pop tokens) ; Consume '('

        ;; Parse argument list
        (let ((args '())
              (remaining-tokens tokens))
          (unless (and remaining-tokens (grid-parser-token-is-p (car remaining-tokens) grid-parser-token-type-rparen))
            (let* ((arg-result (grid-parser-parse-expression remaining-tokens))
                   (arg (car arg-result))
                   (new-remaining (cdr arg-result)))
              (push arg args)
              (setq remaining-tokens new-remaining)
              (while (and remaining-tokens (grid-parser-token-is-p (car remaining-tokens) grid-parser-token-type-comma))
                (pop remaining-tokens) ; Consume ','
                (let* ((next-arg-result (grid-parser-parse-expression remaining-tokens))
                       (next-arg (car next-arg-result))
                       (next-remaining (cdr next-arg-result)))
                  (push next-arg args)
                  (setq remaining-tokens next-remaining)))))

          (unless (and remaining-tokens (grid-parser-token-is-p (car remaining-tokens) grid-parser-token-type-rparen))
            (error "Expected ')' after function arguments"))
          (pop remaining-tokens) ; Consume ')'

          (cons (grid-ast-make-function-call func-name (nreverse args)) remaining-tokens))))

     ;; Parenthesized expressions
     ((grid-parser-token-is-p token grid-parser-token-type-lparen)
      (pop tokens) ; Consume '('
      (let* ((expr-result (grid-parser-parse-expression tokens))
             (expr (car expr-result))
             (remaining (cdr expr-result)))
        (unless (and remaining (grid-parser-token-is-p (car remaining) grid-parser-token-type-rparen))
          (error "Expected ')' after expression"))
        (pop remaining) ; Consume ')'
        (cons expr remaining)))

     (t
      (error "Unexpected token: %s" token)))))

;;;----------------------------------------------------------------------
;;; Main Parser Function
;;;----------------------------------------------------------------------

(defun grid-parser-parse (formula-str)
  "Parse a formula string into an AST."
  (let ((tokens (grid-parser-tokenize formula-str)))
    (car (grid-parser-parse-expression tokens))))

;;;----------------------------------------------------------------------
;;; AST Evaluator (Moved to grid-table-calc.el)
;;;----------------------------------------------------------------------

;; AST evaluation is now handled by grid-table-calc.el
;; Use grid-calc-eval-ast function for evaluation

;;;----------------------------------------------------------------------
;;; Test Functions
;;;----------------------------------------------------------------------

(defun grid-parser-test-tokenizer ()
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
                       "100 / 5 % 2"
                       "NOT(TRUE)"
                       "2^3^4"
                       "A1=B1"
                       "XOR(TRUE, FALSE)"
                       )))
    (dolist (formula test-cases)
      (message "Tokenizing: %s" formula)
      (let ((tokens (grid-parser-tokenize formula)))
        (message "  Tokens: %s" tokens)
        (message "")))))

(defun grid-parser-test-parser ()
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
          (let ((ast (grid-parser-parse formula)))
            (message "  AST: %s" ast))
        (error
         (message "  Error: %s" (error-message-string err))))
      (message ""))))

(defun grid-parser-test-evaluator ()
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
            (let* ((ast (grid-parser-parse formula))
                   (result (grid-calc-eval-ast nil ast)))
              (message "  Result: %s" result))
          (error
           (message "  Error: %s" (error-message-string err))))
        (message "")))))

(provide 'grid-table-parser)
