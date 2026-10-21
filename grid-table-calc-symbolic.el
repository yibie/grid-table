;;; grid-table-calc-symbolic.el --- Symbolic math through Emacs Calc -*- lexical-binding: t -*-

;;; Commentary:

;; CALC hands its text to Emacs Calc's algebraic entry and gives back
;; what Calc prints: =CALC("deriv(x^2, x)") is "2 x".  Calc's language
;; reaches only Calc's own math functions, those named calcFunc-..., so
;; it needs no leave to run Lisp, as =elisp: does.
;;
;; Calc runs in radians, at 15 digits, with its working messages off and
;; its plain language, whatever the user's Calc is set to.  The answer
;; is a number when Calc prints one a double holds exactly, and text
;; otherwise.  It is not part of Excel, which shows #NAME? for it.
;;
;; A long computation must not hold Emacs, so in an interactive Emacs
;; Calc runs in another one, started once and kept, which
;; grid-table-calc-worker.el drives.  A cell whose answer has not come
;; yet shows #GETTING_DATA; when it comes, the cells that asked are
;; computed again and `grid-calc-calc-ready-functions' run.  A job that
;; takes longer than `grid-calc-calc-timeout' is #CALC!: that Emacs is
;; stopped and another started for the jobs after it.  In batch, as in
;; the tests, Calc runs here and the answer is there at once.

;;; Code:

(require 'grid-table-calc)

(declare-function calc-eval "calc" (str &optional separator &rest args))

(defcustom grid-calc-calc-async (not noninteractive)
  "Non-nil makes CALC compute in another Emacs, without holding this one.
Nil computes here: a long computation then holds Emacs until it ends or
\\[keyboard-quit]."
  :type 'boolean
  :group 'grid-table)

(defcustom grid-calc-calc-timeout 10
  "Seconds a CALC computation may take in the other Emacs before it is #CALC!."
  :type 'number
  :group 'grid-table)

(defvar grid-calc-calc-ready-functions nil
  "Functions run when answers CALC was waiting for have come.
Each is called with the formulas that asked, a list of (MODEL ROW COL)
as `grid-calc--here' has them, once their cells are marked to be
computed again.  The view uses it to recalculate and show them.")

(defconst grid-calc--calc-modes
  '(calc-angle-mode rad calc-internal-prec 15 calc-language nil
    calc-display-working-message nil calc-symbolic-mode nil)
  "The Calc modes CALC computes under, as `calc-eval' takes them.")

(defvar grid-calc--calc-answers (make-hash-table :test 'equal)
  "What CALC gave for each text it was handed, so far.")

(defconst grid-calc--calc-answers-limit 500
  "How many answers `grid-calc--calc-answers' keeps before it starts over.")

(defvar grid-calc--calc-waiting (make-hash-table :test 'equal)
  "For each text sent to the other Emacs, the formulas waiting for it.")

(defvar grid-calc--calc-queue nil
  "The texts to send to the other Emacs after the one it is computing.")

(defvar grid-calc--calc-job nil
  "(ID . TEXT) of what the other Emacs is computing, or nil.")

(defvar grid-calc--calc-process nil
  "The other Emacs, or nil before it is started.")

(defvar grid-calc--calc-output ""
  "What the other Emacs has written of an answer line not yet ended.")

(defvar grid-calc--calc-timer nil
  "The timer that stops the job in progress when it takes too long.")

(defvar grid-calc--calc-sent 0
  "How many jobs have been sent to the other Emacs, also each one's id.")

(defun grid-calc--calc-value (printed)
  "The value of what Calc PRINTED: a number when it is one, else text.
PRINTED is what `calc-eval' gave, or (error MESSAGE) for Calc failing."
  (cond ((and (consp printed) (eq (car printed) 'error)) (grid-calc-err "#NUM!"))
        ;; (POSITION MESSAGE): the text does not parse.
        ((not (stringp printed)) (grid-calc-err "#VALUE!"))
        (t
         (let ((number (and (string-match-p
                             "\\`-?[0-9]+\\.?[0-9]*\\(?:e-?[0-9]+\\)?\\'" printed)
                            (string-to-number printed))))
           (cond ((null number) printed)
                 ((integerp number)
                  (if (<= (abs number) (expt 2 53)) number printed))
                 ((or (isnan number) (= (abs number) 1.0e+INF)) printed)
                 (t number))))))

(defun grid-calc--calc-keep (text answer)
  "Remember that CALC gives ANSWER for TEXT."
  (when (>= (hash-table-count grid-calc--calc-answers)
            grid-calc--calc-answers-limit)
    (clrhash grid-calc--calc-answers))
  (puthash text answer grid-calc--calc-answers))

(defun grid-calc-calc-forget ()
  "Forget the answers CALC has had, so that each is computed again."
  (clrhash grid-calc--calc-answers))

(defun grid-calc--calc-here (text)
  "What Emacs Calc makes of TEXT, computed in this Emacs."
  (grid-calc--calc-value
   (condition-case err
       ;; Calc says what rules it is compiling as it goes.
       (let ((inhibit-message t) (message-log-max nil))
         (calc-eval (cons text grid-calc--calc-modes)))
     (error (list 'error (error-message-string err))))))

;;;----------------------------------------------------------------------
;;; The other Emacs
;;;----------------------------------------------------------------------

(defun grid-calc--calc-start ()
  "Start the other Emacs, unless it is running."
  (unless (process-live-p grid-calc--calc-process)
    (setq grid-calc--calc-output "")
    (setq grid-calc--calc-process
          (make-process
           :name "grid-table-calc"
           :command (list (expand-file-name invocation-name invocation-directory)
                          "-Q" "--batch"
                          "-l" (or (locate-library "grid-table-calc-worker")
                                   (error "grid-table-calc-worker is not on load-path"))
                          "-f" "grid-calc-worker-run")
           :connection-type 'pipe
           :noquery t
           :stderr (get-buffer-create " *grid-table-calc errors*")
           :filter #'grid-calc--calc-filter
           :sentinel #'grid-calc--calc-sentinel))))

(defun grid-calc--calc-send-next ()
  "Send the next text in the queue to the other Emacs, if it is free."
  (when (and (null grid-calc--calc-job) grid-calc--calc-queue)
    (let ((text (pop grid-calc--calc-queue))
          (print-escape-newlines t) (print-length nil) (print-level nil))
      (grid-calc--calc-start)
      (setq grid-calc--calc-job (cons (cl-incf grid-calc--calc-sent) text))
      (process-send-string
       grid-calc--calc-process
       (concat (prin1-to-string (list (car grid-calc--calc-job) text
                                      grid-calc--calc-modes))
               "\n"))
      (setq grid-calc--calc-timer
            (run-at-time grid-calc-calc-timeout nil #'grid-calc--calc-too-long
                         (car grid-calc--calc-job))))))

(defun grid-calc--calc-done (answer)
  "Give ANSWER to what waits for the job in progress, and start the next."
  (let ((text (cdr grid-calc--calc-job)))
    (when (timerp grid-calc--calc-timer) (cancel-timer grid-calc--calc-timer))
    (setq grid-calc--calc-timer nil grid-calc--calc-job nil)
    (grid-calc--calc-keep text answer)
    (let ((waiters (gethash text grid-calc--calc-waiting)))
      (remhash text grid-calc--calc-waiting)
      (grid-calc--calc-send-next)
      (when waiters
        (dolist (waiter waiters)
          (when (nth 1 waiter)
            (grid-calc-touch (nth 0 waiter) (nth 1 waiter) (nth 2 waiter))))
        (with-demoted-errors "grid-table CALC: %S"
          (run-hook-with-args 'grid-calc-calc-ready-functions waiters))))))

(defun grid-calc--calc-filter (_process output)
  "Take OUTPUT, part of the other Emacs's answers."
  (setq grid-calc--calc-output (concat grid-calc--calc-output output))
  (while (string-match "\n" grid-calc--calc-output)
    (let ((line (substring grid-calc--calc-output 0 (match-beginning 0))))
      (setq grid-calc--calc-output
            (substring grid-calc--calc-output (match-end 0)))
      (pcase (ignore-errors (car (read-from-string line)))
        (`(,id ,printed)
         (when (eql id (car grid-calc--calc-job))
           (grid-calc--calc-done (grid-calc--calc-value printed))))))))

(defun grid-calc--calc-too-long (id)
  "Stop the job ID, which took too long; its answer is #CALC!."
  (when (eql id (car grid-calc--calc-job))
    (let ((process grid-calc--calc-process))
      (setq grid-calc--calc-process nil)
      (when (process-live-p process) (delete-process process)))
    (grid-calc--calc-done (grid-calc-err "#CALC!"))))

(defun grid-calc--calc-sentinel (process _event)
  "Pick up after the other Emacs, PROCESS, stopped by itself."
  (when (and (eq process grid-calc--calc-process)
             (not (process-live-p process)))
    (setq grid-calc--calc-process nil)
    (when grid-calc--calc-job
      (grid-calc--calc-done (grid-calc-err "#CALC!")))))

(defun grid-calc--calc-ask (text)
  "Have the other Emacs compute TEXT for the formula being computed."
  (let ((waiters (gethash text grid-calc--calc-waiting 'none)))
    ;; A text already sent or queued is not sent again.
    (when (eq waiters 'none)
      (setq waiters nil
            grid-calc--calc-queue (append grid-calc--calc-queue (list text))))
    (when (and grid-calc--here (not (member grid-calc--here waiters)))
      (push (copy-sequence grid-calc--here) waiters))
    (puthash text waiters grid-calc--calc-waiting)
    (grid-calc--calc-send-next)))

(defun grid-calc--calc (text)
  "What Emacs Calc makes of TEXT, as CALC gives it."
  (let ((answer (gethash text grid-calc--calc-answers 'none)))
    (cond ((not (eq answer 'none)) answer)
          (grid-calc-calc-async
           (grid-calc--calc-ask text)
           (grid-calc-err "#GETTING_DATA"))
          (t (grid-calc--calc-keep text (grid-calc--calc-here text))))))

(grid-calc-define calc ((expression text))
  "What Emacs Calc makes of EXPRESSION, written in Calc's algebraic
notation: deriv(x^2, x) is \"2 x\", integ(sin(x), x) is \"-cos(x)\",
roots(x^2 - 5 x + 6, x) is \"[3., 2.]\" and 2^0.5 is 1.4142135623731.
A number comes back as a number, anything else as text.  EXPRESSION that
Calc cannot read is #VALUE!, one it fails on #NUM!, and one that takes
longer than `grid-calc-calc-timeout' #CALC!.  While the answer is being
computed it is #GETTING_DATA."
  (grid-calc--calc expression))

(provide 'grid-table-calc-symbolic)

;;; grid-table-calc-symbolic.el ends here
