;;; exc-calc-symbolic.el --- Symbolic math through Emacs Calc -*- lexical-binding: t -*-

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
;; exc-calc-worker.el drives.  A cell whose answer has not come
;; yet shows #GETTING_DATA; when it comes, the cells that asked are
;; computed again and `exc-calc-calc-ready-functions' run.  There are
;; up to `exc-calc-calc-workers' of them, so a slow job holds only its
;; own.  A job that takes longer than `exc-calc-calc-timeout' is
;; #CALC!: that Emacs is stopped and another started for later jobs.
;; In batch, as in the tests, Calc runs here and the answer is there at
;; once.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'exc-calc)
(require 'exc-obsolete)

(declare-function calc-eval "calc" (str &optional separator &rest args))

(defcustom exc-calc-calc-async (not noninteractive)
  "Non-nil makes CALC compute in another Emacs, without holding this one.
Nil computes here: a long computation then holds Emacs until it ends or
\\[keyboard-quit]."
  :type 'boolean
  :group 'exc)

(defcustom exc-calc-calc-timeout 10
  "Seconds a CALC computation may take in the other Emacs before it is #CALC!."
  :type 'number
  :group 'exc)

(defcustom exc-calc-calc-workers 2
  "How many other Emacsen CALC computes in at most.
A slow computation holds only the one computing it, so the others go on
with the texts after it."
  :type 'natnum
  :group 'exc)

(defvar exc-calc-calc-ready-functions nil
  "Functions run when answers CALC was waiting for have come.
Each is called with the formulas that asked, a list of (MODEL ROW COL)
as `exc-calc--here' has them, once their cells are marked to be
computed again.  The view uses it to recalculate and show them.")

(defconst exc-calc--calc-modes
  '(calc-angle-mode rad calc-internal-prec 15 calc-language nil
    calc-display-working-message nil calc-symbolic-mode nil)
  "The Calc modes CALC computes under, as `calc-eval' takes them.")

(defvar exc-calc--calc-answers (make-hash-table :test 'equal)
  "What CALC gave for each text it was handed, so far.")

(defconst exc-calc--calc-answers-limit 500
  "How many answers `exc-calc--calc-answers' keeps before it starts over.")

(defvar exc-calc--calc-waiting (make-hash-table :test 'equal)
  "For each text sent to the other Emacs, the formulas waiting for it.")

;; One of the other Emacsen: its process, whether it has said it is
;; ready, the job it is computing as (ID . TEXT), what it has written of
;; an answer line not yet ended, and the timer that stops the job when
;; it takes too long.
(cl-defstruct (exc-calc--worker (:constructor exc-calc--worker-make))
  process ready job (output "") timer)

(defvar exc-calc--calc-queue nil
  "The texts waiting for one of the other Emacsen to be free.")

(defvar exc-calc--calc-workers nil
  "The other Emacsen started so far, as `exc-calc--worker's.")

(defvar exc-calc--calc-sent 0
  "How many jobs have been sent to the other Emacsen, also each one's id.")

(defun exc-calc--calc-value (printed)
  "The value of what Calc PRINTED: a number when it is one, else text.
PRINTED is what `calc-eval' gave, or (error MESSAGE) for Calc failing."
  (cond ((and (consp printed) (eq (car printed) 'error)) (exc-calc-err "#NUM!"))
        ;; (POSITION MESSAGE): the text does not parse.
        ((not (stringp printed)) (exc-calc-err "#VALUE!"))
        (t
         (let ((number (and (string-match-p
                             "\\`-?[0-9]+\\.?[0-9]*\\(?:e-?[0-9]+\\)?\\'" printed)
                            (string-to-number printed))))
           (cond ((null number) printed)
                 ((integerp number)
                  (if (<= (abs number) (expt 2 53)) number printed))
                 ((or (isnan number) (= (abs number) 1.0e+INF)) printed)
                 (t number))))))

(defun exc-calc--calc-keep (text answer)
  "Remember that CALC gives ANSWER for TEXT."
  (when (>= (hash-table-count exc-calc--calc-answers)
            exc-calc--calc-answers-limit)
    (clrhash exc-calc--calc-answers))
  (puthash text answer exc-calc--calc-answers))

(defun exc-calc-calc-forget ()
  "Forget the answers CALC has had, so that each is computed again."
  (clrhash exc-calc--calc-answers))

(defun exc-calc--calc-here (text)
  "What Emacs Calc makes of TEXT, computed in this Emacs."
  (exc-calc--calc-value
   (condition-case err
       ;; Calc says what rules it is compiling as it goes.
       (let ((inhibit-message t) (message-log-max nil))
         (calc-eval (cons text exc-calc--calc-modes)))
     (error (list 'error (error-message-string err))))))

;;;----------------------------------------------------------------------
;;; The other Emacs
;;;----------------------------------------------------------------------

(defun exc-calc--calc-start (worker)
  "Start WORKER's Emacs, unless it is running."
  (unless (process-live-p (exc-calc--worker-process worker))
    (setf (exc-calc--worker-output worker) ""
          (exc-calc--worker-ready worker) nil)
    (setf (exc-calc--worker-process worker)
          (make-process
           :name "exc-calc"
           :command (list (expand-file-name invocation-name invocation-directory)
                          "-Q" "--batch"
                          "-l" (or (locate-library "exc-calc-worker")
                                   (error "exc-calc-worker is not on load-path"))
                          "-f" "exc-calc-worker-run")
           :connection-type 'pipe
           :noquery t
           :stderr (get-buffer-create " *exc-calc errors*")
           :filter (lambda (process output)
                     (exc-calc--calc-filter worker process output))
           :sentinel (lambda (process event)
                       (exc-calc--calc-sentinel worker process event))))
    (when-let* ((stderr (get-buffer-process " *exc-calc errors*")))
      (set-process-query-on-exit-flag stderr nil))))

(defun exc-calc--calc-free-worker ()
  "A worker computing nothing, made when there are fewer than allowed."
  (or (seq-find (lambda (worker) (null (exc-calc--worker-job worker)))
                exc-calc--calc-workers)
      (when (< (length exc-calc--calc-workers) (max 1 exc-calc-calc-workers))
        (car (push (exc-calc--worker-make) exc-calc--calc-workers)))))

(defun exc-calc--calc-send-next ()
  "Send the texts in the queue to the other Emacsen that are free."
  (let (worker)
    (while (and exc-calc--calc-queue
                (setq worker (exc-calc--calc-free-worker)))
      (let* ((text (pop exc-calc--calc-queue))
             (id (cl-incf exc-calc--calc-sent))
             (print-escape-newlines t) (print-length nil) (print-level nil))
        (exc-calc--calc-start worker)
        (setf (exc-calc--worker-job worker) (cons id text))
        (process-send-string
         (exc-calc--worker-process worker)
         (concat (prin1-to-string (list id text exc-calc--calc-modes)) "\n"))
        (exc-calc--calc-clock worker)))))

(defun exc-calc--calc-clock (worker)
  "Start timing WORKER's job, once its Emacs is ready.
The job waits in the Emacs's input while it starts, which is not
counted against `exc-calc-calc-timeout'."
  (when (and (exc-calc--worker-ready worker) (exc-calc--worker-job worker)
             (not (exc-calc--worker-timer worker)))
    (setf (exc-calc--worker-timer worker)
          (run-at-time exc-calc-calc-timeout nil #'exc-calc--calc-too-long
                       worker (car (exc-calc--worker-job worker))))))

(defun exc-calc--calc-done (worker answer)
  "Give ANSWER to what waits for WORKER's job, and send it the next one."
  (let ((text (cdr (exc-calc--worker-job worker))))
    (when (timerp (exc-calc--worker-timer worker))
      (cancel-timer (exc-calc--worker-timer worker)))
    (setf (exc-calc--worker-timer worker) nil (exc-calc--worker-job worker) nil)
    (exc-calc--calc-keep text answer)
    (let ((waiters (gethash text exc-calc--calc-waiting)))
      (remhash text exc-calc--calc-waiting)
      (exc-calc--calc-send-next)
      (when waiters
        (dolist (waiter waiters)
          (when (nth 1 waiter)
            (exc-calc-touch (nth 0 waiter) (nth 1 waiter) (nth 2 waiter))))
        (with-demoted-errors "exc CALC: %S"
          (run-hook-with-args 'exc-calc-calc-ready-functions waiters))))))

(defun exc-calc--calc-filter (worker _process output)
  "Take OUTPUT, part of WORKER's answers."
  (setf (exc-calc--worker-output worker)
        (concat (exc-calc--worker-output worker) output))
  (while (string-match "\n" (exc-calc--worker-output worker))
    (let* ((pending (exc-calc--worker-output worker))
           (line (substring pending 0 (match-beginning 0))))
      (setf (exc-calc--worker-output worker) (substring pending (match-end 0)))
      (pcase (ignore-errors (car (read-from-string line)))
        ('(ready)
         (setf (exc-calc--worker-ready worker) t)
         (exc-calc--calc-clock worker))
        (`(,id ,printed)
         (when (eql id (car (exc-calc--worker-job worker)))
           (exc-calc--calc-done worker (exc-calc--calc-value printed))))))))

(defun exc-calc--calc-too-long (worker id)
  "Stop WORKER's job ID, which took too long; its answer is #CALC!."
  (when (eql id (car (exc-calc--worker-job worker)))
    (let ((process (exc-calc--worker-process worker)))
      (setf (exc-calc--worker-process worker) nil)
      (when (process-live-p process) (delete-process process)))
    (exc-calc--calc-done worker (exc-calc-err "#CALC!"))))

(defun exc-calc--calc-sentinel (worker process _event)
  "Pick up after WORKER's Emacs, PROCESS, stopped by itself."
  (when (and (eq process (exc-calc--worker-process worker))
             (not (process-live-p process)))
    (setf (exc-calc--worker-process worker) nil)
    (when (exc-calc--worker-job worker)
      (exc-calc--calc-done worker (exc-calc-err "#CALC!")))))

(defun exc-calc-calc-stop ()
  "Stop the other Emacsen CALC computes in; jobs in progress are dropped."
  (dolist (worker exc-calc--calc-workers)
    (when (timerp (exc-calc--worker-timer worker))
      (cancel-timer (exc-calc--worker-timer worker)))
    (let ((process (exc-calc--worker-process worker)))
      (setf (exc-calc--worker-process worker) nil)
      (when (process-live-p process) (delete-process process))))
  (setq exc-calc--calc-workers nil exc-calc--calc-queue nil)
  (clrhash exc-calc--calc-waiting))

(defun exc-calc--calc-ask (text)
  "Have the other Emacs compute TEXT for the formula being computed."
  (let ((waiters (gethash text exc-calc--calc-waiting 'none)))
    ;; A text already sent or queued is not sent again.
    (when (eq waiters 'none)
      (setq waiters nil
            exc-calc--calc-queue (append exc-calc--calc-queue (list text))))
    (when (and exc-calc--here (not (member exc-calc--here waiters)))
      (push (copy-sequence exc-calc--here) waiters))
    (puthash text waiters exc-calc--calc-waiting)
    (exc-calc--calc-send-next)))

(defun exc-calc--calc (text)
  "What Emacs Calc makes of TEXT, as CALC gives it."
  (let ((answer (gethash text exc-calc--calc-answers 'none)))
    (cond ((not (eq answer 'none)) answer)
          (exc-calc-calc-async
           (exc-calc--calc-ask text)
           (exc-calc-err "#GETTING_DATA"))
          (t (exc-calc--calc-keep text (exc-calc--calc-here text))))))

(exc-calc-define calc ((expression text))
  "What Emacs Calc makes of EXPRESSION, written in Calc's algebraic
notation: deriv(x^2, x) is \"2 x\", integ(sin(x), x) is \"-cos(x)\",
roots(x^2 - 5 x + 6, x) is \"[3., 2.]\" and 2^0.5 is 1.4142135623731.
A number comes back as a number, anything else as text.  EXPRESSION that
Calc cannot read is #VALUE!, one it fails on #NUM!, and one that takes
longer than `exc-calc-calc-timeout' #CALC!.  While the answer is being
computed it is #GETTING_DATA."
  (exc-calc--calc expression))

(provide 'exc-calc-symbolic)

;;; exc-calc-symbolic.el ends here
