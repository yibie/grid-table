;;; grid-table-calc-worker.el --- The Emacs that computes CALC -*- lexical-binding: t -*-

;;; Commentary:

;; `grid-calc-worker-run' is what the Emacs started for CALC runs, as
;; emacs -Q --batch -l grid-table-calc-worker -f grid-calc-worker-run.
;; It reads one job a line, (ID TEXT MODES), and writes one answer a
;; line, (ID PRINTED), where PRINTED is what `calc-eval' gave: a string,
;; or (POSITION MESSAGE) for text Calc cannot read, or (error MESSAGE)
;; for Calc failing.  Before the first job it writes (ready), once Calc
;; is loaded, so the time it takes to start is not counted against a
;; job.  It stops when its input ends.

;;; Code:

(require 'calc)

(defun grid-calc-worker-run ()
  "Answer CALC's jobs from standard input until it ends."
  (let ((print-escape-newlines t) (print-length nil) (print-level nil))
    (let ((inhibit-message t)) (calc-eval "1"))
    (princ "(ready)\n")
    (while t
      (let* ((line (condition-case nil (read-from-minibuffer "")
                     (error (kill-emacs 0))))
             (job (read line))
             (printed (condition-case err
                          (let ((inhibit-message t))
                            (calc-eval (cons (nth 1 job) (nth 2 job))))
                        (error (list 'error (error-message-string err))))))
        (princ (format "%S\n" (list (car job) printed)))))))

(provide 'grid-table-calc-worker)

;;; grid-table-calc-worker.el ends here
