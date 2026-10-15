;;; grid-table-corpus.el --- Run real workbooks through grid-table -*- lexical-binding: t; -*-

;;; Commentary:

;; Each workbook of a corpus, such as the test files of other spreadsheet
;; projects that fetch.sh gathers, is opened, written to a new file and
;; opened again, in an Emacs of its own: run.sh starts one per workbook.
;; What the second opening finds is compared with the first, feature by
;; feature, and the formulas are counted with the errors they gave.  Each
;; workbook leaves one JSON record, which `grid-table-corpus-summarize'
;; gathers into a report.
;;
;;   emacs -Q --batch -L . -L plugins -l test/corpus/grid-table-corpus.el \
;;     -f grid-table-corpus-run-file WORKBOOK RECORD
;;   emacs -Q --batch -L . -L plugins -l test/corpus/grid-table-corpus.el \
;;     -f grid-table-corpus-summarize CORPUS-DIR RESULTS-DIR

;;; Code:

(require 'cl-lib)
(require 'json)
(setq load-prefer-newer t)
(require 'grid-table)
(require 'grid-table-xlsx)

(defconst grid-table-corpus-features
  '(values styles merges conditionals charts widths frozen hidden validations)
  "What a sheet keeps that a round trip through a file must keep too.")

(defconst grid-table-corpus--samples 5
  "Differences of each feature kept as examples.")

(defvar grid-table-corpus--record nil
  "Plist of what is found about the workbook being run.")

(defun grid-table-corpus--note (key value)
  "Record VALUE under KEY for the workbook being run."
  (setq grid-table-corpus--record
        (plist-put grid-table-corpus--record key value)))

(defun grid-table-corpus--message (err)
  "Return the message of ERR, shortened."
  (truncate-string-to-width (error-message-string err) 300))

(defmacro grid-table-corpus--stage (name &rest body)
  "Run BODY as the stage NAME and time it.
When BODY signals, record the stage and the error and return nil."
  (declare (indent 1))
  `(let ((start (float-time)))
     (condition-case err
         (prog1 (progn ,@body)
           (grid-table-corpus--note
            (intern (format ":%s-ms" ,name))
            (round (* 1000 (- (float-time) start)))))
       (error (grid-table-corpus--note :failed-stage ,name)
              (grid-table-corpus--note :error (grid-table-corpus--message err))
              nil))))

;;;----------------------------------------------------------------------
;;; What a sheet holds
;;;----------------------------------------------------------------------

(defun grid-table-corpus--sorted-plist (plist)
  "Return PLIST with its properties in the order of their names."
  (let (pairs)
    (while plist
      (push (cons (car plist) (cadr plist)) pairs)
      (setq plist (cddr plist)))
    (cl-loop for (key . value)
             in (sort pairs (lambda (a b) (string< (car a) (car b))))
             append (list key value))))

(defun grid-table-corpus--sorted (items)
  "Return ITEMS in the order of their printed forms."
  (sort (copy-sequence items)
        (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b)))))

(defun grid-table-corpus--cells (source function)
  "Return ((ROW . COL) . X) for each cell of SOURCE where FUNCTION gives X.
FUNCTION is called with the row and the column; nil or \"\" is left out."
  (let (cells)
    (dotimes (row (grid-get-row-count source))
      (dotimes (col (grid-get-column-count source))
        (let ((x (funcall function row col)))
          (unless (or (null x) (equal x ""))
            (push (cons (cons row col) x) cells)))))
    (nreverse cells)))

(defun grid-table-corpus--feature (source feature)
  "Return FEATURE of SOURCE as a list of items.
The items of values and styles are ((ROW . COL) . X), compared by cell;
the others are compared whole."
  (let ((model (gethash :model source)))
    (pcase feature
      ('values (grid-table-corpus--cells
                source (lambda (row col) (grid-get-raw-value-at source row col))))
      ('styles (grid-table-corpus--cells
                source (lambda (row col)
                         (grid-table-corpus--sorted-plist
                          (grid-model-get-style model row col)))))
      ('merges (grid-table-corpus--sorted (grid-model-get-merges model)))
      ('conditionals (grid-table-corpus--sorted
                      (grid-model-get-conditionals model)))
      ('charts (grid-table-corpus--sorted
                (mapcar #'grid-table-corpus--sorted-plist
                        (grid-table-charts source))))
      ('widths (let ((widths (grid-get-column-widths source)) items)
                 (when (hash-table-p widths)
                   (maphash (lambda (col width) (push (cons col width) items))
                            widths))
                 (grid-table-corpus--sorted items)))
      ('frozen (let ((frozen (grid-table-frozen source)))
                 (and (not (equal frozen '(0 . 0))) (list frozen))))
      ('hidden (let ((hidden (grid-table-hidden source)))
                 (and (or (car hidden) (cdr hidden)) (list hidden))))
      ('validations (grid-table-corpus--sorted
                     (grid-table-validations source))))))

(defun grid-table-corpus--differences (feature before after)
  "Return the differences between the items BEFORE and AFTER of FEATURE.
Each is (KEY BEFORE-ITEM AFTER-ITEM), with :none for a missing item.  An
item that is there twice has to be there twice after too."
  (let ((keyed (memq feature '(values styles)))
        (table (make-hash-table :test 'equal))
        (keys nil)
        differences)
    (dolist (item after)
      (let ((key (if keyed (car item) item)))
        (unless (gethash key table) (push key keys))
        (puthash key (append (gethash key table) (list item)) table)))
    (dolist (item before)
      (let* ((key (if keyed (car item) item))
             (others (gethash key table))
             (other (if others (car others) :none)))
        (puthash key (cdr others) table)
        (unless (equal item other)
          (push (list key (if keyed (cdr item) item)
                      (if (and keyed (consp other)) (cdr other) other))
                differences))))
    (dolist (key (nreverse keys))
      (dolist (item (gethash key table))
        (push (list key :none (if keyed (cdr item) item)) differences)))
    (nreverse differences)))

(defun grid-table-corpus--printed (x)
  "Return X as something JSON can hold."
  (if (or (stringp x) (numberp x)) x (prin1-to-string x)))

(defun grid-table-corpus--compare (before after names-before names-after)
  "Compare the sheets BEFORE and AFTER, ((NAME . SOURCE) ...) each.
NAMES-BEFORE and NAMES-AFTER are the defined names of the two workbooks."
  (let ((counts nil) (samples nil) (missing nil))
    (pcase-dolist (`(,name . ,source) before)
      (if-let* ((other (cdr (assoc name after))))
          (dolist (feature grid-table-corpus-features)
            (let ((differences (grid-table-corpus--differences
                                feature
                                (grid-table-corpus--feature source feature)
                                (grid-table-corpus--feature other feature))))
              (when differences
                (setf (alist-get feature counts)
                      (+ (alist-get feature counts 0) (length differences)))
                (dolist (difference differences)
                  (when (< (cl-count feature samples :key #'car)
                           grid-table-corpus--samples)
                    (push (cons feature
                                (vconcat (list name)
                                         (mapcar #'grid-table-corpus--printed
                                                 difference)))
                          samples))))))
        (push name missing)))
    (let ((differences (grid-table-corpus--differences
                        'names (grid-table-corpus--sorted names-before)
                        (grid-table-corpus--sorted names-after))))
      (when differences
        (push (cons 'names (length differences)) counts)
        (push (cons 'names (vconcat
                            (mapcar #'grid-table-corpus--printed
                                    (car differences))))
              samples)))
    (when counts
      (grid-table-corpus--note :differences counts))
    (when samples
      (grid-table-corpus--note
       :samples
       (vconcat (mapcar (lambda (sample)
                          (vconcat (list (symbol-name (car sample)))
                                   (cdr sample)))
                        (nreverse samples)))))
    (when missing
      (grid-table-corpus--note :missing-sheets (vconcat missing)))))

;;;----------------------------------------------------------------------
;;; Formulas
;;;----------------------------------------------------------------------

(defun grid-table-corpus--formulas (sheets)
  "Count the formulas of SHEETS and the errors they evaluate to.
For #NAME? the functions the formula calls are counted too."
  (let ((formulas 0)
        (errors (make-hash-table :test 'equal))
        (functions (make-hash-table :test 'equal)))
    (pcase-dolist (`(,_ . ,source) sheets)
      (dotimes (row (grid-get-row-count source))
        (dotimes (col (grid-get-column-count source))
          (let ((raw (grid-get-raw-value-at source row col)))
            (when (and (stringp raw) (string-prefix-p "=" raw))
              (cl-incf formulas)
              (let ((value (ignore-errors
                             (grid-get-computed-value-at source row col))))
                (when (and (stringp value)
                           (string-match "\\`#[A-Z/0!?]+" value))
                  (cl-incf (gethash (match-string 0 value) errors 0))
                  (when (string-prefix-p "#NAME" value)
                    (let ((start 0))
                      (while (string-match "\\([A-Z][A-Z0-9.]*\\)(" raw start)
                        (cl-incf (gethash (match-string 1 raw) functions 0))
                        (setq start (match-end 0))))))))))))
    (grid-table-corpus--note :formulas formulas)
    (cl-flet ((alist (table)
                (let (items)
                  (maphash (lambda (key count) (push (cons key count) items))
                           table)
                  items)))
      (when (> (hash-table-count errors) 0)
        (grid-table-corpus--note :formula-errors (alist errors)))
      (when (> (hash-table-count functions) 0)
        (grid-table-corpus--note :name-error-functions (alist functions))))))

;;;----------------------------------------------------------------------
;;; One workbook
;;;----------------------------------------------------------------------

(defun grid-table-corpus--read (file names)
  "Return ((NAME . SOURCE) ...) for the sheets NAMES of FILE that open.
A sheet that does not open is recorded."
  (let (opened failed)
    (dolist (name names)
      (condition-case err
          (push (cons name (grid-table-xlsx-create-data-source file name))
                opened)
        (error (push (vector name (grid-table-corpus--message err)) failed))))
    (when failed
      (grid-table-corpus--note :sheet-errors (vconcat (nreverse failed))))
    (nreverse opened)))

(defun grid-table-corpus-run (file record)
  "Open the workbook FILE, write it, open it again, and write RECORD.
RECORD is a JSON file of what was found."
  (setq file (expand-file-name file)
        grid-table-corpus--record nil)
  (grid-table-corpus--note :file file)
  (let ((copy (make-temp-file "grid-table-corpus-" nil ".xlsx")))
    (delete-file copy)
    (unwind-protect
        (when-let* ((names (grid-table-corpus--stage "sheets"
                             (grid-table-xlsx-sheet-names file))))
          (grid-table-corpus--note :sheets (length names))
          (when-let* ((sheets (grid-table-corpus--stage "read"
                                (grid-table-corpus--read file names))))
            (grid-table-corpus--note :read-sheets (length sheets))
            (grid-table-corpus--note
             :cells (apply #'+ (mapcar (lambda (sheet)
                                         (length (grid-table-corpus--feature
                                                  (cdr sheet) 'values)))
                                       sheets)))
            (when-let* ((workbook (grid-table-corpus--stage "recalc"
                                    (let ((workbook (grid-workbook-create sheets t)))
                                      (when-let* ((defined (grid-table-xlsx--names file)))
                                        (grid-workbook-set-names workbook defined))
                                      workbook))))
              (grid-table-corpus--stage "formulas"
                (grid-table-corpus--formulas sheets))
              (when (grid-table-corpus--stage "write"
                      (grid-table-xlsx-write-workbook workbook copy)
                      t)
                (when-let* ((again (grid-table-corpus--stage "reread"
                                     (grid-table-xlsx-create-workbook copy))))
                  (grid-table-corpus--stage "compare"
                    (grid-table-corpus--compare
                     sheets
                     (mapcar (lambda (sheet)
                               (cons (grid-workbook-sheet-name sheet) sheet))
                             (grid-workbook-sheets again))
                     (grid-workbook-names workbook)
                     (grid-workbook-names again))))))))
      (when (file-exists-p copy)
        (delete-file copy))
      (with-temp-file record
        (insert (json-encode grid-table-corpus--record))))))

(defun grid-table-corpus-run-file ()
  "Run `grid-table-corpus-run' on the workbook and record named on the command line."
  (let ((file (pop command-line-args-left))
        (record (pop command-line-args-left)))
    (grid-table-corpus-run file record)))

;;;----------------------------------------------------------------------
;;; The report
;;;----------------------------------------------------------------------

(defun grid-table-corpus--records (results)
  "Return the records in the directory RESULTS, as alists."
  (delq nil
        (mapcar (lambda (file)
                  (ignore-errors
                    (with-temp-buffer
                      (insert-file-contents file)
                      (json-parse-buffer :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil))))
                (directory-files results t "\\.json\\'"))))

(defun grid-table-corpus--source (record corpus)
  "Return the project RECORD's workbook comes from, its first directory under CORPUS."
  (car (split-string (file-relative-name (alist-get 'file record) corpus) "/")))

(defun grid-table-corpus--outcome (record)
  "Return what happened to the workbook of RECORD, a short string."
  (cond ((alist-get 'failed-stage record)
         (concat "fail:" (alist-get 'failed-stage record)))
        ((alist-get 'sheet-errors record) "some-sheets-failed")
        ((or (alist-get 'differences record) (alist-get 'missing-sheets record))
         "roundtrip-diff")
        (t "clean")))

(defun grid-table-corpus--shape (message)
  "Return MESSAGE with its file names and numbers made general."
  (let* ((lines (split-string (string-trim (or message "")) "\n"))
         (text (or (car (last lines)) "")))
    (setq text (replace-regexp-in-string "/[^ \"()]*\\.xls[xm]" "<file>" text))
    (setq text (replace-regexp-in-string "[0-9]+" "N" text))
    (truncate-string-to-width text 140)))

(defun grid-table-corpus--tally (table key)
  "Add one to KEY in the hash TABLE."
  (puthash key (1+ (gethash key table 0)) table))

(defun grid-table-corpus--ranked (table)
  "Return the (KEY . COUNT) of TABLE, the largest count first."
  (let (items)
    (maphash (lambda (key count) (push (cons key count) items)) table)
    (sort items (lambda (a b) (> (cdr a) (cdr b))))))

(defun grid-table-corpus-report (corpus results)
  "Return the report on the records in RESULTS of the workbooks in CORPUS."
  (let* ((corpus (file-name-as-directory (expand-file-name corpus)))
         (records (grid-table-corpus--records results))
         (outcomes (make-hash-table :test 'equal))
         (columns nil)
         (errors (make-hash-table :test 'equal))
         (examples (make-hash-table :test 'equal))
         (features (make-hash-table :test 'equal))
         (feature-files (make-hash-table :test 'equal))
         (samples (make-hash-table :test 'equal)))
    (dolist (record records)
      (let ((source (grid-table-corpus--source record corpus))
            (outcome (grid-table-corpus--outcome record))
            (file (file-relative-name (alist-get 'file record) corpus)))
        (cl-pushnew outcome columns :test #'equal)
        (grid-table-corpus--tally outcomes (cons source outcome))
        (grid-table-corpus--tally outcomes (cons source "total"))
        (when-let* ((stage (alist-get 'failed-stage record)))
          (let ((key (format "%-8s %s" stage
                             (grid-table-corpus--shape (alist-get 'error record)))))
            (grid-table-corpus--tally errors key)
            (unless (gethash key examples) (puthash key file examples))))
        (dolist (failure (alist-get 'sheet-errors record))
          (let ((key (format "%-8s %s" "sheet"
                             (grid-table-corpus--shape (cadr failure)))))
            (grid-table-corpus--tally errors key)
            (unless (gethash key examples) (puthash key file examples))))
        (pcase-dolist (`(,feature . ,count) (alist-get 'differences record))
          (puthash feature (+ count (gethash feature features 0)) features)
          (grid-table-corpus--tally feature-files feature))
        (dolist (sample (alist-get 'samples record))
          (let ((feature (intern (car sample))))
            (when (< (length (gethash feature samples)) 4)
              (push (cons file (cdr sample)) (gethash feature samples)))))))
    (setq columns (sort columns #'string<))
    (with-temp-buffer
      (let ((sources (sort (delete-dups
                            (mapcar (lambda (record)
                                      (grid-table-corpus--source record corpus))
                                    records))
                           #'string<)))
        (insert (format "%-16s%7s" "source" "total"))
        (dolist (column columns) (insert (format (format "%%%ds" (+ 2 (length column))) column)))
        (insert "\n")
        (dolist (source (append sources '(nil)))
          (insert (format "%-16s%7d" (or source "ALL")
                          (if source
                              (gethash (cons source "total") outcomes 0)
                            (length records))))
          (dolist (column columns)
            (insert (format (format "%%%dd" (+ 2 (length column)))
                            (if source
                                (gethash (cons source column) outcomes 0)
                              (cl-loop for s in sources
                                       sum (gethash (cons s column) outcomes 0))))))
          (insert "\n")))
      (insert "\n== Errors ==\n")
      (pcase-dolist (`(,key . ,count) (grid-table-corpus--ranked errors))
        (insert (format "%5d %s\n        e.g. %s\n" count key (gethash key examples))))
      (insert "\n== Round-trip differences: items (workbooks) ==\n")
      (pcase-dolist (`(,feature . ,count) (grid-table-corpus--ranked features))
        (insert (format "%7d (%d) %s\n" count (gethash feature feature-files 0)
                        feature))
        (dolist (sample (reverse (gethash feature samples)))
          (insert (format "          %s %s\n" (car sample)
                          (truncate-string-to-width
                           (mapconcat (lambda (x) (format "%S" x)) (cdr sample) " ")
                           200)))))
      (insert "\n== Slowest ==\n")
      (dolist (entry (last (sort (delq nil
                                       (mapcar
                                        (lambda (record)
                                          (unless (alist-get 'failed-stage record)
                                            (list (cl-loop for (key . value) in record
                                                           when (string-suffix-p
                                                                 "-ms" (symbol-name key))
                                                           sum value)
                                                  (alist-get 'cells record)
                                                  (file-relative-name
                                                   (alist-get 'file record) corpus))))
                                        records))
                                 (lambda (a b) (< (car a) (car b))))
                           8))
        (insert (format "%7.1fs %8s cells %s\n" (/ (car entry) 1000.0)
                        (nth 1 entry) (nth 2 entry))))
      (buffer-string))))

(defun grid-table-corpus-summarize ()
  "Print the report on the corpus and results named on the command line."
  (let ((corpus (pop command-line-args-left))
        (results (pop command-line-args-left)))
    (princ (grid-table-corpus-report corpus results))))

(provide 'grid-table-corpus)

;;; grid-table-corpus.el ends here
