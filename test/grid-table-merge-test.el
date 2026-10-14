;;; grid-table-merge-test.el --- Horizontal merge integration tests -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'grid-table)

;; These tests characterize the classic renderer.
(require 'grid-table-csv)

(defun grid-table-test--source ()
  "Return a small source whose second model row can be merged."
  (make-default-data-source
   '("H1" "H2" "H3" "H4")
   '(("H1" "H2" "H3" "H4")
     ("10" "" "" "x")
     ("a" "b" "c" "d"))
   "Merge test"))

(ert-deftest grid-table-merge-keeps-covered-values-empty ()
  (let* ((model (grid-model-create
                 1 4 '(("10" "" "" "=SUM(A1:C1)"))))
         (merged (grid-model-merge-cells model 0 0 2)))
    (should (equal (grid-model-get-merges merged) '((0 0 0 2))))
    (should (equal (grid-cell-raw-input
                    (grid-model-get-cell merged 0 1))
                   ""))
    (should (equal (grid-model-get-computed-value merged 0 3) 10))
    (should-error (grid-model-set-raw-value merged 0 1 "hidden"))
    (let ((unmerged (grid-model-unmerge-cell merged 0 1)))
      (should-not (grid-model-get-merges unmerged))
      (should (equal (grid-cell-raw-input
                      (grid-model-get-cell unmerged 0 0))
                     "10")))))

(ert-deftest grid-table-merge-refuses-nonempty-covered-cells ()
  (let ((model (grid-model-create 1 3 '(("anchor" "value" "")))))
    (let ((err (should-error (grid-model-merge-cells model 0 0 2))))
      (should (string-match-p
               (regexp-quote "((0 1))")
               (error-message-string err))))))

(ert-deftest grid-table-sort-remaps-merges-by-row-identity ()
  (let* ((model (grid-model-create
                 4 3
                 '(("Header" "" "H3")
                   ("b" "x" "")
                   ("a" "" "")
                   ("b" "x" ""))
                 '((0 0 0 1) (1 1 1 2))))
         (sorted (grid-model-sort-by-column model 0 'ascending)))
    (should (equal (grid-model-get-merges sorted)
                   '((0 0 0 1) (2 1 2 2))))
    (should (equal (grid-cell-raw-input
                    (grid-model-get-cell sorted 1 0))
                   "a"))
    (should (equal (grid-cell-raw-input
                    (grid-model-get-cell sorted 2 0))
                   "b"))))

(ert-deftest grid-table-sort-treats-covered-key-as-empty ()
  (let* ((model (grid-model-create
                 3 2
                 '(("H1" "H2")
                   ("z" "")
                   ("other" "a"))
                 '((1 0 1 1))))
         (sorted (grid-model-sort-by-column model 1 'ascending)))
    ;; The covered key is empty, not the merge's "z", and empty cells sort
    ;; last: the merged row moves below "other" and takes its merge along.
    (should (equal (grid-cell-raw-input
                    (grid-model-get-cell sorted 2 0))
                   "z"))
    (should (equal (grid-model-get-merges sorted) '((2 0 2 1))))))

(ert-deftest grid-table-column-edits-transform-merges-atomically ()
  (let* ((model (grid-model-create
                 3 4
                 '(("H1" "H2" "H3" "H4")
                   ("x" "10" "" "")
                   ("a" "b" "c" "d"))
                 '((1 1 1 3))))
         (inserted (grid-model-add-column model 2))
         (anchor-deleted (grid-model-delete-column inserted 1))
         (collapsed (grid-model-delete-column anchor-deleted 1)))
    (should (equal (grid-model-get-merges inserted) '((1 1 1 4))))
    (should (equal (grid-model-get-merges anchor-deleted) '((1 1 1 3))))
    (should (equal (grid-cell-raw-input
                    (grid-model-get-cell anchor-deleted 1 1))
                   "10"))
    (should (equal (grid-model-get-merges collapsed) '((1 1 1 2))))
    (let ((ordinary (grid-model-delete-column collapsed 1)))
      (should-not (grid-model-get-merges ordinary))
      (should (equal (grid-cell-raw-input
                      (grid-model-get-cell ordinary 1 1))
                     "10")))))

(ert-deftest grid-table-anchor-column-delete-preserves-header-content ()
  (let ((source (make-default-data-source
                 '("A" "Title" "" "")
                 '(("A" "Title" "" "")
                   ("a" "b" "c" "d"))
                 nil
                 '((0 1 0 3)))))
    (funcall (gethash :delete-column source) source 1)
    (should (equal (gethash :headers source) '("A" "Title" "")))
    (should (equal (grid-get-merges source) '((0 1 0 2))))))

(ert-deftest grid-table-row-edits-move-and-remove-merges ()
  (let* ((model (grid-model-create
                 3 3
                 '(("H1" "H2" "H3")
                   ("10" "" "x")
                   ("a" "b" "c"))
                 '((1 0 1 1))))
         (inserted (grid-model-add-row model 0))
         (restored (grid-model-delete-row inserted 0))
         (deleted (grid-model-delete-row restored 0)))
    (should (equal (grid-model-get-merges inserted) '((2 0 2 1))))
    (should (equal (grid-model-get-merges restored) '((1 0 1 1))))
    (should-not (grid-model-get-merges deleted))))


(ert-deftest grid-table-merged-content-does-not-size-columns ()
  (should
   (equal
    (grid-table--calculate-column-widths
     '("A very long merged heading" "" "")
     '(("A very long merged heading" "" ""))
     '((0 0 0 2)))
    '(5 5 5))))

(ert-deftest grid-table-persistence-round-trips-v1-and-v2 ()
  (let ((plain-file (make-temp-file "grid-v1-" nil ".grid"))
        (merged-file (make-temp-file "grid-v2-" nil ".grid"))
        (plain (grid-table-test--source))
        (merged (grid-table-test--source)))
    (unwind-protect
        (progn
          (funcall (gethash :merge-cells merged) merged 1 0 2)
          (grid-table-persistence--save-to-file plain plain-file)
          (grid-table-persistence--save-to-file merged merged-file)
          (with-temp-buffer
            (insert-file-contents plain-file)
            (should (keywordp (car (read (current-buffer))))))
          (with-temp-buffer
            (insert-file-contents merged-file)
            (should (eq (car (read (current-buffer))) 'grid-table-file)))
          (let ((loaded
                 (grid-table-persistence-create-data-source-from-file
                  merged-file)))
            (should (equal (grid-get-merges loaded) '((1 0 1 2))))))
      (delete-file plain-file)
      (delete-file merged-file))))

(ert-deftest grid-table-load-discards-invalid-merges-without-data-loss ()
  (let ((file (make-temp-file "grid-invalid-" nil ".grid"))
        warnings)
    (unwind-protect
        (progn
          (with-temp-file file
            (prin1 '(grid-table-file
                     :version 2
                     :title "Invalid"
                     :headers ("" "" "kept")
                     :rows nil
                     :merges ((0 0 2)))
                   (current-buffer)))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (_type message &rest _args)
                       (push message warnings))))
            (let* ((source
                    (grid-table-persistence-create-data-source-from-file file))
                   (model (gethash :model source)))
              (should-not (grid-model-get-merges model))
              (should (equal (grid-cell-raw-input
                              (grid-model-get-cell model 0 2))
                             "kept"))))
          (should (cl-some (lambda (warning)
                             (string-match-p
                              (regexp-quote "((0 2))")
                              warning))
                           warnings)))
      (delete-file file))))

(ert-deftest grid-table-load-warns-on-malformed-merge-list ()
  (let ((file (make-temp-file "grid-malformed-" nil ".grid"))
        warnings)
    (unwind-protect
        (progn
          (with-temp-file file
            (prin1 '(grid-table-file
                     :version 2
                     :title "Invalid"
                     :headers ("A" "B")
                     :rows nil
                     :merges broken)
                   (current-buffer)))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (_type message &rest _args)
                       (push message warnings))))
            (let ((source
                   (grid-table-persistence-create-data-source-from-file file)))
              (should-not (grid-get-merges source))))
          (should warnings))
      (delete-file file))))

(ert-deftest grid-table-save-rejects-invalid-in-memory-merges ()
  (let ((file (make-temp-file "grid-invalid-save-" nil ".grid"))
        (source (grid-table-test--source)))
    (unwind-protect
        (progn
          (set-grid-data-model-merges
           (gethash :model source) '((0 0 1 1)))
          (should-error
           (grid-table-persistence--save-to-file source file)))
      (delete-file file))))

(ert-deftest grid-table-csv-exports-covered-cells-as-empty ()
  (let ((file (make-temp-file "grid-merge-" nil ".csv"))
        (source (make-default-data-source
                 '("A" "B" "C")
                 '(("10" "" "")))))
    (unwind-protect
        (progn
          (funcall (gethash :merge-cells source) source 0 0 2)
          (grid-table-csv--save-data-source source file)
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "^10,,$" (buffer-string)))))
      (delete-file file))))

(ert-deftest grid-table-package-root-exposes-plugin-directory ()
  (let* ((root (file-name-directory (locate-library "grid-table")))
         (plugin-dir (expand-file-name "plugins" root))
         (load-path (delete plugin-dir (copy-sequence load-path))))
    (load (expand-file-name "grid-table.el" root) nil t)
    (should (member plugin-dir load-path))
    (should (locate-library "grid-table-csv"))))

(ert-deftest grid-table-api-create-defaults-key-bindings ()
  (let (bindings)
    (cl-letf (((symbol-function 'grid-table--create-grid)
               (lambda (_source _columns value _title _file-path)
                 (setq bindings value))))
      (grid-table-api-create :data-source (make-hash-table))
      (should (equal bindings grid-table-default-bindings)))))

(ert-deftest grid-table-match-defaults-to-exact-match ()
  (should (= 2 (grid-calc-call-function nil "MATCH"
                                        '("beta" ("alpha" "beta"))))))

(ert-deftest grid-table-csv-open-preserves-csv-data-source ()
  (let ((input (make-temp-file "grid-csv-input-" nil ".csv"))
        (output (make-temp-file "grid-csv-output-" nil ".csv"))
        source)
    (unwind-protect
        (progn
          (with-temp-file input
            (insert "name,amount\nalpha,10\nbeta,20\n"))
          (cl-letf (((symbol-function 'grid-table-api-create)
                     (lambda (&rest args)
                       (setq source (plist-get args :data-source)))))
            (grid-table-find-file-csv input))
          (should (equal (gethash :file-path source) input))
          (should (functionp (gethash :save-to-file source)))
          (let ((grid-table--data-source source))
            (grid-table-csv-save output))
          (with-temp-buffer
            (insert-file-contents output)
            (should (equal (buffer-string)
                           "name,amount\nalpha,10\nbeta,20\n"))))
      (delete-file input)
      (delete-file output))))

;;;----------------------------------------------------------------------
;;; Persistence: old loaders must reject v2 tagged files
;;;----------------------------------------------------------------------

(defun grid-table-test--legacy-loader-dispatch (data)
  "Inline snapshot of the pre-merge loader's top-level dispatch.
Copied from the loader before tagged v2 support (see
`git show HEAD:grid-table-persistence.el'): keyword car -> v1 plist,
list car -> legacy alist, anything else -> error.

This guards the whole reason the tagged v2 format exists: a file with
merge metadata must be loudly rejected by old grid-table versions,
never silently misparsed (which would destroy the merges on the next
save from the old version)."
  (cond
   ((keywordp (car data)) :v1-plist)
   ((listp (car data)) :legacy-alist)
   (t (error "Unknown grid file format"))))

(ert-deftest grid-table-legacy-loader-rejects-v2-tagged-files ()
  (let ((plain-file (make-temp-file "grid-v1-" nil ".grid"))
        (merged-file (make-temp-file "grid-v2-" nil ".grid"))
        (plain (grid-table-test--source))
        (merged (grid-table-test--source)))
    (unwind-protect
        (progn
          (funcall (gethash :merge-cells merged) merged 1 0 2)
          (grid-table-persistence--save-to-file plain plain-file)
          (grid-table-persistence--save-to-file merged merged-file)
          ;; (a) v2 tagged data saved by the new loader is rejected by
          ;; the old dispatch: its car is the symbol `grid-table-file',
          ;; neither a keyword nor a list.
          (let ((v2 (with-temp-buffer
                      (insert-file-contents merged-file)
                      (read (current-buffer)))))
            (should (eq (car v2) 'grid-table-file))
            (let ((err (should-error
                        (grid-table-test--legacy-loader-dispatch v2))))
              (should (string-match-p
                       (regexp-quote "Unknown grid file format")
                       (error-message-string err)))))
          ;; (b) v1 plist data still passes the old dispatch untouched.
          (let ((v1 (with-temp-buffer
                      (insert-file-contents plain-file)
                      (read (current-buffer)))))
            (should (eq (grid-table-test--legacy-loader-dispatch v1)
                        :v1-plist))))
      (delete-file plain-file)
      (delete-file merged-file))))

;;;----------------------------------------------------------------------
;;; Merge preconditions: formula and whitespace covered cells
;;;----------------------------------------------------------------------

(ert-deftest grid-table-merge-refuses-formula-covered-cells ()
  ;; Guards against a future "optimization" that evaluates the formula
  ;; and accepts the merge when the computed value happens to be
  ;; empty: validation must look at the raw layer only.  Here D1:D3 is
  ;; empty, so the formula computes to 0, yet the merge is refused.
  (let ((model (grid-model-create
                3 4
                '(("anchor" "=SUM(D1:D3)" "" "")
                  ("" "" "" "")
                  ("" "" "" "")))))
    (let ((err (should-error (grid-model-merge-cells model 0 0 2))))
      (should (string-match-p
               (regexp-quote "((0 1))")
               (error-message-string err))))))

(ert-deftest grid-table-merge-refuses-whitespace-covered-cells ()
  ;; A single space is user data, not an empty cell: merging must not
  ;; trim covered cells before validating them.
  (let ((model (grid-model-create 1 3 '(("anchor" " " "")))))
    (let ((err (should-error (grid-model-merge-cells model 0 0 2))))
      (should (string-match-p
               (regexp-quote "((0 1))")
               (error-message-string err))))))


(provide 'grid-table-merge-test)

;;; grid-table-merge-test.el ends here
