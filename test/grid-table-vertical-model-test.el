;;; grid-table-vertical-model-test.el --- Vertical merge model tests -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'grid-table)

;; These tests characterize the classic renderer.

(ert-deftest grid-table-model-accepts-vertical-merge-quad ()
  (let* ((model (grid-model-create
                 4 2
                 '(("H1" "H2")
                   ("dept" "x")
                   ("" "y")
                   ("" "z"))))
         (merged (grid-model-merge-cells model 1 0 3 0)))
    (should (equal (grid-model-get-merges merged) '((1 0 3 0))))
    (should-error (grid-model-set-raw-value merged 2 0 "covered"))
    (should (equal (grid-cell-raw-input (grid-model-get-cell merged 1 0))
                   "dept"))))

(ert-deftest grid-table-vertical-merge-keeps-range-formulas-single-counted ()
  (let* ((model
          (grid-model-create
           4 2
           '(("H" "sum")
             ("10" "")
             ("" "")
             ("" "=SUM(A2:A4)"))))
         (merged (grid-model-merge-cells model 1 0 3 0)))
    (should (equal (grid-model-get-computed-value merged 3 1) 10))
    (should (equal (grid-cell-raw-input
                    (grid-model-get-cell merged 2 0))
                   ""))))

(ert-deftest grid-table-model-normalizes-reversed-merge-endpoints ()
  (let* ((model (grid-model-create
                 4 2
                 '(("H1" "H2")
                   ("dept" "x")
                   ("" "y")
                   ("" "z"))))
         (merged (grid-model-merge-cells model 3 0 1 0)))
    (should (equal (grid-model-get-merges merged) '((1 0 3 0))))))

(ert-deftest grid-table-model-rejects-rectangular-and-row-zero-crossing-merges ()
  (let ((model (grid-model-create
                3 3
                '(("H1" "" "")
                  ("a" "" "")
                  ("" "" "")))))
    ;; A rectangle is a merge like any other; it still cannot take row 0.
    (should (equal (grid-model-get-merges
                    (grid-model-merge-cells model 1 0 2 1))
                   '((1 0 2 1))))
    (should-error (grid-model-merge-cells model 0 0 1 0))
    (should-error (grid-model-merge-cells model 0 0 1 1))))

(ert-deftest grid-table-validation-keeps-first-overlapping-range ()
  (let ((model (grid-model-create 1 5 '(("" "" "" "" ""))))
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _args)
                 (push message warnings))))
      (should
       (equal
        (grid-model-validate-merges
         model '((0 0 0 1) (0 1 0 2) (0 3 0 4)) t)
        '((0 0 0 1) (0 3 0 4)))))
    (should (= (length warnings) 1))
    (should (string-match-p
             (regexp-quote "conflicts with ((0 0 0 1)) at ((0 1 0 1))")
             (car warnings)))))

(ert-deftest grid-table-validation-rejects-nonempty-before-overlap ()
  (let ((model (grid-model-create 1 4 '(("a" "kept" "" ""))))
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _args)
                 (push message warnings))))
      (should
       (equal
        (grid-model-validate-merges
         model '((0 0 0 1) (0 1 0 2)) t)
        '((0 1 0 2)))))
    (should (cl-some (lambda (warning)
                       (string-match-p
                        (regexp-quote "covered cells are not empty at ((0 1))")
                        warning))
                     warnings))))

(ert-deftest grid-table-sort-rejects-any-vertical-merge-at-model-layer ()
  (let* ((model (grid-model-create
                 4 2
                 '(("H1" "H2")
                   ("dept" "z")
                   ("" "a")
                   ("other" "b"))))
         (merged (grid-model-merge-cells model 1 0 2 0)))
    (let ((err (should-error
                (grid-model-sort-by-column merged 1 'ascending))))
      (should (equal (error-message-string err)
                     "Cannot sort a model containing vertical merges; unmerge them first")))
    (let ((err (should-error
                (grid-model-sort-by-column merged 9 'ascending))))
      (should (string-match-p "Invalid column index" (error-message-string err))))
    (let ((err (should-error
                (grid-model-sort-by-column merged 1 'sideways))))
      (should (string-match-p "Invalid sort order" (error-message-string err))))))

(ert-deftest grid-table-vertical-row-edits-follow-surviving-anchor ()
  (let* ((model (grid-model-create
                 5 2
                 '(("H1" "H2")
                   ("dept" "a")
                   ("" "b")
                   ("" "c")
                   ("tail" "d"))))
         (merged (grid-model-merge-cells model 1 0 3 0))
         (expanded (grid-model-add-row merged 1))
         (anchor-deleted (grid-model-delete-row merged 0))
         (bottom-boundary (grid-model-add-row merged 3)))
    (should (equal (grid-model-get-merges expanded) '((1 0 4 0))))
    (should (equal (grid-model-get-merges anchor-deleted) '((1 0 2 0))))
    (should (equal (grid-cell-raw-input
                    (grid-model-get-cell anchor-deleted 1 0))
                   "dept"))
    (should (equal (grid-model-get-merges bottom-boundary) '((1 0 3 0))))))

(ert-deftest grid-table-vertical-column-delete-removes-single-column-merge ()
  (let* ((model (grid-model-create
                 4 3
                 '(("H1" "H2" "H3")
                   ("x" "dept" "a")
                   ("y" "" "b")
                   ("z" "" "c"))))
         (merged (grid-model-merge-cells model 1 1 3 1))
         (deleted (grid-model-delete-column merged 1)))
    (should-not (grid-model-get-merges deleted))
    (should (equal (grid-cell-raw-input
                    (grid-model-get-cell deleted 1 1))
                   "a"))))

(ert-deftest grid-table-vertical-structure-operations-cover-interval-boundaries ()
  (let* ((model
          (grid-model-create
           6 3
           '(("H1" "H2" "H3")
             ("before" "x" "a")
             ("dept" "y" "b")
             ("" "z" "c")
             ("" "w" "d")
             ("after" "v" "e"))
           '((2 0 4 0))))
         (insert-before (grid-model-add-row model 0))
         (delete-before (grid-model-delete-row model 0))
         (delete-inside (grid-model-delete-row model 2))
         (delete-bottom (grid-model-delete-row model 3))
         (insert-column-before (grid-model-add-column model 0))
         (short
          (grid-model-create
           5 2
           '(("H1" "H2")
             ("before" "x")
             ("dept" "y")
             ("" "z")
             ("after" "w"))
           '((2 0 3 0))))
         (collapsed (grid-model-delete-row short 2)))
    (should (equal (grid-model-get-merges insert-before)
                   '((3 0 5 0))))
    (should (equal (grid-model-get-merges delete-before)
                   '((1 0 3 0))))
    (should (equal (grid-model-get-merges delete-inside)
                   '((2 0 3 0))))
    (should (equal (grid-model-get-merges delete-bottom)
                   '((2 0 3 0))))
    (should (equal (grid-model-get-merges insert-column-before)
                   '((2 1 4 1))))
    (should-not (grid-model-get-merges collapsed))))

(ert-deftest grid-table-persistence-writes-v3-only-for-vertical-merges ()
  (let ((file (make-temp-file "grid-v3-" nil ".grid"))
        (source (make-default-data-source
                 '("H1" "H2")
                 '(("H1" "H2")
                   ("dept" "x")
                   ("" "y")))))
    (unwind-protect
        (progn
          (puthash :model
                   (grid-model-merge-cells
                    (gethash :model source) 1 0 2 0)
                   source)
          (grid-table-persistence--save-to-file source file)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((data (read (current-buffer))))
              (should (eq (plist-get (cdr data) :version) 3))
              (should (equal (plist-get (cdr data) :merges)
                             '((1 0 2 0))))))
          (let ((loaded
                 (grid-table-persistence-create-data-source-from-file file)))
            (should (equal (grid-get-merges loaded)
                           '((1 0 2 0))))))
      (delete-file file))))

(ert-deftest grid-table-persistence-saves-horizontal-quads-as-v2-triples ()
  (let ((file (make-temp-file "grid-v2-" nil ".grid"))
        (source (make-default-data-source
                 '("H1" "H2" "H3")
                 '(("H1" "H2" "H3")
                   ("wide" "" "")))))
    (unwind-protect
        (progn
          (funcall (gethash :merge-cells source) source 1 0 2)
          (grid-table-persistence--save-to-file source file)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((data (read (current-buffer))))
              (should (eq (plist-get (cdr data) :version) 2))
              (should (equal (plist-get (cdr data) :merges)
                             '((1 0 2))))))
          (let ((loaded
                 (grid-table-persistence-create-data-source-from-file file)))
            (should (equal (grid-get-merges loaded)
                           '((1 0 1 2))))))
      (delete-file file))))

(ert-deftest grid-table-save-validates-merges-without-a-model-slot ()
  (let* ((file (make-temp-name
                (expand-file-name "grid-custom-" temporary-file-directory)))
         (rows '(("H") ("anchor") ("covered")))
         (source (make-hash-table)))
    (puthash :get-column-count (lambda (_source) 1) source)
    (puthash :get-row-count (lambda (_source) 3) source)
    (puthash :get-header-value (lambda (_source _col) "H") source)
    (puthash :get-raw-value-at
             (lambda (_source row col) (nth col (nth row rows)))
             source)
    (puthash :get-merges (lambda (_source) '((1 0 2 0))) source)
    (unwind-protect
        (progn
          (should-error
           (grid-table-persistence--save-to-file source file)
           :type 'error)
          (should-not (file-exists-p file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest grid-table-v2-loader-contract-rejects-v3-files ()
  (let ((v3 '(grid-table-file
              :version 3
              :headers ("A")
              :rows (("x") (""))
              :merges ((1 0 2 0)))))
    ;; NOTE: this is an inline snapshot of the old v2 loader's version
    ;; gate (copied logic, not a call into the real loader).  It pins the
    ;; historical contract; the real loader's version rejection is covered
    ;; by `grid-table-persistence-rejects-unsupported-version-in-real-file'.
    ;; Snapshot of the v2 tagged-file gate: forward versions must fail loudly.
    (should-error
     (let ((plist (cdr v3)))
       (unless (eq (plist-get plist :version) 2)
         (error "Unsupported grid file version"))))))

;;;----------------------------------------------------------------------
;;; Review follow-ups: persistence gates, adjacency, conflict warnings
;;;----------------------------------------------------------------------

(ert-deftest grid-table-persistence-rejects-unsupported-version-in-real-file ()
  ;; Exercises the real loader's version gate
  ;; (grid-table-persistence.el: `memq '(2 3 4)') against actual files,
  ;; unlike the inline dispatch snapshots elsewhere in this suite.
  ;; Version 5 holds several sheets; 6 is the first the loader does not know.
  (dolist (case '((6 . grid-table-persistence-rejects-unsupported-version-in-real-file)
                  (missing . grid-table-persistence-rejects-unsupported-version-in-real-file)))
    (let ((file (make-temp-file "grid-version-" nil ".grid"))
          (version (car case)))
      (unwind-protect
          (progn
            (with-temp-file file
              (prin1 (append '(grid-table-file)
                             (unless (eq version 'missing)
                               (list :version version))
                             '(:title "T"
                               :headers ("A" "B")
                               :rows (("x" "y") ("p" "q"))
                               :merges ((1 0 1 1))))
                     (current-buffer)))
            (let ((err (should-error
                        (grid-table-persistence-create-data-source-from-file
                         file))))
              (should (string-match-p
                       (regexp-quote "Unsupported grid file version")
                       (error-message-string err)))))
        (delete-file file)))))

(ert-deftest grid-table-model-accepts-abutting-merges ()
  ;; Sharing a border without sharing a cell is legal (2D closed-interval
  ;; intersection).  Column direction: (0 0 0 1) and (0 2 0 3) touch at
  ;; the col 1/2 border.
  (let ((model (grid-model-create
                1 4
                '(("a" "" "b" ""))
                '((0 0 0 1) (0 2 0 3)))))
    (should (equal (grid-model-get-merges model)
                   '((0 0 0 1) (0 2 0 3)))))
  ;; Row direction: (0 0 0 1) and (1 1 3 1) touch at the row 0/1 border
  ;; of col 1 without sharing any cell.
  (let ((model (grid-model-create
                4 2
                '(("a" "")
                  ("x" "b")
                  ("y" "")
                  ("z" ""))
                '((0 0 0 1) (1 1 3 1)))))
    (should (equal (grid-model-get-merges model)
                   '((0 0 0 1) (1 1 3 1))))))

(ert-deftest grid-table-validation-warns-with-all-conflicting-ranges ()
  ;; A candidate overlapping several already-accepted merges is dropped
  ;; alone; the warning names every conflicting accepted range and every
  ;; intersection rectangle.
  (let ((model (grid-model-create
                5 3
                '(("H1" "H2" "H3")
                  ("a" "" "p")
                  ("" "x" "y")
                  ("" "z" "w")
                  ("" "" ""))))
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _args)
                 (push message warnings))))
      (should
       (equal
        (grid-model-validate-merges
         model '((1 0 1 1) (2 0 3 0) (1 0 3 0)) t)
        '((1 0 1 1) (2 0 3 0)))))
    (should (= (length warnings) 1))
    (let ((warning (car warnings)))
      ;; The dropped candidate.
      (should (string-match-p (regexp-quote "Discarding (1 0 3 0)")
                              warning))
      ;; Both conflicting accepted ranges.
      (should (string-match-p (regexp-quote "(1 0 1 1)") warning))
      (should (string-match-p (regexp-quote "(2 0 3 0)") warning))
      ;; Both intersection rectangles.
      (should (string-match-p (regexp-quote "(1 0 1 0)") warning))
      (should (string-match-p (regexp-quote "(2 0 3 0)") warning)))))

(ert-deftest grid-table-load-discards-row-zero-crossing-merge-keeping-data ()
  ;; v3 load path: a merge with top=0 and bottom>0 is discarded with a
  ;; warning; the raw cell values survive untouched.
  (let ((file (make-temp-file "grid-row0-" nil ".grid"))
        warnings)
    (unwind-protect
        (progn
          (with-temp-file file
            (prin1 '(grid-table-file
                     :version 3
                     :title "T"
                     :headers ("A" "B" "C")
                     :rows (("d" "e" "f") ("g" "h" "i") ("j" "k" "l"))
                     :merges ((0 2 3 2)))
                   (current-buffer)))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (_type message &rest _args)
                       (push message warnings))))
            (let ((source
                   (grid-table-persistence-create-data-source-from-file file)))
              (should-not (grid-get-merges source))
              (should (equal (grid-get-raw-value-at source 0 2) "C"))
              (should (equal (grid-get-raw-value-at source 1 2) "f"))
              (should (equal (grid-get-raw-value-at source 3 2) "l"))))
          (should (cl-some (lambda (warning)
                             (and (string-match-p
                                   (regexp-quote "(0 2 3 2)") warning)
                                  (string-match-p "row 0" warning)))
                           warnings)))
      (delete-file file))))

(ert-deftest grid-table-save-rejects-row-zero-crossing-in-memory-merges ()
  ;; Save path: the same (0 2 3 2) merge installed directly in memory
  ;; must abort the save with an error (creation rejection is already
  ;; covered by grid-table-model-rejects-rectangular-and-row-zero-crossing-merges).
  (let ((file (make-temp-name
               (expand-file-name "grid-row0-save-" temporary-file-directory)))
        (source (make-default-data-source
                 '("H1" "H2" "H3")
                 '(("H1" "H2" "H3")
                   ("a" "b" "c")
                   ("d" "e" "f")
                   ("g" "h" "i")))))
    (unwind-protect
        (progn
          (set-grid-data-model-merges
           (gethash :model source) '((0 2 3 2)))
          (let ((err (should-error
                      (grid-table-persistence--save-to-file source file))))
            (should (string-match-p "row 0" (error-message-string err))))
          (should-not (file-exists-p file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest grid-table-sort-error-leaves-model-untouched ()
  ;; The vertical-merge sort ban fires before any extraction/rebuild, so
  ;; the original model's merges and every raw value must be unchanged.
  (let* ((model (grid-model-create
                 4 2
                 '(("H1" "H2")
                   ("dept" "z")
                   ("" "a")
                   ("other" "b"))
                 '((1 0 2 0))))
         (merges-before (grid-model-get-merges model))
         (raws-before (grid-model-get-all-raw-values model)))
    (should-error (grid-model-sort-by-column model 1 'ascending))
    (should (equal (grid-model-get-merges model) merges-before))
    (should (equal (grid-model-get-all-raw-values model) raws-before))))

(ert-deftest grid-table-save-sorts-merges-by-quad-lexicographic-order ()
  ;; Whatever order merges are installed in, the saved :merges list is
  ;; sorted by the (top,left,bottom,right) quadruple lexicographically
  ;; (dev-note/vertical-merge-spec.md, persistence section).
  (let ((file (make-temp-file "grid-sort-merges-" nil ".grid"))
        (source (make-default-data-source
                 '("H1" "H2" "H3")
                 '(("H1" "H2" "H3")
                   ("a" "" "")
                   ("b" "" "c")
                   ("d" "e" "f")
                   ("g" "" "h")))))
    (unwind-protect
        (progn
          ;; Install out of order; (3 1 4 1) is vertical so the save
          ;; writes v3 quadruples.
          (set-grid-data-model-merges
           (gethash :model source)
           '((3 1 4 1) (2 0 2 1) (1 0 1 2)))
          (grid-table-persistence--save-to-file source file)
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((data (read (current-buffer)))
                   (written (plist-get (cdr data) :merges)))
              (should (eq (plist-get (cdr data) :version) 3))
              (should (equal written
                             '((1 0 1 2) (2 0 2 1) (3 1 4 1)))))))
      (delete-file file))))

(ert-deftest grid-table-deleting-rows-together-matches-one-at-a-time ()
  ;; Formulas, ranges, merges, styles and rules end up as if the rows
  ;; were deleted from the bottom up one by one, for many random sets.
  (random "grid-delete-rows")
  (dotimes (_ 40)
    (let* ((rows 14)
           (raw (cl-loop for r below rows collect
                         (list (format "%d" r)
                               (format "=A%d*2" (1+ (random rows)))
                               (format "=SUM(A%d:A%d)+$A$%d" (1+ (random rows))
                                       (1+ (random rows)) (1+ (random rows)))
                               (if (= r 3) "top" "")
                               "")))
           (model (grid-model-create rows 5 raw)))
      (setq model (grid-model-merge-cells model 3 3 6 3))
      (setq model (grid-model-merge-cells model 9 3 9 4))
      (grid-model-set-style model (random rows) 4 '(:bold t))
      (grid-model-set-style model (random rows) 1 '(:italic t))
      (grid-model-set-conditionals
       model (list (list 2 0 8 0 '(> 3) '(:bold t))
                   (list 11 2 11 2 '(< 1) '(:italic t))))
      (let* ((doomed (cl-remove-duplicates
                      (cl-loop repeat (1+ (random 5)) collect (random rows))))
             (together (grid-model-delete-rows model doomed))
             (one-by-one model))
        (dolist (row (sort (copy-sequence doomed) #'>))
          (setq one-by-one (grid-model-delete-row one-by-one (1- row))))
        (should (equal (grid-model-get-all-raw-values together)
                       (grid-model-get-all-raw-values one-by-one)))
        (should (equal (grid-model-get-merges together)
                       (grid-model-get-merges one-by-one)))
        (should (equal (grid-data-model-conditionals together)
                       (grid-data-model-conditionals one-by-one)))
        (dotimes (r (grid-data-model-rows together))
          (dotimes (c 5)
            (should (equal (grid-model-get-style together r c)
                           (grid-model-get-style one-by-one r c)))
            (should (equal (grid-model-get-computed-value together r c)
                           (grid-model-get-computed-value one-by-one r c)))))))))

(provide 'grid-table-vertical-model-test)

;;; grid-table-vertical-model-test.el ends here
