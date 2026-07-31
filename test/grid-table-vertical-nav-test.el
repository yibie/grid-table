;;; grid-table-vertical-nav-test.el --- Vertical merge navigation tests -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'grid-table)

(defun grid-table-vertical-nav-test--source (&optional hooks)
  "Return a minimal data source for synthetic navigation buffers.
HOOKS is an alist keyed by :set-raw-value-at, :add-row or :unmerge-cell."
  (let ((source (make-hash-table)))
    (puthash :get-row-count (lambda (_s) 10) source)
    (puthash :get-column-count (lambda (_s) 3) source)
    (puthash :get-raw-value-at
             (lambda (_s row col)
               (when-let* ((hook (alist-get :get-raw-value-at hooks)))
                 (funcall hook row col))
               "mix")
             source)
    (puthash :set-raw-value-at
             (lambda (_s row col value)
               (funcall (alist-get :set-raw-value-at hooks) row col value)
               t)
             source)
    (puthash :add-row
             (lambda (s row)
               (funcall (alist-get :add-row hooks) row)
               s)
             source)
    (puthash :unmerge-cell
             (lambda (s row col)
               (funcall (alist-get :unmerge-cell hooks) row col)
               s)
             source)
    source))

(defun grid-table-vertical-nav-test--segment
    (text row-start row-end col-start col-end slice-row)
  "Return TEXT carrying the future vertical-merge navigation properties."
  (propertize text
              'grid-cell t
              'grid-display-row-start row-start
              'grid-display-row-end row-end
              'grid-display-col-start col-start
              'grid-display-col-end col-end
              'grid-display-slice-row slice-row
              'grid-display-slice-col col-start))

(defun grid-table-vertical-nav-test--ordinary (text row col)
  "Return TEXT as an ordinary synthetic cell."
  (grid-table-vertical-nav-test--segment text row row col col row))

(defun grid-table-vertical-nav-test--insert-synthetic-table ()
  "Insert enough synthetic table fragments to exercise vertical navigation."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (grid-table-vertical-nav-test--ordinary "#" -1 0) " "
            (grid-table-vertical-nav-test--ordinary "A" -1 1) " "
            (grid-table-vertical-nav-test--ordinary "B" -1 2) " "
            (grid-table-vertical-nav-test--ordinary "C" -1 3) "\n")
    (insert (grid-table-vertical-nav-test--ordinary "1" 0 0) " "
            (grid-table-vertical-nav-test--ordinary "A1" 0 1) " "
            (grid-table-vertical-nav-test--ordinary "B1" 0 2) " "
            (grid-table-vertical-nav-test--ordinary "C1" 0 3) "\n")
    (insert (grid-table-vertical-nav-test--ordinary "2" 1 0) " "
            (grid-table-vertical-nav-test--ordinary "A2" 1 1) " "
            (grid-table-vertical-nav-test--segment "mix-top" 1 8 2 2 1) " "
            (grid-table-vertical-nav-test--ordinary "C2" 1 3) "\n")
    (insert (grid-table-vertical-nav-test--ordinary "4" 3 0) " "
            (grid-table-vertical-nav-test--ordinary "A4" 3 1) " "
            (grid-table-vertical-nav-test--segment "      " 1 8 2 2 3) " "
            (grid-table-vertical-nav-test--ordinary "C4" 3 3) "\n")
    (insert (grid-table-vertical-nav-test--ordinary "5" 4 0) " "
            (grid-table-vertical-nav-test--ordinary "A5" 4 1) " "
            (grid-table-vertical-nav-test--segment "mix-slice-5" 1 8 2 2 4) " "
            (grid-table-vertical-nav-test--ordinary "C5" 4 3) "\n")
    (insert (grid-table-vertical-nav-test--ordinary "9" 8 0) " "
            (grid-table-vertical-nav-test--ordinary "A9" 8 1) " "
            (grid-table-vertical-nav-test--segment "mix-bottom" 1 8 2 2 8) " "
            (grid-table-vertical-nav-test--ordinary "C9" 8 3) "\n")
    (insert (grid-table-vertical-nav-test--ordinary "10" 9 0) " "
            (grid-table-vertical-nav-test--ordinary "A10" 9 1) " "
            (grid-table-vertical-nav-test--ordinary "B10" 9 2) " "
            (grid-table-vertical-nav-test--ordinary "C10" 9 3) "\n")))

(defmacro grid-table-vertical-nav-test--with-synthetic-buffer (&rest body)
  "Run BODY in a grid-table buffer with synthetic future text properties."
  (declare (indent 0))
  `(with-temp-buffer
     (grid-table-mode)
     (setq-local grid-table--data-source
                 (grid-table-vertical-nav-test--source))
     (grid-table-vertical-nav-test--insert-synthetic-table)
     (goto-char (point-min))
     ,@body))

(defun grid-table-vertical-nav-test--goto-text (text)
  "Move point to TEXT in the current buffer."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(ert-deftest grid-table-get-cell-at-point-returns-vertical-anchor-and-slice ()
  (grid-table-vertical-nav-test--with-synthetic-buffer
    (grid-table-vertical-nav-test--goto-text "mix-slice-5")
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) 1))
      (should (= (plist-get coords :row-end) 8))
      (should (= (plist-get coords :col-start) 2))
      (should (= (plist-get coords :col-end) 2))
      (should (= (plist-get coords :anchor-row) 1))
      (should (= (plist-get coords :anchor-col) 2))
      (should (= (plist-get coords :slice-row) 4))
      (should (= (plist-get coords :slice-col) 2)))))

(ert-deftest grid-table-up-enters-bottom-slice-and-right-exits-same-row ()
  (grid-table-vertical-nav-test--with-synthetic-buffer
    (grid-table-vertical-nav-test--goto-text "B10")
    (grid-table-previous-line)
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) 1))
      (should (= (plist-get coords :row-end) 8))
      (should (= (plist-get coords :slice-row) 8)))
    (grid-table-cell-right)
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) 8))
      (should (= (plist-get coords :col-start) 3)))))

(ert-deftest grid-table-vertical-up-down-skip-the-whole-span ()
  (grid-table-vertical-nav-test--with-synthetic-buffer
    (grid-table-vertical-nav-test--goto-text "mix-slice-5")
    (grid-table-next-line)
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) 9))
      (should (= (plist-get coords :col-start) 2)))
    (grid-table-previous-line)
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) 1))
      (should (= (plist-get coords :row-end) 8))
      (should (= (plist-get coords :slice-row) 8)))))

(ert-deftest grid-table-tab-enters-vertical-merge-at-anchor ()
  (grid-table-vertical-nav-test--with-synthetic-buffer
    (grid-table-vertical-nav-test--goto-text "A2")
    (grid-table-next-cell)
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) 1))
      (should (= (plist-get coords :row-end) 8))
      (should (= (plist-get coords :slice-row) 1)))))

(ert-deftest grid-table-blank-padding-slice-is-navigable ()
  (grid-table-vertical-nav-test--with-synthetic-buffer
    (should (grid-table--move-to-cell 3 2))
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) 1))
      (should (= (plist-get coords :row-end) 8))
      (should (= (plist-get coords :slice-row) 3)))))

(ert-deftest grid-table-edit-on-vertical-slice-writes-anchor-cell ()
  (let (written)
    (with-temp-buffer
      (grid-table-mode)
      (setq-local grid-table--data-source
                  (grid-table-vertical-nav-test--source
                   `((:set-raw-value-at
                      . ,(lambda (row col value)
                           (setq written (list row col value)))))))
      (grid-table-vertical-nav-test--insert-synthetic-table)
      (grid-table-vertical-nav-test--goto-text "mix-slice-5")
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "edited"))
                ((symbol-function 'grid-table-refresh) #'ignore)
                ((symbol-function 'grid-table--highlight-cell) #'ignore))
        (grid-table-edit-cell))
      (should (equal written '(1 1 "edited"))))))

(ert-deftest grid-table-insert-row-on-vertical-slice-uses-slice-row ()
  (let (inserted-row)
    (with-temp-buffer
      (grid-table-mode)
      (setq-local grid-table--data-source
                  (grid-table-vertical-nav-test--source
                   `((:add-row . ,(lambda (row) (setq inserted-row row))))))
      (grid-table-vertical-nav-test--insert-synthetic-table)
      (grid-table-vertical-nav-test--goto-text "mix-slice-5")
      (cl-letf (((symbol-function 'grid-table-refresh) #'ignore))
        (grid-table-insert-row-above))
      (should (= inserted-row 4)))))

(ert-deftest grid-table-unmerge-on-vertical-slice-uses-anchor-cell ()
  (let (unmerged)
    (with-temp-buffer
      (grid-table-mode)
      (setq-local grid-table--data-source
                  (grid-table-vertical-nav-test--source
                   `((:unmerge-cell
                      . ,(lambda (row col)
                           (setq unmerged (list row col)))))))
      (grid-table-vertical-nav-test--insert-synthetic-table)
      (grid-table-vertical-nav-test--goto-text "mix-slice-5")
      (cl-letf (((symbol-function 'grid-table-refresh) #'ignore))
        (grid-table-unmerge-cell))
      (should (equal unmerged '(1 1))))))

(ert-deftest grid-table-refresh-preserves-vertical-slice-coordinate ()
  (let* ((source (make-default-data-source
                  '("A" "B" "C")
                  '(("A" "B" "C")
                    ("a1" "mix" "c1")
                    ("a2" "" "c2")
                    ("a3" "" "c3")
                    ("a4" "" "c4")
                    ("a5" "" "c5")
                    ("a6" "" "c6")
                    ("a7" "" "c7")
                    ("a8" "" "c8")
                    ("a9" "b9" "c9")))))
    (set-grid-data-model-merges (gethash :model source) '((1 1 8 1)))
    (with-temp-buffer
      (grid-table-mode)
      (grid-table--show-grid-in-current-buffer source)
      (should (grid-table--move-to-cell 4 1))
      (grid-table-cell-right)
      (should (= (plist-get (grid-table-get-cell-at-point) :slice-row) 4))
      (grid-table-refresh)
      (should (= (plist-get (grid-table-get-cell-at-point) :slice-row) 4))
      (grid-table-cell-right)
      (let ((coords (grid-table-get-cell-at-point)))
        (should (= (plist-get coords :row-start) 4))
        (should (= (plist-get coords :col-start) 3))))))

(ert-deftest grid-table-row-header-and-abc-navigation-ignore-vertical-merges ()
  (grid-table-vertical-nav-test--with-synthetic-buffer
    (grid-table-vertical-nav-test--goto-text "1")
    (grid-table-previous-line)
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) -1))
      (should (= (plist-get coords :col-start) 0)))
    (grid-table-previous-line)
    (let ((coords (grid-table-get-cell-at-point)))
      (should (= (plist-get coords :row-start) 9))
      (should (= (plist-get coords :col-start) 0)))))

(ert-deftest grid-table-highlight-covers-every-vertical-slice ()
  (let ((source
         (make-default-data-source
          '("A" "B")
          '(("A" "B")
            ("left1" "mix")
            ("left2" "")
            ("left3" ""))
          nil
          '((1 1 3 1)))))
    (with-temp-buffer
      (grid-table-mode)
      (grid-table--show-grid-in-current-buffer source)
      (grid-table--highlight-cell 2 2)
      (should (listp grid-table--highlight-overlay))
      (should (> (length grid-table--highlight-overlay) 3))
      (should (cl-every #'overlayp grid-table--highlight-overlay)))))

(ert-deftest grid-table-merge-command-accepts-one-axis-ranges-only ()
  (let ((source
         (make-default-data-source
          '("A" "B")
          '(("A" "B")
            ("a1" "mix")
            ("a2" "")
            ("a3" "")))))
    (with-temp-buffer
      (grid-table-mode)
      (grid-table--show-grid-in-current-buffer source)
      (grid-table-merge-cells "B2:B4")
      (should (equal (grid-get-merges grid-table--data-source)
                     '((1 1 3 1))))
      (should-error (grid-table-merge-cells "A1:B2")
                    :type 'user-error))))

(ert-deftest grid-table-structural-commands-handle-invalid-coordinates ()
  (with-temp-buffer
    (grid-table-mode)
    (setq-local grid-table--data-source (make-hash-table))
    (should-not
     (condition-case nil
         (progn
           (grid-table-add-row nil)
           (grid-table-delete-row nil)
           (grid-table-delete-row 0)
           (grid-table-add-column nil)
           (grid-table-delete-column nil)
           nil)
       (error t)))))

(provide 'grid-table-vertical-nav-test)

;;; grid-table-vertical-nav-test.el ends here
