;;; exc-render-characterization-test.el --- Renderer characterization tests -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'exc)

;; These tests characterize the classic renderer.

(defun exc-render-test--render (source)
  "Return SOURCE rendered as plain buffer text."
  (with-temp-buffer
    (insert (exc-text-render source))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun exc-render-test--source (headers rows &optional merges)
  "Return a default source for render characterization."
  (make-exc-default-data-source headers rows nil merges))

(defun exc-render-test--computed-source (headers rows)
  "Return a minimal source whose computed values may be arbitrary objects."
  (let ((source (make-hash-table)))
    (puthash :get-column-count (lambda (_source) (length headers)) source)
    (puthash :get-row-count (lambda (_source) (length rows)) source)
    (puthash :get-header-value
             (lambda (_source col) (nth col headers))
             source)
    (puthash :get-computed-value-at
             (lambda (_source row col) (nth col (nth row rows)))
             source)
    (puthash :get-merges (lambda (_source) nil) source)
    source))

(defun exc-render-test--property-ranges (source)
  "Return contiguous `exc-cell' text property ranges after rendering SOURCE."
  (with-temp-buffer
    (insert (exc-text-render source))
    (let ((pos (point-min))
          ranges)
      (while (< pos (point-max))
        (let* ((next (or (next-single-property-change
                          pos 'exc-cell nil (point-max))
                         (point-max)))
               (cell (get-text-property pos 'exc-cell)))
          (when cell
            (push (list pos
                        next
                        (buffer-substring-no-properties pos next)
                        (get-text-property pos 'exc-display-row-idx)
                        (get-text-property pos 'exc-display-col-idx)
                        (get-text-property pos
                                           'exc-display-col-end-idx))
                  ranges))
          (setq pos next)))
      (nreverse ranges))))

(ert-deftest exc-render-outputs-exact-full-ordinary-table ()
  (should
   (equal
    (exc-render-test--render
     (exc-render-test--source
      '("Name" "Qty")
      '(("Name" "Qty")
        ("Pen" "12"))))
    "┌───┬───────┬───────┐
│   │ A     │ B     │
├───┼───────┼───────┤
│1  │ Name  │ Qty   │
├───┼───────┼───────┤
│2  │ Pen   │ 12    │
└───┴───────┴───────┘
")))

(ert-deftest exc-computed-width-matches-rendered-table ()
  (let ((source
         (exc-render-test--source
          '("Name" "Qty")
          '(("Name" "Qty") ("Pen" "12")))))
    (should (= (exc--compute-table-width source) 21))))

(ert-deftest exc-render-keeps-explicit-newlines-and-variable-row-height ()
  (should
   (equal
    (exc-render-test--render
     (exc-render-test--source
      '("A" "B")
      '(("Line one\nLine two" "wrap words here now")
        ("x" "y"))))
    "┌───┬──────────┬─────────────────────┐
│   │ A        │ B                   │
├───┼──────────┼─────────────────────┤
│1  │ Line one │ wrap words here now │
│   │ Line two │                     │
├───┼──────────┼─────────────────────┤
│2  │ x        │ y                   │
└───┴──────────┴─────────────────────┘
")))

(ert-deftest exc-render-wraps-at-the-forty-column-limit ()
  (should
   (equal
    (exc-render-test--render
     (exc-render-test--source
      '("A")
      '(("This sentence is deliberately longer than forty characters so it wraps.")
        ("short"))))
    "┌───┬──────────────────────────────────────────┐
│   │ A                                        │
├───┼──────────────────────────────────────────┤
│1  │ This sentence is deliberately longer     │
│   │ than forty characters so it wraps.       │
├───┼──────────────────────────────────────────┤
│2  │ short                                    │
└───┴──────────────────────────────────────────┘
")))

(ert-deftest exc-render-image-slices-have-fixed-character-height ()
  (let ((source
         (exc-render-test--source
          '("Img") '(("image") ("tail"))))
        (exc-image-target-char-height 3))
    (cl-letf (((symbol-function 'exc--extract-image-path)
               (lambda (text)
                 (and (equal text "image") "/stub/image.png")))
              ((symbol-function 'exc--get-loadable-image-source)
               (lambda (&rest _) '("/stub/image.png" png)))
              ((symbol-function 'create-image)
               (lambda (&rest _) 'fake-image))
              ((symbol-function 'insert-sliced-image)
               (lambda (&rest _) (insert "s1\ns2\ns3"))))
      (should
       (equal
        (exc-render-test--render source)
        "┌───┬──────────────────────┐
│   │ A                    │
├───┼──────────────────────┤
│1  │ s1                   │
│   │ s2                   │
│   │ s3                   │
├───┼──────────────────────┤
│2  │ tail                 │
└───┴──────────────────────┘
")))))

(ert-deftest exc-render-draws-horizontal-merge-t-junctions-between-ordinary-rows ()
  (should
   (equal
    (exc-render-test--render
     (exc-render-test--source
      '("A" "B" "C" "D")
      '(("top-a" "top-b" "top-c" "top-d")
        ("wide" "" "" "tail")
        ("bot-a" "bot-b" "bot-c" "bot-d"))
      '((1 0 1 2))))
    "┌───┬───────┬───────┬───────┬───────┐
│   │ A     │ B     │ C     │ D     │
├───┼───────┼───────┼───────┼───────┤
│1  │ top-a │ top-b │ top-c │ top-d │
├───┼───────┴───────┴───────┼───────┤
│2  │ wide                  │ tail  │
├───┼───────┬───────┬───────┼───────┤
│3  │ bot-a │ bot-b │ bot-c │ bot-d │
└───┴───────┴───────┴───────┴───────┘
")))

(ert-deftest exc-render-draws-one-row-with-multiple-horizontal-merges ()
  (should
   (equal
    (exc-render-test--render
     (exc-render-test--source
      '("A" "B" "C" "D" "E")
      '(("left" "" "mid" "" "right"))
      '((0 0 0 1) (0 2 0 3))))
    "┌───┬───────┬───────┬───────┬───────┬───────┐
│   │ A     │ B     │ C     │ D     │ E     │
├───┼───────┴───────┼───────┴───────┼───────┤
│1  │ left          │ mid           │ right │
└───┴───────────────┴───────────────┴───────┘
")))

(ert-deftest exc-render-chart-object-as-one-line-placeholder-with-current-width ()
  (let ((chart (list :exc-chart t :type 'bar :data nil :options nil)))
    (should
     (equal
      (exc-render-test--render
       (exc-render-test--computed-source
        '("Kind" "Viz")
        (list (list "Chart row" chart))))
      "┌───┬───────────┬──────────────────────────────────────────┐
│   │ A         │ B                                        │
├───┼───────────┼──────────────────────────────────────────┤
│1  │ Chart row │ [Chart]                                  │
└───┴───────────┴──────────────────────────────────────────┘
"))))

(ert-deftest exc-render-applies-grid-cell-properties-to-cell-interiors ()
  (should
   (equal
    (exc-render-test--property-ranges
     (exc-render-test--source
      '("Name" "Qty")
      '(("Name" "Qty")
        ("Pen" "12"))))
    '((24 27 "   " -1 0 0)
      (28 35 " A     " -1 1 1)
      (36 43 " B     " -1 2 2)
      (68 71 "1  " 0 0 0)
      (72 79 " Name  " 0 1 1)
      (80 87 " Qty   " 0 2 2)
      (112 115 "2  " 1 0 0)
      (116 123 " Pen   " 1 1 1)
      (124 131 " 12    " 1 2 2)))))

(provide 'exc-render-characterization-test)

;;; exc-render-characterization-test.el ends here

(ert-deftest exc-org-preview-heads-the-table-once ()
  "The Org preview puts the header row above the rule, and not again below."
  (require 'exc-org)
  (let ((file (make-temp-file "exc-org-" nil ".grid")))
    (unwind-protect
        (let ((source (make-exc-default-data-source
                       '("Region" "Total")
                       '(("Region" "Total") ("North" "10") ("South" "20")))))
          (exc-persistence--save-to-file source file)
          (let ((preview (exc-org--render-preview-from-file file)))
            (should (= (exc-test--occurrences "Region" preview) 1))
            (should (string-match-p "\\`| Region +| Total +|\n|-" preview))
            (should (string-match-p "| North +| 10 +|" preview))
            (should (string-match-p "| South +| 20 +|" preview))))
      (delete-file file))))

(defun exc-test--occurrences (needle haystack)
  "Return how many times NEEDLE occurs in HAYSTACK."
  (let ((count 0) (start 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun exc-test--block-file ()
  "Save a small grid in a directory with a space and return its file."
  (let ((file (expand-file-name "my table.grid" (make-temp-file "exc-block-" t))))
    (exc-persistence--save-to-file
     (make-exc-default-data-source '("Name" "Qty") '(("Name" "Qty") ("a" "1") ("b" "=B2*2")))
     file)
    file))

(defun exc-test--block-round-trip (setup insert refresh open)
  "Insert a block, edit its file, refresh it and open it; return what happened.
SETUP prepares the buffer; INSERT, REFRESH and OPEN are the plugin's commands.
The result is (TEXT-AFTER-INSERT TEXT-AFTER-REFRESH OPENED-FILE FILE)."
  (let ((file (exc-test--block-file)) opened)
    (unwind-protect
        (with-temp-buffer
          (funcall setup)
          (insert "Before\n")
          (funcall insert file)
          (insert "After\n")
          (let ((inserted (buffer-string)))
            (exc-persistence--save-to-file
             (make-exc-default-data-source '("Name" "Qty") '(("Name" "Qty") ("a" "1") ("c" "7")))
             file)
            (goto-char (point-min))
            (re-search-forward "[|│] a ")
            (funcall refresh)
            (cl-letf (((symbol-function 'exc-find-file)
                       (lambda (f) (setq opened f))))
              (funcall open))
            (list inserted (buffer-string) opened file)))
      (delete-directory (file-name-directory file) t))))

(ert-deftest exc-org-block-inserts-refreshes-and-opens ()
  "An inserted Org block can be refreshed and opened, even with a space in its path."
  (require 'exc-org)
  (pcase-let ((`(,inserted ,refreshed ,opened ,file)
               (exc-test--block-round-trip
                #'org-mode #'exc-org-insert-block
                #'exc-org-refresh-block #'exc-org-open-block)))
    (should (string-match-p
             (concat "^#\\+BEGIN_SRC grid-table :file \"" (regexp-quote file) "\"$")
             inserted))
    (should (string-match-p "| b +| 2 +|\n#\\+END_SRC\nAfter\n\\'" inserted))
    (should (string-match-p "^#\\+BEGIN_SRC grid-table :file " refreshed))
    (should (string-match-p "| c +| 7 +|\n#\\+END_SRC\nAfter\n\\'" refreshed))
    (should-not (string-match-p "| b +| 2 +|" refreshed))
    (should (equal opened file))))

(ert-deftest exc-markdown-block-inserts-refreshes-and-opens ()
  "An inserted Markdown block can be refreshed and opened, even with a space in its path."
  (require 'exc-markdown)
  (pcase-let ((`(,inserted ,refreshed ,opened ,file)
               (exc-test--block-round-trip
                #'ignore #'exc-markdown-insert-block
                #'exc-markdown-refresh-block #'exc-markdown-open-block)))
    (should (string-match-p
             (concat "^```grid-view file=\"" (regexp-quote file) "\"$")
             inserted))
    (should (string-match-p "\n```\nAfter\n\\'" inserted))
    (should (string-match-p "│ c +│ 7 +│" refreshed))
    (should-not (string-match-p "│ b +│ 2 +│" refreshed))
    (should (string-match-p "\n```\nAfter\n\\'" refreshed))
    (should (equal opened file))))
