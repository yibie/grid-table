;;; grid-table-render-characterization-test.el --- Renderer characterization tests -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'grid-table)

;; These tests characterize the classic renderer.
(setq grid-table-renderer 'classic)

(defun grid-table-render-test--render (source)
  "Return SOURCE rendered as plain buffer text."
  (with-temp-buffer
    (grid-table-mode)
    (grid-table--show-grid-in-current-buffer source)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun grid-table-render-test--source (headers rows &optional merges)
  "Return a default source for render characterization."
  (make-default-data-source headers rows nil merges))

(defun grid-table-render-test--computed-source (headers rows)
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

(defun grid-table-render-test--property-ranges (source)
  "Return contiguous `grid-cell' text property ranges after rendering SOURCE."
  (with-temp-buffer
    (grid-table-mode)
    (grid-table--show-grid-in-current-buffer source)
    (let ((pos (point-min))
          ranges)
      (while (< pos (point-max))
        (let* ((next (or (next-single-property-change
                          pos 'grid-cell nil (point-max))
                         (point-max)))
               (cell (get-text-property pos 'grid-cell)))
          (when cell
            (push (list pos
                        next
                        (buffer-substring-no-properties pos next)
                        (get-text-property pos 'grid-display-row-idx)
                        (get-text-property pos 'grid-display-col-idx)
                        (get-text-property pos
                                           'grid-display-col-end-idx))
                  ranges))
          (setq pos next)))
      (nreverse ranges))))

(ert-deftest grid-table-render-outputs-exact-full-ordinary-table ()
  (should
   (equal
    (grid-table-render-test--render
     (grid-table-render-test--source
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

(ert-deftest grid-table-computed-width-matches-rendered-table ()
  (let ((source
         (grid-table-render-test--source
          '("Name" "Qty")
          '(("Name" "Qty") ("Pen" "12")))))
    (should (= (grid-table--compute-table-width source) 21))))

(ert-deftest grid-table-render-keeps-explicit-newlines-and-variable-row-height ()
  (should
   (equal
    (grid-table-render-test--render
     (grid-table-render-test--source
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

(ert-deftest grid-table-render-wraps-at-the-forty-column-limit ()
  (should
   (equal
    (grid-table-render-test--render
     (grid-table-render-test--source
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

(ert-deftest grid-table-render-image-slices-have-fixed-character-height ()
  (let ((source
         (grid-table-render-test--source
          '("Img") '(("image") ("tail"))))
        (grid-table-image-target-char-height 3))
    (cl-letf (((symbol-function 'grid-table--extract-image-path)
               (lambda (text)
                 (and (equal text "image") "/stub/image.png")))
              ((symbol-function 'grid-table--get-loadable-image-source)
               (lambda (&rest _) '("/stub/image.png" png)))
              ((symbol-function 'create-image)
               (lambda (&rest _) 'fake-image))
              ((symbol-function 'insert-sliced-image)
               (lambda (&rest _) (insert "s1\ns2\ns3"))))
      (should
       (equal
        (grid-table-render-test--render source)
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

(ert-deftest grid-table-render-draws-horizontal-merge-t-junctions-between-ordinary-rows ()
  (should
   (equal
    (grid-table-render-test--render
     (grid-table-render-test--source
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

(ert-deftest grid-table-render-draws-one-row-with-multiple-horizontal-merges ()
  (should
   (equal
    (grid-table-render-test--render
     (grid-table-render-test--source
      '("A" "B" "C" "D" "E")
      '(("left" "" "mid" "" "right"))
      '((0 0 0 1) (0 2 0 3))))
    "┌───┬───────┬───────┬───────┬───────┬───────┐
│   │ A     │ B     │ C     │ D     │ E     │
├───┼───────┴───────┼───────┴───────┼───────┤
│1  │ left          │ mid           │ right │
└───┴───────────────┴───────────────┴───────┘
")))

(ert-deftest grid-table-render-chart-object-as-one-line-placeholder-with-current-width ()
  (let ((chart (list :grid-chart t :type 'bar :data nil :options nil)))
    (should
     (equal
      (grid-table-render-test--render
       (grid-table-render-test--computed-source
        '("Kind" "Viz")
        (list (list "Chart row" chart))))
      "┌───┬───────────┬──────────────────────────────────────────┐
│   │ A         │ B                                        │
├───┼───────────┼──────────────────────────────────────────┤
│1  │ Chart row │ [Chart]                                  │
└───┴───────────┴──────────────────────────────────────────┘
"))))

(ert-deftest grid-table-render-applies-grid-cell-properties-to-cell-interiors ()
  (should
   (equal
    (grid-table-render-test--property-ranges
     (grid-table-render-test--source
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

(provide 'grid-table-render-characterization-test)

;;; grid-table-render-characterization-test.el ends here
