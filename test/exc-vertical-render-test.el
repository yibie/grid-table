;;; exc-vertical-render-test.el --- Vertical merge render tests -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'exc)

;; These tests characterize the classic renderer.

(defun exc-vertical-test--source (headers rows merges)
  "Return a source for vertical merge rendering tests."
  (make-exc-default-data-source headers rows nil merges))

(defun exc-vertical-test--render (source)
  "Return SOURCE rendered as plain buffer text."
  (with-temp-buffer
    (insert (exc-text-render source))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun exc-vertical-test--computed-source (headers rows merges)
  "Return a render source whose ROWS may contain computed objects."
  (let ((source (make-hash-table)))
    (puthash :get-column-count (lambda (_source) (length headers)) source)
    (puthash :get-row-count (lambda (_source) (length rows)) source)
    (puthash :get-header-value
             (lambda (_source col) (nth col headers))
             source)
    (puthash :get-computed-value-at
             (lambda (_source row col) (nth col (nth row rows)))
             source)
    (puthash :get-merges (lambda (_source) merges) source)
    source))

(ert-deftest exc-vertical-width-includes-vertical-anchor-only ()
  (should
   (equal
    (exc--calculate-column-widths
     '("A" "B" "C" "D")
     '(("head" "head" "head" "head")
       ("short" "bake 350F\n30 to 40 min" "wide horizontal title" "")
       ("short" "" "" ""))
     '((1 1 2 1) (1 2 1 3)))
    '(5 12 5 5))))

(ert-deftest exc-vertical-render-centers-content-and-suppresses-inner-rules ()
  (should
   (equal
    (exc-vertical-test--render
     (exc-vertical-test--source
      '("A" "B")
      '(("A" "B")
        ("left1\nleft1b\nleft1c" "v1\nv2\nv3")
        ("left2" "")
        ("left3" ""))
      '((1 1 3 1))))
    "┌───┬────────┬───────┐
│   │ A      │ B     │
├───┼────────┼───────┤
│1  │ A      │ B     │
├───┼────────┼───────┤
│2  │ left1  │       │
│   │ left1b │ v1    │
│   │ left1c │ v2    │
├───┼────────┤       │
│3  │ left2  │ v3    │
├───┼────────┤       │
│4  │ left3  │       │
└───┴────────┴───────┘
")))

(ert-deftest exc-vertical-render-is-independent-of-merge-order ()
  (let* ((headers '("A" "B" "C"))
         (rows '(("A" "B" "C")
                 ("a1\na2\na3\na4\na5\na6"
                  "b1\nb2\nb3\nb4\nb5" "r1")
                 ("" "" "r2")
                 ("tail" "" "r3")))
         (left-first (exc-vertical-test--source
                      headers rows '((1 0 2 0) (1 1 3 1))))
         (right-first (exc-vertical-test--source
                       headers rows '((1 1 3 1) (1 0 2 0)))))
    (should (equal (exc-vertical-test--render left-first)
                   (exc-vertical-test--render right-first)))
    (should
     (equal
      (append
       (plist-get
        (exc--build-layout
         rows '(5 5 5) '((1 0 2 0) (1 1 3 1)))
        :heights)
       nil)
      '(1 3 3 1)))))

(ert-deftest exc-vertical-render-properties-cover-blank-slices ()
  (let ((source (exc-vertical-test--source
                 '("A" "B")
                 '(("A" "B")
                   ("left1\nleft1b\nleft1c" "v1\nv2\nv3")
                   ("left2" "")
                   ("left3" ""))
                 '((1 1 3 1))))
        matches)
    (with-temp-buffer
      (insert (exc-text-render source))
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (when (and (get-text-property pos 'exc-cell)
                     (equal (get-text-property pos 'exc-display-row-start) 1)
                     (equal (get-text-property pos 'exc-display-row-end) 3)
                     (equal (get-text-property pos 'exc-display-col-start) 2)
                     (equal (get-text-property pos 'exc-display-col-end) 2))
            (push (buffer-substring-no-properties
                   pos
                   (or (next-single-property-change
                        pos 'exc-cell nil (point-max))
                       (point-max)))
                  matches))
          (setq pos (1+ pos))))
      (should (member "       " matches))
      (should (member " v1    " matches))
      (should (member " v3    " matches)))))

(ert-deftest exc-vertical-render-centers-chart-anchor ()
  (let ((chart (list :exc-chart t :type 'bar :data nil :options nil)))
    (should
     (string-match-p
      "│3  │ left2 │ \\[Chart\\]                                  │"
      (exc-vertical-test--render
       (exc-vertical-test--computed-source
        '("A" "B")
        (list '("A" "B")
              (list "left1" chart)
              '("left2" "")
              '("left3" ""))
        '((1 1 3 1))))))))

(ert-deftest exc-vertical-render-centers-stubbed-image-slices ()
  (let* ((source
          (exc-vertical-test--source
           '("A" "B")
           '(("A" "B")
             ("left1" "image")
             ("left2" "")
             ("left3" ""))
           '((1 1 3 1))))
         (original (symbol-function 'exc--cell-lines))
         rendered)
    (cl-letf (((symbol-function 'exc--cell-lines)
               (lambda (value width)
                 (if (equal value "image")
                     '("img1" "img2" "img3" "img4" "img5")
                   (funcall original value width)))))
      (setq rendered (exc-vertical-test--render source)))
    (let ((one (string-match "img1" rendered))
          (two (string-match "img2" rendered))
          (three (string-match "img3" rendered))
          (four (string-match "img4" rendered))
          (five (string-match "img5" rendered)))
      (should (< one two three four five))
      (should (string-match-p "│2  │ left1 │ img1  │" rendered))
      (should (string-match-p "│4  │ left3 │ img5  │" rendered)))))

(ert-deftest exc-vertical-render-matches-recipe-flow-acceptance-table ()
  (should
   (equal
    (exc-vertical-test--render
     (exc-vertical-test--source
      '("Prep" "" "" "" "")
      '(("Prep" "" "" "" "")
        ("Heat oven" "" "" "" "")
        ("butter" "melt" "mix" "mix"
         "bake\n350F\n170C\n30 to 40\nmin")
        ("sugar" "" "" "" "")
        ("vanilla" "stir" "" "" "")
        ("eggs" "whisk" "fold" "" "")
        ("flour" "sift" "rest" "fold in" "")
        ("salt" "add" "done" "serve" ""))
      '((0 0 0 4)
        (1 0 1 4)
        (2 1 3 1)
        (2 2 4 2)
        (2 3 5 3)
        (2 4 7 4))))
    "┌───┬─────────┬───────┬───────┬─────────┬──────────┐
│   │ A       │ B     │ C     │ D       │ E        │
├───┼─────────┴───────┴───────┴─────────┴──────────┤
│1  │ Prep                                         │
├───┼──────────────────────────────────────────────┤
│2  │ Heat oven                                    │
├───┼─────────┬───────┬───────┬─────────┬──────────┤
│3  │ butter  │ melt  │       │         │ bake     │
├───┼─────────┤       │       │         │          │
│4  │ sugar   │       │ mix   │ mix     │ 350F     │
├───┼─────────┼───────┤       │         │          │
│5  │ vanilla │ stir  │       │         │ 170C     │
├───┼─────────┼───────┼───────┤         │          │
│6  │ eggs    │ whisk │ fold  │         │ 30 to 40 │
├───┼─────────┼───────┼───────┼─────────┤          │
│7  │ flour   │ sift  │ rest  │ fold in │ min      │
├───┼─────────┼───────┼───────┼─────────┤          │
│8  │ salt    │ add   │ done  │ serve   │          │
└───┴─────────┴───────┴───────┴─────────┴──────────┘
")))

(provide 'exc-vertical-render-test)

;;; exc-vertical-render-test.el ends here
