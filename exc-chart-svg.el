;;; exc-chart-svg.el --- Charts drawn as SVG over a grid -*- lexical-binding: t -*-

;;; Commentary:
;; A floating chart is described by a plist:
;;
;;   (:anchor (ROW . COL) :size (COLUMNS . LINES) :type TYPE
;;    :range "A1:D7" :by BY :title TITLE)
;;
;; ROW and COL are the cell its top left corner sits on, COLUMNS and LINES
;; its size in characters and lines.  An :offset (X . 0) moves the corner
;; X characters, a fraction or more, right of that cell's left edge; with
;; none it is on the edge.  RANGE holds the data, read the way
;; a spreadsheet reads it: a first row of text names the series, a first
;; column of text names the categories, and the series run down the
;; columns unless the range is wider than it is tall.  BY, `columns' or
;; `rows', overrides that choice; nil keeps it.
;;
;; This file only reads data and draws; where a chart sits is the view's
;; business.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'exc-calc)
(require 'exc-format)
(require 'exc-obsolete)

(defconst exc-chart-svg-types
  '((column . "Column") (column-stacked . "Stacked column")
    (bar . "Bar") (bar-stacked . "Stacked bar")
    (line . "Line") (area . "Area")
    (pie . "Pie") (doughnut . "Doughnut")
    (scatter . "Scatter"))
  "Chart types a floating chart can take, with their names.")

(defcustom exc-chart-svg-palette
  '("#4472C4" "#ED7D31" "#A5A5A5" "#FFC000" "#5B9BD5" "#70AD47"
    "#264478" "#9E480E" "#636363" "#997300")
  "Colors given to the series of a chart, in turn."
  :type '(repeat color)
  :group 'exc)

;;;----------------------------------------------------------------------
;;; Reading a range
;;;----------------------------------------------------------------------

(defun exc-chart-svg-parse-range (text)
  "Return (TOP LEFT BOTTOM RIGHT) for range TEXT such as A1:D7, or nil."
  (when (and (stringp text)
             (string-match
              (concat "\\`\\$?\\([A-Za-z]+\\)\\$?\\([0-9]+\\)"
                      "\\(?::\\$?\\([A-Za-z]+\\)\\$?\\([0-9]+\\)\\)?\\'")
              text))
    (let* ((left (exc-calc-col-letter-to-number (match-string 1 text)))
           (top (1- (string-to-number (match-string 2 text))))
           (right (if (match-string 3 text)
                      (exc-calc-col-letter-to-number (match-string 3 text))
                    left))
           (bottom (if (match-string 4 text)
                       (1- (string-to-number (match-string 4 text)))
                     top)))
      (and (>= top 0)
           (list (min top bottom) (min left right)
                 (max top bottom) (max left right))))))

(defun exc-chart-svg-range-text (top left bottom right)
  "Return the range text of TOP LEFT BOTTOM RIGHT, as A1:D7."
  (format "%s%d:%s%d"
          (exc-calc-col-number-to-letter left) (1+ top)
          (exc-calc-col-number-to-letter right) (1+ bottom)))

(defun exc-chart-svg--number (value)
  "Return VALUE as a number, or nil when it is not one."
  (cond ((numberp value) value)
        ((and (stringp value) (not (exc-format-text-p value)))
         (exc-format-number (substring-no-properties value)))))

(defun exc-chart-svg--label-p (value)
  "Return non-nil when VALUE is text that names something."
  (and (stringp value) (not (string-empty-p value))
       (not (exc-chart-svg--number value))))

(defun exc-chart-svg-data (range value-fn label-fn &optional by type labels names)
  "Read the chart data of RANGE, (TOP LEFT BOTTOM RIGHT).
VALUE-FN, called with a row and a column, returns a cell's computed
value; LABEL-FN returns the text it shows.  BY is `columns', `rows' or
nil to choose.  TYPE `scatter' takes the first column, or row, as the
x values whatever it holds.  With BY given, LABELS t says the first
column, or row, of RANGE labels the categories and `none' that nothing
does; NAMES says the same of the first row, or column, naming the
series.  Left nil, what the cells hold decides.

Return (:categories LABELS :series ((NAME . VALUES) ...) :by BY
:ranges RANGES :category-range RANGE :names CELLS), where VALUES holds
a number or nil for each category, RANGES the cells each series reads,
RANGE the cells holding the labels, nil when they are numbered, and
CELLS the cell naming each series, nil when they are numbered; ranges
and cells are (TOP LEFT BOTTOM RIGHT)."
  (pcase-let* ((`(,top ,left ,bottom ,right) range)
               (rows (1+ (- bottom top)))
               (cols (1+ (- right left)))
               (value (lambda (r c) (funcall value-fn (+ top r) (+ left c))))
               (label (lambda (r c) (funcall label-fn (+ top r) (+ left c))))
               (corner-empty (let ((v (funcall value 0 0)))
                               (or (null v) (equal v ""))))
               ;; What a workbook says of its first row and column.
               (header-given (pcase by ('columns names) ('rows labels)))
               (side-given (pcase by ('columns labels) ('rows names)))
               ;; A first row of text names the series of columns.
               (header (if header-given
                           (and (> rows 1) (eq header-given t))
                         (and (> rows 1)
                              (or (and corner-empty (> cols 1))
                                  (cl-loop for c from 1 below cols
                                           thereis (exc-chart-svg--label-p
                                                    (funcall value 0 c)))
                                  (and (= cols 1)
                                       (exc-chart-svg--label-p
                                        (funcall value 0 0)))))))
               ;; A first column of text names the categories.
               (side (if side-given
                         (and (> cols 1) (eq side-given t))
                       (and (> cols 1)
                            (or (eq type 'scatter)
                                (and corner-empty header)
                                (cl-loop for r from (if header 1 0) below rows
                                         thereis (exc-chart-svg--label-p
                                                  (funcall value r 0)))))))
               (h (if header 1 0))
               (k (if side 1 0))
               (by (or by (if (>= (- rows h) (- cols k)) 'columns 'rows))))
    (if (eq by 'columns)
        (list :by 'columns
              :ranges (cl-loop for c from k below cols
                               collect (list (+ top h) (+ left c) bottom (+ left c)))
              :category-range (and side (list (+ top h) left bottom left))
              :names (and header (cl-loop for c from k below cols
                                          collect (list top (+ left c) top (+ left c))))
              :categories (cl-loop for r from h below rows
                                   collect (if side (funcall label r 0)
                                             (number-to-string (1+ (- r h)))))
              :series (cl-loop for c from k below cols
                               collect (cons (if header (funcall label 0 c)
                                               (format "Series %d" (1+ (- c k))))
                                             (cl-loop for r from h below rows
                                                      collect (exc-chart-svg--number
                                                               (funcall value r c))))))
      (list :by 'rows
            :ranges (cl-loop for r from h below rows
                             collect (list (+ top r) (+ left k) (+ top r) right))
            :category-range (and header (list top (+ left k) top right))
            :names (and side (cl-loop for r from h below rows
                                      collect (list (+ top r) left (+ top r) left)))
            :categories (cl-loop for c from k below cols
                                 collect (if header (funcall label 0 c)
                                           (number-to-string (1+ (- c k)))))
            :series (cl-loop for r from h below rows
                             collect (cons (if side (funcall label r 0)
                                             (format "Series %d" (1+ (- r h))))
                                           (cl-loop for c from k below cols
                                                    collect (exc-chart-svg--number
                                                             (funcall value r c)))))))))

;;;----------------------------------------------------------------------
;;; Drawing
;;;----------------------------------------------------------------------

(defun exc-chart-svg--color (index)
  "Return the palette color of series INDEX."
  (nth (mod index (length exc-chart-svg-palette)) exc-chart-svg-palette))

(defun exc-chart-svg--format (number)
  "Return NUMBER written for an axis or a label."
  (cond ((and (integerp number) (< (abs number) 1e15)) (number-to-string number))
        ((and (floatp number) (= number (ftruncate number)) (< (abs number) 1e15))
         (number-to-string (truncate number)))
        (t (let ((text (format "%.3g" number)))
             (if (string-match-p "e" text) text
               (replace-regexp-in-string "\\.?0+\\'" "" (format "%.2f" number)))))))

(defun exc-chart-svg--ticks (low high)
  "Return evenly spaced round numbers covering LOW to HIGH."
  (when (= low high)
    (if (= high 0) (setq high 1)
      (setq low (min 0 low) high (max 0 high))))
  (let* ((span (- high low))
         (raw (/ span 5.0))
         (magnitude (expt 10.0 (floor (log raw 10))))
         (step (* magnitude
                  (let ((f (/ raw magnitude)))
                    (cond ((<= f 1) 1) ((<= f 2) 2) ((<= f 2.5) 2.5)
                          ((<= f 5) 5) (t 10)))))
         (start (* step (floor (/ low step))))
         (end (* step (ceiling (/ high step)))))
    (cl-loop for v = start then (+ v step)
             while (<= v (+ end (* step 1e-9)))
             collect (if (< (abs v) (* step 1e-9)) 0 v))))

(defun exc-chart-svg--text-width (text size)
  "Estimate the width in pixels of TEXT drawn SIZE pixels high."
  (* 0.6 size (string-width text)))

(defun exc-chart-svg--fit (text size room)
  "Cut TEXT, drawn SIZE high, to fit ROOM pixels."
  (if (<= (exc-chart-svg--text-width text size) room)
      text
    (let ((keep (max 1 (floor (/ room (* 0.6 size))))))
      (if (<= keep 1) "" (concat (truncate-string-to-width text (1- keep)) "…")))))

(defun exc-chart-svg-render (chart data width height
                                    &optional foreground background view)
  "Return the SVG text that draws CHART with DATA, WIDTH by HEIGHT pixels.
CHART is a chart plist and DATA what `exc-chart-svg-data' returns.
FOREGROUND and BACKGROUND are the text and paper colors.  VIEW, when
given, is (X Y W H): only that part is shown, at its own size."
  (let* ((fg (or foreground "#202020"))
         (bg (or background "#ffffff"))
         (svg (svg-create width height))
         (font (max 8 (min 14 (/ height 16))))
         (type (plist-get chart :type))
         (title (plist-get chart :title))
         (series (plist-get data :series))
         (categories (plist-get data :categories))
         (pie (memq type '(pie doughnut)))
         (legend (or pie (> (length series) 1)))
         (top (+ 6 (if (and title (not (string-empty-p title))) (* 1.8 font) 0)))
         (bottom (- height 6 (if legend (* 1.8 font) 0)))
         (grid-color (if (equal bg "#ffffff") "#d9d9d9" "#606060")))
    (svg-rectangle svg 0 0 width height :fill bg :stroke grid-color)
    ;; A selected chart has a thick frame.
    (when (plist-get chart :selected)
      (svg-rectangle svg 1.5 1.5 (- width 3) (- height 3) :fill "none"
                     :stroke "#2F6BD8" :stroke-width 3))
    (when (and title (not (string-empty-p title)))
      (svg-text svg (exc-chart-svg--fit title (* 1.2 font) (- width 12))
                :x (/ width 2.0) :y (+ 6 (* 1.2 font)) :text-anchor "middle"
                :font-size (* 1.2 font) :font-weight "bold" :fill fg
                :font-family "sans-serif"))
    (when legend
      (let* ((names (if pie categories (mapcar #'car series)))
             (x 8)
             (y (- height 6 (* 0.4 font))))
        (cl-loop for name in names for i from 0
                 while (< (+ x font) width) do
                 (let ((text (exc-chart-svg--fit (or name "") font
                                                  (- width x font 10))))
                   (svg-rectangle svg x (- y (* 0.8 font)) (* 0.8 font) (* 0.8 font)
                                  :fill (exc-chart-svg--color i))
                   (svg-text svg text :x (+ x font) :y y :font-size font
                             :fill fg :font-family "sans-serif")
                   (setq x (+ x font (exc-chart-svg--text-width text font)
                              (* 1.2 font)))))))
    (cond
     ((null series)
      (svg-text svg "No data" :x (/ width 2.0) :y (/ (+ top bottom) 2.0)
                :text-anchor "middle" :font-size font :fill fg
                :font-family "sans-serif"))
     (pie (exc-chart-svg--pie svg type series categories
                               width top bottom fg bg))
     (t (exc-chart-svg--axes svg type series categories
                              width top bottom font fg grid-color)))
    (when view
      (pcase-let ((`(,x ,y ,w ,h) view))
        (dom-set-attribute svg 'width (max 1 w))
        (dom-set-attribute svg 'height (max 1 h))
        (dom-set-attribute svg 'viewBox (format "%s %s %s %s" x y (max 1 w) (max 1 h)))))
    (with-temp-buffer
      (svg-print svg)
      (buffer-string))))

(defun exc-chart-svg--pie (svg type series categories width top bottom fg bg)
  "Draw the first of SERIES as a pie or doughnut of TYPE into SVG.
CATEGORIES name the slices; the chart sits between TOP and BOTTOM of a
drawing WIDTH wide.  FG and BG are the text and paper colors."
  (let* ((values (mapcar (lambda (v) (max 0 (or v 0))) (cdr (car series))))
         (total (apply #'+ 0 values))
         (cx (/ width 2.0))
         (cy (/ (+ top bottom) 2.0))
         (r (max 4 (- (/ (min width (- bottom top)) 2.0) 6)))
         (angle (- (/ float-pi 2))))
    (ignore categories)
    (if (<= total 0)
        (svg-text svg "No data" :x cx :y cy :text-anchor "middle" :fill fg)
      (cl-loop for v in values for i from 0 do
               (let* ((sweep (* 2 float-pi (/ v (float total))))
                      (end (+ angle sweep)))
                 (cond ((>= sweep (* 2 float-pi 0.9999))
                        (svg-circle svg cx cy r :fill (exc-chart-svg--color i)))
                       ((> sweep 0)
                        (svg-node svg 'path
                                  :d (format "M %.2f %.2f L %.2f %.2f A %.2f %.2f 0 %d 1 %.2f %.2f Z"
                                             cx cy
                                             (+ cx (* r (cos angle))) (+ cy (* r (sin angle)))
                                             r r (if (> sweep float-pi) 1 0)
                                             (+ cx (* r (cos end))) (+ cy (* r (sin end))))
                                  :fill (exc-chart-svg--color i)
                                  :stroke bg :stroke-width 1)))
                 (when (> sweep 0.25)
                   (let ((mid (+ angle (/ sweep 2))))
                     (svg-text svg (format "%d%%" (round (* 100 (/ v (float total)))))
                               :x (+ cx (* 0.7 r (cos mid)))
                               :y (+ cy (* 0.7 r (sin mid)) 4)
                               :text-anchor "middle" :font-size 11 :fill "#ffffff"
                               :font-family "sans-serif")))
                 (setq angle end)))
      (when (eq type 'doughnut)
        (svg-circle svg cx cy (* 0.5 r) :fill bg)))))

(defun exc-chart-svg--axes (svg type series categories width top bottom font
                                 fg grid-color)
  "Draw SERIES over CATEGORIES as a chart of TYPE with axes into SVG.
The plot sits between TOP and BOTTOM of a drawing WIDTH wide; FONT is the
text size, FG and GRID-COLOR the text and grid line colors."
  (let* ((stacked (memq type '(column-stacked bar-stacked)))
         (horizontal (memq type '(bar bar-stacked)))
         (scatter (eq type 'scatter))
         (n (length categories))
         (xs (and scatter
                  (let ((numbers (mapcar #'exc-chart-svg--number categories)))
                    (if (cl-every #'identity numbers) numbers
                      (number-sequence 1 n)))))
         (values (apply #'append (mapcar #'cdr series)))
         (numbers (or (delq nil (copy-sequence values)) '(0)))
         (low (if stacked
                  (min 0 (apply #'min (cl-loop for i below n collect
                                               (apply #'+ (cl-loop for s in series
                                                                   for v = (nth i (cdr s))
                                                                   when (and v (< v 0)) collect v)))))
                (min 0 (apply #'min numbers))))
         (high (if stacked
                   (max 0 (apply #'max 0 (cl-loop for i below n collect
                                                  (apply #'+ (cl-loop for s in series
                                                                      for v = (nth i (cdr s))
                                                                      when (and v (> v 0)) collect v)))))
                 (max (if (memq type '(line scatter)) (apply #'min numbers) 0)
                      (apply #'max numbers))))
         (low (if (memq type '(line scatter)) (min low (apply #'min numbers)) low))
         (ticks (exc-chart-svg--ticks low high))
         (vmin (car ticks))
         (vmax (car (last ticks)))
         (labels (mapcar #'exc-chart-svg--format ticks))
         (value-room (+ 8 (apply #'max (mapcar (lambda (l) (exc-chart-svg--text-width l font))
                                               labels))))
         (category-room (if horizontal
                            (min (* 0.3 width)
                                 (+ 8 (apply #'max 0 (mapcar (lambda (c) (exc-chart-svg--text-width c font))
                                                             categories))))
                          0))
         (left (+ 4 (if horizontal category-room value-room)))
         ;; The last value label of a bar chart is centered on the edge.
         (right (- width 10 (if horizontal
                                (/ (exc-chart-svg--text-width (car (last labels)) font) 2)
                              0)))
         (plot-bottom (- bottom (* 1.6 font)))
         (plot-top (+ top 4))
         (plot-w (max 1 (- right left)))
         (plot-h (max 1 (- plot-bottom plot-top))))
    (cl-flet* ((vpos (v)
                 ;; Where value V falls along the value axis.
                 (let ((f (/ (- v vmin) (float (max 1e-12 (- vmax vmin))))))
                   (if horizontal (+ left (* f plot-w)) (- plot-bottom (* f plot-h)))))
               (xpos (x)
                 (let ((lo (apply #'min xs)) (hi (apply #'max xs)))
                   ;; A little room either side keeps the end points whole.
                   (+ left (* plot-w (if (= lo hi) 0.5
                                       (+ 0.04 (* 0.92 (/ (- x lo) (float (- hi lo))))))))))
               (text (string x y anchor)
                 (svg-text svg string :x x :y y :text-anchor anchor
                           :font-size font :fill fg :font-family "sans-serif")))
      ;; Value grid lines and their labels.
      (cl-loop for v in ticks for l in labels do
               (let ((p (vpos v)))
                 (if horizontal
                     (progn (svg-line svg p plot-top p plot-bottom :stroke grid-color)
                            (text l p (+ plot-bottom (* 1.2 font)) "middle"))
                   (svg-line svg left p right p :stroke grid-color)
                   (text l (- left 6) (+ p (* 0.35 font)) "end"))))
      ;; Category labels.
      (let ((band (/ (if horizontal plot-h plot-w) (float (max 1 n)))))
        (cond
         (scatter
          (let ((ticks (exc-chart-svg--ticks (apply #'min xs) (apply #'max xs))))
            (dolist (x ticks)
              (when (<= (apply #'min xs) x (apply #'max xs))
                (text (exc-chart-svg--format x) (xpos x)
                      (+ plot-bottom (* 1.2 font)) "middle")))))
         (horizontal
          (cl-loop for c in categories for i from 0 do
                   (text (exc-chart-svg--fit c font (- category-room 8))
                         (- left 6) (+ plot-top (* band (+ i 0.5)) (* 0.35 font)) "end")))
         (t
          (let ((every (max 1 (ceiling (/ (* n (apply #'max 1 (mapcar (lambda (c) (exc-chart-svg--text-width c font)) categories)))
                                          (* 0.9 plot-w))))))
            (cl-loop for c in categories for i from 0
                     when (= 0 (mod i every)) do
                     (text (exc-chart-svg--fit c font (* every band))
                           (+ left (* band (+ i 0.5))) (+ plot-bottom (* 1.2 font))
                           "middle")))))
        ;; The axis at zero.
        (let ((zero (vpos (max vmin (min vmax 0)))))
          (if horizontal
              (svg-line svg zero plot-top zero plot-bottom :stroke fg)
            (svg-line svg left zero right zero :stroke fg)))
        ;; The series.
        (pcase type
          ((or 'column 'bar)
           (let* ((count (length series))
                  (gap (* 0.2 band))
                  (thick (/ (- band (* 2 gap)) (float (max 1 count)))))
             (cl-loop for s in series for j from 0 do
                      (cl-loop for v in (cdr s) for i from 0 when v do
                               (let* ((a (vpos (max vmin (min vmax 0)))) (b (vpos v))
                                      (start (+ (if horizontal plot-top left)
                                                (* band i) gap (* thick j))))
                                 (if horizontal
                                     (svg-rectangle svg (min a b) start (max 1 (abs (- b a))) thick
                                                    :fill (exc-chart-svg--color j))
                                   (svg-rectangle svg start (min a b) thick (max 1 (abs (- b a)))
                                                  :fill (exc-chart-svg--color j))))))))
          ((or 'column-stacked 'bar-stacked)
           (let ((gap (* 0.2 band))
                 (up (make-vector n 0))
                 (down (make-vector n 0)))
             (cl-loop for s in series for j from 0 do
                      (cl-loop for v in (cdr s) for i from 0 when (and v (/= v 0)) do
                               (let* ((base (if (> v 0) (aref up i) (aref down i)))
                                      (a (vpos base)) (b (vpos (+ base v)))
                                      (start (+ (if horizontal plot-top left) (* band i) gap))
                                      (thick (- band (* 2 gap))))
                                 (if (> v 0) (aset up i (+ base v)) (aset down i (+ base v)))
                                 (if horizontal
                                     (svg-rectangle svg (min a b) start (max 1 (abs (- b a))) thick
                                                    :fill (exc-chart-svg--color j))
                                   (svg-rectangle svg start (min a b) thick (max 1 (abs (- b a)))
                                                  :fill (exc-chart-svg--color j))))))))
          ((or 'line 'area 'scatter)
           (cl-loop for s in series for j from 0 do
                    (let ((points (cl-loop for v in (cdr s) for i from 0 when v
                                           collect (cons (if scatter (xpos (nth i xs))
                                                           (+ left (* band (+ i 0.5))))
                                                         (vpos v))))
                          (color (exc-chart-svg--color j)))
                      (when (and (eq type 'area) points)
                        (let ((zero (vpos (max vmin (min vmax 0)))))
                          (svg-polygon svg (append (list (cons (caar points) zero))
                                                   points
                                                   (list (cons (car (car (last points))) zero)))
                                       :fill color :fill-opacity 0.45 :stroke "none")))
                      (unless scatter
                        (when (cdr points)
                          (svg-polyline svg points :fill "none" :stroke color
                                        :stroke-width 2)))
                      (when (or scatter (eq type 'line))
                        (dolist (p points)
                          (svg-circle svg (car p) (cdr p) (if scatter 3.5 2.5)
                                      :fill color)))))))))))

(defun exc-chart-svg-text (chart data columns lines)
  "Return a plain text picture of CHART with DATA, COLUMNS by LINES.
It stands in for the drawing where images cannot be shown."
  (let* ((title (or (plist-get chart :title)
                    (cdr (assq (plist-get chart :type) exc-chart-svg-types))))
         (series (plist-get data :series))
         (categories (plist-get data :categories))
         (rows (list (truncate-string-to-width (format "[%s]" title) columns))))
    (when (and series (not (memq (plist-get chart :type) '(pie doughnut scatter))))
      (let* ((first (cdr (car series)))
             (top (apply #'max 1e-9 (mapcar (lambda (v) (abs (or v 0))) first)))
             (label-w (min 10 (apply #'max 1 (mapcar #'string-width categories))))
             (bar-w (max 1 (- columns label-w 2))))
        (cl-loop for c in categories for v in first
                 while (< (length rows) lines) do
                 (push (concat (truncate-string-to-width c label-w nil ?\s) " "
                               (make-string (round (* bar-w (/ (abs (or v 0)) (float top)))) ?█))
                       rows))))
    (when (< (length rows) lines)
      (push (truncate-string-to-width
             (mapconcat #'car series ", ") columns)
            rows))
    (mapconcat #'identity (nreverse rows) "\n")))

(provide 'exc-chart-svg)

;;; exc-chart-svg.el ends here
