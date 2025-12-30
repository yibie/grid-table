;;; grid-table-chart.el --- Charting extension for grid-table -*- lexical-binding: t -*-

(require 'grid-table-calc)
(require 'cl-lib)

;;;----------------------------------------------------------------------
;;; Parameter System
;;;----------------------------------------------------------------------

(cl-defstruct grid-chart-params
  title width height border margin padding color xlabel ylabel labels symbol
  reverse xscale nbins closed canvas xlim ylim grid name fmt bar-colors)

(defun grid-chart-params-create (&rest args)
  "Helper constructor for `grid-chart-params' accepting plist style ARGS."
  (apply #'make-grid-chart-params args))

(defun grid-chart--plist-merge (base override)
  "Merge OVERRIDE plist onto BASE plist, overriding duplicate keys."
  (let ((result (copy-sequence (or base '())))
        (cursor override))
    (while cursor
      (setq result (plist-put result (car cursor) (cadr cursor)))
      (setq cursor (cddr cursor)))
    result))

(defun grid-chart--params-to-plist (params)
  "Convert PARAMS struct to a plist, skipping nil values."
  (let ((pairs '((:title . grid-chart-params-title)
                 (:width . grid-chart-params-width)
                 (:height . grid-chart-params-height)
                 (:border . grid-chart-params-border)
                 (:margin . grid-chart-params-margin)
                 (:padding . grid-chart-params-padding)
                 (:color . grid-chart-params-color)
                 (:xlabel . grid-chart-params-xlabel)
                 (:ylabel . grid-chart-params-ylabel)
                 (:labels . grid-chart-params-labels)
                 (:symbol . grid-chart-params-symbol)
                 (:reverse . grid-chart-params-reverse)
                 (:xscale . grid-chart-params-xscale)
                 (:nbins . grid-chart-params-nbins)
                 (:closed . grid-chart-params-closed)
                 (:canvas . grid-chart-params-canvas)
                 (:xlim . grid-chart-params-xlim)
                 (:ylim . grid-chart-params-ylim)
                 (:grid . grid-chart-params-grid)
                 (:name . grid-chart-params-name)
                 (:fmt . grid-chart-params-fmt)))
        (plist '()))
    (dolist (pair pairs plist)
      (let* ((key (car pair))
             (accessor (cdr pair))
             (value (funcall accessor params)))
        (when value
          (setq plist (plist-put plist key value)))))))

(defun grid-chart--options-to-params (options defaults)
  "Convert chart OPTIONS plist/struct to PARAMS applying DEFAULTS."
  (let* ((merged (grid-chart--plist-merge defaults
                                          (cond
                                           ((grid-chart-params-p options)
                                            (grid-chart--params-to-plist options))
                                           ((listp options) options)
                                           (t nil)))))
    (apply #'grid-chart-params-create merged)))

(defun grid-chart--build-style-options (params &optional extra)
  "Assemble render options from PARAMS and EXTRA plist."
  (let ((options (copy-sequence extra)))
    (dolist (pair '((:border . grid-chart-params-border)
                    (:margin . grid-chart-params-margin)
                    (:padding . grid-chart-params-padding)
                    (:color . grid-chart-params-color)
                    (:grid . grid-chart-params-grid)))
      (let ((value (funcall (cdr pair) params)))
        (when value
          (setq options (plist-put options (car pair) value)))))
    options))

(defun grid-chart--border-chars (style)
  "Return a plist of characters for STYLE."
  (pcase style
    ('rounded '(:h "─" :v "│" :tl "╭" :tr "╮" :bl "╰" :br "╯"))
    ('double '(:h "═" :v "║" :tl "╔" :tr "╗" :bl "╚" :br "╝"))
    (_ '(:h "─" :v "│" :tl "┌" :tr "┐" :bl "└" :br "┘"))))

(defun grid-chart--apply-padding (content padding)
  "Pad CONTENT with spaces on both sides."
  (if (and padding (> padding 0))
      (concat (make-string padding ?\s) content (make-string padding ?\s))
    content))

(defun grid-chart--apply-grid-guides (content)
  "Overlay simple grid markers on CONTENT string."
  (let* ((len (length content))
         (positions (delete-dups
                     (list (/ len 4) (/ len 2) (/ (* 3 len) 4))))
         (target (if (multibyte-string-p content)
                     content
                   (string-make-multibyte (copy-sequence content))))
         (grid-char (if (multibyte-string-p target) ?· ?.)))
    (dolist (pos positions target)
      (when (and (> pos 0) (< pos len))
        (when (char-equal (aref target pos) ?\s)
          (aset target pos grid-char))))))

(defun grid-chart--apply-margin (lines margin)
  "Prefix each line in LINES with MARGIN spaces."
  (if (and margin (> margin 0))
      (let ((prefix (make-string margin ?\s)))
        (mapcar (lambda (line) (concat prefix line)) lines))
    lines))

(defun grid-chart--apply-color (text color)
  "Apply COLOR (string or symbol) to TEXT."
  (if (not color)
      text
    (let ((color-name (if (symbolp color) (symbol-name color) color)))
      (propertize text 'face `(:foreground ,color-name)))))

;;;----------------------------------------------------------------------
;;; Nice Tick Calculation (YouPlot style)
;;;----------------------------------------------------------------------

(defun grid-chart--nice-number (x round-p)
  "Return a 'nice' number close to X. If ROUND-P, round; else ceiling."
  (let* ((exp (floor (log x 10)))
         (f (/ x (expt 10.0 exp)))
         (nf (cond
              (round-p
               (cond ((< f 1.5) 1.0)
                     ((< f 3.0) 2.0)
                     ((< f 7.0) 5.0)
                     (t 10.0)))
              (t
               (cond ((<= f 1.0) 1.0)
                     ((<= f 2.0) 2.0)
                     ((<= f 5.0) 5.0)
                     (t 10.0))))))
    (* nf (expt 10.0 exp))))

(defun grid-chart--nice-ticks (min-val max-val num-ticks)
  "Compute a list of NUM-TICKS nice tick values between MIN-VAL and MAX-VAL."
  (when (= min-val max-val)
    (setq min-val (- min-val 0.5))
    (setq max-val (+ max-val 0.5)))
  (let* ((range (grid-chart--nice-number (- max-val min-val) nil))
         (d (grid-chart--nice-number (/ range (max 1 (1- num-ticks))) t))
         (graph-min (* (floor (/ min-val d)) d))
         (graph-max (* (ceiling (/ max-val d)) d))
         (ticks '())
         (tick graph-min))
    (while (<= tick graph-max)
      (push tick ticks)
      (setq tick (+ tick d)))
    (nreverse ticks)))

(defun grid-chart--expand-range (min-val max-val &optional padding-pct)
  "Expand range by PADDING-PCT (default 5%) to avoid data on edges."
  (let* ((pct (or padding-pct 0.05))
         (range (- max-val min-val))
         (pad (* range pct)))
    (when (zerop range)
      (setq pad (max 0.5 (abs (* min-val 0.1)))))
    (cons (- min-val pad) (+ max-val pad))))

;;;----------------------------------------------------------------------
;;; Statistical Utilities
;;;----------------------------------------------------------------------

(defun grid-chart--quantile (sorted-values q)
  "Compute quantile Q (0..1) from SORTED-VALUES using linear interpolation."
  (let* ((count (length sorted-values))
         (pos (* q (max 0 (- count 1))))
         (lower (floor pos))
         (upper (ceiling pos))
         (frac (- pos lower)))
    (if (zerop count)
        0
      (+ (* (- 1 frac) (nth lower sorted-values))
         (* frac (nth upper sorted-values))))))

(defun grid-chart--quantiles (values)
  "Return (Q1 MEDIAN Q3) for VALUES."
  (let ((sorted (sort (cl-copy-list (mapcar #'grid-calc-to-number values)) #'<)))
    (list (grid-chart--quantile sorted 0.25)
          (grid-chart--quantile sorted 0.5)
          (grid-chart--quantile sorted 0.75))))

(defun grid-chart--histogram-bins (values &optional nbins closed)
  "Compute histogram bins for VALUES.
Returns a plist (:edges list :counts list :min float :max float)."
  (let* ((numeric (mapcar #'grid-calc-to-number values))
         (count (length numeric))
         (min-val (if numeric (apply #'min numeric) 0))
         (max-val (if numeric (apply #'max numeric) 0))
         (bin-count (max 1 (or nbins (ceiling (sqrt (max count 1))))))
         (range (max 0.0001 (- max-val min-val)))
         (width (/ range bin-count))
         (edges (cl-loop for i from 0 to bin-count
                         collect (+ min-val (* i width))))
         (counts (make-vector bin-count 0))
         (mode (or closed 'left))
         (epsilon 1e-9))
    (dolist (value numeric)
      (let* ((normalized (/ (- value min-val) (max width epsilon)))
             (idx (cond
                   ((eq mode 'right)
                    (cond
                     ((= value min-val) 0)
                     ((= value max-val) (1- bin-count))
                     (t (max 0 (min (1- bin-count)
                                    (floor (- normalized epsilon)))))))
                   (t
                    (if (= value max-val)
                        (1- bin-count)
                      (max 0 (min (1- bin-count) (floor normalized))))))))
        (aset counts idx (1+ (aref counts idx)))))
    (list :edges edges
          :counts (append counts nil)
          :min min-val
          :max max-val
          :width width)))

(defun grid-chart--density-estimate (x-values y-values grid-w grid-h)
  "Build a simple 2D density grid with GRID-W by GRID-H buckets."
  (let* ((xs (mapcar #'grid-calc-to-number x-values))
         (ys (mapcar #'grid-calc-to-number y-values))
         (pairs (cl-mapcar #'cons xs ys))
         (min-x (if xs (apply #'min xs) 0))
         (max-x (if xs (apply #'max xs) 1))
         (min-y (if ys (apply #'min ys) 0))
         (max-y (if ys (apply #'max ys) 1))
         (range-x (max 0.0001 (- max-x min-x)))
         (range-y (max 0.0001 (- max-y min-y)))
         (grid (make-vector grid-h nil)))
    (dotimes (row grid-h)
      (aset grid row (make-vector grid-w 0)))
    (dolist (pair pairs)
      (let* ((x (car pair))
             (y (cdr pair))
             (col (min (1- grid-w)
                       (floor (* (/ (- x min-x) range-x) grid-w))))
             (row (min (1- grid-h)
                       (floor (* (/ (- y min-y) range-y) grid-h)))))
        (aset (aref grid row) col (1+ (aref (aref grid row) col)))))
    (list :grid grid
          :min-x min-x :max-x max-x
          :min-y min-y :max-y max-y)))

(defun grid-chart--density-char (value max-value)
  "Map VALUE density to a Unicode shade."
  (let* ((ratio (if (<= max-value 0)
                    0.0
                  (/ (float value) max-value))))
    (cond
     ((<= ratio 0.05) " ")
     ((<= ratio 0.25) "░")
     ((<= ratio 0.5) "▒")
     ((<= ratio 0.75) "▓")
     (t "█"))))

(defun grid-chart--count-values (values &optional reverse)
  "Count frequency of VALUES, returning (labels counts)."
  (let ((table (make-hash-table :test 'equal))
        (result '()))
    (dolist (item values)
      (puthash item (1+ (gethash item table 0)) table))
    (maphash (lambda (k v) (push (cons k v) result)) table)
    (setq result (sort result
                       (lambda (a b)
                         (let ((diff (- (cdr b) (cdr a))))
                           (if (/= diff 0)
                               (> diff 0)
                             (string< (format "%s" (car a))
                                      (format "%s" (car b))))))))
    (when reverse
      (setq result (nreverse result)))
    (list (mapcar #'car result)
          (mapcar #'cdr result))))

(defun grid-chart--normalize-series (series-data)
  "Normalize SERIES-DATA into a list of numeric series."
  (let ((data (if (vectorp series-data)
                  (append series-data nil)
                series-data)))
    (if (and (listp data)
             data
             (or (listp (car data))
                 (vectorp (car data))))
        (mapcar #'grid-chart--parse-data data)
      (list (grid-chart--parse-data data)))))

(defun grid-chart--value->col (value min-value max-value width)
  "Map VALUE in [MIN-VALUE, MAX-VALUE] to column index within WIDTH."
  (let* ((span (max 0.0001 (- max-value min-value)))
         (normalized (/ (- value min-value) span)))
    (max 0 (min (1- width) (floor (* normalized (max 1 (1- width))))))))

(defun grid-chart--fill-range (line start end char)
  "Fill LINE string from START to END (inclusive) with CHAR."
  (when (<= start end)
    (dotimes (i (1+ (- end start)))
      (aset line (+ start i) char)))
  line)

;;;----------------------------------------------------------------------
;;; Braille Canvas Implementation (For Line/Scatter/Sparkline)
;;;----------------------------------------------------------------------

(defun grid-chart--canvas-get (canvas x y)
  (or (gethash (cons x y) canvas) 0))

(defun grid-chart--canvas-set (canvas x y val)
  (puthash (cons x y) val canvas))

(defun grid-chart--pixel-mask (px py)
  (let ((col (mod px 2))
        (row (mod py 4)))
    (cond
     ((and (= col 0) (= row 0)) #x01)
     ((and (= col 0) (= row 1)) #x02)
     ((and (= col 0) (= row 2)) #x04)
     ((and (= col 1) (= row 0)) #x08)
     ((and (= col 1) (= row 1)) #x10)
     ((and (= col 1) (= row 2)) #x20)
     ((and (= col 0) (= row 3)) #x40)
     ((and (= col 1) (= row 3)) #x80))))

(defun grid-chart-set-pixel (canvas px py)
  (let* ((char-x (/ px 2))
         (char-y (/ py 4))
         (mask (grid-chart--pixel-mask px py))
         (current-val (grid-chart--canvas-get canvas char-x char-y)))
    (grid-chart--canvas-set canvas char-x char-y (logior current-val mask))))

(defun grid-chart-draw-line (canvas x0 y0 x1 y1)
  "Bresenham's line algorithm for Braille canvas."
  (let* ((dx (abs (- x1 x0)))
         (dy (abs (- y1 y0)))
         (sx (if (< x0 x1) 1 -1))
         (sy (if (< y0 y1) 1 -1))
         (err (if (> dx dy) (/ dx 2) (- (/ dy 2)))))
    (while (not (and (= x0 x1) (= y0 y1)))
      (grid-chart-set-pixel canvas x0 y0)
      (let ((e2 err))
        (when (> e2 (- dx))
          (setq err (- err dy))
          (setq x0 (+ x0 sx)))
        (when (< e2 dy)
          (setq err (+ err dx))
          (setq y0 (+ y0 sy)))))))

(defun grid-chart--canvas-bounds (canvas width height)
  "Find the actual content bounds of Braille CANVAS.
Returns (min-x max-x min-y max-y) or nil if canvas is empty."
  (let ((min-x width)
        (max-x -1)
        (min-y height)
        (max-y -1)
        (has-content nil))
    (dotimes (y height)
      (dotimes (x width)
        (let ((val (grid-chart--canvas-get canvas x y)))
          (when (> val 0)
            (setq has-content t)
            (setq min-x (min min-x x))
            (setq max-x (max max-x x))
            (setq min-y (min min-y y))
            (setq max-y (max max-y y))))))
    (when has-content
      (list min-x max-x min-y max-y))))

(defun grid-chart--render-braille-buffer (canvas width height &optional crop-bounds)
  "Render the Braille canvas to a list of strings (lines).
If CROP-BOUNDS is non-nil, crop to actual content bounds."
  (let* ((bounds (if crop-bounds (grid-chart--canvas-bounds canvas width height) nil))
         (render-width (if bounds
                           (1+ (- (nth 1 bounds) (nth 0 bounds)))
                         width))
         (render-height (if bounds
                            (1+ (- (nth 3 bounds) (nth 2 bounds)))
                          height))
         (start-x (if bounds (nth 0 bounds) 0))
         (start-y (if bounds (nth 2 bounds) 0))
         (lines '()))
    (dotimes (y render-height)
      (let ((line-str ""))
        (dotimes (x render-width)
          (let* ((canvas-x (+ start-x x))
                 (canvas-y (+ start-y y))
                 (val (grid-chart--canvas-get canvas canvas-x canvas-y)))
            (setq line-str (concat line-str (char-to-string (+ #x2800 val))))))
        (push line-str lines)))
    (nreverse lines)))

;;;----------------------------------------------------------------------
;;; Common Chart Utilities
;;;----------------------------------------------------------------------

(defun grid-chart--parse-data (input)
  "Convert INPUT (string, vector, or list) into a list of numbers."
  (cond
   ((vectorp input)
    (mapcar #'grid-calc-to-number (append input nil)))
   ((listp input)
    (mapcar #'grid-calc-to-number input))
   ((stringp input)
    (mapcar #'string-to-number (split-string input "," t "[ \t\n\r]+")))
   (t (list 0))))

(defun grid-chart--format-number (num)
  "Format number for axis labels. Uses integer format if it's effectively an integer."
  (if (= num (truncate num))
      (number-to-string (truncate num))
    (format "%.1f" num)))

(defun grid-chart--pad-left (str width)
  "Pad string STR with spaces on the left to reach WIDTH."
  (let ((len (string-width str)))
    (if (< len width)
        (concat (make-string (- width len) ?\s) str)
      str)))

(defun grid-chart--pad-right (str width)
  "Pad string STR with spaces on the right to reach WIDTH."
  (let ((len (string-width str)))
    (if (< len width)
        (concat str (make-string (- width len) ?\s))
      str)))

(defun grid-chart--calculate-content-width (content-lines content-width padding)
  "Calculate final content width.
CONTENT-LINES: List of content strings.
CONTENT-WIDTH: Explicit width if provided, nil otherwise.
PADDING: Padding to add on both sides.
Returns the final width including padding."
  (let ((base-width (or content-width
                        (apply #'max 0 (mapcar #'string-width content-lines)))))
    (+ base-width (* 2 (max 0 padding)))))

;;;----------------------------------------------------------------------
;;; Axis and Frame Rendering (YouPlot Style)
;;;----------------------------------------------------------------------

(defun grid-chart--overlay-grid-line (line-content width grid-char)
  "Overlay horizontal grid markers on LINE-CONTENT."
  (let ((result (copy-sequence line-content))
        (positions (list (/ width 4) (/ width 2) (/ (* 3 width) 4))))
    (unless (multibyte-string-p result)
      (setq result (string-make-multibyte result)))
    (when (and (not (multibyte-string-p result)) (> grid-char 255))
      (setq grid-char ?.))
    (dolist (pos positions)
      (when (and (>= pos 0) (< pos (length result)))
        (when (char-equal (aref result pos) ?\s)
          (aset result pos grid-char))))
    result))

(defun grid-chart--build-y-axis-info (height y-min y-max custom-y-labels)
  "Build Y axis labels and grid row information.
Returns (y-tick-labels y-label-width grid-rows)."
  (if custom-y-labels
      (list (mapcar (lambda (s) (format "%s" s)) custom-y-labels)
            (apply #'max 0 (mapcar #'string-width custom-y-labels))
            nil)
    (let* ((y-ticks (grid-chart--nice-ticks y-min y-max (min 5 (max 3 (/ height 3)))))
           (y-tick-labels (mapcar #'grid-chart--format-number y-ticks))
           (y-label-width (apply #'max 0 (mapcar #'string-width y-tick-labels)))
           (y-range (max 0.0001 (- y-max y-min)))
           (grid-rows (mapcar (lambda (tick)
                                (round (* (/ (- y-max tick) y-range) (1- height))))
                              y-ticks)))
      (list y-tick-labels y-label-width grid-rows))))

(defun grid-chart--build-chart-body-line (i content-lines height width padding y-label-width
                                           y-tick-labels grid-rows custom-y-labels
                                           draw-grid border-style border-type)
  "Build a single chart body line with axis."
  (let* ((line-content (nth i content-lines))
         (line-content (grid-chart--apply-padding line-content padding))
         (is-grid-row (and draw-grid (not custom-y-labels) (member i grid-rows)))
         (line-content (if is-grid-row
                           (grid-chart--overlay-grid-line line-content width ?·)
                         line-content))
         (line-len (string-width line-content))
         (pad-len (max 0 (- width line-len)))
         (padded-content (concat line-content (make-string pad-len ?\s)))
         (label (if custom-y-labels
                    (nth i y-tick-labels)
                  (let ((tick-idx (cl-position i grid-rows :test #'=)))
                    (if tick-idx (nth tick-idx y-tick-labels) ""))))
         (padded-label (grid-chart--pad-left (or label "") y-label-width))
         (body-separator (if custom-y-labels
                             (pcase border-type
                               ('double "╣")
                               (_ "┤"))
                           (if is-grid-row "┼" (plist-get border-style :v)))))
    ;; Use concat instead of format to preserve text properties (colors)
    (concat padded-label " " body-separator padded-content (plist-get border-style :v))))

(defun grid-chart--build-x-axis-labels (content-width padding x-min x-max y-label-width)
  "Build X axis label line showing min, middle values, and max.
Simple approach: show actual data range boundaries and middle points."
  (let* ((total-width (+ content-width (* 2 padding)))
         (x-range (- x-max x-min))
         ;; Generate 5 evenly spaced ticks including endpoints
         (num-ticks 5)
         (x-ticks (cl-loop for i from 0 to (1- num-ticks)
                           collect (+ x-min (* (/ (float i) (1- num-ticks)) x-range))))
         (x-axis-line (make-string total-width ?\s)))
    (dolist (tick x-ticks)
      (let* ((ratio (/ (- tick x-min) (max 0.0001 x-range)))
             (pos-in-content (round (* ratio (1- content-width))))
             (pos-in-total (+ padding pos-in-content))
             (label (grid-chart--format-number tick))
             (label-len (length label))
             ;; Center the label on the tick position
             (start (max 0 (min (- total-width label-len) (- pos-in-total (/ label-len 2))))))
        ;; Only place if it fits and doesn't overlap previous
        (when (and (>= start 0) (<= (+ start label-len) total-width))
          ;; Check if space is empty (avoid overlap)
          (let ((can-place t))
            (dotimes (j label-len)
              (when (/= (aref x-axis-line (+ start j)) ?\s)
                (setq can-place nil)))
            (when can-place
              (dotimes (j label-len)
                (aset x-axis-line (+ start j) (aref label j))))))))
    (concat (make-string (+ y-label-width 2) ?\s) x-axis-line)))

(defun grid-chart--build-x-category-labels (labels col-width spacing content-width y-label-width)
  "Build X axis category labels for column plot.
LABELS: list of category labels.
COL-WIDTH: width of each column.
SPACING: spacing between columns.
CONTENT-WIDTH: total content width.
Y-LABEL-WIDTH: width of Y axis labels."
  (let* ((count (length labels))
         (cell-width (+ col-width spacing))
         ;; Build label characters as a list to support Unicode
         (total-label-width content-width)
         (label-chars (make-list total-label-width ?\s)))
    ;; Place each label centered under its column
    (cl-loop for col-idx from 0 below count
             for lbl in labels
             for lbl-str = (format "%s" lbl)
             for x-center = (+ (* col-idx cell-width) (/ col-width 2))
             for lbl-len = (length lbl-str)
             for start = (max 0 (- x-center (/ lbl-len 2)))
             do (cl-loop for i from 0 below lbl-len
                         for pos = (+ start i)
                         when (< pos total-label-width)
                         do (setcar (nthcdr pos label-chars) (aref lbl-str i))))
    (concat (make-string (+ y-label-width 2) ?\s) (apply #'string label-chars))))

(defun grid-chart-render-with-axis (content-lines x-min x-max y-min y-max &optional title x-label y-label options)
  "Wrap CONTENT-LINES (list of strings) with axes and borders.
CONTENT-LINES: The inner chart content.
X-MIN, X-MAX: Range for X axis (bottom).
Y-MIN, Y-MAX: Range for Y axis (left).
TITLE: Optional chart title.
X-LABEL, Y-LABEL: Optional axis labels.
OPTIONS: Plist of extra options:
  :y-labels - Category labels for Y axis (bar charts)
  :x-labels - Category labels for X axis (column charts)
  :col-width - Column width for x-labels positioning
  :col-spacing - Column spacing for x-labels positioning"
  (let* ((height (length content-lines))
         (padding (max 0 (or (plist-get options :padding) 0)))
         (explicit-content-width (plist-get options :content-width))
         (calculated-content-width (apply #'max 0 (mapcar #'string-width content-lines)))
         (content-width (or explicit-content-width calculated-content-width))
         (width (grid-chart--calculate-content-width content-lines explicit-content-width padding))
         (border-type (plist-get options :border))
         (border-style (grid-chart--border-chars border-type))
         (margin (max 0 (or (plist-get options :margin) 0)))
         (draw-grid (plist-get options :grid))
         (color (plist-get options :color))
         (custom-y-labels (plist-get options :y-labels))
         (custom-x-labels (plist-get options :x-labels))
         (col-width (or (plist-get options :col-width) 2))
         (col-spacing (or (plist-get options :col-spacing) 1))
         (y-axis-info (grid-chart--build-y-axis-info height y-min y-max custom-y-labels))
         (y-tick-labels (nth 0 y-axis-info))
         (y-label-width (nth 1 y-axis-info))
         (grid-rows (nth 2 y-axis-info))
         (result '()))

    ;; Title
    (when title
      (setq result (cons (concat (make-string (+ y-label-width 2) ?\s) (format "%s" title)) result)))

    ;; Top Border
    (setq result (cons (concat (make-string (+ y-label-width 1) ?\s)
                               (plist-get border-style :tl)
                               (make-string width (string-to-char (plist-get border-style :h)))
                               (plist-get border-style :tr))
                       result))

    ;; Chart Body
    (dotimes (i height)
      (setq result (cons (grid-chart--build-chart-body-line i content-lines height width padding
                                                           y-label-width y-tick-labels grid-rows
                                                           custom-y-labels draw-grid border-style border-type)
                         result)))

    ;; Bottom Border
    (setq result (cons (concat (make-string (+ y-label-width 1) ?\s)
                               (plist-get border-style :bl)
                               (make-string width (string-to-char (plist-get border-style :h)))
                               (plist-get border-style :br))
                       result))

    ;; X Axis Labels: category labels for column plots, or numeric for other plots
    (cond
     (custom-x-labels
      ;; Column plot: show category labels
      (setq result (cons (grid-chart--build-x-category-labels
                          custom-x-labels col-width col-spacing content-width y-label-width)
                         result)))
     ((not custom-y-labels)
      ;; Regular plot: show numeric X axis
      (setq result (cons (grid-chart--build-x-axis-labels content-width padding x-min x-max y-label-width) result))))

    ;; Axis titles
    (when x-label
      (setq result (cons (concat (make-string (+ y-label-width 2) ?\s) x-label) result)))
    (when y-label
      (setq result (cons (concat " " y-label) result)))

    ;; Finalize
    (let* ((ordered (nreverse result))
           (with-margin (grid-chart--apply-margin ordered margin))
           (joined (string-join with-margin "\n")))
      (if color
          (grid-chart--apply-color joined color)
        joined))))

;;;----------------------------------------------------------------------
;;; Bar Plot Implementation
;;;----------------------------------------------------------------------

(defun grid-chart--make-bar-string (char-count bar-char)
  "Make a bar string of CHAR-COUNT characters using BAR-CHAR.
Returns the string and its display width."
  (let* ((str (make-string char-count bar-char))
         (display-width (string-width str)))
    (cons str display-width)))

(defun grid-chart-barplot (labels values params)
  "Generate a horizontal bar plot string using PARAMS configuration.
Layout: label ┤ bar... value │
Uses ASCII character (#) for bars to ensure perfect alignment."
  (let* ((params (or params (grid-chart-params-create)))
         (total-width (or (grid-chart-params-width params) 40))
         (title (grid-chart-params-title params))
         (vals (grid-chart--parse-data values))
         (count (length vals))
         (max-val (if vals (apply #'max vals) 0))
         ;; Use Unicode Dark Shade character for better visual appearance
         (bar-char ?▓)
         (labels-str (mapcar (lambda (x) (format "%s" x)) labels))
         (val-strs (mapcar (lambda (v) (grid-chart--format-number v)) vals))
         (val-col-width (+ 1 (apply #'max 0 (mapcar #'length val-strs))))
         (bar-area-width (max 5 (- total-width val-col-width)))
         (lines (list))
         ;; Default color palette for bars
         (default-colors '("blue" "green" "yellow" "cyan" "magenta"
                          "#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6"))
         (bar-colors (or (grid-chart-params-bar-colors params) default-colors)))

    (dotimes (i count)
      (let* ((val (nth i vals))
             (val-str (grid-chart--format-number val))
             (bar-len (if (> max-val 0)
                          (round (* (/ (float val) max-val) bar-area-width))
                        0))
             (bar-str (make-string bar-len bar-char))
             (bar-padding (make-string (- bar-area-width bar-len) ?\s))
             (val-padded (concat (make-string (- val-col-width (length val-str)) ?\s)
                                 val-str))
             ;; Apply color to the bar (cycle through palette)
             (bar-colored (if bar-colors
                              (grid-chart--apply-color
                               (concat bar-str bar-padding)
                               (nth (mod i (length bar-colors)) bar-colors))
                            (concat bar-str bar-padding)))
             (line (concat bar-colored val-padded)))
        (push line lines)))

    (let ((render-options (grid-chart--build-style-options params (list :y-labels labels))))
      (plist-put render-options :content-width total-width)
      (plist-put render-options :padding 0)
      (grid-chart-render-with-axis
       (nreverse lines)
       0 max-val 0 0
       title
       (grid-chart-params-xlabel params)
       (grid-chart-params-ylabel params)
       render-options))))

;;;----------------------------------------------------------------------
;;; Column Plot (Vertical Bar Chart) Implementation
;;;----------------------------------------------------------------------

(defun grid-chartcolumnplot--build-content-lines (col-heights col-width spacing count chart-height &optional fill-char colors)
  "Build content lines for column plot.
COL-HEIGHTS: List of column heights (in rows).
COL-WIDTH: Width of each column in characters.
SPACING: Spacing between columns.
COUNT: Number of columns.
CHART-HEIGHT: Total height of the chart.
FILL-CHAR: Character to use for bars.
COLORS: Optional list of colors for each column.
Returns a list of strings (from top to bottom)."
  (let* ((content-width (* count (+ col-width spacing)))
         (lines '())
         (bar-char (if (characterp fill-char) fill-char ?█))
         (cell-width (+ col-width spacing)))
    ;; Generate lines from top (y = chart-height-1) to bottom (y = 0)
    (dotimes (y chart-height)
      (let* ((row-from-bottom (- chart-height 1 y))
             ;; Build each column segment separately for coloring
             (segments '()))
        ;; For each column, build its segment
        (cl-loop for col-idx from 0 below count
                 for col-h in col-heights
                 for color = (and colors (nth (mod col-idx (length colors)) colors))
                 do (let* ((bar-filled (< row-from-bottom col-h))
                           (bar-str (if bar-filled
                                        (make-string col-width bar-char)
                                      (make-string col-width ?\s)))
                           (space-str (make-string spacing ?\s))
                           (segment (concat bar-str space-str)))
                      ;; Apply color if bar is filled and color is specified
                      (when (and bar-filled color)
                        (setq segment (propertize segment 'face `(:foreground ,color))))
                      (push segment segments)))
        ;; Combine segments into line (reverse because we pushed)
        (push (apply #'concat (nreverse segments)) lines)))
    (nreverse lines)))

(defun grid-chartcolumnplot--resolve-fill-char (params)
  "Pick a displayable fill character for column plots."
  (let* ((raw (and params (grid-chart-params-symbol params)))
         (preferred (cond
                     ((characterp raw) raw)
                     ((and (stringp raw) (> (length raw) 0)) (aref raw 0))
                     (t ?█)))
         (candidates (list preferred ?▓ ?#)))
    (if (fboundp 'char-displayable-p)
        (or (cl-find-if #'char-displayable-p candidates) ?#)
      preferred)))

(defun grid-chartcolumnplot--calculate-y-ticks (nice-max chart-height)
  "Calculate Y axis tick values for column plot.
NICE-MAX: Nice round maximum value for Y axis.
CHART-HEIGHT: Height of the chart in rows.
Returns a list of tick values."
  (let* ((num-ticks (max 3 (min 5 (round (/ chart-height 3)))))
         (step (max 0.001 (/ nice-max (1- num-ticks))))
         (ticks '()))
    (dotimes (i num-ticks)
      (push (* i step) ticks))
    (nreverse ticks)))

(defun grid-chartcolumnplot--calculate-col-heights (vals y-max chart-height)
  "Calculate column heights in rows.
VALS: List of data values.
Y-MAX: Maximum value for Y axis.
CHART-HEIGHT: Height of the chart in rows.
Returns a list of column heights."
  (if (<= y-max 0)
      (make-list (length vals) 0)
    (mapcar (lambda (v)
              ;; Use full chart-height for proper scaling
              (round (* (/ (float v) y-max) chart-height)))
            vals)))

(defun grid-chart-columnplot (labels values params)
  "Generate a vertical column plot using PARAMS configuration.
Uses grid-chart-render-with-axis for consistent rendering.
Layout: Y axis with values, X axis with labels, columns grow upward."
  (let* ((params (or params (grid-chart-params-create)))
         (title (grid-chart-params-title params))
         (chart-width (or (grid-chart-params-width params) 40))
         (chart-height (or (grid-chart-params-height params) 15))
         (vals (grid-chart--parse-data values))
         (count (length vals))
         (max-val (if vals (apply #'max vals) 0))
         ;; Convert labels to strings first to calculate widths
         (label-strings (mapcar (lambda (l) (format "%s" l)) labels))
         (max-label-width (apply #'max 1 (mapcar #'string-width label-strings)))
         ;; Default color palette (same as barplot)
         (default-colors '("blue" "green" "yellow" "cyan" "magenta"
                          "#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6"))
         (bar-colors (or (grid-chart-params-bar-colors params) default-colors)))
    (when (zerop count)
      (cl-return-from grid-chart-columnplot ""))
    ;; Ensure we have nice round max value for Y axis
    (let* ((nice-max (grid-chart--nice-number max-val nil))
           (y-ticks (grid-chartcolumnplot--calculate-y-ticks nice-max chart-height))
           (y-max (or (plist-get (grid-chart--params-to-plist params) :ylim)
                      (car (last y-ticks))))
           ;; Calculate column width based on label length (minimum 2 for the bar)
           (col-width (max 2 max-label-width))
           (spacing 1)
           (col-heights (grid-chartcolumnplot--calculate-col-heights vals y-max chart-height))
           (fill-char (grid-chartcolumnplot--resolve-fill-char params))
           (content-lines (grid-chartcolumnplot--build-content-lines
                           col-heights col-width spacing count chart-height fill-char bar-colors))
           (render-options (list
                            :width chart-width
                            :content-width (* count (+ col-width spacing))
                            :padding 0
                            :border nil
                            :x-labels label-strings
                            :col-width col-width
                            :col-spacing spacing)))
      (grid-chart-render-with-axis
       content-lines
       0 count        ; x range (not used for category axis)
       0 y-max        ; y range
       title
       nil            ; x-label
       nil            ; y-label
       render-options))))

;;;----------------------------------------------------------------------
;;; Histogram Implementation
;;;----------------------------------------------------------------------

(defun grid-chart--histogram-label (start end closed)
  "Format histogram interval label."
  (let* ((left (if (eq closed 'right) "(" "["))
         (right (if (eq closed 'right) "]" ")")))
    (format "%s%s, %s%s"
            left
            (grid-chart--format-number start)
            (grid-chart--format-number end)
            right)))

(defun grid-chart-histogram (values params)
  "Render a histogram using PARAMS configuration."
  (let* ((params (or params (grid-chart-params-create)))
         (nbins (grid-chart-params-nbins params))
         (closed (grid-chart-params-closed params))
         (bins (grid-chart--histogram-bins values nbins closed))
         (edges (plist-get bins :edges))
         (counts (plist-get bins :counts))
         (labels (cl-loop for i from 0 below (length counts)
                          collect (grid-chart--histogram-label
                                   (nth i edges)
                                   (nth (1+ i) edges)
                                   closed))))
    (grid-chart-barplot labels counts params)))

;;;----------------------------------------------------------------------
;;; Count Plot Implementation
;;;----------------------------------------------------------------------

(defun grid-chart-count (values params)
  "Render a frequency count bar plot."
  (let* ((params (or params (grid-chart-params-create)))
         (reverse (grid-chart-params-reverse params))
         (result (grid-chart--count-values values reverse))
         (labels (nth 0 result))
         (counts (nth 1 result)))
    (grid-chart-barplot labels counts params)))

;;;----------------------------------------------------------------------
;;; Boxplot Implementation
;;;----------------------------------------------------------------------

(defun grid-chart--boxplot-line (stats min-value max-value width)
  "Build a single boxplot line for STATS."
  (let* ((line (make-string width ?\s))
         (min-col (grid-chart--value->col (plist-get stats :min) min-value max-value width))
         (max-col (grid-chart--value->col (plist-get stats :max) min-value max-value width))
         (q1-col (grid-chart--value->col (plist-get stats :q1) min-value max-value width))
         (q3-col (grid-chart--value->col (plist-get stats :q3) min-value max-value width))
         (median-col (grid-chart--value->col (plist-get stats :median) min-value max-value width)))
    (grid-chart--fill-range line min-col max-col ?-)
    (grid-chart--fill-range line q1-col q3-col ?=)
    (aset line min-col ?+)
    (aset line max-col ?+)
    (aset line q1-col ?\[)
    (aset line q3-col ?\])
    (aset line median-col ?|)
    (concat line
            (format "  min:%s med:%s max:%s"
                    (grid-chart--format-number (plist-get stats :min))
                    (grid-chart--format-number (plist-get stats :median))
                    (grid-chart--format-number (plist-get stats :max))))))

(defun grid-chart-boxplot (series params)
  "Render a textual boxplot for SERIES using PARAMS."
  (let* ((params (or params (grid-chart-params-create)))
         (series-list (grid-chart--normalize-series series))
         (default-labels (cl-loop for idx from 1 to (length series-list)
                                  collect (format "Series %d" idx)))
         (labels (let* ((raw (grid-chart-params-labels params))
                        (provided (cond
                                   ((listp raw) raw)
                                   ((null raw) nil)
                                   (t (list raw)))))
                   (cl-loop for idx from 0 below (length series-list)
                            collect (or (nth idx provided)
                                        (nth idx default-labels)))))
         (plot-width (max 20 (or (grid-chart-params-width params) 40)))
         (stats (mapcar (lambda (values)
                          (let* ((sorted (sort (cl-copy-list values) #'<))
                                 (min-val (if sorted (car sorted) 0))
                                 (max-val (if sorted (car (last sorted)) min-val))
                                 (quartiles (grid-chart--quantiles sorted)))
                            (list :min min-val
                                  :max max-val
                                  :q1 (nth 0 quartiles)
                                  :median (nth 1 quartiles)
                                  :q3 (nth 2 quartiles))))
                        series-list))
         (overall-min (apply #'min (mapcar (lambda (st) (plist-get st :min)) stats)))
         (overall-max (apply #'max (mapcar (lambda (st) (plist-get st :max)) stats)))
         ;; Default color palette for boxplot series
         (default-colors '("blue" "green" "yellow" "cyan" "magenta"
                          "#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6"))
         (box-colors (or (grid-chart-params-bar-colors params) default-colors))
         (lines (cl-loop for st in stats for idx from 0
                        collect (let* ((line (grid-chart--boxplot-line st overall-min overall-max plot-width))
                                       (color (nth (mod idx (length box-colors)) box-colors))
                                       (colored-line (if color
                                                        (grid-chart--apply-color line color)
                                                      line)))
                                  colored-line))))
    (let ((render-options (grid-chart--build-style-options params (list :y-labels labels))))
      (grid-chart-render-with-axis
       lines
       overall-min overall-max 0 0
       (grid-chart-params-title params)
       (grid-chart-params-xlabel params)
       (grid-chart-params-ylabel params)
       render-options))))

;;;----------------------------------------------------------------------
;;; Line/Scatter Plot Implementation
;;;----------------------------------------------------------------------

(defun grid-chart--lineplot-render (x-data y-series params &optional type)
  "Render line or scatter plot for Y-SERIES using shared X-DATA."
  (let* ((params (or params (grid-chart-params-create)))
         (width (or (grid-chart-params-width params) 40))
         (height (or (grid-chart-params-height params) 15))
         (title (grid-chart-params-title params))
         (x-values (grid-chart--parse-data x-data))
         (series-list (mapcar #'grid-chart--parse-data y-series))
         (count (apply #'min (cons (length x-values) (mapcar #'length series-list))))
         (raw-x-min (if x-values (apply #'min x-values) 0))
         (raw-x-max (if x-values (apply #'max x-values) 0))
         (all-y (cl-loop for s in series-list append s))
         (raw-y-min (if all-y (apply #'min all-y) 0))
         (raw-y-max (if all-y (apply #'max all-y) 0)))
    (when (or (zerop count) (null series-list))
      (cl-return-from grid-chart--lineplot-render "No data for plot"))
    (when (= raw-x-min raw-x-max)
      (setq x-values (number-sequence 1 count))
      (setq raw-x-min 1)
      (setq raw-x-max count))
    ;; Use nice ticks to determine axis range (ensures data maps to displayed ticks)
    (let* ((x-ticks (grid-chart--nice-ticks raw-x-min raw-x-max 5))
           (y-ticks (grid-chart--nice-ticks raw-y-min raw-y-max 5))
           (x-min (car x-ticks))
           (x-max (car (last x-ticks)))
           (y-min (car y-ticks))
           (y-max (car (last y-ticks)))
           (x-range (max 0.0001 (- x-max x-min)))
           (y-range (max 0.0001 (- y-max y-min)))
           (canvas (make-hash-table :test 'equal))
           (pixel-w (* width 2))
           (pixel-h (* height 4))
           (x-pixels (cl-loop for i from 0 below count
                              collect (round (* (/ (- (nth i x-values) x-min) x-range)
                                                (1- pixel-w))))))
      (cl-loop for series in series-list
               for series-idx from 0 do
               (dotimes (i count)
                 (let* ((px (nth i x-pixels))
                        (y-val (nth i series))
                        (py (round (- (1- pixel-h)
                                      (* (/ (- y-val y-min) y-range) (1- pixel-h))))))
                   (when (and (eq type 'line) (> i 0))
                     (let* ((prev-px (nth (1- i) x-pixels))
                            (prev-y (nth (1- i) series))
                            (prev-py (round (- (1- pixel-h)
                                               (* (/ (- prev-y y-min) y-range) (1- pixel-h))))))
                       (grid-chart-draw-line canvas prev-px prev-py px py)))
                   (grid-chart-set-pixel canvas px py))))
      (let* ((content-lines (grid-chart--render-braille-buffer canvas width height))
             ;; Calculate actual content width (find rightmost non-empty column)
             (bounds (grid-chart--canvas-bounds canvas width height))
             (actual-content-width (if bounds
                                       (1+ (- (nth 1 bounds) (nth 0 bounds)))
                                     width))
             (render-options (grid-chart--build-style-options params nil))
             (rendered (grid-chart-render-with-axis content-lines
                                                    x-min x-max y-min y-max
                                                    title
                                                    (grid-chart-params-xlabel params)
                                                    (grid-chart-params-ylabel params)
                                                    (plist-put render-options :content-width actual-content-width)))
             (legend (let* ((raw (grid-chart-params-labels params))
                            (labels (and raw (if (listp raw) raw (list raw)))))
                       (when (and labels (> (length labels) 0))
                         (concat "\nLegend: " (string-join labels ", "))))))
        (if legend
            (concat rendered legend)
          rendered)))))

(defun grid-chart-lineplot (x-data y-data params &optional type)
  "Generate a Line or Scatter plot string using PARAMS configuration."
  (grid-chart--lineplot-render x-data (list y-data) params type))

(defun grid-chart-multiline (x-data series params)
  "Render multiple line series sharing the same X-DATA."
  (grid-chart--lineplot-render x-data series params 'line))

;;;----------------------------------------------------------------------
;;; Density Plot Implementation (1D KDE Curve)
;;;----------------------------------------------------------------------

(defun grid-chart--kde-bandwidth (values)
  "Calculate Silverman's rule of thumb bandwidth for KDE.
Returns a minimum of 0.1 to avoid division by zero."
  (let* ((n (length values))
         (mean (/ (apply #'+ values) (float n)))
         (variance (/ (cl-loop for v in values
                               sum (expt (- v mean) 2))
                      (max 1 (1- n))))
         (std-dev (sqrt variance)))
    ;; Silverman's rule with minimum bandwidth
    (max 0.1 (* 1.06 std-dev (expt (float n) -0.2)))))

(defun grid-chart--kde-evaluate (x values bandwidth)
  "Evaluate KDE at point X given VALUES and BANDWIDTH."
  (let ((n (length values))
        (bw (max 0.1 bandwidth)))  ; Ensure non-zero
    (/ (cl-loop for v in values
                sum (exp (- (/ (expt (- x v) 2)
                               (* 2 bw bw)))))
       (* n bw (sqrt (* 2 float-pi))))))

(defun grid-chart-density (data _unused params)
  "Render a 1D density curve (KDE) for DATA.
_UNUSED is ignored (kept for API compatibility).
Uses Kernel Density Estimation with Gaussian kernel."
  (let* ((params (or params (grid-chart-params-create)))
         (width (max 10 (or (grid-chart-params-width params) 40)))
         (height (max 5 (or (grid-chart-params-height params) 15)))
         (values (grid-chart--parse-data data))
         (n (length values)))
    (if (< n 2)
        ;; Not enough data
        (grid-chart-render-with-axis
         (list (format "Need >= 2 data points (got %d)" n))
         0 1 0 1
         (grid-chart-params-title params) nil nil
         (grid-chart--build-style-options params nil))
      ;; Compute KDE
      (let* ((min-val (apply #'min values))
             (max-val (apply #'max values))
             (data-range (max 0.0001 (- max-val min-val)))
             ;; Internal range extended by 10% for smooth curve edges
             (internal-x-min (- min-val (* 0.1 data-range)))
             (internal-x-max (+ max-val (* 0.1 data-range)))
             (x-range (- internal-x-max internal-x-min))
             ;; Display range uses original data bounds (what user sees)
             (x-min min-val)
             (x-max max-val)
             (bandwidth (grid-chart--kde-bandwidth values))
             ;; Evaluate KDE at each x position (using internal extended range for smooth edges)
             (density-values
              (cl-loop for i from 0 below width
                       collect (let ((x (+ internal-x-min (* (/ (float i) (1- width)) x-range))))
                                 (grid-chart--kde-evaluate x values bandwidth))))
             (max-density (max 0.0001 (apply #'max density-values)))
             ;; Create Braille canvas
             (canvas (make-hash-table :test 'equal))
             (pixel-w (* width 2))
             (pixel-h (* height 4)))
        ;; Draw the density curve
        (cl-loop for i from 0 below width
                 for density in density-values
                 for px = (* i 2)
                 for py = (if (> max-density 0)
                              (round (- (1- pixel-h)
                                        (* (/ density max-density) (1- pixel-h))))
                            (1- pixel-h))
                 do (progn
                      ;; Draw filled area below curve
                      (cl-loop for fill-y from py below pixel-h
                               do (grid-chart-set-pixel canvas px fill-y))
                      ;; Connect to previous point
                      (when (> i 0)
                        (let* ((prev-density (nth (1- i) density-values))
                               (prev-py (if (> max-density 0)
                                            (round (- (1- pixel-h)
                                                      (* (/ prev-density max-density) (1- pixel-h))))
                                          (1- pixel-h))))
                          (grid-chart-draw-line canvas (- px 2) prev-py px py)))))
        ;; Render
        (let ((content-lines (grid-chart--render-braille-buffer canvas width height)))
          (grid-chart-render-with-axis
           content-lines
           x-min x-max 0 max-density
           (grid-chart-params-title params)
           (grid-chart-params-xlabel params)
           (grid-chart-params-ylabel params)
           (grid-chart--build-style-options params nil)))))))

;;;----------------------------------------------------------------------
;;; Sparkline (Mini-chart) - Legacy Compatibility
;;;----------------------------------------------------------------------

(defun grid-chart-sparkline-render (data width height)
  "Render a simple sparkline without axes (raw string)."
  (let* ((values (grid-chart--parse-data data))
         (count (length values))
         (min-val (if values (apply #'min values) 0))
         (max-val (if values (apply #'max values) 0))
         (range (max 0.0001 (- max-val min-val)))
         (canvas (make-hash-table :test 'equal))
         (pixel-w (* width 2))
         (pixel-h (* height 4)))

    (when (> count 1)
      (dotimes (i count)
        (let* ((val (nth i values))
               (px (round (* (/ (float i) (1- count)) (1- pixel-w))))
               (py (round (- (1- pixel-h) (* (/ (- val min-val) range) (1- pixel-h))))))
          (when (> i 0)
            (let* ((prev-val (nth (1- i) values))
                   (prev-px (round (* (/ (float (1- i)) (1- count)) (1- pixel-w))))
                   (prev-py (round (- (1- pixel-h) (* (/ (- prev-val min-val) range) (1- pixel-h))))))
              (grid-chart-draw-line canvas prev-px prev-py px py)))
          (grid-chart-set-pixel canvas px py))))

    (string-join (grid-chart--render-braille-buffer canvas width height) "\n")))

;;;----------------------------------------------------------------------
;;; Formula & Data Objects
;;;----------------------------------------------------------------------

(defun grid-chart-make-object (type data options)
  "Create a standardized chart object."
  (list :grid-chart t
        :type type
        :data data
        :options options))

(grid-calc-defun BARPLOT (labels values &optional title width)
  "Formula to create a Bar Plot. Returns a chart object."
  (ignore model)
  (grid-chart-make-object 'bar
                          (list :labels labels :values values)
                          (list :title title :width (or width 40))))

(grid-calc-defun COLUMNPLOT (labels values &optional title width height)
  "Formula to create a vertical Column Plot. Returns a chart object."
  (ignore model)
  (grid-chart-make-object 'column
                          (list :labels labels :values values)
                          (list :title title
                                :width (or width 40)
                                :height (or height 15))))

(grid-calc-defun HISTOGRAM (values &optional nbins title width closed)
  "Formula to create a Histogram chart object."
  (ignore model)
  (grid-chart-make-object 'histogram
                          (list :values values)
                          (list :title title
                                :width (or width 40)
                                :nbins nbins
                                :closed closed)))

(grid-calc-defun BOXPLOT (&rest args)
  "Formula to create a Boxplot chart object.
Usage: =BOXPLOT(series1, series2, ..., [title], [width])
All list/range arguments before the first string (title) or number (width) are treated as data series."
  (ignore model)
  (when (< (length args) 1)
    (error "BOXPLOT requires at least one series"))
  (let* ((series-list '())
         (title nil)
         (width nil))
    ;; Collect series until we hit a string (title) or number (width)
    (while (and args
                (not (stringp (car args)))
                (not (and (numberp (car args)) (not (listp (car args))))))
      (push (car args) series-list)
      (setq args (cdr args)))
    ;; Parse optional arguments
    (when args
      (let ((arg (car args)))
        (cond
         ((stringp arg)
          (setq title arg)
          (setq args (cdr args))
          (when (and args (numberp (car args)))
            (setq width (car args))))
         ((numberp arg)
          (setq width arg)))))
    (grid-chart-make-object 'boxplot
                            (list :series (nreverse series-list))
                            (list :title title
                                  :width (or width 50)))))

(grid-calc-defun MULTILINE (&rest args)
  "Formula to create a multi-series line chart.
Usage: =MULTILINE(x-data, series1, series2, ..., [title], [width], [height])
First arg is x-data, then multiple series (lists/ranges), then optional title/width/height."
  (ignore model)
  (when (< (length args) 2)
    (error "MULTILINE requires at least x-data and one series"))
  (let* ((x-data (car args))
         (remaining (cdr args))
         (series-list '())
         (title nil)
         (width nil)
         (height nil))
    ;; Collect series until we hit a string (title) or number (width/height)
    (while (and remaining
                (not (stringp (car remaining)))
                (not (and (numberp (car remaining)) (not (listp (car remaining))))))
      (push (car remaining) series-list)
      (setq remaining (cdr remaining)))
    ;; Parse optional arguments
    (when remaining
      (let ((arg (car remaining)))
        (cond
         ((stringp arg)
          (setq title arg)
          (setq remaining (cdr remaining))
          (when (and remaining (numberp (car remaining)))
            (setq width (car remaining))
            (setq remaining (cdr remaining))
            (when (and remaining (numberp (car remaining)))
              (setq height (car remaining)))))
         ((numberp arg)
          (setq width arg)
          (setq remaining (cdr remaining))
          (when (and remaining (numberp (car remaining)))
            (setq height (car remaining)))))))
    (grid-chart-make-object 'multiline
                            (list :x x-data :series (nreverse series-list))
                            (list :title title
                                  :width (or width 40)
                                  :height (or height 15)))))

(grid-calc-defun DENSITY (data &optional title width height)
  "Formula to create a 1D density plot (KDE curve).
Usage: =DENSITY(data, [title], [width], [height])"
  (ignore model)
  (grid-chart-make-object 'density
                          (list :data data)
                          (list :title title
                                :width (or width 40)
                                :height (or height 15))))

(grid-calc-defun COUNT (values &optional title reverse width)
  "Formula to create a Count/Frequency bar chart.
Usage: =COUNT(values, [title], [reverse], [width])"
  (ignore model)
  (grid-chart-make-object 'count
                          (list :values values)
                          (list :title title
                                :width (or width 40)
                                :reverse reverse)))

(grid-calc-defun LINEPLOT (x-data y-data &optional title width height)
  "Formula to create a Line Plot. Returns a chart object."
  (ignore model)
  (grid-chart-make-object 'line
                          (list :x x-data :y y-data)
                          (list :title title :width (or width 40) :height (or height 15))))

(grid-calc-defun SCATTER (x-data y-data &optional title width height)
  "Formula to create a Scatter Plot. Returns a chart object."
  (ignore model)
  (grid-chart-make-object 'scatter
                          (list :x x-data :y y-data)
                          (list :title title :width (or width 40) :height (or height 15))))

;;;----------------------------------------------------------------------
;;; Register with Grid Table Calc (Sparkline)
;;;----------------------------------------------------------------------
                                                                          
(grid-calc-defun SPARKLINE (&rest args)
  "Draws a sparkline (returns string).
   Usage: =SPARKLINE(A1:A5)
      OR: =SPARKLINE(1, 2, 3, 4, 5)"
  (ignore model)
  (let ((data (cond
               ((and (= (length args) 1) (listp (car args))) (car args))
               ((and (= (length args) 1) (stringp (car args))) (car args))
               (t args))))
    (grid-chart-sparkline-render data 10 1)))
                                                                          
(grid-calc-defun LINECHART (data width height)
  "Draws a line chart with custom dimensions (Sparkline style, returns string).
   Usage: =LINECHART(A1:A10, 20, 5)"
  (ignore model)
  (grid-chart-sparkline-render data
                       (grid-calc-to-number width)
                       (grid-calc-to-number height)))

(provide 'grid-table-chart)
