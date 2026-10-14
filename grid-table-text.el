;;; grid-table-text.el --- Grids as plain text  -*- lexical-binding: t; -*-

;;; Commentary:

;; What turns a grid into text without a buffer to show it in: wrapping,
;; column widths, and a table with box-drawing borders.  The exporters
;; and previews build on it, as does the classic view, which adds a
;; buffer, a cursor and commands on top.  The ETAF view shares the
;; helpers for wrapping, merges and column letters.

;;; Code:

(require 'cl-lib)

(declare-function grid-get-merges "grid-table")
(declare-function grid-get-display-value-at "grid-table")
(declare-function grid-get-column-count "grid-table")

(defcustom grid-table-image-target-char-height 5
  "Image target display height in table cells (in character lines)."
  :type 'integer
  :group 'grid-table)

(defcustom grid-table-image-max-width-ratio 0.8
  "Maximum ratio of image width to cell content space."
  :type 'float
  :group 'grid-table)

(defcustom grid-table-image-column-width 20
  "Default display width of image column (in characters)."
  :type 'integer
  :group 'grid-table)

(defconst grid-table--col-header-row-idx -1
  "Special row index for the A, B, C... column header row.")

(defconst grid-table--row-header-col-idx 0
  "Special column index for the 1, 2, 3... row header column.")

(defun grid-table--classic-merges (source)
  "Return the merges of SOURCE the classic renderer can draw.
It draws merges along one row or one column; a rectangular merge is
shown as separate cells, of which only the first holds a value."
  (cl-remove-if (lambda (merge)
                  (and (< (nth 0 merge) (nth 2 merge))
                       (< (nth 1 merge) (nth 3 merge))))
                (grid-get-merges source)))

(defun grid-table--merge-at (merges row col)
  "Return the range in MERGES covering ROW and COL."
  (cl-find-if (lambda (merge)
                (and (<= (nth 0 merge) row)
                     (<= row (nth 2 merge))
                     (<= (nth 1 merge) col)
                     (<= col (nth 3 merge))))
              merges))

(defun grid-table--calculate-column-widths (headers data-rows &optional merges)
  "Calculate the optimal column widths based on headers and data.
HEADERS is a list of strings for the column titles.
DATA-ROWS is a list of lists, where each inner list represents a row.
MERGES contains horizontal merge ranges whose contents do not size columns.
Returns a list of integers representing the calculated width for each column."
  (when (or headers data-rows)
    (let* ((num-columns (length headers))
           (min-width 5) ; Minimum width for any column
           (max-width 40) ; Maximum width for any column
           (widths (make-list num-columns 0)))

      ;; 1. Calculate initial widths from headers
      (dotimes (i num-columns)
        (setf (nth i widths)
              (max min-width
                   (string-width (grid-table--col-number-to-letter i))
                   (if (let ((merge (grid-table--merge-at merges 0 i)))
                         (and merge
                              (< (nth 1 merge) (nth 3 merge))))
                       0
                     (string-width (or (nth i headers) ""))))))

      ;; 2. Expand widths based on data rows and A, B, C... headers
      (cl-loop for row in data-rows
               for row-idx from 0 do
        (dotimes (i num-columns)
          (when (and (< i (length row))
                     (not (let ((merge (grid-table--merge-at merges row-idx i)))
                            (and merge
                                 (or (< (nth 1 merge) (nth 3 merge))
                                     (not (and (= row-idx (nth 0 merge))
                                               (= i (nth 1 merge)))))))))
            (let* ((cell-content (format "%s" (or (nth i row) "")))
                   (content-width 
                      (if (grid-table--extract-image-path cell-content)
                         ;; Use configured width for images
                         grid-table-image-column-width
                       ;; Calculate width considering newlines - use the longest line
                       (apply #'max 0 (mapcar #'string-width (split-string cell-content "\n" t ""))))))
              ;; Update width based on cell content
              (setf (nth i widths) (max (nth i widths) content-width))))))

      ;; 3. Apply max width constraint
      (dotimes (i num-columns)
        (setf (nth i widths) (min max-width (nth i widths))))

      ;; 4. (Future) Adjust total width to fit window size.
      ;; For now, we just return the calculated widths.

      widths)))

(defun grid-table--wrap-text (text width)
  "Wrap TEXT into a list of strings, each fitting within WIDTH.
Always breaks at newlines. If a newline coincides with where the text
would break anyway, only makes one break. Newlines reset line length to 0."
  (let ((result-lines '()))
    ;; First, split by explicit newlines
    (dolist (line-segment (split-string text "\n" t ""))
      ;; Handle each segment separately
      (if (<= (string-width line-segment) width)
          ;; Segment fits within width, add it directly
          (push line-segment result-lines)
        ;; Segment needs wrapping
        (let ((remaining-text line-segment))
          (while (> (string-width remaining-text) width)
            (let* ((break-pos (or (cl-loop for i from (min (length remaining-text) width) downto 1
                                           when (<= (string-width remaining-text 0 i) width)
                                           return i)
                                  1))
                   ;; Try to find a natural break point (space)
                   (space-pos (or (cl-position ?\s remaining-text :from-end t :end break-pos)
                                  break-pos)))
              (push (substring remaining-text 0 space-pos) result-lines)
              (setq remaining-text (string-trim (substring remaining-text space-pos)))))
          ;; Add the remaining part of this segment
          (when (not (string-empty-p remaining-text))
            (push remaining-text result-lines)))))
    ;; Handle case where original text was empty or contained only newlines
    (if (null result-lines)
        (list "")
      (nreverse result-lines))))

(defun grid-table--pad-string (text width)
  "Pad TEXT with spaces to fit WIDTH."
  (let* ((text-str (if (stringp text) text (format "%s" text)))
         (padding (- width (string-width text-str))))
    (if (> padding 0)
        (concat text-str (make-string padding ?\s))
      text-str)))

(defun grid-table--extract-image-path (text)
  "Extract an image file path from TEXT.
Handles absolute paths, paths with `~`, and org-style links.
Returns the valid, existing path if found, otherwise nil."
  (when (stringp text)
    (save-match-data
      (let ((case-fold-search t) ; Match extensions case-insensitively
            (path
             (cond
              ;; Case 1: Org-style file link: [[file:/path/to/image.png]]
              ((string-match "\\[\\[file:\\([^]]+\\)\\]\\]" text)
               (match-string 1 text))
              ;; Case 2: Bare absolute path or path with ~
              ((string-match
                "\\b\\([~/][^[:space:]'\"()]*\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\|bmp\\)\\b\\)" text)
               (match-string 1 text))
              (t nil))))
        (when path
          (let ((expanded-path (expand-file-name path)))
            (when (file-exists-p expanded-path)
              expanded-path)))))))

(defun grid-table--get-exact-line-height ()
  "Get the exact line height (in pixels) for the current environment, including frame-level line-spacing."
  (let* ((char-height (frame-char-height))
         (buffer-line-spacing (or line-spacing 0))
         (frame-line-spacing (or (frame-parameter nil 'line-spacing) 0))
         ;; Use larger line-spacing value
         (effective-line-spacing (max buffer-line-spacing frame-line-spacing))
         (total-line-height (+ char-height effective-line-spacing)))
    ;; Ensure slice height equals actual line height, no gaps
    total-line-height))

(defun grid-table--compute-table-width (data-source)
  "Compute the total table width for DATA-SOURCE, for title alignment."
  (let* ((get-col-count-fn (gethash :get-column-count data-source))
         (get-row-count-fn (gethash :get-row-count data-source))
         (get-computed-fn (gethash :get-computed-value-at data-source))
         (get-header-fn (gethash :get-header-value data-source))
         (num-rows (funcall get-row-count-fn data-source))
         (num-cols (funcall get-col-count-fn data-source))
         (merges (grid-table--classic-merges data-source))
         (headers (cl-loop for c from 0 below num-cols
                           collect (funcall get-header-fn data-source c)))
         (all-rows (cl-loop for r from 0 below num-rows
                            collect (cl-loop for c from 0 below num-cols
                                             collect (grid-get-display-value-at data-source r c))))
         (widths (grid-table--calculate-column-widths headers all-rows merges)))
    (let* ((max-row-number-width (string-width (number-to-string num-rows)))
           (row-header-width (max 3 (+ 1 max-row-number-width))))
      (+ row-header-width (apply #'+ widths) (* 3 num-cols) 2))))

(defun grid-table--get-loadable-image-source (file &optional force-conversion)
  "Find a loadable image source.
It checks the original file, and if Emacs doesn't support it, it tries to convert it to PNG using 'sips' on macOS.
Returns a list '(FILE-PATH TYPE)' on success, nil on failure.
This version does not use condition-case for error handling."
  (let ((normalized-file (expand-file-name file)))
    (when (file-exists-p normalized-file)
      (or
       ;; Strategy 1: Check for native support (png, jpeg, etc.)
       (let ((native-type (and (not force-conversion) (image-type-from-file-name normalized-file))))
         (when (and native-type (image-type-available-p native-type))
           (list normalized-file native-type)))
       ;; Strategy 2: Check for ImageMagick support, which handles webp etc.
       (when (and (not force-conversion) (image-type-available-p 'imagemagick))
         (list normalized-file 'imagemagick))
       ;; Strategy 3: On macOS, try to convert to PNG using 'sips'
       (when (eq system-type 'darwin)
         (let* ((temp-file (make-temp-file "gv-img-" nil ".png"))
                (exit-code (call-process "sips" nil nil nil "-s" "format" "png" normalized-file "--out" temp-file)))
           (and (zerop exit-code) (file-exists-p temp-file)
                (list temp-file 'png))))))))

(defvar grid-table--debug-slices nil
  "Output each slice's position and size when non-nil, for debugging.")

(defun grid-table--cell-lines (value width)
  "Render VALUE into display lines fitting WIDTH."
  (let* ((content (cond
                   ((and (listp value) (plist-get value :grid-chart)) "[Chart]")
                   ((stringp value) value)
                   (t (format "%s" value))))
         (image-path (grid-table--extract-image-path content)))
    (if (not image-path)
        (grid-table--wrap-text content width)
      (with-temp-buffer
        (if-let* ((source-info (ignore-errors
                                 (grid-table--get-loadable-image-source image-path)))
                  (source-file (car source-info))
                  (source-type (cadr source-info))
                  (target-px-w (* width (frame-char-width)))
                  (target-px-h (* grid-table-image-target-char-height
                                  (grid-table--get-exact-line-height)))
                  (image (create-image source-file source-type nil
                                       :width target-px-w :height target-px-h
                                       :ascent 'center)))
            (insert-sliced-image image " " nil
                                 grid-table-image-target-char-height width)
          (insert (format "[IMG ERR: %s]"
                          (file-name-nondirectory image-path))))
        (let ((lines (split-string (buffer-string) "\n" t)))
          (if (> (length lines) grid-table-image-target-char-height)
              (cl-subseq lines 0 grid-table-image-target-char-height)
            (if (and (= (length lines) 1) (string= (car lines) ""))
                (list "")
              lines)))))))

(defun grid-table--display-line-count (lines)
  "Return the height contribution of display LINES."
  (if (and (= (length lines) 1) (string= (car lines) ""))
      0
    (length lines)))

(defun grid-table--cell-properties
    (top left bottom right slice-row slice-col)
  "Return text properties for a rendered cell slice.
Columns are display coordinates, including the row-header column."
  (list 'grid-cell t
        'grid-display-row-idx top
        'grid-display-col-idx left
        'grid-display-col-end-idx right
        'grid-display-row-start top
        'grid-display-row-end bottom
        'grid-display-col-start left
        'grid-display-col-end right
        'grid-display-anchor-row top
        'grid-display-anchor-col left
        'grid-display-slice-row slice-row
        'grid-display-slice-col slice-col))

(defun grid-table--draw-column-header-row
    (headers widths row-header-width)
  "Draw the single ABC header row."
  (let ((row grid-table--col-header-row-idx))
    (format
     "│%s│%s│"
     (apply #'propertize
            (make-string row-header-width ?\s)
            (grid-table--cell-properties row 0 row 0 row 0))
     (string-join
      (cl-loop for header in headers
               for width in widths
               for col from 1
               collect
               (apply #'propertize
                      (format " %s "
                              (grid-table--pad-string header width))
                      (grid-table--cell-properties
                       row col row col row col)))
      "│"))))

(defun grid-table--build-layout (rows widths merges)
  "Build the cached row layout for ROWS, WIDTHS, and MERGES.
WIDTHS excludes the row-header width."
  (let* ((row-count (length rows))
         (base-heights (make-vector row-count 1))
         (extras (make-vector row-count 0))
         (nodes (make-hash-table :test #'equal)))
    (dotimes (row row-count)
      (dotimes (col (length widths))
        (let ((merge (grid-table--merge-at merges row col)))
          (when (or (null merge)
                    (and (= row (nth 0 merge))
                         (= col (nth 1 merge))))
            (let* ((top (if merge (nth 0 merge) row))
                   (left (if merge (nth 1 merge) col))
                   (bottom (if merge (nth 2 merge) row))
                   (right (if merge (nth 3 merge) col))
                   (span-width
                    (+ (apply #'+ (cl-subseq widths left (1+ right)))
                       (* 3 (- right left))))
                   (lines (grid-table--cell-lines
                           (nth col (nth row rows)) span-width))
                   (node (list :top top :left left
                               :bottom bottom :right right
                               :width span-width :lines lines)))
              (puthash (cons top left) node nodes)
              (when (= top bottom)
                (aset base-heights row
                      (max (aref base-heights row)
                           (grid-table--display-line-count lines)))))))))
    ;; All proposals read the same base heights; only the per-row max is kept.
    (maphash
     (lambda (_key node)
       (let ((top (plist-get node :top))
             (bottom (plist-get node :bottom)))
         (when (< top bottom)
           (let* ((span (1+ (- bottom top)))
                  (required (grid-table--display-line-count
                             (plist-get node :lines)))
                  (base-total
                   (cl-loop for row from top to bottom
                            sum (aref base-heights row)))
                  (deficit (max 0 (- required base-total)))
                  (share (/ deficit span))
                  (remainder (% deficit span)))
             (cl-loop for row from top to bottom
                      for offset from 0
                      for proposal = (+ share (if (< offset remainder) 1 0))
                      do (aset extras row
                               (max (aref extras row) proposal)))))))
     nodes)
    (let ((heights (make-vector row-count 1)))
      (dotimes (row row-count)
        (aset heights row
              (+ (aref base-heights row) (aref extras row))))
      (maphash
       (lambda (key node)
         (when (< (plist-get node :top) (plist-get node :bottom))
           (let* ((height
                   (cl-loop for row from (plist-get node :top)
                            to (plist-get node :bottom)
                            sum (aref heights row)))
                  (padding
                   (/ (- height
                         (grid-table--display-line-count
                          (plist-get node :lines)))
                      2)))
             (puthash key (plist-put node :top-padding padding) nodes))))
       nodes)
      (list :rows rows :widths widths :merges merges
            :heights heights :nodes nodes))))

(defun grid-table--layout-node-at (layout row col)
  "Return the layout node covering ROW and COL."
  (let* ((merge (grid-table--merge-at (plist-get layout :merges) row col))
         (key (if merge
                  (cons (nth 0 merge) (nth 1 merge))
                (cons row col))))
    (gethash key (plist-get layout :nodes))))

(defun grid-table--vertical-line-at (layout node row line)
  "Return NODE content for ROW and physical LINE in LAYOUT."
  (let* ((heights (plist-get layout :heights))
         (offset
          (+ (cl-loop for prior from (plist-get node :top) below row
                      sum (aref heights prior))
             line))
         (content-index (- offset (or (plist-get node :top-padding) 0))))
    (when (>= content-index 0)
      (nth content-index (plist-get node :lines)))))

(defun grid-table--draw-layout-row (layout row row-header-width)
  "Draw logical ROW from LAYOUT."
  (let* ((widths (plist-get layout :widths))
         (height (aref (plist-get layout :heights) row))
         lines)
    (dotimes (line height)
      (let ((col 0)
            parts)
        (while (< col (length widths))
          (let* ((node (grid-table--layout-node-at layout row col))
                 (left (plist-get node :left))
                 (right (plist-get node :right))
                 (vertical (< (plist-get node :top)
                              (plist-get node :bottom)))
                 (content
                  (or (if vertical
                          (grid-table--vertical-line-at
                           layout node row line)
                        (nth line (plist-get node :lines)))
                      ""))
                 (padded
                  (grid-table--pad-string content (plist-get node :width)))
                 (display-left (1+ left))
                 (display-right (1+ right)))
            (push
             (apply #'propertize
                    (format " %s " padded)
                    (grid-table--cell-properties
                     (plist-get node :top) display-left
                     (plist-get node :bottom) display-right
                     row display-left))
             parts)
            (setq col (1+ right))))
        (let* ((header (if (= line 0)
                           (number-to-string (1+ row))
                         " "))
               (header-text
                (apply #'propertize
                       (grid-table--pad-string header row-header-width)
                       (grid-table--cell-properties
                        row 0 row 0 row 0))))
          (push (format "│%s│%s│"
                        header-text
                        (string-join (nreverse parts) "│"))
                lines))))
    (string-join (nreverse lines) "\n")))

(defun grid-table--vertical-edge-p (merges row boundary column-count)
  "Return non-nil when ROW has a vertical edge at full-grid BOUNDARY."
  (or (= boundary 0)
      (= boundary 1)
      (= boundary (1+ column-count))
      (let ((data-boundary (1- boundary)))
        (not
         (cl-some
          (lambda (merge)
            (and (= row (nth 0 merge))
                 (= row (nth 2 merge))
                 (< (nth 1 merge) data-boundary)
                 (<= data-boundary (nth 3 merge))))
          merges)))))

(defun grid-table--vertical-merge-across-boundary (merges upper-row col)
  "Return a vertical merge spanning below UPPER-ROW at COL."
  (cl-find-if
   (lambda (merge)
     (and (= (nth 1 merge) (nth 3 merge))
          (= col (nth 1 merge))
          (<= (nth 0 merge) upper-row)
          (< upper-row (nth 2 merge))))
   merges))

(defun grid-table--junction-char (up down left right)
  "Return the Unicode junction for the four incident edge booleans."
  (cond
   ((and up down left right) "┼")
   ((and up down left) "┤")
   ((and up down right) "├")
   ((and up left right) "┴")
   ((and down left right) "┬")
   ((and up right) "└")
   ((and up left) "┘")
   ((and down right) "┌")
   ((and down left) "┐")
   ((or up down) "│")
   ((or left right) "─")
   (t " ")))

(defun grid-table--draw-layout-separator
    (widths merges upper-row lower-row)
  "Draw the separator between UPPER-ROW and LOWER-ROW.
Nil denotes the outside of the table; row -1 is the ABC header."
  (let* ((column-count (1- (length widths)))
         (segments
          (cl-loop for slot from 0 below (length widths)
                   collect
                   (if (= slot 0)
                       (list t nil)
                     (let ((vertical
                            (and (integerp upper-row)
                                 (>= upper-row 0)
                                 (integerp lower-row)
                                 (= lower-row (1+ upper-row))
                                 (grid-table--vertical-merge-across-boundary
                                  merges upper-row (1- slot)))))
                       (list (not vertical) vertical)))))
         result)
    (cl-loop
     for boundary from 0 to (length widths) do
     (let* ((up (and (integerp upper-row)
                     (grid-table--vertical-edge-p
                      merges upper-row boundary column-count)))
            (down (and (integerp lower-row)
                       (grid-table--vertical-edge-p
                        merges lower-row boundary column-count)))
            (left (and (> boundary 0)
                       (car (nth (1- boundary) segments))))
            (right (and (< boundary (length widths))
                        (car (nth boundary segments)))))
       (setq result
             (concat result
                     (grid-table--junction-char up down left right))))
     (when (< boundary (length widths))
       (let* ((slot-width (+ (nth boundary widths)
                             (if (= boundary 0) 0 2)))
              (segment (nth boundary segments))
              (draw (car segment))
              (vertical (cadr segment))
              (text (make-string slot-width (if draw ?─ ?\s))))
         (when vertical
           (let ((display-col boundary))
             (setq text
                   (apply #'propertize text
                          (grid-table--cell-properties
                           (nth 0 vertical) display-col
                           (nth 2 vertical) display-col
                           upper-row display-col)))))
         (setq result (concat result text)))))
    result))

(defun grid-table--col-number-to-letter (col-num)
  "Converts a 0-based column number to an Excel-style letter (e.g., 0 -> A, 25 -> Z, 26 -> AA)."
  (let ((result ""))
    (while (>= col-num 0)
      (setq result (concat (char-to-string (+ ?A (mod col-num 26))) result))
      (setq col-num (1- (/ col-num 26))))
    result))

(defun grid-table-text-render (source)
  "Return the grid of the data source SOURCE as a table of plain text.
The table has box-drawing borders, a row of column letters and a column
of row numbers; long values are wrapped and merges along a row or a
column are drawn as one cell.  Nothing but SOURCE is read: no buffer,
window or renderer is involved."
  (let* ((get-col-count-fn (gethash :get-column-count source))
         (get-row-count-fn (gethash :get-row-count source))
         (get-header-fn (gethash :get-header-value source))
         (num-rows (funcall get-row-count-fn source))
         (num-cols (funcall get-col-count-fn source))
         (merges (grid-table--classic-merges source))
         (headers (cl-loop for c from 0 below num-cols
                           collect (funcall get-header-fn source c)))
         (all-rows (cl-loop for r from 0 below num-rows
                            collect (cl-loop for c from 0 below num-cols ; Data model rows are 0-indexed
                                             collect (grid-get-display-value-at source r c))))
         (data-widths
          (grid-table--calculate-column-widths headers all-rows merges))
         (col-header-data (cl-loop for c from 0 below num-cols
                                   collect (grid-table--col-number-to-letter c))))
    (let* ((max-row-number-width (string-width (number-to-string num-rows)))
           (row-header-width (max 3 (+ 1 max-row-number-width)))
           (widths (cons row-header-width data-widths))
           (layout (grid-table--build-layout
                    all-rows data-widths merges)))
      (with-temp-buffer
        (insert (grid-table--draw-layout-separator
                 widths merges nil grid-table--col-header-row-idx)
                "\n"
                (grid-table--draw-column-header-row
                 col-header-data data-widths row-header-width)
                "\n"
                (grid-table--draw-layout-separator
                 widths merges grid-table--col-header-row-idx
                 (and (> num-rows 0) 0))
                "\n")
        (cl-loop for row from 0 below num-rows do
                 (insert (grid-table--draw-layout-row
                          layout row row-header-width)
                         "\n"
                         (grid-table--draw-layout-separator
                          widths merges row
                          (and (< row (1- num-rows)) (1+ row)))
                         "\n"))
        (buffer-string)))))

(provide 'grid-table-text)

;;; grid-table-text.el ends here
