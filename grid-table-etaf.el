;;; grid-table-etaf.el --- ETAF renderer for grid-table -*- lexical-binding: t -*-

;;; Commentary:

;; Renders a grid-table data source through ETAF (https://github.com/emacs-taf)
;; instead of drawing box characters into the buffer.  Only the rows and
;; columns that fit the window are mounted, so the cost of moving, scrolling
;; and editing does not grow with the size of the sheet.
;;
;; This file is loaded on demand by `grid-table--resolve-renderer' when
;; `grid-table-renderer' is `etaf'.  It needs the ecss, ebox and etaf
;; repository roots on `load-path'.  Developed against ecss 9a4270e,
;; ebox 99cdf0d and etaf 2e08bd9.
;;
;; The command layer in grid-table.el is shared with the classic renderer:
;; `grid-table-get-cell-at-point', `grid-table-refresh', `grid-table-edit-cell'
;; and the navigation commands dispatch here in an ETAF buffer.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'etaf)
(require 'grid-table)

(defface grid-table-etaf-header-face
  '((t :inherit mode-line-inactive))
  "Face whose colors paint the column letter row and the row number column."
  :group 'grid-table)

(defface grid-table-etaf-border-face
  '((t :inherit shadow))
  "Face whose foreground paints the grid lines."
  :group 'grid-table)

(defface grid-table-etaf-cursor-face
  '((t :inherit highlight))
  "Face whose colors paint the current cell."
  :group 'grid-table)

(defface grid-table-etaf-selection-face
  '((t :inherit region))
  "Face whose colors paint the selected range."
  :group 'grid-table)

(defcustom grid-table-etaf-min-column-width 5
  "Narrowest content width of a column, in characters."
  :type 'integer
  :group 'grid-table)

(defcustom grid-table-etaf-max-column-width 40
  "Widest content width of a column, in characters."
  :type 'integer
  :group 'grid-table)

(cl-defstruct (grid-table-etaf--state
               (:constructor grid-table-etaf--state-create))
  buffer top left cursor anchor tick editing widths session
  ;; Undo history: lists of (cells (ROW COL OLD NEW)...) and (model OLD NEW).
  undo redo model
  ;; Column widths set by dragging, as a hash from column to width, and the
  ;; width a resize drag started from.
  manual resize-start
  ;; Filters, as an alist from column to (OP VALUE), and the vector of
  ;; model rows they leave shown; nil while nothing is filtered.
  filters shown)

(defvar-local grid-table-etaf--state nil
  "Renderer state of the current ETAF grid buffer.")

(defvar grid-table-etaf-mount-options nil
  "Options passed to `etaf-mount' when a grid is shown.
A fixed size is (:viewport-width PIXELS :viewport-height LINES); nil
follows the window.")

(defconst grid-table-etaf--fallback-viewport '(80 . 30)
  "Viewport in characters and lines used before a size is known.")

;;;----------------------------------------------------------------------
;;; Geometry
;;;----------------------------------------------------------------------

(defun grid-table-etaf--runtime (&optional buffer)
  "Return the mounted Runtime of BUFFER, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer) (etaf-runtime-for-buffer buffer))))

(defun grid-table-etaf--char-width ()
  "Return the pixel width of one `ch' unit."
  (max 1 (ebox-string-pixel-width "0")))

(defun grid-table-etaf--viewport (state)
  "Return STATE's viewport as (PIXELS . LINES)."
  (let* ((runtime (grid-table-etaf--runtime
                   (grid-table-etaf--state-buffer state)))
         (viewport (and runtime (etaf-runtime-viewport runtime)))
         (width (car-safe viewport))
         (height (cdr-safe viewport)))
    (cons (if (and (integerp width) (> width 0))
              width
            (* (car grid-table-etaf--fallback-viewport)
               (grid-table-etaf--char-width)))
          (if (and (integerp height) (> height 0))
              height
            (cdr grid-table-etaf--fallback-viewport)))))

(defun grid-table-etaf--source (state)
  "Return the data source shown by STATE."
  (buffer-local-value 'grid-table--data-source
                      (grid-table-etaf--state-buffer state)))

(defvar grid-table-etaf--chart-cache
  (make-hash-table :test 'eq :weakness 'key)
  "Drawings of chart values, so that a chart is drawn once per value.")

(defun grid-table-etaf--chart-p (value)
  "Return non-nil when computed VALUE is a chart."
  (and (consp value) (plist-get value :grid-chart)))

(defun grid-table-etaf--chart-text (chart)
  "Return the text drawing of CHART, or a placeholder when it fails."
  (or (gethash chart grid-table-etaf--chart-cache)
      (puthash chart
               (condition-case nil
                   (string-trim-right (grid-table-chart-content chart) "\n+")
                 (error "[Chart]"))
               grid-table-etaf--chart-cache)))

(defun grid-table-etaf--display-text (value &optional format)
  "Return the text shown for computed VALUE under FORMAT."
  (cond ((null value) "")
        ((grid-table-etaf--chart-p value) (grid-table-etaf--chart-text value))
        (t (grid-format-display value format))))

(defconst grid-table-etaf--max-chart-width 120
  "Widest column a chart may ask for, in characters.")

(defun grid-table-etaf--value-width (value &optional format)
  "Return the column width computed VALUE asks for under FORMAT, in characters.
A chart is drawn at its own size, so it is not held to the width limit
of ordinary text, which wraps instead."
  (let ((width (grid-table-etaf--text-width
                (grid-table-etaf--display-text value format))))
    (if (grid-table-etaf--chart-p value)
        (max grid-table-etaf-min-column-width
             (min grid-table-etaf--max-chart-width width))
      (grid-table-etaf--clamp-width width))))

(defun grid-table-etaf--text-width (text)
  "Return the column width TEXT asks for, in characters."
  (if (grid-table--extract-image-path text)
      grid-table-image-column-width
    (apply #'max 0 (mapcar #'string-width (split-string text "\n")))))

(defun grid-table-etaf--clamp-width (width)
  "Clamp WIDTH to the configured column width range."
  (max grid-table-etaf-min-column-width
       (min grid-table-etaf-max-column-width width)))

(defun grid-table-etaf--compute-widths (source)
  "Return a vector of content widths for every column of SOURCE.
Covered cells and horizontally merged anchors do not widen a column."
  (let* ((rows (grid-get-row-count source))
         (cols (grid-get-column-count source))
         (merges (grid-get-merges source))
         (widths (make-vector cols 0)))
    (dotimes (col cols)
      (aset widths col
            (string-width (grid-table--col-number-to-letter col))))
    (dotimes (row rows)
      (dotimes (col cols)
        (let ((merge (and merges (grid-table--merge-at merges row col))))
          (unless (and merge
                       (or (/= (nth 1 merge) (nth 3 merge))
                           (/= row (nth 0 merge))))
            (let ((width (grid-table-etaf--value-width
                          (grid-get-computed-value-at source row col)
                          (grid-table-cell-format source row col))))
              (when (> width (aref widths col))
                (aset widths col width)))))))
    (dotimes (col cols)
      (aset widths col (max grid-table-etaf-min-column-width
                            (aref widths col))))
    widths))

(defun grid-table-etaf--fit-widths (state)
  "Recompute STATE's column widths, keeping the ones set by dragging."
  (let ((widths (grid-table-etaf--compute-widths
                 (grid-table-etaf--source state)))
        (manual (grid-table-etaf--manual state)))
    (when manual
      (maphash (lambda (col width)
                 (when (< col (length widths))
                   (aset widths col width)))
               manual))
    (setf (grid-table-etaf--state-widths state) widths)))

(defun grid-table-etaf--row-header-width (source)
  "Return the content width of SOURCE's row number column."
  (max 3 (1+ (length (number-to-string (grid-get-row-count source))))))

(defun grid-table-etaf--visible-cols (state left)
  "Return the model columns STATE shows when the first one is LEFT."
  (let* ((source (grid-table-etaf--source state))
         (widths (grid-table-etaf--state-widths state))
         (cols (min (grid-get-column-count source) (length widths)))
         (char (grid-table-etaf--char-width))
         (budget (- (car (grid-table-etaf--viewport state))
                    (* char (+ 2 (grid-table-etaf--row-header-width source)))
                    1))
         (col left)
         result)
    (while (and (< col cols)
                (let ((need (1+ (* char (+ 2 (aref widths col))))))
                  (when (or (null result) (<= need budget))
                    (setq budget (- budget need))
                    t)))
      (push col result)
      (setq col (1+ col)))
    (nreverse result)))

(defun grid-table-etaf--cell-lines (value width &optional format)
  "Estimate how many lines computed VALUE takes in a cell WIDTH wide.
FORMAT is the format the value is shown with."
  (let ((text (grid-table-etaf--display-text value format)))
    (cond ((grid-table-etaf--chart-p value)
           (length (split-string text "\n")))
          ((grid-table--extract-image-path text)
           grid-table-image-target-char-height)
          ((string-empty-p text) 1)
          (t (length (grid-table--wrap-text text width))))))

(defun grid-table-etaf--row-lines (state row cols merges)
  "Estimate the height of ROW over COLS in STATE, in lines."
  (let ((source (grid-table-etaf--source state))
        (widths (grid-table-etaf--state-widths state))
        (lines 1))
    (dolist (col cols)
      (unless (and merges (grid-table--merge-at merges row col))
        (setq lines
              (max lines
                   (grid-table-etaf--cell-lines
                    (grid-get-computed-value-at source row col)
                    (aref widths col)
                    (grid-table-cell-format source row col))))))
    lines))

(defun grid-table-etaf--title-lines (state)
  "Return how many lines STATE's title takes above the grid."
  (let ((title (buffer-local-value 'grid-table--title
                                   (grid-table-etaf--state-buffer state))))
    (if (and title (not (string-empty-p title))) 2 0)))

(defun grid-table-etaf--row-count (state)
  "Return how many rows STATE shows once its filters are applied."
  (let ((shown (grid-table-etaf--state-shown state)))
    (if shown
        (length shown)
      (grid-get-row-count (grid-table-etaf--source state)))))

(defun grid-table-etaf--row-at (state position)
  "Return the model row at POSITION among the rows STATE shows.
POSITION is clamped to the shown rows."
  (let* ((shown (grid-table-etaf--state-shown state))
         (position (max 0 (min (1- (grid-table-etaf--row-count state))
                               position))))
    (if shown (aref shown position) position)))

(defun grid-table-etaf--row-position (state row)
  "Return the position of model ROW among the rows STATE shows.
A filtered-out row gives the position of the next shown row, or of the
last one when none follows."
  (let ((shown (grid-table-etaf--state-shown state)))
    (if (null shown)
        (max 0 (min (1- (grid-table-etaf--row-count state)) row))
      (let ((low 0) (high (length shown)))
        (while (< low high)
          (let ((middle (/ (+ low high) 2)))
            (if (< (aref shown middle) row)
                (setq low (1+ middle))
              (setq high middle))))
        (min low (1- (length shown)))))))

(defun grid-table-etaf--row-shown-p (state row)
  "Return non-nil when model ROW passes STATE's filters."
  (or (null (grid-table-etaf--state-shown state))
      (= (grid-table-etaf--row-at
          state (grid-table-etaf--row-position state row))
         row)))

(defun grid-table-etaf--step-row (state row count)
  "Return the shown row COUNT positions after model ROW in STATE."
  (grid-table-etaf--row-at
   state (+ (grid-table-etaf--row-position state row) count)))

(defun grid-table-etaf--apply-filters (state)
  "Recompute which rows of STATE its filters leave shown.
The first row holds the column titles and is always shown."
  (let* ((source (grid-table-etaf--source state))
         (filters (grid-table-etaf--state-filters state)))
    (setf (grid-table-etaf--state-shown state)
          (and filters
               (vconcat
                (cons 0
                      (cl-loop
                       for row from 1 below (grid-get-row-count source)
                       when (cl-every
                             (lambda (filter)
                               (grid-table-conditional-match-p
                                (nth 1 filter) (nth 2 filter)
                                (grid-get-computed-value-at
                                 source row (car filter))))
                             filters)
                       collect row)))))))

(defun grid-table-etaf--visible-rows (state top cols)
  "Return the model rows STATE shows over COLS when the first one is TOP."
  (let* ((source (grid-table-etaf--source state))
         (count (grid-table-etaf--row-count state))
         (merges (grid-get-merges source))
         ;; The letter row, the formula bar and the status bar take a line each.
         (budget (- (cdr (grid-table-etaf--viewport state))
                    3 (grid-table-etaf--title-lines state)))
         (position (grid-table-etaf--row-position state top))
         result)
    (while (and (< position count)
                (let ((need (grid-table-etaf--row-lines
                             state (grid-table-etaf--row-at state position)
                             cols merges)))
                  (when (or (null result) (<= need budget))
                    (setq budget (- budget need))
                    t)))
      (push (grid-table-etaf--row-at state position) result)
      (setq position (1+ position)))
    (nreverse result)))

;;;----------------------------------------------------------------------
;;; View
;;;----------------------------------------------------------------------

(defun grid-table-etaf--face-color (face attribute)
  "Return FACE's color for ATTRIBUTE as a string, or nil."
  (let ((color (face-attribute face attribute nil t)))
    (and (stringp color) color)))

(defun grid-table-etaf--numeric-p (value)
  "Return non-nil when VALUE should be right-aligned as a number."
  (or (numberp value)
      (and (stringp value)
           (not (grid-format-text-p value))
           (string-match-p
            "\\`[ \t]*[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)%?[ \t]*\\'"
            value))))

(defvar grid-table-etaf--slot-refs (make-hash-table :test 'equal)
  "Host references of cell slots, keyed by (ROW-SLOT . COL-SLOT).")

(defun grid-table-etaf--slot-ref (row-slot col-slot)
  "Return the Host reference of the cell at ROW-SLOT and COL-SLOT.
A slot is a position in the window, not in the sheet, so the set of
references stays as small as the window."
  (let ((key (cons row-slot col-slot)))
    (or (gethash key grid-table-etaf--slot-refs)
        (let ((ref (intern (format "grid-table-etaf-cell-%d-%d"
                                   row-slot col-slot))))
          (put ref 'grid-table-etaf-slot key)
          (puthash key ref grid-table-etaf--slot-refs)))))

(defun grid-table-etaf--cell-at-gesture (state gesture)
  "Return the model (ROW . COL) under the pointer of GESTURE in STATE."
  (let* ((event (plist-get gesture :event))
         (position (and (consp event) (posn-point (event-start event))))
         (runtime (grid-table-etaf--runtime
                   (grid-table-etaf--state-buffer state)))
         (ref (and (integerp position) runtime
                   (etaf-host-ref-at
                    runtime position
                    (lambda (ref)
                      (and (symbolp ref)
                           (get ref 'grid-table-etaf-slot))))))
         (slot (and ref (get ref 'grid-table-etaf-slot))))
    (when slot
      (let* ((cols (grid-table-etaf--visible-cols
                    state (etaf-value (grid-table-etaf--state-left state))))
             (rows (grid-table-etaf--visible-rows
                    state (etaf-value (grid-table-etaf--state-top state))
                    cols))
             (row (nth (car slot) rows))
             (col (nth (cdr slot) cols)))
        (and row col (cons row col))))))

(defun grid-table-etaf--drag-cell (buffer row col)
  "Return the pointer handler of the cell at ROW and COL in BUFFER.
A press moves the cursor there, or extends the selection with shift
held; dragging extends the selection to the cell under the pointer."
  (lambda (gesture)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((state grid-table-etaf--state))
          (pcase (plist-get gesture :phase)
            ('start
             (grid-table-etaf-goto
              row col
              (memq 'shift (event-modifiers (plist-get gesture :event)))))
            ((or 'move 'end)
             (let ((cell (grid-table-etaf--cell-at-gesture state gesture)))
               (when (and cell
                          (not (equal cell (grid-table-etaf--cursor))))
                 (grid-table-etaf-goto (car cell) (cdr cell) t))))))))))

(defun grid-table-etaf--select (cursor anchor)
  "Select from ANCHOR to CURSOR, both (ROW . COL), without scrolling."
  (let ((state (grid-table-etaf--state-or-error)))
    (grid-table-etaf--finish-edit state)
    (etaf-batch
      (etaf-set-value (grid-table-etaf--state-cursor state) cursor)
      (etaf-set-value (grid-table-etaf--state-anchor state) anchor))
    (setq grid-table--nav-row (car cursor)
          grid-table--nav-col (1+ (cdr cursor)))
    (grid-table-etaf--sync-point state)))

(defun grid-table-etaf-select-column (col)
  "Select every cell of model column COL."
  (grid-table-etaf--select
   (cons 0 col)
   (cons (1- (grid-get-row-count grid-table--data-source)) col)))

(defun grid-table-etaf-select-row (row)
  "Select every cell of model ROW."
  (grid-table-etaf--select
   (cons row 0)
   (cons row (1- (grid-get-column-count grid-table--data-source)))))

(defun grid-table-etaf-select-all ()
  "Select every cell of the sheet."
  (interactive)
  (grid-table-etaf--select
   (cons 0 0)
   (cons (1- (grid-get-row-count grid-table--data-source))
         (1- (grid-get-column-count grid-table--data-source)))))

(defun grid-table-etaf--drag-header (buffer function)
  "Return a pointer handler calling FUNCTION in BUFFER when pressed."
  (lambda (gesture)
    (when (and (buffer-live-p buffer)
               (eq (plist-get gesture :phase) 'start))
      (with-current-buffer buffer
        (funcall function)))))

(defun grid-table-etaf--manual (state)
  "Return the table of STATE's column widths set by hand, or nil.
The data source keeps them when it can, so that they are saved with the
grid and follow their columns; otherwise they live as long as the view."
  (or (grid-get-column-widths (grid-table-etaf--source state))
      (grid-table-etaf--state-manual state)))

(defun grid-table-etaf-set-column-width (col width)
  "Give model column COL a content width of WIDTH characters.
The width is saved with the grid and moves with its column."
  (let* ((state (grid-table-etaf--state-or-error))
         (width (max 1 (min 200 width))))
    (unless (grid-set-column-width (grid-table-etaf--source state) col width)
      (unless (grid-table-etaf--state-manual state)
        (setf (grid-table-etaf--state-manual state) (make-hash-table)))
      (puthash col width (grid-table-etaf--state-manual state)))
    (aset (grid-table-etaf--state-widths state) col width)
    (grid-table-etaf--bump state)))

(defun grid-table-etaf--drag-resize (buffer col)
  "Return the pointer handler resizing column COL of BUFFER.
The column follows the pointer; a cancelled drag restores its width."
  (lambda (gesture)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((state grid-table-etaf--state)
               (widths (grid-table-etaf--state-widths state)))
          (pcase (plist-get gesture :phase)
            ('start
             (setf (grid-table-etaf--state-resize-start state)
                   (aref widths col)))
            ((or 'move 'end)
             (let ((start (grid-table-etaf--state-resize-start state)))
               (when start
                 (grid-table-etaf-set-column-width
                  col (+ start (round (car (plist-get gesture :delta))
                                      (grid-table-etaf--char-width)))))))
            ('cancel
             (let ((start (grid-table-etaf--state-resize-start state)))
               (when start
                 (grid-table-etaf-set-column-width col start))))))))))

(defun grid-table-etaf--direction (buffer)
  "Return the arrow key handler of BUFFER's sheet."
  (lambda (direction)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (pcase direction
          ('up (grid-table-etaf-move -1 0))
          ('down (grid-table-etaf-move 1 0))
          ('left (grid-table-etaf-move 0 -1))
          ('right (grid-table-etaf-move 0 1)))))
    'handled))

(defconst grid-table-etaf--keys
  '(("RET" grid-table-edit-cell "Edit cell")
    ("<f2>" grid-table-edit-cell "Edit cell")
    ("C-c C-r" grid-table-refresh "Refresh")
    ("C-c C-q" quit-window "Quit")
    ("C-n" grid-table-next-line "Next row")
    ("C-p" grid-table-previous-line "Previous row")
    ("C-f" grid-table-cell-right "Next column")
    ("C-b" grid-table-cell-left "Previous column")
    ("C-a" grid-table-move-beginning-of-line "First column")
    ("C-e" grid-table-move-end-of-line "Last column")
    ("M-<" grid-table-etaf-first-cell "First cell")
    ("M->" grid-table-etaf-last-cell "Last cell")
    ("<next>" grid-table-etaf-page-down "Page down")
    ("<prior>" grid-table-etaf-page-up "Page up")
    ("S-<down>" grid-table-etaf-extend-down "Extend selection down")
    ("S-<up>" grid-table-etaf-extend-up "Extend selection up")
    ("S-<right>" grid-table-etaf-extend-right "Extend selection right")
    ("S-<left>" grid-table-etaf-extend-left "Extend selection left")
    ("M-w" grid-table-etaf-copy "Copy")
    ("C-w" grid-table-etaf-cut "Cut")
    ("C-y" grid-table-etaf-paste "Paste")
    ("s-c" grid-table-etaf-copy "Copy")
    ("s-x" grid-table-etaf-cut "Cut")
    ("s-v" grid-table-etaf-paste "Paste")
    ("C-/" grid-table-etaf-undo "Undo")
    ("C-_" grid-table-etaf-undo "Undo")
    ("C-x u" grid-table-etaf-undo "Undo")
    ("s-z" grid-table-etaf-undo "Undo")
    ("C-?" grid-table-etaf-redo "Redo")
    ("C-M-_" grid-table-etaf-redo "Redo")
    ("s-Z" grid-table-etaf-redo "Redo")
    ("C-x h" grid-table-etaf-select-all "Select all")
    ("C-<next>" grid-table-sheet-next "Next sheet")
    ("C-<prior>" grid-table-sheet-previous "Previous sheet")
    ("C-c w n" grid-table-sheet-new "New sheet")
    ("C-c w r" grid-table-sheet-rename "Rename sheet")
    ("C-c w d" grid-table-sheet-delete "Delete sheet")
    ("C-c w s" grid-table-sheet-select "Go to sheet")
    ("C-c w <" grid-table-sheet-move-left "Move sheet left")
    ("C-c w >" grid-table-sheet-move-right "Move sheet right")
    ("C-c / f" grid-table-etaf-filter-column "Filter column")
    ("C-c / v" grid-table-etaf-filter-by-cell "Filter by this value")
    ("C-c / c" grid-table-etaf-clear-column-filter "Clear column filter")
    ("C-c / a" grid-table-etaf-clear-filters "Clear all filters")
    ("C-c f b" grid-table-etaf-toggle-bold "Bold")
    ("C-c f i" grid-table-etaf-toggle-italic "Italic")
    ("s-b" grid-table-etaf-toggle-bold "Bold")
    ("s-i" grid-table-etaf-toggle-italic "Italic")
    ("C-c f c" grid-table-etaf-set-text-color "Text color")
    ("C-c f g" grid-table-etaf-set-fill-color "Fill color")
    ("C-c f l" grid-table-etaf-align-left "Align left")
    ("C-c f e" grid-table-etaf-align-center "Align center")
    ("C-c f r" grid-table-etaf-align-right "Align right")
    ("C-c f x" grid-table-etaf-clear-format "Clear formatting")
    ("C-c f G" grid-table-etaf-format-general "General format")
    ("C-c f ," grid-table-etaf-format-number "Number format")
    ("C-c f p" grid-table-etaf-format-percent "Percent format")
    ("C-c f $" grid-table-etaf-format-currency "Currency format")
    ("C-c f d" grid-table-etaf-format-date "Date format")
    ("C-c f t" grid-table-etaf-format-text "Text format")
    ("C-c f +" grid-table-etaf-more-decimals "More decimals")
    ("C-c f -" grid-table-etaf-fewer-decimals "Fewer decimals")
    ("C-c f n" grid-table-etaf-add-conditional-format "Conditional format")
    ("C-c f N" grid-table-etaf-clear-conditional-formats
     "Clear conditional formats")
    ("DEL" grid-table-etaf-clear-cells "Clear cells")
    ("<deletechar>" grid-table-etaf-clear-cells "Clear cells")
    ("C-c t" grid-table-edit-title "Edit title")
    ("C-c r a" grid-table-add-row "Add row")
    ("C-c r d" grid-table-delete-row "Delete row")
    ("C-c c a" grid-table-add-column "Add column")
    ("C-c c d" grid-table-delete-column "Delete column")
    ("C-c C-w" grid-table-write-file "Save")
    ("C-c C-f" grid-table-find-file "Open")
    ("C-c s" grid-table-sort-column "Sort")
    ("C-c m" grid-table-etaf-merge "Merge")
    ("C-c u" grid-table-unmerge-cell "Unmerge")
    ("C-c v" grid-table-view-chart "View chart")
    ("C-c i" grid-table-insert-chart "Insert chart"))
  "Key bindings of the sheet, as ETAF `:keys' entries.
Printing keys are not commands: they start editing the cursor cell, see
`grid-table-etaf-typing-map'.")

(defvar grid-table-etaf-traverse-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'grid-table-next-cell)
    (define-key map [tab] #'grid-table-next-cell)
    (define-key map [backtab] #'grid-table-previous-cell)
    (define-key map (kbd "S-TAB") #'grid-table-previous-cell)
    (define-key map [wheel-down] #'grid-table-etaf-scroll-down)
    (define-key map [wheel-up] #'grid-table-etaf-scroll-up)
    map)
  "Keymap for the events ETAF and Ebox would otherwise consume.
ETAF `:keys' cannot bind TAB, and Ebox scrolls the window on wheel
events while the sheet scrolls by changing its first row.  This map is
installed ahead of ETAF's own in `emulation-mode-map-alists'.")

(defvar-local grid-table-etaf--traverse-keys nil
  "Non-nil in buffers where `grid-table-etaf-traverse-map' is active.")

(defvar grid-table-etaf-typing-map
  (let ((map (make-sparse-keymap)))
    ;; Ebox binds digits and SPC itself, so every ASCII key is bound here;
    ;; the remap catches other characters, such as input method text.
    (cl-loop for char from ?\s to ?~ do
             (define-key map (vector char) #'grid-table-etaf-self-insert))
    (define-key map [remap self-insert-command] #'grid-table-etaf-self-insert)
    map)
  "Keymap making printing keys start an edit of the cursor cell.
It is active while no cell is being edited.")

(defvar-local grid-table-etaf--typing-keys nil
  "Non-nil while `grid-table-etaf-typing-map' is active in the buffer.")

(defvar grid-table-etaf--emulation-alist
  (list (cons 'grid-table-etaf--typing-keys grid-table-etaf-typing-map)
        (cons 'grid-table-etaf--traverse-keys grid-table-etaf-traverse-map))
  "Entry of `emulation-mode-map-alists' carrying the sheet's own key maps.")

(defun grid-table-etaf--cell-node (state source row col span spec)
  "Return the View node of the cell at ROW and COL.
SPAN is nil or (ROW-SPAN . COL-SPAN).  SPEC is a plist carrying the grid
position, the node key and the paint state of the cell."
  (let* ((editing (plist-get spec :editing))
         (value (cond ((stringp editing) editing)
                      (editing (grid-table-edit-text source row col))
                      (t (grid-get-computed-value-at source row col))))
         (text (if editing
                   value
                 (grid-table-etaf--display-text
                  value (grid-table-cell-format source row col))))
         (image (and (not editing) (grid-table--extract-image-path text)))
         (face (cond ((plist-get spec :cursor) 'grid-table-etaf-cursor-face)
                     ((plist-get spec :selected)
                      'grid-table-etaf-selection-face)))
         (line (plist-get spec :line))
         (grid-row (plist-get spec :grid-row))
         (grid-col (plist-get spec :grid-col))
         (style (grid-table-effective-style source row col
                                            (plist-get spec :rules))))
    (etaf-node
     'box
     (list :key (plist-get spec :key)
           :grid-row (if span (list grid-row :span (car span)) grid-row)
           :grid-column (if span (list grid-col :span (cdr span)) grid-col)
           :padding '((lh 0) (ch 1))
           ;; A chart is a drawing: wrapping its lines would break it.
           :wrap-mode (and (not editing) (grid-table-etaf--chart-p value) 'none)
           :text-align (cond ((plist-get style :align))
                             ((and (not editing)
                                   (grid-table-etaf--numeric-p value))
                              'right)
                             (t 'left))
           ;; The cursor and the selection paint over a cell's own colors.
           :color (or (and face (grid-table-etaf--face-color face :foreground))
                      (plist-get style :color))
           :background-color
           (if face
               (grid-table-etaf--face-color face :background)
             (plist-get style :background))
           :border-right line
           :border-bottom line
           :ref (plist-get spec :ref)
           :on-drag (grid-table-etaf--drag-cell
                     (grid-table-etaf--state-buffer state) row col))
     (list
      (if image
          (etaf-node 'image
                     (list :src image
                           :columns (aref (grid-table-etaf--state-widths state)
                                          col)
                           :rows grid-table-image-target-char-height)
                     nil)
        ;; A separate key while editing gives the input session a fresh
        ;; text Host, so a cancelled edit leaves nothing behind.
        (etaf-node 'text
                   (append (list :key (if editing 'editing 'value)
                                 :font-weight (and (plist-get style :bold) 'bold)
                                 :font-style
                                 (and (plist-get style :italic) 'italic))
                           (and (plist-get spec :cursor)
                                (list :ref 'grid-table-etaf-cursor)))
                   (list text)))))))

(defun grid-table-etaf--header-node (key grid-row grid-col label align
                                         marked line &optional on-drag
                                         on-resize)
  "Return a header cell View node.
KEY names the node, GRID-ROW and GRID-COL place it, LABEL is its text
aligned by ALIGN.  MARKED paints it as part of the selection and LINE is
the grid line border.  ON-DRAG handles the pointer on the label.
ON-RESIZE, when non-nil, adds a handle on the right edge that takes the
pointer instead."
  (let* ((face (if marked
                   'grid-table-etaf-selection-face
                 'grid-table-etaf-header-face))
         (color (grid-table-etaf--face-color face :foreground))
         (background (grid-table-etaf--face-color face :background)))
    (etaf-node
     'flex
     (list :key key
           :grid-row grid-row
           :grid-column grid-col
           :color color
           :background-color background
           :border-right line
           :border-bottom line)
     (delq nil
           (list
            (etaf-node 'box
                       (list :key 'label
                             :flex-grow 1
                             :padding (if on-resize
                                          '((lh 0) (ch 0) (lh 0) (ch 1))
                                        '((lh 0) (ch 1)))
                             :text-align align
                             :on-drag on-drag)
                       (list (etaf-node 'text nil (list label))))
            (and on-resize
                 (etaf-node 'box
                            (list :key 'handle
                                  :width '(ch 1)
                                  :help-echo "Drag to resize the column"
                                  :on-drag on-resize)
                            (list (etaf-node 'text nil (list " "))))))))))

(defun grid-table-etaf--title-node (state)
  "Return the title View node of STATE, or nil without a title."
  (let* ((buffer (grid-table-etaf--state-buffer state))
         (title (buffer-local-value 'grid-table--title buffer)))
    (when (and title (not (string-empty-p title)))
      (etaf-node
       'box
       (list :key 'title
             :text-align (buffer-local-value 'grid-table-title-align buffer)
             :padding '((lh 0) (ch 1))
             :border-bottom
             (and (eq (buffer-local-value 'grid-table-title-decoration buffer)
                      'rule)
                  '(px 1))
             :margin-bottom '(lh 1))
       (list (etaf-node 'text '(:font-weight bold) (list title)))))))

(defun grid-table-etaf--address (row col)
  "Return the A1-style address of model ROW and COL."
  (format "%s%d" (grid-table--col-number-to-letter col) (1+ row)))

(defun grid-table-etaf--number (value)
  "Return computed VALUE as a number, or nil when it is not numeric."
  (cond ((numberp value) value)
        ((and (stringp value)
              (string-match-p
               "\\`[ \t]*[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)[ \t]*\\'"
               value))
         (string-to-number value))))

(defun grid-table-etaf--format-number (number)
  "Return NUMBER as text without a needless fraction."
  (if (or (integerp number) (= number (ftruncate number)))
      (format "%d" (truncate number))
    (string-trim-right (string-trim-right (format "%.4f" number) "0+") "\\.")))

(defun grid-table-etaf--formula-bar-node (source row col range line)
  "Return the formula bar View node.
It names the cell at ROW and COL of SOURCE, or RANGE when several cells
are selected, and shows the raw value of that cell.  LINE is the grid
line border."
  (let ((raw (grid-table-edit-text source row col))
        (face 'grid-table-etaf-header-face))
    (etaf-node
     'flex
     (list :key 'formula-bar :border-bottom line)
     (list
      (etaf-node
       'box
       (list :key 'address
             :width '(ch 14)
             :padding '((lh 0) (ch 1))
             :text-align 'center
             :wrap-mode 'none
             :overflow 'hidden
             :color (grid-table-etaf--face-color face :foreground)
             :background-color (grid-table-etaf--face-color face :background)
             :border-right line)
       (list (etaf-node
              'text nil
              (list (if range
                        (format "%s:%s"
                                (grid-table-etaf--address (nth 0 range)
                                                          (nth 1 range))
                                (grid-table-etaf--address (nth 2 range)
                                                          (nth 3 range)))
                      (grid-table-etaf--address row col))))))
      (etaf-node
       'box
       (list :key 'raw
             :flex-grow 1
             :height '(lh 1)
             :padding '((lh 0) (ch 1))
             :wrap-mode 'none
             :overflow 'hidden)
       (list (etaf-node
              'text nil
              (list (string-replace "\n" "⏎" raw)))))))))

(defun grid-table-etaf--status-text (state source range)
  "Return the status bar text for SOURCE as STATE shows it.
RANGE is the selected (TOP LEFT BOTTOM RIGHT), or nil for a single cell.
Rows hidden by a filter are left out of the figures."
  (if (null range)
      (if (grid-table-etaf--state-shown state)
          (format "%d of %d rows shown (filtered) × %d columns"
                  (1- (grid-table-etaf--row-count state))
                  (1- (grid-get-row-count source))
                  (grid-get-column-count source))
        (format "%d rows × %d columns"
                (grid-get-row-count source) (grid-get-column-count source)))
    (let ((count 0) (numbers 0) (sum 0))
      (cl-loop
       for row from (nth 0 range) to (nth 2 range)
       when (grid-table-etaf--row-shown-p state row) do
       (cl-loop
        for col from (nth 1 range) to (nth 3 range) do
        (let* ((value (grid-get-computed-value-at source row col))
               (number (grid-table-etaf--number value)))
          (unless (member value '(nil ""))
            (setq count (1+ count)))
          (when number
            (setq numbers (1+ numbers)
                  sum (+ sum number))))))
      (if (zerop numbers)
          (format "Count: %d" count)
        (format "Count: %d   Sum: %s   Average: %s"
                count
                (grid-table-etaf--format-number sum)
                (grid-table-etaf--format-number (/ (float sum) numbers)))))))

(defun grid-table-etaf--tab-node (buffer sheet current line)
  "Return the tab of SHEET for the sheet bar of BUFFER.
CURRENT is non-nil for the sheet being shown; LINE is the grid line
border.  Pressing the tab shows its sheet."
  (let ((face (if current
                  'grid-table-etaf-selection-face
                'grid-table-etaf-header-face)))
    (etaf-node
     'box
     (list :key (grid-workbook-sheet-name sheet)
           :padding '((lh 0) (ch 1))
           :wrap-mode 'none
           :font-weight (and current 'bold)
           :color (grid-table-etaf--face-color face :foreground)
           :background-color (grid-table-etaf--face-color face :background)
           :border-right line
           :on-drag
           (lambda (gesture)
             (when (and (buffer-live-p buffer)
                        (eq (plist-get gesture :phase) 'start))
               ;; Showing a sheet mounts the buffer anew, which must not
               ;; happen while the pointer is being handled.
               (run-at-time 0 nil
                            (lambda ()
                              (when (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (grid-table-switch-to-sheet sheet))))))))
     (list (etaf-node 'text nil (list (grid-workbook-sheet-name sheet)))))))

(defun grid-table-etaf--status-node (state source range &optional line)
  "Return the status bar View node of STATE for SOURCE and the selected RANGE.
When SOURCE is one of several sheets their tabs come first, separated by
LINE, the grid line border."
  (let* ((sheets (grid-table-sheets))
         (status
          (etaf-node
           'box
           (list :key 'status
                 :flex-grow 1
                 :height '(lh 1)
                 :padding '((lh 0) (ch 1))
                 :wrap-mode 'none
                 :overflow 'hidden
                 :color (grid-table-etaf--face-color 'shadow :foreground))
           (list (etaf-node
                  'text nil
                  (list (grid-table-etaf--status-text state source range)))))))
    (if (null (cdr sheets))
        status
      (etaf-node
       'flex
       (list :key 'sheet-bar :height '(lh 1) :overflow 'hidden)
       (append
        (mapcar (lambda (sheet)
                  (grid-table-etaf--tab-node
                   (grid-table-etaf--state-buffer state) sheet
                   (eq sheet source) line))
                sheets)
        (list status))))))

(defun grid-table-etaf--render (state)
  "Return the View of STATE's visible window."
  (etaf-value (grid-table-etaf--state-tick state))
  (let ((runtime (grid-table-etaf--runtime
                  (grid-table-etaf--state-buffer state))))
    (when runtime (etaf-runtime-viewport runtime)))
  (let* ((source (grid-table-etaf--source state))
         (buffer (grid-table-etaf--state-buffer state))
         (widths (grid-table-etaf--state-widths state))
         (top (etaf-value (grid-table-etaf--state-top state)))
         (cursor (etaf-value (grid-table-etaf--state-cursor state)))
         (anchor (etaf-value (grid-table-etaf--state-anchor state)))
         (editing (etaf-value (grid-table-etaf--state-editing state)))
         (cols (grid-table-etaf--visible-cols
                state (etaf-value (grid-table-etaf--state-left state))))
         (rows (grid-table-etaf--visible-rows state top cols))
         (first-col (car cols))
         (last-col (car (last cols)))
         (merges (grid-get-merges source))
         (rules (grid-get-conditional-formats source))
         (cursor-merge (grid-table--merge-at merges (car cursor) (cdr cursor)))
         (cursor-row (if cursor-merge (nth 0 cursor-merge) (car cursor)))
         (cursor-col (if cursor-merge (nth 1 cursor-merge) (cdr cursor)))
         (sel-top (min (car cursor) (car anchor)))
         (sel-bottom (max (car cursor) (car anchor)))
         (sel-left (min (cdr cursor) (cdr anchor)))
         (sel-right (max (cdr cursor) (cdr anchor)))
         (ranged (not (equal cursor anchor)))
         (border (grid-table-etaf--face-color 'grid-table-etaf-border-face
                                              :foreground))
         (line (if border (list '(px 1) 'solid border) '(px 1)))
         children)
    (push (grid-table-etaf--header-node
           'corner 1 1 "" 'left nil line
           (grid-table-etaf--drag-header buffer #'grid-table-etaf-select-all))
          children)
    (cl-loop
     for col in cols for slot from 0 do
     (push (grid-table-etaf--header-node
            (format "c%d" slot) 1 (+ 2 slot)
            (concat (grid-table--col-number-to-letter col)
                    (and (assq col (grid-table-etaf--state-filters state))
                         " ▼"))
            'center
            (and (<= sel-left col) (<= col sel-right)) line
            (grid-table-etaf--drag-header
             buffer (lambda () (grid-table-etaf-select-column col)))
            (grid-table-etaf--drag-resize buffer col))
           children))
    (cl-loop
     for row in rows for row-slot from 0 do
     (push (grid-table-etaf--header-node
            (format "r%d" row-slot) (+ 2 row-slot) 1
            (number-to-string (1+ row)) 'right
            (and (<= sel-top row) (<= row sel-bottom)) line
            (grid-table-etaf--drag-header
             buffer (lambda () (grid-table-etaf-select-row row))))
           children)
     (cl-loop
      for col in cols for col-slot from 0 do
      (let* ((merge (and merges (grid-table--merge-at merges row col)))
             ;; Filters can hide rows inside a merge: it spans the rows of
             ;; it that are shown, starting at the first of them.
             (merge-rows (and merge
                              (cl-remove-if-not
                               (lambda (shown)
                                 (<= (nth 0 merge) shown (nth 2 merge)))
                               rows)))
             (vis-top (car merge-rows))
             (vis-left (and merge (max (nth 1 merge) first-col)))
             (cell-row (if merge (nth 0 merge) row))
             (cell-col (if merge (nth 1 merge) col)))
        (unless (and merge (not (and (= row vis-top) (= col vis-left))))
          (let ((cursor-p (and (= cell-row cursor-row)
                               (= cell-col cursor-col))))
            (push
             (grid-table-etaf--cell-node
              state source cell-row cell-col
              (and merge
                   (cons (length merge-rows)
                         (1+ (- (min (nth 3 merge) last-col) vis-left))))
              (list :key (+ (* 10000 row-slot) col-slot)
                    :rules rules
                    :ref (grid-table-etaf--slot-ref row-slot col-slot)
                    :grid-row (+ 2 row-slot)
                    :grid-col (+ 2 col-slot)
                    :line line
                    :cursor cursor-p
                    :editing (and cursor-p editing)
                    :selected
                    (and ranged
                         (<= sel-top (if merge (nth 2 merge) row))
                         (<= (if merge (nth 0 merge) row) sel-bottom)
                         (<= sel-left (if merge (nth 3 merge) col))
                         (<= (if merge (nth 1 merge) col) sel-right))))
             children))))))
    (etaf-node
     'column
     (list :ref 'grid-table-etaf-root
           :tab-index 0
           :on-direction (grid-table-etaf--direction buffer)
           :keys grid-table-etaf--keys)
     (delq nil
           (list
            (grid-table-etaf--title-node state)
            (grid-table-etaf--formula-bar-node
             source cursor-row cursor-col
             (and ranged (list sel-top sel-left sel-bottom sel-right))
             line)
            (etaf-node
             'grid
             (list :key 'sheet
                   :grid-template-columns
                   (cons (list 'ch (+ 2 (grid-table-etaf--row-header-width
                                         source)))
                         (mapcar (lambda (col)
                                   `(calc (+ (ch ,(+ 2 (aref widths col)))
                                             (px 1))))
                                 cols)))
             (nreverse children))
            (grid-table-etaf--status-node
             state source
             (and ranged (list sel-top sel-left sel-bottom sel-right))
             line))))))

(etaf-define-component grid-table-etaf-sheet (&key state)
  :render (grid-table-etaf--render (etaf-current-prop 'state)))

;;;----------------------------------------------------------------------
;;; Cursor and selection
;;;----------------------------------------------------------------------

(defun grid-table-etaf--state-or-error ()
  "Return the current buffer's renderer state."
  (or grid-table-etaf--state
      (user-error "Not in an ETAF grid buffer")))

(defun grid-table-etaf--cursor ()
  "Return the cursor of the current buffer as (ROW . COL)."
  (etaf-value (grid-table-etaf--state-cursor (grid-table-etaf--state-or-error))))

(defun grid-table-etaf--sync-point (state)
  "Put point on the cursor cell of STATE when it is painted."
  (let* ((buffer (grid-table-etaf--state-buffer state))
         (runtime (grid-table-etaf--runtime buffer))
         (position (and runtime
                        (etaf-host-ref-position runtime
                                                'grid-table-etaf-cursor))))
    (when position
      (with-current-buffer buffer
        (goto-char position)
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window position)
          (set-window-start window (point-min) t))))
    position))

(defun grid-table-etaf--scroll-to-cursor (state)
  "Adjust STATE's first row and column so that its cursor is shown."
  (let* ((cursor (etaf-value (grid-table-etaf--state-cursor state)))
         (row (car cursor))
         (col (cdr cursor))
         (left-ref (grid-table-etaf--state-left state))
         (top-ref (grid-table-etaf--state-top state)))
    (when (< col (etaf-value left-ref))
      (etaf-set-value left-ref col))
    (while (not (memq col (grid-table-etaf--visible-cols
                           state (etaf-value left-ref))))
      (etaf-set-value left-ref (1+ (etaf-value left-ref))))
    (when (< row (etaf-value top-ref))
      (etaf-set-value top-ref row))
    (let ((cols (grid-table-etaf--visible-cols state (etaf-value left-ref))))
      (while (not (memq row (grid-table-etaf--visible-rows
                             state (etaf-value top-ref) cols)))
        (etaf-set-value top-ref (grid-table-etaf--step-row
                                 state (etaf-value top-ref) 1))))))

(defun grid-table-etaf--settle (state)
  "Show STATE's cursor and put point on it.
Row heights are estimated before layout, so a row taller than estimated
can still push the cursor below the window; scroll further until the
painted cursor fits."
  (etaf-batch (grid-table-etaf--scroll-to-cursor state))
  (let ((lines (cdr (grid-table-etaf--viewport state)))
        (top-ref (grid-table-etaf--state-top state))
        (row (car (etaf-value (grid-table-etaf--state-cursor state))))
        (guard 0)
        position)
    (while (and (setq position (grid-table-etaf--sync-point state))
                (< guard 50)
                (< (etaf-value top-ref) row)
                (with-current-buffer (grid-table-etaf--state-buffer state)
                  (> (line-number-at-pos position) lines)))
      (setq guard (1+ guard))
      (etaf-set-value top-ref (grid-table-etaf--step-row
                               state (etaf-value top-ref) 1)))))

(defun grid-table-etaf-goto (row col &optional extend)
  "Move the cursor to model ROW and COL.
With EXTEND non-nil keep the selection anchor, extending the range."
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         ;; A filtered-out row is not a place to be: take the next shown.
         (row (grid-table-etaf--row-at
               state (grid-table-etaf--row-position state row)))
         (col (max 0 (min (1- (grid-get-column-count source)) col))))
    (grid-table-etaf--finish-edit state)
    (etaf-batch
      (etaf-set-value (grid-table-etaf--state-cursor state) (cons row col))
      (unless extend
        (etaf-set-value (grid-table-etaf--state-anchor state)
                        (cons row col))))
    (setq grid-table--nav-row row
          grid-table--nav-col (1+ col))
    (grid-table-etaf--settle state)
    t))

(defun grid-table-etaf--cell-extent (row col)
  "Return (TOP LEFT BOTTOM RIGHT) of the cell covering ROW and COL."
  (or (grid-table--merge-at (grid-get-merges grid-table--data-source) row col)
      (list row col row col)))

(defun grid-table-etaf-move (rows cols &optional extend)
  "Move the cursor ROWS down and COLS right, stepping over merged cells.
With EXTEND non-nil extend the selection instead."
  (let* ((state (grid-table-etaf--state-or-error))
         (cursor (grid-table-etaf--cursor))
         (extent (grid-table-etaf--cell-extent (car cursor) (cdr cursor))))
    (grid-table-etaf-goto
     ;; Rows are counted among the shown ones, so filtered rows are
     ;; stepped over.
     (cond ((> rows 0) (grid-table-etaf--step-row state (nth 2 extent) rows))
           ((< rows 0)
            (let ((target (+ (grid-table-etaf--row-position
                              state (nth 0 extent))
                             rows)))
              (grid-table-etaf--row-at state target)))
           (t (car cursor)))
     (cond ((> cols 0) (+ (nth 3 extent) cols))
           ((< cols 0) (+ (nth 1 extent) cols))
           (t (cdr cursor)))
     extend)))

(defun grid-table-etaf-move-tab (direction)
  "Move to the next cell in reading order, or the previous one.
DIRECTION is 1 or -1.  Movement wraps at both ends of the sheet."
  (let* ((state (grid-table-etaf--state-or-error))
         (source grid-table--data-source)
         (rows (grid-table-etaf--row-count state))
         (cols (grid-get-column-count source))
         (cursor (grid-table-etaf--cursor))
         (extent (grid-table-etaf--cell-extent (car cursor) (cdr cursor)))
         (position (grid-table-etaf--row-position state (car cursor)))
         (col (if (> direction 0) (1+ (nth 3 extent)) (1- (nth 1 extent)))))
    (cond ((>= col cols) (setq col 0 position (mod (1+ position) rows)))
          ((< col 0) (setq col (1- cols) position (mod (1- position) rows))))
    (grid-table-etaf-goto (grid-table-etaf--row-at state position) col)))

(defun grid-table-etaf--page-size ()
  "Return how many rows the window currently shows."
  (let* ((state (grid-table-etaf--state-or-error))
         (cols (grid-table-etaf--visible-cols
                state (etaf-value (grid-table-etaf--state-left state)))))
    (max 1 (length (grid-table-etaf--visible-rows
                    state (etaf-value (grid-table-etaf--state-top state))
                    cols)))))

(defun grid-table-etaf-page-down ()
  "Move the cursor one window of rows down."
  (interactive)
  (grid-table-etaf-move (grid-table-etaf--page-size) 0))

(defun grid-table-etaf-page-up ()
  "Move the cursor one window of rows up."
  (interactive)
  (grid-table-etaf-move (- (grid-table-etaf--page-size)) 0))

(defun grid-table-etaf--scroll (rows)
  "Scroll the window ROWS down without moving the cursor."
  (let* ((state (grid-table-etaf--state-or-error))
         (top (grid-table-etaf--state-top state)))
    (etaf-set-value top (grid-table-etaf--step-row
                         state (etaf-value top) rows))))

(defun grid-table-etaf-scroll-down ()
  "Scroll the sheet three rows down."
  (interactive)
  (grid-table-etaf--scroll 3))

(defun grid-table-etaf-scroll-up ()
  "Scroll the sheet three rows up."
  (interactive)
  (grid-table-etaf--scroll -3))

(defun grid-table-etaf-first-cell ()
  "Move the cursor to the first cell of the sheet."
  (interactive)
  (grid-table-etaf-goto 0 0))

(defun grid-table-etaf-last-cell ()
  "Move the cursor to the last cell of the sheet."
  (interactive)
  (grid-table-etaf-goto (1- (grid-get-row-count grid-table--data-source))
                        (1- (grid-get-column-count grid-table--data-source))))

;;;----------------------------------------------------------------------
;;; Filters
;;;----------------------------------------------------------------------

(defun grid-table-etaf--refilter (state)
  "Apply STATE's filters again and show the result."
  (grid-table-etaf--finish-edit state)
  (grid-table-etaf--apply-filters state)
  (etaf-set-value (grid-table-etaf--state-top state) 0)
  (grid-table-etaf--bump state)
  (let ((cursor (etaf-value (grid-table-etaf--state-cursor state))))
    (grid-table-etaf-goto (car cursor) (cdr cursor))))

(defun grid-table-etaf-filter-column (col op value)
  "Show only the rows whose cell in model column COL satisfies OP and VALUE.
OP is one of `grid-table-conditional-operators'.  The first row holds
the column titles and stays.  Filters on several columns combine.  Rows
are filtered when the filter is set and on `grid-table-refresh', not
after each edit."
  (interactive
   (let* ((col (cdr (grid-table-etaf--cursor)))
          (choices (mapcar (lambda (entry) (cons (cdr entry) (car entry)))
                           grid-table-conditional-operators))
          (op (cdr (assoc (completing-read
                           (format "Show rows whose column %s: "
                                   (grid-table--col-number-to-letter col))
                           choices nil t)
                          choices))))
     (list col op (if (member op '("empty" "error")) "" (read-string "Value: ")))))
  (unless (assoc op grid-table-conditional-operators)
    (user-error "Unknown condition: %s" op))
  (let ((state (grid-table-etaf--state-or-error)))
    (setf (alist-get col (grid-table-etaf--state-filters state))
          (list op value))
    (grid-table-etaf--refilter state)
    (message "Showing %d of %d rows"
             (1- (grid-table-etaf--row-count state))
             (1- (grid-get-row-count (grid-table-etaf--source state))))))

(defun grid-table-etaf-filter-by-cell ()
  "Show only the rows that hold the cursor cell's value in its column."
  (interactive)
  (let* ((cursor (grid-table-etaf--cursor))
         (value (grid-table-etaf--display-text
                 (grid-get-computed-value-at grid-table--data-source
                                             (car cursor) (cdr cursor))
                 (grid-table-cell-format grid-table--data-source
                                         (car cursor) (cdr cursor)))))
    (if (string-empty-p value)
        (grid-table-etaf-filter-column (cdr cursor) "empty" "")
      (grid-table-etaf-filter-column (cdr cursor) "=" value))))

(defun grid-table-etaf-clear-column-filter ()
  "Remove the filter on the cursor's column."
  (interactive)
  (let ((state (grid-table-etaf--state-or-error)))
    (setf (grid-table-etaf--state-filters state)
          (assq-delete-all (cdr (grid-table-etaf--cursor))
                           (grid-table-etaf--state-filters state)))
    (grid-table-etaf--refilter state)))

(defun grid-table-etaf-clear-filters ()
  "Remove every filter and show all rows."
  (interactive)
  (let ((state (grid-table-etaf--state-or-error)))
    (setf (grid-table-etaf--state-filters state) nil)
    (grid-table-etaf--refilter state)))

(defun grid-table-etaf-row-start ()
  "Move the cursor to the first column of its row."
  (interactive)
  (grid-table-etaf-goto (car (grid-table-etaf--cursor)) 0))

(defun grid-table-etaf-row-end ()
  "Move the cursor to the last column of its row."
  (interactive)
  (grid-table-etaf-goto (car (grid-table-etaf--cursor))
                        (1- (grid-get-column-count grid-table--data-source))))

(defun grid-table-etaf-extend-down ()
  "Extend the selection one row down."
  (interactive)
  (grid-table-etaf-move 1 0 t))

(defun grid-table-etaf-extend-up ()
  "Extend the selection one row up."
  (interactive)
  (grid-table-etaf-move -1 0 t))

(defun grid-table-etaf-extend-right ()
  "Extend the selection one column right."
  (interactive)
  (grid-table-etaf-move 0 1 t))

(defun grid-table-etaf-extend-left ()
  "Extend the selection one column left."
  (interactive)
  (grid-table-etaf-move 0 -1 t))

(defun grid-table-etaf-selection ()
  "Return the selected range as (TOP LEFT BOTTOM RIGHT) in model coordinates."
  (let* ((state (grid-table-etaf--state-or-error))
         (cursor (etaf-value (grid-table-etaf--state-cursor state)))
         (anchor (etaf-value (grid-table-etaf--state-anchor state))))
    (list (min (car cursor) (car anchor)) (min (cdr cursor) (cdr anchor))
          (max (car cursor) (car anchor)) (max (cdr cursor) (cdr anchor)))))

(defun grid-table-etaf-cell-at-point ()
  "Return the descriptor of the cursor cell.
The plist has the shape `grid-table-get-cell-at-point' returns, in
display coordinates: display column is model column plus one."
  (when grid-table-etaf--state
    (let* ((cursor (grid-table-etaf--cursor))
           (extent (grid-table-etaf--cell-extent (car cursor) (cdr cursor))))
      (list :row-start (nth 0 extent) :row-end (nth 2 extent)
            :col-start (1+ (nth 1 extent)) :col-end (1+ (nth 3 extent))
            :anchor-row (nth 0 extent) :anchor-col (1+ (nth 1 extent))
            :slice-row (car cursor) :slice-col (1+ (cdr cursor))
            :display-row-idx (nth 0 extent)
            :display-col-idx (1+ (nth 1 extent))
            :display-col-end-idx (1+ (nth 3 extent))))))

;;;----------------------------------------------------------------------
;;; Editing
;;;----------------------------------------------------------------------

(defun grid-table-etaf--bump (state)
  "Re-render STATE's window from the data source."
  (let ((tick (grid-table-etaf--state-tick state)))
    (etaf-set-value tick (1+ (etaf-value tick)))))

(defun grid-table-etaf--widen-visible (state)
  "Widen STATE's columns to fit the values now in the window.
An edit can change any formula cell, so every shown cell is measured.
Columns only shrink on `grid-table-refresh'."
  (let* ((source (grid-table-etaf--source state))
         (widths (grid-table-etaf--state-widths state))
         (manual (grid-table-etaf--manual state))
         (merges (grid-get-merges source))
         (cols (grid-table-etaf--visible-cols
                state (etaf-value (grid-table-etaf--state-left state))))
         (rows (grid-table-etaf--visible-rows
                state (etaf-value (grid-table-etaf--state-top state)) cols)))
    (dolist (row rows)
      (dolist (col cols)
        (let ((merge (and merges (grid-table--merge-at merges row col))))
          (unless (or (and manual (gethash col manual))
                      (and merge
                           (or (/= (nth 1 merge) (nth 3 merge))
                               (/= row (nth 0 merge)))))
            (aset widths col
                  (max (aref widths col)
                       (grid-table-etaf--value-width
                        (grid-get-computed-value-at source row col)
                        (grid-table-cell-format source row col))))))))))

(defun grid-table-etaf--end-session (state)
  "End STATE's input session and return its text, or nil without one."
  (let ((session (grid-table-etaf--state-session state)))
    (when session
      (setf (grid-table-etaf--state-session state) nil)
      (with-current-buffer (grid-table-etaf--state-buffer state)
        (setq grid-table-etaf--typing-keys t))
      (prog1 (etaf-input-end session)
        (etaf-set-value (grid-table-etaf--state-editing state) nil)))))

(defun grid-table-etaf--finish-edit (state)
  "Commit STATE's in-place edit, if one is open."
  (when (grid-table-etaf--state-session state)
    (let* ((cursor (etaf-value (grid-table-etaf--state-cursor state)))
           (extent (with-current-buffer (grid-table-etaf--state-buffer state)
                     (grid-table-etaf--cell-extent (car cursor) (cdr cursor))))
           (value (grid-table-etaf--end-session state)))
      (if value
          (grid-table-etaf--enter-cells
           state (list (list (nth 0 extent) (nth 1 extent) value)))
        (grid-table-etaf--bump state)
        (grid-table-etaf--settle state)))))

(defun grid-table-etaf-commit-edit ()
  "Store the text of the in-place edit in its cell."
  (interactive)
  (grid-table-etaf--finish-edit (grid-table-etaf--state-or-error)))

(defun grid-table-etaf-cancel-edit ()
  "Leave the in-place edit without changing its cell."
  (interactive)
  (let ((state (grid-table-etaf--state-or-error)))
    (when (grid-table-etaf--end-session state)
      (grid-table-etaf--bump state)
      (grid-table-etaf--settle state))))

(defun grid-table-etaf-commit-edit-and-move-down ()
  "Store the text of the in-place edit and move to the row below."
  (interactive)
  (grid-table-etaf-commit-edit)
  (grid-table-etaf-move 1 0))

(defvar grid-table-etaf-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'grid-table-etaf-commit-edit-and-move-down)
    (define-key map (kbd "C-c C-c") #'grid-table-etaf-commit-edit)
    (define-key map (kbd "M-RET") #'newline)
    (define-key map (kbd "C-c C-k") #'grid-table-etaf-cancel-edit)
    (define-key map (kbd "TAB") #'grid-table-next-cell)
    (define-key map [tab] #'grid-table-next-cell)
    (define-key map [backtab] #'grid-table-previous-cell)
    map)
  "Keymap consulted before the editing commands of an in-place cell edit.")

(defun grid-table-etaf-edit-cell (&optional initial)
  "Edit the raw value of the cursor cell in place.
INITIAL, a string, replaces the cell text the edit starts from.
RET stores the text and moves down, C-c C-c stores it and stays, M-RET
inserts a newline and C-c C-k cancels.  Moving to another cell, with
TAB or the mouse, stores the text too."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (buffer (grid-table-etaf--state-buffer state)))
    (unless (grid-table-etaf--state-session state)
      (grid-table-etaf--settle state)
      (etaf-set-value (grid-table-etaf--state-editing state) (or initial t))
      (condition-case err
          (progn
            (setf (grid-table-etaf--state-session state)
                  (etaf-input-begin
                   (grid-table-etaf--runtime buffer) 'grid-table-etaf-cursor
                   :keymap grid-table-etaf-edit-map
                   :on-leave (lambda ()
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (grid-table-etaf-commit-edit))))))
            ;; The session's own editing commands take the printing keys.
            (setq grid-table-etaf--typing-keys nil))
        (error
         (etaf-set-value (grid-table-etaf--state-editing state) nil)
         (signal (car err) (cdr err)))))))

(defun grid-table-etaf-self-insert ()
  "Start editing the cursor cell with the typed character as its text."
  (interactive)
  (if (characterp last-command-event)
      (grid-table-etaf-edit-cell (string last-command-event))
    (grid-table-etaf-edit-cell)))

(defun grid-table-etaf-clear-cells ()
  "Empty every cell of the selected range."
  (interactive)
  (let ((state (grid-table-etaf--state-or-error))
        (range (grid-table-etaf-selection))
        changes)
    (cl-loop for row from (nth 0 range) to (nth 2 range)
             when (grid-table-etaf--row-shown-p state row) do
             (cl-loop for col from (nth 1 range) to (nth 3 range) do
                      (push (list row col "") changes)))
    (grid-table-etaf--set-cells state (nreverse changes))))

(defun grid-table-etaf-set-cell (row col value)
  "Store raw VALUE in model ROW and COL as one undoable change."
  (grid-table-etaf--set-cells (grid-table-etaf--state-or-error)
                              (list (list row col value))))

;;;----------------------------------------------------------------------
;;; Undo
;;;----------------------------------------------------------------------

(defconst grid-table-etaf--undo-limit 200
  "How many changes the undo history of a grid keeps.")

(defun grid-table-etaf--record (state record)
  "Push RECORD on STATE's undo history and drop its redo history."
  (let ((undo (cons record (grid-table-etaf--state-undo state))))
    (when (> (length undo) grid-table-etaf--undo-limit)
      (setcdr (nthcdr (1- grid-table-etaf--undo-limit) undo) nil))
    (setf (grid-table-etaf--state-undo state) undo
          (grid-table-etaf--state-redo state) nil)))

(defun grid-table-etaf--as-one-step (state function)
  "Call FUNCTION and fold the undo records it leaves on STATE into one.
Return what FUNCTION returns."
  (let* ((before (grid-table-etaf--state-undo state))
         (value (funcall function))
         (after (grid-table-etaf--state-undo state))
         (added (cl-loop for tail on after until (eq tail before)
                         collect (car tail))))
    (when (cdr added)
      (setf (grid-table-etaf--state-undo state) before)
      ;; ADDED is newest first; a group replays oldest first.
      (grid-table-etaf--record state (cons 'group (nreverse added))))
    value))

(defun grid-table-etaf--write-cells (state changes)
  "Store CHANGES, a list of (ROW COL VALUE), in STATE's data source.
Return the list of (ROW COL OLD NEW) for the cells that changed.  Cells
that reject the value, such as the covered part of a merge, are skipped."
  (let ((source (grid-table-etaf--source state))
        applied)
    (pcase-dolist (`(,row ,col ,value) changes)
      (let ((old (or (grid-get-raw-value-at source row col) "")))
        (unless (equal old value)
          (condition-case nil
              (progn
                (grid-set-raw-value-at source row col value)
                (push (list row col old value) applied))
            (error nil)))))
    (nreverse applied)))

(defun grid-table-etaf--set-cells (state changes)
  "Store CHANGES, a list of (ROW COL VALUE), as one undoable step in STATE.
Return the number of cells that changed."
  (let ((applied (grid-table-etaf--write-cells state changes)))
    (when applied
      (grid-table-etaf--record state (cons 'cells applied)))
    (grid-table-etaf--widen-visible state)
    (grid-table-etaf--bump state)
    (grid-table-etaf--settle state)
    (length applied)))

(defun grid-table-etaf--enter-cells (state changes)
  "Store CHANGES, a list of (ROW COL TEXT), as a user types them.
A date or a percentage is stored as its number and gives a cell without
a format the one that shows it as typed.  The whole is one undoable step
in STATE.  Return the number of cells that changed."
  (let ((source (grid-table-etaf--source state))
        values styles)
    (pcase-dolist (`(,row ,col ,text) changes)
      (let ((entry (grid-table-read-entry source row col text)))
        (push (list row col (car entry)) values)
        (unless (eq (cdr entry) 'keep)
          (push (list row col (cdr entry)) styles))))
    (grid-table-etaf--as-one-step
     state
     (lambda ()
       ;; The format first, so the value is fitted as it will be shown.
       (when styles
         (grid-table-etaf--set-styles state (nreverse styles)))
       (grid-table-etaf--set-cells state (nreverse values))))))

(defun grid-table-etaf--text-format-changed-p (cells)
  "Return non-nil when CELLS, (ROW COL OLD NEW) styles, touch a text format.
Whether a cell holds text or a number then has to be worked out again."
  (cl-some (lambda (cell)
             (not (eq (eq (car-safe (plist-get (nth 2 cell) :format)) 'text)
                      (eq (car-safe (plist-get (nth 3 cell) :format)) 'text))))
           cells))

(defun grid-table-etaf--set-styles (state changes)
  "Give cells their styles from CHANGES, a list of (ROW COL STYLE).
The change is one undoable step in STATE.  Nothing happens when the data
source stores no styles.  Return the number of cells that changed."
  (let ((source (grid-table-etaf--source state))
        applied)
    (when (gethash :set-style-at source)
      (pcase-dolist (`(,row ,col ,style) changes)
        (let ((old (grid-get-style-at source row col)))
          (unless (equal old style)
            (grid-set-style-at source row col style)
            (push (list row col old style) applied)))))
    (when applied
      (when (grid-table-etaf--text-format-changed-p applied)
        (grid-table-recalculate source))
      (grid-table-etaf--record state (cons 'styles (nreverse applied)))
      (grid-table-etaf--widen-visible state)
      (grid-table-etaf--bump state)
      (grid-table-etaf--settle state))
    (length applied)))

(defun grid-table-etaf--track-model (state)
  "Record a structural change of STATE's data source since the last look.
Adding, deleting, sorting and merging replace the source's model, so the
previous model is all that undoing them needs."
  (let* ((source (grid-table-etaf--source state))
         (model (gethash :model source))
         (known (grid-table-etaf--state-model state)))
    (when (and model known (not (eq model known)))
      (setf (grid-table-etaf--state-manual state) nil)
      (grid-table-etaf--forget-moved-filters state known model)
      (grid-table-etaf--record state (list 'model known model)))
    (setf (grid-table-etaf--state-model state) model)))

(defun grid-table-etaf--forget-moved-filters (state old new)
  "Drop STATE's filters when model NEW has other columns than OLD.
A filter names its column by index, which an inserted or deleted column
shifts."
  (unless (= (grid-data-model-cols old) (grid-data-model-cols new))
    (setf (grid-table-etaf--state-filters state) nil)))

(defun grid-table-etaf--replay (state record forward)
  "Apply undo RECORD to STATE: its new side when FORWARD, else its old side."
  (let ((source (grid-table-etaf--source state)))
    (pcase record
      (`(group . ,records)
       (dolist (part (if forward records (reverse records)))
         (grid-table-etaf--replay state part forward)))
      (`(cells . ,cells)
       (grid-table-etaf--write-cells
        state
        (mapcar (lambda (cell)
                  (list (nth 0 cell) (nth 1 cell)
                        (nth (if forward 3 2) cell)))
                (if forward cells (reverse cells))))
       (grid-table-etaf--widen-visible state)
       (grid-table-etaf--bump state)
       (grid-table-etaf-goto (nth 0 (car cells)) (nth 1 (car cells))))
      (`(styles . ,cells)
       (dolist (cell cells)
         (grid-set-style-at source (nth 0 cell) (nth 1 cell)
                            (nth (if forward 3 2) cell)))
       (when (grid-table-etaf--text-format-changed-p cells)
         (grid-table-recalculate source))
       (grid-table-etaf--bump state)
       (grid-table-etaf--settle state))
      (`(conditionals ,old ,new)
       (grid-set-conditional-formats source (if forward new old))
       (grid-table-etaf--bump state)
       (grid-table-etaf--settle state))
      (`(model ,old ,new)
       (let ((model (if forward new old))
             (cursor (etaf-value (grid-table-etaf--state-cursor state))))
         (puthash :model model source)
         (grid-workbook-model-restored source (if forward old new) model)
         ;; Dragged widths belong to the columns of the model they were set on.
         (grid-table-etaf--forget-moved-filters
          state (if forward old new) model)
         (setf (grid-table-etaf--state-model state) model
               (grid-table-etaf--state-manual state) nil)
         (grid-table-etaf--apply-filters state)
         (grid-table-etaf--fit-widths state)
         (grid-table-etaf--bump state)
         (grid-table-etaf-goto (car cursor) (cdr cursor)))))))

(defun grid-table-etaf-undo ()
  "Undo the last change to the grid."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (record (car (grid-table-etaf--state-undo state))))
    (unless record (user-error "Nothing to undo"))
    (grid-table-etaf--end-session state)
    (pop (grid-table-etaf--state-undo state))
    (push record (grid-table-etaf--state-redo state))
    (grid-table-etaf--replay state record nil)))

(defun grid-table-etaf-redo ()
  "Redo the change undone last."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (record (car (grid-table-etaf--state-redo state))))
    (unless record (user-error "Nothing to redo"))
    (grid-table-etaf--end-session state)
    (pop (grid-table-etaf--state-redo state))
    (push record (grid-table-etaf--state-undo state))
    (grid-table-etaf--replay state record t)))

(defun grid-table-etaf-merge ()
  "Merge the selected range into one cell, or ask for a range to merge.
Only the top left cell of the range may hold a value."
  (interactive)
  (let ((range (grid-table-etaf-selection)))
    (if (and (= (nth 0 range) (nth 2 range)) (= (nth 1 range) (nth 3 range)))
        (call-interactively #'grid-table-merge-cells)
      (grid-table-merge-cells
       (format "%s:%s"
               (grid-table-etaf--address (nth 0 range) (nth 1 range))
               (grid-table-etaf--address (nth 2 range) (nth 3 range)))))))

;;;----------------------------------------------------------------------
;;; Formatting
;;;----------------------------------------------------------------------

(defun grid-table-etaf--plist-without (plist key)
  "Return PLIST without KEY."
  (cl-loop for (name value) on plist by #'cddr
           unless (eq name key) append (list name value)))

(defun grid-table-etaf--restyle (function)
  "Give every cell of the selection the style FUNCTION makes of its own.
FUNCTION receives a cell's style plist and returns the new one.  The
change is one undoable step.  Covered cells of a merge are skipped."
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (merges (grid-get-merges source))
         changed)
    (unless (gethash :set-style-at source)
      (user-error "This grid's data source does not store cell styles"))
    (cl-loop
     for row from (nth 0 range) to (nth 2 range)
     when (grid-table-etaf--row-shown-p state row) do
     (cl-loop
      for col from (nth 1 range) to (nth 3 range) do
      (let ((merge (and merges (grid-table--merge-at merges row col))))
        (when (or (null merge)
                  (and (= row (nth 0 merge)) (= col (nth 1 merge))))
          (let* ((old (grid-get-style-at source row col))
                 (new (funcall function (copy-sequence old))))
            (unless (equal old new)
              (grid-set-style-at source row col new)
              (push (list row col old new) changed)))))))
    (when changed
      (when (grid-table-etaf--text-format-changed-p changed)
        (grid-table-recalculate source))
      (grid-table-etaf--record state (cons 'styles (nreverse changed)))
      (grid-table-etaf--widen-visible state))
    (grid-table-etaf--bump state)
    (grid-table-etaf--settle state)
    (length changed)))

(defun grid-table-etaf--set-format (key value)
  "Set style property KEY to VALUE on the selection; nil removes it."
  (grid-table-etaf--restyle
   (lambda (style)
     (if value
         (plist-put style key value)
       (grid-table-etaf--plist-without style key)))))

(defun grid-table-etaf--toggle-format (key)
  "Toggle style property KEY on the selection, following the cursor cell."
  (let* ((state (grid-table-etaf--state-or-error))
         (cursor (grid-table-etaf--cursor))
         (extent (grid-table-etaf--cell-extent (car cursor) (cdr cursor)))
         (on (plist-get (grid-get-style-at (grid-table-etaf--source state)
                                           (nth 0 extent) (nth 1 extent))
                        key)))
    (grid-table-etaf--set-format key (not on))))

(defun grid-table-etaf-toggle-bold ()
  "Make the selection bold, or plain when the cursor cell is bold."
  (interactive)
  (grid-table-etaf--toggle-format :bold))

(defun grid-table-etaf-toggle-italic ()
  "Make the selection italic, or upright when the cursor cell is italic."
  (interactive)
  (grid-table-etaf--toggle-format :italic))

(defun grid-table-etaf--color (name)
  "Return color NAME as #RRGGBB when the display can resolve it."
  (cond ((or (null name) (string-empty-p name)) nil)
        ((string-prefix-p "#" name) name)
        (t (let ((rgb (ignore-errors (color-name-to-rgb name))))
             (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) name)))))

(defun grid-table-etaf-set-text-color (color)
  "Paint the text of the selection in COLOR; empty input removes the color."
  (interactive (list (read-color "Text color (empty to clear): ")))
  (grid-table-etaf--set-format :color (grid-table-etaf--color color)))

(defun grid-table-etaf-set-fill-color (color)
  "Fill the cells of the selection with COLOR; empty input removes the fill."
  (interactive (list (read-color "Fill color (empty to clear): ")))
  (grid-table-etaf--set-format :background (grid-table-etaf--color color)))

(defun grid-table-etaf-align-left ()
  "Align the selection to the left."
  (interactive)
  (grid-table-etaf--set-format :align 'left))

(defun grid-table-etaf-align-center ()
  "Center the selection."
  (interactive)
  (grid-table-etaf--set-format :align 'center))

(defun grid-table-etaf-align-right ()
  "Align the selection to the right."
  (interactive)
  (grid-table-etaf--set-format :align 'right))

(defun grid-table-etaf--cursor-format ()
  "Return the format the cursor cell is shown with."
  (let* ((cursor (grid-table-etaf--cursor))
         (extent (grid-table-etaf--cell-extent (car cursor) (cdr cursor))))
    (grid-table-cell-format grid-table--data-source
                            (nth 0 extent) (nth 1 extent))))

(defun grid-table-etaf-format-general ()
  "Show the selection in the general format."
  (interactive)
  (grid-table-etaf--set-format :format nil))

(defun grid-table-etaf-format-number (decimals)
  "Show the selection as numbers with DECIMALS decimal places.
The whole part is grouped in thousands."
  (interactive (list (read-number "Decimal places: " 2)))
  (grid-table-etaf--set-format
   :format (list 'number (max 0 (min 12 (truncate decimals))) t)))

(defun grid-table-etaf-format-percent ()
  "Show the selection as percentages, so 0.125 reads 12.5%."
  (interactive)
  (grid-table-etaf--set-format
   :format (list 'percent
                 (or (grid-format-decimals (grid-table-etaf--cursor-format))
                     0))))

(defun grid-table-etaf-format-currency (symbol)
  "Show the selection as amounts of money led by SYMBOL."
  (interactive
   (list (read-string (format "Currency symbol (default %s): "
                              grid-table-currency-symbol)
                      nil nil grid-table-currency-symbol)))
  (grid-table-etaf--set-format :format (list 'currency 2 symbol)))

(defun grid-table-etaf-format-date (pattern)
  "Show the numbers of the selection as dates written after PATTERN.
PATTERN is made of yyyy, yy, mm, m, dd, d, hh, h and ss."
  (interactive
   (list (completing-read "Date format (default yyyy-mm-dd): "
                          grid-format-date-patterns nil nil nil nil
                          "yyyy-mm-dd")))
  (grid-table-etaf--set-format :format (list 'date pattern)))

(defun grid-table-etaf-format-text ()
  "Keep the selection as text: what is typed there is never a number."
  (interactive)
  (grid-table-etaf--set-format :format (list 'text)))

(defun grid-table-etaf--change-decimals (delta)
  "Show the selection with DELTA more decimal places than the cursor cell."
  (let* ((format (grid-table-etaf--cursor-format))
         (decimals (or (grid-format-decimals format) 0)))
    (grid-table-etaf--restyle
     (lambda (style)
       (plist-put style :format
                  (grid-format-with-decimals
                   (if (grid-format-decimals (plist-get style :format))
                       (plist-get style :format)
                     format)
                   (+ decimals delta)))))))

(defun grid-table-etaf-more-decimals ()
  "Show one more decimal place in the selection."
  (interactive)
  (grid-table-etaf--change-decimals 1))

(defun grid-table-etaf-fewer-decimals ()
  "Show one decimal place less in the selection."
  (interactive)
  (grid-table-etaf--change-decimals -1))

(defun grid-table-etaf-clear-format ()
  "Remove every style from the selection."
  (interactive)
  (grid-table-etaf--restyle #'ignore))

(defun grid-table-etaf--set-conditionals (rules)
  "Replace the grid's conditional format rules with RULES, undoably."
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (old (grid-get-conditional-formats source)))
    (unless (gethash :set-conditional-formats source)
      (user-error "This grid's data source does not store conditional formats"))
    (unless (equal old rules)
      (grid-set-conditional-formats source rules)
      (grid-table-etaf--record state (list 'conditionals old rules)))
    (grid-table-etaf--bump state)
    (grid-table-etaf--settle state)))

(defun grid-table-etaf-add-conditional-format (op value style)
  "Show the cells of the selection that satisfy OP against VALUE in STYLE.
OP is one of `grid-table-conditional-operators' and STYLE a style
plist.  The rule keeps applying as values change."
  (interactive
   (let* ((choices (mapcar (lambda (entry) (cons (cdr entry) (car entry)))
                           grid-table-conditional-operators))
          (op (cdr (assoc (completing-read "Format cells whose value: "
                                           choices nil t)
                          choices)))
          (value (if (member op '("empty" "error"))
                     ""
                   (read-string "Value: ")))
          (fill (grid-table-etaf--color
                 (read-color "Fill color (empty for none): ")))
          (color (grid-table-etaf--color
                  (read-color "Text color (empty for none): ")))
          (bold (y-or-n-p "Bold? ")))
     (list op value
           (append (and fill (list :background fill))
                   (and color (list :color color))
                   (and bold (list :bold t))))))
  (unless (assoc op grid-table-conditional-operators)
    (user-error "Unknown condition: %s" op))
  (unless style
    (user-error "A conditional format needs a fill, a text color or bold"))
  (let ((range (grid-table-etaf-selection)))
    (grid-table-etaf--set-conditionals
     (append (grid-get-conditional-formats
              (grid-table-etaf--source (grid-table-etaf--state-or-error)))
             (list (append range (list op value style)))))))

(defun grid-table-etaf-clear-conditional-formats ()
  "Remove the conditional format rules that touch the selection."
  (interactive)
  (let ((range (grid-table-etaf-selection)))
    (grid-table-etaf--set-conditionals
     (cl-remove-if
      (lambda (rule)
        (and (<= (nth 0 rule) (nth 2 range)) (<= (nth 0 range) (nth 2 rule))
             (<= (nth 1 rule) (nth 3 range)) (<= (nth 1 range) (nth 3 rule))))
      (grid-get-conditional-formats
       (grid-table-etaf--source (grid-table-etaf--state-or-error)))))))

;;;----------------------------------------------------------------------
;;; Clipboard
;;;----------------------------------------------------------------------

(defvar grid-table-etaf--clipboard nil
  "The last copied range as (TEXT ROWS TOP LEFT CUT STYLES).
TEXT is what went to the kill ring and ROWS the raw values, formulas
included, copied from model row TOP and column LEFT.  Pasting uses ROWS
while TEXT is still the current kill.  CUT is non-nil when the range was
cut, so its formulas move without being adjusted.  STYLES holds the
cells' styles, shaped like ROWS.")

(defun grid-table-etaf--tsv-field (text)
  "Return TEXT as one tab-separated field, quoted when it needs to be."
  (if (string-match-p "[\t\n\"]" text)
      (concat "\"" (string-replace "\"" "\"\"" text) "\"")
    text))

(defun grid-table-etaf--tsv-encode (rows)
  "Return ROWS, a list of lists of strings, as tab-separated text."
  (mapconcat (lambda (row)
               (mapconcat #'grid-table-etaf--tsv-field row "\t"))
             rows "\n"))

(defun grid-table-etaf--tsv-decode (text)
  "Return tab-separated TEXT as a list of lists of strings.
Quoted fields may hold tabs, newlines and doubled quotes."
  (let ((pos 0) (end (length text)) rows row (field "") quoted)
    (while (< pos end)
      (let ((char (aref text pos)))
        (cond
         (quoted
          (cond ((and (eq char ?\") (< (1+ pos) end)
                      (eq (aref text (1+ pos)) ?\"))
                 (setq field (concat field "\"") pos (1+ pos)))
                ((eq char ?\") (setq quoted nil))
                (t (setq field (concat field (string char))))))
         ((and (eq char ?\") (string-empty-p field)) (setq quoted t))
         ((eq char ?\t) (push field row) (setq field ""))
         ((eq char ?\n)
          (push field row)
          (push (nreverse row) rows)
          (setq row nil field ""))
         ((eq char ?\r) nil)
         (t (setq field (concat field (string char))))))
      (setq pos (1+ pos)))
    ;; A trailing newline ends the last row; it does not start an empty one.
    (unless (and (null row) (string-empty-p field) rows)
      (push field row)
      (push (nreverse row) rows))
    (nreverse rows)))

(defun grid-table-etaf--range-rows (state source range reader)
  "Return the cells of RANGE in SOURCE as rows of what READER returns.
Rows hidden by STATE's filters are left out."
  (cl-loop for row from (nth 0 range) to (nth 2 range)
           when (grid-table-etaf--row-shown-p state row) collect
           (cl-loop for col from (nth 1 range) to (nth 3 range) collect
                    (funcall reader source row col))))

(defun grid-table-etaf-copy ()
  "Copy the selected range.
The kill ring receives the shown values as tab-separated text; pasting
back into a grid restores the raw values, formulas included, and the
cells' formats."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (text (grid-table-etaf--tsv-encode
                (grid-table-etaf--range-rows
                 state source range
                 (lambda (source row col)
                   (grid-table-etaf--display-text
                    (grid-get-computed-value-at source row col)
                    (grid-table-cell-format source row col)))))))
    (kill-new text)
    (setq grid-table-etaf--clipboard
          (list text
                (grid-table-etaf--range-rows
                 state source range
                 (lambda (source row col)
                   (or (grid-get-raw-value-at source row col) "")))
                (nth 0 range) (nth 1 range) nil
                (grid-table-etaf--range-rows state source range
                                             #'grid-get-style-at)))
    (message "Copied %d × %d cells"
             (1+ (- (nth 2 range) (nth 0 range)))
             (1+ (- (nth 3 range) (nth 1 range))))))

(defun grid-table-etaf-cut ()
  "Copy the selected range and empty it, formats included."
  (interactive)
  (grid-table-etaf-copy)
  (setf (nth 4 grid-table-etaf--clipboard) t)
  (let ((state (grid-table-etaf--state-or-error))
        (range (grid-table-etaf-selection)))
    (grid-table-etaf--as-one-step
     state
     (lambda ()
       (grid-table-etaf-clear-cells)
       (grid-table-etaf--set-styles
        state
        (cl-loop for row from (nth 0 range) to (nth 2 range)
                 when (grid-table-etaf--row-shown-p state row) nconc
                 (cl-loop for col from (nth 1 range) to (nth 3 range)
                          collect (list row col nil))))))))

(defun grid-table-etaf--shifted (value origin row col)
  "Return VALUE as it reads when its range moves from ORIGIN to ROW and COL.
ORIGIN is the (ROW . COL) the copied range started at, or nil when the
value should be pasted as it is."
  (if origin
      (grid-calc-shift-formula value (- row (car origin)) (- col (cdr origin)))
    value))

(defun grid-table-etaf-paste ()
  "Paste the current kill at the selected range.
The top left cell of the range receives the first value.  A single
copied cell fills the whole range.  The grid grows to take values that
reach past its last row or column; a data source that cannot grow drops
them.  Copied formulas have their relative references moved
by the distance they travel, as in a spreadsheet; cut formulas do not.
Cells copied in a grid bring their formats; text from elsewhere leaves
the formats of the cells it lands in."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (text (current-kill 0))
         (own (and grid-table-etaf--clipboard
                   (equal text (car grid-table-etaf--clipboard))
                   grid-table-etaf--clipboard))
         (rows (if own
                   (nth 1 own)
                 (grid-table-etaf--tsv-decode
                  (substring-no-properties text))))
         ;; Where a copied range came from, to move its references.
         (origin (and own (not (nth 4 own)) (cons (nth 2 own) (nth 3 own))))
         (range (grid-table-etaf-selection))
         (top (nth 0 range))
         (left (nth 1 range))
         (single (and (null (cdr rows)) (null (cdr (car rows)))))
         ;; How far the pasted block reaches past the grid
         (more-rows (if (or single (null (gethash :grow source)))
                        0
                      (max 0 (- (+ top (length rows))
                                (grid-get-row-count source)))))
         (more-cols (if (or single (null (gethash :grow source)))
                        0
                      (max 0 (- (+ left (apply #'max (mapcar #'length rows)))
                                (grid-get-column-count source)))))
         (max-row (+ more-rows (1- (grid-get-row-count source))))
         (max-col (+ more-cols (1- (grid-get-column-count source))))
         (bottom top)
         (right left)
         (styled (and own (nth 5 own)))
         changes styles)
    (unless rows (user-error "Nothing to paste"))
    (if single
        (cl-loop for row from top to (nth 2 range) do
                 (cl-loop for col from left to (nth 3 range) do
                          (push (list row col
                                      (grid-table-etaf--shifted
                                       (caar rows) origin row col))
                                changes)
                          (when styled
                            (push (list row col (caar styled)) styles)))
                 finally (setq bottom (nth 2 range) right (nth 3 range)))
      (cl-loop
       for values in rows for row from top to max-row do
       (setq bottom row)
       (cl-loop for value in values for col from left to max-col do
                (setq right (max right col))
                (push (list row col
                            ;; Every cell travels as far as the first one.
                            (grid-table-etaf--shifted value origin top left))
                      changes)
                (when styled
                  (push (list row col
                              (nth (- col left) (nth (- row top) styled)))
                        styles)))))
    (let ((count (grid-table-etaf--as-one-step
                  state
                  (lambda ()
                    (when (or (> more-rows 0) (> more-cols 0))
                      (funcall (gethash :grow source) source
                               more-rows more-cols)
                      (grid-table-etaf--track-model state)
                      (grid-table-etaf--apply-filters state)
                      (grid-table-etaf--fit-widths state))
                    (if own
                        (prog1 (grid-table-etaf--set-cells
                                state (nreverse changes))
                          (grid-table-etaf--set-styles state
                                                       (nreverse styles)))
                      ;; Text from elsewhere is read as if it were typed.
                      (grid-table-etaf--enter-cells
                       state (nreverse changes)))))))
      (grid-table-etaf-goto bottom right)
      (grid-table-etaf-goto top left t)
      (message "Pasted %d cell%s" count (if (= count 1) "" "s")))))

;;;----------------------------------------------------------------------
;;; Mode and entry points
;;;----------------------------------------------------------------------

(defun grid-table-etaf--viewport-changed (buffer)
  "Keep BUFFER's cursor shown and point on it after its window resized."
  (when (buffer-live-p buffer)
    (let ((state (buffer-local-value 'grid-table-etaf--state buffer)))
      (when (and state (grid-table-etaf--runtime buffer))
        (grid-table-etaf--settle state)))))

;; Depth 90 runs this after ETAF has published the new viewport.
(add-hook 'ebox-viewport-change-functions
          #'grid-table-etaf--viewport-changed 90)

(defun grid-table-etaf--write-contents ()
  "Save the grid of the current buffer instead of its rendered text."
  (when grid-table--data-source
    (grid-table-write-file)
    t))

(define-derived-mode grid-table-etaf-mode special-mode "Grid Table"
  "Major mode for a grid shown through the ETAF renderer.
Keys are owned by the mounted sheet; see `grid-table-etaf--keys'."
  :group 'grid-table
  (setq buffer-read-only nil)
  (setq truncate-lines t)
  (setq-local grid-table--renderer 'etaf)
  (add-hook 'write-contents-functions #'grid-table-etaf--write-contents nil t))

(defun grid-table-etaf-refresh (&optional restore-row restore-col)
  "Re-render the current grid from its data source.
RESTORE-ROW and RESTORE-COL, when given, are the display coordinates the
cursor moves to: display column is model column plus one."
  (let* ((state (grid-table-etaf--state-or-error))
         (cursor (etaf-value (grid-table-etaf--state-cursor state)))
         (row (if restore-row (max 0 restore-row) (car cursor)))
         (col (if restore-col (max 0 (1- restore-col)) (cdr cursor))))
    (grid-table-etaf--end-session state)
    (grid-table-etaf--track-model state)
    (grid-table-etaf--apply-filters state)
    (grid-table-etaf--fit-widths state)
    (grid-table-etaf--bump state)
    (grid-table-etaf-goto row col)))

(defun grid-table-etaf-show-in-current-buffer (data-source &optional title)
  "Mount DATA-SOURCE in the current buffer.  TITLE replaces the grid title."
  (let ((file-path grid-table--file-path)
        (old-title grid-table--title)
        (allow-elisp grid-table-allow-elisp)
        (buffer (current-buffer))
        (runtime (grid-table-etaf--runtime)))
    ;; An edit in progress belongs to the sheet being left.
    (when (and runtime grid-table-etaf--state)
      (grid-table-etaf--finish-edit grid-table-etaf--state))
    ;; Changing the major mode drops the Runtime's buffer-local hooks.
    (when runtime (etaf-unmount runtime))
    (grid-table-etaf-mode)
    (setq grid-table--data-source data-source
          grid-table--file-path file-path
          grid-table--title (or title old-title)
          grid-table-allow-elisp allow-elisp)
    (let* ((start (cons 0 0))
           ;; A sheet shown here before comes back as it was left: cursor,
           ;; scroll position, filters and undo history.
           (kept (gethash :etaf-state data-source))
           (state (if (and kept (eq (grid-table-etaf--state-buffer kept) buffer))
                      kept
                    (grid-table-etaf--state-create
                     :buffer buffer
                     :top (etaf-ref 0) :left (etaf-ref 0)
                     :cursor (etaf-ref start) :anchor (etaf-ref start)
                     :tick (etaf-ref 0) :editing (etaf-ref nil)
                     :widths (grid-table-etaf--compute-widths data-source)
                     :model (gethash :model data-source)))))
      (setq grid-table-etaf--state state)
      (puthash :etaf-state state data-source)
      (when (eq state kept)
        ;; Other sheets may have changed what this one shows.
        (grid-table-etaf--track-model state)
        (grid-table-etaf--apply-filters state)
        (grid-table-etaf--fit-widths state))
      (etaf-mount buffer
                  (etaf-node 'grid-table-etaf-sheet (list :state state) nil)
                  grid-table-etaf-mount-options)
      (etaf-focus (grid-table-etaf--runtime buffer) 'grid-table-etaf-root)
      ;; Mounting put ETAF's key map first; the traversal keys go before it.
      (setq grid-table-etaf--traverse-keys t
            grid-table-etaf--typing-keys t)
      (setq-local emulation-mode-map-alists
                  (cons 'grid-table-etaf--emulation-alist
                        (remq 'grid-table-etaf--emulation-alist
                              emulation-mode-map-alists)))
      (setq grid-table--nav-row 0
            grid-table--nav-col 1)
      (grid-table-etaf--settle state)
      (set-buffer-modified-p nil))))

(provide 'grid-table-etaf)

;;; grid-table-etaf.el ends here
