;;; grid-table-etaf.el --- ETAF renderer for grid-table -*- lexical-binding: t -*-

;;; Commentary:

;; Shows a grid-table data source through ETAF (https://github.com/emacs-taf).
;; Only the rows and columns that fit the window are mounted, so the cost
;; of moving, scrolling and editing does not grow with the size of the
;; sheet.
;;
;; This file is loaded by `grid-table--require-view' the first time a grid
;; is shown.  It needs the ecss, ebox and etaf repository roots on
;; `load-path'.  Developed against ecss 9a4270e, ebox 99cdf0d and etaf
;; 2e08bd9.
;;
;; The commands in grid-table.el work on the cell this view reports:
;; `grid-table-get-cell-at-point', `grid-table-refresh', `grid-table-edit-cell'
;; and the navigation commands end up here.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'etaf)
(require 'grid-table)
(require 'grid-table-chart-svg)

(defface grid-table-etaf-header-face
  '((t :inherit mode-line-inactive))
  "Face whose colors paint the column letter row and the row number column."
  :group 'grid-table)

(defface grid-table-etaf-border-face
  '((t :inherit shadow))
  "Face whose foreground paints the grid lines."
  :group 'grid-table)

(defface grid-table-etaf-freeze-face
  '((t :inherit link))
  "Face whose foreground paints the line along frozen rows and columns."
  :group 'grid-table)

(defface grid-table-etaf-cursor-face
  '((t :inherit highlight))
  "Face whose colors paint the current cell."
  :group 'grid-table)

(defface grid-table-etaf-selection-face
  '((t :inherit region))
  "Face whose colors paint the selected range."
  :group 'grid-table)

(defface grid-table-etaf-note-face
  '((t :foreground "#d0342c"))
  "Face whose foreground marks the right edge of a cell with a note."
  :group 'grid-table)

(defface grid-table-etaf-link-face
  '((((background dark)) :foreground "#7aa6ff")
    (t :foreground "#1a5fb4"))
  "Face whose foreground colors the text of a cell with a link."
  :group 'grid-table)

(defface grid-table-etaf-precedent-face
  '((((background dark)) :background "#1f3a5c")
    (t :background "#d6e6fb"))
  "Face whose background paints the cells a traced formula reads."
  :group 'grid-table)

(defface grid-table-etaf-dependent-face
  '((((background dark)) :background "#4d3812")
    (t :background "#fbe3c2"))
  "Face whose background paints the formulas that read a traced cell."
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

(defvar-local grid-table-etaf--traces nil
  "Traced cells painted in the grid, as (SHEET TOP LEFT BOTTOM RIGHT KIND).
SHEET is the data source of the sheet, KIND `precedent' or `dependent'.
Open ends of whole columns and rows are nil.  The list outlives a switch
to another sheet and is dropped when a cell or the structure changes.")
(put 'grid-table-etaf--traces 'permanent-local t)

(defvar-local grid-table-etaf--next-trace 0
  "Index of the trace `grid-table-etaf-next-trace' visits next.")
(put 'grid-table-etaf--next-trace 'permanent-local t)

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
         ;; Frozen columns come first whatever the scroll position, and
         ;; one column after them is always shown.
         (frozen (min (cdr (grid-table-frozen source)) (max 0 (1- cols))))
         (col (max left frozen))
         (scrolled nil)
         result)
    ;; A window too narrow for all of them shows the first ones, leaving
    ;; room for a column that scrolls.
    (let ((room (and (< frozen cols)
                     (1+ (* char (+ 2 (aref widths frozen))))))
          (kept 0))
      (while (and (< kept frozen)
                  (or (not (grid-table-etaf--col-shown-p state kept))
                      (let ((need (1+ (* char (+ 2 (aref widths kept))))))
                        (when (or (null room) (<= (+ need room) budget))
                          (setq budget (- budget need))
                          t))))
        (when (grid-table-etaf--col-shown-p state kept)
          (push kept result))
        (setq kept (1+ kept))))
    (while (and (< col cols)
                (or (not (grid-table-etaf--col-shown-p state col))
                    (let ((need (1+ (* char (+ 2 (aref widths col))))))
                      (when (or (not scrolled) (<= need budget))
                        (setq budget (- budget need))
                        t))))
      (when (grid-table-etaf--col-shown-p state col)
        (push col result)
        (setq scrolled t))
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
  "Recompute which rows of STATE its filters and hidden rows leave shown.
A header row holds the column titles and is not filtered; hiding it
hides it."
  (let* ((source (grid-table-etaf--source state))
         (filters (grid-table-etaf--state-filters state))
         (hidden (car (grid-table-hidden source)))
         (kept (if (grid-table-header-row-p source) 1 0)))
    (setf (grid-table-etaf--state-shown state)
          (and (or filters hidden)
               (vconcat
                (cl-remove-if
                 (lambda (row) (memq row hidden))
                 (append
                      (number-sequence 0 (1- kept))
                      (cl-loop
                       for row from kept below (grid-get-row-count source)
                       when (cl-every
                             (lambda (filter)
                               (grid-table-conditional-match-p
                                (nth 1 filter) (nth 2 filter)
                                (grid-get-computed-value-at
                                 source row (car filter))))
                             filters)
                       collect row))))))))

(defun grid-table-etaf--col-shown-p (state col)
  "Return non-nil when model column COL of STATE is not hidden."
  (not (memq col (cdr (grid-table-hidden (grid-table-etaf--source state))))))

(defun grid-table-etaf--shown-col (state col &optional backward)
  "Return the shown column nearest model column COL of STATE.
Look right of COL first, or left of it when BACKWARD, then the other way."
  (let* ((cols (grid-get-column-count (grid-table-etaf--source state)))
         (col (max 0 (min (1- cols) col)))
         (step (if backward -1 1)))
    (or (cl-loop for c = col then (+ c step)
                 while (< -1 c cols)
                 when (grid-table-etaf--col-shown-p state c) return c)
        (cl-loop for c = col then (- c step)
                 while (< -1 c cols)
                 when (grid-table-etaf--col-shown-p state c) return c)
        col)))

(defun grid-table-etaf--step-col (state col count)
  "Return the shown column COUNT shown columns right of COL in STATE.
A negative COUNT goes left; the walk stops at the edge of the sheet."
  (let ((cols (grid-get-column-count (grid-table-etaf--source state)))
        (step (if (< count 0) -1 1))
        (left (abs count))
        (at col))
    (cl-loop for c = (+ col step) then (+ c step)
             while (and (< -1 c cols) (> left 0))
             when (grid-table-etaf--col-shown-p state c)
             do (setq at c left (1- left)))
    at))

(defun grid-table-etaf--visible-rows (state top cols)
  "Return the model rows STATE shows over COLS when the first one is TOP."
  (let* ((source (grid-table-etaf--source state))
         (count (grid-table-etaf--row-count state))
         (merges (grid-get-merges source))
         ;; The letter row, the formula bar and the status bar take a line each.
         (budget (- (cdr (grid-table-etaf--viewport state))
                    3 (grid-table-etaf--title-lines state)))
         (frozen (car (grid-table-frozen source)))
         (position 0)
         (scrolled nil)
         result)
    ;; Frozen rows come first whatever the scroll position, and one row
    ;; after them is always shown.
    ;; A window too low for all of them shows the first ones, leaving a
    ;; line for a row that scrolls.
    (while (and (< position count)
                (< (grid-table-etaf--row-at state position) frozen)
                (let ((need (grid-table-etaf--row-lines
                             state (grid-table-etaf--row-at state position)
                             cols merges)))
                  (when (< need budget)
                    (setq budget (- budget need))
                    t)))
      (push (grid-table-etaf--row-at state position) result)
      (setq position (1+ position)))
    (while (and (< position count)
                (< (grid-table-etaf--row-at state position) frozen))
      (setq position (1+ position)))
    (setq position (max position (grid-table-etaf--row-position state top)))
    (while (and (< position count)
                (>= (grid-table-etaf--row-at state position) frozen)
                (let ((need (grid-table-etaf--row-lines
                             state (grid-table-etaf--row-at state position)
                             cols merges)))
                  (when (or (not scrolled) (<= need budget))
                    (setq budget (- budget need))
                    t)))
      (push (grid-table-etaf--row-at state position) result)
      (setq scrolled t
            position (1+ position)))
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
            ;; START is the width at the press and the width set by
            ;; hand then, for the one undo step the drag makes.
            ('start
             (setf (grid-table-etaf--state-resize-start state)
                   (cons (aref widths col)
                         (when-let* ((manual (grid-table-etaf--manual state)))
                           (gethash col manual)))))
            ((or 'move 'end)
             (let ((start (grid-table-etaf--state-resize-start state)))
               (when start
                 (grid-table-etaf-set-column-width
                  col (+ (car start)
                         (round (car (plist-get gesture :delta))
                                (grid-table-etaf--char-width))))
                 (when (eq (plist-get gesture :phase) 'end)
                   (setf (grid-table-etaf--state-resize-start state) nil)
                   (let ((new (aref (grid-table-etaf--state-widths state) col)))
                     (unless (eql new (car start))
                       (grid-table-etaf--record
                        state (list 'width col (cdr start) new))))))))
            ('cancel
             (let ((start (grid-table-etaf--state-resize-start state)))
               (when start
                 (setf (grid-table-etaf--state-resize-start state) nil)
                 (grid-table-etaf--restore-column-width
                  state col (cdr start)))))))))))

(defun grid-table-etaf--restore-column-width (state col width)
  "Give column COL of STATE the width WIDTH set by hand; nil fits it again."
  (let ((source (grid-table-etaf--source state)))
    (unless (grid-set-column-width source col width)
      (when-let* ((manual (grid-table-etaf--state-manual state)))
        (if width (puthash col width manual) (remhash col manual))))
    (grid-table-etaf--fit-widths state)
    (grid-table-etaf--bump state)))

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
    ("C-c C-r" grid-table-recalculate-all "Recalculate and refresh")
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
    ("C-c y v" grid-table-etaf-paste-values "Paste values")
    ("C-c y f" grid-table-etaf-paste-formats "Paste formats")
    ("C-c y t" grid-table-etaf-paste-transposed "Paste transposed")
    ("C-d" grid-table-etaf-fill-down "Fill down")
    ("C-r" grid-table-etaf-fill-right "Fill right")
    ("C-c C-s" grid-table-etaf-fill-series "Fill series")
    ("C-s" grid-table-etaf-find "Find")
    ("s-f" grid-table-etaf-find "Find")
    ("M-%" grid-table-etaf-replace "Replace")
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
    ("C-c g" grid-table-etaf-goto-address "Go to cell or range")
    ("C-c ;" grid-table-etaf-edit-note "Add or edit a note")
    ("C-c :" grid-table-etaf-delete-note "Delete notes")
    ("C-c C-l" grid-table-etaf-set-link "Add or edit a link")
    ("C-c C-o" grid-table-etaf-open-link "Open the link")
    ("C-c n d" grid-table-etaf-define-name "Name the selection")
    ("C-c n l" grid-table-etaf-goto-name "Go to a name")
    ("C-c n k" grid-table-etaf-delete-name "Delete a name")
    ("C-c a p" grid-table-etaf-trace-precedents "Trace precedents")
    ("C-c a d" grid-table-etaf-trace-dependents "Trace dependents")
    ("C-c a n" grid-table-etaf-next-trace "Go to next traced cell")
    ("C-c a x" grid-table-etaf-clear-traces "Clear traces")
    ("C-<next>" grid-table-sheet-next "Next sheet")
    ("C-<prior>" grid-table-sheet-previous "Previous sheet")
    ("C-c w n" grid-table-sheet-new "New sheet")
    ("C-c w r" grid-table-sheet-rename "Rename sheet")
    ("C-c w d" grid-table-sheet-delete "Delete sheet")
    ("C-c w s" grid-table-sheet-select "Go to sheet")
    ("C-c w <" grid-table-sheet-move-left "Move sheet left")
    ("C-c w >" grid-table-sheet-move-right "Move sheet right")
    ("C-c w h" grid-table-toggle-header-row "Header row on or off")
    ("C-c w f" grid-table-etaf-freeze-panes "Freeze rows above, columns left")
    ("C-c w u" grid-table-unfreeze-panes "Unfreeze rows and columns")
    ("C-c r i" grid-table-insert-row-above "Insert row above")
    ("C-c r h" grid-table-etaf-hide-rows "Hide rows")
    ("C-c r s" grid-table-etaf-show-rows "Show hidden rows")
    ("C-c c h" grid-table-etaf-hide-columns "Hide columns")
    ("C-c c s" grid-table-etaf-show-columns "Show hidden columns")
    ("C-c / f" grid-table-etaf-filter-column "Filter column")
    ("C-c / v" grid-table-etaf-filter-by-cell "Filter by this value")
    ("C-c / c" grid-table-etaf-clear-column-filter "Clear column filter")
    ("C-c / a" grid-table-etaf-clear-filters "Clear all filters")
    ("C-c / d" grid-table-etaf-remove-duplicates "Remove duplicate rows")
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
    ("C-c f v" grid-table-etaf-set-validation-list "Allow only listed values")
    ("C-c f V" grid-table-etaf-clear-validations "Clear validation")
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
    ("C-c i" grid-table-etaf-insert-chart "Insert chart")
    ("C-c k t" grid-table-etaf-chart-type "Chart type")
    ("C-c k s" grid-table-etaf-chart-switch "Switch chart rows and columns")
    ("C-c k h" grid-table-etaf-chart-title "Chart title")
    ("C-c k r" grid-table-etaf-chart-range "Chart data range")
    ("C-c k m" grid-table-etaf-chart-move-here "Move chart to cursor")
    ("C-c k z" grid-table-etaf-chart-resize "Chart size")
    ("C-c k n" grid-table-etaf-chart-next "Select next chart")
    ("C-c k x" grid-table-etaf-chart-deselect "Deselect chart")
    ("C-c k d" grid-table-etaf-chart-delete "Delete chart"))
  "Key bindings of the sheet, as ETAF `:keys' entries.
Printing keys are not commands: see `grid-table-etaf-typing-map' and
`grid-table-etaf-type-to-edit'.")

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
  "Keymap catching printing keys while no cell is being edited.
They start an edit of the cursor cell when `grid-table-etaf-type-to-edit'
is on, and change nothing otherwise.")

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
         (text (cond (editing value)
                     ((grid-table-false-value-p source row col value) "FALSE")
                     (t (grid-table-etaf--display-text
                         value (grid-table-cell-format source row col)))))
         (image (and (not editing) (grid-table--extract-image-path text)))
         (face (cond ((plist-get spec :cursor) 'grid-table-etaf-cursor-face)
                     ((plist-get spec :selected)
                      'grid-table-etaf-selection-face)
                     ((eq (plist-get spec :traced) 'precedent)
                      'grid-table-etaf-precedent-face)
                     ((eq (plist-get spec :traced) 'dependent)
                      'grid-table-etaf-dependent-face)))
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
                      (plist-get style :color)
                      (and (plist-get style :link)
                           (grid-table-etaf--face-color
                            'grid-table-etaf-link-face :foreground)))
           :background-color
           (if face
               (grid-table-etaf--face-color face :background)
             (plist-get style :background))
           ;; A note shows as a colored right edge, Excel's red corner.
           :border-right (or (plist-get spec :line-right)
                             (and (plist-get style :note)
                                  (grid-table-etaf--face-color
                                   'grid-table-etaf-note-face :foreground))
                             line)
           :border-bottom (or (plist-get spec :line-bottom) line)
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
                                         on-resize right bottom)
  "Return a header cell View node.
KEY names the node, GRID-ROW and GRID-COL place it, LABEL is its text
aligned by ALIGN.  MARKED paints it as part of the selection and LINE is
the grid line border.  ON-DRAG handles the pointer on the label.
ON-RESIZE, when non-nil, adds a handle on the right edge that takes the
pointer instead.  RIGHT and BOTTOM, when non-nil, replace LINE on that
edge."
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
           :border-right (or right line)
           :border-bottom (or bottom line))
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

(defun grid-table-etaf--size-text (state source)
  "Return how many rows and columns SOURCE has, as STATE shows them."
  (if (grid-table-etaf--state-filters state)
      (format "%d of %d rows shown (filtered) × %d columns"
              (1- (grid-table-etaf--row-count state))
              (1- (grid-get-row-count source))
              (grid-get-column-count source))
    (format "%d rows × %d columns"
            (grid-get-row-count source) (grid-get-column-count source))))

(defun grid-table-etaf--status-text (state source range)
  "Return the status bar text for SOURCE as STATE shows it.
RANGE is the selected (TOP LEFT BOTTOM RIGHT), or nil for a single cell.
Rows hidden by a filter are left out of the figures."
  (if-let* ((chart (grid-table-etaf--selected-chart source)))
      (format "Chart: %s (%s, %s)   C-c k t type, s switch rows/columns, h title, d delete"
              (or (plist-get chart :title) "untitled")
              (cdr (assq (plist-get chart :type) grid-chart-svg-types))
              (plist-get chart :range))
  (if (null range)
      (let* ((cursor (etaf-value (grid-table-etaf--state-cursor state)))
             (style (grid-get-style-at source (car cursor) (cdr cursor)))
             (note (plist-get style :note))
             (link (plist-get style :link)))
        (cond
         (note (concat "Note: " (replace-regexp-in-string "\n" " / " note)
                       (if link (format "   Link: %s" link) "")))
         (link (format "Link: %s   (C-c C-o opens it)" link))
         (t (grid-table-etaf--size-text state source))))
    (let ((count 0) (numbers 0) (sum 0))
      (cl-loop
       for row from (nth 0 range) to (nth 2 range)
       when (grid-table-etaf--row-shown-p state row) do
       (cl-loop
        for col from (nth 1 range) to (nth 3 range)
        when (grid-table-etaf--col-shown-p state col) do
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
                (grid-table-etaf--format-number (/ (float sum) numbers))))))))

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
         (frozen (grid-table-frozen source))
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
         ;; The line under the last frozen row and right of the last
         ;; frozen column is drawn in the text color.
         (edge (if-let* ((color (grid-table-etaf--face-color
                                 'grid-table-etaf-freeze-face :foreground)))
                   (list '(px 1) 'solid color)
                 line))
         (edge-row (and (> (car frozen) 0)
                        (car (last (cl-remove-if-not
                                    (lambda (row) (< row (car frozen)))
                                    rows)))))
         (edge-col (and (> (cdr frozen) 0)
                        (car (last (cl-remove-if-not
                                    (lambda (col) (< col (cdr frozen)))
                                    cols)))))
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
            (grid-table-etaf--drag-resize buffer col)
            (and (eql col edge-col) edge))
           children))
    (cl-loop
     for row in rows for row-slot from 0 do
     (push (grid-table-etaf--header-node
            (format "r%d" row-slot) (+ 2 row-slot) 1
            (number-to-string (1+ row)) 'right
            (and (<= sel-top row) (<= row sel-bottom)) line
            (grid-table-etaf--drag-header
             buffer (lambda () (grid-table-etaf-select-row row)))
            nil nil (and (eql row edge-row) edge))
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
             ;; Frozen columns are not next to the ones after them, so
             ;; a merge spans the columns of it that are shown as well.
             (merge-cols (and merge
                              (cl-remove-if-not
                               (lambda (shown)
                                 (<= (nth 1 merge) shown (nth 3 merge)))
                               cols)))
             (vis-top (car merge-rows))
             (vis-left (car merge-cols))
             (cell-row (if merge (nth 0 merge) row))
             (cell-col (if merge (nth 1 merge) col)))
        (unless (and merge (not (and (= row vis-top) (= col vis-left))))
          (let ((cursor-p (and (= cell-row cursor-row)
                               (= cell-col cursor-col))))
            (push
             (grid-table-etaf--cell-node
              state source cell-row cell-col
              (and merge
                   (cons (length merge-rows) (length merge-cols)))
              (list :key (+ (* 10000 row-slot) col-slot)
                    :rules rules
                    :ref (grid-table-etaf--slot-ref row-slot col-slot)
                    :grid-row (+ 2 row-slot)
                    :grid-col (+ 2 col-slot)
                    :line line
                    :line-right
                    (and edge-col
                         (eql (if merge (car (last merge-cols)) col) edge-col)
                         edge)
                    :line-bottom
                    (and edge-row
                         (eql (if merge (car (last merge-rows)) row) edge-row)
                         edge)
                    :cursor cursor-p
                    :traced (and grid-table-etaf--traces
                                 (grid-table-etaf--traced
                                  source cell-row cell-col))
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
           (append
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
              (nreverse children)))
            ;; Floating charts sit over the grid.
            (grid-table-etaf--chart-nodes state source rows cols)
            (list
             (grid-table-etaf--status-node
              state source
              (and ranged (list sel-top sel-left sel-bottom sel-right))
              line)))))))

;;;----------------------------------------------------------------------
;;; Floating charts
;;;----------------------------------------------------------------------

(defvar-local grid-table-etaf--chart nil
  "Index of the selected floating chart of the sheet shown, or nil.")

(defcustom grid-table-etaf-chart-size '(56 . 16)
  "Size of a new floating chart, as (CHARACTERS . LINES)."
  :type '(cons natnum natnum)
  :group 'grid-table)

(defvar grid-table-etaf--chart-images (make-hash-table :test #'equal)
  "Images of floating charts, keyed by everything their drawing depends on.")

(defun grid-table-etaf--selected-chart (source)
  "Return the selected floating chart of SOURCE, or nil."
  (and grid-table-etaf--chart
       (nth grid-table-etaf--chart (grid-table-charts source))))

(defun grid-table-etaf--chart-colors ()
  "Return (FOREGROUND . BACKGROUND) for drawing charts, or nils."
  (cl-flet ((color (attribute)
              (let ((color (grid-table-etaf--face-color 'default attribute)))
                (and color (not (string-prefix-p "unspecified" color))
                     (color-defined-p color)
                     (apply #'format "#%02x%02x%02x"
                            (mapcar (lambda (c) (/ c 256))
                                    (color-values color)))))))
    (cons (color :foreground) (color :background))))

(defun grid-table-etaf--line-pixels ()
  "Return the pixel height of one line."
  (max 1 (or (and (display-graphic-p) (default-line-height)) 1)))

(defvar-local grid-table-etaf--chart-data-cache nil
  "(TICK . TABLE): chart data read while the view's tick was TICK.
Cells change only with a new tick, so cursor moves and scrolling reuse
what was read.")

(defun grid-table-etaf--chart-data (source chart &optional tick)
  "Return the data CHART reads from SOURCE, as `grid-chart-svg-data' does.
TICK, the view's change counter, lets the data be read once per change."
  (if (null tick)
      (grid-table-etaf--read-chart-data source chart)
    (unless (eql (car grid-table-etaf--chart-data-cache) tick)
      (setq grid-table-etaf--chart-data-cache
            (cons tick (make-hash-table :test #'equal))))
    (let ((key (list source (plist-get chart :range) (plist-get chart :by)
                     (plist-get chart :type)))
          (table (cdr grid-table-etaf--chart-data-cache)))
      (or (gethash key table)
          (puthash key (grid-table-etaf--read-chart-data source chart) table)))))

(defun grid-table-etaf--read-chart-data (source chart)
  "Read the data CHART draws from SOURCE."
  (grid-table-chart-data source chart))

(defun grid-table-etaf--chart-image (chart data columns lines view)
  "Return the image of CHART with DATA, COLUMNS by LINES, showing VIEW.
VIEW is (LEFT TOP SHOWN-COLUMNS SHOWN-LINES) in characters and lines."
  (let* ((cw 8) (lh 18)
         (colors (grid-table-etaf--chart-colors))
         (key (list chart data columns lines view colors)))
    (or (gethash key grid-table-etaf--chart-images)
        (progn
          (when (> (hash-table-count grid-table-etaf--chart-images) 200)
            (clrhash grid-table-etaf--chart-images))
          (puthash key
                   (list 'image :type 'svg
                         :data (grid-chart-svg-render
                                chart data (* cw columns) (* lh lines)
                                (car colors) (cdr colors)
                                (list (* cw (nth 0 view)) (* lh (nth 1 view))
                                      (* cw (nth 2 view)) (* lh (nth 3 view)))))
                   grid-table-etaf--chart-images)))))

(defun grid-table-etaf--col-pixels (state col)
  "Return the pixel width column COL takes in STATE's grid."
  (1+ (* (grid-table-etaf--char-width)
         (+ 2 (aref (grid-table-etaf--state-widths state) col)))))

(defun grid-table-etaf--span (state from to axis cols merges)
  "Return how far shown line FROM is from shown line TO along AXIS of STATE.
AXIS `row' counts lines over row positions, `col' pixels over columns.
The result is negative when TO comes before FROM.  COLS and MERGES are
what row heights are measured over."
  (let ((sign (if (< to from) -1 1))
        (low (min from to)) (high (max from to))
        (total 0))
    (if (eq axis 'row)
        (cl-loop for position from low below high do
                 (cl-incf total (grid-table-etaf--row-lines
                                 state (grid-table-etaf--row-at state position)
                                 cols merges)))
      (cl-loop for col from low below high
               when (grid-table-etaf--col-shown-p state col) do
               (cl-incf total (grid-table-etaf--col-pixels state col))))
    (* sign total)))

(defun grid-table-etaf--chart-frame (state source rows cols)
  "Return the window's layout that charts are placed in, as a plist.
ROWS and COLS are the model rows and columns STATE shows of SOURCE."
  (let* ((merges (grid-get-merges source))
         (frozen (grid-table-frozen source))
         (char (grid-table-etaf--char-width))
         ;; Lines and pixels where the cells of the grid begin
         (y0 (+ 2 (grid-table-etaf--title-lines state)))
         (x0 (* char (+ 2 (grid-table-etaf--row-header-width source))))
         (frozen-rows (cl-remove-if-not (lambda (r) (< r (car frozen))) rows))
         (scrolled-rows (cl-remove-if (lambda (r) (< r (car frozen))) rows))
         (frozen-cols (cl-remove-if-not (lambda (c) (< c (cdr frozen))) cols))
         (scrolled-cols (cl-remove-if (lambda (c) (< c (cdr frozen))) cols))
         (sy0 (+ y0 (cl-loop for r in frozen-rows
                             sum (grid-table-etaf--row-lines state r cols merges))))
         (sy1 (+ sy0 (cl-loop for r in scrolled-rows
                              sum (grid-table-etaf--row-lines state r cols merges))))
         (sx0 (+ x0 (cl-loop for c in frozen-cols
                             sum (grid-table-etaf--col-pixels state c))))
         ;; A chart may run past the last column, to the window's edge.
         (sx1 (max (+ sx0 (cl-loop for c in scrolled-cols
                                   sum (grid-table-etaf--col-pixels state c)))
                   (car (grid-table-etaf--viewport state)))))
    (list :merges merges :frozen frozen :char char :y0 y0 :x0 x0
          :frozen-rows frozen-rows :scrolled-rows scrolled-rows
          :frozen-cols frozen-cols :scrolled-cols scrolled-cols
          :sy0 sy0 :sy1 sy1 :sx0 sx0 :sx1 sx1)))

(defun grid-table-etaf--chart-place (state frame chart cols)
  "Return where CHART shows in STATE's window, or nil when it does not.
FRAME is what `grid-table-etaf--chart-frame' returns and COLS the model
columns shown.  The result is (LEFT-PIXELS TOP-LINE COLUMNS LINES
CUT-COLUMNS CUT-LINES): the part of the chart shown, and how much of it
is cut off at the left and top."
  (let* ((merges (plist-get frame :merges))
         (frozen (plist-get frame :frozen))
         (char (plist-get frame :char))
         (y0 (plist-get frame :y0)) (x0 (plist-get frame :x0))
         (frozen-rows (plist-get frame :frozen-rows))
         (scrolled-rows (plist-get frame :scrolled-rows))
         (frozen-cols (plist-get frame :frozen-cols))
         (scrolled-cols (plist-get frame :scrolled-cols))
         (sy0 (plist-get frame :sy0)) (sy1 (plist-get frame :sy1))
         (sx0 (plist-get frame :sx0)) (sx1 (plist-get frame :sx1))
         (size (plist-get chart :size))
         (anchor (plist-get chart :anchor))
         ;; An anchor past the sheet's edge, as a file may hold, sits on
         ;; its last row or column.
         (row (max 0 (min (car anchor) (1- (grid-table-etaf--row-count state)))))
         (col (max 0 (min (cdr anchor)
                          (1- (length (grid-table-etaf--state-widths state))))))
         ;; A chart anchored in the frozen rows or columns stays put along
         ;; that axis; any other scrolls with the cells.
         (y (if (< row (car frozen))
                (+ y0 (cl-loop for r in frozen-rows while (< r row)
                               sum (grid-table-etaf--row-lines state r cols merges)))
              (and scrolled-rows
                   (+ sy0 (grid-table-etaf--span
                           state (grid-table-etaf--row-position state (car scrolled-rows))
                           (grid-table-etaf--row-position state row)
                           'row cols merges)))))
         (x (if (< col (cdr frozen))
                (+ x0 (cl-loop for c in frozen-cols while (< c col)
                               sum (grid-table-etaf--col-pixels state c)))
              (and scrolled-cols
                   (+ sx0 (grid-table-etaf--span
                           state (car scrolled-cols) col 'col nil nil)))))
         (clip-top (if (< row (car frozen)) y0 sy0))
         (clip-left (if (< col (cdr frozen)) x0 sx0)))
    (when (and x y)
      (let* ((vy0 (max y clip-top)) (vy1 (min (+ y (cdr size)) sy1))
             (cut-cols (if (< x clip-left) (ceiling (- clip-left x) char) 0))
             (left (+ x (* char cut-cols)))
             (shown-cols (min (- (car size) cut-cols)
                              (floor (- sx1 left) char))))
        (when (and (> shown-cols 0) (> vy1 vy0))
          (list left vy0 shown-cols (- vy1 vy0) cut-cols (- vy0 y)))))))

(defun grid-table-etaf--chart-nodes (state source rows cols)
  "Return the View nodes of SOURCE's floating charts for STATE's window.
ROWS and COLS are the model rows and columns shown."
  (let* ((buffer (grid-table-etaf--state-buffer state))
         (charts (grid-table-charts source))
         (frame (and charts
                     (grid-table-etaf--chart-frame state source rows cols)))
         (tick (etaf-value (grid-table-etaf--state-tick state))))
    (cl-loop
     for chart in charts for index from 0
     for place = (grid-table-etaf--chart-place state frame chart cols)
     when place collect
     (pcase-let* ((`(,left ,top ,columns ,lines ,cut-cols ,cut-lines) place)
                  (size (plist-get chart :size))
                  (data (grid-table-etaf--chart-data source chart tick))
                  (drawn (if (eql index grid-table-etaf--chart)
                             (plist-put (copy-sequence chart) :selected t)
                           chart)))
       (etaf-node
        'box
        (list :key (format "chart%d" index)
              :ref (intern (format "grid-table-etaf-chart-%d" index))
              :position 'absolute
              :left (list 'px left) :top (list 'lh top)
              :width (list 'ch columns) :height (list 'lh lines)
              :z-index 5
              :on-press (lambda ()
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (grid-table-etaf--select-chart state index))))
              :on-drag (grid-table-etaf--drag-chart buffer index))
        (list
         (if (display-graphic-p)
             (etaf-node 'image
                        (list :src (grid-table-etaf--chart-image
                                    drawn data (car size) (cdr size)
                                    (list cut-cols cut-lines columns lines))
                              :columns columns :rows lines)
                        nil)
           (etaf-node 'text (list :key 'chart-text)
                      (list (grid-chart-svg-text drawn data columns lines))))))))))

(defun grid-table-etaf--select-chart (state index)
  "Select floating chart INDEX of STATE's sheet; nil selects none."
  (setq grid-table-etaf--chart index)
  (grid-table-etaf--bump state))

(defun grid-table-etaf--set-charts (state charts)
  "Give STATE's sheet the floating CHARTS, as one undo step."
  (let* ((source (grid-table-etaf--source state))
         (old (copy-tree (grid-table-charts source))))
    (unless (grid-table-set-charts source charts)
      (user-error "This sheet cannot hold charts"))
    (grid-table-etaf--record state (list 'charts old (copy-tree charts)))
    (grid-table-etaf--bump state)))

(defvar-local grid-table-etaf--chart-drag nil
  "The charts as they were when a chart drag started, or nil.
It lives in the buffer, not in the handler: the grid redraws while the
chart moves, and each redraw makes a new handler.")

(defun grid-table-etaf--drag-chart (buffer index)
  "Return the pointer handler moving floating chart INDEX of BUFFER."
  (lambda (gesture)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((state grid-table-etaf--state)
               (source (grid-table-etaf--source state))
               (start grid-table-etaf--chart-drag))
          (pcase (plist-get gesture :phase)
            ('start
             (setq grid-table-etaf--chart-drag
                   (copy-tree (grid-table-charts source)))
             (setq grid-table-etaf--chart index))
            ((or 'move 'end)
             (when start
               (let* ((delta (plist-get gesture :delta))
                      (chart (copy-sequence (nth index start)))
                      (anchor (plist-get chart :anchor)))
                 (setq chart
                       (plist-put chart :anchor
                                  (cons (grid-table-etaf--walk-rows
                                         state (car anchor)
                                         (/ (cdr delta)
                                            (float (grid-table-etaf--line-pixels))))
                                        (grid-table-etaf--walk-cols
                                         state (cdr anchor) (car delta)))))
                 (let ((new (copy-tree start)))
                   (setf (nth index new) chart)
                   (grid-table-set-charts source new)
                   (when (eq (plist-get gesture :phase) 'end)
                     (unless (equal new start)
                       (grid-table-etaf--record state (list 'charts start new)))
                     (setq grid-table-etaf--chart-drag nil)))
                 (grid-table-etaf--bump state))))
            ('cancel
             (when start
               (grid-table-set-charts source start)
               (setq grid-table-etaf--chart-drag nil)
               (grid-table-etaf--bump state)))))))))
(defun grid-table-etaf--walk-rows (state row lines)
  "Return the shown row LINES lines below model ROW of STATE; negative goes up."
  (let* ((position (grid-table-etaf--row-position state row))
         (count (grid-table-etaf--row-count state))
         (step (if (< lines 0) -1 1))
         (left (abs lines)))
    (while (and (> left 0.5)
                (< -1 (+ position step) count))
      (setq position (+ position step)
            left (- left (grid-table-etaf--row-lines
                          state (grid-table-etaf--row-at state position)
                          nil nil))))
    (grid-table-etaf--row-at state position)))

(defun grid-table-etaf--walk-cols (state col pixels)
  "Return the shown column PIXELS right of model column COL of STATE."
  (let ((cols (grid-get-column-count (grid-table-etaf--source state)))
        (left (abs pixels))
        (step (if (< pixels 0) -1 1)))
    (while (and (> left (/ (grid-table-etaf--col-pixels state col) 2.0))
                (< -1 (+ col step) cols))
      (setq left (- left (grid-table-etaf--col-pixels state col))
            col (grid-table-etaf--step-col state col step)))
    col))

(defun grid-table-etaf--current-region (state)
  "Return the block of filled cells around STATE's cursor, as a range.
It grows from the cursor while a row or column next to it holds
something, as a spreadsheet picks the data for a chart."
  (let* ((source (grid-table-etaf--source state))
         (rows (grid-get-row-count source))
         (cols (grid-get-column-count source))
         (cursor (etaf-value (grid-table-etaf--state-cursor state)))
         (top (car cursor)) (bottom (car cursor))
         (left (cdr cursor)) (right (cdr cursor))
         (filled (lambda (r1 c1 r2 c2)
                   (and (<= 0 r1) (< r2 rows) (<= 0 c1) (< c2 cols)
                        (cl-loop for r from r1 to r2 thereis
                                 (cl-loop for c from c1 to c2 thereis
                                          (not (member (grid-get-raw-value-at source r c)
                                                       '(nil ""))))))))
         (grown t))
    (while grown
      (setq grown nil)
      (when (funcall filled (1- top) left (1- top) right) (cl-decf top) (setq grown t))
      (when (funcall filled (1+ bottom) left (1+ bottom) right) (cl-incf bottom) (setq grown t))
      (when (funcall filled top (1- left) bottom (1- left)) (cl-decf left) (setq grown t))
      (when (funcall filled top (1+ right) bottom (1+ right)) (cl-incf right) (setq grown t)))
    (list top left bottom right)))

(defun grid-table-etaf--read-chart-type (&optional default)
  "Read a chart type, offering DEFAULT first."
  (let* ((names (mapcar #'cdr grid-chart-svg-types))
         (default (cdr (assq (or default 'column) grid-chart-svg-types)))
         (name (completing-read (format "Chart type (default %s): " default)
                                names nil t nil nil default)))
    (car (rassoc name grid-chart-svg-types))))

(defun grid-table-etaf-insert-chart (type)
  "Put a floating chart of TYPE over the sheet, drawing the selected cells.
With no cells selected it draws the block of filled cells around the
cursor.  The chart sits right of its data, and keeps drawing whatever
the cells hold."
  (interactive (list (grid-table-etaf--read-chart-type)))
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (cursor (etaf-value (grid-table-etaf--state-cursor state)))
         (anchor (etaf-value (grid-table-etaf--state-anchor state)))
         (range (if (equal cursor anchor)
                    (grid-table-etaf--current-region state)
                  (grid-table-etaf-selection)))
         (cols (grid-get-column-count source))
         (chart (list :anchor (cons (nth 0 range)
                                    (if (< (1+ (nth 3 range)) cols)
                                        (1+ (nth 3 range))
                                      (nth 1 range)))
                      :size grid-table-etaf-chart-size
                      :type type
                      :range (apply #'grid-chart-svg-range-text range)
                      :by nil :title nil))
         (charts (append (grid-table-charts source) (list chart))))
    (setq grid-table-etaf--chart (1- (length charts)))
    (grid-table-etaf--set-charts state charts)
    (message "Chart of %s added" (plist-get chart :range))))

(defun grid-table-etaf--chart-index (state)
  "Return the index of the chart a chart command works on in STATE's sheet.
That is the selected chart, the only chart, or one read from the user."
  (let* ((source (grid-table-etaf--source state))
         (charts (grid-table-charts source)))
    (cond ((null charts) (user-error "No chart on this sheet; C-c i adds one"))
          ((and grid-table-etaf--chart (nth grid-table-etaf--chart charts))
           grid-table-etaf--chart)
          ((null (cdr charts)) 0)
          (t (let* ((names (cl-loop for chart in charts for i from 0
                                    collect (cons (format "%d: %s %s (%s)" (1+ i)
                                                          (cdr (assq (plist-get chart :type)
                                                                     grid-chart-svg-types))
                                                          (or (plist-get chart :title) "")
                                                          (plist-get chart :range))
                                                  i)))
                    (name (completing-read "Chart: " names nil t)))
               (cdr (assoc name names)))))))

(defun grid-table-etaf--change-chart (function)
  "Replace the chart a chart command works on with FUNCTION's result.
FUNCTION gets a copy of the chart; nil from it deletes the chart."
  (let* ((state (grid-table-etaf--state-or-error))
         (index (grid-table-etaf--chart-index state))
         (charts (copy-tree (grid-table-charts (grid-table-etaf--source state))))
         (chart (funcall function (nth index charts))))
    (setq grid-table-etaf--chart (and chart index))
    (grid-table-etaf--set-charts
     state (if chart
               (progn (setf (nth index charts) chart) charts)
             (append (cl-subseq charts 0 index) (nthcdr (1+ index) charts))))))

(defun grid-table-etaf-chart-type (type)
  "Draw the selected chart as TYPE."
  (interactive
   (let* ((state (grid-table-etaf--state-or-error))
          (chart (nth (grid-table-etaf--chart-index state)
                      (grid-table-charts (grid-table-etaf--source state)))))
     (list (grid-table-etaf--read-chart-type (plist-get chart :type)))))
  (grid-table-etaf--change-chart (lambda (chart) (plist-put chart :type type))))

(defun grid-table-etaf-chart-switch ()
  "Make the selected chart's series run the other way, rows for columns."
  (interactive)
  (let ((state (grid-table-etaf--state-or-error)))
    (grid-table-etaf--change-chart
     (lambda (chart)
       (let ((by (plist-get (grid-table-etaf--chart-data
                             (grid-table-etaf--source state) chart)
                            :by)))
         (plist-put chart :by (if (eq by 'rows) 'columns 'rows)))))))

(defun grid-table-etaf-chart-title (title)
  "Give the selected chart the TITLE; an empty one removes it."
  (interactive
   (let* ((state (grid-table-etaf--state-or-error))
          (chart (nth (grid-table-etaf--chart-index state)
                      (grid-table-charts (grid-table-etaf--source state)))))
     (list (read-string "Chart title: " (plist-get chart :title)))))
  (grid-table-etaf--change-chart
   (lambda (chart)
     (plist-put chart :title (and (not (string-empty-p title)) title)))))

(defun grid-table-etaf-chart-range ()
  "Make the selected chart draw the selected cells."
  (interactive)
  (let ((range (grid-table-etaf-selection)))
    (grid-table-etaf--change-chart
     (lambda (chart)
       (plist-put chart :range (apply #'grid-chart-svg-range-text range))))))

(defun grid-table-etaf-chart-move-here ()
  "Put the top left corner of the selected chart on the cursor cell."
  (interactive)
  (let ((cursor (grid-table-etaf--cursor)))
    (grid-table-etaf--change-chart
     (lambda (chart) (plist-put chart :anchor (cons (car cursor) (cdr cursor)))))))

(defun grid-table-etaf-chart-resize (columns lines)
  "Make the selected chart COLUMNS characters wide and LINES lines high."
  (interactive
   (let* ((state (grid-table-etaf--state-or-error))
          (chart (nth (grid-table-etaf--chart-index state)
                      (grid-table-charts (grid-table-etaf--source state))))
          (size (plist-get chart :size)))
     (list (read-number "Width in characters: " (car size))
           (read-number "Height in lines: " (cdr size)))))
  (unless (and (integerp columns) (integerp lines)
               (< 7 columns 1000) (< 3 lines 1000))
    (user-error "A chart is 8 to 999 characters wide and 4 to 999 lines high"))
  (grid-table-etaf--change-chart
   (lambda (chart) (plist-put chart :size (cons columns lines)))))

(defun grid-table-etaf-chart-delete ()
  "Delete the selected chart."
  (interactive)
  (grid-table-etaf--change-chart #'ignore))

(defun grid-table-etaf-chart-next ()
  "Select the next chart of the sheet and show where it is."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (charts (grid-table-charts (grid-table-etaf--source state))))
    (unless charts (user-error "No chart on this sheet; C-c i adds one"))
    (let* ((index (if grid-table-etaf--chart
                      (mod (1+ grid-table-etaf--chart) (length charts))
                    0))
           (anchor (plist-get (nth index charts) :anchor)))
      (grid-table-etaf-goto (car anchor) (cdr anchor))
      (grid-table-etaf--select-chart state index))))

(defun grid-table-etaf-chart-deselect ()
  "Select no chart."
  (interactive)
  (grid-table-etaf--select-chart (grid-table-etaf--state-or-error) nil))

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
         (top-ref (grid-table-etaf--state-top state))
         (frozen (grid-table-frozen (grid-table-etaf--source state))))
    ;; A frozen row or column is shown wherever the window is.
    (when (< (etaf-value top-ref) (car frozen))
      (etaf-set-value top-ref (car frozen)))
    (when (< (etaf-value left-ref) (cdr frozen))
      (etaf-set-value left-ref (cdr frozen)))
    (when (and (>= col (cdr frozen)) (< col (etaf-value left-ref)))
      (etaf-set-value left-ref col))
    ;; A frozen column the window has no room for cannot be scrolled to.
    (while (and (>= col (cdr frozen))
                (not (memq col (grid-table-etaf--visible-cols
                                state (etaf-value left-ref)))))
      (etaf-set-value left-ref (1+ (etaf-value left-ref))))
    (when (and (>= row (car frozen)) (< row (etaf-value top-ref)))
      (etaf-set-value top-ref row))
    (let ((cols (grid-table-etaf--visible-cols state (etaf-value left-ref))))
      (while (and (>= row (car frozen))
                  (not (memq row (grid-table-etaf--visible-rows
                                  state (etaf-value top-ref) cols))))
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
         ;; A filtered-out row is not a place to be: take the next shown.
         (row (grid-table-etaf--row-at
               state (grid-table-etaf--row-position state row)))
         ;; Nor is a hidden column.
         (col (grid-table-etaf--shown-col state col)))
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

(defvar grid-table-etaf--goto-history nil
  "Addresses typed to `grid-table-etaf-goto-address'.")

(defun grid-table-etaf--parse-address (text)
  "Return (SHEET TOP LEFT BOTTOM RIGHT) for the address TEXT, or nil.
TEXT is a cell (D120), a range (A1:B3), whole columns (B:D) or whole
rows (5:9), after an optional sheet prefix (Sheet2!A1, \='My Sheet\='!A1).
SHEET is nil without a prefix.  Ends left open, as the rows of a whole
column, are nil."
  (let* ((text (string-trim text))
         (prefix (and (> (length text) 0)
                      (grid-parser-sheet-prefix-at text 0)))
         (rest (if prefix (substring text (car prefix)) text))
         (ends (split-string rest ":"))
         (start (grid-calc--reference-coords (car ends)))
         (end (if (cdr ends) (grid-calc--reference-coords (cadr ends)) start)))
    (when (and start end (null (cddr ends))
               ;; Both ends are cells, both columns, or both rows.
               (eq (null (car start)) (null (car end)))
               (eq (null (cdr start)) (null (cdr end)))
               ;; Past column XFD a word is a word, not a column.
               (< (or (cdr start) 0) 16384) (< (or (cdr end) 0) 16384))
      (list (cdr prefix)
            (and (car start) (min (car start) (car end)))
            (and (cdr start) (min (cdr start) (cdr end)))
            (and (car start) (max (car start) (car end)))
            (and (cdr start) (max (cdr start) (cdr end)))))))

(defun grid-table-etaf-goto-address (address)
  "Go to ADDRESS, a cell, range, whole column or row, maybe on another sheet.
D120 moves the cursor there; A1:B3 selects that range, B:B a column and
5:5 a row; Sheet2!A1 shows Sheet2 first, as the name box of a
spreadsheet does."
  (interactive
   (list (completing-read
          "Go to (e.g. D120, A1:B3, Sheet2!A1): "
          (append
           (mapcar #'car (and (grid-workbook-of grid-table--data-source)
                              (grid-workbook-names
                               (grid-workbook-of grid-table--data-source))))
           (mapcar (lambda (sheet)
                     (grid-calc-sheet-prefix
                             (grid-workbook-sheet-name sheet)))
                   (cdr (grid-table-sheets))))
          nil nil nil 'grid-table-etaf--goto-history)))
  (pcase-let ((`(,sheet ,top ,left ,bottom ,right)
               ;; A name first: Qty is a name before it is column QTY.
               (or (when-let* ((workbook (grid-workbook-of
                                          grid-table--data-source))
                               (definition (grid-workbook-name-definition
                                            workbook (string-trim address))))
                     (grid-table-etaf--parse-address (substring definition 1)))
                   (grid-table-etaf--parse-address address)
                   (user-error "Not a cell, range or name: %s" address))))
    (when sheet
      (grid-table-switch-to-sheet
       (or (grid-workbook-sheet (grid-table--workbook) sheet)
           (user-error "No sheet called %s" sheet))))
    (let* ((source grid-table--data-source)
           (rows (grid-get-row-count source))
           (cols (grid-get-column-count source)))
      (when (or (and top (>= top rows)) (and left (>= left cols)))
        (user-error "%s is outside the sheet, which has %d rows and %d columns"
                    address rows cols))
      (grid-table-etaf-goto (or bottom (1- rows)) (or right (1- cols)))
      (grid-table-etaf-goto (or top 0) (or left 0)
                            (or (not (eql top bottom)) (not (eql left right))
                                (null top) (null left))))))

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
     (cond ((> cols 0) (grid-table-etaf--step-col state (nth 3 extent) cols))
           ((< cols 0) (grid-table-etaf--step-col state (nth 1 extent) cols))
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
         (edge (if (> direction 0) (nth 3 extent) (nth 1 extent)))
         (col (grid-table-etaf--step-col state edge direction)))
    ;; Stepping off either end of the row wraps to the next or previous row.
    (when (= col edge)
      (setq position (mod (+ position direction) rows)
            col (grid-table-etaf--shown-col
                 state (if (> direction 0) 0 (1- cols)) (< direction 0))))
    (grid-table-etaf-goto (grid-table-etaf--row-at state position) col)))

(defun grid-table-etaf--page-size ()
  "Return how many rows the window currently shows."
  (let* ((state (grid-table-etaf--state-or-error))
         (cols (grid-table-etaf--visible-cols
                state (etaf-value (grid-table-etaf--state-left state)))))
    ;; Frozen rows are on every page.
    (max 1 (cl-count-if
            (lambda (row)
              (>= row (car (grid-table-frozen
                            (grid-table-etaf--source state)))))
            (grid-table-etaf--visible-rows
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
         (top (grid-table-etaf--state-top state))
         (frozen (car (grid-table-frozen (grid-table-etaf--source state)))))
    ;; The frozen rows do not scroll: the window starts under them.
    (etaf-set-value top (max frozen
                             (grid-table-etaf--step-row
                              state (max frozen (etaf-value top)) rows)))))

(defun grid-table-etaf-scroll-down ()
  "Scroll the sheet three rows down."
  (interactive)
  (grid-table-etaf--scroll 3))

(defun grid-table-etaf-scroll-up ()
  "Scroll the sheet three rows up."
  (interactive)
  (grid-table-etaf--scroll -3))

(defun grid-table-etaf-freeze-panes ()
  "Keep the rows above the cursor and the columns left of it in place.
They stay at the top and the left of the window while the rest of the
sheet scrolls, and are saved with the grid.  With the cursor in the
first cell nothing is above or left of it, and every row and column
scrolls again, as after `grid-table-unfreeze-panes'."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (cursor (grid-table-etaf--cursor))
         (extent (grid-table-etaf--cell-extent (car cursor) (cdr cursor)))
         (rows (nth 0 extent))
         (cols (nth 1 extent))
         (old (grid-table-frozen source)))
    (unless (grid-table-set-frozen source rows cols)
      (user-error "This grid cannot keep rows and columns in place"))
    ;; A window shows the frozen rows and columns it has room for: all of
    ;; them have to fit, with the cell under the cursor.
    (let* ((shown-cols (grid-table-etaf--visible-cols state cols))
           (shown-rows (grid-table-etaf--visible-rows state rows shown-cols))
           (last-row (and (> rows 0)
                          (grid-table-etaf--row-at
                           state (1- (grid-table-etaf--row-position
                                      state rows))))))
      (unless (and (or (zerop cols) (memq (1- cols) shown-cols))
                   (or (null last-row) (>= last-row rows)
                       (memq last-row shown-rows)))
        (grid-table-set-frozen source (car old) (cdr old))
        (user-error "The window is too small to freeze that much")))
    (grid-table-etaf-note-frozen old)
    (grid-table-etaf--bump state)
    (grid-table-etaf--settle state)
    (message (if (and (zerop rows) (zerop cols))
                 "No row or column is frozen"
               "Frozen: %d row%s, %d column%s")
             rows (if (= rows 1) "" "s") cols (if (= cols 1) "" "s"))))

(defun grid-table-etaf-first-cell ()
  "Move the cursor to the first cell of the sheet."
  (interactive)
  (grid-table-etaf-goto 0 0))

(defun grid-table-etaf-last-cell ()
  "Move the cursor to the last cell of the sheet."
  (interactive)
  (grid-table-etaf-goto (1- (grid-get-row-count grid-table--data-source))
                        (grid-table-etaf--shown-col
                         grid-table-etaf--state
                         (1- (grid-get-column-count grid-table--data-source))
                         t)))

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
                        (grid-table-etaf--shown-col
                         grid-table-etaf--state
                         (1- (grid-get-column-count grid-table--data-source))
                         t)))

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
           (value (grid-table-etaf--end-session state))
           ;; A cell that only takes some values refuses others, and the
           ;; edit is dropped, as a spreadsheet's stop alert does.
           (problem (and value
                         (grid-table-validation-problem
                          (grid-table-etaf--source state)
                          (nth 0 extent) (nth 1 extent) value))))
      (if (and value (not problem))
          (grid-table-etaf--enter-cells
           state (list (list (nth 0 extent) (nth 1 extent) value)))
        (grid-table-etaf--bump state)
        (grid-table-etaf--settle state)
        (when problem
          (user-error "%s" problem))))))

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
         (buffer (grid-table-etaf--state-buffer state))
         (cursor (grid-table-etaf--cursor))
         (rule (and (not initial)
                    (grid-table-validation-at (grid-table-etaf--source state)
                                              (car cursor) (cdr cursor)))))
    (if (eq (nth 4 rule) 'list)
        (grid-table-etaf--choose state cursor rule)
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
           (signal (car err) (cdr err))))))))

(defun grid-table-etaf--choose (state cursor rule)
  "Have the cell at CURSOR take one of the values its list RULE allows.
The choice is read in the minibuffer; an empty one clears the cell."
  (let* ((source (grid-table-etaf--source state))
         (options (grid-table-validation-options source rule))
         (current (grid-get-raw-value-at source (car cursor) (cdr cursor)))
         (choice (completing-read
                  "Value (empty clears): " options nil
                  (lambda (text)
                    (or (string-empty-p text)
                        (member-ignore-case text options)))
                  nil nil (and (member current options) current))))
    (grid-table-etaf--enter-cells
     state (list (list (car cursor) (cdr cursor) choice)))))

(defcustom grid-table-etaf-type-to-edit t
  "Non-nil makes typing a character replace the cursor cell, as in Excel.
nil leaves the cell alone until RET or F2 starts an edit, so a stray key
changes nothing."
  :type 'boolean
  :group 'grid-table)

(defun grid-table-etaf-self-insert ()
  "Handle a printing key typed while no cell is being edited.
With `grid-table-etaf-type-to-edit' it starts editing the cursor cell
with the typed character as its text; otherwise it only says how to
edit."
  (interactive)
  (cond ((not grid-table-etaf-type-to-edit)
         (message "Press RET or F2 to edit this cell"))
        ((characterp last-command-event)
         (grid-table-etaf-edit-cell (string last-command-event)))
        (t (grid-table-etaf-edit-cell))))

(defun grid-table-etaf-clear-cells ()
  "Empty every cell of the selected range."
  (interactive)
  (let ((state (grid-table-etaf--state-or-error))
        (range (grid-table-etaf-selection))
        changes)
    (cl-loop for row from (nth 0 range) to (nth 2 range)
             when (grid-table-etaf--row-shown-p state row) do
             (cl-loop for col from (nth 1 range) to (nth 3 range)
                      when (grid-table-etaf--col-shown-p state col) do
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

(defun grid-table-etaf-note-frozen (old)
  "Record for undo that the frozen rows and columns were OLD, a (ROWS . COLS).
Nothing is recorded when they did not change."
  (when-let* ((state grid-table-etaf--state))
    (let ((new (grid-table-frozen (grid-table-etaf--source state))))
      (unless (equal old new)
        (grid-table-etaf--record state (list 'frozen old new))))))

(defcustom grid-table-etaf-undo-model-cells 1000000
  "How many cells the whole-sheet copies kept for undo may hold in all.
Undoing an added or deleted line, a sort or a merge needs the sheet as it
was before.  When the copies the undo history holds exceed this many
cells, the oldest such changes are forgotten; the newest change can
always be undone.  nil means no limit beyond the number of changes."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'grid-table)

(defun grid-table-etaf--record-models (record)
  "Return the models RECORD keeps, looking inside groups."
  (pcase record
    (`(model ,old ,new) (list old new))
    (`(group . ,records) (mapcan #'grid-table-etaf--record-models records))))

(defun grid-table-etaf--trim-undo (undo)
  "Cut UNDO, a list of records newest first, to fit the undo limits.
Keep at most `grid-table-etaf--undo-limit' records and no more models
than `grid-table-etaf-undo-model-cells' allows, but always the first."
  (let ((seen (make-hash-table :test #'eq))
        (cells 0)
        (count 0)
        (budget grid-table-etaf-undo-model-cells))
    (cl-loop for tail on undo
             do (cl-incf count)
                (dolist (model (grid-table-etaf--record-models (car tail)))
                  (unless (or (null model) (gethash model seen))
                    (puthash model t seen)
                    (cl-incf cells (* (grid-data-model-rows model)
                                      (grid-data-model-cols model)))))
             when (and (cdr tail)
                       (or (>= count grid-table-etaf--undo-limit)
                           (and budget
                                (grid-table-etaf--record-models (cadr tail))
                                (> (+ cells
                                      (cl-loop for model in (grid-table-etaf--record-models
                                                             (cadr tail))
                                               unless (or (null model)
                                                          (gethash model seen))
                                               sum (* (grid-data-model-rows model)
                                                      (grid-data-model-cols model))))
                                   budget))))
             do (setcdr tail nil))
    undo))

(defun grid-table-etaf--record (state record)
  "Push RECORD on STATE's undo history and drop its redo history."
  (setf (grid-table-etaf--state-undo state)
        (grid-table-etaf--trim-undo
         (cons record (grid-table-etaf--state-undo state)))
        (grid-table-etaf--state-redo state) nil))

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
            (grid-model-covered-cell nil)))))
    (nreverse applied)))

(defun grid-table-etaf--set-cells (state changes)
  "Store CHANGES, a list of (ROW COL VALUE), as one undoable step in STATE.
Return the number of cells that changed."
  (let ((applied (grid-table-etaf--write-cells state changes)))
    (when applied
      (setq grid-table-etaf--traces nil)
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
      (setq grid-table-etaf--traces nil)
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
       (setq grid-table-etaf--traces nil)
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
      (`(width ,col ,old ,new)
       (grid-table-etaf--restore-column-width state col (if forward new old)))
      (`(names ,workbook ,old ,new)
       (grid-workbook-set-names workbook (if forward new old))
       (grid-table-etaf--bump state))
      (`(validations ,old ,new)
       (grid-table-set-validations source (if forward new old))
       (grid-table-etaf--bump state))
      (`(charts ,old ,new)
       (grid-table-set-charts source (if forward new old))
       (setq grid-table-etaf--chart nil)
       (grid-table-etaf--bump state))
      (`(hidden ,old ,new)
       (let ((hidden (if forward new old)))
         (grid-table-set-hidden source (car hidden) (cdr hidden)))
       (grid-table-etaf--apply-filters state)
       (grid-table-etaf--bump state)
       (grid-table-etaf--settle state))
      (`(frozen ,old ,new)
       (let ((frozen (if forward new old)))
         (grid-table-set-frozen source (car frozen) (cdr frozen)))
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
      for col from (nth 1 range) to (nth 3 range)
      when (grid-table-etaf--col-shown-p state col) do
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
  "Show the selection in the general format.
A date formula then shows the number behind its date."
  (interactive)
  (grid-table-etaf--set-format :format (list 'general)))

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

(defconst grid-table-etaf--content-keys '(:note :link)
  "Style properties that belong to a cell's content, not its look.
Clearing formats and pasting only formats leave them alone.")

(defun grid-table-etaf--content-of (style)
  "Return the part of STYLE that `grid-table-etaf--content-keys' name."
  (cl-loop for (name value) on style by #'cddr
           when (memq name grid-table-etaf--content-keys)
           append (list name value)))

(defun grid-table-etaf-clear-format ()
  "Remove every style from the selection, but not notes and links."
  (interactive)
  (grid-table-etaf--restyle #'grid-table-etaf--content-of))

;;;----------------------------------------------------------------------
;;; Notes and links
;;;----------------------------------------------------------------------

(defun grid-table-etaf--cursor-cell ()
  "Return (ROW . COL) of the cursor cell, the top left one of a merge."
  (let* ((cursor (grid-table-etaf--cursor))
         (extent (grid-table-etaf--cell-extent (car cursor) (cdr cursor))))
    (cons (nth 0 extent) (nth 1 extent))))

(defun grid-table-etaf--cursor-style ()
  "Return the style of the cursor cell."
  (let ((cell (grid-table-etaf--cursor-cell)))
    (grid-get-style-at (grid-table-etaf--source (grid-table-etaf--state-or-error))
                       (car cell) (cdr cell))))

(defun grid-table-etaf-edit-note (note)
  "Give the cursor cell NOTE, a remark shown when the cursor is on it.
An empty NOTE removes it.  The cell's right edge shows that it has one,
and the status bar shows the note."
  (interactive
   (list (read-string "Note (empty removes it): "
                      (plist-get (grid-table-etaf--cursor-style) :note))))
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (cell (grid-table-etaf--cursor-cell))
         (style (copy-sequence (grid-get-style-at source (car cell) (cdr cell))))
         (note (string-trim note)))
    (unless (gethash :set-style-at source)
      (user-error "This grid's data source does not store notes"))
    (grid-table-etaf--set-styles
     state (list (list (car cell) (cdr cell)
                       (if (string-empty-p note)
                           (grid-table-etaf--plist-without style :note)
                         (plist-put style :note note)))))))

(defun grid-table-etaf-delete-note ()
  "Remove the notes of the selected cells."
  (interactive)
  (grid-table-etaf--set-format :note nil))

(defun grid-table-etaf-set-link (link)
  "Make the selected cells link to LINK; an empty LINK removes the link.
LINK is a web address, a file name, or #Sheet2!A1 for a cell of this
workbook.  `grid-table-etaf-open-link' follows it."
  (interactive
   (list (read-string "Link (URL, file or #A1; empty removes it): "
                      (plist-get (grid-table-etaf--cursor-style) :link))))
  (let ((link (string-trim link)))
    (grid-table-etaf--set-format :link (and (not (string-empty-p link)) link))))

(defun grid-table-etaf-open-link ()
  "Follow the link of the cursor cell.
A web address opens in the browser, #ADDRESS goes to that cell or range
of the workbook, and anything else is visited as a file, relative to
the grid's own."
  (interactive)
  (let ((link (plist-get (grid-table-etaf--cursor-style) :link)))
    (cond
     ((null link) (user-error "This cell has no link"))
     ((string-prefix-p "#" link)
      (grid-table-etaf-goto-address (substring link 1)))
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" link)
      (browse-url link))
     (t (find-file (expand-file-name link))))))

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
           (cl-loop for col from (nth 1 range) to (nth 3 range)
                    when (grid-table-etaf--col-shown-p state col) collect
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
                   (let ((value (grid-get-computed-value-at source row col)))
                     (if (grid-table-false-value-p source row col value)
                         "FALSE"
                       (grid-table-etaf--display-text
                        value (grid-table-cell-format source row col)))))))))
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
                          when (grid-table-etaf--col-shown-p state col)
                          collect (list row col nil))))))))

(defun grid-table-etaf--shifted (value origin row col)
  "Return VALUE as it reads when its range moves from ORIGIN to ROW and COL.
ORIGIN is the (ROW . COL) the copied range started at, or nil when the
value should be pasted as it is."
  (if origin
      (grid-calc-shift-formula value (- row (car origin)) (- col (cdr origin)))
    value))

(defun grid-table-etaf--transpose (rows)
  "Return the matrix ROWS turned so that its rows become its columns.
Short rows are padded with nil."
  (let ((width (apply #'max 0 (mapcar #'length rows))))
    (cl-loop for col below width collect
             (mapcar (lambda (row) (nth col row)) rows))))

(defun grid-table-etaf--paste-material (own text mode top left)
  "Return (ROWS STYLES ORIGIN OWN) for pasting at TOP and LEFT in MODE.
OWN is the grid's own clipboard entry when it holds TEXT, else nil.
MODE nil pastes values, formulas and formats; `values' pastes what the
copied cells show, as if typed; `formats' only formats; `transpose'
turns rows into columns.  ROWS are the values, STYLES the formats or
nil, ORIGIN the cell to move formula references from, or nil; OWN is
nil when the values are to be read as typed."
  (let ((rows (if own (nth 1 own) (grid-table-etaf--tsv-decode text)))
        (styles (and own (nth 5 own)))
        (origin (and own (not (nth 4 own)) (cons (nth 2 own) (nth 3 own)))))
    (pcase mode
      ('values
       (when own
         ;; What the cells show, except that text kept as text stays so.
         (setq rows (cl-mapcar
                     (lambda (shown raws)
                       (cl-mapcar (lambda (value raw)
                                    (if (string-prefix-p "'" raw) raw value))
                                  shown raws))
                     (grid-table-etaf--tsv-decode (car own)) rows)))
       (list rows nil nil nil))
      ('formats
       (unless own
         (user-error "Only cells copied in a grid carry formats"))
       (list rows styles nil own))
      ('transpose
       ;; Each formula moves by its own distance: the cell copied from row
       ;; I and column J lands in row J and column I of the target.
       (when origin
         (setq rows
               (cl-loop for values in rows for i from 0 collect
                        (cl-loop for value in values for j from 0 collect
                                 (grid-calc-shift-formula
                                  value
                                  (- (+ top j) (+ (car origin) i))
                                  (- (+ left i) (+ (cdr origin) j)))))))
       (list (mapcar (lambda (row) (mapcar (lambda (v) (or v "")) row))
                     (grid-table-etaf--transpose rows))
             (and styles (grid-table-etaf--transpose styles))
             nil own))
      (_ (list rows styles origin own)))))

(defun grid-table-etaf-paste-values ()
  "Paste what the copied cells show, without their formulas or formats."
  (interactive)
  (grid-table-etaf-paste 'values))

(defun grid-table-etaf-paste-formats ()
  "Paste only the formats of the copied cells."
  (interactive)
  (grid-table-etaf-paste 'formats))

(defun grid-table-etaf-paste-transposed ()
  "Paste the copied cells with their rows turned into columns."
  (interactive)
  (grid-table-etaf-paste 'transpose))

(defun grid-table-etaf-paste (&optional mode)
  "Paste the current kill at the selected range.
The top left cell of the range receives the first value.  A single
copied cell fills the whole range.  The grid grows to take values that
reach past its last row or column; a data source that cannot grow drops
them.  Copied formulas have their relative references moved
by the distance they travel, as in a spreadsheet; cut formulas do not.
Cells copied in a grid bring their formats; text from elsewhere leaves
the formats of the cells it lands in.  Rows hidden by a filter are
passed over: the pasted lines go to the rows that are shown.
MODE is as for `grid-table-etaf--paste-material'."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (text (substring-no-properties (current-kill 0)))
         (range (grid-table-etaf-selection))
         (top (nth 0 range))
         (left (nth 1 range))
         (material (grid-table-etaf--paste-material
                    (and grid-table-etaf--clipboard
                         (equal text (car grid-table-etaf--clipboard))
                         grid-table-etaf--clipboard)
                    text mode top left))
         (rows (nth 0 material))
         ;; Where a copied range came from, to move its references.
         (origin (nth 2 material))
         (own (nth 3 material))
         (single (and (null (cdr rows)) (null (cdr (car rows)))))
         (row-count (grid-get-row-count source))
         ;; The rows the pasted lines land in: rows hidden by a filter
         ;; are passed over, as copying and clearing pass over them.
         (targets (let ((row top) targets)
                    (while (< (length targets) (length rows))
                      (when (or (>= row row-count)
                                (grid-table-etaf--row-shown-p state row))
                        (push row targets))
                      (setq row (1+ row)))
                    (nreverse targets)))
         (col-count (grid-get-column-count source))
         ;; Hidden columns are passed over in the same way.
         (col-targets (let ((col left) targets
                            (width (apply #'max (mapcar #'length rows))))
                        (while (< (length targets) width)
                          (when (or (>= col col-count)
                                    (grid-table-etaf--col-shown-p state col))
                            (push col targets))
                          (setq col (1+ col)))
                        (nreverse targets)))
         ;; How far the pasted block reaches past the grid
         (more-rows (if (or single (null (gethash :grow source)))
                        0
                      (max 0 (- (1+ (car (last targets))) row-count))))
         (more-cols (if (or single (null (gethash :grow source)))
                        0
                      (max 0 (- (1+ (car (last col-targets))) col-count))))
         (max-row (+ more-rows (1- row-count)))
         (max-col (+ more-cols (1- col-count)))
         (bottom top)
         (right left)
         (styled (nth 1 material))
         changes styles)
    (unless rows (user-error "Nothing to paste"))
    (if single
        (cl-loop for row from top to (nth 2 range)
                 when (grid-table-etaf--row-shown-p state row) do
                 (cl-loop for col from left to (nth 3 range)
                          when (grid-table-etaf--col-shown-p state col) do
                          (push (list row col
                                      (grid-table-etaf--shifted
                                       (caar rows) origin row col))
                                changes)
                          (when styled
                            (push (list row col (caar styled)) styles)))
                 finally (setq bottom (nth 2 range) right (nth 3 range)))
      (cl-loop
       for values in rows for row in targets for line from 0
       while (<= row max-row) do
       (setq bottom row)
       (cl-loop for value in values for col in col-targets
                for column from 0
                while (<= col max-col) do
                (setq right (max right col))
                (push (list row col
                            ;; Every cell travels as far as the first one.
                            (grid-table-etaf--shifted value origin top left))
                      changes)
                (when styled
                  (push (list row col
                              (nth column (nth line styled)))
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
                                state (if (eq mode 'formats)
                                          nil
                                        (nreverse changes)))
                          (grid-table-etaf--set-styles
                           state
                           (if (eq mode 'formats)
                               ;; Formats only: the cells keep their own
                               ;; notes and links.
                               (mapcar
                                (pcase-lambda (`(,row ,col ,style))
                                  (list row col
                                        (append
                                         (cl-loop for (name value) on style
                                                  by #'cddr
                                                  unless (memq name grid-table-etaf--content-keys)
                                                  append (list name value))
                                         (grid-table-etaf--content-of
                                          (grid-get-style-at source row col)))))
                                (nreverse styles))
                             (nreverse styles))))
                      ;; Text from elsewhere is read as if it were typed.
                      (grid-table-etaf--enter-cells
                       state (nreverse changes)))))))
      (grid-table-etaf-goto bottom right)
      (grid-table-etaf-goto top left t)
      (if (eq mode 'formats)
          (message "Pasted formats")
        (message "Pasted %d cell%s" count (if (= count 1) "" "s"))))))

;;;----------------------------------------------------------------------
;;; Hiding rows and columns
;;;----------------------------------------------------------------------

(defun grid-table-etaf--set-hidden (state rows cols)
  "Hide ROWS and COLS of STATE's sheet, as one undoable step."
  (let* ((source (grid-table-etaf--source state))
         (old (grid-table-hidden source)))
    (unless (grid-table-set-hidden source rows cols)
      (user-error "This grid cannot hide rows and columns"))
    (let ((new (grid-table-hidden source)))
      (unless (equal old new)
        (grid-table-etaf--record state (list 'hidden old new))))
    (grid-table-etaf--apply-filters state)
    (grid-table-etaf--bump state)
    ;; The cursor leaves a line that was just hidden.
    (let ((cursor (grid-table-etaf--cursor)))
      (grid-table-etaf-goto (car cursor) (cdr cursor)))))

(defun grid-table-etaf-hide-rows ()
  "Hide the rows of the selection.
They keep their data, are saved hidden, and are passed over by movement,
copying, clearing and pasting.  `grid-table-etaf-show-rows' shows them."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (hidden (grid-table-hidden source))
         ;; Rows a filter hides are left to the filter.
         (rows (append (car hidden)
                       (cl-remove-if-not
                        (lambda (row) (grid-table-etaf--row-shown-p state row))
                        (number-sequence (nth 0 range) (nth 2 range))))))
    (when (cl-every (lambda (row)
                      (or (memq row rows)
                          (not (grid-table-etaf--row-shown-p state row))))
                    (number-sequence 0 (1- (grid-get-row-count source))))
      (user-error "At least one row has to stay shown"))
    (grid-table-etaf--set-hidden state rows (cdr hidden))))

(defun grid-table-etaf-show-rows ()
  "Show the hidden rows inside the selection.
Select across the place a row is missing from, or the whole sheet with
\\[grid-table-etaf-select-all], to show them."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (hidden (grid-table-hidden source)))
    (grid-table-etaf--set-hidden
     state
     (cl-remove-if (lambda (row) (<= (nth 0 range) row (nth 2 range)))
                   (car hidden))
     (cdr hidden))))

(defun grid-table-etaf-hide-columns ()
  "Hide the columns of the selection.
They keep their data, are saved hidden, and are passed over like hidden
rows.  `grid-table-etaf-show-columns' shows them."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (hidden (grid-table-hidden source))
         (cols (append (cdr hidden)
                       (number-sequence (nth 1 range) (nth 3 range)))))
    (when (cl-every (lambda (col) (memq col cols))
                    (number-sequence 0 (1- (grid-get-column-count source))))
      (user-error "At least one column has to stay shown"))
    (grid-table-etaf--set-hidden state (car hidden) cols)))

(defun grid-table-etaf-show-columns ()
  "Show the hidden columns inside the selection."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (hidden (grid-table-hidden source)))
    (grid-table-etaf--set-hidden
     state
     (car hidden)
     (cl-remove-if (lambda (col) (<= (nth 1 range) col (nth 3 range)))
                   (cdr hidden)))))

;;;----------------------------------------------------------------------
;;; Removing duplicates
;;;----------------------------------------------------------------------

(defun grid-table-etaf--duplicate-rows (source rows cols)
  "Return the ROWS of SOURCE that repeat an earlier one in the COLS.
Values compare as they are shown, ignoring case."
  (let ((seen (make-hash-table :test 'equal))
        duplicates)
    (dolist (row rows)
      (let ((key (mapcar (lambda (col)
                           (downcase (grid-table-etaf--copy-text source row col)))
                         cols)))
        (if (gethash key seen)
            (push row duplicates)
          (puthash key t seen))))
    (nreverse duplicates)))

(defun grid-table-etaf--copy-text (source row col)
  "Return the text the cell at ROW and COL of SOURCE shows."
  (let ((value (grid-get-computed-value-at source row col)))
    (if (grid-table-false-value-p source row col value)
        "FALSE"
      (grid-table-etaf--display-text
       value (grid-table-cell-format source row col)))))

(defun grid-table-etaf-remove-duplicates ()
  "Delete the rows that repeat an earlier row in the selected columns.
With one cell selected, whole rows of the sheet are compared; the header
row is never deleted.  The first of equal rows stays.  Formulas move
with the rows that stay, and references to the deleted rows become
#REF!; the whole is one step to undo."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (whole (and (= (nth 0 range) (nth 2 range))
                     (= (nth 1 range) (nth 3 range))))
         (first (max (if (grid-table-header-row-p source) 1 0)
                     (if whole 0 (nth 0 range))))
         (last (if whole (1- (grid-get-row-count source)) (nth 2 range)))
         (cols (if whole
                   (number-sequence 0 (1- (grid-get-column-count source)))
                 (number-sequence (nth 1 range) (nth 3 range))))
         (duplicates (grid-table-etaf--duplicate-rows
                      source (number-sequence first last) cols))
         (delete-all (gethash :delete-rows source))
         (delete (gethash :delete-row source)))
    (unless (or delete-all delete)
      (user-error "This grid cannot delete rows"))
    (if (null duplicates)
        (message "No duplicate rows")
      (grid-table-etaf--as-one-step
       state
       (lambda ()
         (if delete-all
             (progn (funcall delete-all source duplicates)
                    (grid-table-etaf--track-model state))
           (grid-workbook-with-held-recalculation
             ;; From the bottom, so the rows still to go keep their place.
             (dolist (row (reverse duplicates))
               ;; The row argument counts from the row under the first.
               (funcall delete source (1- row))
               (grid-table-etaf--track-model state)))
           (unless (grid-workbook-of source)
             (grid-table-recalculate source t)))))
      (grid-table-etaf--apply-filters state)
      (grid-table-etaf--fit-widths state)
      ;; The selection may reach past the rows left.
      (grid-table-etaf-goto (min (nth 0 range)
                                 (1- (grid-get-row-count source)))
                            (nth 1 range))
      (grid-table-etaf--bump state)
      (message "Removed %d duplicate row%s" (length duplicates)
               (if (cdr duplicates) "s" "")))))

;;;----------------------------------------------------------------------
;;; Validation
;;;----------------------------------------------------------------------

(defun grid-table-etaf--set-validations (state rules)
  "Give STATE's sheet the validation RULES as one undoable step."
  (let* ((source (grid-table-etaf--source state))
         (old (copy-tree (grid-table-validations source))))
    (unless (grid-table-set-validations source rules)
      (user-error "This grid's data source does not keep validation rules"))
    (grid-table-etaf--record state (list 'validations old (copy-tree rules)))
    (grid-table-etaf--bump state)))

(defun grid-table-etaf-set-validation-list (choices)
  "Let the selected cells take only CHOICES.
CHOICES is a comma-separated list, such as \"Yes, No\", or a formula
giving the values, such as =$F$2:$F$9 or =Sizes.  Editing such a cell
offers the choices; typing anything else is refused.  Pasting and
filling are not checked."
  (interactive
   (list (string-trim (read-string
                       "Allowed values (a, b, c or =range or =name): "))))
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (spec (cond ((string-empty-p choices)
                      (user-error "No values given"))
                     ((string-prefix-p "=" choices)
                      (condition-case nil
                          (progn (grid-parser-parse (substring choices 1))
                                 choices)
                        (error (user-error "Not a formula: %s" choices))))
                     (t (delete "" (mapcar #'string-trim
                                           (split-string choices ","))))))
         (rule (append (cl-subseq range 0 4) (list 'list spec))))
    (grid-table-etaf--set-validations
     state (append (grid-table-validations source) (list rule)))
    (message "Allowed: %s"
             (string-join (grid-table-validation-options source rule) ", "))))

(defun grid-table-etaf-clear-validations ()
  "Let the selected cells take any value again."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection)))
    (grid-table-etaf--set-validations
     state
     (cl-remove-if (lambda (rule)
                     (and (<= (nth 0 rule) (nth 2 range))
                          (<= (nth 0 range) (nth 2 rule))
                          (<= (nth 1 rule) (nth 3 range))
                          (<= (nth 1 range) (nth 3 rule))))
                   (grid-table-validations source)))))

;;;----------------------------------------------------------------------
;;; Defined names
;;;----------------------------------------------------------------------

(defun grid-table-etaf--set-names (state workbook names)
  "Give WORKBOOK the defined NAMES as one undoable step of STATE."
  (let ((old (copy-tree (grid-workbook-names workbook))))
    (grid-workbook-set-names workbook names)
    (grid-table-etaf--record state (list 'names workbook old
                                         (copy-tree names)))
    (grid-table-etaf--bump state)))

(defun grid-table-etaf-define-name (name)
  "Give the selected cell or range the defined NAME.
Formulas then read it by name, as =SUM(Sales) or =Price*Rate; the name
follows its cells when lines are inserted or deleted, and a name already
defined is moved to the selection."
  (interactive
   (list (string-trim (read-string "Name for the selection: "))))
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (workbook (grid-table--workbook))
         (range (grid-table-etaf-selection))
         (cell (lambda (row col)
                 (format "$%s$%d" (grid-calc-col-number-to-letter col) (1+ row))))
         (reference
          (concat "=" (grid-calc-sheet-prefix (grid-workbook-sheet-name source))
                  (funcall cell (nth 0 range) (nth 1 range))
                  (unless (and (= (nth 0 range) (nth 2 range))
                               (= (nth 1 range) (nth 3 range)))
                    (concat ":" (funcall cell (nth 2 range) (nth 3 range))))))
         (names (copy-tree (grid-workbook-names workbook)))
         (entry (assoc-string name names t)))
    (when-let* ((problem (grid-calc-name-problem name)))
      (user-error "%s" problem))
    (if entry
        (setcdr entry reference)
      (setq names (append names (list (cons name reference)))))
    (grid-table-etaf--set-names state workbook names)
    (message "%s is %s" name (substring reference 1))))

(defun grid-table-etaf--read-name (prompt)
  "Read with PROMPT a name defined in the current grid's workbook."
  (let ((names (mapcar #'car (and (grid-workbook-of grid-table--data-source)
                                  (grid-workbook-names
                                   (grid-workbook-of grid-table--data-source))))))
    (unless names
      (user-error "No names are defined; C-c n d names the selection"))
    (completing-read prompt names nil t)))

(defun grid-table-etaf-goto-name (name)
  "Select the cells the defined NAME stands for, showing their sheet."
  (interactive (list (grid-table-etaf--read-name "Go to name: ")))
  (grid-table-etaf-goto-address name))

(defun grid-table-etaf-delete-name (name)
  "Forget the defined NAME; formulas that use it show #NAME?."
  (interactive (list (grid-table-etaf--read-name "Delete name: ")))
  (let ((workbook (grid-table--workbook)))
    (grid-table-etaf--set-names
     (grid-table-etaf--state-or-error) workbook
     (cl-remove-if (lambda (entry) (string-equal-ignore-case (car entry) name))
                   (grid-workbook-names workbook)))))

;;;----------------------------------------------------------------------
;;; Tracing formulas
;;;----------------------------------------------------------------------

(defun grid-table-etaf--traced (source row col)
  "Return the kind of the trace painting the cell at ROW and COL of SOURCE."
  (cl-loop for trace in grid-table-etaf--traces
           when (and (eq (car trace) source)
                     (grid-calc-reference-covers-p trace row col))
           return (nth 5 trace)))

(defun grid-table-etaf--sheet-named (source name)
  "Return the sheet NAME of SOURCE's workbook; SOURCE itself when NAME is nil."
  (if (null name)
      source
    (when-let* ((workbook (grid-workbook-of source)))
      (grid-workbook-sheet workbook name))))

(defun grid-table-etaf--show-traces (traces kind)
  "Paint TRACES and report how many of KIND there are."
  (let* ((source grid-table--data-source)
         (here (cl-count source traces :key #'car)))
    (setq grid-table-etaf--traces traces
          grid-table-etaf--next-trace 0)
    (grid-table-etaf--bump (grid-table-etaf--state-or-error))
    (message (if (null traces)
                 (format "No %ss" kind)
               (format "%d %s%s%s; C-c a n visits them, C-c a x clears"
                       (length traces) kind (if (cdr traces) "s" "")
                       (if (< here (length traces))
                           (format " (%d on other sheets)"
                                   (- (length traces) here))
                         ""))))))

(defun grid-table-etaf-trace-precedents ()
  "Paint the cells and ranges the formula under the cursor reads.
References into other sheets count, and `grid-table-etaf-next-trace'
goes there."
  (interactive)
  (let* ((source grid-table--data-source)
         (model (or (gethash :model source)
                    (user-error "This grid keeps no formulas")))
         (cursor (grid-table-etaf--cursor))
         (traces
          (delq nil
                (mapcar
                 (lambda (reference)
                   (let ((sheet (grid-table-etaf--sheet-named
                                 source (car reference))))
                     (and sheet
                          (append (list sheet) (cdr reference)
                                  '(precedent)))))
                 (let ((grid-calc-names
                        (and (grid-workbook-of source)
                             (grid-workbook--names-resolver
                              (grid-workbook-of source)))))
                   (grid-calc-precedents model (car cursor) (cdr cursor)))))))
    (unless (string-prefix-p
             "=" (or (grid-get-raw-value-at source (car cursor) (cdr cursor))
                     ""))
      (user-error "The cell holds no formula"))
    (grid-table-etaf--show-traces traces "precedent")))

(defun grid-table-etaf-trace-dependents ()
  "Paint the formulas that read the cell under the cursor.
Formulas on every sheet of the workbook are looked at."
  (interactive)
  (let* ((source grid-table--data-source)
         (cursor (grid-table-etaf--cursor))
         (row (car cursor)) (col (cdr cursor))
         traces)
    (dolist (sheet (or (grid-table-sheets) (list source)))
      (when-let* ((model (gethash :model sheet)))
        (let ((grid-calc-names
               (and (grid-workbook-of sheet)
                    (grid-workbook--names-resolver (grid-workbook-of sheet)))))
        (dotimes (r (grid-data-model-rows model))
          (dotimes (c (grid-data-model-cols model))
            (when (cl-some
                   (lambda (reference)
                     (and (eq (grid-table-etaf--sheet-named sheet (car reference))
                              source)
                          (grid-calc-reference-covers-p reference row col)))
                   (grid-calc-precedents model r c))
              (push (list sheet r c r c 'dependent) traces)))))))
    (grid-table-etaf--show-traces (nreverse traces) "dependent")))

(defun grid-table-etaf-next-trace ()
  "Go to the next traced cell or range, showing its sheet."
  (interactive)
  (unless grid-table-etaf--traces
    (user-error "Nothing is traced; C-c a p or C-c a d traces a cell"))
  (let* ((index (mod grid-table-etaf--next-trace
                     (length grid-table-etaf--traces)))
         (trace (nth index grid-table-etaf--traces)))
    (setq grid-table-etaf--next-trace (1+ index))
    (unless (memq (car trace) (or (grid-table-sheets) (list (car trace))))
      (user-error "The traced sheet is gone"))
    (grid-table-switch-to-sheet (car trace))
    (pcase-let* ((source grid-table--data-source)
                 (`(,_ ,top ,left ,bottom ,right) trace)
                 (last-row (1- (grid-get-row-count source)))
                 (last-col (1- (grid-get-column-count source))))
      (grid-table-etaf-goto (min last-row (or bottom last-row))
                            (min last-col (or right last-col)))
      (grid-table-etaf-goto (or top 0) (or left 0)
                            (not (and top left (eql top bottom)
                                      (eql left right)))))))

(defun grid-table-etaf-clear-traces ()
  "Stop painting traced cells."
  (interactive)
  (setq grid-table-etaf--traces nil)
  (grid-table-etaf--bump (grid-table-etaf--state-or-error)))

;;;----------------------------------------------------------------------
;;; Filling
;;;----------------------------------------------------------------------

(defun grid-table-etaf--fill (state changes styles)
  "Store CHANGES and STYLES, lists of (ROW COL VALUE), as one step in STATE.
Return the number of cells whose value changed."
  (grid-table-etaf--as-one-step
   state
   (lambda ()
     (prog1 (grid-table-etaf--set-cells state changes)
       (grid-table-etaf--set-styles state styles)))))

(defun grid-table-etaf--fill-from-edge (down)
  "Copy the first row of the selection down, or its first column right.
DOWN chooses the direction.  Formulas have their relative references
moved along, and the cells take the formats of the ones they copy."
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (top (nth 0 range)) (left (nth 1 range))
         (styled (gethash :set-style-at source))
         changes styles)
    (when (if down (= top (nth 2 range)) (= left (nth 3 range)))
      (user-error "Select the cells to fill, starting with the one to copy"))
    (cl-loop
     for row from top to (nth 2 range)
     when (grid-table-etaf--row-shown-p state row) do
     (cl-loop
      for col from left to (nth 3 range)
      when (grid-table-etaf--col-shown-p state col) do
      (let ((from-row (if down top row))
            (from-col (if down col left)))
        (unless (and (= row from-row) (= col from-col))
          (push (list row col
                      (grid-calc-shift-formula
                       (or (grid-get-raw-value-at source from-row from-col) "")
                       (- row from-row) (- col from-col)))
                changes)
          (when styled
            (push (list row col (grid-get-style-at source from-row from-col))
                  styles))))))
    (let ((count (grid-table-etaf--fill state (nreverse changes)
                                        (nreverse styles))))
      (message "Filled %d cell%s" count (if (= count 1) "" "s")))))

(defun grid-table-etaf-fill-down ()
  "Copy the first row of the selection into the rows below it."
  (interactive)
  (grid-table-etaf--fill-from-edge t))

(defun grid-table-etaf-fill-right ()
  "Copy the first column of the selection into the columns right of it."
  (interactive)
  (grid-table-etaf--fill-from-edge nil))

(defun grid-table-etaf--series (seeds count)
  "Return COUNT values that continue SEEDS, a list of raw cell values.
Numbers go on by the step between the last two seeds, or by one from a
single seed.  Text that ends in a number, such as Week 1, counts on the
same way.  Anything else repeats the seeds in turn.  Each value is
\(RAW . SEED-INDEX), the index naming the seed it takes after."
  (let* ((size (length seeds))
         (last (car (last seeds)))
         (previous (and (> size 1) (nth (- size 2) seeds)))
         (numbered "\\`\\(\\(?:.\\|\n\\)*?\\)\\([0-9]+\\)\\'")
         (number (and (not (string-prefix-p "=" last))
                      (grid-format-number last)))
         (before (and previous (grid-format-number previous))))
    (cond
     ((and number (or (null previous) before))
      (let ((step (if before (- number before) 1)))
        (cl-loop for k from 1 to count collect
                 (cons (grid-format-general
                        (let ((value (+ number (* k step))))
                          ;; Steps such as 0.1 would pile up binary noise.
                          (if (floatp value)
                              (/ (grid-format-round (* value 1e10)) 1e10)
                            value)))
                       (1- size)))))
     ((and (not (string-prefix-p "=" last)) (string-match numbered last))
      (let* ((prefix (match-string 1 last))
             (digits (match-string 2 last))
             (start (string-to-number digits))
             (step (if (and previous (string-match numbered previous)
                            (equal (match-string 1 previous) prefix))
                       (- start (string-to-number (match-string 2 previous)))
                     1)))
        (cl-loop for k from 1 to count collect
                 (cons (concat prefix
                               (format (format "%%0%dd" (length digits))
                                       (max 0 (+ start (* k step)))))
                       (1- size)))))
     (t (cl-loop for k from 0 below count collect
                 (cons (nth (% k size) seeds) (% k size)))))))

(defun grid-table-etaf-fill-series ()
  "Continue the values that start the selection through the rest of it.
A selection several rows tall is filled down each of its columns; one
row is filled to the right.  The cells that start a line, up to its
first empty cell, are what the line continues: 1, 2 goes on 3, 4; a
single 5 goes on 6, 7; Jan 1 dates go on day by day; Week 1 goes on
Week 2; a formula is copied with its references moved along."
  (interactive)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (range (grid-table-etaf-selection))
         (down (/= (nth 0 range) (nth 2 range)))
         (styled (gethash :set-style-at source))
         changes styles)
    (when (and (= (nth 0 range) (nth 2 range)) (= (nth 1 range) (nth 3 range)))
      (user-error "Select the starting values and the cells to fill"))
    (cl-loop
     for line from (if down (nth 1 range) (nth 0 range))
     to (if down (nth 3 range) (nth 2 range)) do
     (let* ((cells (if down
                       (cl-loop for row from (nth 0 range) to (nth 2 range)
                                when (grid-table-etaf--row-shown-p state row)
                                collect (cons row line))
                     (cl-loop for col from (nth 1 range) to (nth 3 range)
                              when (grid-table-etaf--col-shown-p state col)
                              collect (cons line col))))
            (raws (mapcar (lambda (cell)
                            (or (grid-get-raw-value-at source
                                                       (car cell) (cdr cell))
                                ""))
                          cells))
            (run (or (cl-position "" raws :test #'equal) (length raws)))
            ;; A line that is full already continues its first cell.
            (seeded (cond ((= run 0) 0)
                          ((= run (length raws)) 1)
                          (t run)))
            (seeds (cl-subseq raws 0 seeded))
            (targets (nthcdr seeded cells)))
       (when (and seeds targets)
         (let ((first (car (last (cl-subseq cells 0 seeded)))))
           (cl-loop
            for target in targets
            for (raw . index) in (grid-table-etaf--series seeds
                                                          (length targets))
            do
            (let ((seed (nth index cells)))
              (push (list (car target) (cdr target)
                          (if (string-prefix-p "=" raw)
                              (grid-calc-shift-formula
                               raw (- (car target) (car seed))
                               (- (cdr target) (cdr seed)))
                            raw))
                    changes)
              (when styled
                (push (list (car target) (cdr target)
                            (grid-get-style-at source (car first) (cdr first)))
                      styles))))))))
    (unless changes
      (user-error "Nothing to continue: the selection starts with empty cells"))
    (let ((count (grid-table-etaf--fill state (nreverse changes)
                                        (nreverse styles))))
      (message "Filled %d cell%s" count (if (= count 1) "" "s")))))

;;;----------------------------------------------------------------------
;;; Find and replace
;;;----------------------------------------------------------------------

(defvar-local grid-table-etaf--last-search nil
  "The text `grid-table-etaf-find' last looked for in this grid.")

(defun grid-table-etaf--cell-matches-p (source row col text)
  "Return non-nil when the cell at ROW and COL of SOURCE holds TEXT.
What the cell shows and what it stores are both looked at, ignoring
case."
  (let ((case-fold-search t)
        (pattern (regexp-quote text))
        (shown (grid-get-display-value-at source row col))
        (raw (grid-get-raw-value-at source row col)))
    (or (and (stringp shown) (string-match-p pattern shown))
        (and (stringp raw) (string-match-p pattern raw)))))

(defun grid-table-etaf-find (text &optional backward)
  "Move to the next cell that holds TEXT, wrapping around the sheet.
What a cell shows and its formula are both searched, ignoring case.
Empty input repeats the last search.  With a prefix argument, BACKWARD,
the search goes the other way."
  (interactive
   (list (read-string (if grid-table-etaf--last-search
                          (format "Find (default %s): "
                                  grid-table-etaf--last-search)
                        "Find: ")
                      nil nil grid-table-etaf--last-search)
         current-prefix-arg))
  (when (or (null text) (string-empty-p text))
    (user-error "Nothing to find"))
  (setq grid-table-etaf--last-search text)
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (rows (grid-get-row-count source))
         (cols (grid-get-column-count source))
         (total (* rows cols))
         (cursor (grid-table-etaf--cursor))
         (start (+ (* (car cursor) cols) (cdr cursor)))
         found)
    (cl-loop for step from 1 to total until found do
             (let* ((index (mod (+ start (if backward (- step) step)) total))
                    (row (/ index cols))
                    (col (% index cols)))
               (when (and (grid-table-etaf--row-shown-p state row)
                          (grid-table-etaf--cell-matches-p source row col text))
                 (setq found (cons row col)))))
    (if (null found)
        (message "No cell holds %s" text)
      (grid-table-etaf-goto (car found) (cdr found))
      (when (equal found (grid-table-etaf--cursor))
        (message "Found in %s"
                 (grid-table-etaf--address (car found) (cdr found)))))))

(defun grid-table-etaf-replace (from to)
  "Replace FROM with TO in the cells of the selection, or of the sheet.
A selection of several cells limits the change to them; otherwise every
cell is looked at.  What cells store is changed, formulas included,
ignoring the case of FROM.  The whole is one undoable step."
  (interactive
   (let ((from (read-string "Replace: ")))
     (list from (read-string (format "Replace %s with: " from)))))
  (when (string-empty-p from)
    (user-error "Nothing to replace"))
  (let* ((state (grid-table-etaf--state-or-error))
         (source (grid-table-etaf--source state))
         (selection (grid-table-etaf-selection))
         (range (if (and (= (nth 0 selection) (nth 2 selection))
                         (= (nth 1 selection) (nth 3 selection)))
                    (list 0 0 (1- (grid-get-row-count source))
                          (1- (grid-get-column-count source)))
                  selection))
         (case-fold-search t)
         (pattern (regexp-quote from))
         changes)
    (cl-loop
     for row from (nth 0 range) to (nth 2 range)
     when (grid-table-etaf--row-shown-p state row) do
     (cl-loop
      for col from (nth 1 range) to (nth 3 range)
      when (grid-table-etaf--col-shown-p state col) do
      (let ((raw (grid-get-raw-value-at source row col)))
        (when (and (stringp raw) (string-match-p pattern raw))
          (push (list row col
                      (replace-regexp-in-string pattern to raw t t))
                changes)))))
    (let ((count (if changes
                     (grid-table-etaf--set-cells state (nreverse changes))
                   0)))
      (message "Replaced in %d cell%s" count (if (= count 1) "" "s")))))

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

(define-derived-mode grid-table-etaf-mode grid-table-mode "Grid Table"
  "Major mode for a grid, which ETAF shows.
Keys are owned by the mounted sheet; see `grid-table-etaf--keys'."
  :group 'grid-table
  (setq buffer-read-only nil)
  (setq truncate-lines t)
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
  ;; A chart selected on another sheet is not one of this sheet's.
  (setq grid-table-etaf--chart nil)
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
      (if (eq state kept)
          ;; Other sheets may have changed what this one shows.
          (progn
            (grid-table-etaf--track-model state)
            (grid-table-etaf--apply-filters state)
            (grid-table-etaf--fit-widths state))
        ;; Widths saved with the grid apply from the first showing.
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
