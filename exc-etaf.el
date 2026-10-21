;;; exc-etaf.el --- ETAF renderer for exc -*- lexical-binding: t -*-

;;; Commentary:

;; Shows a exc data source through ETAF (https://github.com/emacs-taf).
;; Only the rows and columns that fit the window are mounted, so the cost
;; of moving, scrolling and editing does not grow with the size of the
;; sheet.
;;
;; This file is loaded by `exc--require-view' the first time a grid
;; is shown.  It needs the ecss, ebox and etaf repository roots on
;; `load-path'.  Developed against ecss 9a4270e, ebox 99cdf0d and etaf
;; 2e08bd9.
;;
;; The commands in exc.el work on the cell this view reports:
;; `exc-get-cell-at-point', `exc-refresh', `exc-edit-cell'
;; and the navigation commands end up here.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'etaf)
(require 'exc)
(require 'exc-chart-svg)

(defface exc-etaf-header-face
  '((t :inherit mode-line-inactive))
  "Face whose colors paint the column letter row and the row number column."
  :group 'exc)

(defface exc-etaf-border-face
  '((t :inherit shadow))
  "Face whose foreground paints the grid lines."
  :group 'exc)

(defface exc-etaf-freeze-face
  '((t :inherit link))
  "Face whose foreground paints the line along frozen rows and columns."
  :group 'exc)

(defface exc-etaf-cursor-face
  '((t :inherit highlight))
  "Face whose colors paint the current cell."
  :group 'exc)

(defface exc-etaf-selection-face
  '((t :inherit region))
  "Face whose colors paint the selected range."
  :group 'exc)

(defface exc-etaf-note-face
  '((t :foreground "#d0342c"))
  "Face whose foreground marks the right edge of a cell with a note."
  :group 'exc)

(defface exc-etaf-link-face
  '((((background dark)) :foreground "#7aa6ff")
    (t :foreground "#1a5fb4"))
  "Face whose foreground colors the text of a cell with a link."
  :group 'exc)

(defface exc-etaf-precedent-face
  '((((background dark)) :background "#1f3a5c")
    (t :background "#d6e6fb"))
  "Face whose background paints the cells a traced formula reads."
  :group 'exc)

(defface exc-etaf-dependent-face
  '((((background dark)) :background "#4d3812")
    (t :background "#fbe3c2"))
  "Face whose background paints the formulas that read a traced cell."
  :group 'exc)

(defcustom exc-etaf-min-column-width 5
  "Narrowest content width of a column, in characters."
  :type 'integer
  :group 'exc)

(defcustom exc-etaf-max-column-width 40
  "Widest content width of a column, in characters."
  :type 'integer
  :group 'exc)

(cl-defstruct (exc-etaf--state
               (:constructor exc-etaf--state-create))
  buffer top left cursor anchor tick editing widths session
  ;; Undo history: lists of (cells (ROW COL OLD NEW)...) and (model OLD NEW).
  undo redo model
  ;; Column widths set by dragging, as a hash from column to width, and the
  ;; width a resize drag started from.
  manual resize-start
  ;; Filters, as an alist from column to (OP VALUE), and the vector of
  ;; model rows they leave shown; nil while nothing is filtered.
  filters shown)

(defvar-local exc-etaf--state nil
  "Renderer state of the current ETAF grid buffer.")

(defvar-local exc-etaf--traces nil
  "Traced cells painted in the grid, as (SHEET TOP LEFT BOTTOM RIGHT KIND).
SHEET is the data source of the sheet, KIND `precedent' or `dependent'.
Open ends of whole columns and rows are nil.  The list outlives a switch
to another sheet and is dropped when a cell or the structure changes.")
(put 'exc-etaf--traces 'permanent-local t)

(defvar-local exc-etaf--next-trace 0
  "Index of the trace `exc-etaf-next-trace' visits next.")
(put 'exc-etaf--next-trace 'permanent-local t)

(defvar exc-etaf-mount-options nil
  "Options passed to `etaf-mount' when a grid is shown.
A fixed size is (:viewport-width PIXELS :viewport-height LINES); nil
follows the window.")

(defconst exc-etaf--fallback-viewport '(80 . 30)
  "Viewport in characters and lines used before a size is known.")

;;;----------------------------------------------------------------------
;;; Geometry
;;;----------------------------------------------------------------------

(defun exc-etaf--runtime (&optional buffer)
  "Return the mounted Runtime of BUFFER, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer) (etaf-runtime-for-buffer buffer))))

(defun exc-etaf--char-width ()
  "Return the pixel width of one `ch' unit."
  (max 1 (ebox-string-pixel-width "0")))

(defun exc-etaf--viewport (state)
  "Return STATE's viewport as (PIXELS . LINES)."
  (let* ((runtime (exc-etaf--runtime
                   (exc-etaf--state-buffer state)))
         (viewport (and runtime (etaf-runtime-viewport runtime)))
         (width (car-safe viewport))
         (height (cdr-safe viewport)))
    (cons (if (and (integerp width) (> width 0))
              width
            (* (car exc-etaf--fallback-viewport)
               (exc-etaf--char-width)))
          (if (and (integerp height) (> height 0))
              height
            (cdr exc-etaf--fallback-viewport)))))

(defun exc-etaf--source (state)
  "Return the data source shown by STATE."
  (buffer-local-value 'exc--data-source
                      (exc-etaf--state-buffer state)))

(defvar exc-etaf--chart-cache
  (make-hash-table :test 'eq :weakness 'key)
  "Drawings of chart values, so that a chart is drawn once per value.")

(defun exc-etaf--chart-p (value)
  "Return non-nil when computed VALUE is a chart."
  (and (consp value) (plist-get value :exc-chart)))

(defun exc-etaf--chart-text (chart)
  "Return the text drawing of CHART, or a placeholder when it fails."
  (or (gethash chart exc-etaf--chart-cache)
      (puthash chart
               (condition-case nil
                   (string-trim-right (exc-chart-content chart) "\n+")
                 (error "[Chart]"))
               exc-etaf--chart-cache)))

(defun exc-etaf--display-text (value &optional format shown)
  "Return the text shown for computed VALUE under FORMAT.
With SHOWN, the text the cell paints on screen, where a number without a
format is written as Excel's general format shows it, in eleven
characters at most; without, every digit, for copying and finding."
  (cond ((null value) "")
        ((exc-etaf--chart-p value) (exc-etaf--chart-text value))
        (shown (exc-format-shown value format))
        (t (exc-format-display value format))))

(defconst exc-etaf--max-chart-width 120
  "Widest column a chart may ask for, in characters.")

(defun exc-etaf--value-width (value &optional format)
  "Return the column width computed VALUE asks for under FORMAT, in characters.
A chart is drawn at its own size, so it is not held to the width limit
of ordinary text, which wraps instead."
  (let ((width (exc-etaf--text-width
                (exc-etaf--display-text value format t))))
    (if (exc-etaf--chart-p value)
        (max exc-etaf-min-column-width
             (min exc-etaf--max-chart-width width))
      (exc-etaf--clamp-width width))))

(defun exc-etaf--text-width (text)
  "Return the column width TEXT asks for, in characters."
  (if (exc--extract-image-path text)
      exc-image-column-width
    (apply #'max 0 (mapcar #'string-width (split-string text "\n")))))

(defun exc-etaf--clamp-width (width)
  "Clamp WIDTH to the configured column width range."
  (max exc-etaf-min-column-width
       (min exc-etaf-max-column-width width)))

(defun exc-etaf--compute-widths (source)
  "Return a vector of content widths for every column of SOURCE.
Covered cells and horizontally merged anchors do not widen a column."
  (let* ((rows (exc-get-row-count source))
         (cols (exc-get-column-count source))
         (merges (exc-get-merges source))
         (widths (make-vector cols 0)))
    (dotimes (col cols)
      (aset widths col
            (string-width (exc--col-number-to-letter col))))
    (dotimes (row rows)
      (dotimes (col cols)
        (let ((merge (and merges (exc--merge-at merges row col))))
          (unless (and merge
                       (or (/= (nth 1 merge) (nth 3 merge))
                           (/= row (nth 0 merge))))
            (let ((width (exc-etaf--value-width
                          (exc-get-computed-value-at source row col)
                          (exc-cell-format source row col))))
              (when (> width (aref widths col))
                (aset widths col width)))))))
    (dotimes (col cols)
      (aset widths col (max exc-etaf-min-column-width
                            (aref widths col))))
    widths))

(defun exc-etaf--fit-widths (state)
  "Recompute STATE's column widths, keeping the ones set by dragging."
  (let ((widths (exc-etaf--compute-widths
                 (exc-etaf--source state)))
        (manual (exc-etaf--manual state)))
    (when manual
      (maphash (lambda (col width)
                 (when (< col (length widths))
                   (aset widths col width)))
               manual))
    (setf (exc-etaf--state-widths state) widths)))

(defun exc-etaf--row-header-width (source)
  "Return the content width of SOURCE's row number column.
It holds the number of the last row the window can reach, which past
the sheet's last row is an empty one's."
  (let ((reach (exc-get-row-count source)))
    (when-let* ((state exc-etaf--state)
                ((eq (exc-etaf--source state) source)))
      (setq reach (max reach
                       (1+ (car (etaf-value (exc-etaf--state-cursor state))))
                       (+ (etaf-value (exc-etaf--state-top state))
                          (cdr (exc-etaf--viewport state))))))
    (max 3 (1+ (length (number-to-string reach))))))

(defun exc-etaf--max-rows ()
  "Return the most rows a sheet may have, as Excel's."
  (car exc-calc-sheet-limit))

(defun exc-etaf--max-cols ()
  "Return the most columns a sheet may have, as Excel's."
  (cdr exc-calc-sheet-limit))

(defun exc-etaf--visible-cols (state left)
  "Return the model columns STATE shows when the first one is LEFT."
  (let* ((source (exc-etaf--source state))
         (widths (exc-etaf--state-widths state))
         (cols (min (exc-get-column-count source) (length widths)))
         (char (exc-etaf--char-width))
         (budget (- (car (exc-etaf--viewport state))
                    (* char (+ 2 (exc-etaf--row-header-width source)))
                    1))
         ;; Frozen columns come first whatever the scroll position, and
         ;; one column after them is always shown.
         (frozen (min (cdr (exc-frozen source)) (max 0 (1- cols))))
         (col (max left frozen))
         (scrolled nil)
         result)
    ;; A window too narrow for all of them shows the first ones, leaving
    ;; room for a column that scrolls.
    (let ((room (and (< frozen cols)
                     (1+ (* char (+ 2 (aref widths frozen))))))
          (kept 0))
      (while (and (< kept frozen)
                  (or (not (exc-etaf--col-shown-p state kept))
                      (let ((need (1+ (* char (+ 2 (aref widths kept))))))
                        (when (or (null room) (<= (+ need room) budget))
                          (setq budget (- budget need))
                          t))))
        (when (exc-etaf--col-shown-p state kept)
          (push kept result))
        (setq kept (1+ kept))))
    ;; Past the last column the window goes on over empty columns, as a
    ;; spreadsheet's does.
    (while (and (< col (exc-etaf--max-cols))
                (or (not (exc-etaf--col-shown-p state col))
                    (let ((need (1+ (* char (+ 2 (exc-etaf--col-chars
                                                  state col))))))
                      (when (or (not scrolled) (<= need budget))
                        (setq budget (- budget need))
                        t))))
      (when (exc-etaf--col-shown-p state col)
        (push col result)
        (setq scrolled t))
      (setq col (1+ col)))
    (nreverse result)))

(defun exc-etaf--cell-lines (value width &optional format)
  "Estimate how many lines computed VALUE takes in a cell WIDTH wide.
FORMAT is the format the value is shown with."
  (let ((text (exc-etaf--display-text value format t)))
    (cond ((exc-etaf--chart-p value)
           (length (split-string text "\n")))
          ((exc--extract-image-path text)
           exc-image-target-char-height)
          ((string-empty-p text) 1)
          (t (length (exc--wrap-text text width))))))

(defun exc-etaf--row-lines (state row cols merges)
  "Estimate the height of ROW over COLS in STATE, in lines."
  (let ((source (exc-etaf--source state))
        (widths (exc-etaf--state-widths state))
        (lines 1))
    (dolist (col cols)
      (unless (and merges (exc--merge-at merges row col))
        (setq lines
              (max lines
                   (exc-etaf--cell-lines
                    (exc-get-computed-value-at source row col)
                    (exc-etaf--col-chars state col)
                    (exc-cell-format source row col))))))
    lines))

(defun exc-etaf--title-lines (state)
  "Return how many lines STATE's title takes above the grid."
  (let ((title (buffer-local-value 'exc--title
                                   (exc-etaf--state-buffer state))))
    (if (and title (not (string-empty-p title))) 2 0)))

(defun exc-etaf--row-count (state)
  "Return how many rows STATE shows once its filters are applied."
  (let ((shown (exc-etaf--state-shown state)))
    (if shown
        (length shown)
      (exc-get-row-count (exc-etaf--source state)))))

(defun exc-etaf--row-at (state position)
  "Return the model row at POSITION among the rows STATE shows.
Past the shown rows come the empty rows below the sheet's last row, up
to the most a sheet may have; POSITION is clamped to those."
  (let* ((shown (exc-etaf--state-shown state))
         (count (exc-etaf--row-count state))
         (rows (exc-get-row-count (exc-etaf--source state)))
         (position (max 0 (min (+ count (- (exc-etaf--max-rows) rows) -1)
                               position))))
    (cond ((>= position count) (+ rows (- position count)))
          (shown (aref shown position))
          (t position))))

(defun exc-etaf--row-position (state row)
  "Return the position of model ROW among the rows STATE shows.
A filtered-out row gives the position of the next shown row, or of the
last one when none follows."
  (let ((shown (exc-etaf--state-shown state))
        (rows (exc-get-row-count (exc-etaf--source state))))
    (cond
     ;; A row below the sheet's last comes after every shown row.
     ((>= row rows)
      (+ (exc-etaf--row-count state)
         (- (min row (1- (exc-etaf--max-rows))) rows)))
     ((null shown) (max 0 row))
     (t
      (let ((low 0) (high (length shown)))
        (while (< low high)
          (let ((middle (/ (+ low high) 2)))
            (if (< (aref shown middle) row)
                (setq low (1+ middle))
              (setq high middle))))
        (min low (1- (length shown))))))))

(defun exc-etaf--row-shown-p (state row)
  "Return non-nil when model ROW passes STATE's filters.
An empty row below the sheet's last is shown."
  (or (null (exc-etaf--state-shown state))
      (>= row (exc-get-row-count (exc-etaf--source state)))
      (= (exc-etaf--row-at
          state (exc-etaf--row-position state row))
         row)))

(defun exc-etaf--step-row (state row count)
  "Return the shown row COUNT positions after model ROW in STATE."
  (exc-etaf--row-at
   state (+ (exc-etaf--row-position state row) count)))

(defun exc-etaf--apply-filters (state)
  "Recompute which rows of STATE its filters and hidden rows leave shown.
A header row holds the column titles and is not filtered; hiding it
hides it."
  (let* ((source (exc-etaf--source state))
         (filters (exc-etaf--state-filters state))
         (hidden (car (exc-hidden source)))
         (kept (if (exc-header-row-p source) 1 0)))
    (setf (exc-etaf--state-shown state)
          (and (or filters hidden)
               (vconcat
                (cl-remove-if
                 (lambda (row) (memq row hidden))
                 (append
                      (number-sequence 0 (1- kept))
                      (cl-loop
                       for row from kept below (exc-get-row-count source)
                       when (cl-every
                             (lambda (filter)
                               (exc-conditional-match-p
                                (nth 1 filter) (nth 2 filter)
                                (exc-get-computed-value-at
                                 source row (car filter))))
                             filters)
                       collect row))))))))

(defun exc-etaf--col-shown-p (state col)
  "Return non-nil when model column COL of STATE is not hidden."
  (not (memq col (cdr (exc-hidden (exc-etaf--source state))))))

(defun exc-etaf--shown-col (state col &optional backward)
  "Return the shown column nearest model column COL of STATE.
Look right of COL first, or left of it when BACKWARD, then the other way."
  (let* ((cols (exc-etaf--max-cols))
         (col (max 0 (min (1- cols) col)))
         (step (if backward -1 1)))
    (or (cl-loop for c = col then (+ c step)
                 while (< -1 c cols)
                 when (exc-etaf--col-shown-p state c) return c)
        (cl-loop for c = col then (- c step)
                 while (< -1 c cols)
                 when (exc-etaf--col-shown-p state c) return c)
        col)))

(defun exc-etaf--step-col (state col count)
  "Return the shown column COUNT shown columns right of COL in STATE.
A negative COUNT goes left; the walk goes on past the last column, over
empty ones, and stops at the most columns a sheet may have."
  (let ((cols (exc-etaf--max-cols))
        (step (if (< count 0) -1 1))
        (left (abs count))
        (at col))
    (cl-loop for c = (+ col step) then (+ c step)
             while (and (< -1 c cols) (> left 0))
             when (exc-etaf--col-shown-p state c)
             do (setq at c left (1- left)))
    at))

(defun exc-etaf--visible-rows (state top cols)
  "Return the model rows STATE shows over COLS when the first one is TOP."
  (let* ((source (exc-etaf--source state))
         ;; Past the shown rows the window goes on over empty rows, as a
         ;; spreadsheet's does.
         (count (+ (exc-etaf--row-count state)
                   (- (exc-etaf--max-rows) (exc-get-row-count source))))
         (merges (exc-get-merges source))
         ;; The letter row, the formula bar and the status bar take a line each.
         (budget (- (cdr (exc-etaf--viewport state))
                    3 (exc-etaf--title-lines state)))
         (frozen (car (exc-frozen source)))
         (position 0)
         (scrolled nil)
         result)
    ;; Frozen rows come first whatever the scroll position, and one row
    ;; after them is always shown.
    ;; A window too low for all of them shows the first ones, leaving a
    ;; line for a row that scrolls.
    (while (and (< position count)
                (< (exc-etaf--row-at state position) frozen)
                (let ((need (exc-etaf--row-lines
                             state (exc-etaf--row-at state position)
                             cols merges)))
                  (when (< need budget)
                    (setq budget (- budget need))
                    t)))
      (push (exc-etaf--row-at state position) result)
      (setq position (1+ position)))
    (while (and (< position count)
                (< (exc-etaf--row-at state position) frozen))
      (setq position (1+ position)))
    (setq position (max position (exc-etaf--row-position state top)))
    (while (and (< position count)
                (>= (exc-etaf--row-at state position) frozen)
                (let ((need (exc-etaf--row-lines
                             state (exc-etaf--row-at state position)
                             cols merges)))
                  (when (or (not scrolled) (<= need budget))
                    (setq budget (- budget need))
                    t)))
      (push (exc-etaf--row-at state position) result)
      (setq scrolled t
            position (1+ position)))
    (nreverse result)))

;;;----------------------------------------------------------------------
;;; View
;;;----------------------------------------------------------------------

(defun exc-etaf--face-color (face attribute)
  "Return FACE's color for ATTRIBUTE as a string, or nil.
The faces that paint the cursor, the selection and the headers give
their background a foreground that reads on it when they name none:
left to a terminal's own, which may be light on a dark screen, the
text could vanish on a pale fill."
  (let ((color (face-attribute face attribute nil t)))
    (cond ((stringp color) color)
          ((and (eq attribute :foreground)
                (memq face '(exc-etaf-cursor-face
                             exc-etaf-selection-face
                             exc-etaf-header-face)))
           (let* ((background (face-attribute face :background nil t))
                  (rgb (and (stringp background)
                            (not (string-prefix-p "unspecified" background))
                            (color-defined-p background)
                            (color-name-to-rgb background))))
             (and rgb
                  (if (> (+ (* 0.299 (nth 0 rgb)) (* 0.587 (nth 1 rgb))
                            (* 0.114 (nth 2 rgb)))
                         0.5)
                      "black" "white")))))))

(defun exc-etaf--numeric-p (value)
  "Return non-nil when VALUE should be right-aligned as a number."
  (or (numberp value)
      (and (stringp value)
           (not (exc-format-text-p value))
           (string-match-p
            "\\`[ \t]*[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)%?[ \t]*\\'"
            value))))

(defvar exc-etaf--slot-refs (make-hash-table :test 'equal)
  "Host references of cell slots, keyed by (ROW-SLOT . COL-SLOT).")

(defun exc-etaf--slot-ref (row-slot col-slot)
  "Return the Host reference of the cell at ROW-SLOT and COL-SLOT.
A slot is a position in the window, not in the sheet, so the set of
references stays as small as the window."
  (let ((key (cons row-slot col-slot)))
    (or (gethash key exc-etaf--slot-refs)
        (let ((ref (intern (format "exc-etaf-cell-%d-%d"
                                   row-slot col-slot))))
          (put ref 'exc-etaf-slot key)
          (puthash key ref exc-etaf--slot-refs)))))

(defun exc-etaf--cell-at-gesture (state gesture)
  "Return the model (ROW . COL) under the pointer of GESTURE in STATE.
The pointer is where the gesture last saw it, in pixels; an event's own
start is no use at the release, whose start is the press, taken before
the grid was drawn again."
  (let* ((xy (plist-get gesture :position))
         (window (plist-get gesture :window))
         (position (and (consp xy) (window-live-p window)
                        (posn-point (posn-at-x-y (car xy) (cdr xy) window))))
         (runtime (exc-etaf--runtime
                   (exc-etaf--state-buffer state)))
         (ref (and (integerp position) runtime
                   (etaf-host-ref-at
                    runtime position
                    (lambda (ref)
                      (and (symbolp ref)
                           (get ref 'exc-etaf-slot))))))
         (slot (and ref (get ref 'exc-etaf-slot))))
    (when slot
      (let* ((cols (exc-etaf--visible-cols
                    state (etaf-value (exc-etaf--state-left state))))
             (rows (exc-etaf--visible-rows
                    state (etaf-value (exc-etaf--state-top state))
                    cols))
             (row (nth (car slot) rows))
             (col (nth (cdr slot) cols)))
        (and row col (cons row col))))))

(defun exc-etaf--drag-cell (buffer row col)
  "Return the pointer handler of the cell at ROW and COL in BUFFER.
A press moves the cursor there, or extends the selection with shift
held; dragging extends the selection to the cell under the pointer."
  (lambda (gesture)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((state exc-etaf--state))
          (pcase (exc-etaf--drag-phase gesture)
            ('start
             (exc-etaf-goto
              row col
              (memq 'shift (event-modifiers (plist-get gesture :event)))))
            ((or 'move 'end)
             (let ((cell (exc-etaf--cell-at-gesture state gesture)))
               (when (and cell
                          (not (equal cell (exc-etaf--cursor))))
                 (exc-etaf-goto (car cell) (cdr cell) t)
                 (exc-etaf--unscroll-into-frozen state))))
            ('edge (exc-etaf--drag-past-edge state gesture))))))))

(defun exc-etaf--drag-past-edge (state gesture)
  "Extend STATE's selection one cell past the window edges of GESTURE.
A drag-selection resting against an edge of the window goes on into the
rows or columns beyond it, the window scrolling to show them, as long as
the pointer stays there.  The step is from the window's edge, not from
the selection, which may have been left short of it.  Along an axis
whose edge it does not touch the selection follows the cell under the
pointer."
  (let* ((edges (plist-get gesture :edges))
         (cursor (exc-etaf--cursor))
         (under (exc-etaf--cell-at-gesture state gesture))
         (frozen (exc-frozen (exc-etaf--source state)))
         (left (etaf-value (exc-etaf--state-left state)))
         (top (etaf-value (exc-etaf--state-top state)))
         (cols (exc-etaf--visible-cols state left))
         (rows (exc-etaf--visible-rows state top cols))
         (row (cond ((memq 'top edges)
                     (exc-etaf--step-row
                      state (min (car cursor) (max top (car frozen))) -1))
                    ((memq 'bottom edges)
                     (exc-etaf--step-row
                      state (max (car cursor) (car (last rows))) 1))
                    (under (car under))
                    (t (car cursor))))
         (col (cond ((memq 'left edges)
                     (exc-etaf--step-col
                      state (min (cdr cursor) (max left (cdr frozen))) -1))
                    ((memq 'right edges)
                     (exc-etaf--step-col
                      state (max (cdr cursor) (car (last cols))) 1))
                    (under (cdr under))
                    (t (cdr cursor)))))
    (when (and row col (not (equal (cons row col) cursor)))
      (exc-etaf-goto row col t))
    (exc-etaf--unscroll-into-frozen state)))

(defun exc-etaf--unscroll-into-frozen (state)
  "Scroll STATE's window back to the start when its cursor is in a frozen pane.
A drag-selection reaching into the frozen rows or columns takes in what
lies between them and where it began, so the cells scrolled out of
sight between come back."
  (let* ((cursor (exc-etaf--cursor))
         (frozen (exc-frozen (exc-etaf--source state)))
         (left (exc-etaf--state-left state))
         (top (exc-etaf--state-top state)))
    (when (and (< (cdr cursor) (cdr frozen))
               (> (etaf-value left) (cdr frozen)))
      (etaf-set-value left (cdr frozen)))
    (when (and (< (car cursor) (car frozen))
               (> (etaf-value top) (car frozen)))
      (etaf-set-value top (car frozen)))))

(defun exc-etaf--select (cursor anchor)
  "Select from ANCHOR to CURSOR, both (ROW . COL), without scrolling."
  (let ((state (exc-etaf--state-or-error)))
    (exc-etaf--finish-edit state)
    (etaf-batch
      (etaf-set-value (exc-etaf--state-cursor state) cursor)
      (etaf-set-value (exc-etaf--state-anchor state) anchor))
    (setq exc--nav-row (car cursor)
          exc--nav-col (1+ (cdr cursor)))
    (exc-etaf--sync-point state)))

(defun exc-etaf-select-column (col)
  "Select every cell of model column COL."
  (exc-etaf--select
   (cons 0 col)
   (cons (1- (exc-get-row-count exc--data-source)) col)))

(defun exc-etaf-select-row (row)
  "Select every cell of model ROW."
  (exc-etaf--select
   (cons row 0)
   (cons row (1- (exc-get-column-count exc--data-source)))))

(defun exc-etaf-select-all ()
  "Select every cell of the sheet."
  (interactive)
  (exc-etaf--select
   (cons 0 0)
   (cons (1- (exc-get-row-count exc--data-source))
         (1- (exc-get-column-count exc--data-source)))))

(defun exc-etaf--drag-header (buffer function)
  "Return a pointer handler calling FUNCTION in BUFFER when pressed."
  (lambda (gesture)
    (when (and (buffer-live-p buffer)
               (eq (plist-get gesture :phase) 'start))
      (with-current-buffer buffer
        (funcall function)))))

(defun exc-etaf--manual (state)
  "Return the table of STATE's column widths set by hand, or nil.
The data source keeps them when it can, so that they are saved with the
grid and follow their columns; otherwise they live as long as the view."
  (or (exc-get-column-widths (exc-etaf--source state))
      (exc-etaf--state-manual state)))

(defun exc-etaf-set-column-width (col width)
  "Give model column COL a content width of WIDTH characters.
The width is saved with the grid and moves with its column."
  (let* ((state (exc-etaf--state-or-error))
         (width (max 1 (min 200 width))))
    (if (exc-set-column-width (exc-etaf--source state) col width)
        (exc-etaf--changed state)
      (unless (exc-etaf--state-manual state)
        (setf (exc-etaf--state-manual state) (make-hash-table)))
      (puthash col width (exc-etaf--state-manual state)))
    (aset (exc-etaf--state-widths state) col width)
    (exc-etaf--bump state)))

(defvar exc-etaf--drag-help nil
  "While a drag lasts, (FUNCTION), the `show-help-function' it put aside.")

(defun exc-etaf--drag-phase (gesture)
  "Return the phase of the drag GESTURE, a pointer leaving the window ending it.
Then the drag stops where it last was, rather than undoing it all.  A
drag cancelled by Esc is not followed by Esc read again, which would
begin a Meta key.  No tooltip shows while the drag lasts."
  (let* ((phase (plist-get gesture :phase))
         (event (plist-get gesture :event))
         (phase (if (not (eq phase 'cancel))
                    phase
                  (pcase (plist-get gesture :reason)
                    ('window-left 'end)
                    ('input
                     ;; The drag hands back the key that stopped it to be
                     ;; read again.
                     (when (or (eq event 'escape) (eql event ?\e))
                       (setq unread-command-events
                             (delq event unread-command-events)))
                     phase)
                    (_ phase)))))
    (pcase phase
      ;; A tooltip is a frame of its own: one shown under the pointer,
      ;; such as a column edge's, takes the pointer from the window, and
      ;; the drag ends there or never sees the button let go.
      ('start
       (unless exc-etaf--drag-help
         (setq exc-etaf--drag-help (list show-help-function)))
       (setq show-help-function #'ignore)
       (when (fboundp 'tooltip-hide)
         (tooltip-hide)))
      ((or 'end 'cancel)
       (when exc-etaf--drag-help
         (setq show-help-function (car exc-etaf--drag-help)
               exc-etaf--drag-help nil))))
    phase))

(defun exc-etaf--drag-resize (buffer col)
  "Return the pointer handler resizing column COL of BUFFER.
The column follows the pointer; a cancelled drag restores its width."
  (lambda (gesture)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((state exc-etaf--state)
               (widths (exc-etaf--state-widths state))
               (phase (exc-etaf--drag-phase gesture)))
          (pcase phase
            ;; START is the width at the press and the width set by
            ;; hand then, for the one undo step the drag makes.
            ('start
             (setf (exc-etaf--state-resize-start state)
                   (cons (aref widths col)
                         (when-let* ((manual (exc-etaf--manual state)))
                           (gethash col manual)))))
            ((or 'move 'end)
             (let ((start (exc-etaf--state-resize-start state)))
               (when start
                 (exc-etaf-set-column-width
                  col (+ (car start)
                         (round (car (plist-get gesture :delta))
                                (exc-etaf--char-width))))
                 (when (eq phase 'end)
                   (setf (exc-etaf--state-resize-start state) nil)
                   (let ((new (aref (exc-etaf--state-widths state) col)))
                     (unless (eql new (car start))
                       (exc-etaf--record
                        state (list 'width col (cdr start) new))))))))
            ('cancel
             (let ((start (exc-etaf--state-resize-start state)))
               (when start
                 (setf (exc-etaf--state-resize-start state) nil)
                 (exc-etaf--restore-column-width
                  state col (cdr start)))))))))))

(defun exc-etaf--restore-column-width (state col width)
  "Give column COL of STATE the width WIDTH set by hand; nil fits it again."
  (let ((source (exc-etaf--source state)))
    (if (exc-set-column-width source col width)
        (exc-etaf--changed state)
      (when-let* ((manual (exc-etaf--state-manual state)))
        (if width (puthash col width manual) (remhash col manual))))
    (exc-etaf--fit-widths state)
    (exc-etaf--bump state)))

(defun exc-etaf--direction (buffer)
  "Return the arrow key handler of BUFFER's sheet."
  (lambda (direction)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (pcase direction
          ('up (exc-etaf-move -1 0))
          ('down (exc-etaf-move 1 0))
          ('left (exc-etaf-move 0 -1))
          ('right (exc-etaf-move 0 1)))))
    'handled))

(defconst exc-etaf--keys
  '(("RET" exc-edit-cell "Edit cell")
    ;; A window system's Return key, which Ebox binds apart from RET.
    ("<return>" exc-edit-cell "Edit cell")
    ("<f2>" exc-edit-cell "Edit cell")
    ("C-c C-r" exc-recalculate-all "Recalculate and refresh")
    ("C-c C-q" quit-window "Quit")
    ("C-n" exc-next-line "Next row")
    ("C-p" exc-previous-line "Previous row")
    ("C-f" exc-cell-right "Next column")
    ("C-b" exc-cell-left "Previous column")
    ("C-a" exc-move-beginning-of-line "First column")
    ("C-e" exc-move-end-of-line "Last column")
    ("M-<" exc-etaf-first-cell "First cell")
    ("M->" exc-etaf-last-cell "Last cell")
    ("C-<home>" exc-etaf-first-cell "First cell")
    ("C-<end>" exc-etaf-last-cell "Last cell")
    ("<next>" exc-etaf-page-down "Page down")
    ("<prior>" exc-etaf-page-up "Page up")
    ("S-<down>" exc-etaf-extend-down "Extend selection down")
    ("S-<up>" exc-etaf-extend-up "Extend selection up")
    ("S-<right>" exc-etaf-extend-right "Extend selection right")
    ("S-<left>" exc-etaf-extend-left "Extend selection left")
    ("M-w" exc-etaf-copy "Copy")
    ("C-w" exc-etaf-cut "Cut")
    ("C-y" exc-etaf-paste "Paste")
    ("C-c y v" exc-etaf-paste-values "Paste values")
    ("C-c y f" exc-etaf-paste-formats "Paste formats")
    ("C-c y t" exc-etaf-paste-transposed "Paste transposed")
    ("C-d" exc-etaf-fill-down "Fill down")
    ("C-r" exc-etaf-fill-right "Fill right")
    ("C-c C-s" exc-etaf-fill-series "Fill series")
    ("C-s" exc-etaf-find "Find")
    ("s-f" exc-etaf-find "Find")
    ("M-%" exc-etaf-replace "Replace")
    ("s-c" exc-etaf-copy "Copy")
    ("s-x" exc-etaf-cut "Cut")
    ("s-v" exc-etaf-paste "Paste")
    ("C-/" exc-etaf-undo "Undo")
    ("C-_" exc-etaf-undo "Undo")
    ("C-x u" exc-etaf-undo "Undo")
    ("s-z" exc-etaf-undo "Undo")
    ("C-?" exc-etaf-redo "Redo")
    ("C-M-_" exc-etaf-redo "Redo")
    ("s-Z" exc-etaf-redo "Redo")
    ("C-x h" exc-etaf-select-all "Select all")
    ("C-c g" exc-etaf-goto-address "Go to cell or range")
    ("C-c ;" exc-etaf-edit-note "Add or edit a note")
    ("C-c :" exc-etaf-delete-note "Delete notes")
    ("C-c C-l" exc-etaf-set-link "Add or edit a link")
    ("C-c C-o" exc-etaf-open-link "Open the link")
    ("C-c n d" exc-etaf-define-name "Name the selection (C-u: a formula)")
    ("C-c n l" exc-etaf-goto-name "Go to a name")
    ("C-c n k" exc-etaf-delete-name "Delete a name")
    ("C-c a p" exc-etaf-trace-precedents "Trace precedents")
    ("C-c a d" exc-etaf-trace-dependents "Trace dependents")
    ("C-c a n" exc-etaf-next-trace "Go to next traced cell")
    ("C-c a x" exc-etaf-clear-traces "Clear traces")
    ("C-<next>" exc-sheet-next "Next sheet")
    ("C-<prior>" exc-sheet-previous "Previous sheet")
    ("C-c w n" exc-sheet-new "New sheet")
    ("C-c w r" exc-sheet-rename "Rename sheet")
    ("C-c w d" exc-sheet-delete "Delete sheet")
    ("C-c w s" exc-sheet-select "Go to sheet")
    ("C-c w <" exc-sheet-move-left "Move sheet left")
    ("C-c w >" exc-sheet-move-right "Move sheet right")
    ("C-c w h" exc-toggle-header-row "Header row on or off")
    ("C-c w f" exc-etaf-freeze-panes "Freeze rows above, columns left")
    ("C-c w u" exc-unfreeze-panes "Unfreeze rows and columns")
    ("C-c r i" exc-insert-row-above "Insert row above")
    ("C-c r h" exc-etaf-hide-rows "Hide rows")
    ("C-c r s" exc-etaf-show-rows "Show hidden rows")
    ("C-c c h" exc-etaf-hide-columns "Hide columns")
    ("C-c c s" exc-etaf-show-columns "Show hidden columns")
    ("C-c / f" exc-etaf-filter-column "Filter column")
    ("C-c / v" exc-etaf-filter-by-cell "Filter by this value")
    ("C-c / c" exc-etaf-clear-column-filter "Clear column filter")
    ("C-c / a" exc-etaf-clear-filters "Clear all filters")
    ("C-c / d" exc-etaf-remove-duplicates "Remove duplicate rows")
    ("C-c f b" exc-etaf-toggle-bold "Bold")
    ("C-c f i" exc-etaf-toggle-italic "Italic")
    ("s-b" exc-etaf-toggle-bold "Bold")
    ("s-i" exc-etaf-toggle-italic "Italic")
    ("C-c f c" exc-etaf-set-text-color "Text color")
    ("C-c f g" exc-etaf-set-fill-color "Fill color")
    ("C-c f l" exc-etaf-align-left "Align left")
    ("C-c f e" exc-etaf-align-center "Align center")
    ("C-c f r" exc-etaf-align-right "Align right")
    ("C-c f x" exc-etaf-clear-format "Clear formatting")
    ("C-c f G" exc-etaf-format-general "General format")
    ("C-c f ," exc-etaf-format-number "Number format")
    ("C-c f p" exc-etaf-format-percent "Percent format")
    ("C-c f $" exc-etaf-format-currency "Currency format")
    ("C-c f d" exc-etaf-format-date "Date format")
    ("C-c f t" exc-etaf-format-text "Text format")
    ("C-c f +" exc-etaf-more-decimals "More decimals")
    ("C-c f -" exc-etaf-fewer-decimals "Fewer decimals")
    ("C-c f v" exc-etaf-set-validation-list "Allow only listed values")
    ("C-c f V" exc-etaf-clear-validations "Clear validation")
    ("C-c f n" exc-etaf-add-conditional-format "Conditional format")
    ("C-c f N" exc-etaf-clear-conditional-formats
     "Clear conditional formats")
    ("DEL" exc-etaf-clear-cells "Clear cells")
    ("<deletechar>" exc-etaf-clear-cells "Clear cells")
    ("C-c t" exc-edit-title "Edit title")
    ("C-c r a" exc-add-row "Add row")
    ("C-c r d" exc-delete-row "Delete row")
    ("C-c c a" exc-add-column "Add column")
    ("C-c c d" exc-delete-column "Delete column")
    ("C-c C-w" exc-write-file "Save")
    ("C-c C-f" exc-find-file "Open")
    ("C-c s" exc-sort-column "Sort")
    ("C-c m" exc-etaf-merge "Merge")
    ("C-c u" exc-unmerge-cell "Unmerge")
    ("C-c v" exc-view-chart "View chart")
    ("C-c i" exc-etaf-insert-chart "Insert chart")
    ("C-c k t" exc-etaf-chart-type "Chart type")
    ("C-c k s" exc-etaf-chart-switch "Switch chart rows and columns")
    ("C-c k h" exc-etaf-chart-title "Chart title")
    ("C-c k r" exc-etaf-chart-range "Chart data range")
    ("C-c k m" exc-etaf-chart-move-here "Move chart to cursor")
    ("C-c k z" exc-etaf-chart-resize "Chart size")
    ("C-c k n" exc-etaf-chart-next "Select next chart")
    ("C-c k x" exc-etaf-chart-deselect "Deselect chart")
    ("C-c k d" exc-etaf-chart-delete "Delete chart"))
  "Key bindings of the sheet, as ETAF `:keys' entries.
Printing keys are not commands: see `exc-etaf-typing-map' and
`exc-etaf-type-to-edit'.")

(defvar exc-etaf-traverse-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'exc-next-cell)
    (define-key map [tab] #'exc-next-cell)
    (define-key map [backtab] #'exc-previous-cell)
    (define-key map (kbd "S-TAB") #'exc-previous-cell)
    ;; A quick turn of the wheel arrives as double and triple events.
    (dolist (event '(wheel-down double-wheel-down triple-wheel-down))
      (define-key map (vector event) #'exc-etaf-scroll-down))
    (dolist (event '(wheel-up double-wheel-up triple-wheel-up))
      (define-key map (vector event) #'exc-etaf-scroll-up))
    ;; Undo, from a key of the user's or from the toolbar and Edit menu,
    ;; means the sheet's own undo while a grid is shown.
    (define-key map [remap undo] #'exc-etaf-undo)
    (define-key map [remap undo-only] #'exc-etaf-undo)
    (define-key map [remap undo-redo] #'exc-etaf-redo)
    ;; An indirect buffer shares the drawn text, which one sheet owns.
    (define-key map [remap clone-indirect-buffer] #'exc-etaf-new-view)
    (define-key map [remap clone-indirect-buffer-other-window]
                #'exc-etaf-new-view)
    (define-key map [remap clone-buffer] #'exc-etaf-new-view)
    map)
  "Keymap for the events ETAF and Ebox would otherwise consume.
ETAF `:keys' cannot bind TAB, and Ebox scrolls the window on wheel
events while the sheet scrolls by changing its first row.  This map is
installed ahead of ETAF's own in `emulation-mode-map-alists'.")

(defvar-local exc-etaf--traverse-keys nil
  "Non-nil in buffers where `exc-etaf-traverse-map' is active.")

(defvar exc-etaf-typing-map
  (let ((map (make-sparse-keymap)))
    ;; Ebox binds digits and SPC itself, so every ASCII key is bound here;
    ;; the remap catches other characters, such as input method text.
    (cl-loop for char from ?\s to ?~ do
             (define-key map (vector char) #'exc-etaf-self-insert))
    (define-key map [remap self-insert-command] #'exc-etaf-self-insert)
    map)
  "Keymap catching printing keys while no cell is being edited.
They start an edit of the cursor cell when `exc-etaf-type-to-edit'
is on, and change nothing otherwise.")

(defvar-local exc-etaf--typing-keys nil
  "Non-nil while `exc-etaf-typing-map' is active in the buffer.")

(defvar exc-etaf--emulation-alist
  (list (cons 'exc-etaf--typing-keys exc-etaf-typing-map)
        (cons 'exc-etaf--traverse-keys exc-etaf-traverse-map))
  "Entry of `emulation-mode-map-alists' carrying the sheet's own key maps.")

(defun exc-etaf--cell-node (state source row col span spec)
  "Return the View node of the cell at ROW and COL.
SPAN is nil or (ROW-SPAN . COL-SPAN).  SPEC is a plist carrying the grid
position, the node key and the paint state of the cell."
  (let* ((editing (plist-get spec :editing))
         (value (cond ((stringp editing) editing)
                      (editing (exc-edit-text source row col))
                      (t (exc-get-computed-value-at source row col))))
         (text (cond (editing value)
                     ((exc-false-value-p source row col value) "FALSE")
                     (t (exc-etaf--display-text
                         value (exc-cell-format source row col) t))))
         (image (and (not editing) (exc--extract-image-path text)))
         (face (cond ((plist-get spec :cursor) 'exc-etaf-cursor-face)
                     ((plist-get spec :selected)
                      'exc-etaf-selection-face)
                     ((eq (plist-get spec :traced) 'precedent)
                      'exc-etaf-precedent-face)
                     ((eq (plist-get spec :traced) 'dependent)
                      'exc-etaf-dependent-face)))
         (line (plist-get spec :line))
         (grid-row (plist-get spec :grid-row))
         (grid-col (plist-get spec :grid-col))
         (style (exc-effective-style source row col
                                            (plist-get spec :rules))))
    (etaf-node
     'box
     (list :key (plist-get spec :key)
           :grid-row (if span (list grid-row :span (car span)) grid-row)
           :grid-column (if span (list grid-col :span (cdr span)) grid-col)
           :padding '((lh 0) (ch 1))
           ;; A chart is a drawing: wrapping its lines would break it.
           :wrap-mode (and (not editing) (exc-etaf--chart-p value) 'none)
           :text-align (cond ((plist-get style :align))
                             ((and (not editing)
                                   (exc-etaf--numeric-p value))
                              'right)
                             (t 'left))
           ;; The cursor and the selection paint over a cell's own colors.
           :color (or (and face (exc-etaf--face-color face :foreground))
                      (plist-get style :color)
                      (and (plist-get style :link)
                           (exc-etaf--face-color
                            'exc-etaf-link-face :foreground)))
           :background-color
           (if face
               (exc-etaf--face-color face :background)
             (plist-get style :background))
           ;; A note shows as a colored right edge, Excel's red corner.
           :border-right (or (plist-get spec :line-right)
                             (and (plist-get style :note)
                                  (exc-etaf--face-color
                                   'exc-etaf-note-face :foreground))
                             line)
           :border-bottom (or (plist-get spec :line-bottom) line)
           :ref (plist-get spec :ref)
           :on-drag (exc-etaf--drag-cell
                     (exc-etaf--state-buffer state) row col)
           :drag-edge t)
     (list
      (if image
          (etaf-node 'image
                     (list :src image
                           :columns (aref (exc-etaf--state-widths state)
                                          col)
                           :rows exc-image-target-char-height)
                     nil)
        ;; A separate key while editing gives the input session a fresh
        ;; text Host, so a cancelled edit leaves nothing behind.
        (etaf-node 'text
                   (append (list :key (if editing 'editing 'value)
                                 :font-weight (and (plist-get style :bold) 'bold)
                                 :font-style
                                 (and (plist-get style :italic) 'italic))
                           (and (plist-get spec :cursor)
                                (list :ref 'exc-etaf-cursor)))
                   (list text)))))))

(defun exc-etaf--header-node (key grid-row grid-col label align
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
                   'exc-etaf-selection-face
                 'exc-etaf-header-face))
         (color (exc-etaf--face-color face :foreground))
         (background (exc-etaf--face-color face :background)))
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

(defun exc-etaf--title-node (state)
  "Return the title View node of STATE, or nil without a title."
  (let* ((buffer (exc-etaf--state-buffer state))
         (title (buffer-local-value 'exc--title buffer)))
    (when (and title (not (string-empty-p title)))
      (etaf-node
       'box
       (list :key 'title
             :text-align (buffer-local-value 'exc-title-align buffer)
             :padding '((lh 0) (ch 1))
             :border-bottom
             (and (eq (buffer-local-value 'exc-title-decoration buffer)
                      'rule)
                  '(px 1))
             :margin-bottom '(lh 1))
       (list (etaf-node 'text '(:font-weight bold) (list title)))))))

(defun exc-etaf--address (row col)
  "Return the A1-style address of model ROW and COL."
  (format "%s%d" (exc--col-number-to-letter col) (1+ row)))

(defun exc-etaf--number (value)
  "Return computed VALUE as a number, or nil when it is not numeric."
  (cond ((numberp value) value)
        ((and (stringp value)
              (string-match-p
               "\\`[ \t]*[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)[ \t]*\\'"
               value))
         (string-to-number value))))

(defun exc-etaf--format-number (number)
  "Return NUMBER as text without a needless fraction."
  (if (or (integerp number) (= number (ftruncate number)))
      (format "%d" (truncate number))
    (string-trim-right (string-trim-right (format "%.4f" number) "0+") "\\.")))

(defun exc-etaf--formula-bar-node (source row col range line)
  "Return the formula bar View node.
It names the cell at ROW and COL of SOURCE, or RANGE when several cells
are selected, and shows the raw value of that cell.  LINE is the grid
line border."
  (let ((raw (exc-edit-text source row col))
        (face 'exc-etaf-header-face))
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
             :color (exc-etaf--face-color face :foreground)
             :background-color (exc-etaf--face-color face :background)
             :border-right line)
       (list (etaf-node
              'text nil
              (list (if range
                        (format "%s:%s"
                                (exc-etaf--address (nth 0 range)
                                                          (nth 1 range))
                                (exc-etaf--address (nth 2 range)
                                                          (nth 3 range)))
                      (exc-etaf--address row col))))))
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

(defun exc-etaf--data-row-counts (state)
  "Return (SHOWN . ALL), the rows of STATE's sheet below its header row.
As Excel's \"N of M records found\", neither count takes in the header
row, and with no header row the first row counts like any other."
  (let* ((source (exc-etaf--source state))
         (header (if (exc-header-row-p source) 1 0))
         (shown (exc-etaf--state-shown state)))
    (cons (- (exc-etaf--row-count state)
             (if (and (= header 1) (or (null shown) (eql (aref shown 0) 0)))
                 1 0))
          (- (exc-get-row-count source) header))))

(defun exc-etaf--sheet-empty-p (source)
  "Return non-nil when no cell of SOURCE holds anything.
Looks at the first few thousand cells only, so that a large sheet does
not make the status bar slow; one that big is taken as not empty."
  (let ((rows (exc-get-row-count source))
        (cols (exc-get-column-count source)))
    (and (<= (* rows cols) 4000)
         (cl-loop for row below rows
                  always (cl-loop for col below cols
                                  always (equal (or (exc-get-raw-value-at
                                                     source row col)
                                                    "")
                                                ""))))))

(defun exc-etaf--size-text (state source)
  "Return how many rows and columns SOURCE has, as STATE shows them."
  (let ((counts (exc-etaf--data-row-counts state)))
    (cond
     ((exc-etaf--state-filters state)
      (format "%d of %d rows shown (filtered) × %d columns"
              (car counts) (cdr counts) (exc-get-column-count source)))
     ((exc-etaf--sheet-empty-p source) "Empty sheet")
     (t (format "%d rows × %d columns"
                (cdr counts) (exc-get-column-count source))))))

(defun exc-etaf--status-text (state source range)
  "Return the status bar text for SOURCE as STATE shows it.
RANGE is the selected (TOP LEFT BOTTOM RIGHT), or nil for a single cell.
Rows hidden by a filter are left out of the figures."
  (if-let* ((chart (exc-etaf--selected-chart source)))
      (format "Chart: %s (%s, %s)   C-c k t type, s switch rows/columns, h title, d delete"
              (or (plist-get chart :title) "untitled")
              (cdr (assq (plist-get chart :type) exc-chart-svg-types))
              (plist-get chart :range))
  (if (null range)
      (let* ((cursor (etaf-value (exc-etaf--state-cursor state)))
             (style (exc-get-style-at source (car cursor) (cdr cursor)))
             (note (plist-get style :note))
             (link (plist-get style :link)))
        (cond
         (note (concat "Note: " (replace-regexp-in-string "\n" " / " note)
                       (if link (format "   Link: %s" link) "")))
         (link (format "Link: %s   (C-c C-o opens it)" link))
         (t (exc-etaf--size-text state source))))
    (let ((count 0) (numbers 0) (sum 0))
      (cl-loop
       for row from (nth 0 range) to (nth 2 range)
       when (exc-etaf--row-shown-p state row) do
       (cl-loop
        for col from (nth 1 range) to (nth 3 range)
        when (exc-etaf--col-shown-p state col) do
        (let* ((value (exc-get-computed-value-at source row col))
               (number (exc-etaf--number value)))
          (unless (member value '(nil ""))
            (setq count (1+ count)))
          (when number
            (setq numbers (1+ numbers)
                  sum (+ sum number))))))
      (if (zerop numbers)
          (format "Count: %d" count)
        (format "Count: %d   Sum: %s   Average: %s"
                count
                (exc-etaf--format-number sum)
                (exc-etaf--format-number (/ (float sum) numbers))))))))

(defun exc-etaf--tab-node (buffer sheet current line)
  "Return the tab of SHEET for the sheet bar of BUFFER.
CURRENT is non-nil for the sheet being shown; LINE is the grid line
border.  Pressing the tab shows its sheet."
  (let ((face (if current
                  'exc-etaf-selection-face
                'exc-etaf-header-face)))
    (etaf-node
     'box
     (list :key (exc-workbook-sheet-name sheet)
           :padding '((lh 0) (ch 1))
           :wrap-mode 'none
           :font-weight (and current 'bold)
           :color (exc-etaf--face-color face :foreground)
           :background-color (exc-etaf--face-color face :background)
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
                                  (exc-switch-to-sheet sheet))))))))
     (list (etaf-node 'text nil (list (exc-workbook-sheet-name sheet)))))))

(defun exc-etaf--status-node (state source range &optional line)
  "Return the status bar View node of STATE for SOURCE and the selected RANGE.
When SOURCE is one of several sheets their tabs come first, separated by
LINE, the grid line border."
  (let* ((sheets (exc-sheets))
         (status
          (etaf-node
           'box
           (list :key 'status
                 :flex-grow 1
                 :height '(lh 1)
                 :padding '((lh 0) (ch 1))
                 :wrap-mode 'none
                 :overflow 'hidden
                 :color (exc-etaf--face-color 'shadow :foreground))
           (list (etaf-node
                  'text nil
                  (list (exc-etaf--status-text state source range)))))))
    (if (null (cdr sheets))
        status
      (etaf-node
       'flex
       (list :key 'sheet-bar :height '(lh 1) :overflow 'hidden)
       (append
        (mapcar (lambda (sheet)
                  (exc-etaf--tab-node
                   (exc-etaf--state-buffer state) sheet
                   (eq sheet source) line))
                sheets)
        (list status))))))

(defvar-local exc-etaf--drawn nil
  "(TOP COLS ROWS) of the last drawing: its first row, columns and rows.")

(defun exc-etaf--render (state)
  "Return the View of STATE's visible window."
  (etaf-value (exc-etaf--state-tick state))
  (let ((runtime (exc-etaf--runtime
                  (exc-etaf--state-buffer state))))
    (when runtime (etaf-runtime-viewport runtime)))
  (let* ((source (exc-etaf--source state))
         (buffer (exc-etaf--state-buffer state))
         (widths (exc-etaf--state-widths state))
         (top (etaf-value (exc-etaf--state-top state)))
         (cursor (etaf-value (exc-etaf--state-cursor state)))
         (anchor (etaf-value (exc-etaf--state-anchor state)))
         (editing (etaf-value (exc-etaf--state-editing state)))
         (cols (exc-etaf--visible-cols
                state (etaf-value (exc-etaf--state-left state))))
         (rows (exc-etaf--visible-rows state top cols))
         (frozen (exc-frozen source))
         (merges (exc-get-merges source))
         (_ (with-current-buffer buffer
              (setq exc-etaf--drawn (list top cols rows))))
         (rules (exc-get-conditional-formats source))
         (cursor-merge (exc--merge-at merges (car cursor) (cdr cursor)))
         (cursor-row (if cursor-merge (nth 0 cursor-merge) (car cursor)))
         (cursor-col (if cursor-merge (nth 1 cursor-merge) (cdr cursor)))
         (sel-top (min (car cursor) (car anchor)))
         (sel-bottom (max (car cursor) (car anchor)))
         (sel-left (min (cdr cursor) (cdr anchor)))
         (sel-right (max (cdr cursor) (cdr anchor)))
         (ranged (not (equal cursor anchor)))
         (border (exc-etaf--face-color 'exc-etaf-border-face
                                              :foreground))
         (line (if border (list '(px 1) 'solid border) '(px 1)))
         ;; The line under the last frozen row and right of the last
         ;; frozen column is drawn in the text color.
         (edge (if-let* ((color (exc-etaf--face-color
                                 'exc-etaf-freeze-face :foreground)))
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
    (push (exc-etaf--header-node
           'corner 1 1 "" 'left nil line
           (exc-etaf--drag-header buffer #'exc-etaf-select-all))
          children)
    (cl-loop
     for col in cols for slot from 0 do
     (push (exc-etaf--header-node
            (format "c%d" slot) 1 (+ 2 slot)
            (concat (exc--col-number-to-letter col)
                    (and (assq col (exc-etaf--state-filters state))
                         " ▼"))
            'center
            (and (<= sel-left col) (<= col sel-right)) line
            (exc-etaf--drag-header
             buffer (lambda () (exc-etaf-select-column col)))
            ;; An empty column past the edge has no width of its own.
            (and (< col (length widths))
                 (exc-etaf--drag-resize buffer col))
            (and (eql col edge-col) edge))
           children))
    (cl-loop
     for row in rows for row-slot from 0 do
     (push (exc-etaf--header-node
            (format "r%d" row-slot) (+ 2 row-slot) 1
            (number-to-string (1+ row)) 'right
            (and (<= sel-top row) (<= row sel-bottom)) line
            (exc-etaf--drag-header
             buffer (lambda () (exc-etaf-select-row row)))
            nil nil (and (eql row edge-row) edge))
           children)
     (cl-loop
      for col in cols for col-slot from 0 do
      (let* ((merge (and merges (exc--merge-at merges row col)))
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
             (exc-etaf--cell-node
              state source cell-row cell-col
              (and merge
                   (cons (length merge-rows) (length merge-cols)))
              (list :key (+ (* 10000 row-slot) col-slot)
                    :rules rules
                    :ref (exc-etaf--slot-ref row-slot col-slot)
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
                    :traced (and exc-etaf--traces
                                 (exc-etaf--traced
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
     (list :ref 'exc-etaf-root
           :tab-index 0
           :on-direction (exc-etaf--direction buffer)
           :keys exc-etaf--keys)
     (delq nil
           (append
            (list
             (exc-etaf--title-node state)
             (exc-etaf--formula-bar-node
              source cursor-row cursor-col
              (and ranged (list sel-top sel-left sel-bottom sel-right))
              line)
             (etaf-node
              'grid
              (list :key 'sheet
                    :grid-template-columns
                    (cons (list 'ch (+ 2 (exc-etaf--row-header-width
                                          source)))
                          (mapcar (lambda (col)
                                    `(calc (+ (ch ,(+ 2 (exc-etaf--col-chars
                                                          state col)))
                                              (px 1))))
                                  cols)))
              (nreverse children)))
            ;; Floating charts sit over the grid, and over the empty lines
            ;; left below it for a chart that runs past the last row.
            (let ((frame (and (exc-charts source)
                              (exc-etaf--chart-frame
                               state source rows cols))))
              (append
               (and frame (> (plist-get frame :extra) 0)
                    (list (etaf-node 'box
                                     (list :key 'chart-room
                                           :height (list 'lh (plist-get frame :extra)))
                                     nil)))
               (and frame
                    (exc-etaf--chart-nodes state source frame cols))))
            (list
             (exc-etaf--status-node
              state source
              (and ranged (list sel-top sel-left sel-bottom sel-right))
              line)))))))

;;;----------------------------------------------------------------------
;;; Floating charts
;;;----------------------------------------------------------------------

(defvar-local exc-etaf--chart nil
  "Index of the selected floating chart of the sheet shown, or nil.")

(defcustom exc-etaf-chart-size '(56 . 16)
  "Size of a new floating chart, as (CHARACTERS . LINES)."
  :type '(cons natnum natnum)
  :group 'exc)

(defvar exc-etaf--chart-images (make-hash-table :test #'equal)
  "Images of floating charts, keyed by everything their drawing depends on.")

(defun exc-etaf--selected-chart (source)
  "Return the selected floating chart of SOURCE, or nil."
  (and exc-etaf--chart
       (nth exc-etaf--chart (exc-charts source))))

(defun exc-etaf--chart-colors ()
  "Return (FOREGROUND . BACKGROUND) for drawing charts, or nils."
  (cl-flet ((color (attribute)
              (let ((color (exc-etaf--face-color 'default attribute)))
                (and color (not (string-prefix-p "unspecified" color))
                     (color-defined-p color)
                     (apply #'format "#%02x%02x%02x"
                            (mapcar (lambda (c) (/ c 256))
                                    (color-values color)))))))
    (cons (color :foreground) (color :background))))

(defun exc-etaf--line-pixels ()
  "Return the pixel height of one line."
  (max 1 (or (and (display-graphic-p) (default-line-height)) 1)))

(defvar-local exc-etaf--chart-data-cache nil
  "(TICK . TABLE): chart data read while the view's tick was TICK.
Cells change only with a new tick, so cursor moves and scrolling reuse
what was read.")

(defun exc-etaf--chart-data (source chart &optional tick)
  "Return the data CHART reads from SOURCE, as `exc-chart-svg-data' does.
TICK, the view's change counter, lets the data be read once per change."
  (if (null tick)
      (exc-etaf--read-chart-data source chart)
    (unless (eql (car exc-etaf--chart-data-cache) tick)
      (setq exc-etaf--chart-data-cache
            (cons tick (make-hash-table :test #'equal))))
    (let ((key (list source (plist-get chart :range) (plist-get chart :by)
                     (plist-get chart :type) (plist-get chart :labels)
                     (plist-get chart :names)))
          (table (cdr exc-etaf--chart-data-cache)))
      (or (gethash key table)
          (puthash key (exc-etaf--read-chart-data source chart) table)))))

(defun exc-etaf--read-chart-data (source chart)
  "Read the data CHART draws from SOURCE."
  (exc-chart-data source chart))

(defun exc-etaf--chart-image (chart data columns lines view)
  "Return the image of CHART with DATA, COLUMNS by LINES, showing VIEW.
VIEW is (LEFT TOP SHOWN-COLUMNS SHOWN-LINES) in characters and lines."
  (let* ((cw 8) (lh 18)
         (colors (exc-etaf--chart-colors))
         (key (list chart data columns lines view colors)))
    (or (gethash key exc-etaf--chart-images)
        (progn
          (when (> (hash-table-count exc-etaf--chart-images) 200)
            (clrhash exc-etaf--chart-images))
          (puthash key
                   (list 'image :type 'svg
                         :data (exc-chart-svg-render
                                chart data (* cw columns) (* lh lines)
                                (car colors) (cdr colors)
                                (list (* cw (nth 0 view)) (* lh (nth 1 view))
                                      (* cw (nth 2 view)) (* lh (nth 3 view)))))
                   exc-etaf--chart-images)))))

(defconst exc-etaf--blank-col-chars 8
  "Width in characters of a column past the sheet's right edge.
Charts may stand there, as on a spreadsheet's empty cells.")

(defun exc-etaf--col-chars (state col)
  "Return the width of column COL of STATE, in characters.
A column past the sheet's right edge has the width of an empty one."
  (let ((widths (exc-etaf--state-widths state)))
    (if (< col (length widths))
        (aref widths col)
      exc-etaf--blank-col-chars)))

(defun exc-etaf--col-pixels (state col)
  "Return the pixel width column COL takes in STATE's grid.
A column past the sheet's right edge has the width of an empty one."
  (let ((widths (exc-etaf--state-widths state)))
    (1+ (* (exc-etaf--char-width)
           (+ 2 (if (< col (length widths))
                    (aref widths col)
                  exc-etaf--blank-col-chars))))))

(defun exc-etaf--span (state from to axis cols merges)
  "Return how far shown line FROM is from shown line TO along AXIS of STATE.
AXIS `row' counts lines over row positions, `col' pixels over columns.
The result is negative when TO comes before FROM.  COLS and MERGES are
what row heights are measured over."
  (let ((sign (if (< to from) -1 1))
        (low (min from to)) (high (max from to))
        (total 0))
    (if (eq axis 'row)
        (cl-loop for position from low below high do
                 (cl-incf total (exc-etaf--row-lines
                                 state (exc-etaf--row-at state position)
                                 cols merges)))
      (let ((edge (length (exc-etaf--state-widths state))))
        (cl-loop for col from low below (min high edge)
                 when (exc-etaf--col-shown-p state col) do
                 (cl-incf total (exc-etaf--col-pixels state col)))
        ;; Past the last column every column is an empty one of the
        ;; same width, however far a file's chart stands.
        (when (> high edge)
          (cl-incf total (* (- high (max low edge))
                            (exc-etaf--col-pixels state edge))))))
    (* sign total)))

(defun exc-etaf--chart-frame (state source rows cols)
  "Return the window's layout that charts are placed in, as a plist.
ROWS and COLS are the model rows and columns STATE shows of SOURCE."
  (let* ((merges (exc-get-merges source))
         (frozen (exc-frozen source))
         (char (exc-etaf--char-width))
         ;; Lines and pixels where the cells of the grid begin
         (y0 (+ 2 (exc-etaf--title-lines state)))
         (x0 (* char (+ 2 (exc-etaf--row-header-width source))))
         (frozen-rows (cl-remove-if-not (lambda (r) (< r (car frozen))) rows))
         (scrolled-rows (cl-remove-if (lambda (r) (< r (car frozen))) rows))
         (frozen-cols (cl-remove-if-not (lambda (c) (< c (cdr frozen))) cols))
         (scrolled-cols (cl-remove-if (lambda (c) (< c (cdr frozen))) cols))
         (sy0 (+ y0 (cl-loop for r in frozen-rows
                             sum (exc-etaf--row-lines state r cols merges))))
         (sy1 (+ sy0 (cl-loop for r in scrolled-rows
                              sum (exc-etaf--row-lines state r cols merges))))
         (sx0 (+ x0 (cl-loop for c in frozen-cols
                             sum (exc-etaf--col-pixels state c))))
         ;; A chart may run past the last column, to the window's edge.
         (sx1 (max (+ sx0 (cl-loop for c in scrolled-cols
                                   sum (exc-etaf--col-pixels state c)))
                   (car (exc-etaf--viewport state)))))
    (let* ((frame (list :merges merges :frozen frozen :char char :y0 y0 :x0 x0
                        :frozen-rows frozen-rows :scrolled-rows scrolled-rows
                        :frozen-cols frozen-cols :scrolled-cols scrolled-cols
                        :sy0 sy0 :sy1 most-positive-fixnum :sx0 sx0 :sx1 sx1))
           ;; A chart may run past the last row into the window's empty
           ;; lines below the grid, as onto a sheet's empty cells; those
           ;; lines are then left free and the status bar moves below them.
           ;; One line stays for the status bar.
           (free (- (cdr (exc-etaf--viewport state)) sy1 1))
           (reach (cl-loop for chart in (exc-charts source)
                           for place = (exc-etaf--chart-place
                                        state frame chart cols)
                           when place
                           maximize (+ (nth 1 place) (nth 3 place))))
           (extra (max 0 (min free (- (or reach 0) sy1)))))
      (plist-put frame :sy1 (+ sy1 extra))
      (plist-put frame :extra extra))))

(defun exc-etaf--chart-place (state frame chart cols)
  "Return where CHART shows in STATE's window, or nil when it does not.
FRAME is what `exc-etaf--chart-frame' returns and COLS the model
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
         ;; An anchor below the last row, as a file may hold, sits on that
         ;; row; one right of the last column stands on the empty space
         ;; there, as on a spreadsheet's empty cells.
         (row (max 0 (min (car anchor) (1- (exc-etaf--row-count state)))))
         (col (max 0 (cdr anchor)))
         ;; A chart anchored in the frozen rows or columns stays put along
         ;; that axis; any other scrolls with the cells.
         (y (if (< row (car frozen))
                (+ y0 (cl-loop for r in frozen-rows while (< r row)
                               sum (exc-etaf--row-lines state r cols merges)))
              (and scrolled-rows
                   (+ sy0 (exc-etaf--span
                           state (exc-etaf--row-position state (car scrolled-rows))
                           (exc-etaf--row-position state row)
                           'row cols merges)))))
         (x (if (< col (cdr frozen))
                (+ x0 (cl-loop for c in frozen-cols while (< c col)
                               sum (exc-etaf--col-pixels state c)))
              (and scrolled-cols
                   (+ sx0 (exc-etaf--span
                           state (car scrolled-cols) col 'col nil nil)))))
         ;; Within its cell the chart stands to the pixel.
         (x (and x (+ x (round (* char (or (car (plist-get chart :offset)) 0))))))
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

(defvar-local exc-etaf--chart-boxes nil
  "Where the last redraw put each chart: ((INDEX . GEOMETRY) ...).
GEOMETRY holds the :left, :top, :width and :height of the chart's box.")

(defvar-local exc-etaf--chart-corner '(0 . 0)
  "Where the last redraw began the cells, as (PIXELS . LINES).
A dragged chart stops there, as it does when let go, and does not cover
the row numbers or the column letters.")

(defun exc-etaf--chart-nodes (state source frame cols)
  "Return the View nodes of SOURCE's floating charts for STATE's window.
FRAME is what `exc-etaf--chart-frame' returns, and COLS the model
columns shown."
  (let* ((buffer (exc-etaf--state-buffer state))
         (charts (exc-charts source))
         (tick (etaf-value (exc-etaf--state-tick state)))
         (boxes nil)
         (nodes
          (cl-loop
           for chart in charts for index from 0
           for place = (exc-etaf--chart-place state frame chart cols)
           when place collect
           (pcase-let* ((`(,left ,top ,columns ,lines ,cut-cols ,cut-lines) place)
                        (size (plist-get chart :size))
                        (data (exc-etaf--chart-data source chart tick))
                        (drawn (if (eql index exc-etaf--chart)
                                   (plist-put (copy-sequence chart) :selected t)
                                 chart))
                        (box (list :left (list 'px left) :top (list 'lh top)
                                   :width (list 'ch columns) :height (list 'lh lines))))
             (push (cons index box) boxes)
             (etaf-node
              'box
              (append
               (list :key (format "chart%d" index)
                     :ref (intern (format "exc-etaf-chart-%d" index))
                     :position 'absolute)
               box
               (list :z-index 5
                     :on-press (lambda ()
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (exc-etaf--select-chart state index))))
                     :on-drag (exc-etaf--drag-chart buffer index)
                     :drag-edge t))
              (list
               (if (display-graphic-p)
                   (etaf-node 'image
                              (list :src (exc-etaf--chart-image
                                          drawn data (car size) (cdr size)
                                          (list cut-cols cut-lines columns lines))
                                    :columns columns :rows lines)
                              nil)
                 (etaf-node 'text (list :key 'chart-text)
                            (list (exc-chart-svg-text drawn data columns lines))))))))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq exc-etaf--chart-boxes boxes
              exc-etaf--chart-corner
              (cons (plist-get frame :x0) (plist-get frame :y0)))))
    nodes))

(defun exc-etaf--select-chart (state index)
  "Select floating chart INDEX of STATE's sheet; nil selects none."
  (setq exc-etaf--chart index)
  (exc-etaf--bump state))

(defun exc-etaf--set-charts (state charts)
  "Give STATE's sheet the floating CHARTS, as one undo step."
  (let* ((source (exc-etaf--source state))
         (old (copy-tree (exc-charts source))))
    (unless (exc-set-charts source charts)
      (user-error "This sheet cannot hold charts"))
    (exc-etaf--record state (list 'charts old (copy-tree charts)))
    (exc-etaf--bump state)))

(defvar-local exc-etaf--chart-drag nil
  "The charts and the dragged chart's box as a chart drag started, or nil.
It is (CHARTS . GEOMETRY), GEOMETRY as in `exc-etaf--chart-boxes'.
It lives in the buffer, not in the handler: the grid redraws as the drag
starts, and each redraw makes a new handler.")

(defvar-local exc-etaf--chart-drag-scroll '(0 . 0)
  "How far the sheet has scrolled under a chart drag, as (PIXELS . PIXELS).
The chart goes as far again as the pointer has moved.")

(defun exc-etaf--scroll-past-edges (state edges)
  "Scroll STATE's window one row or column toward each of EDGES.
EDGES name window edges, of `left', `right', `top' and `bottom'.  Frozen
rows and columns stay.  Return how far the cells moved under the window,
as (PIXELS . PIXELS), right and down positive."
  (let* ((frozen (exc-frozen (exc-etaf--source state)))
         (top-ref (exc-etaf--state-top state))
         (left-ref (exc-etaf--state-left state))
         (top (max (car frozen) (etaf-value top-ref)))
         (left (max (cdr frozen) (etaf-value left-ref)))
         (lines (exc-etaf--line-pixels))
         (dx 0) (dy 0) next)
    (cond ((and (memq 'bottom edges)
                (not (eql top (setq next (exc-etaf--step-row
                                          state top 1)))))
           (setq dy (* lines (exc-etaf--row-lines state top nil nil)))
           (etaf-set-value top-ref next))
          ((and (memq 'top edges)
                (not (eql top (setq next (exc-etaf--step-row
                                          state top -1))))
                (>= next (car frozen)))
           (setq dy (- (* lines (exc-etaf--row-lines state next nil nil))))
           (etaf-set-value top-ref next)))
    (cond ((and (memq 'right edges)
                (not (eql left (setq next (exc-etaf--step-col
                                           state left 1)))))
           (setq dx (exc-etaf--col-pixels state left))
           (etaf-set-value left-ref next))
          ((and (memq 'left edges)
                (not (eql left (setq next (exc-etaf--step-col
                                           state left -1))))
                (>= next (cdr frozen)))
           (setq dx (- (exc-etaf--col-pixels state next)))
           (etaf-set-value left-ref next)))
    (cons dx dy)))

(defun exc-etaf--move-chart-box (index geometry)
  "Put the box of chart INDEX where GEOMETRY says, without a redraw."
  (ignore-errors
    (apply #'etaf-runtime-update-host-geometry
           (exc-etaf--runtime)
           (intern (format "exc-etaf-chart-%d" index))
           geometry)))

(defun exc-etaf--chart-drag-delta (gesture)
  "Return how far a chart drag GESTURE moves its chart over the cells.
That is the pointer's move and the sheet's scroll under it."
  (let ((delta (or (plist-get gesture :delta) '(0 . 0)))
        (scroll exc-etaf--chart-drag-scroll))
    (cons (+ (car delta) (car scroll)) (+ (cdr delta) (cdr scroll)))))

(defun exc-etaf--end-chart-drag (state index start)
  "Finish dragging chart INDEX of STATE, which started at START.
The box goes back where the last redraw put it, so that the redraw now
puts it where the charts say; a redraw leaves alone what it drew the
same."
  (setq exc-etaf--chart-drag nil
        exc-etaf--chart-drag-scroll '(0 . 0))
  (exc-etaf--move-chart-box
   index (or (cdr (assq index exc-etaf--chart-boxes)) (cdr start)))
  (exc-etaf--bump state))

(defun exc-etaf--dragged-chart (state chart delta)
  "Return CHART of STATE moved by DELTA, (PIXELS . PIXELS).
Across the chart goes to the pixel, within a cell by an :offset; down
it goes a line at a time, a line being as small as Emacs can show."
  (let* ((anchor (plist-get chart :anchor))
         (char (exc-etaf--char-width))
         (x (exc-etaf--walk-cols-exactly
             state (cdr anchor)
             (+ (* char (or (car (plist-get chart :offset)) 0)) (car delta))))
         (offset (/ (round (* 100 (/ (cdr x) (float char)))) 100.0))
         (chart (plist-put (copy-sequence chart) :anchor
                           (cons (exc-etaf--walk-rows
                                  state (car anchor)
                                  (/ (cdr delta)
                                     (float (exc-etaf--line-pixels))))
                                 (car x)))))
    (if (> offset 0)
        (plist-put chart :offset (cons offset 0))
      (cl-loop for (key value) on chart by #'cddr
               unless (eq key :offset) append (list key value)))))

(defun exc-etaf--drag-chart (buffer index)
  "Return the pointer handler moving floating chart INDEX of BUFFER.
While the pointer moves only the chart's box moves; the sheet takes the
chart's new place, as one undo step, when the drag ends.  A pointer
leaving the window ends the drag where the chart last stood; Esc puts
it back and does nothing more."
  (lambda (gesture)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((state exc-etaf--state)
               (source (exc-etaf--source state))
               (start exc-etaf--chart-drag))
          (pcase (exc-etaf--drag-phase gesture)
            ('start
             (setq exc-etaf--chart-drag-scroll '(0 . 0)
                   exc-etaf--chart-drag
                   (cons (copy-tree (exc-charts source))
                         (cdr (assq index exc-etaf--chart-boxes))))
             (unless (eql exc-etaf--chart index)
               (exc-etaf--select-chart state index)
               ;; The redraw selecting it may have moved the box.
               (setcdr exc-etaf--chart-drag
                       (cdr (assq index exc-etaf--chart-boxes)))))
            ('move
             (when-let* ((geometry (cdr start))
                         (delta (plist-get gesture :delta)))
               (exc-etaf--move-chart-box
                index
                (list :left (list 'px (max (car exc-etaf--chart-corner)
                                           (+ (cadr (plist-get geometry :left))
                                              (car delta))))
                      :top (list 'lh (max (cdr exc-etaf--chart-corner)
                                          (+ (cadr (plist-get geometry :top))
                                             (round (cdr delta)
                                                    (exc-etaf--line-pixels)))))))))
            ('edge
             ;; Resting against a window edge, the sheet scrolls under the
             ;; chart, which is drawn where it would land.
             (when start
               (let ((moved (exc-etaf--scroll-past-edges
                             state (plist-get gesture :edges)))
                     (scroll exc-etaf--chart-drag-scroll))
                 (unless (equal moved '(0 . 0))
                   (setq exc-etaf--chart-drag-scroll
                         (cons (+ (car scroll) (car moved))
                               (+ (cdr scroll) (cdr moved))))
                   (let ((new (copy-tree (car start))))
                     (setf (nth index new)
                           (exc-etaf--dragged-chart
                            state (nth index (car start))
                            (exc-etaf--chart-drag-delta gesture)))
                     (exc-set-charts source new)
                     (exc-etaf--bump state))))))
            ('end
             (when start
               (let ((new (copy-tree (car start))))
                 (setf (nth index new)
                       (exc-etaf--dragged-chart
                        state (nth index (car start))
                        (exc-etaf--chart-drag-delta gesture)))
                 (exc-set-charts source new)
                 (unless (equal new (car start))
                   (exc-etaf--record state (list 'charts (car start) new)))
                 (exc-etaf--end-chart-drag state index start))))
            ('cancel
             (when start
               (exc-set-charts source (car start))
               (exc-etaf--end-chart-drag state index start)))))))))

(defun exc-etaf--walk-rows (state row lines)
  "Return the shown row LINES lines below model ROW of STATE; negative goes up."
  (let* ((position (exc-etaf--row-position state row))
         (count (exc-etaf--row-count state))
         (step (if (< lines 0) -1 1))
         (left (abs lines)))
    (while (and (> left 0.5)
                (< -1 (+ position step) count))
      (setq position (+ position step)
            left (- left (exc-etaf--row-lines
                          state (exc-etaf--row-at state position)
                          nil nil))))
    (exc-etaf--row-at state position)))

(defun exc-etaf--walk-cols (state col pixels)
  "Return the shown column PIXELS right of model column COL of STATE."
  (let ((cols (exc-get-column-count (exc-etaf--source state)))
        (left (abs pixels))
        (step (if (< pixels 0) -1 1)))
    ;; Past the last column the walk goes on over empty columns, where a
    ;; chart may stand.
    (while (and (> left (/ (exc-etaf--col-pixels state col) 2.0))
                (<= 0 (+ col step)))
      (setq left (- left (exc-etaf--col-pixels state col))
            col (if (>= (+ col step) cols)
                    (+ col step)
                  (exc-etaf--step-col state col step))))
    col))

(defun exc-etaf--walk-cols-exactly (state col pixels)
  "Return where PIXELS right of the left edge of model column COL of STATE is.
The result is (COLUMN . PIXELS-INTO-IT).  Negative PIXELS go left, but
not past the first shown column, nor right past the last one a sheet may
have."
  (let ((left pixels) next)
    (while (and (>= left (exc-etaf--col-pixels state col))
                (/= col (setq next (exc-etaf--step-col state col 1))))
      (setq left (- left (exc-etaf--col-pixels state col))
            col next))
    (while (and (< left 0)
                (/= col (setq next (exc-etaf--step-col state col -1))))
      (setq col next
            left (+ left (exc-etaf--col-pixels state col))))
    (cons col (max 0 (min left (1- (exc-etaf--col-pixels state col)))))))

(defun exc-etaf--current-region (state)
  "Return the block of filled cells around STATE's cursor, as a range.
It grows from the cursor while a row or column next to it holds
something, as a spreadsheet picks the data for a chart."
  (let ((cursor (etaf-value (exc-etaf--state-cursor state))))
    (exc-etaf--region-around state (car cursor) (cdr cursor))))

(defun exc-etaf--region-around (state row col)
  "Return the block of filled cells around ROW and COL of STATE, as a range."
  (let* ((source (exc-etaf--source state))
         (rows (exc-get-row-count source))
         (cols (exc-get-column-count source))
         (top row) (bottom row) (left col) (right col)
         (filled (lambda (r1 c1 r2 c2)
                   (and (<= 0 r1) (< r2 rows) (<= 0 c1) (< c2 cols)
                        (cl-loop for r from r1 to r2 thereis
                                 (cl-loop for c from c1 to c2 thereis
                                          (not (member (exc-get-raw-value-at source r c)
                                                       '(nil ""))))))))
         (grown t))
    (while grown
      (setq grown nil)
      (when (funcall filled (1- top) left (1- top) right) (cl-decf top) (setq grown t))
      (when (funcall filled (1+ bottom) left (1+ bottom) right) (cl-incf bottom) (setq grown t))
      (when (funcall filled top (1- left) bottom (1- left)) (cl-decf left) (setq grown t))
      (when (funcall filled top (1+ right) bottom (1+ right)) (cl-incf right) (setq grown t)))
    (list top left bottom right)))

(defun exc-etaf--read-chart-type (&optional default)
  "Read a chart type, offering DEFAULT first."
  (let* ((names (mapcar #'cdr exc-chart-svg-types))
         (default (cdr (assq (or default 'column) exc-chart-svg-types)))
         (name (completing-read (format "Chart type (default %s): " default)
                                names nil t nil nil default)))
    (car (rassoc name exc-chart-svg-types))))

(defun exc-etaf-insert-chart (type)
  "Put a floating chart of TYPE over the sheet, drawing the selected cells.
With no cells selected it draws the block of filled cells around the
cursor.  The chart sits right of its data, and keeps drawing whatever
the cells hold."
  (interactive (list (exc-etaf--read-chart-type)))
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (cursor (etaf-value (exc-etaf--state-cursor state)))
         (anchor (etaf-value (exc-etaf--state-anchor state)))
         (range (if (equal cursor anchor)
                    (exc-etaf--current-region state)
                  (exc-etaf-selection)))
         ;; Right of the whole block of data the range is part of, so it
         ;; covers none of it; past the sheet's edge when that is where
         ;; the data ends, as on a spreadsheet's empty cells.
         (block (exc-etaf--region-around state (nth 0 range) (nth 1 range)))
         (chart (list :anchor (cons (nth 0 range)
                                    (1+ (max (nth 3 range) (nth 3 block))))
                      :size exc-etaf-chart-size
                      :type type
                      :range (apply #'exc-chart-svg-range-text range)
                      :by nil :title nil))
         (charts (append (exc-charts source) (list chart))))
    (setq exc-etaf--chart (1- (length charts)))
    (exc-etaf--set-charts state charts)
    (message "Chart of %s added" (plist-get chart :range))))

(defun exc-etaf--chart-index (state)
  "Return the index of the chart a chart command works on in STATE's sheet.
That is the selected chart, the only chart, or one read from the user."
  (let* ((source (exc-etaf--source state))
         (charts (exc-charts source)))
    (cond ((null charts) (user-error "No chart on this sheet; C-c i adds one"))
          ((and exc-etaf--chart (nth exc-etaf--chart charts))
           exc-etaf--chart)
          ((null (cdr charts)) 0)
          (t (let* ((names (cl-loop for chart in charts for i from 0
                                    collect (cons (format "%d: %s %s (%s)" (1+ i)
                                                          (cdr (assq (plist-get chart :type)
                                                                     exc-chart-svg-types))
                                                          (or (plist-get chart :title) "")
                                                          (plist-get chart :range))
                                                  i)))
                    (name (completing-read "Chart: " names nil t)))
               (cdr (assoc name names)))))))

(defun exc-etaf--change-chart (function)
  "Replace the chart a chart command works on with FUNCTION's result.
FUNCTION gets a copy of the chart; nil from it deletes the chart."
  (let* ((state (exc-etaf--state-or-error))
         (index (exc-etaf--chart-index state))
         (charts (copy-tree (exc-charts (exc-etaf--source state))))
         (chart (funcall function (nth index charts))))
    (setq exc-etaf--chart (and chart index))
    (exc-etaf--set-charts
     state (if chart
               (progn (setf (nth index charts) chart) charts)
             (append (cl-subseq charts 0 index) (nthcdr (1+ index) charts))))))

(defun exc-etaf-chart-type (type)
  "Draw the selected chart as TYPE."
  (interactive
   (let* ((state (exc-etaf--state-or-error))
          (chart (nth (exc-etaf--chart-index state)
                      (exc-charts (exc-etaf--source state)))))
     (list (exc-etaf--read-chart-type (plist-get chart :type)))))
  (exc-etaf--change-chart (lambda (chart) (plist-put chart :type type))))

(defun exc-etaf-chart-switch ()
  "Make the selected chart's series run the other way, rows for columns."
  (interactive)
  (let ((state (exc-etaf--state-or-error)))
    (exc-etaf--change-chart
     (lambda (chart)
       (let ((by (plist-get (exc-etaf--chart-data
                             (exc-etaf--source state) chart)
                            :by)))
         ;; The first column and row keep what they hold: what named
         ;; the series now labels the categories, and the other way.
         (let ((labels (plist-get chart :labels)))
           (setq chart (plist-put chart :labels (plist-get chart :names)))
           (setq chart (plist-put chart :names labels)))
         (plist-put chart :by (if (eq by 'rows) 'columns 'rows)))))))

(defun exc-etaf-chart-title (title)
  "Give the selected chart the TITLE; an empty one removes it."
  (interactive
   (let* ((state (exc-etaf--state-or-error))
          (chart (nth (exc-etaf--chart-index state)
                      (exc-charts (exc-etaf--source state)))))
     (list (read-string "Chart title: " (plist-get chart :title)))))
  (exc-etaf--change-chart
   (lambda (chart)
     (plist-put chart :title (and (not (string-empty-p title)) title)))))

(defun exc-etaf-chart-range ()
  "Make the selected chart draw the selected cells."
  (interactive)
  (let ((range (exc-etaf-selection)))
    (exc-etaf--change-chart
     (lambda (chart)
       ;; What labels the new cells is read from them again.
       (cl-remf chart :labels)
       (cl-remf chart :names)
       (cl-remf chart :series-names)
       (plist-put chart :range (apply #'exc-chart-svg-range-text range))))))

(defun exc-etaf-chart-move-here ()
  "Put the top left corner of the selected chart on the cursor cell."
  (interactive)
  (let ((cursor (exc-etaf--cursor)))
    (exc-etaf--change-chart
     (lambda (chart)
       (cl-loop for (key value) on (plist-put chart :anchor
                                              (cons (car cursor) (cdr cursor)))
                by #'cddr
                unless (eq key :offset) append (list key value))))))

(defun exc-etaf-chart-resize (columns lines)
  "Make the selected chart COLUMNS characters wide and LINES lines high."
  (interactive
   (let* ((state (exc-etaf--state-or-error))
          (chart (nth (exc-etaf--chart-index state)
                      (exc-charts (exc-etaf--source state))))
          (size (plist-get chart :size)))
     (list (read-number "Width in characters: " (car size))
           (read-number "Height in lines: " (cdr size)))))
  (unless (and (integerp columns) (integerp lines)
               (< 7 columns 1000) (< 3 lines 1000))
    (user-error "A chart is 8 to 999 characters wide and 4 to 999 lines high"))
  (exc-etaf--change-chart
   (lambda (chart) (plist-put chart :size (cons columns lines)))))

(defun exc-etaf-chart-delete ()
  "Delete the selected chart."
  (interactive)
  (exc-etaf--change-chart #'ignore))

(defun exc-etaf-chart-next ()
  "Select the next chart of the sheet and show where it is."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (charts (exc-charts (exc-etaf--source state))))
    (unless charts (user-error "No chart on this sheet; C-c i adds one"))
    (let* ((index (if exc-etaf--chart
                      (mod (1+ exc-etaf--chart) (length charts))
                    0))
           (anchor (plist-get (nth index charts) :anchor)))
      ;; Selecting a chart leaves the selected cells alone, so a chart
      ;; can then be given them (C-c k r); only a chart out of sight
      ;; brings the cursor to it.
      (unless (exc-etaf--chart-shown-p index)
        (exc-etaf-goto (car anchor)
                              (min (cdr anchor)
                                   (1- (exc-get-column-count
                                        (exc-etaf--source state))))))
      (exc-etaf--select-chart state index)
      ;; A chart standing right of the window's edge cannot be scrolled
      ;; to, as the cells end before it: say how to bring it back.
      (unless (exc-etaf--chart-shown-p index)
        (message "Chart %d stands past the window's right edge; C-c k m moves it here"
                 (1+ index))))))

(defun exc-etaf--chart-shown-p (index)
  "Return non-nil when chart INDEX of the sheet is in the window."
  (ignore-errors
    (etaf-runtime-host-node (exc-etaf--runtime)
                            (intern (format "exc-etaf-chart-%d" index)))))

(defun exc-etaf-chart-deselect ()
  "Select no chart."
  (interactive)
  (exc-etaf--select-chart (exc-etaf--state-or-error) nil))

(etaf-define-component exc-etaf-sheet (&key state)
  :render (exc-etaf--render (etaf-current-prop 'state)))

;;;----------------------------------------------------------------------
;;; Cursor and selection
;;;----------------------------------------------------------------------

(defun exc-etaf--state-or-error ()
  "Return the current buffer's renderer state."
  (or exc-etaf--state
      (user-error "Not in an ETAF grid buffer")))

(defun exc-etaf--cursor ()
  "Return the cursor of the current buffer as (ROW . COL)."
  (etaf-value (exc-etaf--state-cursor (exc-etaf--state-or-error))))

(defun exc-etaf--sync-point (state)
  "Put point on the cursor cell of STATE when it is painted."
  (let* ((buffer (exc-etaf--state-buffer state))
         (runtime (exc-etaf--runtime buffer))
         (position (and runtime
                        (etaf-host-ref-position runtime
                                                'exc-etaf-cursor))))
    (when position
      (with-current-buffer buffer
        (goto-char position)
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window position)
          (set-window-start window (point-min) t))))
    position))

(defun exc-etaf--scroll-to-cursor (state)
  "Adjust STATE's first row and column so that its cursor is shown."
  (let* ((cursor (etaf-value (exc-etaf--state-cursor state)))
         (row (car cursor))
         (col (cdr cursor))
         (left-ref (exc-etaf--state-left state))
         (top-ref (exc-etaf--state-top state))
         (frozen (exc-frozen (exc-etaf--source state))))
    ;; A frozen row or column is shown wherever the window is.
    (when (< (etaf-value top-ref) (car frozen))
      (etaf-set-value top-ref (car frozen)))
    (when (< (etaf-value left-ref) (cdr frozen))
      (etaf-set-value left-ref (cdr frozen)))
    (when (and (>= col (cdr frozen)) (< col (etaf-value left-ref)))
      (etaf-set-value left-ref col))
    ;; A frozen column the window has no room for cannot be scrolled to.
    (while (and (>= col (cdr frozen))
                (not (memq col (exc-etaf--visible-cols
                                state (etaf-value left-ref)))))
      (etaf-set-value left-ref (1+ (etaf-value left-ref))))
    (when (and (>= row (car frozen)) (< row (etaf-value top-ref)))
      (etaf-set-value top-ref row))
    (let ((cols (exc-etaf--visible-cols state (etaf-value left-ref)))
          (drawn (buffer-local-value 'exc-etaf--drawn
                                     (exc-etaf--state-buffer state))))
      ;; A row the last drawing showed, above its last one, is still
      ;; shown when the window has not moved: no need to lay the rows
      ;; out again.  Should its content have grown meanwhile,
      ;; `exc-etaf--settle' scrolls on from where it is painted.
      (unless (and (eql (nth 0 drawn) (etaf-value top-ref))
                   (equal (nth 1 drawn) cols)
                   (memq row (nth 2 drawn))
                   (not (eql row (car (last (nth 2 drawn))))))
        ;; Far below the window, start from the first row that leaves
        ;; the cursor's row last, instead of laying the window out again
        ;; for every row between.
        (let ((shown (exc-etaf--visible-rows
                      state (etaf-value top-ref) cols)))
          (when (and (>= row (car frozen))
                     shown
                     (> (exc-etaf--row-position state row)
                        (exc-etaf--row-position
                         state (car (last shown)))))
            (etaf-set-value top-ref
                            (exc-etaf--top-ending-at state row cols))))
        (while (and (>= row (car frozen))
                    (not (memq row (exc-etaf--visible-rows
                                    state (etaf-value top-ref) cols))))
          (etaf-set-value top-ref (exc-etaf--step-row
                                   state (etaf-value top-ref) 1)))))))

(defun exc-etaf--top-ending-at (state row cols)
  "Return the first scrolled row of STATE that shows ROW last, over COLS.
Rows are laid out from ROW upwards only as far as the window reaches."
  (let* ((source (exc-etaf--source state))
         (merges (exc-get-merges source))
         (frozen (car (exc-frozen source)))
         (position (exc-etaf--row-position state row))
         (first 0)
         ;; As in `exc-etaf--visible-rows'.
         (budget (- (cdr (exc-etaf--viewport state))
                    3 (exc-etaf--title-lines state)
                    (exc-etaf--row-lines state row cols merges))))
    (while (and (< first position)
                (< (exc-etaf--row-at state first) frozen))
      (setq budget (- budget (exc-etaf--row-lines
                              state (exc-etaf--row-at state first)
                              cols merges))
            first (1+ first)))
    (while (and (> position first)
                (let ((need (exc-etaf--row-lines
                             state (exc-etaf--row-at state (1- position))
                             cols merges)))
                  (when (<= need budget)
                    (setq budget (- budget need))
                    t)))
      (setq position (1- position)))
    (exc-etaf--row-at state position)))

(defun exc-etaf--settle (state)
  "Show STATE's cursor and put point on it.
Row heights are estimated before layout, so a row taller than estimated
can still push the cursor below the window; scroll further until the
painted cursor fits."
  (etaf-batch (exc-etaf--scroll-to-cursor state))
  (let ((lines (cdr (exc-etaf--viewport state)))
        (top-ref (exc-etaf--state-top state))
        (row (car (etaf-value (exc-etaf--state-cursor state))))
        (guard 0)
        position)
    (while (and (setq position (exc-etaf--sync-point state))
                (< guard 50)
                (< (etaf-value top-ref) row)
                (with-current-buffer (exc-etaf--state-buffer state)
                  (> (line-number-at-pos position) lines)))
      (setq guard (1+ guard))
      (etaf-set-value top-ref (exc-etaf--step-row
                               state (etaf-value top-ref) 1)))))

(defun exc-etaf-goto (row col &optional extend)
  "Move the cursor to model ROW and COL.
With EXTEND non-nil keep the selection anchor, extending the range; a
cons (ROW . COL) is the anchor to extend from."
  (let* ((state (exc-etaf--state-or-error))
         ;; A filtered-out row is not a place to be: take the next shown.
         (row (exc-etaf--row-at
               state (exc-etaf--row-position state row)))
         ;; Nor is a hidden column.
         (col (exc-etaf--shown-col state col)))
    (exc-etaf--finish-edit state)
    (etaf-batch
      (etaf-set-value (exc-etaf--state-cursor state) (cons row col))
      (cond ((consp extend)
             (etaf-set-value (exc-etaf--state-anchor state) extend))
            ((not extend)
             (etaf-set-value (exc-etaf--state-anchor state)
                             (cons row col)))))
    (setq exc--nav-row row
          exc--nav-col (1+ col))
    (exc-etaf--settle state)
    t))

(defvar exc-etaf--goto-history nil
  "Addresses typed to `exc-etaf-goto-address'.")

(defun exc-etaf--parse-address (text)
  "Return (SHEET TOP LEFT BOTTOM RIGHT) for the address TEXT, or nil.
TEXT is a cell (D120), a range (A1:B3), whole columns (B:D) or whole
rows (5:9), after an optional sheet prefix (Sheet2!A1, \='My Sheet\='!A1).
SHEET is nil without a prefix.  Ends left open, as the rows of a whole
column, are nil."
  (let* ((text (string-trim text))
         (prefix (and (> (length text) 0)
                      (exc-parser-sheet-prefix-at text 0)))
         (rest (if prefix (substring text (car prefix)) text))
         (ends (split-string rest ":"))
         (start (exc-calc--reference-coords (car ends)))
         (end (if (cdr ends) (exc-calc--reference-coords (cadr ends)) start)))
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

(defun exc-etaf-goto-address (address)
  "Go to ADDRESS, a cell, range, whole column or row, maybe on another sheet.
D120 moves the cursor there; A1:B3 selects that range, B:B a column and
5:5 a row; Sheet2!A1 shows Sheet2 first, as the name box of a
spreadsheet does."
  (interactive
   (list (completing-read
          "Go to (e.g. D120, A1:B3, Sheet2!A1): "
          (append
           (mapcar #'car (and (exc-workbook-of exc--data-source)
                              (exc-workbook-names
                               (exc-workbook-of exc--data-source))))
           (mapcar (lambda (sheet)
                     (exc-calc-sheet-prefix
                             (exc-workbook-sheet-name sheet)))
                   (cdr (exc-sheets))))
          nil nil nil 'exc-etaf--goto-history)))
  (pcase-let ((`(,sheet ,top ,left ,bottom ,right)
               ;; A name first: Qty is a name before it is column QTY.
               (or (when-let* ((workbook (exc-workbook-of
                                          exc--data-source))
                               (definition (exc-workbook-name-definition
                                            workbook (string-trim address))))
                     (exc-etaf--parse-address (substring definition 1)))
                   (exc-etaf--parse-address address)
                   (user-error "Not a cell, range or name: %s" address))))
    (when sheet
      (exc-switch-to-sheet
       (or (exc-workbook-sheet (exc--workbook) sheet)
           (user-error "No sheet called %s" sheet))))
    (let* ((source exc--data-source)
           (rows (exc-get-row-count source))
           (cols (exc-get-column-count source)))
      ;; Past the last row or column are empty cells, as in a
      ;; spreadsheet; only past the most a sheet may have is nothing.
      (when (or (and bottom (>= bottom (exc-etaf--max-rows)))
                (and right (>= right (exc-etaf--max-cols))))
        (user-error "%s is past the last row or column a sheet may have"
                    address))
      (exc-etaf-goto (or bottom (1- rows)) (or right (1- cols)))
      (let ((range (or (not (eql top bottom)) (not (eql left right))
                       (null top) (null left))))
        (exc-etaf-goto (or top 0) (or left 0) range)
        ;; Going to the far corner and back can leave lines whose text
        ;; changed undrawn (the status bar of a long range): draw it
        ;; all.  (ebox writes lines with modification hooks inhibited,
        ;; so a line that only changed its text may not be redrawn.)
        (when range
          (force-window-update (selected-window)))))))

(defun exc-etaf--cell-extent (row col)
  "Return (TOP LEFT BOTTOM RIGHT) of the cell covering ROW and COL."
  (or (exc--merge-at (exc-get-merges exc--data-source) row col)
      (list row col row col)))

(defvar-local exc-etaf--pending-move nil
  "(CURSOR . ANCHOR) of moves made but not drawn yet, or nil.
ANCHOR nil keeps the selection's anchor.")

(defvar-local exc-etaf--pending-timer nil
  "Idle timer that draws the pending move, should no command do so.")

(defun exc-etaf-move (rows cols &optional extend)
  "Move the cursor ROWS down and COLS right, stepping over merged cells.
With EXTEND non-nil extend the selection instead.

While more input is waiting, as when an arrow key is held faster than
a move can be drawn, the move is only noted; the waiting keys move on
from it, and the cursor is drawn once they are done.  Emacs's own
redisplay skips frames the same way."
  (let* ((state (exc-etaf--state-or-error))
         (pending exc-etaf--pending-move)
         (cursor (or (car pending) (exc-etaf--cursor)))
         (extent (exc-etaf--cell-extent (car cursor) (cdr cursor)))
         ;; Rows are counted among the shown ones, so filtered rows are
         ;; stepped over.
         (row (cond ((> rows 0)
                     (exc-etaf--step-row state (nth 2 extent) rows))
                    ((< rows 0)
                     (exc-etaf--row-at
                      state (+ (exc-etaf--row-position
                                state (nth 0 extent))
                               rows)))
                    (t (car cursor))))
         (col (cond ((> cols 0)
                     (exc-etaf--step-col state (nth 3 extent) cols))
                    ((< cols 0)
                     (exc-etaf--step-col state (nth 1 extent) cols))
                    (t (cdr cursor))))
         (anchor (if extend (cdr pending) (cons row col))))
    (if (and (input-pending-p)
             (not (etaf-value (exc-etaf--state-editing state))))
        (progn
          (setq exc-etaf--pending-move (cons (cons row col) anchor))
          (unless (timerp exc-etaf--pending-timer)
            (setq exc-etaf--pending-timer
                  (run-with-idle-timer
                   0.05 nil #'exc-etaf--draw-pending-move
                   (current-buffer)))))
      (setq exc-etaf--pending-move nil)
      (exc-etaf-goto row col (if extend (or anchor t) nil)))))

(defun exc-etaf--draw-pending-move (&optional buffer)
  "Draw the move noted in BUFFER, the current one by default."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp exc-etaf--pending-timer)
      (cancel-timer exc-etaf--pending-timer))
    (setq exc-etaf--pending-timer nil)
    (when-let* ((pending exc-etaf--pending-move))
      (setq exc-etaf--pending-move nil)
      (exc-etaf-goto (car (car pending)) (cdr (car pending))
                            (or (cdr pending) t)))))

(defun exc-etaf--move-key-p (keys)
  "Return non-nil when KEYS end in an arrow or page key, maybe shifted."
  (let ((key (and (> (length keys) 0) (aref keys (1- (length keys))))))
    (and key
         (memq (event-basic-type key) '(up down left right next prior))
         (null (remq 'shift (event-modifiers key))))))

(defun exc-etaf--draw-before-command ()
  "Draw a pending move before any command but another move.
Everything else, a click or a typed key, sees where the cursor is."
  (when (and exc-etaf--pending-move
             (not (exc-etaf--move-key-p (this-command-keys-vector))))
    (exc-etaf--draw-pending-move)))

(defun exc-etaf--draw-after-command ()
  "Draw a pending move once no more input is waiting."
  (when (and exc-etaf--pending-move (not (input-pending-p)))
    (exc-etaf--draw-pending-move)))

(defun exc-etaf-move-tab (direction)
  "Move to the next cell in reading order, or the previous one.
DIRECTION is 1 or -1.  Movement wraps at both ends of the sheet."
  (let* ((state (exc-etaf--state-or-error))
         (source exc--data-source)
         (rows (exc-etaf--row-count state))
         (cols (exc-get-column-count source))
         (cursor (exc-etaf--cursor))
         (extent (exc-etaf--cell-extent (car cursor) (cdr cursor)))
         (position (exc-etaf--row-position state (car cursor)))
         (edge (if (> direction 0) (nth 3 extent) (nth 1 extent)))
         (col (exc-etaf--step-col state edge direction)))
    ;; Stepping off either end of the row wraps to the next or previous
    ;; row; the last column is the sheet's, past which only a cursor
    ;; already there goes on over empty ones.
    (when (or (= col edge) (and (< edge cols) (>= col cols)))
      (setq position (mod (+ position direction) rows)
            col (exc-etaf--shown-col
                 state (if (> direction 0) 0 (1- cols)) (< direction 0))))
    (exc-etaf-goto (exc-etaf--row-at state position) col)))

(defun exc-etaf--page-size ()
  "Return how many rows the window currently shows."
  (let* ((state (exc-etaf--state-or-error))
         (cols (exc-etaf--visible-cols
                state (etaf-value (exc-etaf--state-left state)))))
    ;; Frozen rows are on every page.
    (max 1 (cl-count-if
            (lambda (row)
              (>= row (car (exc-frozen
                            (exc-etaf--source state)))))
            (exc-etaf--visible-rows
             state (etaf-value (exc-etaf--state-top state))
             cols)))))

(defun exc-etaf-page-down ()
  "Move the cursor one window of rows down."
  (interactive)
  (exc-etaf-move (exc-etaf--page-size) 0))

(defun exc-etaf-page-up ()
  "Move the cursor one window of rows up."
  (interactive)
  (exc-etaf-move (- (exc-etaf--page-size)) 0))

(defun exc-etaf--scroll (rows)
  "Scroll the window ROWS down without moving the cursor."
  (let* ((state (exc-etaf--state-or-error))
         (top (exc-etaf--state-top state))
         (frozen (car (exc-frozen (exc-etaf--source state)))))
    ;; The frozen rows do not scroll: the window starts under them.
    (etaf-set-value top (max frozen
                             (exc-etaf--step-row
                              state (max frozen (etaf-value top)) rows)))))

(defun exc-etaf-scroll-down ()
  "Scroll the sheet three rows down."
  (interactive)
  (exc-etaf--scroll 3))

(defun exc-etaf-scroll-up ()
  "Scroll the sheet three rows up."
  (interactive)
  (exc-etaf--scroll -3))

(defun exc-etaf-freeze-panes ()
  "Keep the rows above the cursor and the columns left of it in place.
They stay at the top and the left of the window while the rest of the
sheet scrolls, and are saved with the grid.  With the cursor in the
first cell nothing is above or left of it, and every row and column
scrolls again, as after `exc-unfreeze-panes'."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (cursor (exc-etaf--cursor))
         (extent (exc-etaf--cell-extent (car cursor) (cdr cursor)))
         (rows (nth 0 extent))
         (cols (nth 1 extent))
         (old (exc-frozen source)))
    (unless (exc-set-frozen source rows cols)
      (user-error "This grid cannot keep rows and columns in place"))
    ;; A window shows the frozen rows and columns it has room for: all of
    ;; them have to fit, with the cell under the cursor.
    (let* ((shown-cols (exc-etaf--visible-cols state cols))
           (shown-rows (exc-etaf--visible-rows state rows shown-cols))
           (last-row (and (> rows 0)
                          (exc-etaf--row-at
                           state (1- (exc-etaf--row-position
                                      state rows))))))
      (unless (and (or (zerop cols) (memq (1- cols) shown-cols))
                   (or (null last-row) (>= last-row rows)
                       (memq last-row shown-rows)))
        (exc-set-frozen source (car old) (cdr old))
        (user-error "The window is too small to freeze that much")))
    (exc-etaf-note-frozen old)
    (exc-etaf--bump state)
    (exc-etaf--settle state)
    (message (if (and (zerop rows) (zerop cols))
                 "No row or column is frozen"
               "Frozen: %d row%s, %d column%s")
             rows (if (= rows 1) "" "s") cols (if (= cols 1) "" "s"))))

(defun exc-etaf-first-cell ()
  "Move the cursor to the first cell of the sheet, scrolling back home.
With frozen rows or columns that is the first cell past them, as
Ctrl+Home goes in a spreadsheet."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (frozen (exc-frozen (exc-etaf--source state))))
    (etaf-batch
      (etaf-set-value (exc-etaf--state-top state) (car frozen))
      (etaf-set-value (exc-etaf--state-left state) (cdr frozen)))
    (exc-etaf-goto (car frozen) (cdr frozen))))

(defun exc-etaf-last-cell ()
  "Move the cursor to the last cell of the sheet."
  (interactive)
  (exc-etaf-goto (1- (exc-get-row-count exc--data-source))
                        (exc-etaf--shown-col
                         exc-etaf--state
                         (1- (exc-get-column-count exc--data-source))
                         t)))

;;;----------------------------------------------------------------------
;;; Filters
;;;----------------------------------------------------------------------

(defun exc-etaf--refilter (state)
  "Apply STATE's filters again and show the result."
  (exc-etaf--finish-edit state)
  (exc-etaf--apply-filters state)
  (etaf-set-value (exc-etaf--state-top state) 0)
  (exc-etaf--bump state)
  (let ((cursor (etaf-value (exc-etaf--state-cursor state))))
    (exc-etaf-goto (car cursor) (cdr cursor))))

(defun exc-etaf-filter-column (col op value)
  "Show only the rows whose cell in model column COL satisfies OP and VALUE.
OP is one of `exc-conditional-operators'.  The first row holds
the column titles and stays.  Filters on several columns combine.  Rows
are filtered when the filter is set and on `exc-refresh', not
after each edit."
  (interactive
   (let* ((col (cdr (exc-etaf--cursor)))
          (choices (mapcar (lambda (entry) (cons (cdr entry) (car entry)))
                           exc-conditional-operators))
          (op (cdr (assoc (completing-read
                           (format "Show rows whose column %s: "
                                   (exc--col-number-to-letter col))
                           choices nil t)
                          choices))))
     (list col op (if (member op '("empty" "error")) "" (read-string "Value: ")))))
  (unless (assoc op exc-conditional-operators)
    (user-error "Unknown condition: %s" op))
  (let ((state (exc-etaf--state-or-error)))
    (setf (alist-get col (exc-etaf--state-filters state))
          (list op value))
    (exc-etaf--refilter state)
    (let ((counts (exc-etaf--data-row-counts state)))
      (message "Showing %d of %d rows" (car counts) (cdr counts)))))

(defun exc-etaf-filter-by-cell ()
  "Show only the rows that hold the cursor cell's value in its column."
  (interactive)
  (let* ((cursor (exc-etaf--cursor))
         (value (exc-etaf--display-text
                 (exc-get-computed-value-at exc--data-source
                                             (car cursor) (cdr cursor))
                 (exc-cell-format exc--data-source
                                         (car cursor) (cdr cursor)))))
    (if (string-empty-p value)
        (exc-etaf-filter-column (cdr cursor) "empty" "")
      (exc-etaf-filter-column (cdr cursor) "=" value))))

(defun exc-etaf-clear-column-filter ()
  "Remove the filter on the cursor's column."
  (interactive)
  (let ((state (exc-etaf--state-or-error)))
    (setf (exc-etaf--state-filters state)
          (assq-delete-all (cdr (exc-etaf--cursor))
                           (exc-etaf--state-filters state)))
    (exc-etaf--refilter state)))

(defun exc-etaf-clear-filters ()
  "Remove every filter and show all rows."
  (interactive)
  (let ((state (exc-etaf--state-or-error)))
    (setf (exc-etaf--state-filters state) nil)
    (exc-etaf--refilter state)))

(defun exc-etaf-row-start ()
  "Move the cursor to the first column of its row."
  (interactive)
  (exc-etaf-goto (car (exc-etaf--cursor)) 0))

(defun exc-etaf-row-end ()
  "Move the cursor to the last column of its row."
  (interactive)
  (exc-etaf-goto (car (exc-etaf--cursor))
                        (exc-etaf--shown-col
                         exc-etaf--state
                         (1- (exc-get-column-count exc--data-source))
                         t)))

(defun exc-etaf-extend-down ()
  "Extend the selection one row down."
  (interactive)
  (exc-etaf-move 1 0 t))

(defun exc-etaf-extend-up ()
  "Extend the selection one row up."
  (interactive)
  (exc-etaf-move -1 0 t))

(defun exc-etaf-extend-right ()
  "Extend the selection one column right."
  (interactive)
  (exc-etaf-move 0 1 t))

(defun exc-etaf-extend-left ()
  "Extend the selection one column left."
  (interactive)
  (exc-etaf-move 0 -1 t))

(defun exc-etaf-selection ()
  "Return the selected range as (TOP LEFT BOTTOM RIGHT) in model coordinates."
  (let* ((state (exc-etaf--state-or-error))
         (cursor (etaf-value (exc-etaf--state-cursor state)))
         (anchor (etaf-value (exc-etaf--state-anchor state))))
    (list (min (car cursor) (car anchor)) (min (cdr cursor) (cdr anchor))
          (max (car cursor) (car anchor)) (max (cdr cursor) (cdr anchor)))))

(defun exc-etaf-cell-at-point ()
  "Return the descriptor of the cursor cell.
The plist has the shape `exc-get-cell-at-point' returns, in
display coordinates: display column is model column plus one."
  (when exc-etaf--state
    (let* ((cursor (exc-etaf--cursor))
           (extent (exc-etaf--cell-extent (car cursor) (cdr cursor))))
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

(defun exc-etaf--bump (state)
  "Re-render STATE's window from the data source."
  (let ((tick (exc-etaf--state-tick state)))
    (etaf-set-value tick (1+ (etaf-value tick)))))

(defun exc-etaf--calc-ready (waiters)
  "Show the answers CALC's WAITERS, (MODEL ROW COL) each, were waiting for.
Every grid showing one of their sheets, or a sheet of the same book, is
recalculated and drawn again."
  (let ((models (delete-dups (mapcar #'car waiters)))
        (everything (cl-some (lambda (waiter) (null (nth 1 waiter))) waiters))
        (done nil))
    (dolist (buffer (buffer-list))
      (let ((state (buffer-local-value 'exc-etaf--state buffer))
            (source (buffer-local-value 'exc--data-source buffer)))
        (when (and state source (gethash :model source))
          (let* ((workbook (exc-workbook-of source))
                 (shown (if workbook (exc-workbook--models workbook)
                          (list (gethash :model source)))))
            (when (cl-some (lambda (model) (memq model shown)) models)
              ;; Sheets of one book are recalculated once, together.
              (unless (memq (or workbook source) done)
                (push (or workbook source) done)
                (exc-recalculate source everything))
              (with-current-buffer buffer
                (exc-etaf--widen-visible state)
                (exc-etaf--bump state)))))))))

(add-hook 'exc-calc-calc-ready-functions #'exc-etaf--calc-ready)

(defun exc-etaf--widen-visible (state)
  "Widen STATE's columns to fit the values now in the window.
An edit can change any formula cell, so every shown cell is measured.
Columns only shrink on `exc-refresh'."
  (let* ((source (exc-etaf--source state))
         (widths (exc-etaf--state-widths state))
         (manual (exc-etaf--manual state))
         (merges (exc-get-merges source))
         (cols (exc-etaf--visible-cols
                state (etaf-value (exc-etaf--state-left state))))
         (rows (exc-etaf--visible-rows
                state (etaf-value (exc-etaf--state-top state)) cols)))
    (dolist (row rows)
      (dolist (col cols)
        (let ((merge (and merges (exc--merge-at merges row col))))
          (unless (or (>= col (length widths))
                      (and manual (gethash col manual))
                      (and merge
                           (or (/= (nth 1 merge) (nth 3 merge))
                               (/= row (nth 0 merge)))))
            (aset widths col
                  (max (aref widths col)
                       (exc-etaf--value-width
                        (exc-get-computed-value-at source row col)
                        (exc-cell-format source row col))))))))))

(defun exc-etaf--end-session (state)
  "End STATE's input session and return its text, or nil without one."
  (let ((session (exc-etaf--state-session state)))
    (when session
      (setf (exc-etaf--state-session state) nil)
      (with-current-buffer (exc-etaf--state-buffer state)
        (setq exc-etaf--typing-keys t))
      (prog1 (etaf-input-end session)
        (etaf-set-value (exc-etaf--state-editing state) nil)))))

(defun exc-etaf--finish-edit (state)
  "Commit STATE's in-place edit, if one is open."
  (when (exc-etaf--state-session state)
    (let* ((cursor (etaf-value (exc-etaf--state-cursor state)))
           (extent (with-current-buffer (exc-etaf--state-buffer state)
                     (exc-etaf--cell-extent (car cursor) (cdr cursor))))
           (value (exc-etaf--end-session state))
           ;; A cell that only takes some values refuses others, and the
           ;; edit is dropped, as a spreadsheet's stop alert does.
           (problem (and value
                         (exc-validation-problem
                          (exc-etaf--source state)
                          (nth 0 extent) (nth 1 extent) value))))
      (if (and value (not problem))
          (exc-etaf--enter-cells
           state (list (list (nth 0 extent) (nth 1 extent) value)))
        (exc-etaf--bump state)
        (exc-etaf--settle state)
        (when problem
          (user-error "%s" problem))))))

(defun exc-etaf-commit-edit ()
  "Store the text of the in-place edit in its cell."
  (interactive)
  (exc-etaf--finish-edit (exc-etaf--state-or-error)))

(defun exc-etaf-cancel-edit ()
  "Leave the in-place edit without changing its cell."
  (interactive)
  (let ((state (exc-etaf--state-or-error)))
    (when (exc-etaf--end-session state)
      (exc-etaf--bump state)
      (exc-etaf--settle state))))

(defun exc-etaf-commit-edit-and-move-down ()
  "Store the text of the in-place edit and move to the row below."
  (interactive)
  (exc-etaf-commit-edit)
  (exc-etaf-move 1 0))

(defun exc-etaf-commit-edit-and-move-up ()
  "Store the text of the in-place edit and move to the row above."
  (interactive)
  (exc-etaf-commit-edit)
  (exc-etaf-move -1 0))

(defun exc-etaf-commit-edit-and-move-right ()
  "Store the text of the in-place edit and move to the cell on the right."
  (interactive)
  (exc-etaf-commit-edit)
  (exc-etaf-move 0 1))

(defun exc-etaf-commit-edit-and-move-left ()
  "Store the text of the in-place edit and move to the cell on the left."
  (interactive)
  (exc-etaf-commit-edit)
  (exc-etaf-move 0 -1))

(defvar exc-etaf-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'exc-etaf-commit-edit-and-move-down)
    (define-key map [return] #'exc-etaf-commit-edit-and-move-down)
    (define-key map (kbd "C-c C-c") #'exc-etaf-commit-edit)
    (define-key map (kbd "M-RET") #'newline)
    (define-key map [M-return] #'newline)
    (define-key map (kbd "C-c C-k") #'exc-etaf-cancel-edit)
    (define-key map [escape] #'exc-etaf-cancel-edit)
    (define-key map (kbd "TAB") #'exc-next-cell)
    (define-key map [tab] #'exc-next-cell)
    (define-key map [backtab] #'exc-previous-cell)
    ;; Undoing in an edit takes back typing, not the change before the cell.
    (define-key map [remap undo] #'exc-etaf-edit-undo)
    (define-key map [remap undo-only] #'exc-etaf-edit-undo)
    (define-key map [remap undo-redo] #'exc-etaf-edit-redo)
    (dolist (key '("C-/" "C-_" "C-x u" "s-z"))
      (define-key map (kbd key) #'exc-etaf-edit-undo))
    (dolist (key '("C-?" "C-M-_" "s-Z"))
      (define-key map (kbd key) #'exc-etaf-edit-redo))
    map)
  "Keymap consulted before the editing commands of an in-place cell edit.
`ESC' and `C-c C-k' cancel the edit, and the undo keys undo the typing.
In a window system `ESC' is a key of its own, so `ESC x' cannot start
`M-x' while an edit is open; a terminal sends `ESC' and the next key
together, and those still reach `M-x'.")

(defvar exc-etaf-input-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map exc-etaf-edit-map)
    (define-key map [down] #'exc-etaf-commit-edit-and-move-down)
    (define-key map [up] #'exc-etaf-commit-edit-and-move-up)
    (define-key map [right] #'exc-etaf-commit-edit-and-move-right)
    (define-key map [left] #'exc-etaf-commit-edit-and-move-left)
    map)
  "Keymap of an edit begun by typing a character, as in a spreadsheet.
It is `exc-etaf-edit-map' plus the arrow keys, which store the text
and move to the next cell.  An edit begun with F2 or RET uses the arrow
keys to move within the text instead.")

(defvar-local exc-etaf--edit-buffer nil
  "The grid buffer of the edit whose draft the current buffer holds.")

(defvar-local exc-etaf--edit-start nil
  "The text the draft of the edit in the current buffer began from.")

(defvar-local exc-etaf--edit-input nil
  "Non-nil when the edit in the current buffer began by typing a character.")

(defun exc-etaf--end-edit-from-draft ()
  "Cancel the in-place edit whose draft is in the current buffer."
  (let ((owner exc-etaf--edit-buffer))
    (when (buffer-live-p owner)
      (with-current-buffer owner
        (exc-etaf-cancel-edit)))))

(defun exc-etaf-edit-undo ()
  "Undo the last change to the text being typed, or cancel the edit.
An edit begun by typing is one step: undo throws the whole input away,
and the cell keeps what it held before.  An edit begun with F2 or RET
takes the typing back a change at a time, and with the text back to what
the cell held when the edit began, one more undo cancels the edit.  Either
way nothing is stored and no undo record is made."
  (interactive)
  (if exc-etaf--edit-input
      (exc-etaf--end-edit-from-draft)
    (let ((start (or exc-etaf--edit-start "")))
      (if (equal (buffer-substring-no-properties (point-min) (point-max)) start)
          (exc-etaf--end-edit-from-draft)
        ;; `undo' alone would redo the last undo when this command is not
        ;; `undo' itself; `undo-only' keeps taking text back.
        (undo-only)
        ;; A reused editing buffer can hold steps from an earlier edit; never
        ;; walk past the text this edit began from.
        (when (< (buffer-size) (length start))
          (exc-etaf--end-edit-from-draft))))))

(defun exc-etaf-edit-redo ()
  "Redo the text last undone in the edit being typed.
Nothing is redone, and nothing is reported, when there is no such text."
  (interactive)
  (when (fboundp 'undo-redo)
    (ignore-errors (undo-redo))))

(defun exc-etaf-edit-cell (&optional initial input)
  "Edit the raw value of the cursor cell in place.
INITIAL, a string, replaces the cell text the edit starts from.  INPUT,
non-nil when the edit began by typing a character, gives the arrow keys
the meaning they have in a spreadsheet: store the text and move to the
adjacent cell.  An edit begun any other way, with F2 or RET, leaves the
arrow keys moving within the text.
RET stores the text and moves down, C-c C-c stores it and stays, M-RET
inserts a newline and C-c C-k or ESC cancels.  Moving to another cell,
with TAB or the mouse, stores the text too.  In an edit begun by typing,
the undo keys throw the whole input away in one step; in one begun with
F2 or RET they take the typing back a change at a time, and cancel the
edit once the text is back to what the cell held."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (buffer (exc-etaf--state-buffer state))
         (cursor (exc-etaf--cursor))
         (source (exc-etaf--source state))
         (extent (exc-etaf--cell-extent (car cursor) (cdr cursor)))
         (start (or initial (exc-edit-text
                             source (nth 0 extent) (nth 1 extent))))
         (rule (and (not initial)
                    (exc-validation-at source (car cursor) (cdr cursor)))))
    (if (eq (nth 4 rule) 'list)
        (exc-etaf--choose state cursor rule)
      (unless (exc-etaf--state-session state)
        (exc-etaf--settle state)
        (etaf-set-value (exc-etaf--state-editing state) (or initial t))
        ;; An undo in the edit runs in the editing buffer, which is why it
        ;; is told where the grid and the text it started from are.
        (etaf-input-edit-command-add 'exc-etaf-edit-undo)
        (etaf-input-edit-command-add 'exc-etaf-edit-redo)
        (condition-case err
            (progn
              (setf (exc-etaf--state-session state)
                    (etaf-input-begin
                     (exc-etaf--runtime buffer) 'exc-etaf-cursor
                     :keymap (if input
                                 exc-etaf-input-map
                               exc-etaf-edit-map)
                     :local-variables
                     (list (cons 'exc-etaf--edit-buffer buffer)
                           (cons 'exc-etaf--edit-start start)
                           (cons 'exc-etaf--edit-input (and input t)))
                     :on-leave (lambda ()
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (exc-etaf-commit-edit))))))
              ;; The session's own editing commands take the printing keys.
              (setq exc-etaf--typing-keys nil))
          (error
           (etaf-set-value (exc-etaf--state-editing state) nil)
           (signal (car err) (cdr err))))))))

(defun exc-etaf--choose (state cursor rule)
  "Have the cell at CURSOR take one of the values its list RULE allows.
The choice is read in the minibuffer; an empty one clears the cell."
  (let* ((source (exc-etaf--source state))
         (options (exc-validation-options source rule))
         (current (exc-get-raw-value-at source (car cursor) (cdr cursor)))
         (choice (completing-read
                  "Value (empty clears): " options nil
                  (lambda (text)
                    (or (string-empty-p text)
                        (member-ignore-case text options)))
                  nil nil (and (member current options) current))))
    (exc-etaf--enter-cells
     state (list (list (car cursor) (cdr cursor) choice)))))

(defcustom exc-etaf-type-to-edit t
  "Non-nil makes typing a character replace the cursor cell, as in Excel.
nil leaves the cell alone until RET or F2 starts an edit, so a stray key
changes nothing."
  :type 'boolean
  :group 'exc)

(defun exc-etaf-self-insert ()
  "Handle a printing key typed while no cell is being edited.
With `exc-etaf-type-to-edit' it starts editing the cursor cell
with the typed character as its text; otherwise it only says how to
edit."
  (interactive)
  (cond ((not exc-etaf-type-to-edit)
         (message "Press RET or F2 to edit this cell"))
        ((characterp last-command-event)
         (exc-etaf-edit-cell (string last-command-event) t))
        (t (exc-etaf-edit-cell))))

(defun exc-etaf-clear-cells ()
  "Empty every cell of the selected range."
  (interactive)
  (let ((state (exc-etaf--state-or-error))
        (range (exc-etaf-selection))
        changes)
    (cl-loop for row from (nth 0 range) to (nth 2 range)
             when (exc-etaf--row-shown-p state row) do
             (cl-loop for col from (nth 1 range) to (nth 3 range)
                      when (exc-etaf--col-shown-p state col) do
                      (push (list row col "") changes)))
    (exc-etaf--set-cells state (nreverse changes))))

(defun exc-etaf-set-cell (row col value)
  "Store raw VALUE in model ROW and COL as one undoable change."
  (exc-etaf--set-cells (exc-etaf--state-or-error)
                              (list (list row col value))))

;;;----------------------------------------------------------------------
;;; Undo
;;;----------------------------------------------------------------------

(defconst exc-etaf--undo-limit 200
  "How many changes the undo history of a grid keeps.")

(defun exc-etaf-note-frozen (old)
  "Record for undo that the frozen rows and columns were OLD, a (ROWS . COLS).
Nothing is recorded when they did not change."
  (when-let* ((state exc-etaf--state))
    (let ((new (exc-frozen (exc-etaf--source state))))
      (unless (equal old new)
        (exc-etaf--record state (list 'frozen old new))))))

(defcustom exc-etaf-undo-model-cells 1000000
  "How many cells the whole-sheet copies kept for undo may hold in all.
Undoing an added or deleted line, a sort or a merge needs the sheet as it
was before.  When the copies the undo history holds exceed this many
cells, the oldest such changes are forgotten; the newest change can
always be undone.  nil means no limit beyond the number of changes."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'exc)

(defun exc-etaf--record-models (record)
  "Return the models RECORD keeps, looking inside groups."
  (pcase record
    (`(model ,old ,new) (list old new))
    (`(group . ,records) (mapcan #'exc-etaf--record-models records))))

(defun exc-etaf--trim-undo (undo)
  "Cut UNDO, a list of records newest first, to fit the undo limits.
Keep at most `exc-etaf--undo-limit' records and no more models
than `exc-etaf-undo-model-cells' allows, but always the first."
  (let ((seen (make-hash-table :test #'eq))
        (cells 0)
        (count 0)
        (budget exc-etaf-undo-model-cells))
    (cl-loop for tail on undo
             do (cl-incf count)
                (dolist (model (exc-etaf--record-models (car tail)))
                  (unless (or (null model) (gethash model seen))
                    (puthash model t seen)
                    (cl-incf cells (* (exc-data-model-rows model)
                                      (exc-data-model-cols model)))))
             when (and (cdr tail)
                       (or (>= count exc-etaf--undo-limit)
                           (and budget
                                (exc-etaf--record-models (cadr tail))
                                (> (+ cells
                                      (cl-loop for model in (exc-etaf--record-models
                                                             (cadr tail))
                                               unless (or (null model)
                                                          (gethash model seen))
                                               sum (* (exc-data-model-rows model)
                                                      (exc-data-model-cols model))))
                                   budget))))
             do (setcdr tail nil))
    undo))

(defun exc-etaf--views (source)
  "The live buffers showing SOURCE's sheet, or a sheet of its workbook."
  (let* ((workbook (exc-workbook-of source))
         (models (if workbook (exc-workbook--models workbook)
                   (list (gethash :model source))))
         views)
    (dolist (buffer (buffer-list))
      (let ((state (buffer-local-value 'exc-etaf--state buffer))
            (shown (buffer-local-value 'exc--data-source buffer)))
        (when (and state shown (memq (gethash :model shown) models))
          (push buffer views))))
    (nreverse views)))

(defvar exc-etaf--stale-views nil
  "Buffers to draw again once the command that changed their grid ends.")

(defun exc-etaf--draw-stale-views ()
  "Draw again the buffers in `exc-etaf--stale-views'."
  (let ((buffers exc-etaf--stale-views))
    (setq exc-etaf--stale-views nil)
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when exc-etaf--state
            (exc-etaf--widen-visible exc-etaf--state)
            (exc-etaf--bump exc-etaf--state)))))))

(defun exc-etaf--changed (state)
  "Mark the buffer of STATE changed, so that `save-buffer' saves the grid.
Other buffers showing the same workbook are drawn again, and those
visiting the same file are marked changed too."
  (let ((buffer (exc-etaf--state-buffer state)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p t))
      (let ((file (buffer-file-name buffer)))
        (dolist (view (exc-etaf--views
                       (buffer-local-value 'exc--data-source buffer)))
          (unless (eq view buffer)
            (when (and file (equal (buffer-file-name view) file))
              (with-current-buffer view (set-buffer-modified-p t)))
            (unless exc-etaf--stale-views
              (run-at-time 0 nil #'exc-etaf--draw-stale-views))
            (cl-pushnew view exc-etaf--stale-views)))))))

(defun exc-etaf--saved ()
  "Mark the other buffers visiting the current buffer's file saved."
  (when buffer-file-name
    (let ((file buffer-file-name))
      (dolist (view (exc-etaf--views exc--data-source))
        (unless (eq view (current-buffer))
          (when (equal (buffer-file-name view) file)
            (with-current-buffer view
              (set-visited-file-modtime)
              (set-buffer-modified-p nil))))))))

(defun exc-etaf-new-view ()
  "Show the current sheet in another buffer and window, a second view.
Both show the same workbook: an edit in one is drawn in the other, and
either saves it."
  (interactive)
  (unless exc--data-source (user-error "No grid in this buffer"))
  (let ((source exc--data-source)
        (title exc--title)
        (file-path exc--file-path)
        (file buffer-file-name)
        (allow-elisp exc-allow-elisp)
        (modified (buffer-modified-p))
        (directory default-directory)
        (view (generate-new-buffer (buffer-name))))
    (with-current-buffer view
      (setq default-directory directory
            exc--file-path file-path
            exc-allow-elisp allow-elisp)
      (exc--show-grid-in-current-buffer source title)
      (when file
        (setq buffer-file-name file)
        (set-visited-file-modtime))
      (set-buffer-modified-p modified))
    (pop-to-buffer view)))

(defun exc-etaf--record (state record)
  "Push RECORD on STATE's undo history and drop its redo history."
  (setf (exc-etaf--state-undo state)
        (exc-etaf--trim-undo
         (cons record (exc-etaf--state-undo state)))
        (exc-etaf--state-redo state) nil)
  (exc-etaf--changed state))

(defun exc-etaf--as-one-step (state function)
  "Call FUNCTION and fold the undo records it leaves on STATE into one.
Return what FUNCTION returns."
  (let* ((before (exc-etaf--state-undo state))
         (value (funcall function))
         (after (exc-etaf--state-undo state))
         (added (cl-loop for tail on after until (eq tail before)
                         collect (car tail))))
    (when (cdr added)
      (setf (exc-etaf--state-undo state) before)
      ;; ADDED is newest first; a group replays oldest first.
      (exc-etaf--record state (cons 'group (nreverse added))))
    value))

(defun exc-etaf--write-cells (state changes)
  "Store CHANGES, a list of (ROW COL VALUE), in STATE's data source.
Return the list of (ROW COL OLD NEW) for the cells that changed.  Cells
that reject the value, such as the covered part of a merge, are skipped."
  (let ((source (exc-etaf--source state))
        applied)
    (cl-flet ((write ()
                (pcase-dolist (`(,row ,col ,value) changes)
                  (let ((old (or (exc-get-raw-value-at source row col) "")))
                    (unless (or (equal old value)
                                (exc-outside-p source row col))
                      (condition-case nil
                          (progn
                            (exc-set-raw-value-at source row col value)
                            (push (list row col old value) applied))
                        (exc-model-covered-cell nil)))))))
      ;; Many cells are recalculated once, after the last of them.
      (let ((model (gethash :model source)))
        (cond ((not (cdr changes)) (write))
              ((exc-workbook-of source)
               (exc-workbook-with-batched-edits (write)))
              (model
               (let ((exc-calc-defer t)) (write))
               (exc-calc-evaluate-models (list model)))
              (t (write)))))
    (nreverse applied)))

(defun exc-etaf--set-cells (state changes)
  "Store CHANGES, a list of (ROW COL VALUE), as one undoable step in STATE.
A formula among them whose values run past the sheet's last row or
column, with nothing on the sheet in the way, makes the sheet grow to
hold them, in the same step.  Return the number of cells that changed."
  (exc-etaf--as-one-step
   state
   (lambda ()
     ;; Writing past the last row or column grows the sheet first;
     ;; clearing a cell there changes nothing.
     (exc-etaf--grow-to-hold
      state (cl-remove-if (lambda (change) (equal (nth 2 change) "")) changes))
     (let ((applied (exc-etaf--write-cells state changes)))
       (when applied
         (setq exc-etaf--traces nil)
         (exc-etaf--record state (cons 'cells applied))
         (exc-etaf--grow-for-spills state applied))
       (exc-etaf--widen-visible state)
       (exc-etaf--bump state)
       (exc-etaf--settle state)
       (length applied)))))

(defun exc-etaf--grow-to-hold (state cells)
  "Grow STATE's sheet to hold CELLS, a list of (ROW COL ...), when they lie past it.
A spreadsheet's cursor writes on the empty cells past the last row and
column: the sheet then takes in the rows and columns up to them, in the
step the writing is.  Return non-nil when the sheet grew."
  (let* ((source (exc-etaf--source state))
         (rows (exc-get-row-count source))
         (cols (exc-get-column-count source))
         (more-rows 0) (more-cols 0))
    (dolist (cell cells)
      (setq more-rows (max more-rows (- (1+ (nth 0 cell)) rows))
            more-cols (max more-cols (- (1+ (nth 1 cell)) cols))))
    (when (or (> more-rows 0) (> more-cols 0))
      (let ((grow (gethash :grow source)))
        (unless grow
          (user-error "This grid cannot grow past its last row or column"))
        (funcall grow source more-rows more-cols)
        (exc-etaf--track-model state)
        (exc-etaf--apply-filters state)
        (exc-etaf--fit-widths state)
        t))))

(defun exc-etaf--grow-for-spills (state applied)
  "Grow STATE's sheet for the formulas APPLIED wrote that spill past it.
APPLIED is a list of (ROW COL OLD NEW).  Only a formula whose #SPILL! is
for the sheet's edge alone counts, as Excel's sheet has no such edge
short of its last row and column; one blocked by a cell stays #SPILL!.
Return non-nil when the sheet grew."
  (let* ((source (exc-etaf--source state))
         (model (gethash :model source))
         (grow (gethash :grow source))
         (rows 0) (cols 0))
    (when (and model grow)
      (pcase-dolist (`(,row ,col ,_old ,new) applied)
        (when (and (stringp new) (string-prefix-p "=" new))
          (when-let* ((more (exc-calc-spill-overflow model row col)))
            (setq rows (max rows (car more))
                  cols (max cols (cdr more))))))
      (when (or (> rows 0) (> cols 0))
        (funcall grow source rows cols)
        (exc-etaf--track-model state)
        (exc-etaf--apply-filters state)
        (exc-etaf--fit-widths state)
        t))))

(defun exc-etaf--enter-cells (state changes)
  "Store CHANGES, a list of (ROW COL TEXT), as a user types them.
A date or a percentage is stored as its number and gives a cell without
a format the one that shows it as typed.  The whole is one undoable step
in STATE.  Return the number of cells that changed."
  (let ((source (exc-etaf--source state))
        values styles)
    (pcase-dolist (`(,row ,col ,text) changes)
      (let ((entry (exc-read-entry source row col text)))
        (push (list row col (car entry)) values)
        (unless (eq (cdr entry) 'keep)
          (push (list row col (cdr entry)) styles))))
    (exc-etaf--as-one-step
     state
     (lambda ()
       ;; The format first, so the value is fitted as it will be shown.
       (when styles
         (exc-etaf--set-styles state (nreverse styles)))
       (exc-etaf--set-cells state (nreverse values))))))

(defun exc-etaf--set-styles (state changes)
  "Give cells their styles from CHANGES, a list of (ROW COL STYLE).
The change is one undoable step in STATE.  Nothing happens when the data
source stores no styles.  Return the number of cells that changed."
  (let ((source (exc-etaf--source state))
        applied)
    (when (gethash :set-style-at source)
      ;; A style past the last row or column grows the sheet to hold it.
      (exc-etaf--grow-to-hold
       state (cl-remove-if-not (lambda (change) (nth 2 change)) changes))
      (pcase-dolist (`(,row ,col ,style) changes)
        (let ((old (exc-get-style-at source row col)))
          (unless (or (equal old style)
                      (exc-outside-p source row col))
            (exc-set-style-at source row col style)
            (push (list row col old style) applied)))))
    (when applied
      (exc-etaf--record state (cons 'styles (nreverse applied)))
      (exc-etaf--widen-visible state)
      (exc-etaf--bump state)
      (exc-etaf--settle state))
    (length applied)))

(defun exc-etaf--track-model (state)
  "Record a structural change of STATE's data source since the last look.
Adding, deleting, sorting and merging replace the source's model, so the
previous model is all that undoing them needs."
  (let* ((source (exc-etaf--source state))
         (model (gethash :model source))
         (known (exc-etaf--state-model state)))
    (when (and model known (not (eq model known)))
      (setq exc-etaf--traces nil)
      (setf (exc-etaf--state-manual state) nil)
      (exc-etaf--forget-moved-filters state known model)
      (exc-etaf--record state (list 'model known model)))
    (setf (exc-etaf--state-model state) model)))

(defun exc-etaf--forget-moved-filters (state old new)
  "Drop STATE's filters when model NEW has other columns than OLD.
A filter names its column by index, which an inserted or deleted column
shifts."
  (unless (= (exc-data-model-cols old) (exc-data-model-cols new))
    (setf (exc-etaf--state-filters state) nil)))

(defun exc-etaf--replay (state record forward)
  "Apply undo RECORD to STATE: its new side when FORWARD, else its old side."
  (let ((source (exc-etaf--source state)))
    (pcase record
      (`(group . ,records)
       (dolist (part (if forward records (reverse records)))
         (exc-etaf--replay state part forward)))
      (`(cells . ,cells)
       (setq exc-etaf--traces nil)
       (exc-etaf--write-cells
        state
        (mapcar (lambda (cell)
                  (list (nth 0 cell) (nth 1 cell)
                        (nth (if forward 3 2) cell)))
                (if forward cells (reverse cells))))
       (exc-etaf--widen-visible state)
       (exc-etaf--bump state)
       (exc-etaf-goto (nth 0 (car cells)) (nth 1 (car cells))))
      (`(styles . ,cells)
       (dolist (cell cells)
         (exc-set-style-at source (nth 0 cell) (nth 1 cell)
                            (nth (if forward 3 2) cell)))
       (exc-etaf--bump state)
       (exc-etaf--settle state))
      (`(width ,col ,old ,new)
       (exc-etaf--restore-column-width state col (if forward new old)))
      (`(names ,workbook ,old ,new)
       (exc-workbook-set-names workbook (if forward new old))
       (exc-etaf--bump state))
      (`(validations ,old ,new)
       (exc-set-validations source (if forward new old))
       (exc-etaf--bump state))
      (`(charts ,old ,new)
       (exc-set-charts source (if forward new old))
       (setq exc-etaf--chart nil)
       (exc-etaf--bump state))
      (`(hidden ,old ,new)
       (let ((hidden (if forward new old)))
         (exc-set-hidden source (car hidden) (cdr hidden)))
       (exc-etaf--apply-filters state)
       (exc-etaf--bump state)
       (exc-etaf--settle state))
      (`(frozen ,old ,new)
       (let ((frozen (if forward new old)))
         (exc-set-frozen source (car frozen) (cdr frozen)))
       (exc-etaf--bump state)
       (exc-etaf--settle state))
      (`(conditionals ,old ,new)
       (exc-set-conditional-formats source (if forward new old))
       (exc-etaf--bump state)
       (exc-etaf--settle state))
      (`(model ,old ,new)
       (let ((model (if forward new old))
             (cursor (etaf-value (exc-etaf--state-cursor state))))
         (puthash :model model source)
         (exc-workbook-model-restored source (if forward old new) model)
         ;; Dragged widths belong to the columns of the model they were set on.
         (exc-etaf--forget-moved-filters
          state (if forward old new) model)
         (setf (exc-etaf--state-model state) model
               (exc-etaf--state-manual state) nil)
         (exc-etaf--apply-filters state)
         (exc-etaf--fit-widths state)
         (exc-etaf--bump state)
         (exc-etaf-goto (car cursor) (cdr cursor)))))))

(defun exc-etaf-undo ()
  "Undo the last change to the grid."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (record (car (exc-etaf--state-undo state))))
    (unless record (user-error "Nothing to undo"))
    (exc-etaf--end-session state)
    (pop (exc-etaf--state-undo state))
    (push record (exc-etaf--state-redo state))
    (exc-etaf--changed state)
    (exc-etaf--replay state record nil)))

(defun exc-etaf-redo ()
  "Redo the change undone last."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (record (car (exc-etaf--state-redo state))))
    (unless record (user-error "Nothing to redo"))
    (exc-etaf--end-session state)
    (pop (exc-etaf--state-redo state))
    (push record (exc-etaf--state-undo state))
    (exc-etaf--changed state)
    (exc-etaf--replay state record t)))

(defun exc-etaf-merge ()
  "Merge the selected range into one cell, or ask for a range to merge.
Only the top left cell of the range may hold a value."
  (interactive)
  (let ((range (exc-etaf-selection))
        (state (exc-etaf--state-or-error)))
    (if (and (= (nth 0 range) (nth 2 range)) (= (nth 1 range) (nth 3 range)))
        (call-interactively #'exc-merge-cells)
      ;; A range reaching past the last row or column grows the sheet
      ;; to hold it, in the step the merge is.
      (exc-etaf--as-one-step
       state
       (lambda ()
         (exc-etaf--grow-to-hold
          state (list (list (nth 2 range) (nth 3 range))))
         (exc-merge-cells
          (format "%s:%s"
                  (exc-etaf--address (nth 0 range) (nth 1 range))
                  (exc-etaf--address (nth 2 range) (nth 3 range)))))))))

;;;----------------------------------------------------------------------
;;; Formatting
;;;----------------------------------------------------------------------

(defun exc-etaf--plist-without (plist key)
  "Return PLIST without KEY."
  (cl-loop for (name value) on plist by #'cddr
           unless (eq name key) append (list name value)))

(defun exc-etaf--restyle (function)
  "Give every cell of the selection the style FUNCTION makes of its own.
FUNCTION receives a cell's style plist and returns the new one.  The
change is one undoable step.  Covered cells of a merge are skipped."
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (merges (exc-get-merges source))
         changed)
    (unless (gethash :set-style-at source)
      (user-error "This grid's data source does not store cell styles"))
    (cl-loop
     for row from (nth 0 range) to (nth 2 range)
     when (exc-etaf--row-shown-p state row) do
     (cl-loop
      for col from (nth 1 range) to (nth 3 range)
      when (exc-etaf--col-shown-p state col) do
      (let ((merge (and merges (exc--merge-at merges row col))))
        (when (or (null merge)
                  (and (= row (nth 0 merge)) (= col (nth 1 merge))))
          (let* ((old (exc-get-style-at source row col))
                 (new (funcall function (copy-sequence old))))
            (unless (equal old new)
              (push (list row col old new) changed)))))))
    (setq changed (nreverse changed))
    (exc-etaf--as-one-step
     state
     (lambda ()
       ;; A style past the last row or column grows the sheet to hold it.
       (exc-etaf--grow-to-hold
        state (cl-remove-if-not (lambda (change) (nth 3 change)) changed))
       (setq changed
             (cl-loop for change in changed
                      for (row col _old new) = change
                      unless (exc-outside-p source row col)
                      do (exc-set-style-at source row col new)
                      and collect change))
       (when changed
         (exc-etaf--record state (cons 'styles changed))
         (exc-etaf--widen-visible state))))
    (exc-etaf--bump state)
    (exc-etaf--settle state)
    (length changed)))

(defun exc-etaf--set-format (key value)
  "Set style property KEY to VALUE on the selection; nil removes it."
  (exc-etaf--restyle
   (lambda (style)
     (if value
         (plist-put style key value)
       (exc-etaf--plist-without style key)))))

(defun exc-etaf--toggle-format (key)
  "Toggle style property KEY on the selection, following the cursor cell."
  (let* ((state (exc-etaf--state-or-error))
         (cursor (exc-etaf--cursor))
         (extent (exc-etaf--cell-extent (car cursor) (cdr cursor)))
         (on (plist-get (exc-get-style-at (exc-etaf--source state)
                                           (nth 0 extent) (nth 1 extent))
                        key)))
    (exc-etaf--set-format key (not on))))

(defun exc-etaf-toggle-bold ()
  "Make the selection bold, or plain when the cursor cell is bold."
  (interactive)
  (exc-etaf--toggle-format :bold))

(defun exc-etaf-toggle-italic ()
  "Make the selection italic, or upright when the cursor cell is italic."
  (interactive)
  (exc-etaf--toggle-format :italic))

(defun exc-etaf--color (name)
  "Return color NAME as #RRGGBB when the display can resolve it."
  (cond ((or (null name) (string-empty-p name)) nil)
        ((string-prefix-p "#" name) name)
        (t (let ((rgb (ignore-errors (color-name-to-rgb name))))
             (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) name)))))

(defun exc-etaf-set-text-color (color)
  "Paint the text of the selection in COLOR; empty input removes the color."
  (interactive (list (read-color "Text color (empty to clear): ")))
  (exc-etaf--set-format :color (exc-etaf--color color)))

(defun exc-etaf-set-fill-color (color)
  "Fill the cells of the selection with COLOR; empty input removes the fill."
  (interactive (list (read-color "Fill color (empty to clear): ")))
  (exc-etaf--set-format :background (exc-etaf--color color)))

(defun exc-etaf-align-left ()
  "Align the selection to the left."
  (interactive)
  (exc-etaf--set-format :align 'left))

(defun exc-etaf-align-center ()
  "Center the selection."
  (interactive)
  (exc-etaf--set-format :align 'center))

(defun exc-etaf-align-right ()
  "Align the selection to the right."
  (interactive)
  (exc-etaf--set-format :align 'right))

(defun exc-etaf--cursor-format ()
  "Return the format the cursor cell is shown with."
  (let* ((cursor (exc-etaf--cursor))
         (extent (exc-etaf--cell-extent (car cursor) (cdr cursor))))
    (exc-cell-format exc--data-source
                            (nth 0 extent) (nth 1 extent))))

(defun exc-etaf-format-general ()
  "Show the selection in the general format.
A date formula then shows the number behind its date."
  (interactive)
  (exc-etaf--set-format :format (list 'general)))

(defun exc-etaf-format-number (decimals)
  "Show the selection as numbers with DECIMALS decimal places.
The whole part is grouped in thousands."
  (interactive (list (read-number "Decimal places: " 2)))
  (exc-etaf--set-format
   :format (list 'number (max 0 (min 12 (truncate decimals))) t)))

(defun exc-etaf-format-percent ()
  "Show the selection as percentages, so 0.125 reads 12.5%."
  (interactive)
  (exc-etaf--set-format
   :format (list 'percent
                 (or (exc-format-decimals (exc-etaf--cursor-format))
                     0))))

(defun exc-etaf-format-currency (symbol)
  "Show the selection as amounts of money led by SYMBOL."
  (interactive
   (list (read-string (format "Currency symbol (default %s): "
                              exc-currency-symbol)
                      nil nil exc-currency-symbol)))
  (exc-etaf--set-format :format (list 'currency 2 symbol)))

(defun exc-etaf-format-date (pattern)
  "Show the numbers of the selection as dates written after PATTERN.
PATTERN is made of yyyy, yy, mm, m, dd, d, hh, h and ss."
  (interactive
   (list (completing-read "Date format (default yyyy-mm-dd): "
                          exc-format-date-patterns nil nil nil nil
                          "yyyy-mm-dd")))
  (exc-etaf--set-format :format (list 'date pattern)))

(defun exc-etaf-format-text ()
  "Keep the selection as text: what is typed there is never a number."
  (interactive)
  (exc-etaf--set-format :format (list 'text)))

(defun exc-etaf--change-decimals (delta)
  "Show the selection with DELTA more decimal places than the cursor cell."
  (let* ((format (exc-etaf--cursor-format))
         (decimals (or (exc-format-decimals format) 0)))
    (exc-etaf--restyle
     (lambda (style)
       (plist-put style :format
                  (exc-format-with-decimals
                   (if (exc-format-decimals (plist-get style :format))
                       (plist-get style :format)
                     format)
                   (+ decimals delta)))))))

(defun exc-etaf-more-decimals ()
  "Show one more decimal place in the selection."
  (interactive)
  (exc-etaf--change-decimals 1))

(defun exc-etaf-fewer-decimals ()
  "Show one decimal place less in the selection."
  (interactive)
  (exc-etaf--change-decimals -1))

(defconst exc-etaf--content-keys '(:note :link)
  "Style properties that belong to a cell's content, not its look.
Clearing formats and pasting only formats leave them alone.")

(defun exc-etaf--content-of (style)
  "Return the part of STYLE that `exc-etaf--content-keys' name."
  (cl-loop for (name value) on style by #'cddr
           when (memq name exc-etaf--content-keys)
           append (list name value)))

(defun exc-etaf-clear-format ()
  "Remove every style from the selection, but not notes and links."
  (interactive)
  (exc-etaf--restyle #'exc-etaf--content-of))

;;;----------------------------------------------------------------------
;;; Notes and links
;;;----------------------------------------------------------------------

(defun exc-etaf--cursor-cell ()
  "Return (ROW . COL) of the cursor cell, the top left one of a merge."
  (let* ((cursor (exc-etaf--cursor))
         (extent (exc-etaf--cell-extent (car cursor) (cdr cursor))))
    (cons (nth 0 extent) (nth 1 extent))))

(defun exc-etaf--cursor-style ()
  "Return the style of the cursor cell."
  (let ((cell (exc-etaf--cursor-cell)))
    (exc-get-style-at (exc-etaf--source (exc-etaf--state-or-error))
                       (car cell) (cdr cell))))

(defun exc-etaf-edit-note (note)
  "Give the cursor cell NOTE, a remark shown when the cursor is on it.
An empty NOTE removes it.  The cell's right edge shows that it has one,
and the status bar shows the note."
  (interactive
   (list (read-string "Note (empty removes it): "
                      (plist-get (exc-etaf--cursor-style) :note))))
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (cell (exc-etaf--cursor-cell))
         (style (copy-sequence (exc-get-style-at source (car cell) (cdr cell))))
         (note (string-trim note)))
    (unless (gethash :set-style-at source)
      (user-error "This grid's data source does not store notes"))
    (exc-etaf--set-styles
     state (list (list (car cell) (cdr cell)
                       (if (string-empty-p note)
                           (exc-etaf--plist-without style :note)
                         (plist-put style :note note)))))))

(defun exc-etaf-delete-note ()
  "Remove the notes of the selected cells."
  (interactive)
  (exc-etaf--set-format :note nil))

(defun exc-etaf-set-link (link)
  "Make the selected cells link to LINK; an empty LINK removes the link.
LINK is a web address, a file name, or #Sheet2!A1 for a cell of this
workbook.  `exc-etaf-open-link' follows it."
  (interactive
   (list (read-string "Link (URL, file or #A1; empty removes it): "
                      (plist-get (exc-etaf--cursor-style) :link))))
  (let ((link (string-trim link)))
    (exc-etaf--set-format :link (and (not (string-empty-p link)) link))))

(defun exc-etaf-open-link ()
  "Follow the link of the cursor cell.
A web address opens in the browser, #ADDRESS goes to that cell or range
of the workbook, and anything else is visited as a file, relative to
the grid's own."
  (interactive)
  (let ((link (plist-get (exc-etaf--cursor-style) :link)))
    (cond
     ((null link) (user-error "This cell has no link"))
     ((string-prefix-p "#" link)
      (exc-etaf-goto-address (substring link 1)))
     ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" link)
      (browse-url link))
     (t (find-file (expand-file-name link))))))

(defun exc-etaf--set-conditionals (rules)
  "Replace the grid's conditional format rules with RULES, undoably."
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (old (exc-get-conditional-formats source)))
    (unless (gethash :set-conditional-formats source)
      (user-error "This grid's data source does not store conditional formats"))
    (unless (equal old rules)
      (exc-set-conditional-formats source rules)
      (exc-etaf--record state (list 'conditionals old rules)))
    (exc-etaf--bump state)
    (exc-etaf--settle state)))

(defun exc-etaf-add-conditional-format (op value style)
  "Show the cells of the selection that satisfy OP against VALUE in STYLE.
OP is one of `exc-conditional-operators' and STYLE a style
plist.  The rule keeps applying as values change."
  (interactive
   (let* ((choices (mapcar (lambda (entry) (cons (cdr entry) (car entry)))
                           exc-conditional-operators))
          (op (cdr (assoc (completing-read "Format cells whose value: "
                                           choices nil t)
                          choices)))
          (value (if (member op '("empty" "error"))
                     ""
                   (read-string "Value: ")))
          (fill (exc-etaf--color
                 (read-color "Fill color (empty for none): ")))
          (color (exc-etaf--color
                  (read-color "Text color (empty for none): ")))
          (bold (y-or-n-p "Bold? ")))
     (list op value
           (append (and fill (list :background fill))
                   (and color (list :color color))
                   (and bold (list :bold t))))))
  (unless (assoc op exc-conditional-operators)
    (user-error "Unknown condition: %s" op))
  (unless style
    (user-error "A conditional format needs a fill, a text color or bold"))
  (let ((range (exc-etaf-selection)))
    (exc-etaf--set-conditionals
     (append (exc-get-conditional-formats
              (exc-etaf--source (exc-etaf--state-or-error)))
             (list (append range (list op value style)))))))

(defun exc-etaf-clear-conditional-formats ()
  "Remove the conditional format rules that touch the selection."
  (interactive)
  (let ((range (exc-etaf-selection)))
    (exc-etaf--set-conditionals
     (cl-remove-if
      (lambda (rule)
        (and (<= (nth 0 rule) (nth 2 range)) (<= (nth 0 range) (nth 2 rule))
             (<= (nth 1 rule) (nth 3 range)) (<= (nth 1 range) (nth 3 rule))))
      (exc-get-conditional-formats
       (exc-etaf--source (exc-etaf--state-or-error)))))))

;;;----------------------------------------------------------------------
;;; Clipboard
;;;----------------------------------------------------------------------

(defvar exc-etaf--clipboard nil
  "The last copied range as (TEXT ROWS TOP LEFT CUT STYLES).
TEXT is what went to the kill ring and ROWS the raw values, formulas
included, copied from model row TOP and column LEFT.  Pasting uses ROWS
while TEXT is still the current kill.  CUT is non-nil when the range was
cut, so its formulas move without being adjusted.  STYLES holds the
cells' styles, shaped like ROWS.")

(defun exc-etaf--tsv-field (text)
  "Return TEXT as one tab-separated field, quoted when it needs to be."
  (if (string-match-p "[\t\n\"]" text)
      (concat "\"" (string-replace "\"" "\"\"" text) "\"")
    text))

(defun exc-etaf--tsv-encode (rows)
  "Return ROWS, a list of lists of strings, as tab-separated text."
  (mapconcat (lambda (row)
               (mapconcat #'exc-etaf--tsv-field row "\t"))
             rows "\n"))

(defun exc-etaf--tsv-decode (text)
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

(defun exc-etaf--range-rows (state source range reader)
  "Return the cells of RANGE in SOURCE as rows of what READER returns.
Rows hidden by STATE's filters are left out."
  (cl-loop for row from (nth 0 range) to (nth 2 range)
           when (exc-etaf--row-shown-p state row) collect
           (cl-loop for col from (nth 1 range) to (nth 3 range)
                    when (exc-etaf--col-shown-p state col) collect
                    (funcall reader source row col))))

(defun exc-etaf-copy ()
  "Copy the selected range.
The kill ring receives the shown values as tab-separated text; pasting
back into a grid restores the raw values, formulas included, and the
cells' formats."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (text (exc-etaf--tsv-encode
                (exc-etaf--range-rows
                 state source range
                 (lambda (source row col)
                   (let ((value (exc-get-computed-value-at source row col)))
                     (if (exc-false-value-p source row col value)
                         "FALSE"
                       (exc-etaf--display-text
                        value (exc-cell-format source row col)))))))))
    (kill-new text)
    (setq exc-etaf--clipboard
          (list text
                (exc-etaf--range-rows
                 state source range
                 (lambda (source row col)
                   (or (exc-get-raw-value-at source row col) "")))
                (nth 0 range) (nth 1 range) nil
                (exc-etaf--range-rows state source range
                                             #'exc-get-style-at)))
    (message "Copied %d × %d cells"
             (1+ (- (nth 2 range) (nth 0 range)))
             (1+ (- (nth 3 range) (nth 1 range))))))

(defun exc-etaf-cut ()
  "Copy the selected range and empty it, formats included."
  (interactive)
  (exc-etaf-copy)
  (setf (nth 4 exc-etaf--clipboard) t)
  (let ((state (exc-etaf--state-or-error))
        (range (exc-etaf-selection)))
    (exc-etaf--as-one-step
     state
     (lambda ()
       (exc-etaf-clear-cells)
       (exc-etaf--set-styles
        state
        (cl-loop for row from (nth 0 range) to (nth 2 range)
                 when (exc-etaf--row-shown-p state row) nconc
                 (cl-loop for col from (nth 1 range) to (nth 3 range)
                          when (exc-etaf--col-shown-p state col)
                          collect (list row col nil))))))))

(defun exc-etaf--shifted (value origin row col)
  "Return VALUE as it reads when its range moves from ORIGIN to ROW and COL.
ORIGIN is the (ROW . COL) the copied range started at, or nil when the
value should be pasted as it is."
  (if origin
      (exc-calc-shift-formula value (- row (car origin)) (- col (cdr origin)))
    value))

(defun exc-etaf--transpose (rows)
  "Return the matrix ROWS turned so that its rows become its columns.
Short rows are padded with nil."
  (let ((width (apply #'max 0 (mapcar #'length rows))))
    (cl-loop for col below width collect
             (mapcar (lambda (row) (nth col row)) rows))))

(defun exc-etaf--paste-material (own text mode top left)
  "Return (ROWS STYLES ORIGIN OWN) for pasting at TOP and LEFT in MODE.
OWN is the grid's own clipboard entry when it holds TEXT, else nil.
MODE nil pastes values, formulas and formats; `values' pastes what the
copied cells show, as if typed; `formats' only formats; `transpose'
turns rows into columns.  ROWS are the values, STYLES the formats or
nil, ORIGIN the cell to move formula references from, or nil; OWN is
nil when the values are to be read as typed."
  (let ((rows (if own (nth 1 own) (exc-etaf--tsv-decode text)))
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
                     (exc-etaf--tsv-decode (car own)) rows)))
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
                                 (exc-calc-shift-formula
                                  value
                                  (- (+ top j) (+ (car origin) i))
                                  (- (+ left i) (+ (cdr origin) j)))))))
       (list (mapcar (lambda (row) (mapcar (lambda (v) (or v "")) row))
                     (exc-etaf--transpose rows))
             (and styles (exc-etaf--transpose styles))
             nil own))
      (_ (list rows styles origin own)))))

(defun exc-etaf-paste-values ()
  "Paste what the copied cells show, without their formulas or formats."
  (interactive)
  (exc-etaf-paste 'values))

(defun exc-etaf-paste-formats ()
  "Paste only the formats of the copied cells."
  (interactive)
  (exc-etaf-paste 'formats))

(defun exc-etaf-paste-transposed ()
  "Paste the copied cells with their rows turned into columns."
  (interactive)
  (exc-etaf-paste 'transpose))

(defun exc-etaf-paste (&optional mode)
  "Paste the current kill at the selected range.
The top left cell of the range receives the first value.  A single
copied cell fills the whole range.  The grid grows to take values that
reach past its last row or column; a data source that cannot grow drops
them.  Copied formulas have their relative references moved
by the distance they travel, as in a spreadsheet; cut formulas do not.
Cells copied in a grid bring their formats; text from elsewhere leaves
the formats of the cells it lands in.  Rows hidden by a filter are
passed over: the pasted lines go to the rows that are shown.
MODE is as for `exc-etaf--paste-material'."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (text (substring-no-properties (current-kill 0)))
         (range (exc-etaf-selection))
         (top (nth 0 range))
         (left (nth 1 range))
         (material (exc-etaf--paste-material
                    (and exc-etaf--clipboard
                         (equal text (car exc-etaf--clipboard))
                         exc-etaf--clipboard)
                    text mode top left))
         (rows (nth 0 material))
         ;; Where a copied range came from, to move its references.
         (origin (nth 2 material))
         (own (nth 3 material))
         (single (and (null (cdr rows)) (null (cdr (car rows)))))
         (row-count (exc-get-row-count source))
         ;; The rows the pasted lines land in: rows hidden by a filter
         ;; are passed over, as copying and clearing pass over them.
         (targets (let ((row top) targets)
                    (while (< (length targets) (length rows))
                      (when (or (>= row row-count)
                                (exc-etaf--row-shown-p state row))
                        (push row targets))
                      (setq row (1+ row)))
                    (nreverse targets)))
         (col-count (exc-get-column-count source))
         ;; Hidden columns are passed over in the same way.
         (col-targets (let ((col left) targets
                            (width (apply #'max (mapcar #'length rows))))
                        (while (< (length targets) width)
                          (when (or (>= col col-count)
                                    (exc-etaf--col-shown-p state col))
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
                 when (exc-etaf--row-shown-p state row) do
                 (cl-loop for col from left to (nth 3 range)
                          when (exc-etaf--col-shown-p state col) do
                          (push (list row col
                                      (exc-etaf--shifted
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
                            (exc-etaf--shifted value origin top left))
                      changes)
                (when styled
                  (push (list row col
                              (nth column (nth line styled)))
                        styles)))))
    ;; The message counts the cells pasted, as many as were copied,
    ;; whether or not they held the same before.
    (let ((pasted (length changes))
          (_changed (exc-etaf--as-one-step
                  state
                  (lambda ()
                    (when (or (> more-rows 0) (> more-cols 0))
                      (funcall (gethash :grow source) source
                               more-rows more-cols)
                      (exc-etaf--track-model state)
                      (exc-etaf--apply-filters state)
                      (exc-etaf--fit-widths state))
                    (if own
                        (prog1 (exc-etaf--set-cells
                                state (if (eq mode 'formats)
                                          nil
                                        (nreverse changes)))
                          (exc-etaf--set-styles
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
                                                  unless (memq name exc-etaf--content-keys)
                                                  append (list name value))
                                         (exc-etaf--content-of
                                          (exc-get-style-at source row col)))))
                                (nreverse styles))
                             (nreverse styles))))
                      ;; Text from elsewhere is read as if it were typed.
                      (exc-etaf--enter-cells
                       state (nreverse changes)))))))
      (exc-etaf-goto bottom right)
      (exc-etaf-goto top left t)
      (if (eq mode 'formats)
          (message "Pasted formats")
        (message "Pasted %d cell%s" pasted (if (= pasted 1) "" "s"))))))

;;;----------------------------------------------------------------------
;;; Hiding rows and columns
;;;----------------------------------------------------------------------

(defun exc-etaf--set-hidden (state rows cols)
  "Hide ROWS and COLS of STATE's sheet, as one undoable step."
  (let* ((source (exc-etaf--source state))
         (old (exc-hidden source)))
    (unless (exc-set-hidden source rows cols)
      (user-error "This grid cannot hide rows and columns"))
    (let ((new (exc-hidden source)))
      (unless (equal old new)
        (exc-etaf--record state (list 'hidden old new))))
    (exc-etaf--apply-filters state)
    (exc-etaf--bump state)
    ;; The cursor leaves a line that was just hidden.
    (let ((cursor (exc-etaf--cursor)))
      (exc-etaf-goto (car cursor) (cdr cursor)))))

(defun exc-etaf-hide-rows ()
  "Hide the rows of the selection.
They keep their data, are saved hidden, and are passed over by movement,
copying, clearing and pasting.  `exc-etaf-show-rows' shows them."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (hidden (exc-hidden source))
         ;; Rows a filter hides are left to the filter.
         (rows (append (car hidden)
                       (cl-remove-if-not
                        (lambda (row) (exc-etaf--row-shown-p state row))
                        (number-sequence (nth 0 range) (nth 2 range))))))
    (when (cl-every (lambda (row)
                      (or (memq row rows)
                          (not (exc-etaf--row-shown-p state row))))
                    (number-sequence 0 (1- (exc-get-row-count source))))
      (user-error "At least one row has to stay shown"))
    (exc-etaf--set-hidden state rows (cdr hidden))))

(defun exc-etaf-show-rows ()
  "Show the hidden rows inside the selection.
Select across the place a row is missing from, or the whole sheet with
\\[exc-etaf-select-all], to show them."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (hidden (exc-hidden source)))
    (exc-etaf--set-hidden
     state
     (cl-remove-if (lambda (row) (<= (nth 0 range) row (nth 2 range)))
                   (car hidden))
     (cdr hidden))))

(defun exc-etaf-hide-columns ()
  "Hide the columns of the selection.
They keep their data, are saved hidden, and are passed over like hidden
rows.  `exc-etaf-show-columns' shows them."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (hidden (exc-hidden source))
         (cols (append (cdr hidden)
                       (number-sequence (nth 1 range) (nth 3 range)))))
    (when (cl-every (lambda (col) (memq col cols))
                    (number-sequence 0 (1- (exc-get-column-count source))))
      (user-error "At least one column has to stay shown"))
    (exc-etaf--set-hidden state (car hidden) cols)))

(defun exc-etaf-show-columns ()
  "Show the hidden columns inside the selection."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (hidden (exc-hidden source)))
    (exc-etaf--set-hidden
     state
     (car hidden)
     (cl-remove-if (lambda (col) (<= (nth 1 range) col (nth 3 range)))
                   (cdr hidden)))))

;;;----------------------------------------------------------------------
;;; Removing duplicates
;;;----------------------------------------------------------------------

(defun exc-etaf--duplicate-rows (source rows cols)
  "Return the ROWS of SOURCE that repeat an earlier one in the COLS.
Values compare as they are shown, ignoring case."
  (let ((seen (make-hash-table :test 'equal))
        duplicates)
    (dolist (row rows)
      (let ((key (mapcar (lambda (col)
                           (downcase (exc-etaf--copy-text source row col)))
                         cols)))
        (if (gethash key seen)
            (push row duplicates)
          (puthash key t seen))))
    (nreverse duplicates)))

(defun exc-etaf--copy-text (source row col)
  "Return the text the cell at ROW and COL of SOURCE shows."
  (let ((value (exc-get-computed-value-at source row col)))
    (if (exc-false-value-p source row col value)
        "FALSE"
      (exc-etaf--display-text
       value (exc-cell-format source row col)))))

(defun exc-etaf-remove-duplicates ()
  "Delete the rows that repeat an earlier row in the selected columns.
With one cell selected, whole rows of the sheet are compared; the header
row is never deleted.  The first of equal rows stays.  Formulas move
with the rows that stay, and references to the deleted rows become
#REF!; the whole is one step to undo."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (whole (and (= (nth 0 range) (nth 2 range))
                     (= (nth 1 range) (nth 3 range))))
         (first (max (if (exc-header-row-p source) 1 0)
                     (if whole 0 (nth 0 range))))
         ;; The empty rows and columns past the sheet's edge hold
         ;; nothing to repeat.
         (last (if whole (1- (exc-get-row-count source))
                 (min (nth 2 range) (1- (exc-get-row-count source)))))
         (cols (if whole
                   (number-sequence 0 (1- (exc-get-column-count source)))
                 (number-sequence (nth 1 range)
                                  (min (nth 3 range)
                                       (1- (exc-get-column-count source))))))
         (duplicates (and (<= first last) cols
                          (exc-etaf--duplicate-rows
                           source (number-sequence first last) cols)))
         (delete-all (gethash :delete-rows source))
         (delete (gethash :delete-row source)))
    (unless (or delete-all delete)
      (user-error "This grid cannot delete rows"))
    (if (null duplicates)
        (message "No duplicate rows")
      (exc-etaf--as-one-step
       state
       (lambda ()
         (if delete-all
             (progn (funcall delete-all source duplicates)
                    (exc-etaf--track-model state))
           (exc-workbook-with-held-recalculation
             ;; From the bottom, so the rows still to go keep their place.
             (dolist (row (reverse duplicates))
               ;; The row argument counts from the row under the first.
               (funcall delete source (1- row))
               (exc-etaf--track-model state)))
           (unless (exc-workbook-of source)
             (exc-recalculate source t)))))
      (exc-etaf--apply-filters state)
      (exc-etaf--fit-widths state)
      ;; The selection may reach past the rows left.
      (exc-etaf-goto (min (nth 0 range)
                                 (1- (exc-get-row-count source)))
                            (nth 1 range))
      (exc-etaf--bump state)
      (message "Removed %d duplicate row%s" (length duplicates)
               (if (cdr duplicates) "s" "")))))

;;;----------------------------------------------------------------------
;;; Validation
;;;----------------------------------------------------------------------

(defun exc-etaf--set-validations (state rules)
  "Give STATE's sheet the validation RULES as one undoable step."
  (let* ((source (exc-etaf--source state))
         (old (copy-tree (exc-validations source))))
    (unless (exc-set-validations source rules)
      (user-error "This grid's data source does not keep validation rules"))
    (exc-etaf--record state (list 'validations old (copy-tree rules)))
    (exc-etaf--bump state)))

(defun exc-etaf-set-validation-list (choices)
  "Let the selected cells take only CHOICES.
CHOICES is a comma-separated list, such as \"Yes, No\", or a formula
giving the values, such as =$F$2:$F$9 or =Sizes.  Editing such a cell
offers the choices; typing anything else is refused.  Pasting and
filling are not checked."
  (interactive
   (list (string-trim (read-string
                       "Allowed values (a, b, c or =range or =name): "))))
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (spec (cond ((string-empty-p choices)
                      (user-error "No values given"))
                     ((string-prefix-p "=" choices)
                      (condition-case nil
                          (progn (exc-parser-parse (substring choices 1))
                                 choices)
                        (error (user-error "Not a formula: %s" choices))))
                     (t (delete "" (mapcar #'string-trim
                                           (split-string choices ","))))))
         (rule (append (cl-subseq range 0 4) (list 'list spec))))
    (exc-etaf--set-validations
     state (append (exc-validations source) (list rule)))
    (message "Allowed: %s"
             (string-join (exc-validation-options source rule) ", "))))

(defun exc-etaf-clear-validations ()
  "Let the selected cells take any value again."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection)))
    (exc-etaf--set-validations
     state
     (cl-remove-if (lambda (rule)
                     (and (<= (nth 0 rule) (nth 2 range))
                          (<= (nth 0 range) (nth 2 rule))
                          (<= (nth 1 rule) (nth 3 range))
                          (<= (nth 1 range) (nth 3 rule))))
                   (exc-validations source)))))

;;;----------------------------------------------------------------------
;;; Defined names
;;;----------------------------------------------------------------------

(defun exc-etaf--set-names (state workbook names)
  "Give WORKBOOK the defined NAMES as one undoable step of STATE."
  (let ((old (copy-tree (exc-workbook-names workbook))))
    (exc-workbook-set-names workbook names)
    (exc-etaf--record state (list 'names workbook old
                                         (copy-tree names)))
    (exc-etaf--bump state)))

(defun exc-etaf--selection-reference ()
  "Return the selection as an absolute reference with its sheet, after =."
  (let* ((source (exc-etaf--source (exc-etaf--state-or-error)))
         (range (exc-etaf-selection))
         (cell (lambda (row col)
                 (format "$%s$%d" (exc-calc-col-number-to-letter col) (1+ row)))))
    (concat "=" (exc-calc-sheet-prefix (exc-workbook-sheet-name source))
            (funcall cell (nth 0 range) (nth 1 range))
            (unless (and (= (nth 0 range) (nth 2 range))
                         (= (nth 1 range) (nth 3 range)))
              (concat ":" (funcall cell (nth 2 range) (nth 3 range)))))))

(defun exc-etaf-define-name (name &optional definition)
  "Give the selected cell or range the defined NAME.
Formulas then read it by name, as =SUM(Sales) or =Price*Rate; the name
follows its cells when lines are inserted or deleted, and a name already
defined is moved to the selection.
With a prefix argument, or DEFINITION, the name stands for that formula
instead, as Excel's Refers to: =Net*(1+Rate) or =LAMBDA(x,x*2); the
selection is offered as the formula to start from."
  (interactive
   (let ((name (string-trim (read-string (if current-prefix-arg "Name: "
                                           "Name for the selection: ")))))
     (list name
           (and current-prefix-arg
                (read-string (format "%s refers to: " name)
                             (exc-etaf--selection-reference))))))
  (let* ((state (exc-etaf--state-or-error))
         (workbook (exc--workbook))
         (reference
          (if definition
              (let ((text (string-trim definition)))
                (concat "=" (string-remove-prefix "=" text)))
            (exc-etaf--selection-reference)))
         (names (copy-tree (exc-workbook-names workbook)))
         (entry (assoc-string name names t)))
    (when-let* ((problem (exc-calc-name-problem name)))
      (user-error "%s" problem))
    (when definition
      (when (string= reference "=")
        (user-error "A name needs a formula to stand for"))
      (condition-case err
          (exc-parser-parse (substring reference 1))
        (error (user-error "Not a formula: %s" (error-message-string err)))))
    (if entry
        (setcdr entry reference)
      (setq names (append names (list (cons name reference)))))
    (exc-etaf--set-names state workbook names)
    (message "%s is %s" name (substring reference 1))))

(defun exc-etaf--read-name (prompt)
  "Read with PROMPT a name defined in the current grid's workbook."
  (let ((names (mapcar #'car (and (exc-workbook-of exc--data-source)
                                  (exc-workbook-names
                                   (exc-workbook-of exc--data-source))))))
    (unless names
      (user-error "No names are defined; C-c n d names the selection"))
    (completing-read prompt names nil t)))

(defun exc-etaf-goto-name (name)
  "Select the cells the defined NAME stands for, showing their sheet."
  (interactive (list (exc-etaf--read-name "Go to name: ")))
  (exc-etaf-goto-address name))

(defun exc-etaf-delete-name (name)
  "Forget the defined NAME; formulas that use it show #NAME?."
  (interactive (list (exc-etaf--read-name "Delete name: ")))
  (let ((workbook (exc--workbook)))
    (exc-etaf--set-names
     (exc-etaf--state-or-error) workbook
     (cl-remove-if (lambda (entry) (string-equal-ignore-case (car entry) name))
                   (exc-workbook-names workbook)))))

;;;----------------------------------------------------------------------
;;; Tracing formulas
;;;----------------------------------------------------------------------

(defun exc-etaf--traced (source row col)
  "Return the kind of the trace painting the cell at ROW and COL of SOURCE."
  (cl-loop for trace in exc-etaf--traces
           when (and (eq (car trace) source)
                     (exc-calc-reference-covers-p trace row col))
           return (nth 5 trace)))

(defun exc-etaf--sheet-named (source name)
  "Return the sheet NAME of SOURCE's workbook; SOURCE itself when NAME is nil."
  (if (null name)
      source
    (when-let* ((workbook (exc-workbook-of source)))
      (exc-workbook-sheet workbook name))))

(defun exc-etaf--show-traces (traces kind)
  "Paint TRACES and report how many of KIND there are."
  (let* ((source exc--data-source)
         (here (cl-count source traces :key #'car)))
    (setq exc-etaf--traces traces
          exc-etaf--next-trace 0)
    (exc-etaf--bump (exc-etaf--state-or-error))
    (message (if (null traces)
                 (format "No %ss" kind)
               (format "%d %s%s%s; C-c a n visits them, C-c a x clears"
                       (length traces) kind (if (cdr traces) "s" "")
                       (if (< here (length traces))
                           (format " (%d on other sheets)"
                                   (- (length traces) here))
                         ""))))))

(defun exc-etaf-trace-precedents ()
  "Paint the cells and ranges the formula under the cursor reads.
References into other sheets count, and `exc-etaf-next-trace'
goes there."
  (interactive)
  (let* ((source exc--data-source)
         (model (or (gethash :model source)
                    (user-error "This grid keeps no formulas")))
         (cursor (exc-etaf--cursor))
         (traces
          (delq nil
                (mapcar
                 (lambda (reference)
                   (let ((sheet (exc-etaf--sheet-named
                                 source (car reference))))
                     (and sheet
                          (append (list sheet) (cdr reference)
                                  '(precedent)))))
                 (let ((exc-calc-names
                        (and (exc-workbook-of source)
                             (exc-workbook--names-resolver
                              (exc-workbook-of source)))))
                   (exc-calc-precedents model (car cursor) (cdr cursor)))))))
    (unless (string-prefix-p
             "=" (or (exc-get-raw-value-at source (car cursor) (cdr cursor))
                     ""))
      (user-error "The cell holds no formula"))
    (exc-etaf--show-traces traces "precedent")))

(defun exc-etaf-trace-dependents ()
  "Paint the formulas that read the cell under the cursor.
Formulas on every sheet of the workbook are looked at."
  (interactive)
  (let* ((source exc--data-source)
         (cursor (exc-etaf--cursor))
         (row (car cursor)) (col (cdr cursor))
         traces)
    (dolist (sheet (or (exc-sheets) (list source)))
      (when-let* ((model (gethash :model sheet)))
        (let ((exc-calc-names
               (and (exc-workbook-of sheet)
                    (exc-workbook--names-resolver (exc-workbook-of sheet)))))
        (dotimes (r (exc-data-model-rows model))
          (dotimes (c (exc-data-model-cols model))
            (when (cl-some
                   (lambda (reference)
                     (and (eq (exc-etaf--sheet-named sheet (car reference))
                              source)
                          (exc-calc-reference-covers-p reference row col)))
                   (exc-calc-precedents model r c))
              (push (list sheet r c r c 'dependent) traces)))))))
    (exc-etaf--show-traces (nreverse traces) "dependent")))

(defun exc-etaf-next-trace ()
  "Go to the next traced cell or range, showing its sheet."
  (interactive)
  (unless exc-etaf--traces
    (user-error "Nothing is traced; C-c a p or C-c a d traces a cell"))
  (let* ((index (mod exc-etaf--next-trace
                     (length exc-etaf--traces)))
         (trace (nth index exc-etaf--traces)))
    (setq exc-etaf--next-trace (1+ index))
    (unless (memq (car trace) (or (exc-sheets) (list (car trace))))
      (user-error "The traced sheet is gone"))
    (exc-switch-to-sheet (car trace))
    (pcase-let* ((source exc--data-source)
                 (`(,_ ,top ,left ,bottom ,right) trace)
                 (last-row (1- (exc-get-row-count source)))
                 (last-col (1- (exc-get-column-count source))))
      (exc-etaf-goto (min last-row (or bottom last-row))
                            (min last-col (or right last-col)))
      (exc-etaf-goto (or top 0) (or left 0)
                            (not (and top left (eql top bottom)
                                      (eql left right)))))))

(defun exc-etaf-clear-traces ()
  "Stop painting traced cells."
  (interactive)
  (setq exc-etaf--traces nil)
  (exc-etaf--bump (exc-etaf--state-or-error)))

;;;----------------------------------------------------------------------
;;; Filling
;;;----------------------------------------------------------------------

(defun exc-etaf--fill (state changes styles)
  "Store CHANGES and STYLES, lists of (ROW COL VALUE), as one step in STATE.
Return the number of cells whose value changed."
  (exc-etaf--as-one-step
   state
   (lambda ()
     (prog1 (exc-etaf--set-cells state changes)
       (exc-etaf--set-styles state styles)))))

(defun exc-etaf--fill-from-edge (down)
  "Copy the first row of the selection down, or its first column right.
DOWN chooses the direction.  Formulas have their relative references
moved along, and the cells take the formats of the ones they copy."
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
         (top (nth 0 range)) (left (nth 1 range))
         (styled (gethash :set-style-at source))
         changes styles)
    (when (if down (= top (nth 2 range)) (= left (nth 3 range)))
      (user-error "Select the cells to fill, starting with the one to copy"))
    (cl-loop
     for row from top to (nth 2 range)
     when (exc-etaf--row-shown-p state row) do
     (cl-loop
      for col from left to (nth 3 range)
      when (exc-etaf--col-shown-p state col) do
      (let ((from-row (if down top row))
            (from-col (if down col left)))
        (unless (and (= row from-row) (= col from-col))
          (push (list row col
                      (exc-calc-shift-formula
                       (or (exc-get-raw-value-at source from-row from-col) "")
                       (- row from-row) (- col from-col)))
                changes)
          (when styled
            (push (list row col (exc-get-style-at source from-row from-col))
                  styles))))))
    (let ((count (exc-etaf--fill state (nreverse changes)
                                        (nreverse styles))))
      (message "Filled %d cell%s" count (if (= count 1) "" "s")))))

(defun exc-etaf-fill-down ()
  "Copy the first row of the selection into the rows below it."
  (interactive)
  (exc-etaf--fill-from-edge t))

(defun exc-etaf-fill-right ()
  "Copy the first column of the selection into the columns right of it."
  (interactive)
  (exc-etaf--fill-from-edge nil))

(defun exc-etaf--series (seeds count)
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
                      (exc-format-number last)))
         (before (and previous (exc-format-number previous))))
    (cond
     ((and number (or (null previous) before))
      (let ((step (if before (- number before) 1)))
        (cl-loop for k from 1 to count collect
                 (cons (exc-format-general
                        (let ((value (+ number (* k step))))
                          ;; Steps such as 0.1 would pile up binary noise.
                          (if (floatp value)
                              (/ (exc-format-round (* value 1e10)) 1e10)
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

(defun exc-etaf-fill-series ()
  "Continue the values that start the selection through the rest of it.
A selection several rows tall is filled down each of its columns; one
row is filled to the right.  The cells that start a line, up to its
first empty cell, are what the line continues: 1, 2 goes on 3, 4; a
single 5 goes on 6, 7; Jan 1 dates go on day by day; Week 1 goes on
Week 2; a formula is copied with its references moved along."
  (interactive)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (range (exc-etaf-selection))
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
                                when (exc-etaf--row-shown-p state row)
                                collect (cons row line))
                     (cl-loop for col from (nth 1 range) to (nth 3 range)
                              when (exc-etaf--col-shown-p state col)
                              collect (cons line col))))
            (raws (mapcar (lambda (cell)
                            (or (exc-get-raw-value-at source
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
            for (raw . index) in (exc-etaf--series seeds
                                                          (length targets))
            do
            (let ((seed (nth index cells)))
              (push (list (car target) (cdr target)
                          (if (string-prefix-p "=" raw)
                              (exc-calc-shift-formula
                               raw (- (car target) (car seed))
                               (- (cdr target) (cdr seed)))
                            raw))
                    changes)
              (when styled
                (push (list (car target) (cdr target)
                            (exc-get-style-at source (car first) (cdr first)))
                      styles))))))))
    (unless changes
      (user-error "Nothing to continue: the selection starts with empty cells"))
    (let ((count (exc-etaf--fill state (nreverse changes)
                                        (nreverse styles))))
      (message "Filled %d cell%s" count (if (= count 1) "" "s")))))

;;;----------------------------------------------------------------------
;;; Find and replace
;;;----------------------------------------------------------------------

(defvar-local exc-etaf--last-search nil
  "The text `exc-etaf-find' last looked for in this grid.")

(defun exc-etaf--cell-matches-p (source row col text)
  "Return non-nil when the cell at ROW and COL of SOURCE holds TEXT.
What the cell shows and what it stores are both looked at, ignoring
case."
  (let ((case-fold-search t)
        (pattern (regexp-quote text))
        (shown (exc-get-display-value-at source row col))
        (raw (exc-get-raw-value-at source row col)))
    (or (and (stringp shown) (string-match-p pattern shown))
        (and (stringp raw) (string-match-p pattern raw)))))

(defun exc-etaf-find (text &optional backward)
  "Move to the next cell that holds TEXT, wrapping around the sheet.
What a cell shows and its formula are both searched, ignoring case.
Empty input repeats the last search.  With a prefix argument, BACKWARD,
the search goes the other way."
  (interactive
   (list (read-string (if exc-etaf--last-search
                          (format "Find (default %s): "
                                  exc-etaf--last-search)
                        "Find: ")
                      nil nil exc-etaf--last-search)
         current-prefix-arg))
  (when (or (null text) (string-empty-p text))
    (user-error "Nothing to find"))
  (setq exc-etaf--last-search text)
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (rows (exc-get-row-count source))
         (cols (exc-get-column-count source))
         (total (* rows cols))
         (cursor (exc-etaf--cursor))
         (start (+ (* (car cursor) cols) (cdr cursor)))
         found)
    (cl-loop for step from 1 to total until found do
             (let* ((index (mod (+ start (if backward (- step) step)) total))
                    (row (/ index cols))
                    (col (% index cols)))
               (when (and (exc-etaf--row-shown-p state row)
                          (exc-etaf--cell-matches-p source row col text))
                 (setq found (cons row col)))))
    (if (null found)
        (message "No cell holds %s" text)
      (exc-etaf-goto (car found) (cdr found))
      (when (equal found (exc-etaf--cursor))
        (message "Found in %s"
                 (exc-etaf--address (car found) (cdr found)))))))

(defun exc-etaf-replace (from to)
  "Replace FROM with TO in the cells of the selection, or of the sheet.
A selection of several cells limits the change to them; otherwise every
cell is looked at.  What cells store is changed, formulas included,
ignoring the case of FROM.  The whole is one undoable step."
  (interactive
   (let ((from (read-string "Replace: ")))
     (list from (read-string (format "Replace %s with: " from)))))
  (when (string-empty-p from)
    (user-error "Nothing to replace"))
  (let* ((state (exc-etaf--state-or-error))
         (source (exc-etaf--source state))
         (selection (exc-etaf-selection))
         (range (if (and (= (nth 0 selection) (nth 2 selection))
                         (= (nth 1 selection) (nth 3 selection)))
                    (list 0 0 (1- (exc-get-row-count source))
                          (1- (exc-get-column-count source)))
                  selection))
         (case-fold-search t)
         (pattern (regexp-quote from))
         changes)
    (cl-loop
     for row from (nth 0 range) to (nth 2 range)
     when (exc-etaf--row-shown-p state row) do
     (cl-loop
      for col from (nth 1 range) to (nth 3 range)
      when (exc-etaf--col-shown-p state col) do
      (let ((raw (exc-get-raw-value-at source row col)))
        (when (and (stringp raw) (string-match-p pattern raw))
          (push (list row col
                      (replace-regexp-in-string pattern to raw t t))
                changes)))))
    (let ((count (if changes
                     (exc-etaf--set-cells state (nreverse changes))
                   0)))
      (message "Replaced in %d cell%s" count (if (= count 1) "" "s")))))

;;;----------------------------------------------------------------------
;;; Mode and entry points
;;;----------------------------------------------------------------------

(defun exc-etaf--viewport-changed (buffer)
  "Keep BUFFER's cursor shown and point on it after its window resized."
  (when (buffer-live-p buffer)
    (let ((state (buffer-local-value 'exc-etaf--state buffer)))
      (when (and state (exc-etaf--runtime buffer))
        (exc-etaf--settle state)))))

;; Depth 90 runs this after ETAF has published the new viewport.
(add-hook 'ebox-viewport-change-functions
          #'exc-etaf--viewport-changed 90)

(defun exc-etaf--write-contents ()
  "Save the grid of the current buffer instead of its rendered text.
A data source with a `:save-to-file' function, such as a CSV file's,
saves itself to the visited file; any other grid is saved as a .grid
file."
  (when exc--data-source
    (let ((save (gethash :save-to-file exc--data-source)))
      (if (and save buffer-file-name
               (not (string-suffix-p ".grid" buffer-file-name)))
          (progn
            (funcall save exc--data-source buffer-file-name)
            (set-visited-file-modtime)
            (set-buffer-modified-p nil)
            (message "Saved %s" buffer-file-name))
        (exc-write-file))
      (exc-etaf--saved))
    t))

(define-derived-mode exc-etaf-mode exc-mode "Grid Table"
  "Major mode for a grid, which ETAF shows.
Keys are owned by the mounted sheet; see `exc-etaf--keys'."
  :group 'exc
  (setq buffer-read-only nil)
  (setq truncate-lines t)
  ;; The buffer's text is the drawn sheet, not the file's.
  (auto-save-mode -1)
  (add-hook 'write-contents-functions #'exc-etaf--write-contents nil t)
  (add-hook 'pre-command-hook #'exc-etaf--gc-hold nil t)
  (add-hook 'pre-command-hook #'exc-etaf--draw-before-command nil t)
  (add-hook 'post-command-hook #'exc-etaf--draw-after-command nil t)
  (exc-etaf--warn-if-uncompiled))

;;;----------------------------------------------------------------------
;;; Garbage collection and compilation
;;;----------------------------------------------------------------------

(defcustom exc-etaf-gc-cons-threshold (* 64 1024 1024)
  "Bytes a grid's commands may allocate before Emacs collects garbage.
Every move draws the cells on screen again, which allocates several
megabytes; at Emacs's default threshold that is several collections a
move, slow enough that a held arrow key outruns the display.  While
commands run in a grid `gc-cons-threshold' is raised to this, and put
back, with one collection, once Emacs has been idle for a second.  nil
leaves `gc-cons-threshold' alone."
  :type '(choice (const :tag "Leave it alone" nil) integer)
  :group 'exc)

(defvar exc-etaf--gc-saved nil
  "`gc-cons-threshold' as it was before a grid raised it, or nil.")

(defvar exc-etaf--gc-timer nil
  "Idle timer that puts `gc-cons-threshold' back.")

(defun exc-etaf--gc-hold ()
  "Raise `gc-cons-threshold' for a grid command, until Emacs is idle.
A named command of some other package, such as `describe-variable',
is left alone: it draws no cells, and should see the user's value."
  (when (and exc-etaf-gc-cons-threshold
             (or (not (and this-command (symbolp this-command)))
                 (string-match-p "\\`\\(?:exc-\\|etaf\\|ebox\\)"
                                 (symbol-name this-command)))
             (null exc-etaf--gc-saved)
             (< gc-cons-threshold exc-etaf-gc-cons-threshold))
    (setq exc-etaf--gc-saved gc-cons-threshold
          gc-cons-threshold exc-etaf-gc-cons-threshold)
    (unless (timerp exc-etaf--gc-timer)
      (setq exc-etaf--gc-timer
            (run-with-idle-timer 1 nil #'exc-etaf--gc-release)))))

(defun exc-etaf--gc-release ()
  "Put back the `gc-cons-threshold' a grid raised, and collect once."
  (setq exc-etaf--gc-timer nil)
  (when exc-etaf--gc-saved
    ;; Something else changed it meanwhile (gcmh, say): leave theirs.
    (when (eql gc-cons-threshold exc-etaf-gc-cons-threshold)
      (setq gc-cons-threshold exc-etaf--gc-saved))
    (setq exc-etaf--gc-saved nil)
    (garbage-collect)))

(defvar exc-etaf--compile-warned nil
  "Non-nil once the uncompiled-library warning has been shown.")

(defun exc-etaf--warn-if-uncompiled ()
  "Warn once when exc or ETAF runs from uncompiled source.
Uncompiled, a move takes about twice as long when only exc is,
and about ten times when ETAF is too."
  (unless (or exc-etaf--compile-warned noninteractive)
    (let ((slow (cl-loop for (library . function)
                         in '(("exc" . exc-etaf-goto)
                              ("etaf" . etaf--patch)
                              ("ebox" . ebox-flush)
                              ("ecss" . ecss-read-rules))
                         when (and (fboundp function)
                                   (not (compiled-function-p
                                         (indirect-function function))))
                         collect library)))
      (setq exc-etaf--compile-warned t)
      (when slow
        (display-warning
         'exc
         (format "%s %s not byte-compiled, so moving around a grid is \
%s slower.  Compile %s; see \"Installation\" in the README."
                 (string-join slow ", ")
                 (if (cdr slow) "are" "is")
                 (if (equal slow '("exc")) "about twice as"
                   "up to ten times")
                 (if (cdr slow) "them" "it")))))))

(defun exc-etaf-refresh (&optional restore-row restore-col)
  "Re-render the current grid from its data source.
RESTORE-ROW and RESTORE-COL, when given, are the display coordinates the
cursor moves to: display column is model column plus one."
  (let* ((state (exc-etaf--state-or-error))
         (cursor (etaf-value (exc-etaf--state-cursor state)))
         (row (if restore-row (max 0 restore-row) (car cursor)))
         (col (if restore-col (max 0 (1- restore-col)) (cdr cursor))))
    (exc-etaf--end-session state)
    (exc-etaf--track-model state)
    (exc-etaf--apply-filters state)
    (exc-etaf--fit-widths state)
    (exc-etaf--bump state)
    (exc-etaf-goto row col)))

(defun exc-etaf-show-in-current-buffer (data-source &optional title)
  "Mount DATA-SOURCE in the current buffer.  TITLE replaces the grid title."
  ;; A chart selected on another sheet is not one of this sheet's.
  (setq exc-etaf--chart nil)
  (let ((file-path exc--file-path)
        (old-title exc--title)
        (allow-elisp exc-allow-elisp)
        (buffer (current-buffer))
        (runtime (exc-etaf--runtime)))
    ;; An edit in progress belongs to the sheet being left.
    (when (and runtime exc-etaf--state)
      (exc-etaf--finish-edit exc-etaf--state))
    ;; Changing the major mode drops the Runtime's buffer-local hooks.
    (when runtime (etaf-unmount runtime))
    (exc-etaf-mode)
    (setq exc--data-source data-source
          exc--file-path file-path
          exc--title (or title old-title)
          exc-allow-elisp allow-elisp)
    (let* ((start (cons 0 0))
           ;; A sheet shown here before comes back as it was left: cursor,
           ;; scroll position, filters and undo history.
           (kept (gethash :etaf-state data-source))
           (state (if (and kept (eq (exc-etaf--state-buffer kept) buffer))
                      kept
                    (exc-etaf--state-create
                     :buffer buffer
                     :top (etaf-ref 0) :left (etaf-ref 0)
                     :cursor (etaf-ref start) :anchor (etaf-ref start)
                     :tick (etaf-ref 0) :editing (etaf-ref nil)
                     :widths (exc-etaf--compute-widths data-source)
                     :model (gethash :model data-source)))))
      (setq exc-etaf--state state)
      (puthash :etaf-state state data-source)
      (if (eq state kept)
          ;; Other sheets may have changed what this one shows.
          (progn
            (exc-etaf--track-model state)
            (exc-etaf--apply-filters state)
            (exc-etaf--fit-widths state))
        ;; Widths saved with the grid apply from the first showing.
        (exc-etaf--fit-widths state))
      (etaf-mount buffer
                  (etaf-node 'exc-etaf-sheet (list :state state) nil)
                  exc-etaf-mount-options)
      (etaf-focus (exc-etaf--runtime buffer) 'exc-etaf-root)
      ;; Mounting put ETAF's key map first; the traversal keys go before it.
      (setq exc-etaf--traverse-keys t
            exc-etaf--typing-keys t)
      (setq-local emulation-mode-map-alists
                  (cons 'exc-etaf--emulation-alist
                        (remq 'exc-etaf--emulation-alist
                              emulation-mode-map-alists)))
      (setq exc--nav-row 0
            exc--nav-col 1)
      (exc-etaf--settle state)
      (set-buffer-modified-p nil))))

(provide 'exc-etaf)

;;; exc-etaf.el ends here
