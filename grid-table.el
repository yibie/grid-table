;;; grid-table.el --- Grid table for displaying and editing tabular data -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'cl-macs)
(require 'grid-data-source)
(require 'grid-data-model)
(require 'grid-table-parser)
(require 'grid-table-persistence)
(require 'grid-table-nav)
(require 'grid-table-plugins)

;;;----------------------------------------------------------------------
;;; Helper functions for data source access
;;;----------------------------------------------------------------------

(defun grid-get-column-count (source)
  "Get column count from data source."
  (funcall (gethash :get-column-count source) source))

(defun grid-get-row-count (source)
  "Get row count from data source."
  (funcall (gethash :get-row-count source) source))

(defun grid-get-header-value (source col)
  "Get header value from data source."
  (funcall (gethash :get-header-value source) source col))

(defun grid-get-raw-value-at (source row col)
  "Get raw value from data source."
  (funcall (gethash :get-raw-value-at source) source row col))

(defun grid-get-computed-value-at (source row col)
  "Get computed value from data source."
  (funcall (gethash :get-computed-value-at source) source row col))

(defun grid-set-raw-value-at (source row col value)
  "Set raw value in data source."
  (funcall (gethash :set-raw-value-at source) source row col value))

;;;----------------------------------------------------------------------
;;; Grid Table Configuration
;;;----------------------------------------------------------------------

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

(defcustom grid-table-title-align 'center
  "Title alignment above the table: left, center, or right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right))
  :group 'grid-table)

(defcustom grid-table-title-decoration 'none
  "Title decoration style above the table. When 'rule, draw a horizontal rule under the title."
  :type '(choice (const :tag "None" none)
                 (const :tag "Rule" rule))
  :group 'grid-table)

(defvar-local grid-table--data-source nil
  "Buffer-local variable to hold the current data source instance.")

(defvar-local grid-table--file-path nil
  "Buffer-local variable to hold the file path of the current grid.")

(defvar-local grid-table--title nil
  "Buffer-local variable to hold the title of the current grid.")

(defconst grid-table--col-header-row-idx -1
  "Special row index for the A, B, C... column header row.")

(defconst grid-table--row-header-col-idx 0
  "Special column index for the 1, 2, 3... row header column.")

(defface grid-table-highlight-face
  '((t :background "gray20"))
  "Face for highlighting the current grid cell."
  :group 'grid-table)

(defface grid-table-markup-face
  '((t :foreground "dim gray"))
  "Face for Org-like markup characters in grid cells."
  :group 'grid-table)

(defvar-local grid-table--highlight-overlay nil)

;;----------------------------------------------------------------------
;; Helpful functions
;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;; Rendering
;;;----------------------------------------------------------------------

(defun grid-table-get-cell-at-point ()
  "Get the logical coordinates of the cell at the current point.
Robustly searches the current line if point is not directly on cell text."
  (interactive)
  (let ((props-at-point (get-text-property (point) 'grid-cell)))
    (if props-at-point
        (list :display-row-idx (get-text-property (point) 'grid-display-row-idx)
              :display-col-idx (get-text-property (point) 'grid-display-col-idx))
      (save-excursion
        (goto-char (line-beginning-position))
        (when-let* ((pos (next-single-property-change (point) 'grid-cell nil (line-end-position))))
          (goto-char pos)
          (list :display-row-idx (get-text-property (point) 'grid-display-row-idx)
                :display-col-idx (get-text-property (point) 'grid-display-col-idx)))))))

;;----------------------------------------------------------------------
;; Column Width Calculation
;;----------------------------------------------------------------------

(defconst grid-table--font-lock-keywords
  (list
   ;; *bold*
   '("\\(\\*\\)\\([^\n*]+\\)\\(\\*\\)"
     (1 'grid-table-markup-face) (2 'bold) (3 'grid-table-markup-face))
   ;; /italic/
   '("\\(/\\)\\([^\n/]+\\)\\(/\\)"
     (1 'grid-table-markup-face) (2 'italic) (3 'grid-table-markup-face))
   ;; _underline_
   '("\\(_\\)\\([^\n_]+\\)\\(_\\)"
     (1 'grid-table-markup-face) (2 'underline) (3 'grid-table-markup-face))
   ;; +strike-through+
   '("\\(\\+\\)\\([^\n+]+\\)\\(\\+\\)"
     (1 'grid-table-markup-face) (2 'strike-through) (3 'grid-table-markup-face))
   ;; ~code~
   '("\\(~\\)\\([^\n~]+\\)\\(~\\)"
     (1 'grid-table-markup-face) (2 'fixed-pitch) (3 'grid-table-markup-face)))
  "Font-lock keywords for grid-table-mode.")

(defun grid-table--calculate-column-widths (headers data-rows)
  "Calculate the optimal column widths based on headers and data.
HEADERS is a list of strings for the column titles.
DATA-ROWS is a list of lists, where each inner list represents a row.
Returns a list of integers representing the calculated width for each column."
  (when (or headers data-rows)
    (let* ((num-columns (length headers))
           (min-width 5) ; Minimum width for any column
           (max-width 40) ; Maximum width for any column
           (widths (make-list num-columns 0)))

      ;; 1. Calculate initial widths from headers
      (dotimes (i num-columns)
        (setf (nth i widths) (max min-width (string-width (nth i headers)))))

      ;; 2. Expand widths based on data rows and A, B, C... headers
      (dolist (row data-rows)
        (dotimes (i num-columns)
          (when (< i (length row))
            (let* ((cell-content (format "%s" (or (nth i row) "")))
                   (content-width 
                      (if (grid-table--extract-image-path cell-content)
                         ;; Use configured width for images
                         grid-table-image-column-width
                       ;; Raw text width calculation is now correct
                       (string-width cell-content))))
              ;; Also consider the A, B, C... headers for width
              (let ((col-letter-width (string-width (grid-table--col-number-to-letter i))))
                (setf (nth i widths) (max (nth i widths) col-letter-width)))
              
              ;; Update width based on cell content
              (setf (nth i widths) (max (nth i widths) content-width))))))

      ;; 3. Apply max width constraint
      (dotimes (i num-columns)
        (setf (nth i widths) (min max-width (nth i widths))))

      ;; 4. (Future) Adjust total width to fit window size.
      ;; For now, we just return the calculated widths.

      widths)))

(defun grid-table--wrap-text (text width)
  "Wrap TEXT into a list of strings, each fitting within WIDTH."
  (if (<= (string-width text) width)
      (list text)
    (let ((lines '())
          (remaining-text text))
      (while (> (string-width remaining-text) width)
        (let* ((break-pos (or (cl-loop for i from (min (length remaining-text) width) downto 1
                                       when (<= (string-width remaining-text 0 i) width)
                                       return i)
                              1))
               ;; Try to find a natural break point (space)
               (space-pos (or (cl-position ?\s remaining-text :from-end t :end break-pos)
                              break-pos)))
          (push (substring remaining-text 0 space-pos) lines)
          (setq remaining-text (string-trim (substring remaining-text space-pos)))))
      (push remaining-text lines)
      (nreverse lines))))

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



(defun grid-table--setup-perfect-display-environment ()
  "Setup perfect display environment, ensure consistent line height."
  ;; Force line spacing to 0 - this is the key setting
  (setq-local line-spacing 0)
  
  ;; Ensure no extra line height factor
  (setq-local line-height-factor 1.0)
  
  ;; Disable features that might affect line height
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  
  (message "Display environment optimized for perfect height matching"))

;;----------------------------------------------------------------------
;; Title rendering helpers
;;----------------------------------------------------------------------

(defun grid-table--compute-table-width (data-source)
  "Compute the total table width for DATA-SOURCE, for title alignment."
  (let* ((get-col-count-fn (gethash :get-column-count data-source))
         (get-row-count-fn (gethash :get-row-count data-source))
         (get-computed-fn (gethash :get-computed-value-at data-source))
         (get-header-fn (gethash :get-header-value data-source))
         (num-rows (funcall get-row-count-fn data-source))
         (num-cols (funcall get-col-count-fn data-source))
         (headers (cl-loop for c from 0 below num-cols
                           collect (funcall get-header-fn data-source c)))
         (all-rows (cl-loop for r from 0 below num-rows
                            collect (cl-loop for c from 0 below num-cols
                                             collect (funcall get-computed-fn data-source r c))))
         (widths (grid-table--calculate-column-widths headers all-rows)))
    (let* ((max-row-number-width (string-width (number-to-string num-rows)))
           (row-header-width (max 3 (+ 1 max-row-number-width)))
           (full-widths (cons row-header-width widths))
           (sep-line (grid-table--draw-separator full-widths "┌" "┬" "┐" t)))
      (string-width sep-line))))

(defun grid-table--render-title-line (data-source)
  "Render title line above the table according to alignment/decoration settings."
  (when grid-table--title
    (let* ((table-width (grid-table--compute-table-width data-source))
           (title-text (format "Title: %s" grid-table--title))
           (title-width (string-width title-text))
           (left-pad (pcase grid-table-title-align
                       ('left 0)
                       ('center (max 0 (floor (/ (max 0 (- table-width title-width)) 2))))
                       ('right (max 0 (- table-width title-width)))
                       (_ 0)))
           (padding (make-string left-pad ?\s))
           (result (concat padding title-text "\n")))
      (when (eq grid-table-title-decoration 'rule)
        (setq result (concat result (make-string table-width ?─) "\n")))
      (concat result "\n"))))

;;----------------------------------------------------------------------
;; Draw Row
;;----------------------------------------------------------------------

(defun grid-table--enforce-line-height (string)
  "Enforce a fixed 'line-height' text property for each line in STRING."
  (propertize string 'line-height (grid-table--get-exact-line-height)))

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

(defun grid-table--draw-row (rowData widths display-row-idx)
  "Draw a table row, supporting image mosaics and text, with uniform line height."
  (let* (;; Phase 1: Process all cells into a list of lists (lines per cell)
         (processed-cells
          (cl-loop for cell in rowData for width in (cdr widths) ; FIX 1: Use CDR to skip row-header width
                   collect (let* ((computed-value (if (stringp cell) cell (format "%s" cell)))
                                  (content (if (stringp computed-value) computed-value (format "%s" computed-value)))
                                  (image-path (grid-table--extract-image-path content)))
                             (if image-path
                                 (with-temp-buffer
                                   (if-let* ((source-info (ignore-errors (grid-table--get-loadable-image-source image-path)))
                                             (source-file (car source-info))
                                             (source-type (cadr source-info))
                                             (target-px-w (* width (frame-char-width)))
                                             (target-px-h (* grid-table-image-target-char-height (grid-table--get-exact-line-height)))
                                             (image (create-image source-file source-type nil :width target-px-w :height target-px-h :ascent 'center)))
                                     (insert-sliced-image image " " nil grid-table-image-target-char-height width)
                                   (insert (format "[IMG ERR: %s]" (file-name-nondirectory image-path))))
                                   (let ((lines (split-string (buffer-string) "\n" t)))
                                     ;; Limit image height to prevent excessive row height
                                     (if (> (length lines) grid-table-image-target-char-height)
                                         (cl-subseq lines 0 grid-table-image-target-char-height)
                                       (if (and (= (length lines) 1) (string= (car lines) ""))
                                           (list "") ; Return single empty line for failed images
                                         lines))))
                               (grid-table--wrap-text content width)))))
         ;; Phase 2: Calculate the maximum height of the row
         (max-row-height (apply #'max 1 (mapcar (lambda (cell-lines) 
                                                   (if (and (= (length cell-lines) 1) 
                                                            (string= (car cell-lines) ""))
                                                       0 ; Empty cells don't contribute to height
                                                     (length cell-lines)))
                                               processed-cells)))
         (row-header-width (nth 0 widths)) ; Get the calculated width for the row header column
         (row-parts
          (cl-loop for line-idx from 0 below max-row-height
                   collect (let* ((line-parts (cl-loop for processed-content in processed-cells
                                                      for width in (cdr widths)
                                                      for col-idx from 0
                                                      collect (let* ((content-part (or (nth line-idx processed-content) ""))
                                                                     (padded (grid-table--pad-string content-part width)))
                                                                (propertize (format " %s " padded)
                                                                            'grid-cell t
                                                                            'grid-display-row-idx display-row-idx
                                                                            'grid-display-col-idx (1+ col-idx)))))
                                  (row-header-content (if (= line-idx 0)
                                                          (if (= display-row-idx grid-table--col-header-row-idx)
                                                              " "
                                                            (format "%s" (number-to-string (1+ display-row-idx)))) ; Display 1-based row number
                                                        " "))
                                  (row-header-padded (grid-table--pad-string row-header-content row-header-width))
                                  (propertized-row-header (propertize row-header-padded
                                                                      'grid-cell t
                                                                      'grid-display-row-idx display-row-idx
                                                                      'grid-display-col-idx grid-table--row-header-col-idx)))
                             (format "│%s│%s│" propertized-row-header (string-join line-parts "│"))))))
    (string-join row-parts "\n")))

(defun grid-table--draw-separator (widths &optional left mid right has-row-header)
  "Draw a horizontal separator line.
LEFT, MID, RIGHT are the characters for left-end, middle, and right-end.
HAS-ROW-HEADER indicates if this separator should include a row header column."
  (let* ((left (or left "├")) (mid (or mid "┼")) (right (or right "┤")))
    (if has-row-header
        (let* ((row-header-width (car widths))
               (data-widths (cdr widths))
               ;; The row header content is NOT padded with spaces, so its separator width is just its raw width.
               (row-header-segment (make-string row-header-width ?─))
               ;; Data cell content IS padded with spaces (" %s "), so separator width is width + 2.
               (data-segments
                (mapcar (lambda (w) (make-string (+ w 2) ?─)) data-widths)))
          ;; The final string is built from these segments.
          (concat left row-header-segment mid (string-join data-segments mid) right))
      ;; This part is for tables without row headers.
      (let ((segments
             (mapcar (lambda (w) (make-string (+ w 2) ?─)) widths)))
        (concat left (string-join segments mid) right)))))



(defun grid-table-edit-cell ()
  "Edit the value of the current cell."
  (interactive)
  (let ((inhibit-read-only t))
    (when-let* ((coords (grid-table-get-cell-at-point))
                (display-row-idx (plist-get coords :display-row-idx))
                (display-col-idx (plist-get coords :display-col-idx)))
      (cond
       ((= display-col-idx grid-table--row-header-col-idx) ; Editing row header (1, 2, 3...)
        (message "Cannot edit row header (1, 2, 3...) column."))
       ((= display-row-idx grid-table--col-header-row-idx) ; Editing A, B, C... header row
        (message "Cannot edit column header (A, B, C...) row."))

       (t ; Editing data cell
        (let* ((get-raw-fn (gethash :get-raw-value-at grid-table--data-source))
               (set-raw-fn (gethash :set-raw-value-at grid-table--data-source))
               ;; Data model row index is the same as display row index for data rows.
               ;; Data model col index is display col index - 1 (for row header).
               (current-value (funcall get-raw-fn grid-table--data-source display-row-idx (1- display-col-idx)))
               (new-value (read-string (format "New value for %s%s: " (grid-table--col-number-to-letter (1- display-col-idx)) (1+ display-row-idx)) current-value)) ; Excel style R/C
               (modified-cell (funcall set-raw-fn grid-table--data-source display-row-idx (1- display-col-idx) new-value)))
          (when modified-cell ; Only redraw if a cell was actually modified
            (grid-table-refresh) ; Full refresh is simpler and safer for now
            (grid-table--move-to-cell display-row-idx display-col-idx)
            (grid-table--highlight-cell display-row-idx display-col-idx)
            (message "Cell updated."))))))))

(defun grid-table-edit-title ()
  "Edit the title of the current grid with alignment and decoration options."
  (interactive)
  (let* ((new-title (read-string (format "Edit Title: %s -> " (or grid-table--title "")) (or grid-table--title "")))
         (new-align (when (not (string-empty-p new-title))
                      (intern (completing-read "Title alignment: " 
                                               '("left" "center" "right") 
                                               nil t 
                                               (symbol-name grid-table-title-align)))))
         (new-decoration (when (not (string-empty-p new-title))
                           (intern (completing-read "Title decoration: " 
                                                    '("none" "rule") 
                                                    nil t 
                                                    (symbol-name grid-table-title-decoration))))))
    (unless (and (string= new-title (or grid-table--title ""))
                 (eq new-align grid-table-title-align)
                 (eq new-decoration grid-table-title-decoration))
      (setq-local grid-table--title new-title)
      (when new-align
        (setq-local grid-table-title-align new-align))
      (when new-decoration
        (setq-local grid-table-title-decoration new-decoration))
      (grid-table-refresh)
      (message "Grid title updated (align: %s, decoration: %s)." 
               (or new-align grid-table-title-align) 
               (or new-decoration grid-table-title-decoration)))))

(defun grid-table-sort-column ()
  "Sort the table by the current column or a user-specified column.
Prompts for sort direction (ascending/descending)."
  (interactive)
  (when grid-table--data-source
    (let* ((coords (grid-table-get-cell-at-point))
           (current-col (if coords (plist-get coords :display-col-idx) 1)) ; Default to first data column
           (col-idx (if (and coords (> current-col 0)) ; Skip row header column
                       (1- current-col) ; Adjust for row header column
                     (read-number "Sort by column (0-based index): " 0)))
           (sort-direction (completing-read "Sort direction: " '("ascending" "descending") nil t "ascending"))
           (sort-order (if (string= sort-direction "ascending") 'ascending 'descending))
           (sort-fn (gethash :sort-by-column grid-table--data-source)))
      (when sort-fn
        (let* ((current-coords (grid-table-get-cell-at-point))
               (restore-row-idx (when current-coords (plist-get current-coords :display-row-idx)))
               (restore-col-idx (when current-coords (plist-get current-coords :display-col-idx)))
               (new-data-source (funcall sort-fn grid-table--data-source col-idx sort-order)))
          (setq-local grid-table--data-source new-data-source)
          (grid-table-refresh restore-row-idx restore-col-idx) ; Force full refresh to display sorted results
          (message "Table sorted by column %s (%s)." col-idx sort-direction))))))

;;----------------------------------------------------------------------
;; Column Letter Conversion
;;----------------------------------------------------------------------

(defun grid-table--col-number-to-letter (col-num)
  "Converts a 0-based column number to an Excel-style letter (e.g., 0 -> A, 25 -> Z, 26 -> AA)."
  (let ((result ""))
    (while (>= col-num 0)
      (setq result (concat (char-to-string (+ ?A (mod col-num 26))) result))
      (setq col-num (1- (/ col-num 26))))
    result))

;;----------------------------------------------------------------------
;; Add/Delete Row/Column
;;----------------------------------------------------------------------

(defun grid-table-add-row (&optional row-idx)
  "Add a new row at ROW-IDX, or at the end if ROW-IDX is nil.
If called interactively, adds a row below the current cell."
  (interactive (list (when-let ((coords (grid-table-get-cell-at-point)))
                       (plist-get coords :display-row-idx))))
  (when grid-table--data-source
    ;; Validate row-idx before proceeding
    (unless (and row-idx (numberp row-idx) (>= row-idx 0))
      (message "Error: Invalid row index. Please place cursor on a valid table cell.")
      (cl-return-from grid-table-add-row))
    (let* ((current-coords (grid-table-get-cell-at-point))
           (restore-row-idx (when current-coords (plist-get current-coords :display-row-idx)))
           (restore-col-idx (when current-coords (plist-get current-coords :display-col-idx)))
           (add-row-fn (gethash :add-row grid-table--data-source))
           (data-model-row-idx row-idx) ; row-idx is already correct (0-based data row index)
           (new-data-source (funcall add-row-fn grid-table--data-source data-model-row-idx)))
      (setq-local grid-table--data-source new-data-source)
      (grid-table-refresh restore-row-idx restore-col-idx)
      (message "Row added."))))

(defun grid-table-delete-row (&optional row-idx)
  "Delete the row at ROW-IDX.
If called interactively, deletes the current row."
  (interactive (list (when-let ((coords (grid-table-get-cell-at-point)))
                       (plist-get coords :display-row-idx))))
  (when grid-table--data-source
    ;; Validate row-idx before proceeding
    (unless (and row-idx (numberp row-idx) (>= row-idx 0))
      (message "Error: Invalid row index. Please place cursor on a valid table cell.")
      (cl-return-from grid-table-delete-row))
    ;; Row index 0 is the user-defined header row, not allowed to be deleted
    (when (= row-idx 0)
      (message "Cannot delete header row.")
      (cl-return-from grid-table-delete-row))
    (let* ((current-coords (grid-table-get-cell-at-point))
           (restore-row-idx (when current-coords (plist-get current-coords :display-row-idx)))
           (restore-col-idx (when current-coords (plist-get current-coords :display-col-idx)))
           ;; Convert display row index to data area (excluding header): data row index = display row index - 1
           (data-model-row-idx (1- row-idx)))
      (condition-case err
          (let* ((delete-row-fn (gethash :delete-row grid-table--data-source))
                 (new-data-source (funcall delete-row-fn grid-table--data-source data-model-row-idx)))
            (setq-local grid-table--data-source new-data-source)
            ;; After deletion, calculate a safe recovery position to avoid the cursor falling outside the table (especially when deleting the last row)
            (let* ((get-row-count-fn (gethash :get-row-count grid-table--data-source))
                   (get-col-count-fn (gethash :get-column-count grid-table--data-source))
                   (new-num-rows (funcall get-row-count-fn grid-table--data-source))
                   (new-num-cols (funcall get-col-count-fn grid-table--data-source))
                   ;; Display layer row index range: [0 .. new-num-rows-1]
                   (safe-row (max 0 (min (or restore-row-idx 0) (1- new-num-rows))))
                   ;; Display layer column index range: [0 .. new-num-cols] (including row header column 0)
                   (safe-col (max 0 (min (or restore-col-idx 0) new-num-cols))))
              (grid-table-refresh safe-row safe-col))
            (message "Row deleted."))
        (error (message "Error deleting row: %s" (error-message-string err)))))))

(defun grid-table-add-column (&optional col-idx)
  "Add a new column at COL-IDX, or at the end if COL-IDX is nil.
If called interactively, adds a column to the right of the current cell."
  (interactive (list (when-let ((coords (grid-table-get-cell-at-point)))
                       (plist-get coords :display-col-idx))))
  (when grid-table--data-source
    ;; Validate col-idx before proceeding
    (unless (and col-idx (numberp col-idx) (>= col-idx 0))
      (message "Error: Invalid column index. Please place cursor on a valid table cell.")
      (cl-return-from grid-table-add-column))
    (let* ((current-coords (grid-table-get-cell-at-point))
           (restore-row-idx (when current-coords (plist-get current-coords :display-row-idx)))
           (restore-col-idx (when current-coords (plist-get current-coords :display-col-idx)))
           ;; To insert to the right of the current column, the new column should appear after the current data column.
           ;; grid-model-add-column inserts the new column before the given index (left).
           ;; Therefore we pass the display index (including header 0), which is equivalent to (data-col-index + 1).
           ;; This ensures insertion to the right of the current data column.
           (insertion-col-idx col-idx))
      (let* ((add-column-fn (gethash :add-column grid-table--data-source))
             (new-data-source (funcall add-column-fn grid-table--data-source insertion-col-idx)))
        (setq-local grid-table--data-source new-data-source)
        (grid-table-refresh restore-row-idx restore-col-idx)
        (message "Column added.")))))

(defun grid-table-delete-column (&optional col-idx)
  "Delete the column at COL-IDX.
If called interactively, deletes the current column."
  (interactive (list (when-let ((coords (grid-table-get-cell-at-point)))
                       (plist-get coords :display-col-idx))))
  (when grid-table--data-source
    ;; Validate col-idx before proceeding
    (unless (and col-idx (numberp col-idx) (>= col-idx 0))
      (message "Error: Invalid column index. Please place cursor on a valid table cell.")
      (cl-return-from grid-table-delete-column))
    (let* ((current-coords (grid-table-get-cell-at-point))
           (restore-row-idx (when current-coords (plist-get current-coords :display-row-idx)))
           (restore-col-idx (when current-coords (plist-get current-coords :display-col-idx)))
           (data-model-col-idx (1- col-idx))) ; display col 1 = data col 0, display col 2 = data col 1, etc.
      (condition-case err
          (let* ((delete-column-fn (gethash :delete-column grid-table--data-source))
                 (new-data-source (funcall delete-column-fn grid-table--data-source data-model-col-idx)))
            (setq-local grid-table--data-source new-data-source)
            (grid-table-refresh restore-row-idx restore-col-idx)
            (message "Column deleted."))
        (error (message "Error deleting column: %s" (error-message-string err)))))))

(defun grid-table-insert-row-above ()
  "Insert a new row above the current row."
  (interactive)
  (when-let ((coords (grid-table-get-cell-at-point)))
    (grid-table-add-row (plist-get coords :display-row-idx))))

(defun grid-table-insert-column-left ()
  "Insert a new column to the left of the current column."
  (interactive)
  (when-let ((coords (grid-table-get-cell-at-point)))
    ;; To insert to the left of the current column, reuse the implementation of "insert to the right":
    ;; Pass (display-col-idx - 1), which is equivalent to the left of the current column.
    (let* ((display-col (plist-get coords :display-col-idx))
           (target (max 0 (1- display-col))))
      (grid-table-add-column target))))

;;----------------------------------------------------------------------
;; Save/Load
;;----------------------------------------------------------------------

(defun grid-table-write-file ()
  "Save the current grid to its associated file, or prompt for a new one."
  (interactive)
  (let ((file-path (or grid-table--file-path ; Use existing path if available
                       (read-file-name "Save Grid As: " nil nil t nil ".grid")))) ; Prompt for new path, suggest .grid
    (when file-path
      ;; Update the title in the data source before saving
      (puthash :title grid-table--title grid-table--data-source)
      (grid-table-persistence-save-to-file grid-table--data-source file-path)
      (setq-local grid-table--file-path file-path)
      (rename-buffer (file-name-nondirectory file-path))
      (set-visited-file-name file-path)
      (set-buffer-modified-p nil))))

(defun grid-table-find-file (file-path)
  "Open a grid from a file."
  (interactive "fFind grid file: " nil nil ".grid") ; Suggest .grid extension
  (let* ((data-source (grid-table-persistence-create-data-source-from-file file-path))
         (buffer-name (file-name-nondirectory file-path)))
    (if data-source
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (grid-table-mode)
            (setq-local grid-table--file-path file-path)
            ;; Get title from data source
            (setq-local grid-table--title (gethash :title data-source))
            (grid-table--show-grid-in-current-buffer data-source))
          (switch-to-buffer buffer))
      (message "Failed to load grid from %s" file-path))))

;;;###autoload
(defun grid-open (file-path)
  "Open a .grid file directly in grid-table view."
  (interactive "fOpen grid file: ")
  (grid-table-find-file file-path))


;;----------------------------------------------------------------------
;; Main Functions & Mode Definition
;;----------------------------------------------------------------------

(defun grid-table--redraw-cells (cells)
  "Redraw a list of CELLS that have been updated."
  (let ((inhibit-read-only t)
        (rows-to-redraw (make-hash-table)))
    ;; 1. Identify which rows need redrawing
    (dolist (cell cells)
      (puthash (grid-cell-row cell) t rows-to-redraw))
    ;; 2. Redraw each affected row
    (maphash
     (lambda (row-idx _)
       (let* ((get-col-count-fn (gethash :get-column-count grid-table--data-source))
              (get-row-count-fn (gethash :get-row-count grid-table--data-source))
              (get-computed-fn (gethash :get-computed-value-at grid-table--data-source))
              (get-header-fn (gethash :get-header-value grid-table--data-source))
              (num-cols (funcall get-col-count-fn grid-table--data-source))
              (num-rows (funcall get-row-count-fn grid-table--data-source))
              (row-values (cl-loop for c from 0 below num-cols
                                   collect (funcall get-computed-fn grid-table--data-source row-idx c))) ; Direct call to grid-model-get-computed-value
              (headers (cl-loop for c from 0 below num-cols
                                collect (funcall get-header-fn grid-table--data-source c)))
              ;; For column width calculation, we still need all rows' computed values
              (all-rows (cl-loop for r from 0 below num-rows
                                 collect (cl-loop for c from 0 below num-cols
                                                  collect (funcall get-computed-fn grid-table--data-source r c)))) ; Direct call to grid-model-get-computed-value
              (widths (grid-table--calculate-column-widths headers all-rows))
              (new-row-content (grid-table--draw-row row-values widths row-idx))
              (bounds (grid-table--get-row-bounds row-idx)))
         (when bounds
           (delete-region (car bounds) (cdr bounds))
           (goto-char (car bounds))
           (insert new-row-content))))
     rows-to-redraw)
  (redisplay))) ; Force redisplay after changes

(defun grid-table--get-row-bounds (row-idx)
  "Get the start and end positions of a given ROW-IDX."
  (save-excursion
    (when (grid-table--move-to-cell row-idx 0)
      (let ((start (line-beginning-position))
            (end (line-end-position)))
        (cons start end)))))

(defun grid-table--insert-table-from-data-source ()
  "Insert a grid table into the current buffer from `grid-table--data-source`."
  (let* ((get-col-count-fn (gethash :get-column-count grid-table--data-source))
         (get-row-count-fn (gethash :get-row-count grid-table--data-source))
         (get-computed-fn (gethash :get-computed-value-at grid-table--data-source))
         (get-header-fn (gethash :get-header-value grid-table--data-source))
         (num-rows (funcall get-row-count-fn grid-table--data-source))
         (num-cols (funcall get-col-count-fn grid-table--data-source))
         (headers (cl-loop for c from 0 below num-cols
                           collect (funcall get-header-fn grid-table--data-source c)))
         (all-rows (cl-loop for r from 0 below num-rows
                            collect (cl-loop for c from 0 below num-cols ; Data model rows are 0-indexed
                                             collect (funcall get-computed-fn grid-table--data-source r c))))
         (widths (grid-table--calculate-column-widths headers all-rows))
         (col-header-data (cl-loop for c from 0 below num-cols
                                   collect (grid-table--col-number-to-letter c))))

    ;; Prepend width for row header column.
    ;; Use let* because the calculation of `row-header-width` depends on `max-row-number-width`.
    (let* ((max-row-number-width (string-width (number-to-string num-rows)))
           (row-header-width (max 3 (+ 1 max-row-number-width)))) ; Add 1 for padding, ensure min width of 3
      (setq widths (cons row-header-width widths)))

    ;; Draw A, B, C... column headers
    (insert (grid-table--draw-separator widths "┌" "┬" "┐" t) "\n") ; Top border, 't' for row header
    (insert (grid-table--draw-row col-header-data widths grid-table--col-header-row-idx) "\n") ; A, B, C... row, special row_idx
    (insert (grid-table--draw-separator widths "├" "┼" "┤" t) "\n") ; Separator below A, B, C..., 't' for row header

    ;; Draw user-defined headers (now the first data row) and subsequent data rows
    (cl-loop for r from 0 below num-rows do
             (let ((row-data (nth r all-rows)))
               ;; r is the data model row index, which is also the display row index for data rows
               ;; (since A,B,C header is -1, and user-defined header is 0)
               (insert (grid-table--draw-row row-data widths r))
               (unless (= r (1- num-rows)) ; Don't draw separator after the last row
                 (insert "\n" (grid-table--draw-separator widths "├" "┼" "┤" t))) ; 't' for row header
               (insert "\n")))

    (insert (grid-table--draw-separator widths "└" "┴" "┘" t) "\n") ; Bottom border, 't' for row header
    ))

(defun grid-table--show-grid-in-current-buffer (data-source)
  "Render the grid from DATA-SOURCE in the current buffer.
Assumes `grid-table-mode` is already active."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local grid-table--data-source data-source)
    (grid-table--setup-perfect-display-environment)
    ;; Display title with alignment and decoration
    (when-let ((title-content (grid-table--render-title-line data-source)))
      (insert title-content))
    (grid-table--insert-table-from-data-source)
    (goto-char (point-min))
    (when (> (grid-get-row-count data-source) 0)
      (grid-table--move-to-cell 0 1) ; Move to first user-defined header cell (row header is 0, first data column is 1)
      (grid-table--highlight-cell 0 1)))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun grid-table--show-grid (data-source &optional buffer-name)
  "Show a grid view in a dedicated buffer using DATA-SOURCE."
  (let ((buffer (get-buffer-create (or buffer-name "*Grid Table*"))))
    (with-current-buffer buffer
      (grid-table-mode)
      (grid-table--show-grid-in-current-buffer data-source))
    (switch-to-buffer buffer)))

(defun grid-table-refresh (&optional restore-row-idx restore-col-idx)
  "Refresh the grid view by re-generating its content from the data source.
If RESTORE-ROW-IDX and RESTORE-COL-IDX are provided, move cursor to that position after refresh.
When called interactively, automatically saves and restores the current cursor position."
  (interactive)
  (let ((inhibit-read-only t))
    (when (and grid-table--data-source (current-buffer)) ; Ensure data source and buffer exist
      ;; If called interactively and no coordinates provided, save current position
      (when (and (called-interactively-p 'interactive) (not restore-row-idx) (not restore-col-idx))
        (let ((current-coords (grid-table-get-cell-at-point)))
          (when current-coords
            (setq restore-row-idx (plist-get current-coords :display-row-idx))
            (setq restore-col-idx (plist-get current-coords :display-col-idx)))))
      (erase-buffer)
      ;; Render title with alignment and decoration, same as initial render
      (when-let ((title-content (grid-table--render-title-line grid-table--data-source)))
        (insert title-content))
      (grid-table--insert-table-from-data-source)
      ;; Restore cursor position if coordinates are provided
      (when (and restore-row-idx restore-col-idx)
        (grid-table--move-to-cell restore-row-idx restore-col-idx)
        (grid-table--highlight-cell restore-row-idx restore-col-idx)))))

(define-derived-mode grid-table-mode special-mode "Grid Table"
  "Major mode for displaying and editing grid data."
  :group 'grid-table
  (setq truncate-lines nil)
  (setq buffer-read-only t)
  (setq header-line-format
        (propertize " Grid Table" 'face '(:weight bold)))
  (setq-local font-lock-defaults '(grid-table--font-lock-keywords))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'grid-table-refresh)
    (define-key map (kbd "e") 'grid-table-edit-cell)
    (define-key map (kbd "<tab>") 'grid-table-next-cell)
    (define-key map (kbd "<backtab>") 'grid-table-previous-cell)
    (define-key map (kbd "n") 'grid-table-next-line)
    (define-key map (kbd "p") 'grid-table-previous-line)
    (define-key map (kbd "C-c t") 'grid-table-edit-title) ; New keybinding for editing title
    (define-key map (kbd "C-c r a") 'grid-table-add-row)
    (define-key map (kbd "C-c r d") 'grid-table-delete-row)
    (define-key map (kbd "C-c c a") 'grid-table-add-column)
    (define-key map (kbd "C-c c d") 'grid-table-delete-column)
    (define-key map (kbd "C-c C-w") 'grid-table-write-file)
    (define-key map (kbd "C-c C-f") 'grid-table-find-file)
    (define-key map (kbd "C-c s") 'grid-table-sort-column) ; New keybinding for sorting

    (use-local-map map)))

(defun grid-table--open-demo ()
  "Open a grid-table buffer with a demonstration table for internal testing."
  (interactive)
  (let* ((headers '("Item" "Quantity" "Unit Price" "Total" "Notes"))
         (rows
          '(("Laptop" "2" "1200" "=B2*C2" "For new hires")
            ("Monitor" "2" "350" "=B3*C3" "*Urgent* requirement")
            ("Keyboard" "5" "75.50" "=B4*C4" "Mechanical keyboards")
            ("Subtotal" "" "" "=SUM(D2:D4)" "Auto-calculated")
            ("Tax (8%)" "" "" "=D5*0.08" "")
            ("Grand Total" "" "" "=D5+D6" "Final Price")
            ("" "" "" "" "/Users/chenyibin/Pictures/GEdZiU0WUAAoaSj.jpg")
            ("Stats" "Count:" "=COUNT(B2:B4)" "Max Price:" "=MAX(C2:C4)")))
         (data-source (make-default-data-source headers rows "Demo Grid Table")))
    (setq-local grid-table--title "Demo Grid Table") ; Set default title for demo
    (grid-table--show-grid data-source)))

;;;###autoload
(defun grid-table-create (headers rows)
  "Create and display a new grid view from raw data.
This is the simple, user-facing API.
HEADERS is a list of strings (now the first data row).
ROWS is a list of lists of strings."
  (interactive
   (list (list "Column 1" "Column 2" "Column 3" "Column 4") ; Default 4 user-defined headers (now first data row)
         (list (list "" "" "" "") ; Default 4x4 empty data (excluding user-defined headers)
               (list "" "" "" "")
               (list "" "" "" "")
               (list "" "" "" ""))))
  (let ((data-source (make-default-data-source headers rows "New Grid Table"))) ; headers are now the first data row
    (setq-local grid-table--title "New Grid Table") ; Set default title for new grids
    (grid-table--show-grid data-source (generate-new-buffer-name "*grid*"))))


(provide 'grid-table)

;;; grid-table.el ends here 

