;;; grid-table-org.el --- Org integration for grid-table -*- lexical-binding: t; -*-

(require 'grid-table)
(require 'grid-table-persistence)
(require 'org)
(require 'org-element)
(require 'cl-lib)

(defcustom grid-table-org-image-column-width 20
  "Default display width of image column in Org table preview (in characters)."
  :type 'integer
  :group 'grid-table-org)

(defun grid-table-org--render-preview-from-file (file-path &optional pic-dir org-file-dir)
  "Render a static preview string for FILE-PATH (.grid) with smart image linking.
PIC-DIR specifies the base directory for image references.
ORG-FILE-DIR is the directory of the org file for relative link generation."
  (unless (and file-path (file-exists-p file-path))
    (error "Grid file not found: %s" file-path))
  (let* ((data-source (grid-table-persistence-create-data-source-from-file file-path))
         (preview ""))
    (with-temp-buffer
      (let ((grid-table--data-source data-source)
            (image-base-dir (or pic-dir (file-name-directory file-path))))
        (setq preview (grid-table-org--convert-images-to-org-links data-source image-base-dir org-file-dir)))
      preview)))

(defun grid-table-org-insert-block (file-path)
  "Insert a grid-table source block previewing the grid FILE-PATH.
`grid-table-org-refresh-block' draws the preview again and
`grid-table-org-open-block' opens the grid."
  (interactive
   (list (read-file-name "Grid file: ")))
  (let* ((fpath (expand-file-name file-path))
         (preview (grid-table-org--render-preview-from-file fpath nil default-directory)))
    (unless (bolp) (insert "\n"))
    (insert (format "#+BEGIN_SRC grid-table :file %s\n"
                    (grid-table-org--quote-path fpath)))
    (insert preview)
    (unless (string-suffix-p "\n" preview)
      (insert "\n"))
    (insert "#+END_SRC\n")))

(defun grid-table-org--quote-path (path)
  "Return PATH as a block's header line writes it, quoted when it has spaces."
  (if (string-match-p "[ \t]" path) (format "\"%s\"" path) path))

(defun grid-table-org--convert-images-to-org-links (data-source image-base-dir org-file-dir)
  "Convert all image cells in DATA-SOURCE to Org-mode [[file:]] links."
  (let* ((get-row-count-fn (gethash :get-row-count data-source))
         (get-col-count-fn (gethash :get-column-count data-source))
         (get-computed-fn (gethash :get-computed-value-at data-source))
         (get-header-fn (gethash :get-header-value data-source)) ; Get header function
         (num-rows (funcall get-row-count-fn data-source))
         (num-cols (funcall get-col-count-fn data-source))
         (all-rows (cl-loop for r from 0 below num-rows
                            collect (cl-loop for c from 0 below num-cols
                                             collect (grid-get-display-value-at data-source r c))))
         ;; The first row of the grid is its header row, unless that is
         ;; turned off; it heads the Org table once.
         (actual-headers (and (grid-table-header-row-p data-source) (car all-rows)))
         (all-data-rows (if actual-headers (cdr all-rows) all-rows))
         (num-rows (length all-data-rows))
         (column-widths (grid-table-org--calculate-column-widths
                         (or actual-headers (make-list num-cols "")) all-data-rows))
         (buffer-content ""))
    (with-temp-buffer
      (let ((first-row t))
        ;; Insert actual headers if available
        (when actual-headers
          (let ((processed-header-row (grid-table-org--process-row actual-headers column-widths image-base-dir org-file-dir)))
            (insert "| " (string-join processed-header-row " | ") " |\n")
            (insert "|-" (string-join (cl-loop for w in column-widths collect (make-string w ?-) ) "-+-") "-|\n")))

        (cl-loop for r from 0 below num-rows do
                 (let* ((row-values (nth r all-data-rows)) ; Use already fetched all-data-rows
                        (processed-row (grid-table-org--process-row row-values column-widths image-base-dir org-file-dir)))
                   (insert "| " (string-join processed-row "\n| ") " |\n"))))
      (setq buffer-content (buffer-string)))
    buffer-content))

(defun grid-table-org--process-row (row-data column-widths image-base-dir org-file-dir)
  "Process a single row for Org table output, applying wrapping and padding.
Returns a list of strings, each representing a line of the Org table row."
  (let* (;; Phase 1: Process all cells into a list of lists (wrapped lines per cell)
         (processed-cells
          (cl-loop for value in row-data
                   for col-idx from 0
                   for width in column-widths
                   collect (grid-table-org--process-single-cell-for-org-output value image-base-dir org-file-dir width)))
         ;; Phase 2: Calculate the maximum height of the row
         (max-row-height (apply #'max 1 (mapcar #'length processed-cells)))
         (row-lines '()))
    ;; Phase 3: Construct each line of the Org table row
    (cl-loop for line-idx from 0 below max-row-height do
             (let ((line-parts
                    (cl-loop for cell-wrapped-lines in processed-cells
                             for width in column-widths
                             collect (grid-table-org--pad-string (or (nth line-idx cell-wrapped-lines) "") width))))
               (push (string-join line-parts " | ") row-lines)))
    (nreverse row-lines)))

(defun grid-table-org--process-single-cell-for-org-output (value image-base-dir org-file-dir width)
  "Helper to process a single cell value for Org table output, handling image links and wrapping."
  (let* ((processed-value (if (stringp value) value (format "%s" value)))
         (org-link (if (grid-table-org--is-image-value processed-value)
                       (grid-table-org--format-org-link processed-value image-base-dir org-file-dir)
                     processed-value)))
    (if (grid-table-org--is-image-value processed-value)
        (list org-link) ; For image links, return as a single line (no wrapping)
      (grid-table-org--wrap-text org-link width)))) ; For other text, apply wrapping

(defun grid-table-org--calculate-column-widths (headers data-rows)
  "Calculate the optimal column widths for Org table preview.
HEADERS is a list of strings for the column titles.
DATA-ROWS is a list of lists, where each inner list represents a row."
  (when (or headers data-rows)
    (let* ((num-columns (length headers))
           (min-width 5) ; Minimum width for any column
           (max-width 40) ; Maximum width for any column
           (widths (make-list num-columns 0)))

      ;; 1. Calculate initial widths from headers
      (dotimes (i num-columns)
        (setf (nth i widths) (max min-width (string-width (nth i headers)))))

      ;; 2. Expand widths based on data rows
      (dolist (row data-rows)
        (dotimes (i num-columns)
          (when (< i (length row))
            (let* ((cell-content (format "%s" (or (nth i row) "")))
                   (content-width
                    (if (grid-table-org--extract-image-path cell-content)
                        grid-table-org-image-column-width ; Use configured width for images
                      (string-width cell-content))))
              (setf (nth i widths) (max (nth i widths) content-width))))))

      ;; 3. Apply max width constraint
      (dotimes (i num-columns)
        (setf (nth i widths) (min max-width (nth i widths))))
      widths)))

(defun grid-table-org--wrap-text (text width)
  "Wrap TEXT into a list of strings, each fitting within WIDTH."
  (if (<= (string-width text) width)
      (list text)
    (let (lines remaining-text) 
      (setq lines '()) 
      (setq remaining-text text) 
      (while (> (string-width remaining-text) width)
        (let* ((break-pos (or (cl-loop for i from (min (length remaining-text) width) downto 1
                                       when (<= (string-width (substring remaining-text 0 i)) width)
                                       return i)
                              1))
               ;; Try to find a natural break point (space)
               (space-pos (or (cl-position ?\s remaining-text :from-end t :end break-pos)
                              break-pos)))
          (push (substring remaining-text 0 space-pos) lines)
          (setq remaining-text (string-trim (substring remaining-text space-pos)))))
      (push remaining-text lines)
      (nreverse lines))))

(defun grid-table-org--pad-string (text width)
  "Pad TEXT with spaces to fit WIDTH."
  (let* ((text-str (if (stringp text) text (format "%s" text)))
         (padding (- width (string-width text-str))))
    (if (> padding 0)
        (concat text-str (make-string padding ?\s))
      text-str)))

(defun grid-table-org--extract-image-path (text)
  "Extract an image file path from TEXT.
Handles absolute paths, paths with `~`, and org-style links.
Returns the valid, existing path if found, otherwise nil."
  (when (stringp text)
    (save-match-data
      (let ((case-fold-search t) ; Match extensions case-insensitively
            (path
             (cond
              ;; Case 1: Org-style file link: [[file:/path/to/image.png]]
              ((string-match "\\(\\[\\[file:\\([^]]+\\)\\]\\]\\)" text)
               (match-string 2 text))
              ;; Case 2: Bare absolute path or path with ~
              ((string-match
                "\\b\\([~/][^[:space:]'()]*\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\|bmp\\)\\b\\)" text)
               (match-string 1 text))
              (t nil))))
        (when path
          (let ((expanded-path (expand-file-name path)))
            (when (file-exists-p expanded-path)
              expanded-path)))))))

(defun grid-table-org--is-image-value (text)
  (and (stringp text)
       (or (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\|bmp\\)$" (downcase text))
           (string-match-p "\\[\\[file:" text)
           (string-match-p "\\~\\/?" text))))

(defun grid-table-org--format-org-link (text image-base-dir org-file-dir)
  "Convert image path TEXT to an Org [[file:]] link relative to ORG-FILE-DIR.
This function is called from grid-table-org--process-row, which handles wrapping."
  (let* ((clean-path (if (string-match "\\[\\[file:\\([^\\]]+\\)\\]\\]" text)
                         (match-string 1 text)
                       text))
         (image-full-path (expand-file-name clean-path image-base-dir))
         (org-dir (or org-file-dir default-directory))
         (relative-path (file-relative-name image-full-path org-dir)))
    (format "[[file:%s]]" (replace-regexp-in-string "\\\\" "/" relative-path))))

(defun grid-table-org--find-current-block ()
  "Return (BEGIN . END) of the grid-table src block at point, or nil.
BEGIN is the start of its #+BEGIN_SRC line and END the start of its
#+END_SRC line."
  (save-excursion
    (let ((element (org-element-at-point)))
      (when (and (eq (org-element-type element) 'src-block)
                 (equal (org-element-property :language element) "grid-table"))
        (let ((begin (org-element-property :begin element)))
          (goto-char (org-element-property :end element))
          (when (re-search-backward "^[ \t]*#\\+END_SRC" begin t)
            (let ((end (match-beginning 0)))
              (goto-char begin)
              (when (re-search-forward "^[ \t]*#\\+BEGIN_SRC" end t)
                (cons (line-beginning-position) end)))))))))

(defun grid-table-org--header-arg (params name)
  "Return the value of the header argument NAME in PARAMS, or nil.
A value with spaces is written in double quotes."
  (when (string-match (concat "\\(?:^\\|[ \t]\\)" (regexp-quote name)
                              "[ \t]+\\(?:\"\\([^\"]+\\)\"\\|\\([^ \t]+\\)\\)")
                      params)
    (or (match-string 1 params) (match-string 2 params))))

(defun grid-table-org--get-file-params-at-begin ()
  "Return (:file FILE :pic-dir DIR) of the #+BEGIN_SRC grid-table line at point.
Either is nil when the line does not give it; the result is nil when the
line is not such a line."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position)))
        (case-fold-search t))
    (when (string-match "^[ \t]*#\\+BEGIN_SRC[ \t]+grid-table\\(.*\\)$" line)
      (let* ((params (match-string 1 line))
             (file (grid-table-org--header-arg params ":file"))
             (pic-dir (grid-table-org--header-arg params ":pic-dir")))
        (list :file (and file (expand-file-name file))
              :pic-dir (and pic-dir (expand-file-name pic-dir)))))))

(defun grid-table-org--block-file ()
  "Return the bounds and header arguments of the grid-table block at point."
  (let ((bounds (grid-table-org--find-current-block)))
    (unless bounds (user-error "Not inside a grid-table src block"))
    (let ((params (save-excursion
                    (goto-char (car bounds))
                    (grid-table-org--get-file-params-at-begin))))
      (unless (plist-get params :file)
        (user-error "Missing :file parameter on src block line"))
      (cons bounds params))))

(defun grid-table-org-refresh-block ()
  "Draw the preview of the grid-table src block at point again from its :file."
  (interactive)
  (pcase-let* ((`((,begin . ,end) . ,params) (grid-table-org--block-file))
               (preview (grid-table-org--render-preview-from-file
                         (plist-get params :file) (plist-get params :pic-dir)
                         default-directory)))
    (save-excursion
      (goto-char begin)
      (forward-line 1)
      (let ((inhibit-read-only t))
        (delete-region (point) end)
        (insert preview)
        (unless (string-suffix-p "\n" preview)
          (insert "\n"))))))

(defun grid-table-org-open-block ()
  "Open the grid file of the grid-table src block at point."
  (interactive)
  (grid-table-find-file (plist-get (cdr (grid-table-org--block-file)) :file)))

(provide 'grid-table-org)

