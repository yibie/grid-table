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
  "Insert a grid-table source block with preview for FILE-PATH."
  (interactive
   (list (read-file-name "Grid file: ")))
  (let ((inhibit-read-only t))
    (let* ((fpath (expand-file-name file-path))
           (preview (grid-table-org--render-preview-from-file fpath nil default-directory)))
      (insert (format "# Table generated from: %s\n" fpath))
      (insert preview)
      (unless (string-suffix-p "\n" preview)
        (insert "\n")))))

(defun grid-table-org--convert-images-to-org-links (data-source image-base-dir org-file-dir)
  "Convert all image cells in DATA-SOURCE to Org-mode [[file:]] links."
  (let* ((get-row-count-fn (gethash :get-row-count data-source))
         (get-col-count-fn (gethash :get-column-count data-source))
         (get-computed-fn (gethash :get-computed-value-at data-source))
         (get-header-fn (gethash :get-header-value data-source)) ; Get header function
         (num-rows (funcall get-row-count-fn data-source))
         (num-cols (funcall get-col-count-fn data-source))
         (actual-headers (gethash :headers data-source)) ; Original headers from data-source
         (all-data-rows (cl-loop for r from 0 below num-rows ; Get all data rows
                                 collect (cl-loop for c from 0 below num-cols
                                                 collect (funcall get-computed-fn data-source r c))))
         (column-widths (grid-table-org--calculate-column-widths actual-headers all-data-rows))
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
  "Return (BEGIN-POS . END-POS) for current grid-table src block, or nil."
  (save-excursion
    (let ((element (org-element-at-point)))
      (when (and (eq (org-element-type element) 'src-block)
                 (string= (org-element-property :language element) "grid-table"))
        (cons (org-element-property :begin element)
              (org-element-property :end element))))))

(defun grid-table-org--get-file-params-at-begin ()
  "Parse :file and optional :pic-dir from current line if it's a grid-table src block."
  (save-excursion
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
          result)
      (when (string-match "^#\\+BEGIN_SRC\\s-+grid-table\\(.*\\)$" line)
        (let ((params (match-string 1 line)))
          (setf result (list :file nil :pic-dir nil))
          ;; Parse :file parameter
          (when (string-match ":file\\s-+\\([^\\s-][^:]*?\\)\\(?:\\s-\\|$\.*\)?" params)
            (setf (plist-get result :file) (expand-file-name (match-string 1 params))))
          ;; Parse :pic-dir parameter
          (when (string-match ":pic-dir\\s-+\\([^\\s-][^:]*?\\)\\(?:\\s-\\|$\.*\)?" params)
            (setf (plist-get result :pic-dir) (expand-file-name (match-string 1 params))))
          result)))))

(defun grid-table-org-refresh-block ()
  "Refresh the current grid-table src block preview from its :file and :pic-dir."
  (interactive)
  (save-excursion
    (let* ((bounds (grid-table-org--find-current-block)))
      (unless bounds (error "Not inside a grid-table src block"))
      (goto-char (car bounds))
      (let* ((file-params (grid-table-org--get-file-params-at-begin))
             (file (plist-get file-params :file))
             (pic-dir (plist-get file-params :pic-dir)))
        (unless file (error "Missing :file parameter on src block line"))
        (let ((preview (grid-table-org--render-preview-from-file file pic-dir default-directory)))
          (forward-line 1)
          (let ((content-start (point)))
            (goto-char (cdr bounds))
            (forward-line -1)
            (let ((inhibit-read-only t))
              (delete-region content-start (point)))
            (goto-char content-start)
            (insert preview)))))))

(defun grid-table-org-open-block ()
  "Open the grid file referenced by the current grid-table src block."
  (interactive)
  (save-excursion
    (let* ((bounds (grid-table-org--find-current-block)))
      (unless bounds (error "Not inside a grid-table src block"))
      (goto-char (car bounds))
      (let* ((file-params (grid-table-org--get-file-params-at-begin))
             (file (plist-get file-params :file)))
        (unless file (error "Missing :file parameter on src block line"))
        (grid-table-find-file file)))))

(provide 'grid-table-org)

