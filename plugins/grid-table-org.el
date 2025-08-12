;;; grid-table-org.el --- Org integration for grid-table -*- lexical-binding: t; -*-

(require 'grid-table)
(require 'grid-table-persistence)

;; Minimal Org Block integration (only :file supported)
;; Block form:
;;   #+BEGIN_SRC grid-table :file /absolute/path/to/file.grid
;;   <static preview content>
;;   #+END_SRC

(defun grid-table-org--render-preview-from-file (file-path &optional pic-dir)
  "Render a static preview string for FILE-PATH (.grid) with smart image linking.
PIC-DIR specifies the base directory for image references."
  (unless (and file-path (file-exists-p file-path))
    (error "Grid file not found: %s" file-path))
  (let* ((data-source (grid-table-persistence-create-data-source-from-file file-path))
         (preview ""))
    (with-temp-buffer
      (let ((grid-table--data-source data-source)
            ;; Configure image directory for relative paths
            (relative-base (or pic-dir (file-name-directory file-path))))
        ;; Convert all image values to Org format
        (setq preview (grid-table-org--convert-images-to-org-links data-source file-path relative-base)))
      preview)))

(defun grid-table-org-insert-block (file-path &optional pic-dir)
  "Insert a grid-table source block with preview for FILE-PATH.
PIC-DIR optionally specifies the image reference base directory."
  (interactive
   (let ((file (read-file-name "Grid file: ")))
     (list file
           (when (y-or-n-p "Set custom image directory? ")
             (read-directory-name "Image directory: " (file-name-directory file))))))
  (let ((inhibit-read-only t))
    (let* ((fpath (expand-file-name file-path))
           (pic-path (when pic-dir (expand-file-name pic-dir)))
           (preview (grid-table-org--render-preview-from-file fpath pic-path)))
      (insert (format "#+BEGIN_SRC grid-table :file %s%s\n" 
                      fpath 
                      (if pic-path (format " :pic-dir %s" pic-path) "")))
      (insert preview)
      (unless (string-suffix-p "\n" preview)
        (insert "\n"))
      (insert "#+END_SRC\n"))))

(defun grid-table-org--convert-images-to-org-links (data-source file-path pic-dir)
  "Convert all image cells in DATA-SOURCE to Org-mode [[file:]] links.
FILE-PATH is the source .grid file, PIC-DIR is the image reference base directory."
  (let* ((get-row-count-fn (gethash :get-row-count data-source))
         (get-col-count-fn (gethash :get-column-count data-source))
         (get-header-fn (gethash :get-header-value data-source))
         (get-computed-fn (gethash :get-computed-value-at data-source))
         (num-rows (funcall get-row-count-fn data-source))
         (num-cols (funcall get-col-count-fn data-source))
         (headers (cl-loop for c from 0 below num-cols
                           collect (funcall get-header-fn data-source c)))
         (buffer-content ""))
    (with-temp-buffer
      ;; Build table header
      (insert "| " (string-join headers " | ") " |\n")
      (insert "|" (make-string (length headers) ?-) "|\n")
      
      ;; Build data rows with image conversion
      (cl-loop for r from 0 below num-rows do
               (let ((row-values (cl-loop for c from 0 below num-cols
                                        collect (funcall get-computed-fn data-source r c))))
                 (cl-loop for value in row-values
                          for c from 0 collect
                          (let ((processed-value (if (stringp value) value (format "%s" value))))
                            ;; Detect absolute/relative image paths and convert to Org links
                            (if (grid-table-org--is-image-value processed-value)
                                (grid-table-org--format-org-link processed-value file-path pic-dir)
                              processed-value)))
                 into processed-row
                 finally (insert "| " (string-join processed-row " | ") " |\n")))
      (setq buffer-content (buffer-string)))
    buffer-content))

(defun grid-table-org--is-image-value (text)
  "Check if TEXT contains or is an image file path reference."
  (and (stringp text)
       (or (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\|bmp\\)$" (downcase text))
           (string-match-p "\\[\\[file:" text)
           (string-match-p "\\~\\/?" text))))

(defun grid-table-org--format-org-link (text file-path pic-dir)
  "Convert image path TEXT to relative Org [[file:]] link."
  (let* ((absolute-base (if pic-dir (expand-file-name pic-dir) (file-name-directory file-path)))
         (clean-path (if (stringp text)
                         ;; Extract path from org links or clean bare path
                         (or (and (string-match "\\[\\[file:\\([^\\]]+\\)\\]\\]" text) (match-string 1 text))
                             text)
                       text))
         (full-path (expand-file-name clean-path absolute-base))
         (relative-path (file-relative-name full-path absolute-base)))
    ;; Ensure Org links always use forward slashes
    (format "[[file:%s]]" (replace-regexp-in-string "\\\\" "/" relative-path))))

(defun grid-table-org--find-current-block ()
  "Return (BEGIN-POS . END-POS) for current grid-table src block, or nil."
  (save-excursion)

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
          (when (string-match ":file\\s-+\\([^\\s-][^:
]*?\\)\\(?:\\s-\\|$\.*\)?" params)
            (setf (plist-get result :file) (expand-file-name (match-string 1 params))))
          ;; Parse :pic-dir parameter
          (when (string-match ":pic-dir\\s-+\\([^\\s-][^:
]*?\\)\\(?:\\s-\\|$\.*\)?" params)
            (setf (plist-get result :pic-dir) (expand-file-name (match-string 1 params))))
          result))))))

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
        (let ((preview (grid-table-org--render-preview-from-file file pic-dir)))
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
      (let ((file (grid-table-org--get-file-param-at-begin)))
        (unless file (error "Missing :file parameter on src block line"))
        (grid-table-find-file file)))))

(provide 'grid-table-org)


