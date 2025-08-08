;;; grid-table-markdown.el --- Markdown fenced block integration plugin (static preview + active editing) -*- lexical-binding: t; -*-

(require 'grid-table)
(require 'grid-table-persistence)

;; Design: Use Markdown fenced block as container
;; Example:
;; ```grid-view file=/absolute/path/to/table.grid
;; <static preview (Unicode border table)>
;; ```

(defun grid-table-markdown--render-preview-from-file (file-path)
  "Render static preview string from FILE-PATH (.grid)."
  (unless (and file-path (file-exists-p file-path))
    (error "Grid file not found: %s" file-path))
  (let* ((data-source (grid-table-persistence-create-data-source-from-file file-path))
         (preview ""))
    (with-temp-buffer
      ;; Reuse core build function to generate plain text table
      (let ((grid-table--data-source data-source))
        (grid-table--insert-table-from-data-source))
      (setq preview (buffer-string)))
    preview))

(defun grid-table-markdown-insert-block (file-path)
  "Insert a Markdown fenced block and fill static preview, pointing to FILE-PATH."
  (interactive "fGrid file: ")
  (let* ((abs (expand-file-name file-path))
         (preview (grid-table-markdown--render-preview-from-file abs)))
    (insert (format "```grid-view file=%s\n" abs))
    (let ((content-start (point)))
      (insert preview)
      (unless (string-suffix-p "\n" preview)
        (insert "\n"))
      (let ((content-end (point)))
        (add-text-properties content-start content-end '(read-only t))))
    (insert "```\n")))

(defun grid-table-markdown--find-current-block ()
  "Find the grid-view fenced block surrounding the current point, return (BEGIN . END), otherwise nil."
  (save-excursion
    (let ((case-fold-search t)
          begin end)
      (when (re-search-backward "^```grid-view\\(.*\\)$" nil t)
        (setq begin (line-beginning-position))
        (when (re-search-forward "^```$" nil t)
          (setq end (line-end-position))
          (cons begin end))))))

(defun grid-table-markdown--get-file-param-at-begin ()
  "Parse file= parameter on current line (should be ```grid-view ...) and return absolute path or nil."
  (save-excursion
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (when (string-match "^```grid-view\\(.*\\)$" line)
        (let ((params (match-string 1 line)))
          (when (string-match "file=\\([^\\s-].*\)" params)
            (expand-file-name (string-trim (match-string 1 params)))))))))

(defun grid-table-markdown-refresh-block ()
  "Refresh static preview content of current grid-view fenced block."
  (interactive)
  (save-excursion
    (let* ((bounds (grid-table-markdown--find-current-block)))
      (unless bounds (error "Not inside a grid-view fenced block"))
      (goto-char (car bounds))
      (let* ((file (grid-table-markdown--get-file-param-at-begin)))
        (unless file (error "Missing file= parameter on ```grid-view line"))
        (let ((preview (grid-table-markdown--render-preview-from-file file)))
          ;; Replace content: from next line after start to previous line before end
          (forward-line 1)
          (let ((content-start (point)))
            (goto-char (cdr bounds))
            (forward-line -1)
            (let ((inhibit-read-only t))
              (delete-region content-start (point)))
            (goto-char content-start)
            (let ((insert-start (point)))
              (insert preview)
              (unless (string-suffix-p "\n" preview)
                (insert "\n"))
              (let ((insert-end (point)))
                (add-text-properties insert-start insert-end '(read-only t))))))))))

(defun grid-table-markdown-open-block ()
  "Open the .grid file referenced by the current grid-view fenced block for interactive editing."
  (interactive)
  (save-excursion
    (let* ((bounds (grid-table-markdown--find-current-block)))
      (unless bounds (error "Not inside a grid-view fenced block"))
      (goto-char (car bounds))
      (let ((file (grid-table-markdown--get-file-param-at-begin)))
        (unless file (error "Missing file= parameter on ```grid-view line"))
        (grid-table-find-file file)))))

(provide 'grid-table-markdown)


