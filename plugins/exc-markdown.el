;;; exc-markdown.el --- Markdown fenced block integration plugin (static preview + active editing) -*- lexical-binding: t; -*-

(require 'exc)
(require 'exc-persistence)

;; Design: Use Markdown fenced block as container
;; Example:
;; ```grid-view file=/absolute/path/to/table.grid
;; <static preview (Unicode border table)>
;; ```

(defun exc-markdown--render-preview-from-file (file-path)
  "Render static preview string from FILE-PATH (.grid)."
  (unless (and file-path (file-exists-p file-path))
    (error "Grid file not found: %s" file-path))
  (let* ((data-source (exc-persistence-create-data-source-from-file file-path))
         (preview (exc-text-render data-source)))
    preview))

(defun exc-markdown-insert-block (file-path)
  "Insert a grid-view fenced block previewing the grid FILE-PATH.
`exc-markdown-refresh-block' draws the preview again and
`exc-markdown-open-block' opens the grid."
  (interactive (list (read-file-name "Grid file: ")))
  (let* ((abs (expand-file-name file-path))
         (preview (exc-markdown--render-preview-from-file abs)))
    (unless (bolp) (insert "\n"))
    (insert (format "```grid-view file=%s\n" (exc-markdown--quote-path abs)))
    (insert preview)
    (unless (string-suffix-p "\n" preview)
      (insert "\n"))
    (insert "```\n")))

(defun exc-markdown--quote-path (path)
  "Return PATH as a block's opening line writes it, quoted when it has spaces."
  (if (string-match-p "[ \t]" path) (format "\"%s\"" path) path))

(defun exc-markdown--find-current-block ()
  "Return (BEGIN . END) of the grid-view fenced block at point, or nil.
BEGIN is the start of its opening line and END the start of its closing
line."
  (let ((pos (point)))
    (save-excursion
      (end-of-line)
      (when (re-search-backward "^```grid-view\\b" nil t)
        (let ((begin (point)))
          (forward-line 1)
          (when (re-search-forward "^```[ \t]*$" nil t)
            (let ((end (match-beginning 0)))
              (when (<= pos (match-end 0))
                (cons begin end)))))))))

(defun exc-markdown--get-file-param-at-begin ()
  "Return the absolute file= path of the ```grid-view line at point, or nil."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (when (string-match "^```grid-view\\(.*\\)$" line)
      (let ((params (match-string 1 line)))
        (when (string-match "\\(?:^\\|[ \t]\\)file=\\(?:\"\\([^\"]+\\)\"\\|\\([^ \t]+\\)\\)"
                            params)
          (expand-file-name (or (match-string 1 params) (match-string 2 params))))))))

(defun exc-markdown--block-file ()
  "Return the bounds and file of the grid-view block at point."
  (let ((bounds (exc-markdown--find-current-block)))
    (unless bounds (user-error "Not inside a grid-view fenced block"))
    (let ((file (save-excursion
                  (goto-char (car bounds))
                  (exc-markdown--get-file-param-at-begin))))
      (unless file (user-error "Missing file= parameter on ```grid-view line"))
      (cons bounds file))))

(defun exc-markdown-refresh-block ()
  "Draw the preview of the grid-view fenced block at point again from its file."
  (interactive)
  (pcase-let* ((`((,begin . ,end) . ,file) (exc-markdown--block-file))
               (preview (exc-markdown--render-preview-from-file file)))
    (save-excursion
      (goto-char begin)
      (forward-line 1)
      (let ((inhibit-read-only t))
        (delete-region (point) end)
        (insert preview)
        (unless (string-suffix-p "\n" preview)
          (insert "\n"))))))

(defun exc-markdown-open-block ()
  "Open the grid file of the grid-view fenced block at point."
  (interactive)
  (exc-find-file (cdr (exc-markdown--block-file))))

(provide 'exc-markdown)


