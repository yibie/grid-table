;;; grid-table-org.el --- Org integration for grid-table -*- lexical-binding: t; -*-

(require 'grid-table)
(require 'grid-table-persistence)

;; Minimal Org Block integration (only :file supported)
;; Block form:
;;   #+BEGIN_GRID_VIEW :file /absolute/path/to/file.grid
;;   <static preview content>
;;   #+END_GRID_VIEW

(defun grid-table-org--render-preview-from-file (file-path)
  "Render a static preview string for FILE-PATH (.grid)."
  (unless (and file-path (file-exists-p file-path))
    (error "Grid file not found: %s" file-path))
  (let* ((data-source (grid-table-persistence-create-data-source-from-file file-path))
         (preview ""))
    (with-temp-buffer
      ;; Reuse the core table builder to generate the static text
      (let ((grid-table--data-source data-source))
        (grid-table--insert-table-from-data-source))
      (setq preview (buffer-string)))
    preview))

(defun grid-table-org-insert-block (file-path)
  "Insert a GRID_VIEW org block with preview for FILE-PATH."
  (interactive "fGrid file: ")
  (let ((preview (grid-table-org--render-preview-from-file file-path)))
    (insert (format "#+BEGIN_GRID_VIEW :file %s\n" (expand-file-name file-path)))
    (let ((content-start (point)))
      (insert preview)
      (unless (string-suffix-p "\n" preview)
        (insert "\n"))
      (let ((content-end (point)))
        (add-text-properties content-start content-end '(read-only t))))
    (insert "#+END_GRID_VIEW\n")))

(defun grid-table-org--find-current-block ()
  "Return (BEGIN-POS . END-POS) for current GRID_VIEW block, or nil."
  (save-excursion
    (let ((case-fold-search t)
          begin end)
      ;; Find begin
      (when (re-search-backward "^#\\+BEGIN_GRID_VIEW\\(.*\\)$" nil t)
        (setq begin (line-beginning-position))
        ;; Find corresponding end
        (when (re-search-forward "^#\\+END_GRID_VIEW$" nil t)
          (setq end (line-end-position))
          (cons begin end))))))

(defun grid-table-org--get-file-param-at-begin ()
  "Parse :file param from current line if it is a BEGIN_GRID_VIEW line."
  (save-excursion
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (when (string-match "^#\\+BEGIN_GRID_VIEW\\(.*\\)$" line)
        (let ((params (match-string 1 line)))
          (when (string-match ":file\\s-+\\([^\\s-].*?\)\\s-*$" params)
            (expand-file-name (match-string 1 params))))))))

(defun grid-table-org-refresh-block ()
  "Refresh the current GRID_VIEW block preview from its :file."
  (interactive)
  (save-excursion
    (let* ((bounds (grid-table-org--find-current-block)))
      (unless bounds (error "Not inside a GRID_VIEW block"))
      (goto-char (car bounds))
      (let* ((file (grid-table-org--get-file-param-at-begin)))
        (unless file (error "Missing :file parameter on BEGIN_GRID_VIEW"))
        (let ((preview (grid-table-org--render-preview-from-file file)))
          ;; Replace content between begin/end lines (exclusive)
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

(defun grid-table-org-open-block ()
  "Open the grid file referenced by the current GRID_VIEW block."
  (interactive)
  (save-excursion
    (let* ((bounds (grid-table-org--find-current-block)))
      (unless bounds (error "Not inside a GRID_VIEW block"))
      (goto-char (car bounds))
      (let ((file (grid-table-org--get-file-param-at-begin)))
        (unless file (error "Missing :file parameter on BEGIN_GRID_VIEW"))
        (grid-table-find-file file)))))

(provide 'grid-table-org)


