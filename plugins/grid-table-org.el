;;; grid-table-org.el --- Org integration for grid-table -*- lexical-binding: t; -*-

(require 'grid-table)
(require 'grid-table-persistence)

;; Minimal Org Block integration (only :file supported)
;; Block form:
;;   #+BEGIN_SRC grid-table :file /absolute/path/to/file.grid
;;   <static preview content>
;;   #+END_SRC

(defun grid-table-org--render-preview-from-file (file-path)
  "Render a static preview string for FILE-PATH (.grid)."
  (unless (and file-path (file-exists-p file-path))
    (error "Grid file not found: %s" file-path))
  (let* ((data-source (grid-table-persistence-create-data-source-from-file file-path))
         (preview ""))
    (with-temp-buffer
      (let ((grid-table--data-source data-source))
        (grid-table--insert-table-from-data-source))
      (setq preview (buffer-string)))
    preview))

(defun grid-table-org-insert-block (file-path)
  "Insert a grid-table source block with preview for FILE-PATH."
  (interactive "fGrid file: ")
  (let ((inhibit-read-only t))
    (let* ((fpath (expand-file-name file-path))
           (preview (grid-table-org--render-preview-from-file fpath)))
      (insert (format "#+BEGIN_SRC grid-table :file %s\n" fpath))
      (insert preview)
      (unless (string-suffix-p "\n" preview)
        (insert "\n"))
      (insert "#+END_SRC\n"))))

(defun grid-table-org--find-current-block ()
  "Return (BEGIN-POS . END-POS) for current grid-table src block, or nil."
  (save-excursion
    (let ((case-fold-search t)
          begin end)
      (when (re-search-backward "^#\\+BEGIN_SRC\\s-+grid-table" nil t)
        (setq begin (line-beginning-position))
        (when (re-search-forward "^#\\+END_SRC" nil t)
          (setq end (line-end-position))
          (cons begin end))))))

(defun grid-table-org--get-file-param-at-begin ()
  "Parse :file param from current line if it is a grid-table src block line."
  (save-excursion
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (when (string-match "^#\\+BEGIN_SRC\\s-+grid-table\\(.*\\)$" line)
        (let ((params (match-string 1 line)))
          (when (string-match ":file\\s-+\\([^\\s-].*?\\)\\s-*$" params)
            (expand-file-name (match-string 1 params))))))))

(defun grid-table-org-refresh-block ()
  "Refresh the current grid-table src block preview from its :file."
  (interactive)
  (save-excursion
    (let* ((bounds (grid-table-org--find-current-block)))
      (unless bounds (error "Not inside a grid-table src block"))
      (goto-char (car bounds))
      (let* ((file (grid-table-org--get-file-param-at-begin)))
        (unless file (error "Missing :file parameter on src block line"))
        (let ((preview (grid-table-org--render-preview-from-file file)))
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


