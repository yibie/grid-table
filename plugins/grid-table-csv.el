;;; grid-table-csv.el --- A CSV data source for grid-table -*- lexical-binding: t; -*-

(require 'grid-table)
(require 'grid-table-plugins)
(require 'grid-table-api)

(defun grid-table-csv--parse-line (line)
  "A proper CSV line parser that handles quoted fields correctly."
  (let ((fields '())
        (start 0)
        (in-quote nil)
        (i 0))
    (while (< i (length line))
      (let ((char (aref line i)))
        (cond
         ;; Handle quotes
         ((and (eq char ?\") (not in-quote))
          (setq in-quote t)
          (setq start (1+ i))
          (setq i (1+ i)))
         ((and (eq char ?\") in-quote)
          (setq in-quote nil)
          (push (substring line start i) fields)
          (setq start (+ i 2))
          (setq i (+ i 2)))
         ;; Handle comma outside of quotes
         ((and (eq char ?,) (not in-quote))
          (push (substring line start i) fields)
          (setq start (1+ i))
          (setq i (1+ i)))
         ;; Just advance
         (t (setq i (1+ i))))))
    ;; Add the last field
    (when (< start (length line))
      (push (substring line start) fields))
    (nreverse fields)))

(defun grid-table-csv--format-line (fields)
  "Format a list of fields into a CSV line with proper quoting."
  (mapconcat
   (lambda (field)
     (let ((needs-quotes (or (string-match "[,\"\n\r]" field)
                             (string-match "^\\s-\\|\\s-$" field))))
       (if needs-quotes
           (concat "\"" (replace-regexp-in-string "\"" "\"\"" field) "\"")
         field)))
   fields
   ","))

(defun grid-table-csv--save-data-source (data-source file-path)
  "Save DATA-SOURCE to FILE-PATH as CSV, one line per row.
The header line is the grid's first row, as it was read."
  (let* ((model (gethash :model data-source))
         (rows (grid-data-model-rows model))
         (cols (grid-data-model-cols model))
         (lines '()))
    (dotimes (r rows)
      (let ((row '()))
        (dotimes (c cols)
          (push (or (grid-cell-raw-input (grid-model-get-cell model r c)) "") row))
        (push (grid-table-csv--format-line (nreverse row)) lines)))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file-path
        (insert (mapconcat #'identity (nreverse lines) "\n"))
        (insert "\n")))))

(defun grid-table-csv-create-data-source (file-path)
  "Create a grid-table data source from the CSV file FILE-PATH.
Its first line is the grid's first row, as a .grid file's header line is."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (let* ((lines (split-string (buffer-string) "\r?\n" t))
             (csv-rows (mapcar #'grid-table-csv--parse-line lines))
             (csv-headers (car csv-rows))
             (source (make-default-data-source csv-headers csv-rows
                                               (file-name-nondirectory file-path))))
        ;; Add CSV-specific functionality
        (puthash :file-path file-path source)
        (puthash :save-to-file
                 (lambda (s path)
                   (grid-table-csv--save-data-source s (or path (gethash :file-path s))))
                 source)
        source))))

(defun grid-table-find-file-csv (file-path)
  "Open the CSV file FILE-PATH as a grid.
The buffer visits the file, so `save-buffer' writes the grid back to it
as CSV."
  (interactive "fFind CSV file: ")
  (unless (file-exists-p file-path)
    (user-error "No such file: %s" file-path))
  (let ((buffer (find-file-noselect file-path)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'grid-table-mode)
        (let ((source (grid-table-csv-create-data-source buffer-file-name)))
          (setq-local grid-table--file-path buffer-file-name)
          (setq-local grid-table--title (gethash :title source))
          (let ((inhibit-read-only t))
            (erase-buffer))
          (grid-table--show-grid-in-current-buffer source)
          (set-buffer-modified-p nil))))
    (switch-to-buffer buffer)))

(defun grid-table-csv-save (file-path)
  "Save the current grid-table data back to a CSV file."
  (interactive "FSave to CSV file: ")
  (when-let ((data-source grid-table--data-source)
             (save-func (gethash :save-to-file data-source)))
    (funcall save-func data-source file-path)
    (message "CSV file saved to: %s" file-path)))

(defun grid-table-csv-save-overwrite ()
  "Save the current grid-table data back to the original CSV file."
  (interactive)
  (when-let ((data-source grid-table--data-source)
             (original-path (gethash :file-path data-source))
             (save-func (gethash :save-to-file data-source)))
    (funcall save-func data-source original-path)
    (message "CSV file saved to: %s" original-path)))

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun grid-table-csv-init ()
  "Initialize the CSV plugin."
  ;; Register the plugin
  (grid-table-register-plugin 'csv)
  ;; Register CSV data source type
  (grid-table-register-data-source-type 'csv #'grid-table-csv-create-data-source)
  ;; Register CSV-specific cell renderer if needed
  (grid-table-register-cell-renderer 'csv-text
    (lambda (value &optional cell-props)
      (if (and value (stringp value))
          (format "%s" value)
        ""))))

;; Initialize the plugin
(grid-table-csv-init)

(provide 'grid-table-csv)
