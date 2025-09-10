;;; grid-table-example-plugin.el --- Example plugin for grid-table -*- lexical-binding: t -*-

(require 'grid-table-plugins)
(require 'grid-table-core)

;;;----------------------------------------------------------------------
;;; Example Plugin: Enhanced Text Rendering
;;;----------------------------------------------------------------------

(defun grid-table-example-text-renderer (value &optional cell-props)
  "Enhanced text renderer with syntax highlighting."
  (if (and value (stringp value))
      (let ((rendered value))
        ;; Highlight URLs
        (setq rendered (replace-regexp-in-string 
                        "https?://[^\\s]+" 
                        (lambda (url)
                          (propertize url 'face '(:foreground "blue" :underline t)))
                        rendered))
        ;; Highlight email addresses
        (setq rendered (replace-regexp-in-string 
                        "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
                        (lambda (email)
                          (propertize email 'face '(:foreground "green")))
                        rendered))
        ;; Highlight phone numbers
        (setq rendered (replace-regexp-in-string 
                        "\\b\\d{3}[-.]?\\d{3}[-.]?\\d{4}\\b"
                        (lambda (phone)
                          (propertize phone 'face '(:foreground "orange")))
                        rendered))
        rendered)
    ""))

;;;----------------------------------------------------------------------
;;; Example Plugin: Currency Renderer
;;;----------------------------------------------------------------------

(defun grid-table-currency-renderer (value &optional cell-props)
  "Render currency values with proper formatting."
  (if (and value (stringp value))
      (let ((amount (string-to-number value)))
        (if (numberp amount)
            (format "$%.2f" amount)
          value))
    ""))

;;;----------------------------------------------------------------------
;;; Example Plugin: Progress Bar Renderer
;;;----------------------------------------------------------------------

(defun grid-table-progress-renderer (value &optional cell-props)
  "Render progress values as visual bars."
  (if (and value (stringp value))
      (let* ((percentage (string-to-number value))
             (bar-width 20)
             (filled-width (round (* percentage bar-width 0.01)))
             (bar (make-string bar-width ?-)))
        (when (and (numberp percentage) (>= percentage 0) (<= percentage 100))
          (dotimes (i filled-width)
            (aset bar i ?█))
          (format "[%s] %d%%" bar percentage)))
    ""))

;;;----------------------------------------------------------------------
;;; Example Plugin: Custom Data Source
;;;----------------------------------------------------------------------

(defun grid-table-example-create-data-source (data-list &optional title)
  "Create a data source from a list of alists.
DATA-LIST should be a list of alists where each alist represents a row.
TITLE is an optional string for the grid title."
  (when data-list
    (let* ((headers (mapcar #'car (car data-list)))
           (data (mapcar #'cdr data-list))
           (source (make-default-data-source headers data title)))
      ;; Add custom functionality
      (puthash :custom-filter
               (lambda (s filter-func)
                 (let ((model (gethash :model s))
                       (filtered-data '()))
                   (dotimes (r (grid-data-model-rows model))
                     (when (funcall filter-func r)
                       (push (grid-get-raw-value-at s r 0) filtered-data)))
                   (nreverse filtered-data)))
               source)
      source)))

;;;----------------------------------------------------------------------
;;; Example Plugin: Custom Commands
;;;----------------------------------------------------------------------

(defun grid-table-example-highlight-duplicates ()
  "Highlight duplicate values in the current grid."
  (interactive)
  (when grid-table--data-source
    (let ((duplicates (make-hash-table :test 'equal))
          (rows (grid-get-row-count grid-table--data-source))
          (cols (grid-get-column-count grid-table--data-source)))
      ;; Find duplicates
      (dotimes (r rows)
        (dotimes (c cols)
          (let ((value (grid-get-raw-value-at grid-table--data-source r c)))
            (when (and value (not (string-empty-p value)))
              (puthash value (1+ (gethash value duplicates 0)) duplicates)))))
      ;; Highlight duplicates
      (dotimes (r rows)
        (dotimes (c cols)
          (let ((value (grid-get-raw-value-at grid-table--data-source r c)))
            (when (and value (> (gethash value duplicates 0) 1))
              (grid-table-highlight-cell r c))))))))

(defun grid-table-example-export-to-html ()
  "Export current grid to HTML format."
  (interactive)
  (when grid-table--data-source
    (let* ((rows (grid-get-row-count grid-table--data-source))
           (cols (grid-get-column-count grid-table--data-source))
           (html '("<table border=\"1\">"))
           (filename (read-file-name "Save HTML to: ")))
      ;; Add headers
      (push "<tr>" html)
      (dotimes (c cols)
        (push (format "<th>%s</th>" (grid-get-header-value grid-table--data-source c)) html))
      (push "</tr>" html)
      ;; Add data rows
      (dotimes (r rows)
        (push "<tr>" html)
        (dotimes (c cols)
          (push (format "<td>%s</td>" (grid-get-raw-value-at grid-table--data-source r c)) html))
        (push "</tr>" html))
      (push "</table>" html)
      ;; Write to file
      (with-temp-file filename
        (insert (mapconcat #'identity (nreverse html) "\n")))
      (message "HTML exported to: %s" filename))))

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun grid-table-example-plugin-init ()
  "Initialize the example plugin."
  ;; Register cell renderers
  (grid-table-register-cell-renderer 'enhanced-text #'grid-table-example-text-renderer)
  (grid-table-register-cell-renderer 'currency #'grid-table-currency-renderer)
  (grid-table-register-cell-renderer 'progress #'grid-table-progress-renderer)
  
  ;; Register data source type
  (grid-table-register-data-source-type 'example #'grid-table-example-create-data-source)
  
  ;; Register plugin
  (grid-table-register-plugin 'example-plugin)
  
  (message "Example plugin initialized"))

;;;----------------------------------------------------------------------
;;; Usage Examples
;;;----------------------------------------------------------------------

(defun grid-table-example-demo ()
  "Demonstrate the example plugin functionality."
  (interactive)
  (let* ((data '((("Name" . "Alice") ("Email" . "alice@example.com") ("Phone" . "555-1234"))
                  (("Name" . "Bob") ("Email" . "bob@example.com") ("Phone" . "555-5678"))
                  (("Name" . "Charlie") ("Email" . "charlie@example.com") ("Phone" . "555-9012"))))
         (data-source (grid-table-example-create-data-source data "Example Demo")))
    (grid-table-api-create
     :data-source data-source
     :columns (grid-table-create-columns
               (grid-table-define-column "Name" :type 'enhanced-text :width 20)
               (grid-table-define-column "Email" :type 'enhanced-text :width 30)
               (grid-table-define-column "Phone" :type 'enhanced-text :width 15))
     :title "Example Plugin Demo")))

(defun grid-table-example-currency-demo ()
  "Demonstrate currency rendering."
  (interactive)
  (let* ((headers '("Item" "Price" "Progress"))
         (data '(("Laptop" "1200.50" "75")
                 ("Mouse" "25.99" "90")
                 ("Keyboard" "75.00" "45")))
         (data-source (make-default-data-source headers data "Currency Demo")))
    (grid-table-api-create
     :data-source data-source
     :columns (grid-table-create-columns
               (grid-table-define-column "Item" :type 'text :width 15)
               (grid-table-define-column "Price" :type 'currency :width 12)
               (grid-table-define-column "Progress" :type 'progress :width 25))
     :title "Currency and Progress Demo")))

(provide 'grid-table-example-plugin)
