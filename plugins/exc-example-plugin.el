;;; exc-example-plugin.el --- Example plugin for exc -*- lexical-binding: t -*-

(require 'exc)
(require 'exc-api)

;;;----------------------------------------------------------------------
;;; Example Plugin: Enhanced Text Rendering
;;;----------------------------------------------------------------------

(defun exc-example-text-renderer (value &optional _cell-props)
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

(defun exc-currency-renderer (value &optional _cell-props)
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

(defun exc-progress-renderer (value &optional _cell-props)
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

(defun exc-example-create-data-source (data-list &optional title)
  "Create a data source from a list of alists.
DATA-LIST should be a list of alists where each alist represents a row.
TITLE is an optional string for the grid title."
  (when data-list
    (let* ((headers (mapcar #'car (car data-list)))
           (data (mapcar #'cdr data-list))
           (source (make-exc-default-data-source headers data title)))
      ;; Add custom functionality
      (puthash :custom-filter
               (lambda (s filter-func)
                 (let ((model (gethash :model s))
                       (filtered-data '()))
                   (dotimes (r (exc-data-model-rows model))
                     (when (funcall filter-func r)
                       (push (exc-get-raw-value-at s r 0) filtered-data)))
                   (nreverse filtered-data)))
               source)
      source)))

;;;----------------------------------------------------------------------
;;; Example Plugin: Custom Commands
;;;----------------------------------------------------------------------

(defun exc-example-highlight-duplicates ()
  "Highlight duplicate values in the current grid."
  (interactive)
  (when exc--data-source
    (let ((duplicates (make-hash-table :test 'equal))
          (rows (exc-get-row-count exc--data-source))
          (cols (exc-get-column-count exc--data-source)))
      ;; Find duplicates
      (dotimes (r rows)
        (dotimes (c cols)
          (let ((value (exc-get-raw-value-at exc--data-source r c)))
            (when (and value (not (string-empty-p value)))
              (puthash value (1+ (gethash value duplicates 0)) duplicates)))))
      ;; Highlight duplicates
      (dotimes (r rows)
        (dotimes (c cols)
          (let ((value (exc-get-raw-value-at exc--data-source r c)))
            (when (and value (> (gethash value duplicates 0) 1))
              (exc-highlight-cell r c))))))))

(defun exc-example-export-to-html ()
  "Export current grid to HTML format."
  (interactive)
  (when exc--data-source
    (let* ((rows (exc-get-row-count exc--data-source))
           (cols (exc-get-column-count exc--data-source))
           (html '("<table border=\"1\">"))
           (filename (read-file-name "Save HTML to: ")))
      ;; Add headers
      (push "<tr>" html)
      (dotimes (c cols)
        (push (format "<th>%s</th>" (exc-get-header-value exc--data-source c)) html))
      (push "</tr>" html)
      ;; Add data rows
      (dotimes (r rows)
        (push "<tr>" html)
        (dotimes (c cols)
          (push (format "<td>%s</td>" (exc-get-raw-value-at exc--data-source r c)) html))
        (push "</tr>" html))
      (push "</table>" html)
      ;; Write to file
      (with-temp-file filename
        (insert (mapconcat #'identity (nreverse html) "\n")))
      (message "HTML exported to: %s" filename))))

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun exc-example-plugin-init ()
  "Initialize the example plugin."
  ;; Register cell renderers
  (exc-register-cell-renderer 'enhanced-text #'exc-example-text-renderer)
  (exc-register-cell-renderer 'currency #'exc-currency-renderer)
  (exc-register-cell-renderer 'progress #'exc-progress-renderer)
  
  ;; Register data source type
  (exc-register-data-source-type 'example #'exc-example-create-data-source)
  
  ;; Register plugin
  (exc-register-plugin 'example-plugin)
  
  (message "Example plugin initialized"))

;;;----------------------------------------------------------------------
;;; Usage Examples
;;;----------------------------------------------------------------------

(defun exc-example-demo ()
  "Demonstrate the example plugin functionality."
  (interactive)
  (let* ((data '((("Name" . "Alice") ("Email" . "alice@example.com") ("Phone" . "555-1234"))
                  (("Name" . "Bob") ("Email" . "bob@example.com") ("Phone" . "555-5678"))
                  (("Name" . "Charlie") ("Email" . "charlie@example.com") ("Phone" . "555-9012"))))
         (data-source (exc-example-create-data-source data "Example Demo")))
    (exc-api-create
     :data-source data-source
     :columns (exc-create-columns
               (exc-define-column "Name" :type 'enhanced-text :width 20)
               (exc-define-column "Email" :type 'enhanced-text :width 30)
               (exc-define-column "Phone" :type 'enhanced-text :width 15))
     :title "Example Plugin Demo")))

(defun exc-example-currency-demo ()
  "Demonstrate currency rendering."
  (interactive)
  (let* ((headers '("Item" "Price" "Progress"))
         (data '(("Laptop" "1200.50" "75")
                 ("Mouse" "25.99" "90")
                 ("Keyboard" "75.00" "45")))
         (data-source (make-exc-default-data-source headers data "Currency Demo")))
    (exc-api-create
     :data-source data-source
     :columns (exc-create-columns
               (exc-define-column "Item" :type 'text :width 15)
               (exc-define-column "Price" :type 'currency :width 12)
               (exc-define-column "Progress" :type 'progress :width 25))
     :title "Currency and Progress Demo")))

(provide 'exc-example-plugin)
