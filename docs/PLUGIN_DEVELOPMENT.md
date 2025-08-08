# grid-table Plugin Development Detailed Guide

## 🏗️ Plugin System Architecture

### Core Components

```elisp
;; Plugin registry
(defvar grid-table-plugins '())
(defvar grid-table-cell-renderers (make-hash-table))
(defvar grid-table-data-source-types (make-hash-table))
```

### Registration Functions

```elisp
;; Register plugin
(grid-table-register-plugin 'my-plugin)

;; Register cell renderer
(grid-table-register-cell-renderer 'my-type #'my-renderer)

;; Register data source type
(grid-table-register-data-source-type 'my-format #'my-creator)
```

## 🧩 Plugin Types Explained

### 1. Cell Renderer Plugin

Cell renderer is responsible for converting raw data values to display text.

#### Function Signature
```elisp
(defun my-renderer (value &optional cell-props)
  "Render VALUE with optional CELL-PROPS.
VALUE is the raw cell value.
CELL-PROPS is a plist with cell properties.
Returns formatted text string.")
```

#### Example: Syntax Highlight Renderer
```elisp
(defun grid-table-syntax-highlight-renderer (value &optional cell-props)
  "Render text with syntax highlighting."
  (if (and value (stringp value))
      (let ((rendered value))
        ;; Highlight URL
        (setq rendered (replace-regexp-in-string 
                        "https?://[^\\s]+" 
                        (lambda (url)
                          (propertize url 'face '(:foreground "blue" :underline t)))
                        rendered))
        ;; Highlight email
        (setq rendered (replace-regexp-in-string 
                        "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
                        (lambda (email)
                          (propertize email 'face '(:foreground "green")))
                        rendered))
        rendered)
    ""))
```

### 2. Data Source Type Plugin

Data source type plugin defines how to create a data source from a specific format.

#### Registration Function Signature
```elisp
(defun my-data-source-creator (source &rest args)
  "Create a data source from SOURCE with ARGS.
SOURCE is the data source (file path, URL, etc.).
ARGS are additional arguments.
Returns a data source hash table.")
```

#### Example: JSON Data Source
```elisp
(defun grid-table-json-data-source (json-file)
  "Create a data source from JSON file."
  (when (file-exists-p json-file)
    (let* ((json-data (json-read-file json-file))
           (headers (mapcar #'car (cdr (car json-data))))
           (data (mapcar #'cdr json-data)))
      (make-default-data-source headers data (file-name-nondirectory json-file)))))
```

### 3. Feature Plugin

Feature plugin provides a complete custom functionality module.

#### Example: Export Plugin
```elisp
(defun grid-table-export-to-csv (file-path)
  "Export current grid to CSV file."
  (interactive "FSave to CSV: ")
  (when grid-table--data-source
    (let* ((rows (grid-get-row-count grid-table--data-source))
           (cols (grid-get-column-count grid-table--data-source)))
      (with-temp-file file-path
        ;; Write header row
        (insert (mapconcat #'identity 
                          (mapcar (lambda (c) (grid-get-header-value grid-table--data-source c))
                                  (number-sequence 0 (1- cols)))
                        ","))
        (insert "\n")
        ;; Write data row
        (dotimes (r rows)
          (insert (mapconcat #'identity 
                            (mapcar (lambda (c) (grid-get-raw-value-at grid-table--data-source r c))
                                    (number-sequence 0 (1- cols)))
                          ","))
          (insert "\n")))
      (message "Exported to: %s" file-path))))
```

## 🔧 Development Environment Setup

### 1. Create Plugin Directory Structure

```
my-grid-table-plugin/
├── my-grid-table-plugin.el    ; Main plugin file
├── README.md                  ; Plugin description
├── tests/
│   └── test-my-plugin.el     ; Test file
└── examples/
    └── demo.el               ; Usage example
```

### 2. Plugin File Template

```elisp
;;; my-grid-table-plugin.el --- My custom plugin -*- lexical-binding: t -*-
;;; Version: 1.0.0
;;; Author: Your Name
;;; Keywords: grid-table, plugin
;;; URL: https://github.com/yourname/my-grid-table-plugin

(require 'grid-table-plugins)
(require 'grid-table-core)

;;;----------------------------------------------------------------------
;;; Plugin Implementation
;;;----------------------------------------------------------------------

;; Your plugin code here

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun my-grid-table-plugin-init ()
  "Initialize my grid-table plugin."
  ;; Register components
  (grid-table-register-cell-renderer 'my-type #'my-renderer)
  (grid-table-register-data-source-type 'my-format #'my-creator)
  (grid-table-register-plugin 'my-plugin)
  (message "My plugin initialized"))

;; Auto-initialize
(my-grid-table-plugin-init)

(provide 'my-grid-table-plugin)
```

## 📝 Plugin Development Workflow

### Step 1: Requirement Analysis

1. **Determine plugin type**: Renderer, data source, feature plugin
2. **Define feature requirements**: Input, output, processing logic
3. **Design interface**: Function signature, parameters, return values

### Step 2: Implement Core Functionality

```elisp
;; Implement renderer
(defun my-custom-renderer (value &optional cell-props)
  "My custom cell renderer."
  (if (and value (stringp value))
      ;; Implement rendering logic
      (let ((result value))
        ;; Process logic
        result)
    ""))

;; Implement data source creator
(defun my-data-source-creator (source)
  "Create data source from SOURCE."
  (when (file-exists-p source)
    ;; Parse data
    (let* ((data (my-parse-data source))
           (headers (my-extract-headers data))
           (rows (my-extract-rows data)))
      (make-default-data-source headers rows))))
```

### Step 3: Register Plugin

```elisp
(defun my-plugin-init ()
  "Initialize my plugin."
  ;; Register renderer
  (grid-table-register-cell-renderer 'my-type #'my-custom-renderer)
  
  ;; Register data source type
  (grid-table-register-data-source-type 'my-format #'my-data-source-creator)
  
  ;; Register plugin
  (grid-table-register-plugin 'my-plugin)
  
  (message "My plugin initialized"))
```

### Step 4: Test and Debug

```elisp
(defun my-plugin-test ()
  "Test my plugin functionality."
  (interactive)
  ;; Test renderer
  (let ((result (my-custom-renderer "test value")))
    (message "Renderer test: %s" result))
  
  ;; Test data source
  (let ((source (my-data-source-creator "test.data")))
    (message "Data source test: %s" (if source "success" "failed"))))
```

## 🎯 实际示例

### Example 1: Currency Formatting Plugin

```elisp
;;; grid-table-currency-plugin.el --- Currency formatting plugin -*- lexical-binding: t -*-

(require 'grid-table-plugins)

(defun grid-table-currency-renderer (value &optional cell-props)
  "Render currency values with proper formatting."
  (if (and value (stringp value))
      (let ((amount (string-to-number value)))
        (if (numberp amount)
            (format "$%.2f" amount)
          value))
    ""))

(defun grid-table-currency-plugin-init ()
  "Initialize currency plugin."
  (grid-table-register-cell-renderer 'currency #'grid-table-currency-renderer)
  (grid-table-register-plugin 'currency-plugin)
  (message "Currency plugin initialized"))

(grid-table-currency-plugin-init)

(provide 'grid-table-currency-plugin)
```

### Example 2: Progress Bar Plugin

```elisp
;;; grid-table-progress-plugin.el --- Progress bar plugin -*- lexical-binding: t -*-

(require 'grid-table-plugins)

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

(defun grid-table-progress-plugin-init ()
  "Initialize progress plugin."
  (grid-table-register-cell-renderer 'progress #'grid-table-progress-renderer)
  (grid-table-register-plugin 'progress-plugin)
  (message "Progress plugin initialized"))

(grid-table-progress-plugin-init)

(provide 'grid-table-progress-plugin)
```

### Example 3: JSON Data Source Plugin

```elisp
;;; grid-table-json-plugin.el --- JSON data source plugin -*- lexical-binding: t -*-

(require 'grid-table-plugins)
(require 'json)

(defun grid-table-json-create-data-source (json-file)
  "Create a data source from JSON file."
  (when (file-exists-p json-file)
    (let* ((json-data (json-read-file json-file))
           (headers (mapcar #'car (cdr (car json-data))))
           (data (mapcar #'cdr json-data)))
      (make-default-data-source headers data (file-name-nondirectory json-file)))))

(defun grid-table-json-plugin-init ()
  "Initialize JSON plugin."
  (grid-table-register-data-source-type 'json #'grid-table-json-create-data-source)
  (grid-table-register-plugin 'json-plugin)
  (message "JSON plugin initialized"))

(grid-table-json-plugin-init)

(provide 'grid-table-json-plugin)
```

## 🚀 Advanced Features

### 1. Custom Command

```elisp
(defun my-plugin-custom-command ()
  "My custom command for the grid table."
  (interactive)
  (when grid-table--data-source
    ;; Implement custom functionality
    (let ((rows (grid-get-row-count grid-table--data-source))
          (cols (grid-get-column-count grid-table--data-source)))
      (message "Grid has %d rows and %d columns" rows cols))))

;; Add to key bindings
(define-key grid-table-mode-map (kbd "C-c m") #'my-plugin-custom-command)
```

### 2. Custom Data Source Functionality

```elisp
(defun my-data-source-with-custom-functions (data)
  "Create a data source with custom functions."
  (let ((source (make-default-data-source headers data title)))
    ;; Add custom functionality
    (puthash :my-custom-function
             (lambda (s arg)
               ;; Implement custom functionality
               (message "Custom function called with %s" arg))
             source)
    source))
```

### 3. Plugin Configuration

```elisp
(defcustom my-plugin-config-value "default"
  "Configuration value for my plugin."
  :type 'string
  :group 'grid-table)

(defun my-plugin-with-config ()
  "Use plugin with configuration."
  (let ((config my-plugin-config-value))
    ;; Use configuration value
    (message "Using config: %s" config)))
```

## 🔍 Debugging and Testing

### 1. Plugin Debugging Tool

```elisp
(defun grid-table-debug-plugins ()
  "Debug loaded plugins."
  (interactive)
  (message "Loaded plugins: %s" grid-table-plugins)
  (message "Cell renderers: %s" (hash-table-keys grid-table-cell-renderers))
  (message "Data source types: %s" (hash-table-keys grid-table-data-source-types)))
```

### 2. Plugin Test Framework

```elisp
(defun grid-table-test-plugin (plugin-name)
  "Test a specific plugin."
  (interactive "sPlugin name: ")
  (let ((plugin-symbol (intern plugin-name)))
    (if (member plugin-symbol grid-table-plugins)
        (message "Plugin %s is loaded" plugin-name)
      (message "Plugin %s is not loaded" plugin-name))))
```

### 3. Renderer Test

```elisp
(defun grid-table-test-renderer (type value)
  "Test a cell renderer."
  (interactive "sRenderer type: \nsTest value: ")
  (let ((renderer (grid-table-get-cell-renderer (intern type))))
    (if renderer
        (let ((result (funcall renderer value)))
          (message "Renderer result: %s" result))
      (message "Renderer not found: %s" type))))
```

## 📚 Best Practices

### 1. Naming Convention

```elisp
;; Use grid-table- prefix
(defun grid-table-my-plugin-function () ...)
(defun grid-table-my-renderer () ...)
(defun grid-table-my-data-source () ...)
```

### 2. Error Handling

```elisp
(defun my-safe-renderer (value &optional cell-props)
  "Safe renderer with error handling."
  (condition-case err
      (my-renderer value cell-props)
    (error
     (message "Renderer error: %s" (error-message-string err))
     (format "%s" value))))
```

### 3. Documentation

```elisp
(defun my-renderer (value &optional cell-props)
  "Render VALUE with optional CELL-PROPS.
VALUE is the raw cell value to render.
CELL-PROPS is a plist with cell properties like :width, :align.
Returns a formatted string for display.

Example:
  (my-renderer \"123.45\" '(:width 10))
  => \"$123.45    \"")
```

### 4. Backward Compatibility

```elisp
;; Check version compatibility
(defun my-plugin-check-compatibility ()
  "Check if plugin is compatible with current grid-table version."
  (when (boundp 'grid-table-version)
    (if (version<= "1.0" grid-table-version)
        t
      (message "Warning: Plugin may not be compatible with grid-table %s" grid-table-version))))
```

### 5. Performance Optimization

```elisp
;; Cache rendering results
(defvar my-renderer-cache (make-hash-table :test 'equal))

(defun my-cached-renderer (value &optional cell-props)
  "Cached renderer for better performance."
  (let ((cache-key (format "%s-%s" value cell-props)))
    (or (gethash cache-key my-renderer-cache)
        (let ((result (my-renderer value cell-props)))
          (puthash cache-key result my-renderer-cache)
          result))))
```

## 🎉 Summary

Through the grid-table plugin system, you can:

1. **Extend rendering functionality** - Create custom cell renderers
2. **Support new data formats** - Through data source type plugins
3. **Add custom functionality** - Through feature plugins
4. **Improve development efficiency** - Use provided tools and templates


