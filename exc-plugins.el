;;; exc-plugins.el --- Plugin system for exc -*- lexical-binding: t -*-

(require 'cl-lib)

;;;----------------------------------------------------------------------
;;; Plugin System
;;;----------------------------------------------------------------------

(defvar exc-plugins '()
  "List of loaded plugins.")

(defvar exc-cell-renderers (make-hash-table)
  "Hash table of cell renderers by type.")

(defvar exc-data-source-types (make-hash-table)
  "Hash table of data source types.")

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun exc-register-plugin (plugin)
  "Register a plugin for grid table functionality.
PLUGIN should be a symbol representing the plugin name."
  (add-to-list 'exc-plugins plugin)
  (message "Plugin registered: %s" plugin))

(defun exc-register-cell-renderer (type renderer-func)
  "Register a cell renderer for a specific type.
TYPE is a symbol representing the cell type.
RENDERER-FUNC is a function that takes (value cell-props) and returns formatted text."
  (puthash type renderer-func exc-cell-renderers)
  (message "Cell renderer registered for type: %s" type))

(defun exc-register-data-source-type (type creator-func)
  "Register a data source type.
TYPE is a symbol representing the data source type.
CREATOR-FUNC is a function that creates a data source instance."
  (puthash type creator-func exc-data-source-types)
  (message "Data source type registered: %s" type))

;;;----------------------------------------------------------------------
;;; Plugin Utilities
;;;----------------------------------------------------------------------

(defun exc-get-cell-renderer (type)
  "Get the cell renderer for TYPE."
  (gethash type exc-cell-renderers))

(defun exc-get-data-source-creator (type)
  "Get the data source creator for TYPE."
  (gethash type exc-data-source-types))

(defun exc-render-cell (value type &optional cell-props)
  "Render a cell value using the appropriate renderer.
VALUE is the cell value.
TYPE is the cell type.
CELL-PROPS are optional cell properties."
  (let ((renderer (exc-get-cell-renderer type)))
    (if renderer
        (funcall renderer value cell-props)
      (format "%s" value))))

;;;----------------------------------------------------------------------
;;; Built-in Cell Renderers
;;;----------------------------------------------------------------------

(defun exc-text-renderer (value &optional cell-props)
  "Default text renderer."
  (format "%s" value))

(defun exc-number-renderer (value &optional cell-props)
  "Number renderer with formatting."
  (if (numberp value)
      (format "%.2f" value)
    (format "%s" value)))

(defun exc-image-renderer (value &optional cell-props)
  "Image renderer for image paths."
  (if (and value (file-exists-p value))
      (format "[Image: %s]" (file-name-nondirectory value))
    (format "%s" value)))

;;;----------------------------------------------------------------------
;;; Plugin Loading
;;;----------------------------------------------------------------------

(defun exc-load-plugin (plugin-name)
  "Load a plugin by name."
  (let ((plugin-file (format "exc-%s" plugin-name)))
    (when (require (intern plugin-file) nil t)
      (exc-register-plugin (intern plugin-name))
      (message "Plugin loaded: %s" plugin-name))))

(defun exc-load-all-plugins ()
  "Load all available plugins."
  (mapc #'exc-load-plugin '("csv" "org")))

;;;----------------------------------------------------------------------
;;; Initialize Built-in Renderers
;;;----------------------------------------------------------------------

(defun exc-init-builtin-renderers ()
  "Initialize built-in cell renderers."
  (exc-register-cell-renderer 'text #'exc-text-renderer)
  (exc-register-cell-renderer 'number #'exc-number-renderer)
  (exc-register-cell-renderer 'image #'exc-image-renderer))

;;;----------------------------------------------------------------------
;;; Plugin System Initialization
;;;----------------------------------------------------------------------

(defun exc-plugins-init ()
  "Initialize the plugin system."
  (exc-init-builtin-renderers)
  (exc-load-all-plugins))

(provide 'exc-plugins)
