;;; grid-table-plugins.el --- Plugin system for grid-table -*- lexical-binding: t -*-

(require 'cl-lib)

;;;----------------------------------------------------------------------
;;; Plugin System
;;;----------------------------------------------------------------------

(defvar grid-table-plugins '()
  "List of loaded plugins.")

(defvar grid-table-cell-renderers (make-hash-table)
  "Hash table of cell renderers by type.")

(defvar grid-table-data-source-types (make-hash-table)
  "Hash table of data source types.")

;;;----------------------------------------------------------------------
;;; Plugin Registration
;;;----------------------------------------------------------------------

(defun grid-table-register-plugin (plugin)
  "Register a plugin for grid table functionality.
PLUGIN should be a symbol representing the plugin name."
  (add-to-list 'grid-table-plugins plugin)
  (message "Plugin registered: %s" plugin))

(defun grid-table-register-cell-renderer (type renderer-func)
  "Register a cell renderer for a specific type.
TYPE is a symbol representing the cell type.
RENDERER-FUNC is a function that takes (value cell-props) and returns formatted text."
  (puthash type renderer-func grid-table-cell-renderers)
  (message "Cell renderer registered for type: %s" type))

(defun grid-table-register-data-source-type (type creator-func)
  "Register a data source type.
TYPE is a symbol representing the data source type.
CREATOR-FUNC is a function that creates a data source instance."
  (puthash type creator-func grid-table-data-source-types)
  (message "Data source type registered: %s" type))

;;;----------------------------------------------------------------------
;;; Plugin Utilities
;;;----------------------------------------------------------------------

(defun grid-table-get-cell-renderer (type)
  "Get the cell renderer for TYPE."
  (gethash type grid-table-cell-renderers))

(defun grid-table-get-data-source-creator (type)
  "Get the data source creator for TYPE."
  (gethash type grid-table-data-source-types))

(defun grid-table-render-cell (value type &optional cell-props)
  "Render a cell value using the appropriate renderer.
VALUE is the cell value.
TYPE is the cell type.
CELL-PROPS are optional cell properties."
  (let ((renderer (grid-table-get-cell-renderer type)))
    (if renderer
        (funcall renderer value cell-props)
      (format "%s" value))))

;;;----------------------------------------------------------------------
;;; Built-in Cell Renderers
;;;----------------------------------------------------------------------

(defun grid-table-text-renderer (value &optional cell-props)
  "Default text renderer."
  (format "%s" value))

(defun grid-table-number-renderer (value &optional cell-props)
  "Number renderer with formatting."
  (if (numberp value)
      (format "%.2f" value)
    (format "%s" value)))

(defun grid-table-image-renderer (value &optional cell-props)
  "Image renderer for image paths."
  (if (and value (file-exists-p value))
      (format "[Image: %s]" (file-name-nondirectory value))
    (format "%s" value)))

;;;----------------------------------------------------------------------
;;; Plugin Loading
;;;----------------------------------------------------------------------

(defun grid-table-load-plugin (plugin-name)
  "Load a plugin by name."
  (let ((plugin-file (format "grid-table-%s" plugin-name)))
    (when (require (intern plugin-file) nil t)
      (grid-table-register-plugin (intern plugin-name))
      (message "Plugin loaded: %s" plugin-name))))

(defun grid-table-load-all-plugins ()
  "Load all available plugins."
  (mapc #'grid-table-load-plugin '("csv" "org")))

;;;----------------------------------------------------------------------
;;; Initialize Built-in Renderers
;;;----------------------------------------------------------------------

(defun grid-table-init-builtin-renderers ()
  "Initialize built-in cell renderers."
  (grid-table-register-cell-renderer 'text #'grid-table-text-renderer)
  (grid-table-register-cell-renderer 'number #'grid-table-number-renderer)
  (grid-table-register-cell-renderer 'image #'grid-table-image-renderer))

;;;----------------------------------------------------------------------
;;; Plugin System Initialization
;;;----------------------------------------------------------------------

(defun grid-table-plugins-init ()
  "Initialize the plugin system."
  (grid-table-init-builtin-renderers)
  (grid-table-load-all-plugins))

(provide 'grid-table-plugins)
