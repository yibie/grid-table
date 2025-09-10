;;; grid-table-api.el --- API system for grid-table -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'grid-table-plugins)

;;;----------------------------------------------------------------------
;;; API Configuration
;;;----------------------------------------------------------------------

(defvar grid-table-default-bindings
  '((:navigation
     ("C-n" . grid-table-next-line)
     ("C-p" . grid-table-previous-line)
     ("C-f" . grid-table-next-cell)
     ("C-b" . grid-table-previous-cell))
    (:editing
     ("RET" . grid-table-edit-cell)
     ("C-c C-c" . grid-table-finish-edit)
     ("C-g" . grid-table-cancel-edit))
    (:file
     ("C-x C-s" . grid-table-save)
     ("C-x C-w" . grid-table-save-as)))
  "Default key bindings for grid-table.")

;;;----------------------------------------------------------------------
;;; Data Source API
;;;----------------------------------------------------------------------

(defun grid-table-create-data-source (type &rest args)
  "Create a data source of TYPE with ARGS.
TYPE should be a registered data source type."
  (let ((creator (grid-table-get-data-source-creator type)))
    (if creator
        (apply creator args)
      (error "Unknown data source type: %s" type))))

(defun grid-table-make-hash-table-data-source (data-hash)
  "Create a data source from a hash table."
  (make-default-data-source 
   (gethash :headers data-hash)
   (gethash :data data-hash)
   (gethash :title data-hash)))

;;;----------------------------------------------------------------------
;;; Column Definition API
;;;----------------------------------------------------------------------

(defun grid-table-define-column (name &rest properties)
  "Define a column with NAME and PROPERTIES.
Properties can include :type, :width, :renderer, etc."
  (let ((column (list :name name)))
    (while properties
      (let ((key (pop properties))
            (value (pop properties)))
        (setq column (plist-put column key value))))
    column))

(defun grid-table-create-columns (&rest column-definitions)
  "Create a list of columns from COLUMN-DEFINITIONS."
  (mapcar (lambda (def)
            (if (stringp def)
                (grid-table-define-column def)
              def))
          column-definitions))

;;;----------------------------------------------------------------------
;;; Main API Functions
;;;----------------------------------------------------------------------

(defun grid-table-api-create (&rest args)
  "Create a grid table with ARGS.
Supported arguments:
  :data-source - The data source instance
  :columns - List of column definitions
  :key-bindings - Key bindings configuration
  :plugins - List of plugins to load
  :title - Grid title
  :file-path - Associated file path"
  (let* ((data-source (plist-get args :data-source))
         (columns (plist-get args :columns))
         (key-bindings (plist-get args :key-bindings grid-table-default-bindings))
         (plugins (plist-get args :plugins))
         (title (plist-get args :title))
         (file-path (plist-get args :file-path)))
    
    ;; Load plugins if specified
    (when plugins
      (mapc #'grid-table-load-plugin plugins))
    
    ;; Create the grid table
    (grid-table--create-grid data-source columns key-bindings title file-path)))

(defun grid-table--create-grid (data-source columns key-bindings title file-path)
  "Internal function to create a grid table."
  (let ((buffer (generate-new-buffer "*Grid Table*")))
    (with-current-buffer buffer
      (grid-table-mode)
      (setq grid-table--data-source data-source)
      (setq grid-table--title title)
      (setq grid-table--file-path file-path)
      
      ;; Apply key bindings
      (grid-table--apply-key-bindings key-bindings)
      
      ;; Render the grid
      (grid-table--show-grid data-source)
      
      ;; Switch to the buffer
      (switch-to-buffer buffer))))

;;;----------------------------------------------------------------------
;;; Key Binding API
;;;----------------------------------------------------------------------

(defun grid-table--apply-key-bindings (bindings)
  "Apply key bindings to the current buffer."
  (dolist (category bindings)
    (let ((category-name (car category))
          (key-bindings (cdr category)))
      (dolist (binding key-bindings)
        (let ((key (car binding))
              (command (cdr binding)))
          (local-set-key (kbd key) command))))))

;;;----------------------------------------------------------------------
;;; Utility API Functions
;;;----------------------------------------------------------------------

(defun grid-table-get-current-data-source ()
  "Get the current data source."
  grid-table--data-source)

(defun grid-table-get-current-title ()
  "Get the current grid title."
  grid-table--title)

(defun grid-table-get-current-file-path ()
  "Get the current file path."
  grid-table--file-path)

(defun grid-table-save ()
  "Save the current grid table."
  (interactive)
  (when grid-table--file-path
    (let ((save-func (gethash :save-to-file grid-table--data-source)))
      (if save-func
          (progn
            (funcall save-func grid-table--data-source grid-table--file-path)
            (message "Grid table saved to: %s" grid-table--file-path))
        (message "No save function available for this data source")))))

(defun grid-table-save-as (file-path)
  "Save the current grid table to FILE-PATH."
  (interactive "FSave to file: ")
  (let ((save-func (gethash :save-to-file grid-table--data-source)))
    (if save-func
        (progn
          (funcall save-func grid-table--data-source file-path)
          (setq grid-table--file-path file-path)
          (message "Grid table saved to: %s" file-path))
      (message "No save function available for this data source"))))

;;;----------------------------------------------------------------------
;;; Example Usage Functions
;;;----------------------------------------------------------------------

(defun grid-table-example-basic ()
  "Example of basic grid table usage."
  (interactive)
  (let* ((headers '("Name" "Age" "City"))
         (data '(("Alice" "25" "New York")
                 ("Bob" "30" "London")
                 ("Charlie" "35" "Paris")))
         (data-source (make-default-data-source headers data "Basic Example")))
    (grid-table-api-create
     :data-source data-source
     :columns (grid-table-create-columns
               (grid-table-define-column "Name" :type 'text :width 20)
               (grid-table-define-column "Age" :type 'number :width 10)
               (grid-table-define-column "City" :type 'text :width 15))
     :title "Basic Example")))

(defun grid-table-example-csv (file-path)
  "Example of CSV grid table usage."
  (interactive "fCSV file: ")
  (when-let ((data-source (grid-table-csv-create-data-source file-path)))
    (grid-table-api-create
     :data-source data-source
     :plugins '("csv")
     :title (file-name-nondirectory file-path)
     :file-path file-path)))

(provide 'grid-table-api)
