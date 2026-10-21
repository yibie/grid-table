;;; exc-api.el --- API system for exc -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'exc-plugins)
(require 'exc-obsolete)

;;;----------------------------------------------------------------------
;;; API Configuration
;;;----------------------------------------------------------------------

(defvar exc-default-bindings
  '((:navigation
     ("C-n" . exc-next-line)
     ("C-p" . exc-previous-line)
     ("C-f" . exc-next-cell)
     ("C-b" . exc-previous-cell))
    (:editing
     ("RET" . exc-edit-cell))
    (:file
     ("C-x C-s" . exc-save)
     ("C-x C-w" . exc-save-as)))
  "Default key bindings for exc.")

;;;----------------------------------------------------------------------
;;; Data Source API
;;;----------------------------------------------------------------------

(defun exc-create-data-source (type &rest args)
  "Create a data source of TYPE with ARGS.
TYPE should be a registered data source type."
  (let ((creator (exc-get-data-source-creator type)))
    (if creator
        (apply creator args)
      (error "Unknown data source type: %s" type))))

(defun exc-make-hash-table-data-source (data-hash)
  "Create a data source from a hash table."
  (make-exc-default-data-source 
   (gethash :headers data-hash)
   (gethash :data data-hash)
   (gethash :title data-hash)))

;;;----------------------------------------------------------------------
;;; Column Definition API
;;;----------------------------------------------------------------------

(defun exc-define-column (name &rest properties)
  "Define a column with NAME and PROPERTIES.
Properties can include :type, :width, :renderer, etc."
  (let ((column (list :name name)))
    (while properties
      (let ((key (pop properties))
            (value (pop properties)))
        (setq column (plist-put column key value))))
    column))

(defun exc-create-columns (&rest column-definitions)
  "Create a list of columns from COLUMN-DEFINITIONS."
  (mapcar (lambda (def)
            (if (stringp def)
                (exc-define-column def)
              def))
          column-definitions))

;;;----------------------------------------------------------------------
;;; Main API Functions
;;;----------------------------------------------------------------------

(defun exc-api-create (&rest args)
  "Create a grid with ARGS.
Supported arguments:
  :data-source - The data source instance
  :columns - List of column definitions
  :key-bindings - Key bindings configuration
  :plugins - List of plugins to load
  :title - Grid title
  :file-path - Associated file path"
  (let* ((data-source (plist-get args :data-source))
         (columns (plist-get args :columns))
         (key-bindings (or (plist-get args :key-bindings)
                           exc-default-bindings))
         (plugins (plist-get args :plugins))
         (title (plist-get args :title))
         (file-path (plist-get args :file-path)))
    
    ;; Load plugins if specified
    (when plugins
      (mapc #'exc-load-plugin plugins))
    
    ;; Create the grid
    (exc--create-grid data-source columns key-bindings title file-path)))

(defun exc--create-grid (data-source columns key-bindings title file-path)
  "Internal function to create a grid buffer."
  (let ((buffer (generate-new-buffer "*Exc*")))
    (with-current-buffer buffer
      (setq exc--title title)
      (setq exc--file-path file-path)

      ;; Render the grid; this sets the major mode
      (exc--show-grid-in-current-buffer data-source title)

      ;; Apply key bindings
      (exc--apply-key-bindings key-bindings)

      ;; Switch to the buffer
      (switch-to-buffer buffer))))

;;;----------------------------------------------------------------------
;;; Key Binding API
;;;----------------------------------------------------------------------

(defun exc--apply-key-bindings (bindings)
  "Apply key bindings to the current buffer.
They go into a keymap of the buffer's own, on top of the mode's.  Keys
the view binds itself, such as the arrows, TAB and plain characters, are
not overridden this way."
  ;; The mode's map is shared by every grid: bind in one of this buffer.
  (use-local-map (make-composed-keymap (make-sparse-keymap)
                                       (current-local-map)))
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

(defun exc-get-current-data-source ()
  "Get the current data source."
  exc--data-source)

(defun exc-get-current-title ()
  "Get the current grid title."
  exc--title)

(defun exc-get-current-file-path ()
  "Get the current file path."
  exc--file-path)

(defun exc-save ()
  "Save the current grid.
Without a file to save to, ask for one, as `exc-save-as' does."
  (interactive)
  (if (not exc--file-path)
      (call-interactively #'exc-save-as)
    (let ((save-func (gethash :save-to-file exc--data-source)))
      (if save-func
          (progn
            (funcall save-func exc--data-source exc--file-path)
            (message "Saved to %s" exc--file-path))
        (message "No save function available for this data source")))))

(defun exc-save-as (file-path)
  "Save the current grid to FILE-PATH."
  (interactive "FSave to file: ")
  (let ((save-func (gethash :save-to-file exc--data-source)))
    (if save-func
        (progn
          (funcall save-func exc--data-source file-path)
          (setq exc--file-path file-path)
          (message "Saved to %s" file-path))
      (message "No save function available for this data source"))))

;;;----------------------------------------------------------------------
;;; Example Usage Functions
;;;----------------------------------------------------------------------

(defun exc-example-basic ()
  "Example of basic grid usage."
  (interactive)
  (let* ((headers '("Name" "Age" "City"))
         (data '(("Alice" "25" "New York")
                 ("Bob" "30" "London")
                 ("Charlie" "35" "Paris")))
         (data-source (make-exc-default-data-source headers data "Basic Example")))
    (exc-api-create
     :data-source data-source
     :columns (exc-create-columns
               (exc-define-column "Name" :type 'text :width 20)
               (exc-define-column "Age" :type 'number :width 10)
               (exc-define-column "City" :type 'text :width 15))
     :title "Basic Example")))

(defun exc-example-csv (file-path)
  "Example of CSV grid usage."
  (interactive "fCSV file: ")
  (when-let ((data-source (exc-csv-create-data-source file-path)))
    (exc-api-create
     :data-source data-source
     :plugins '("csv")
     :title (file-name-nondirectory file-path)
     :file-path file-path)))

(provide 'exc-api)
