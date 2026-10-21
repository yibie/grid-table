;;; grid-table.el --- The name this package had before exc  -*- lexical-binding: t -*-

;;; Commentary:

;; The package was renamed from grid-table to exc.  This file is here so that
;; `(require 'grid-table)' in a configuration written before the rename still
;; loads the package; it loads `exc' and says what to write instead.

;;; Code:

(require 'exc)

(display-warning
 'exc
 (concat "The grid-table package is now exc.  Change `(require 'grid-table)'"
         " to `(require 'exc)'; see \"Installation\" in the README.")
 :warning)

(provide 'grid-table)

;;; grid-table.el ends here
