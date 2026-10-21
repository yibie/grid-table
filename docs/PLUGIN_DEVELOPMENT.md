[English](PLUGIN_DEVELOPMENT.md) | [中文](PLUGIN_DEVELOPMENT_CN.md)

# Writing a plugin

A plugin is an Emacs Lisp file that requires `exc` and adds
something to it: a file format to open, an export, a command. There is
no plugin loader to satisfy; the file is loaded with `require` like any
other library. The plugins that come with exc.el live in `plugins/`,
which `exc.el` adds to `load-path`.

## Where things are

| File | What a plugin uses from it |
|---|---|
| `exc-data-source.el` | `make-default-data-source`, the data source a grid shows |
| `exc.el` | `exc-get-row-count`, `exc-get-raw-value-at` and the other accessors |
| `exc-plugins.el` | `exc-register-data-source-type`, `exc-register-plugin` |
| `exc-api.el` | `exc-create-data-source`, `exc-api-create` |
| `exc-persistence.el` | `exc-persistence-create-data-source-from-file`, to read a `.grid` file |
| `exc-text.el` | `exc-text-render`, a grid as a plain-text table |

## Data sources

A grid shows a data source: a hash table from keywords to functions,
plus the data they work on. Each function takes the data source itself
as its first argument.

Build one with `make-default-data-source`:

```elisp
(make-default-data-source HEADERS ROWS &optional TITLE MERGES)
```

- `ROWS` is a list of rows, each a list of strings, as they would be
  typed: `"12"`, `"=SUM(B2:B3)"`, `"pen"`. A short row is filled with
  empty cells.
- `HEADERS` is a list of strings, one for each column; its length is the
  number of columns.
- The first row of `ROWS` is the header row the grid shows, which stays
  in place when sorting and filtering. `HEADERS` is not drawn. So pass
  the header line both ways: as `HEADERS` and as the first element of
  `ROWS`. This is what exc.el does when it reads a `.grid` file.

Do not build the hash table by hand. The view reads the sheet's model
(`:model`) directly in many places, and only `make-default-data-source`
sets it up, along with workbook bookkeeping.

### The keys

Code outside the model goes through accessors in `exc.el`, which
call these keys:

| Key | Accessor |
|---|---|
| `:get-row-count`, `:get-column-count` | `exc-get-row-count`, `exc-get-column-count` |
| `:get-raw-value-at` | `exc-get-raw-value-at`: what was typed, a formula as text |
| `:get-computed-value-at` | `exc-get-computed-value-at`: a formula's value |
| `:set-raw-value-at` | `exc-set-raw-value-at` |
| `:get-style-at`, `:set-style-at` | `exc-get-style-at`, `exc-set-style-at` |
| `:get-merges`, `:merge-cells` | `exc-get-merges`, `exc-merge-cells` |
| `:get-column-widths`, `:set-column-width` | `exc-get-column-widths`, `exc-set-column-width` |
| `:get-conditional-formats`, `:set-conditional-formats` | `exc-get-conditional-formats`, `exc-set-conditional-formats` |

`make-default-data-source` also provides keys the view calls for rows,
columns, sorting, freezing, hiding, filters, validations and charts
(`:add-row`, `:delete-column`, `:sort-by-column`, `:set-frozen`,
`:set-filters`, `:set-charts` and others); see the function itself.
Rows and columns count from 0. A value typed as a number comes back from
`exc-get-computed-value-at` as text (`"12"`).

A plugin can store its own keys in the same table. The CSV plugin adds
two that exc.el uses:

| Key | Used by |
|---|---|
| `:file-path` | The CSV plugin's `exc-csv-save-overwrite` |
| `:save-to-file` | `exc-save` and `exc-save-as`, and `C-x C-s` in a buffer that visits a file other than a `.grid` one. Each calls it as `(funcall FN SOURCE FILE)`. |

## Registering

`exc-plugins.el` has two registers that work:

```elisp
(exc-register-data-source-type 'tsv #'exc-tsv-create-data-source)
(exc-create-data-source 'tsv "~/data.tsv")  ; calls the function with "~/data.tsv"
```

`exc-register-data-source-type` stores a function under a type,
and `exc-create-data-source` calls it with the remaining
arguments. An unknown type signals `Unknown data source type`.

`exc-register-plugin` adds a symbol to `exc-plugins`.
Nothing reads that list; it only records what was loaded.

`exc-load-plugin` requires `exc-NAME` and registers `NAME`:
`(exc-load-plugin "csv")` loads the CSV plugin.

Not used yet: `exc-register-cell-renderer` stores a function in
`exc-cell-renderers`, but the view never calls it, so a renderer
changes nothing on screen. Format cells with number formats and
conditional formats instead. Likewise `exc-plugins-init` and the
built-in renderers it registers are never called, and the `:columns`
argument of `exc-api-create` is ignored.

## Showing a data source

`exc-api-create` shows a data source in a new buffer called
`*Exc*`:

```elisp
(exc-api-create :data-source SOURCE :title TITLE :file-path FILE)
```

In that buffer `C-x C-s` runs `exc-save`, which calls the source's
`:save-to-file` with `FILE`. Without `:file-path`, it asks for a file,
as `exc-save-as` does. The keys it binds are in
`exc-default-bindings`.

The buffer visits no file. To make a grid that visits its file, as
`C-x C-f` does for `.grid` files, see how `exc-find-file-csv`
opens the file with `find-file-noselect` and shows the source in that
buffer: `C-x C-s` then calls `:save-to-file` with the visited file.

## A worked example: tab-separated files

This plugin opens a tab-separated file as a grid and saves it back. It
was loaded and run as shown below.

```elisp
;;; exc-tsv.el --- Tab-separated files for exc.el -*- lexical-binding: t -*-

(require 'exc)
(require 'exc-api)

(defun exc-tsv-create-data-source (file)
  "Return a data source holding the tab-separated FILE."
  (let* ((lines (with-temp-buffer
                  (insert-file-contents file)
                  (split-string (buffer-string) "\n" t)))
         (rows (mapcar (lambda (line) (split-string line "\t")) lines))
         ;; The first line is the header row: it is passed as HEADERS
         ;; and kept as the first row of the grid.
         (source (make-default-data-source (car rows) rows
                                           (file-name-nondirectory file))))
    (puthash :file-path file source)
    (puthash :save-to-file #'exc-tsv--save source)
    source))

(defun exc-tsv--save (source file)
  "Write SOURCE to FILE as tab-separated lines."
  (with-temp-file file
    (dotimes (row (exc-get-row-count source))
      (insert (mapconcat (lambda (col) (exc-get-raw-value-at source row col))
                         (number-sequence 0 (1- (exc-get-column-count source)))
                         "\t")
              "\n"))))

(defun exc-tsv-find-file (file)
  "Show the tab-separated FILE as a grid."
  (interactive "fTSV file: ")
  (exc-api-create
   :data-source (exc-tsv-create-data-source file)
   :title (file-name-nondirectory file)
   :file-path file))

(exc-register-plugin 'tsv)
(exc-register-data-source-type 'tsv #'exc-tsv-create-data-source)

(provide 'exc-tsv)
```

With a file holding `Item Qty Price`, `pen 3 1.5` and `ink 2 4`
separated by tabs, `M-x exc-tsv-find-file` shows three rows with
`Item`, `Qty` and `Price` as the header row. Typing `total` in `A4` and
`=SUM(B2:B3)` in `B4` shows `5`, and `C-x C-s` writes the file back with
the formula as text:

```
Item	Qty	Price
pen	3	1.5
ink	2	4
total	=SUM(B2:B3)	
```

## Commands and keys

A command reads the grid of the current buffer from
`exc--data-source`:

```elisp
(defun my-grid-size ()
  "Say how many rows and columns the grid has."
  (interactive)
  (message "%d rows, %d columns"
           (exc-get-row-count exc--data-source)
           (exc-get-column-count exc--data-source)))

(define-key exc-mode-map (kbd "C-c x") #'my-grid-size)
```

Grids are shown in `exc-etaf-mode`, which derives from
`exc-mode`, so a key bound in `exc-mode-map` works in every
grid unless the view binds it itself. The view binds the arrows, `TAB`,
`RET`, plain characters and many `C-c` keys ([KEYS.md](KEYS.md)); pick a
free key, and check it with `C-h k` in a grid.

After changing cells from Lisp, `exc-refresh` redraws the grid.

## The plugins that come with exc.el

| File | How it hooks in |
|---|---|
| `exc-csv.el` | Registers the `csv` type and the `csv` plugin. `exc-find-file-csv` visits the file and shows it as a grid, its header line as the first row; `C-x C-s` writes it back as CSV through `:save-to-file`. `exc-csv-save` writes it to another file. |
| `exc-xlsx.el` | Registers the `xlsx` type and the `xlsx` plugin. The registered function, `exc-xlsx-create-data-source`, reads one sheet of a workbook through the Rust module; `exc-xlsx-find-file` opens every sheet as a workbook. |
| `exc-org.el`, `exc-markdown.el` | Register nothing. They read a `.grid` file with `exc-persistence-create-data-source-from-file`, draw it as text (Markdown with `exc-text-render`, Org with its own table code), and insert or refresh the text in an Org or Markdown buffer. |
| `exc-rst.el` | Registers nothing. `exc-export-as-rst` and `exc-rst-insert-table-from-file` write a grid as an RST table. |
| `exc-example-plugin.el` | Loads, but most of it does not work: see below. |

### About the example plugin

`exc-example-plugin.el` is older than the current view. Requiring
it registers nothing until `exc-example-plugin-init` is called.
Its data source, `exc-example-create-data-source`, fails with
`wrong-type-argument`, so `exc-example-demo` fails too. Its three
renderers are never used (see above), and
`exc-example-highlight-duplicates` calls `exc-highlight-cell`,
which does not exist. `exc-example-export-to-html` works. Use the
example above, or `exc-csv.el`, as a model instead.
