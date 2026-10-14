[English](README.md) | [中文](README_CN.md)

# 🎯 grid-table - Emacs Advanced Table System

<div align="center">

![figure](pictures/figure1.jpg)

<p><i>A native table system for Emacs! Supports rich text, formula calculation, and plugin extensions 🔥</i></p>

</div>

## ✨ Features

<div align="center">

| Appearance                                                                            | Data Calculation                                                                                      | Extensibility                                                                                |
| ------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| Rich Text Rendering<br/> Cell Images<br/> Adaptive Column Widths<br/> Unicode Borders<br/> Merged Cells | Formula Engine (Excel Style)<br/>📍 Cell References<br/> Real-time Calculation<br/> Elisp Expressions | Plugin Architecture<br/>🔗 Multiple Data Sources<br/> Format Export<br/>🔧 Custom Extensions |

</div>

### 🚀 Core Capabilities

- **Markdown Style Markup** - Write resume summaries in cells
- **Image Visualization** - Display images directly in tables!
- **Excel-Level Formulas** - `=B2*C2· =SUM(A1:A10)· =IF(...)`
- **Elisp Super Formulas** - Powerful `=elisp:(cell"A1")` expressions
- **Real-time Preview** - "Static Preview + Dynamic Editing" in Org/Markdown
- **Merged Cells** - Horizontal and vertical spans with border-aware navigation

### ↔️ Merged Cells

<div align="center">

<img src="pictures/merged-cells.png" alt="Horizontal and vertical merged cells" width="552">

</div>

Press `C-c m` and enter a same-row or same-column A1 range to merge it. Press `C-c u` to unmerge the cell at point. While editing, `M-RET` inserts a newline for multi-line content.

## 🚀 30 Seconds to Get Started

### 📦 1. Installation and Configuration

#### 🎯 Recommended: Use `use-package`

```elisp
;; 🚄 One-stop configuration - Recommended!
(use-package grid-table
  :load-path ("/path/to/grid-table" "/path/to/grid-table/plugins")
  :config
  (require 'grid-table)
  (require 'grid-table-plugins)

  ;; 📁 Custom save directory
  (setq grid-table-default-save-directory "~/Documents/表格/")

  ;; 🖼️ Image display optimization
  (setq grid-table-image-target-char-height 8)
  (setq grid-table-image-max-width-ratio 0.9))
```

#### 📋 Traditional Way

```elisp
;; 📍 Manual configuration path
(add-to-list 'load-path "/path/to/grid-table")
(add-to-list 'load-path "/path/to/grid-table/plugins")

;; 🎯 Load on demand
(require 'grid-table)          ;; Core functionality
(require 'grid-table-plugins)  ;; Plugin system
(require 'grid-table-csv)      ;; CSV support
(require 'grid-table-org)      ;; Org integration
(require 'grid-table-markdown) ;; Markdown integration
```

#### 🖼️ Renderers

`grid-table-renderer` chooses how a grid is drawn:

- `etaf` (default) mounts the grid through [ETAF](https://github.com/emacs-taf). Only the rows and columns that fit the window are rendered, cells are edited in place, and `S-<arrow>` selects a range. It needs the `ecss`, `ebox` and `etaf` repositories on `load-path`:

  ```elisp
  (add-to-list 'load-path "/path/to/ecss")
  (add-to-list 'load-path "/path/to/ebox")
  (add-to-list 'load-path "/path/to/etaf")
  ```

  Keys follow spreadsheet habits: typing replaces the cell and starts editing, `F2` or `RET` edits the existing text, `RET` stores it and moves down, `TAB` stores it and moves right, `C-c C-c` stores it in place and `C-c C-k` cancels. `C-n`/`C-p`/`C-f`/`C-b` and the arrow keys move, `DEL` clears the selection, `M-w`/`C-w`/`C-y` copy, cut and paste a range with its formats, `C-d` and `C-r` fill the selection down or right from its first row or column, `C-c C-s` continues a series (1, 2 → 3, 4; a date day by day; `Week 1` → `Week 2`), `C-s` finds the next cell holding a text (`C-u C-s` backward) and `M-%` replaces in the selection or the whole sheet, `C-/` undoes and `C-?` redoes, `C-x h` selects everything, `C-c C-r` refreshes and `C-c C-q` quits. With the mouse, click a cell to move there, shift-click or drag to select a range, click a row number, a column letter or the corner to select a row, a column or the whole sheet, and drag the right edge of a column letter to resize the column; the width is saved with the grid and stays with its column. A paste that reaches past the last row or column makes the grid grow. The single-letter commands in the tables below (`e`, `n`, `p`, `g`, `q`) belong to the classic renderer.

  Formatting applies to the selection: `C-c f b` bold, `C-c f i` italic, `C-c f c` text color, `C-c f g` fill color, `C-c f l`/`e`/`r` align left, center or right, `C-c f x` clear. Number formats: `C-c f ,` number with thousands separators, `C-c f p` percent, `C-c f $` currency, `C-c f d` date, `C-c f t` text, `C-c f G` general, and `C-c f +`/`-` for more or fewer decimal places. `C-c f n` adds a conditional format (for example "greater than 100" in red) that keeps following the values, and `C-c f N` removes the ones touching the selection. Formats move with their cells when rows and columns are inserted, deleted or sorted, and are saved in the `.grid` file. A file with formats is written as format version 4, which earlier releases cannot open; files without formats keep their older version.

  With several cells selected, `C-c m` merges them into one; the range can be a rectangle, not only a row or a column (the classic renderer shows a rectangular merge as separate cells). A chart formula is drawn inside its cell at the chart's own size, so its row and column grow to fit; `C-c v` still opens it in its own buffer.

  `C-c s` sorts by what a column shows: numbers by value before text, text ignoring case, empty cells last in either direction. `C-c / f` filters the rows by a condition on the cursor's column, `C-c / v` keeps the rows holding the cursor cell's value, `C-c / c` clears that column's filter and `C-c / a` clears all. Filters on several columns combine, a filtered column shows `▼` after its letter, and moving, copying, clearing, formatting and the status bar figures leave hidden rows out. Rows are filtered when a filter is set and on refresh (`C-c C-r`), not after each edit.

  The first row of a sheet is its header row: it stays in place when the sheet is sorted or filtered and cannot be deleted. `C-c w h` turns that off for a sheet that is not one table (a title on top, several blocks, a summary), and its first row is then a row like any other: it sorts, filters, can be deleted and merged downward, and `C-c r i` can insert a row above it. The setting is saved with the grid.

  `C-c w f` freezes the rows above the cursor and the columns left of it: they stay at the top and the left of the window while the rest of the sheet scrolls, with a colored line along them (face `grid-table-etaf-freeze-face`). `C-c w u` releases them, as does `C-c w f` in the first cell. A row or column inserted among the frozen ones is frozen with them. What is frozen is saved with the grid; it does not travel to xlsx, and freezing itself is not an undo step.

  A grid can hold several sheets. `C-c w n` adds one, `C-c w r` renames the current sheet, `C-c w d` deletes it, `C-c w <` and `C-c w >` move it, `C-<next>` and `C-<prior>` show the next and the previous sheet, `C-c w s` asks for one by name, and with ETAF the sheets have tabs on the bottom line that can be clicked. A formula reads another sheet as `Sheet2!A1`, `SUM(Sheet2!A1:B5)` or `'My Sheet'!A1`; it follows a renamed sheet and rows and columns added or deleted there, and shows `#REF!` once the sheet is gone. Each sheet keeps its own cursor, filters and undo history. A grid with one sheet is saved as before; with several the file has version 5, which earlier releases do not read.

- `classic` draws box characters into the buffer and edits through the minibuffer. It is the fallback during the migration to ETAF: it is used automatically when ETAF cannot be loaded, and it will be removed once the migration is complete.

Run the tests with `make test-classic` and `make test-etaf ETAF_SRC=/path/to/checkouts`.

## 📋 Operation Quick Reference

### File Operations Overview

| 🎯 Action | 📝 Command                     | 🔑 Shortcut | ⚡ Tip                |
| --------- | ------------------------------ | ----------- | --------------------- |
| New       | `M-x grid-table-create`        | -           | Create a blank table  |
| Open      | `M-x grid-table-find-file`     | `C-c C-f`   | Open .grid file       |
| Save      | -                              | `C-c C-w`   | Smart path suggestion |
| CSV       | `M-x grid-table-find-file-csv` | -           | Directly read CSV     |
| Excel     | `M-x grid-table-xlsx-find-file` | -          | Open an xlsx workbook |

### Editing Operations Quick Reference

| 🎯 Action                | 🤞 Shortcut        | 📝 Effect                        | ⚠️ Note                                |
| ------------------------ | ------------------ | -------------------------------- | -------------------------------------- |
| **✏️ Basic Editing**     |                    |                                  |                                        |
| Cell Editing             | `e` or right-click | Enter edit mode                  | Cell focus                             |
| Title Editing            | `C-c t`            | Modify table title               | Global display                         |
| Merge Cells              | `C-c m`            | Merge an A1 range                | Same row or same column only           |
| Unmerge Cell             | `C-c u`            | Unmerge the cell at point        | Keeps the anchor value                 |
| **➕ Row Operations**    |                    |                                  |                                        |
| Insert Row Below         | `C-c r a`          | Create new row below current row | Smart format inheritance               |
| Delete Current Row       | `C-c r d`          | Delete entire row                | ⚠️ **Table headers cannot be deleted** |
| **➕ Column Operations** |                    |                                  |                                        |
| Insert Column to Right   | `C-c c a`          | Create new blank column          | Auto-adjust width                      |
| Delete Current Column    | `C-c c d`          | Delete entire column             | Clear data                             |

### Navigation Control Diagram

| 🎯 Direction | ⌨️ Key         | 🖱️ Operation |
| ------------ | -------------- | ------------ |
| ⬆️ Up        | `p` or `↑`     | Previous row |
| ⬇️ Down      | `n` or `↓`     | Next row     |
| ⬅️ Left      | `S-TAB` or `←` | Left cell    |
| ➡️ Right     | `TAB` or `→`   | Right cell   |
| 🔄 Refresh   | `g`            | Re-render    |

**🎯 Tip**: Use `n`/`p` for vertical movement, `TAB` for horizontal movement

## 🧮 Formula Complete Guide

### Basic Syntax Comparison Table

| 🎯 Type     | 📝 Syntax         | 📋 Example         | 💡 Description        |
| ----------- | ----------------- | ------------------ | --------------------- |
| 📍 Cell     | `=A1`、`=B2`      | `=B2*C2`           | Relative reference    |
| 📊 Range    | `=RANGE(A1:B5)`   | `=SUM(A1:A10)`     | Continuous block      |
| ✨ Function | `=FUNCTION(args)` | `=AVERAGE(B2:B10)` | Built-in function set |
| 📌 Anchor   | `=$A$1`、`=A$1`   | `=B2*$C$1`         | `$` keeps a column or row fixed when a formula is copied |
| ➗ Operator | `+ - * / ^ & %`   | `=A1&" kg"`、`=B2*15%` | Arithmetic, power and text joining; `/` gives `2.5` for `=10/4`; `%` after a value divides it by 100 (the remainder is `MOD(7,3)`) |
| 📏 Whole    | `=A:A`、`=2:5`    | `=SUM(B:B)`        | Whole columns or rows, up to the edges of the sheet |
| ⚖️ Compare  | `= <> < > <= >=`  | `=IF(A1<=5,"low","high")` | Numbers compare by value, text ignores case; `TRUE`/`FALSE` are literals |

A cell keeps a number and shows it through its format. What is typed is read as it was meant: `2026-10-13` (or `2026/10/13`, with an optional time) is stored as a date, `12.5%` as 0.125, `$1,234.50` as 1234.5, each shown as typed; other date orders such as `10/13/2026` are not guessed and stay text. A date is a number of days, so `=A2+7` is a week later (give that cell a date format with `C-c f d`), and a formula that starts with `TODAY`, `NOW`, `DATE`, `EDATE`, `EOMONTH` or `DATEVALUE` is shown as a date by itself. `YEAR`, `MONTH`, `DAY`, `WEEKDAY`, `DAYS`, `TEXT` and `VALUE` are there too. Dates kept as text in older grids are left alone; `DATEVALUE` turns one into a date. A bare number typed into a cell with a percentage format is that many percent (`50` is 50%). Start a cell with an apostrophe (`'007`) to keep it as text.

Function names and references may be typed in either case. `SUM`, `AVERAGE`, `COUNT`, `COUNTA`, `MAX` and `MIN` take any number of values and ranges (`=SUM(A1:A5,C1,10)`). `IF` only computes the branch it picks, so `=IF(B2=0,0,A2/B2)` is safe. `ROUND` takes a half away from zero. `XLOOKUP(value, look_in, return_from, if_missing)` finds exact matches; `VLOOKUP` without a fourth argument takes its first column as sorted and finds the last row not above the value, and with `FALSE` matches exactly. `MEDIAN`, `LARGE`, `SMALL`, `RANK`, `SUMPRODUCT`, `TRUNC`, `FLOOR`, `CEILING` and `MROUND` are there too. Arithmetic takes an empty cell as 0, and text that reads as a number or a date as that number; any other text makes the formula `#VALUE!` instead of counting as 0. Sums and other aggregates pass over text, including numbers kept as text. Sorting moves a formula with its row, so `=B2*2` in a row still reads that row afterwards. Formulas are computed in dependency order wherever they sit in the sheet. A formula that cannot be computed shows an error value in its own cell and in the cells that read it: `#DIV/0!`, `#NAME?` (unknown function), `#N/A` (no match), `#REF!`, `#VALUE!` (text where a number is needed), `#NUM!`, `#CYCLE!` (circular reference) or `#ERROR!`. Wrap a formula in `IFERROR` to replace the error.

### Official Built-in Function List

| 📈 Math Functions | 📊 Statistical Functions | 🎯 Conditional Functions | 🎨 Text Functions    |
| ----------------- | ------------------------ | ------------------------ | -------------------- |
| `SUM` Sum         | `AVERAGE` Average        | `IF` Conditional         | `CONCAT` Concatenate |
| `PRODUCT` Product | `COUNT` Count            | `AND/OR` Logical         | `LEFT/RIGHT` Slice   |
| `MOD` Modulus     | `MAX/MIN` Extremes       | `NOT` Invert             | `LEN` Length         |

### Elisp Formula Advanced Techniques (Expert Level)

```elisp
=elisp:(+ 1 2 3)                      ; 🎯 Basic calculation
=elisp:(+ (cell "A1") (cell "B2"))     ; 📍 Cell reference
=elisp:(format "%.2f%%" (* 100 (cell "完成率"))) ; 🎨 Formatted string
=elisp:(let ((x (cell "B2"))) (* x x)) ; 🔧 Complex logic
```

_This feature is turned off by default._ For more detailed guidance, please refer to [ELISP_FORMULA_GUIDE.md](docs/ELISP_FORMULA_GUIDE.md).

> ⚠️ **Security Warning**: `=elisp:` executes arbitrary Elisp code! **Only use in fully trusted documents**  
> 🔒 **Suggestion**: Verify external files are safe and reliable through `M-x checkdoc`

## 📊 Chart Visualization Guide

grid-table provides built-in chart functions that render ASCII/Unicode charts directly in cells. Charts use high-resolution Braille canvas for beautiful terminal-based visualizations.

### Available Chart Functions

| 📊 Chart Type | 📝 Function  | 💡 Description                   |
| ------------- | ------------ | -------------------------------- |
| Bar Chart     | `BARPLOT`    | Horizontal bar chart             |
| Column Chart  | `COLUMNPLOT` | Vertical column chart            |
| Line Chart    | `LINEPLOT`   | Line plot with Braille rendering |
| Scatter Plot  | `SCATTER`    | Scatter plot visualization       |
| Histogram     | `HISTOGRAM`  | Distribution histogram           |
| Box Plot      | `BOXPLOT`    | Statistical box plot             |
| Density       | `DENSITY`    | KDE density curve                |
| Count         | `COUNTPLOT`  | Frequency count chart            |
| Multi-line    | `MULTILINE`  | Multiple series line chart       |
| Sparkline     | `SPARKLINE`  | Compact inline trend             |

### Chart Syntax Examples

#### BARPLOT - Horizontal Bar Chart

```
=BARPLOT(labels, values, [title], [width])
```

**Example:**

```
=BARPLOT(A1:A4, B1:B4, "Revenue by Quarter", 50)
```

#### COLUMNPLOT - Vertical Column Chart

```
=COLUMNPLOT(labels, values, [title], [width], [height])
```

**Example:**

```
=COLUMNPLOT(A1:A4, B1:B4, "Monthly Sales", 40, 15)
```

#### LINEPLOT - Line Chart

```
=LINEPLOT(x-data, y-data, [title], [width], [height])
```

**Example:**

```
=LINEPLOT(A1:A10, B1:B10, "Trend Analysis")
```

#### HISTOGRAM - Distribution Histogram

```
=HISTOGRAM(values, [nbins], [title], [width])
```

**Example:**

```
=HISTOGRAM(C1:C100, 10, "Score Distribution")
```

#### BOXPLOT - Statistical Box Plot

```
=BOXPLOT(series1, series2, ..., [title], [width])
```

**Example:**

```
=BOXPLOT(A1:A20, B1:B20, "Comparison", 60)
```

#### SPARKLINE - Compact Trend Line

```
=SPARKLINE(data)
=SPARKLINE(val1, val2, val3, ...)
```

**Example:**

```
=SPARKLINE(A1:A10)
=SPARKLINE(10, 15, 13, 17, 19)
```

### Chart Preview Gallery

Add demo screenshots under `pictures/` and they will render here.

| Type | Preview |
| --- | --- |
| `BARPLOT` | <img alt="BARPLOT preview" src="pictures/chart-barplot.png" width="260" /> |
| `COLUMNPLOT` | <img alt="COLUMNPLOT preview" src="pictures/chart-columnplot.png" width="260" /> |
| `LINEPLOT` | <img alt="LINEPLOT preview" src="pictures/chart-lineplot.png" width="260" /> |
| `SCATTER` | <img alt="SCATTER preview" src="pictures/chart-scatter.png" width="260" /> |
| `HISTOGRAM` | <img alt="HISTOGRAM preview" src="pictures/chart-histogram.png" width="260" /> |
| `BOXPLOT` | <img alt="BOXPLOT preview" src="pictures/chart-boxplot.png" width="260" /> |
| `DENSITY` | <img alt="DENSITY preview" src="pictures/chart-density.png" width="260" /> |
| `COUNTPLOT` | <img alt="COUNTPLOT preview" src="pictures/chart-count.png" width="260" /> |
| `MULTILINE` | <img alt="MULTILINE preview" src="pictures/chart-multiline.png" width="260" /> |
| `SPARKLINE` | <img alt="SPARKLINE preview" src="pictures/chart-sparkline.png" width="260" /> |

### Chart Demo File

See `examples/chart-gallery.grid` for a complete demonstration:

```elisp
(:title "Chart Gallery"
 :headers ("Type" "Data1" "Data2" "Data3" "Data4" "Chart")
 :rows (
   ("Revenue" "100" "150" "120" "200" "=BARPLOT(B1:E1, B2:E2, \"Revenue\")")
   ("Trend"   "20"  "40"  "30"  "50"  "=LINEPLOT(B1:E1, B3:E3, \"Trend\")")
   ("Mini"    "10"  "12"  "11"  "15"  "=SPARKLINE(B4:E4)")
 ))
```

## 🔗 Format Integration Guide

### Org Mode Deep Integration

```org
#+BEGIN: grid-table
#+OPTIONS: :file "~/project/data.grid"

here display table preview (static preview)

#+END:
```

**three steps**:

1. `M-x grid-table-org-insert-block` - insert grid-table block (static preview)
2. `M-x grid-table-org-refresh-block` - refresh grid-table block (dynamic preview)
3. `M-x grid-table-org-open-block` - open grid-table block (edit mode)

### Markdown GitHub-style

````markdown
```grid-table
:file ~/project/data.grid
```
````

here display table preview (static preview)

```

```

</markdown>

### reStructuredText support

| 🎯 scenario               | 📝 command                                  | 📤 result                 |
| ------------------------- | ------------------------------------------- | ------------------------- |
| from grid export          | `M-x grid-table-export-as-rst`              | 📄 standard RST format    |
| insert existing grid file | `M-x grid-table-rst-insert-table-from-file` | 🔗 smart path recognition |

### Excel workbooks (xlsx)

`(require 'grid-table-xlsx)` adds two commands:

| Command | What it does |
| ------- | ------------ |
| `M-x grid-table-xlsx-find-file` | Open a workbook as a grid, with a sheet for each of its sheets |
| `M-x grid-table-xlsx-export` | Write the grid and all its sheets to a workbook |

Values, formulas, merges, bold, italic, text and fill color, alignment and conditional formats travel both ways. The first row of the sheet becomes the header row. A grid opened from a workbook is not tied to it: `C-c C-w` saves a `.grid` file, and exporting writes the workbook.

What does not travel:

- Dates, percentages, amounts of money and numbers with a fixed number of decimals travel both ways as numbers with their formats. A number format grid-table has no counterpart for (scientific notation, month names, units in quotes) is shown in the general format and written back unchanged. Text that reads as a number, such as `007`, stays text: it comes in as `'007`. A sheet whose first row is merged downward, such as a title over two rows, is opened without a header row so that the merge is kept. Dates before March 1900 are a day off, as the workbook's calendar has a February 29 that year.
- Borders, fonts, column widths, and conditional formats other than comparisons, "contains text", "is blank" and "is an error" are left out on import. On export the cells, merges and conditional formats of the sheet are replaced by the grid's, so those of them that were left out are gone; column widths and the rest of the sheet stay as they were, and the other sheets are untouched.
- A formula that uses a function grid-table lacks shows an error value in the grid. It is kept as written and exported unchanged.
- Written back to the workbook it was opened from, the grid's sheets replace the workbook's: a sheet deleted or renamed in the grid is gone from the file. Written into another workbook, they are added to it and its own sheets stay.
- Chart formulas are exported as text.

The file format is handled by a dynamic module written in Rust (`plugins/xlsx/`), so Emacs must be built with module support. `plugins/` holds a prebuilt module for macOS (`grid-table-xlsx-module.dylib`, Apple silicon and Intel), which is loaded as it is. On other platforms, build the module with `M-x grid-table-xlsx-build-module` or `make module`; building needs cargo. A module you build is used in place of the prebuilt one, and `grid-table-xlsx-module-file` names a module kept elsewhere.

## Core Technology Principles

### Data Flow Architecture Diagram

```
User Interface ←→ grid-table.el ←→ Core Engine
                ↓                ↓
          data-source API ← grid-data-model
                ↓                ↓
        CSV Plugin ←→ Org Plugin ←→ Custom Data Sources
```

### Core API Quick Reference Table

| 🤝 interface type        | 🔗 key                     | 📝 return value type | 💡 key purpose             |
| ------------------------ | -------------------------- | -------------------- | -------------------------- |
| **data acquisition**     |                            |                      |                            |
| raw value                | `:get-raw-value-at`        | String               | value before formula       |
| computed value           | `:get-computed-value-at`   | Any                  | formula calculation result |
| **structure management** |                            |                      |                            |
| dimension query          | `:get-row-count`           | Integer              | import data statistics     |
| new unit                 | `:add-row` / `:add-column` | Boolean              | success/failure            |
| **extension interface**  |                            |                      |                            |
| load interface           | `:load-from-file`          | Object               | custom file support        |
| save interface           | `:save-to-file`            | Boolean              | format export logic        |

## 📁 code organization structure

```
grid-table/                    # project root directory
├── grid-table.el              # main entry + UI rendering
├── grid-data-model.el         # data model core
├── grid-data-source.el        # data source abstraction
├── grid-table-api.el          # public API
├── grid-table-calc.el         # formula calculation engine
├── grid-table-chart.el        # chart visualization engine
├── grid-table-nav.el          # navigation control logic
├── grid-table-parser.el       # text parsing tool
└── grid-table-persistence.el  # persistence support
├── plugins/                   # plugin extension system
│   ├── grid-table-csv.el      # CSV format support
│   ├── grid-table-xlsx.el     # Excel workbook support
│   ├── xlsx/                  # Rust source of the xlsx module
│   ├── grid-table-org.el      # Org mode integration
│   ├── grid-table-markdown.el # Markdown integration
│   ├── grid-table-rst.el      # reStructuredText export
│   └── grid-table-example-plugin.el # plugin development example
├── examples/                  # example files
│   └── chart-gallery.grid     # chart demonstration
├── docs/                      # technical documentation
│   ├── ELISP_FORMULA_GUIDE.md     # formula development guide
│   └── PLUGIN_DEVELOPMENT.md      # plugin development manual
└── pictures/                  # product screenshots and demonstrations
```

## 🤝 how to contribute

### ⚡ plugin development quick start

#### 3 types of plugins (1 minute to master)

| 🎯 type                   | 📝 function description        | 🎯 applicable scenario              |
| ------------------------- | ------------------------------ | ----------------------------------- |
| 🎨 **cell renderer**      | custom cell display appearance | progress bar, currency format, etc. |
| 📊 **data source plugin** | support new file formats       | JSON, Excel reading                 |
| ⚡ **function plugin**    | add new function commands      | data export, chart generation       |

#### quick start template (copy and use)

```elisp
;;; my-plugin.el --- quick template -*- lexical-binding: t -*-
(require 'grid-table-plugins)

;; simple currency formatter
(defun my-currency-renderer (value &optional cell-props)
  "Currency formatter, convert number to ¥XX.XX format"
  (if (and value (stringp value))
      (let ((num (string-to-number value)))
        (format "¥%.2f" num))
    ""))

;; register renderer (done!)
(defun my-plugin-init ()
  (grid-table-register-cell-renderer 'currency #'my-currency-renderer)
  (grid-table-register-plugin 'my-plugin))

(my-plugin-init)
(provide 'my-plugin)
```

> 📚 **complete development guide**→ [plugin development manual](docs/PLUGIN_DEVELOPMENT.md) (from入门 to expert level)

### 🐛 quick feedback path

- 📧 **bug report** → [GitHub Issues](https://github.com/your-repo/issues)
- ✨ **feature suggestion** → [discussion forum](https://github.com/your-repo/discussions)
- 🔧 **plugin contribution** → [plugin development manual](docs/PLUGIN_DEVELOPMENT.md)

### 🎯 community contribution direction

| 🎯 type                | 📋 demand description            | 🏷️ tag                  |
| ---------------------- | -------------------------------- | ----------------------- |
| **data source plugin** | Excel/JSON/Multiple data sources | `enhancement`, `plugin` |
| **format export**      | LaTeX/ASCII/HTML/JSON export     | `extending`, `format`   |
| **theme plugin**       | dark mode                        | `UI/UX`, `theme`        |
| **chart plugin**       | data visualization               | `visualization`         |

---

<div align="center">

**Built with ❤️ for the Emacs community**

_Since 2025, supporting efficient productivity workflows_

🤝 **[→ Join the developer community ←](https://github.com/yibie/grid-table/discussions)**

</div>
