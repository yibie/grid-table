[English](README.md) | [中文](README_CN.md)

# 🎯 grid-table - Emacs Advanced Table System

<div align="center">

![figure](pictures/figure1.jpg)

<p><i>A native table system for Emacs! Supports rich text, formula calculation, and plugin extensions 🔥</i></p>

</div>

## ✨ Features

<div align="center">

| Appearance | Data Calculation | Extensibility |
|--------------|---------------|-------------|
| Rich Text Rendering<br/> Cell Images<br/> Adaptive Column Widths<br/> Unicode Borders |  Formula Engine (Excel Style)<br/>📍 Cell References<br/> Real-time Calculation<br/> Elisp Expressions |  Plugin Architecture<br/>🔗 Multiple Data Sources<br/> Format Export<br/>🔧 Custom Extensions |

</div>

### 🚀 Core Capabilities
- **Markdown Style Markup** - Write resume summaries in cells
- **Image Visualization** - Display images directly in tables!
- **Excel-Level Formulas** - `=B2*C2· =SUM(A1:A10)· =IF(...)`
- **Elisp Super Formulas** - Powerful `=elisp:(cell"A1")` expressions
- **Real-time Preview** - "Static Preview + Dynamic Editing" in Org/Markdown

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
(require 'grid-table-csv)      ;; CSV支持
(require 'grid-table-org)      ;; Org集成
(require 'grid-table-markdown) ;; Markdown集成
```

## 📋 Operation Quick Reference

### File Operations Overview
| 🎯 Action | 📝 Command | 🔑 Shortcut | ⚡ Tip |
|--------|--------|-----------|---------|
| New | `M-x grid-table-create` | - | Create a blank table |  
| Open | `M-x grid-table-find-file` | `C-c C-f` | Open .grid file |
| Save | - | `C-c C-w` | Smart path suggestion |
| CSV | `M-x grid-table-find-file-csv` | - | Directly read CSV |

###   Editing Operations Quick Reference
| 🎯 Action | 🤞 Shortcut | 📝 Effect | ⚠️ Note |
|--------|-----------|---------|---------|
| **✏️ Basic Editing** | |||
| Cell Editing | `e` or right-click | Enter edit mode | Cell focus |
| Title Editing | `C-c t` | Modify table title | Global display |
| **➕ Row Operations** | |||
| Insert Row Below | `C-c r a` | Create new row below current row | Smart format inheritance |
| Delete Current Row | `C-c r d` | Delete entire row | ⚠️ **Table headers cannot be deleted** |
| **➕ Column Operations** | |||
| Insert Column to Right | `C-c c a` | Create new blank column | Auto-adjust width |
| Delete Current Column | `C-c c d` | Delete entire column | Clear data |

### Navigation Control Diagram
| 🎯 Direction | ⌨️ Key | 🖱️ Operation |
|---------|---------|----------|
| ⬆️ Up | `p` or `↑` | Previous row |
| ⬇️ Down | `n` or `↓` | Next row |
| ⬅️ Left | `S-TAB` or `←` | Left cell |
| ➡️ Right | `TAB` or `→` | Right cell |
| 🔄 Refresh | `g` | Re-render |

**🎯 Tip**: Use `n`/`p` for vertical movement, `TAB` for horizontal movement

## 🧮 Formula Complete Guide
### Basic Syntax Comparison Table
| 🎯 Type | 📝 Syntax | 📋 Example | 💡 Description |
|--------|---------|---------|---------|
| 📍 Cell | `=A1`、`=B2` | `=B2*C2` | Relative reference |
| 📊 Range | `=RANGE(A1:B5)` | `=SUM(A1:A10)` | Continuous block |
| ✨ Function | `=FUNCTION(args)` | `=AVERAGE(B2:B10)` | Built-in function set |

### Official Built-in Function List
| 📈 Math Functions | 📊 Statistical Functions | 🎯 Conditional Functions | 🎨 Text Functions |
|-------------|-------------|-------------|-------------|
| `SUM` Sum | `AVERAGE` Average | `IF` Conditional | `CONCAT` Concatenate |
| `PRODUCT` Product | `COUNT` Count | `AND/OR` Logical | `LEFT/RIGHT` Slice |
| `MOD` Modulus | `MAX/MIN` Extremes | `NOT` Invert | `LEN` Length |

### 🎯 Complete Formula Function Library (Based on grid-table-calc.el)

The engine supports **30+ Excel-compatible functions**, organized by category as follows:

#### 📊 **Math Functions**
| Function | Example | Function Description |
|------|------|----------|
| `SUM` | `=SUM(A1:A10)` | Sum all values in the range |
| `AVERAGE` | `=AVERAGE(B2:B10)` | Calculate average |
| `ROUND` | `=ROUND(3.1415, 2)` | Round to 2 decimal places |
| `MOD` | `=MOD(A1, B2)` | Modulus operation |
| `POWER` | `=POWER(2, 3)` | Power operation |
| `SQRT` | `=SQRT(16)` | Square root |
| `ABS` | `=ABS(-5)` | Absolute value |

#### 🔍 **Advanced Query Functions**
| Function | Example | Function Description |
|------|------|----------|
| `VLOOKUP` | `=VLOOKUP("苹果", A2:C6, 2)` | Exact vertical lookup |
| `INDEX` | `=INDEX(A1:C3, 2, 3)` | Return the value of the 2nd row and 3rd column |
| `MATCH` | `=MATCH(100, A1:A10)` | Find the position of the value (1-based) |

#### 🎯 **Conditional Statistics Functions (Essential for Smart Use)**
| Function | Example | Function Description |
|------|------|----------|
| `COUNTIF` | `=COUNTIF(A1:A10, ">50")` | Count based on condition |
| `SUMIF` | `=SUMIF(A1:A10, ">=60", B1:B10)` | Sum based on condition |
| `COUNTA` | `=COUNTA(A1:A10)` | Count non-empty cells |
| `ISBLANK` | `=ISBLANK(A1)` | Check for empty values |

#### 📅 **Date Functions**
| Function | Example | Function Description |
|------|------|----------|
| `TODAY()` | `=TODAY()` | Current date |
| `NOW()` | `=NOW()` | Current date + time |
| `YEAR` | `=YEAR(A1)` | Extract year |
| `MONTH` | `=MONTH(A1)` | Extract month (1-12) |
| `WEEKDAY` | `=WEEKDAY(TODAY())` | Day of the week (1-based, Sunday=1) |
| `EOMONTH` | `=EOMONTH(A1, 3)` | End of month 3 months later |

#### 📝 **Text Processing Functions**
| Function | Example | Function Description |
|------|------|----------|
| `LEN` | `=LEN(A1)` | String length |
| `LEFT` | `=LEFT("Hello", 3)` | Extract first 3 characters: "Hel" |
| `MID` | `=MID("World", 2, 2)` | Extract 2 characters starting from the 2nd position: "or" |
| `FIND` | `=FIND("World", A1)` | Find the position of the substring |
| `SUBSTITUTE` | `=SUBSTITUTE(A1, "World", "Hello")` | Replace substring |

#### 🔄 **Conditional Judgment Scenarios**
```excel
=IF(A1>=90, "优秀", IF(A1>=80, "良好", IF(A1>=60, "及格", "不及格")))
=IFS(A1>100, "超高", A1>=80, "高", A1>=60, "中", TRUE, "低")
=IFERROR(复杂公式, "计算错误")
```

#### ❌ **Error Handling System**
- **Zero Division Protection**: Avoid division by zero
- **Type Conversion**: Text "123" automatically converted to a number
- **Empty Value Handling**: Empty strings are treated as 0 in math

#### 💡 **Absolute/Relative References** (Advanced Usage)
| Symbol | Meaning | Example |
|------|------|------|
|  `$A$1`  | Absolute row and column | Fixed reference |
|  `A$1`   | Absolute row, relative column | Row fixed | 
|  `$A1`   | Relative row, absolute column | Column fixed |

### Elisp Formula Advanced Techniques (Expert Level)
```elisp
=elisp:(+ 1 2 3)                                ; 📍 Basic calculation
=elisp:(+ (cell "A1") (cell "B2"))              ; 📍 Cell reference
=elisp:(let ((total (+ (cell "A1") A2 A3)))      ; 📍 Three numbers sum
          (if (> total 100) "Over budget" total))

=elisp:(format "%.1f%%" (* 100 (cell "占比")))   ; 📍 Percentage format
=elisp:(concat "This quarter total: "                     ; 📍 String concatenation 
          (number-to-string (cell "C2")))
=elisp:(+ (reduce #'+ A1:A10) (reduce #'+ B1:B5)) ; 📍 Multiple area sum
```

For more detailed guidance, please refer to [ELISP_FORMULA_GUIDE.md](docs/ELISP_FORMULA_GUIDE.md).

> ⚠️ **Security Warning**: `=elisp:` executes arbitrary Elisp code! **Only use in fully trusted documents**  
> 🔒 **Suggestion**: Verify external files are safe and reliable through `M-x checkdoc`

## 🔗 Format Integration Guide

### Org Mode Deep Integration
```org
#+BEGIN: grid-table
#+OPTIONS: :file "~/project/data.grid" :width 800 :height 400

**Here is the table preview** (static read-only)

#+END:
```

**Three steps**:
1. `M-x grid-table-org-insert-block` - Insert special block
2. `M-x grid-table-org-refresh-block` - Re-render preview 
3. `M-x grid-table-org-open-block` - Open edit window (hide preview)

### Markdown GitHub Style
```markdown
```grid-table
:file ~/project/data.grid
:width 100%
```
**Special preview area** (safe read-only)
```
```
</markdown>

### reStructuredText Support
| 🎯 Scenario | 📝 Command | 📤 Result |
|---------|---------|---------|
| Export from grid | `M-x grid-table-export-as-rst` | 📄 Standard RST format |
| Insert existing file | `M-x grid-table-rst-insert-table-from-file` | 🔗 Smart path recognition |

## Core Technical Principles

### Data Flow Architecture Diagram
```
User Interface ←→ grid-table.el ←→ Core Engine
                     ↓                ↓
             data-source API ←→ grid-data-model
                     ↓                ↓
         CSV Plugin ←→ Org Plugin ←→ Custom Data Source
```

### 🔑 Core API Quick Reference
| 🤝 Interface Type | 🔗 Key | 📝 Return Type | 💡 Key Usage |
|-------------|----------|---------------|-------------|
| **Data Retrieval** | | | |
| Raw Value | `:get-raw-value-at` | String | Value before formula |
| Computed Value | `:get-computed-value-at` | Any | Formula calculation result |
| **Structure Management** | | | |
| Dimension Query | `:get-row-count` | Integer | Import data statistics |
| Add Unit | `:add-row` / `:add-column` | Boolean | Success/Failure |
| **Extension Interface** | | | |
| Load Interface | `:load-from-file` | Object | Custom file support |
| Save Interface | `:save-to-file` | Boolean | Format export logic |

## 📁 Code Organization Structure

```
grid-table/                    # Project root directory
├── core/                      # Core engine collection
│   ├── grid-table.el          # Main entry + UI rendering
│   ├── grid-data-model.el     # Data model core
│   ├── grid-data-source.el    # Data source abstraction
│   ├── grid-table-api.el      # Public API
│   ├── grid-table-calc.el     # Formula calculation engine
│   ├── grid-table-nav.el      # Navigation control logic
│   ├── grid-table-parser.el   # Text parsing tool
│   └── grid-table-persistence.el # Persistence support
├── plugins/                   # Plugin extension system
│   ├── grid-table-csv.el      # CSV format support
│   ├── grid-table-org.el      # Org mode integration
│   ├── grid-table-markdown.el # Markdown integration
│   ├── grid-table-rst.el      # reStructuredText export
│   └── grid-table-example-plugin.el # Plugin development example
├── docs/                      # Technical documentation
│   ├── ELISP_FORMULA_GUIDE.md     # Formula development guide
│   └── PLUGIN_DEVELOPMENT.md      # Plugin development manual
└── pictures/                  # Product screenshots and demos
```

## 📜 CHANGELOG

- 0.2.0 (2025-08-12)
  - Enhanced environment adaptability - the table now displays properly regardless of user font configuration
  - Added over 20 major Excel formulas
- 0.1.0 (2025-08-11)
  - Initial release

## 🤝 How to Contribute

### ⚡ Plugin Development Quick Start

#### 3 Types of Plugins (1 Minute to Master)

| 🎯 Type | 📝 Function Description | 🎯 Applicable Scenarios |
|---------|-------------|-----------|
| 🎨 **Cell Renderer** | Custom cell display appearance | Progress bars, currency formats, etc. |
| 📊 **Data Source Plugin** | Support new file formats | JSON, Excel reading |
| ⚡ **Feature Plugin** | Add new commands | Data export, chart generation |

#### Quick Start Template (Copy and Use)
```elisp
;;; my-plugin.el --- Quick template -*- lexical-binding: t -*-
(require 'grid-table-plugins)

;; Simple currency format renderer
(defun my-currency-renderer (value &optional cell-props)
  "Currency format renderer, convert numbers to ¥XX.XX format"
  (if (and value (stringp value))
      (let ((num (string-to-number value)))
        (format "¥%.2f" num))
    ""))

;; Register renderer (done!)
(defun my-plugin-init ()
  (grid-table-register-cell-renderer 'currency #'my-currency-renderer)
  (grid-table-register-plugin 'my-plugin))

(my-plugin-init)
(provide 'my-plugin)
```

> 📚 **Complete Development Guide** → [Plugin Development Manual](docs/PLUGIN_DEVELOPMENT.md) (From Beginner to Expert)

### 🐛 Quick Feedback Path
- 📧 **Bug Report** → [GitHub Issues](https://github.com/your-repo/issues)  
- ✨ **Feature Suggestion** → [Discussion Forum](https://github.com/your-repo/discussions)  
- 🔧 **Plugin Contribution** → [Plugin Development Manual](docs/PLUGIN_DEVELOPMENT.md)

### 🎯 Community Contribution Directions
| 🎯 类型 | 📋 需求描述 | 🏷️ 标签 |
|---------|-------------|----------|
| **Data Source Plugin** | Excel/JSON/Database Data Source | `enhancement`, `plugin` |
| **Format Export** | LaTeX/ASCII/HTML/JSON Export | `extending`, `format` |
| **Theme Plugin** | Dark Mode | `UI/UX`, `theme` |
| **Chart Plugin** | Data Visualization | `visualization` |


---

<div align="center">

**Built with ❤️ for the Emacs community**  

*Supporting efficient production workflows since 2025*  

🤝 **[→ Join Developer Community ←](https://github.com/yibie/grid-table/discussions)**

</div>
