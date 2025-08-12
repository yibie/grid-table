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

### Elisp Formula Advanced Techniques (Expert Level)
```elisp
=elisp:(+ 1 2 3)                      ; 🎯 Basic calculation
=elisp:(+ (cell "A1") (cell "B2"))     ; 📍 Cell reference
=elisp:(format "%.2f%%" (* 100 (cell "完成率"))) ; 🎨 Formatted string
=elisp:(let ((x (cell "B2"))) (* x x)) ; 🔧 Complex logic
```

*This feature is turned off by default.* For more detailed guidance, please refer to [ELISP_FORMULA_GUIDE.md](docs/ELISP_FORMULA_GUIDE.md).

> ⚠️ **Security Warning**: `=elisp:` executes arbitrary Elisp code! **Only use in fully trusted documents**  
> 🔒 **Suggestion**: Verify external files are safe and reliable through `M-x checkdoc`

## 🔗 Format Integration Guide

### Org Mode Deep Integration
```org
#+BEGIN: grid-table
#+OPTIONS: :file "~/项目/数据.grid" :width 800 :height 400

**这里显示表格预览** (静态只读)

#+END:
```

**三步操作**:
1. `M-x grid-table-org-insert-block` - 插入专用区块
2. `M-x grid-table-org-refresh-block` - 重新渲染预览 
3. `M-x grid-table-org-open-block` - 打开编辑窗口 (隐藏预览)

### Markdown GitHub风格
```markdown
```grid-table
:file ~/项目/数据.grid
:width 100%
```
**专用预览区域** (安全只读)
```
```
</markdown>

### reStructuredText支持
| 🎯 场景 | 📝 命令 | 📤 结果 |
|---------|---------|---------|
| 从grid导出 | `M-x grid-table-export-as-rst` | 📄 标准RST格式 |
| 插入现有机构文件 | `M-x grid-table-rst-insert-table-from-file` | 🔗 智能路径识别 |

##  核心技术原理

### 数据流架构图
```
用户界面 ←→ grid-table.el ←→ 核心引擎
                ↓                ↓
          data-source API ← grid-data-model
                ↓                ↓
        CSV插件 ←→ Org插件 ←→ 自定义数据源
```

### 🔑 核心API速查表
| 🤝 接口类型 | 🔗 键值 | 📝 返回值类型 | 💡 关键用途 |
|-------------|----------|---------------|-------------|
| **数据获取** | | | |
| 原始值 | `:get-raw-value-at` | String | 公式前的值 |
| 计算值 | `:get-computed-value-at` | Any | 公式计算结果 |
| **结构管理** | | | |
| 维度查询 | `:get-row-count` | Integer | 导入数据统计 |
| 新增单元 | `:add-row` / `:add-column` | Boolean | 成功/失败 |
| **扩展接口** | | | |
| 加载接口 | `:load-from-file` | Object | 自定义文件支持 |
| 保存接口 | `:save-to-file` | Boolean | 格式导出逻辑 |

## 📁 代码组织结构

```
grid-table/                    # 项目根目录
├── core/                      # 核心引擎集合
│   ├── grid-table.el          # 主入口 + UI渲染
│   ├── grid-data-model.el     # 数据模型核心
│   ├── grid-data-source.el    # 数据源抽象
│   ├── grid-table-api.el      # 公开API
│   ├── grid-table-calc.el     # 公式计算引擎
│   ├── grid-table-nav.el      # 导航控制逻辑
│   ├── grid-table-parser.el   # 文本解析工具
│   └── grid-table-persistence.el # 持久化支持
├── plugins/                   # 插件扩展系统
│   ├── grid-table-csv.el      # CSV格式支持
│   ├── grid-table-org.el      # Org模式集成
│   ├── grid-table-markdown.el # Markdown集成
│   ├── grid-table-rst.el      # reStructuredText导出
│   └── grid-table-example-plugin.el # 插件开发示例
├── docs/                      # 技术文档
│   ├── ELISP_FORMULA_GUIDE.md     # 公式开发指南
│   └── PLUGIN_DEVELOPMENT.md      # 插件开发手册
└── pictures/                  #  产品截图和演示
```

## 🤝 如何贡献力量

### ⚡ 插件开发快速入门

#### 3种插件类型（1分钟掌握）

| 🎯 类型 | 📝 功能说明 | 🎯 适用场景 |
|---------|-------------|-----------|
| 🎨 **单元格渲染器** | 自定义单元格显示外观 | 进度条、货币格式等 |
| 📊 **数据源插件** | 支持新的文件格式 | JSON、Excel读取 |
| ⚡ **功能插件** | 添加新功能命令 | 数据导出、图表生成 |

#### 快速开始模板（复制即可用）
```elisp
;;; my-plugin.el --- 快速模板 -*- lexical-binding: t -*-
(require 'grid-table-plugins)

;; 简单的货币格式化渲染器
(defun my-currency-renderer (value &optional cell-props)
  "货币格式化渲染器，将数字转为 ¥XX.XX 格式"
  (if (and value (stringp value))
      (let ((num (string-to-number value)))
        (format "¥%.2f" num))
    ""))

;; 注册渲染器（完成！）
(defun my-plugin-init ()
  (grid-table-register-cell-renderer 'currency #'my-currency-renderer)
  (grid-table-register-plugin 'my-plugin))

(my-plugin-init)
(provide 'my-plugin)
```

> 📚 **完整开发指南**→ [插件开发手册](docs/PLUGIN_DEVELOPMENT.md) (从入门到专家级)

### 🐛 快速反馈路径
- 📧 **Bug报告** → [GitHub Issues](https://github.com/your-repo/issues)  
- ✨ **功能建议** → [讨论论坛](https://github.com/your-repo/discussions)  
- 🔧 **插件贡献** → [插件开发手册](docs/PLUGIN_DEVELOPMENT.md)

### 🎯 社区贡献方向
| 🎯 类型 | 📋 需求描述 | 🏷️ 标签 |
|---------|-------------|----------|
| **数据源插件** | Excel/JSON/数据库数据源 | `enhancement`, `plugin` |
| **格式导出** | LaTeX/ASCII/HTML/JSON 导出 | `extending`, `format` |
| **主题插件** | 深色模式 | `UI/UX`, `theme` |
| **图表插件** | 数据可视化 | `visualization` |


---

<div align="center">

**用 ❤️ 为 Emacs 社区打造**  

*自 2025 年来支持高效生产力工作流*  

🤝 **[→ 加入开发者社区 ←](https://github.com/yibie/grid-table/discussions)**

</div>
