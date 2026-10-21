[English](PLUGIN_DEVELOPMENT.md) | [中文](PLUGIN_DEVELOPMENT_CN.md)

# 编写插件

插件是一个 require 了 `exc` 并为它增加功能的 Emacs Lisp 文件：一种可以打开的
文件格式、一种导出、一个命令。没有需要迎合的插件加载器；这个文件和别的库一样用
`require` 加载。exc 自带的插件放在 `plugins/` 里，`exc.el` 会把这个
目录加入 `load-path`。

## 东西在哪里

| 文件 | 插件从中用到的 |
|---|---|
| `exc-data-source.el` | `make-default-data-source`，表格显示的数据源 |
| `exc.el` | `exc-get-row-count`、`exc-get-raw-value-at` 等访问函数 |
| `exc-plugins.el` | `exc-register-data-source-type`、`exc-register-plugin` |
| `exc-api.el` | `exc-create-data-source`、`exc-api-create` |
| `exc-persistence.el` | `exc-persistence-create-data-source-from-file`，读取 `.grid` 文件 |
| `exc-text.el` | `exc-text-render`，把表格画成纯文本表格 |

## 数据源

表格显示的是一个数据源：一个从关键字映射到函数的哈希表，外加这些函数操作的数据。
每个函数的第一个参数都是数据源本身。

用 `make-default-data-source` 创建：

```elisp
(make-default-data-source HEADERS ROWS &optional TITLE MERGES)
```

- `ROWS` 是行的列表，每行是字符串列表，内容就是输入时的样子：`"12"`、
  `"=SUM(B2:B3)"`、`"pen"`。短的行用空单元格补齐。
- `HEADERS` 是字符串列表，每列一个；它的长度就是列数。
- `ROWS` 的第一行就是表格显示的表头行，排序和筛选时它保持不动。`HEADERS` 不会被画出来。
  所以表头要两处都传：作为 `HEADERS`，也作为 `ROWS` 的第一个元素。exc 读取
  `.grid` 文件时就是这样做的。

不要手工拼这个哈希表。视图在很多地方直接读取工作表的模型（`:model`），而只有
`make-default-data-source` 会把它和工作簿的相关记录一起建好。

### 键

模型之外的代码通过 `exc.el` 里的访问函数调用这些键：

| 键 | 访问函数 |
|---|---|
| `:get-row-count`、`:get-column-count` | `exc-get-row-count`、`exc-get-column-count` |
| `:get-raw-value-at` | `exc-get-raw-value-at`：输入的内容，公式以文本形式给出 |
| `:get-computed-value-at` | `exc-get-computed-value-at`：公式的值 |
| `:set-raw-value-at` | `exc-set-raw-value-at` |
| `:get-style-at`、`:set-style-at` | `exc-get-style-at`、`exc-set-style-at` |
| `:get-merges`、`:merge-cells` | `exc-get-merges`、`exc-merge-cells` |
| `:get-column-widths`、`:set-column-width` | `exc-get-column-widths`、`exc-set-column-width` |
| `:get-conditional-formats`、`:set-conditional-formats` | `exc-get-conditional-formats`、`exc-set-conditional-formats` |

`make-default-data-source` 还提供视图用来处理行、列、排序、冻结、隐藏、筛选、校验和
图表的键（`:add-row`、`:delete-column`、`:sort-by-column`、`:set-frozen`、
`:set-filters`、`:set-charts` 等）；详见这个函数本身。行和列从 0 开始数。输入为数字的值，
`exc-get-computed-value-at` 返回的是文本（`"12"`）。

插件可以在同一个表里存放自己的键。CSV 插件加了两个，exc 会用到：

| 键 | 使用者 |
|---|---|
| `:file-path` | CSV 插件的 `exc-csv-save-overwrite` |
| `:save-to-file` | `exc-save` 和 `exc-save-as`，以及关联到非 `.grid` 文件的缓冲区里的 `C-x C-s`。调用方式都是 `(funcall FN SOURCE FILE)`。 |

## 注册

`exc-plugins.el` 里有两个注册函数是起作用的：

```elisp
(exc-register-data-source-type 'tsv #'exc-tsv-create-data-source)
(exc-create-data-source 'tsv "~/data.tsv")  ; calls the function with "~/data.tsv"
```

`exc-register-data-source-type` 把一个函数存在某个类型名下，
`exc-create-data-source` 用其余的参数调用它。未知类型会报
`Unknown data source type`。

`exc-register-plugin` 把一个符号加入 `exc-plugins`。没有代码读这个列表；
它只记录加载过什么。

`exc-load-plugin` 会 require `exc-NAME` 并注册 `NAME`：
`(exc-load-plugin "csv")` 加载 CSV 插件。

尚未使用：`exc-register-cell-renderer` 把函数存进 `exc-cell-renderers`，
但视图从不调用它，所以渲染函数不会改变屏幕上的任何东西。请改用数字格式和条件格式来
设置单元格的样子。同样，`exc-plugins-init` 及其注册的内置渲染函数从未被调用，
`exc-api-create` 的 `:columns` 参数也会被忽略。

## 显示数据源

`exc-api-create` 在一个名为 `*Exc*` 的新缓冲区里显示数据源：

```elisp
(exc-api-create :data-source SOURCE :title TITLE :file-path FILE)
```

在这个缓冲区里，`C-x C-s` 执行 `exc-save`，它用 `FILE` 调用数据源的
`:save-to-file`。没有 `:file-path` 时，它会像 `exc-save-as` 一样询问存到哪个
文件。它绑定的键见 `exc-default-bindings`。

这个缓冲区不关联文件。要让表格像 `C-x C-f` 打开的 `.grid` 文件那样关联自己的文件，
可以参考 `exc-find-file-csv`：它用 `find-file-noselect` 打开文件，再在那个
缓冲区里显示数据源，这样 `C-x C-s` 就会用关联的文件调用 `:save-to-file`。

## 完整示例：制表符分隔的文件

这个插件把制表符分隔的文件作为表格打开，并能存回去。它按下文所示加载和运行过。

```elisp
;;; exc-tsv.el --- Tab-separated files for exc -*- lexical-binding: t -*-

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

对一个用制表符分隔、内容为 `Item Qty Price`、`pen 3 1.5`、`ink 2 4` 的文件，
`M-x exc-tsv-find-file` 显示三行，`Item`、`Qty`、`Price` 是表头行。在 `A4`
输入 `total`、在 `B4` 输入 `=SUM(B2:B3)` 后显示 `5`，`C-x C-s` 把文件写回，公式以文本
保存：

```
Item	Qty	Price
pen	3	1.5
ink	2	4
total	=SUM(B2:B3)	
```

## 命令和按键

命令从 `exc--data-source` 读取当前缓冲区的表格：

```elisp
(defun my-grid-size ()
  "Say how many rows and columns the grid has."
  (interactive)
  (message "%d rows, %d columns"
           (exc-get-row-count exc--data-source)
           (exc-get-column-count exc--data-source)))

(define-key exc-mode-map (kbd "C-c x") #'my-grid-size)
```

表格显示在 `exc-etaf-mode` 里，它派生自 `exc-mode`，所以绑定在
`exc-mode-map` 里的键在所有表格里都有效，除非视图自己绑定了这个键。视图绑定了
方向键、`TAB`、`RET`、普通字符和许多 `C-c` 键（[KEYS.md](KEYS.md)）；选一个空闲的键，
并在表格里用 `C-h k` 确认。

从 Lisp 修改单元格之后，用 `exc-refresh` 重画表格。

## exc 自带的插件

| 文件 | 如何接入 |
|---|---|
| `exc-csv.el` | 注册 `csv` 类型和 `csv` 插件。`exc-find-file-csv` 关联文件并把它显示成表格，表头行是第一行；`C-x C-s` 通过 `:save-to-file` 按 CSV 写回。`exc-csv-save` 写到另一个文件。 |
| `exc-xlsx.el` | 注册 `xlsx` 类型和 `xlsx` 插件。注册的函数 `exc-xlsx-create-data-source` 通过 Rust 模块读取工作簿的一个工作表；`exc-xlsx-find-file` 把所有工作表作为一个工作簿打开。 |
| `exc-org.el`、`exc-markdown.el` | 不注册任何东西。它们用 `exc-persistence-create-data-source-from-file` 读取 `.grid` 文件，画成文本（Markdown 用 `exc-text-render`，Org 用自己的表格代码），再把文本插入或刷新到 Org 或 Markdown 缓冲区里。 |
| `exc-rst.el` | 不注册任何东西。`exc-export-as-rst` 和 `exc-rst-insert-table-from-file` 把表格写成 RST 表格。 |
| `exc-example-plugin.el` | 能加载，但大部分不能用：见下文。 |

### 关于示例插件

`exc-example-plugin.el` 比当前的视图要老。require 它之后什么也不会注册，
直到调用 `exc-example-plugin-init`。它的数据源
`exc-example-create-data-source` 会报 `wrong-type-argument`，所以
`exc-example-demo` 也会失败。它的三个渲染函数从未被使用（见上文），
`exc-example-highlight-duplicates` 调用了不存在的 `exc-highlight-cell`。
`exc-example-export-to-html` 能用。请以上面的示例或 `exc-csv.el` 为范本。
