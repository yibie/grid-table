[English](README.md) | [中文](README_CN.md)

## grid-table

![](pictures/figure1.jpg)

grid-table 是一个用于 Emacs 的通用网格表格组件，支持富文本与图片渲染、公式计算、交互编辑、排序、持久化与插件扩展。可在 Org/Markdown 中以“静态预览 + 激活编辑”的方式嵌入。

### 特性

- 富文本与图片：单元格内支持简单的 Org 风格标记与图片内联（Unicode 边框预览）。
- 公式引擎：=B2*C2、=SUM(D2:D4)、=IF(A1>0, "Yes", "No") 等；支持单元格/区域引用。
- 交互编辑：导航、编辑、插入/删除行列、列宽自适应、列排序。
- 持久化：.grid 纯文本格式（含公式）；CSV 插件读写。
- 插件系统：CSV 数据源、Org/Markdown 静态预览，易扩展。

### 安装

1) 将项目与 plugins 目录加入 load-path
   (add-to-list 'load-path "/path/to/grid-table")
   (add-to-list 'load-path "/path/to/grid-table/plugins")
2) 加载核心与插件系统
   (require 'grid-table)
   (require 'grid-table-plugins)
   ;; 或按需加载：
   ;; (require 'grid-table-csv)
   ;; (require 'grid-table-org)
   ;; (require 'grid-table-markdown)
   ;; (require 'grid-table-rst) ; 加载 rST 导出功能

### 配置（可选）

你可以通过在 Emacs 配置中设置 `grid-table-default-save-directory` 变量，来自定义新建 `.grid` 文件的默认保存目录：

```elisp
;; 示例：将所有新表格保存到 ~/Documents/grids/ 目录下
(setq grid-table-default-save-directory "~/Documents/grids/")
```

### 快速开始

- 新建：M-x grid-table-create
- 打开 .grid：M-x grid-open 或 M-x grid-table-find-file
- 保存：在表格 buffer 中按 C-c C-w。如果是新文件，会提示输入路径。
- 打开 CSV：M-x grid-table-find-file-csv
- 导出为 rST：在 grid-table 缓冲区中，运行 `M-x grid-table-export-as-rst`。
- 作为 rST 插入：在文本文档（如 .rst 文件）中，运行 `M-x grid-table-rst-insert-table-from-file`，选择一个 `.grid` 文件，即可将其作为 rST 表格插入。

### 常用快捷键（grid-table-mode）

- 导航：n/p 上下，TAB/S-TAB 左右，g 刷新
- 编辑：e 编辑单元格，C-c t 编辑标题
- 行列：C-c r a (向下插入行), C-c r d (删除当前行)；C-c c a (向右插入列), C-c c d (删除当前列)
- 排序：C-c s 按当前列升/降序
- 文件：C-c C-w (保存/另存为), C-c C-f (打开 .grid 文件)

说明：`grid-table-insert-column-left` 命令支持在左侧插入列。删除第 0 行（用户自定义表头）的操作是被保护的。

### 公式

- 以 = 开头：=B2*C2、=SUM(D2:D4)、=IF(A1>0, "Yes", "No")
- 引用：单元格 A1，区域 A1:B5
- 内置：SUM/AVERAGE/COUNT/MAX/MIN/IF
- **Elisp 公式**：`=elisp:(+ 1 2)` 或 `=elisp:(+ (cell "A1") (cell "B2"))`
  - 在单元格内直接执行任意 Elisp 代码。
  - 使用辅助函数 `(cell "A1")` 来获取其他单元格的值。
  - **⚠️ 安全警告**：从文件中执行任意 Elisp 代码存在巨大安全风险，请仅在完全信任的文件中使用此功能。

### 排序

在任意数据列上执行 C-c s，选择 ascending 或 descending 即可（用户自定义表头保持首行）。

### Org 集成（special block）

- 插入块：M-x grid-table-org-insert-block（仅需 :file）
- 刷新预览：M-x grid-table-org-refresh-block
- 打开编辑：M-x grid-table-org-open-block
说明：静态预览为只读；实际修改请在专用表格窗口内进行。

### Markdown 集成（fenced block）

- 插入块：M-x grid-table-markdown-insert-block
- 刷新预览：M-x grid-table-markdown-refresh-block
- 打开编辑：M-x grid-table-markdown-open-block
说明：静态预览为只读；实际修改请在专用表格窗口内进行。

### 持久化

.grid 使用 Lisp S 表达式保存：标题、用户自定义表头以及所有原始值（含公式）。

### 数据源 API（概览）

数据源（哈希表）常用键：
- 读写：:get-row-count、:get-column-count、:get-raw-value-at、:get-computed-value-at、:set-raw-value-at
- 结构：:add-row、:delete-row、:add-column、:delete-column
- 其它：:get-header-value、:set-header-value-at、:sort-by-column
可参考 plugins/grid-table-csv.el 实现自定义数据源并注册。

### 路线图

- Provider 适配器（更稳定的 Provider 契约，兼容现接口）
- 依赖图与增量重算、更多公式函数（如 Lookup 系列）
- Org/Markdown 高级参数（如 :range、:width 等，可选）

### 许可与贡献

欢迎反馈与贡献插件/功能。若你在数据源或渲染方面有需求，欢迎提交 PR/Issue。


