[English](README.md) | [中文](README_CN.md)

# grid-table

Emacs 里的电子表格：在单元格里直接编辑，Excel 风格的公式，多个工作表，
格式、筛选、浮动图表，还能打开 xlsx 文件并写回。

<div align="center">

<img src="pictures/hero.png" alt="带公式栏、冻结窗格、工作表标签和浮动图表的表格" width="800">

</div>

## 这是什么

grid-table 把表格存在 `.grid` 文件里，用 [ETAF](https://github.com/emacs-taf)
画成电子表格。操作方式照着 Excel 用户的习惯，同时在合适的地方保留 Emacs
的习惯：

- **编辑**：直接输入就替换单元格，`F2` 或 `RET` 编辑原有内容，`RET` 存入并下移，
  `TAB` 存入并右移。`C-x C-s` 保存。
- **公式**：约 470 个 Excel 函数，支持单元格和区域引用、跨工作表引用、名称、
  会溢出的动态数组，以及 Excel 的错误值。计算结果用一批真实的 Excel 文件做对照检查。
  另有 `CALC` 函数，借 Emacs Calc 做符号运算。
- **工作簿**：多个工作表和标签、冻结窗格、隐藏行列、筛选、排序、合并单元格、
  批注、链接和下拉列表校验。
- **格式**：粗体、斜体、颜色、对齐、数字格式（千分位、百分比、货币、日期、文本）
  和条件格式。
- **图表**：浮动图表（柱形、条形、折线、面积、饼图、圆环、散点），作为一层盖在
  单元格上，可以按像素拖动；另有画在单元格里的图表公式。
- **其他格式**：xlsx 双向读写、CSV，以及在 Org、Markdown 和 reStructuredText
  里预览表格。

只画窗口里放得下的行和列，所以几千行的表也能直接打开、流畅滚动。

## 安装

grid-table 需要 Emacs 29.1 或更新版本，以及 [emacs-taf](https://github.com/emacs-taf)
的三个库：`ecss`、`ebox` 和 `etaf`。它们都还没有进软件包仓库，所以四个都要克隆：

```sh
git clone https://github.com/yibie/grid-table.git
git clone https://github.com/emacs-taf/ecss.git
git clone https://github.com/emacs-taf/ebox.git
git clone https://github.com/emacs-taf/etaf.git
```

然后加到 `load-path`：

```elisp
(dolist (dir '("~/src/ecss" "~/src/ebox" "~/src/etaf"
               "~/src/grid-table" "~/src/grid-table/plugins"))
  (add-to-list 'load-path (expand-file-name dir)))

(require 'grid-table)
(require 'grid-table-xlsx)   ; xlsx 工作簿（可选）
(require 'grid-table-csv)    ; CSV 文件（可选）
(require 'grid-table-org)    ; 在 Org 里预览（可选）
```

或者用 `use-package`：

```elisp
(use-package grid-table
  :load-path ("~/src/grid-table" "~/src/grid-table/plugins")
  :init
  (dolist (dir '("~/src/ecss" "~/src/ebox" "~/src/etaf"))
    (add-to-list 'load-path (expand-file-name dir)))
  :config
  (require 'grid-table-xlsx))
```

缺少 `ecss`、`ebox` 或 `etaf` 时，打开表格会提示缺的是哪一个。
拉取它们的新版本后要重新编译（`make -C ~/src/ebox compile`，另外两个同理），
以免加载到过期的 `.elc` 而不是新的源码。

### xlsx 模块

xlsx 文件由一个用 Rust 写的动态模块读写（`plugins/xlsx/`），所以 Emacs 必须
编译时带有模块支持。`plugins/grid-table-xlsx-module.dylib` 是为 macOS 预编译的
模块（Apple 芯片和 Intel 都能用）。其他系统上用 `M-x grid-table-xlsx-build-module`
或 `make module` 编译，需要 [cargo](https://rustup.rs)。自己编译的模块会优先于
预编译的那个，也可以用 `grid-table-xlsx-module-file` 指定放在别处的模块。

## 快速上手

1. `C-x C-f data.grid`。文件还不存在时会打开一张空表。
   `M-x grid-table-xlsx-find-file` 打开 xlsx 工作簿，`M-x grid-table-find-file-csv`
   打开 CSV 文件，它的第一行就是表格的第一行，`C-x C-s` 按 CSV 存回去。
2. 用方向键或 `C-n`/`C-p`/`C-f`/`C-b` 移动。直接输入就替换光标所在的单元格，
   按 `RET` 存入并下移。
3. 输入公式：`=SUM(B2:B10)`、`=B2*$C$1`、`=XLOOKUP(A2,Items!A:A,Items!C:C)`。
   表格上方的公式栏显示单元格里存的内容。
4. 用 `S-<方向键>` 选中，再按 `C-c f $` 设为货币格式，或 `C-c f b` 加粗。
   选中一块数据按 `C-c i` 插入图表。
5. `C-x C-s` 保存。以上所有操作，包括格式和图表，都能用 `C-/` 撤销、`C-?` 重做。

## 常用按键

下面是日常最常用的键，全部按键见 [docs/KEYS_CN.md](docs/KEYS_CN.md)。

| 操作 | 按键 |
|---|---|
| 编辑单元格 / 存入并下移 / 存入并右移 | `F2` 或 `RET` / `RET` / `TAB` |
| 编辑时在格内换行 / 取消编辑 | `M-RET` / `C-c C-k` |
| 选中区域 / 全选 | `S-<方向键>` / `C-x h` |
| 跳到单元格、区域或名称（`D120`、`A1:B3`、`Sheet2!A1`） | `C-c g` |
| 复制 / 剪切 / 粘贴 | `M-w` / `C-w` / `C-y` |
| 只粘贴值 / 只粘贴格式 / 转置粘贴 | `C-c y v` / `C-c y f` / `C-c y t` |
| 向下填充 / 向右填充 / 续写序列 | `C-d` / `C-r` / `C-c C-s` |
| 清空选区 | `DEL` |
| 撤销 / 重做 | `C-/` / `C-?` |
| 查找 / 替换 | `C-s` / `M-%` |
| 在上方插入行 / 在下方加行 / 删除行 | `C-c r i` / `C-c r a` / `C-c r d` |
| 加列 / 删除列 | `C-c c a` / `C-c c d` |
| 粗体 / 斜体 / 文字颜色 / 填充颜色 | `C-c f b` / `C-c f i` / `C-c f c` / `C-c f g` |
| 数字 / 百分比 / 货币 / 日期格式 | `C-c f ,` / `C-c f p` / `C-c f $` / `C-c f d` |
| 合并 / 取消合并 | `C-c m` / `C-c u` |
| 排序 / 筛选本列 / 清除所有筛选 | `C-c s` / `C-c / f` / `C-c / a` |
| 在光标处冻结窗格 / 取消冻结 | `C-c w f` / `C-c w u` |
| 下一个 / 上一个工作表 / 新建工作表 | `C-<next>` / `C-<prior>` / `C-c w n` |
| 插入图表 / 把图表移到光标处 | `C-c i` / `C-c k m` |
| 保存 / 另存为 | `C-x C-s` / `C-c C-w` |
| 全部重算 / 退出 | `C-c C-r` / `C-c C-q` |

鼠标：点单元格跳过去，按住 Shift 点击或拖动来选区域，点行号、列标或左上角
选中整行、整列或整张表，拖动列标的右边缘调整列宽。拖选区或拖图表到窗口边缘时，
表格会自动滚动。

### 尽量用键盘代替鼠标

在 macOS 上，系统有时收不到鼠标松开的事件，拖动会在松手后继续：选区一直
跟着指针变，或者图表一直跟着移动，直到下一次点击才停。grid-table 在拖动时
会隐藏提示框，这让问题很少出现，但仍然可能发生
（[ebox#1](https://github.com/emacs-taf/ebox/issues/1)）。遇到时点一下鼠标或按
`Esc` 就能停下；图表被误挪了，可以用 `C-/` 撤销。

| 想拖动来… | 改用 |
|---|---|
| 选一片区域 | `S-<方向键>`，整张表用 `C-x h` |
| 移动图表 | 光标移到目标单元格，再按 `C-c k m` |
| 调整图表大小 | `C-c k z` |
| 选中图表 | `C-c k n` |

列宽目前只能拖动列边来调整。

## 公式

<div align="center">

<img src="pictures/formulas.png" alt="公式栏里的公式和它溢出的结果" width="700">

</div>

以 `=` 开头的单元格是公式。函数名和引用不分大小写。

| 类型 | 写法 | 例子 |
|---|---|---|
| 单元格、区域 | `A1`、`A1:B5` | `=B2*C2`、`=SUM(A1:A10)` |
| 固定行或列 | `$A$1`、`A$1` | `=B2*$C$1` |
| 整列或整行 | `A:A`、`2:5` | `=SUM(B:B)` |
| 其他工作表 | `Sheet2!A1`、`'My Sheet'!A1` | `=SUM(Sheet2!A1:B5)` |
| 溢出区域 | `C1#` | `=SUM(C1#)` |
| 名称 | 用 `C-c n d` 定义 | `=SUM(Sales)`、`=Twice(A2)` |
| 运算符 | `+ - * / ^ & %` 和 `= <> < > <= >=` | `=A1&" kg"`、`=B2*15% |

需要知道的：

- **输入的值**按本意读取：`2026-10-13` 是日期，`12.5%` 是 0.125，`$1,234.50`
  是 1234.5，并按输入的样子显示。用斜杠、年份在最后的日期按月在前读
  （`3/4/2026` 是 3 月 4 日）。以 `'` 开头（`'007`）保留为文本。没有设格式的数字
  按 Excel 的"常规"格式显示，最多 11 个字符，但所有位数都保留。
- **动态数组**：`UNIQUE`、`SORT`、`FILTER`、`SEQUENCE` 等会把结果溢出到下方和
  右侧的单元格；那些单元格不空时显示 `#SPILL!`。对区域做比较和运算是逐格进行的，
  所以 `=SUM((A2:A9="x")*B2:B9)` 能用。
- **错误值**：`#DIV/0!`、`#NAME?`、`#N/A`、`#REF!`、`#VALUE!`、`#NUM!`、`#CALC!`、
  `#SPILL!` 和 `#CYCLE!`（循环引用）会显示在出错的单元格和引用它的单元格里。
  用 `IFERROR` 可以替换掉。
- **重算**：编辑后按依赖顺序重算这个单元格和依赖它的公式。读其他工作表，或调用
  `TODAY`、`NOW`、`RAND` 的公式，每次编辑都会重算。`C-c C-r` 全部重算。
- **追踪**：`C-c a p` 标出光标处公式读取的单元格，`C-c a d` 标出读取这个单元格的
  公式，`C-c a n` 依次跳过去，`C-c a x` 清除标记。
- **和 Excel 不同的地方**：工作表的大小就是它实际的行列数，所以 20 行的表里
  `A:A` 就是这 20 格，`=ROWS(A:A)` 等于 20。排序时公式跟着所在的行移动，
  指向本表自身的引用也一样。

### 用 Emacs Calc 做符号运算

`CALC` 把参数交给 Emacs Calc 计算，显示 Calc 返回的结果，所以单元格里可以求导、
积分、因式分解、求根：

| 公式 | 值 |
|---|---|
| `=CALC("deriv(x^2 + 3 x, x)")` | `2 x + 3` |
| `=CALC("integ(sin(x), x)")` | `-cos(x)` |
| `=CALC("factor(x^2 - 5 x + 6)")` | `(x - 2) (x - 3)` |
| `=CALC("roots(x^2 - 5 x + 6, x)")` | `[3., 2.]` |
| `=CALC("nrat((x^2 - 1) / (x - 1))")` | `x + 1` |
| `=CALC("deriv(" & A2 & ", x)")` | A2 里那个式子的导数 |

参数用 Calc 的代数写法。不管你自己的 Calc 怎么设置，`CALC` 一律用弧度、15 位
精度计算。结果是数字就返回数字（`=CALC("2^10")*2` 等于 2048）；其他结果，以及
大到双精度放不下的数字，返回文本。Calc 读不懂的参数得到 `#VALUE!`。和
`=elisp:` 不同，`CALC` 不需要打开任何开关：Calc 的写法只能调用 Calc 自己的数学
函数。这是 grid-table 自己的函数，Excel 里会显示 `#NAME?`。

Calc 在另一个 Emacs 进程里计算：第一次用到 `CALC` 时启动，之后一直复用，所以很
耗时的计算也不会卡住你的 Emacs。结果回来之前，单元格显示 `#GETTING_DATA`；结果
回来后，这一格和读取它的公式会重新计算。超过 `grid-calc-calc-timeout`（10 秒）
还没算完的计算会被停掉，显示 `#CALC!`。算过的结果会保留，同一个式子不会算第二遍，
`random()` 的结果也不会变；按 `C-c C-r` 会全部重新计算。那个 Emacs 以 `-Q` 启动，
所以你自己用 `defmath` 定义的 Calc 函数在里面用不了。在 batch 模式下，或者把
`grid-calc-calc-async` 设为 nil 时，Calc 就在你的 Emacs 里计算，结果当场就有。

### Lisp 公式

`=elisp:(+ (cell "A1") (cell "B2"))` 执行 Emacs Lisp。它默认关闭，因为表格文件
可以借此执行任意代码：只对信得过的文件把 `grid-table-allow-elisp` 设为 `t`。
详见 [docs/ELISP_FORMULA_GUIDE_CN.md](docs/ELISP_FORMULA_GUIDE_CN.md)。

## 格式、筛选和工作表

<div align="center">

<img src="pictures/format-filter.png" alt="条件格式和已筛选的列" width="700">

</div>

- **格式**作用于选区，插入、删除行列或排序时跟着单元格走。`C-c f n` 添加条件格式
  （比如"大于 100 标红"、"包含"、"为空"、"是错误值"），随数值变化。
- **筛选**：`C-c / f` 按条件筛选光标所在列，`C-c / v` 只留下和光标单元格值相同的行，
  `C-c / d` 删除重复的行。筛选过的列在列标后显示 `▼`，移动、复制、清空和状态栏
  统计都会跳过被筛掉的行。
- **排序**：`C-c s` 按列显示的内容排序，数字在文本前，空格总在最后；
  `C-u C-c s` 可以再加列来区分相同的值。
- **表头行**：排序和筛选时第一行不动。工作表不是一张单纯的表时，用 `C-c w h` 关掉。
- **合并单元格**：`C-c m` 合并选区，可以是一行、一列或一个矩形。编辑时 `M-RET`
  在格内换行。

<div align="center">

<img src="pictures/merged-cells.png" alt="横向与纵向合并单元格" width="552">

</div>

- **工作表**：`C-c w n` 新建，`C-c w r` 重命名，`C-c w d` 删除，`C-c w <` 和
  `C-c w >` 移动位置；底部的标签可以点击。每个工作表有自己的光标、筛选和撤销记录。
- **冻结窗格**：`C-c w f` 冻结光标上方的行和左边的列。
- **隐藏行列**：`C-c r h` / `C-c c h` 隐藏选区的行或列，`C-c r s` / `C-c c s`
  显示选区里隐藏的行或列。
- **批注和链接**：`C-c ;` 给单元格加批注（右边缘显示红线），`C-c C-l` 加链接，
  可以是网址、文件或 `#Sheet2!A1`，`C-c C-o` 打开链接。
- **下拉列表校验**：`C-c f v` 让选中的单元格只能填列出的值（`Yes, No`，或
  `=$F$2:$F$9` 这样的区域）；编辑这些单元格时会给出候选。

和电子表格一样，光标和选区可以越过最后一行和最后一列，在那里写入内容时表格会
自动扩大。

## 图表

### 浮动图表

<div align="center">

<img src="pictures/chart-drag.png" alt="正在单元格上方拖动的浮动图表" width="700">

</div>

选中一块数据（或把光标放在里面），按 `C-c i` 选一种类型：柱形、堆积柱形、条形、
堆积条形、折线、面积、饼图、圆环或散点。图表浮在单元格上方，数据变了会重画。

- 第一行是文本时作为系列名，第一列是文本时作为分类名。`C-c k s` 切换行列。
- 点击图表选中它，拖动来移动。横向按像素、纵向按行移动，松手停在哪里就在哪里。
- 插入或删除行列时，图表跟着单元格走。锚定在冻结行列里的图表，在那个方向上不动。
- 图表存在 `.grid` 文件里，导出 xlsx 时写成工作簿自己的图表。
- 在终端这类不能显示图片的地方，图表显示为文字草图。

| 操作                             | 按键                              |
|----------------------------------|-----------------------------------|
| 插入图表                         | `C-c i`                           |
| 类型 / 切换行列 / 标题           | `C-c k t` / `C-c k s` / `C-c k h` |
| 改画选中的单元格                 | `C-c k r`                         |
| 移到光标处 / 调整大小            | `C-c k m` / `C-c k z`             |
| 选中下一个图表 / 取消选中 / 删除 | `C-c k n` / `C-c k x` / `C-c k d` |

### 图表公式

图表公式在单元格里用 Unicode 方块和盲文点画出一张小图；`C-c v` 在单独的 buffer
里打开它。`M-x grid-table-insert-chart` 插入一个图表公式。

| 函数 | 图表 |
|---|---|
| `BARPLOT(labels, values, [title], [width])` | 横向条形 |
| `COLUMNPLOT(labels, values, [title], [width], [height])` | 纵向柱形 |
| `LINEPLOT(x, y, [title], [width], [height])` | 折线 |
| `SCATTER(x, y, [title])` | 散点 |
| `HISTOGRAM(values, [bins], [title], [width])` | 直方图 |
| `BOXPLOT(series1, series2, …, [title], [width])` | 箱线图 |
| `DENSITY(values, [title])` | 密度曲线 |
| `COUNTPLOT(values, [title])` | 各值的计数 |
| `MULTILINE(x, series1, series2, …, [title])` | 多条折线 |
| `SPARKLINE(range)` 或 `SPARKLINE(v1, v2, …)` | 迷你趋势线 |

<table>
<tr>
<td><img alt="BARPLOT" src="pictures/chart-barplot.png" width="200"></td>
<td><img alt="COLUMNPLOT" src="pictures/chart-columnplot.png" width="200"></td>
<td><img alt="LINEPLOT" src="pictures/chart-lineplot.png" width="200"></td>
<td><img alt="SCATTER" src="pictures/chart-scatter.png" width="200"></td>
</tr>
<tr>
<td><img alt="HISTOGRAM" src="pictures/chart-histogram.png" width="200"></td>
<td><img alt="BOXPLOT" src="pictures/chart-boxplot.png" width="200"></td>
<td><img alt="DENSITY" src="pictures/chart-density.png" width="200"></td>
<td><img alt="MULTILINE" src="pictures/chart-multiline.png" width="200"></td>
</tr>
<tr>
<td><img alt="COUNTPLOT" src="pictures/chart-count.png" width="200"></td>
<td><img alt="SPARKLINE" src="pictures/chart-sparkline.png" width="200"></td>
</tr>
</table>

`examples/chart-gallery.grid` 里有每一种的示例。

## Excel 工作簿（xlsx）

<div align="center">

<img src="pictures/xlsx.png" alt="以表格打开的 xlsx 工作簿" width="700">

</div>

`(require 'grid-table-xlsx)` 增加两个命令：

| 命令 | 作用 |
|---|---|
| `M-x grid-table-xlsx-find-file` | 打开工作簿，每个工作表对应一个 sheet |
| `M-x grid-table-xlsx-export` | 把表格和它的所有工作表写进工作簿 |

从工作簿打开的表格不和它关联：`C-x C-s` 存成 `.grid` 文件，导出才写回工作簿。

双向保留：值、公式、数字格式、合并、粗体、斜体、颜色、对齐、条件格式、列宽、
冻结窗格、隐藏行列、下拉列表校验、工作簿级名称、批注、链接，以及图表（柱形、条形、
折线、面积、饼图、圆环和散点，包括图表在列内的起始偏移），以及每个工作表的
表格标题（存在工作簿的自定义属性里，Excel 不会动它）。

不保留或会变化的：

- 边框、字体、其他类型的条件格式和校验、只属于一个工作表的名称，导入时会丢掉。
  导出时，工作表里的单元格、合并、批注、条件格式、校验和隐藏行列会被表格里的替换，
  所以导入时丢掉的那些就没了；工作表的其他部分和其他工作表保持原样。
- grid-table 没有对应的数字格式，按"常规"显示，写回时原样保留。
- 用了 grid-table 没有的函数的公式会显示错误值，但公式原样保留，导出时原样写回。
- 其他类型的图表（雷达图、三维图、引用其他工作表数据的图表）不导入，写回时保持原样。
  图表公式导出为文本。
- 1900 年 3 月以前的日期会差一天，因为工作簿的日历里那年有 2 月 29 日。
- 写回打开它的那个工作簿时，表格的工作表替换工作簿的工作表；写进别的工作簿时，
  则作为新工作表加进去。

## Org、Markdown 和 reStructuredText

**Org**：`M-x grid-table-org-insert-block` 插入一个预览 `.grid` 文件的源码块。
光标在块里时，`M-x grid-table-org-refresh-block` 按文件重画预览，
`M-x grid-table-org-open-block` 打开表格。

```org
#+BEGIN_SRC grid-table :file /home/me/project/data.grid
| Region | Total |
|--------+-------|
| North  | 120   |
#+END_SRC
```

**Markdown**：`M-x grid-table-markdown-insert-block` 用同样的方式插入一个代码块，
光标所在的块用 `grid-table-markdown-refresh-block` 刷新、
`grid-table-markdown-open-block` 打开。

````markdown
```grid-view file=/home/me/project/data.grid
┌───┬────────┬───────┐
│   │ A      │ B     │
…
```
````

路径里有空格时，用双引号括起来。

**reStructuredText**：`M-x grid-table-export-as-rst` 把表格写成 RST 表，
`M-x grid-table-rst-insert-table-from-file` 从 `.grid` 文件插入一张。

这些预览是 `grid-table-text.el` 画的纯文本表格，不需要 ETAF。

## 开发

```
grid-table.el              入口、命令、文件读写
grid-table-etaf.el         电子表格界面（ETAF）
grid-data-model.el         单元格、行、工作表
grid-data-source.el        数据源接口
grid-table-calc*.el        公式解析和函数
grid-table-format.el       数字格式
grid-table-persistence.el  .grid 文件格式
grid-table-text.el         预览和导出用的纯文本表格
grid-table-chart*.el       图表公式和浮动图表
plugins/                   CSV、xlsx（含 Rust 模块）、Org、Markdown、RST
test/                      ERT 测试和公式语料库
docs/                      按键表、Lisp 公式、插件开发
examples/                  示例表格
```

运行测试：

```sh
make test-model                      # 数据模型、公式、纯文本表格
make test-xlsx                       # xlsx 导入导出
make test-etaf ETAF_SRC=~/src        # 界面；ETAF_SRC 是放 ecss、ebox、etaf 的目录
```

`test/corpus/` 把公式结果和一批真实工作簿里 Excel 存下的值做对照；
`make corpus-fetch` 下载这批文件。

插件可以增加数据源和文件格式，见 [docs/PLUGIN_DEVELOPMENT_CN.md](docs/PLUGIN_DEVELOPMENT_CN.md)
和 `plugins/grid-table-example-plugin.el`。

问题和建议请提到 [GitHub Issues](https://github.com/yibie/grid-table/issues)。
绘制和鼠标方面的问题可能属于 [ETAF](https://github.com/emacs-taf)。
