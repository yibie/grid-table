[English](README.md) | [中文](README_CN.md)

# 🎯 grid-table - Emacs 高端表格系统

<div align="center">

![截图](pictures/figure1.jpg)

<p><i>为 Emacs 源生打造的表格系统！支持富文本、公式计算和插件扩展 🔥</i></p>

</div>

## ✨ 功能亮点

<div align="center">

| 外观体验 | 数据计算 | 可扩展性 |
|--------------|---------------|-------------|
| 富文本渲染<br/> 单元格图片<br/> 自适应列宽<br/> Unicode边框 |  公式引擎 (Excel风格)<br/>📍 单元格引用<br/> 实时计算<br/> Elisp表达式 |  插件架构<br/> 多数据源支持<br/> 格式导出<br/>🔧 自定义扩展 |

</div>

### 🚀 核心能力
- **Markdown风格标记** - 在单元格中写简历总结
- **图片可视化** - 直接在表格中显示图片!
- **Excel级公式** - `=B2*C2· =SUM(A1:A10)· =IF(...)`
- **Elisp超级公式** - 强大的 `=elisp:(cell"A1")` 表达式
- **实时预览** - 在Org/Markdown中"静态预览 + 动态编辑"

## 🚀 30秒上手

### 📦 1. 安装配置

#### 🎯 推荐用 `use-package`
```elisp
;; 🚄 一步到位配置 - 推荐！
(use-package grid-table
  :load-path ("/path/to/grid-table" "/path/to/grid-table/plugins")
  :config
  (require 'grid-table)
  (require 'grid-table-plugins)
  
  ;; 📁 自定义保存目录
  (setq grid-table-default-save-directory "~/Documents/表格/")
  
  ;; 🖼️ 图片显示调优
  (setq grid-table-image-target-char-height 8)
  (setq grid-table-image-max-width-ratio 0.9))
```

#### 📋 传统方式
```elisp
;; 📍 手动配置路径
(add-to-list 'load-path "/path/to/grid-table")
(add-to-list 'load-path "/path/to/grid-table/plugins")

;; 🎯 按需加载
(require 'grid-table)          ;; 核心功能
(require 'grid-table-plugins)  ;; 插件系统
(require 'grid-table-csv)      ;; CSV支持
(require 'grid-table-org)      ;; Org集成
(require 'grid-table-markdown) ;; Markdown集成
```

## 📋 操作速查表

### 文件操作一览表
| 🎯 动作 | 📝 命令 | 🔑 快捷键 | ⚡ 提示 |
|--------|--------|-----------|---------|
| 新建 | `M-x grid-table-create` | - | 创建空白表格 |  
| 打开 | `M-x grid-table-find-file` | `C-c C-f` | 打开.grid文件 |
| 保存 | - | `C-c C-w` | 智能提示路径 |
| CSV | `M-x grid-table-find-file-csv` | - | 直接读取CSV |

###  编辑操作速记
| 🎯 动作 | 🤞 快捷键 | 📝 效果 | ⚠️ 备注 |
|--------|-----------|---------|---------|
| **✏️ 基础编辑** | |||
| 单元格编辑 | `e` 或右键 | 进入编辑模式 | 单元格聚焦 |
| 标题编辑 | `C-c t` | 修改表格标题 | 全局显示 |
| **➕ 行操作** | |||
| 下方插入行 | `C-c r a` | 在当前行下新建 | 智能继承格式 |
| 删除当前行 | `C-c r d` | 快速删除整行 | ⚠️ **表头无法删除** |
| **➕ 列操作** | |||
| 右侧插入列 | `C-c c a` | 新建空白列 | 自动调宽 |
| 删除当前列 | `C-c c d` | 整列删除 | 数据清空 |

### 导航控制图
| 🎯 方向 | ⌨️ 按键 | 🖱️ 操作 |
|---------|---------|----------|
| ⬆️ 向上 | `p` 或 `↑` | 上一行 |
| ⬇️ 向下 | `n` 或 `↓` | 下一行 |
| ⬅️ 向左 | `S-TAB` 或 `←` | 左单元格 |
| ➡️ 向右 | `TAB` 或 `→` | 右单元格 |
| 🔄 刷新 | `g` | 重新渲染 |

**🎯 小贴士**: 使用 `n`/`p` 垂直移动, `TAB` 快速水平移动

## 🧮 公式完全指南

### 基础语法对照表
| 🎯 类型 | 📝 语法 | 📋 示例 | 💡 说明 |
|--------|---------|---------|---------|
| 📍 单元格 | `=A1`、`=B2` | `=B2*C2` | 相对引用 |
| 📊 范围 | `=RANGE(A1:B5)` | `=SUM(A1:A10)` | 连续区块 |
| ✨ 函数 | `=FUNCTION(args)` | `=AVERAGE(B2:B10)` | 内置函数集合 |

###  官方内置函数大全
| 📈 数学函数 | 📊 统计函数 | 🎯 条件函数 | 🎨 文本函数 |
|-------------|-------------|-------------|-------------|
| `SUM` 求和 | `AVERAGE` 平均 | `IF` 条件判断 | `CONCAT` 连接 |
| `PRODUCT` 乘积 | `COUNT` 计数 | `AND/OR` 逻辑 | `LEFT/RIGHT` 切片 |
| `MOD` 取模 | `MAX/MIN` 极值 | `NOT` 反转 | `LEN` 长度 |

### 🎯 完整公式函数库 (基于 grid-table-calc.el)

引擎支持 **30+ Excel 兼容函数**，按分类整理如下：

#### 📊 **数学函数集**
| 函数 | 示例 | 功能说明 |
|------|------|----------|
| `SUM` | `=SUM(A1:A10)` | 求和范围内所有数值 |
| `AVERAGE` | `=AVERAGE(B2:B10)` | 计算平均值 |
| `ROUND` | `=ROUND(3.1415, 2)` | 保留2位小数 |
| `MOD` | `=MOD(A1, B2)` | 取模运算 |
| `POWER` | `=POWER(2, 3)` | 幂运算 |
| `SQRT` | `=SQRT(16)` | 平方根 |
| `ABS` | `=ABS(-5)` | 绝对值 |

#### 🔍 **高级查询函数**
| 函数 | 示例 | 功能说明 |
|------|------|----------|
| `VLOOKUP` | `=VLOOKUP("苹果", A2:C6, 2)` | 精确匹配垂直查找 |
| `INDEX` | `=INDEX(A1:C3, 2, 3)` | 返回第2行第3列的值 |
| `MATCH` | `=MATCH(100, A1:A10)` | 查找值的位置(1开始) |

#### 🎯 **条件统计函数 (智能必备)**
| 函数 | 示例 | 功能说明 |
|------|------|----------|
| `COUNTIF` | `=COUNTIF(A1:A10, ">50")` | 条件计数 |
| `SUMIF` | `=SUMIF(A1:A10, ">=60", B1:B10)` | 条件求和 |
| `COUNTA` | `=COUNTA(A1:A10)` | 非空单元格计数 |
| `ISBLANK` | `=ISBLANK(A1)` | 检查空值 |

#### 📅 **日期函数系统**
| 函数 | 示例 | 功能说明 |
|------|------|----------|
| `TODAY()` | `=TODAY()` | 当前日期 |
| `NOW()` | `=NOW()` | 当前日期+时间 |
| `YEAR` | `=YEAR(A1)` | 提取年份 |
| `MONTH` | `=MONTH(A1)` | 提取月份(1-12) |
| `WEEKDAY` | `=WEEKDAY(TODAY())` | 星期几(默认从周日=1开始) |
| `EOMONTH` | `=EOMONTH(A1, 3)` | 3个月后的月末 |

#### 📝 **文本处理函数制作**
| 函数 | 示例 | 功能说明 |
|------|------|----------|
| `LEN` | `=LEN(A1)` | 字符串长度 |
| `LEFT` | `=LEFT("Hello", 3)` | 提取左3位: "Hel" |
| `MID` | `=MID("World", 2, 2)` | 提取第2位开始2个字符: "or" |
| `FIND` | `=FIND("世界", A1)` | 查找子串位置 |
| `SUBSTITUTE` | `=SUBSTITUTE(A1, "旧", "新")` | 字符串替换 |

#### 🔄 **条件判断实用场景**
```excel
=IF(A1>=90, "优秀", IF(A1>=80, "良好", IF(A1>=60, "及格", "不及格")))
=IFS(A1>100, "超高", A1>=80, "高", A1>=60, "中", TRUE, "低")
=IFERROR(复杂公式, "计算错误")
```

#### ❌ **错误处理系统**
- **除零保护**: 避免除以0的问题
- **类型转换**: 文本"123"自动转数值
- **空值处理**: 空字符串在数学中视为0

#### 💡 **绝对/相对引用** (高级用法)
| 符号 | 含义 | 示例 |
|------|------|------|
|  `$A$1`  | 绝对行列 | 固定引用 |
|  `A$1`   | 绝对行相对列 | 行固定 |
|  `$A1`   | 相对行绝对列 | 列固定 |

###  Elisp公式高级技巧 (专家级别)
```elisp
=elisp:(+ 1 2 3)                                ; 📍 基础计算
=elisp:(+ (cell "A1") (cell "B2"))              ; 📍 单元格引用
=elisp:(let ((total (+ (cell "A1") A2 A3)))      ; 📍 三数求和
          (if (> total 100) "超预算" total))

=elisp:(format "%.1f%%" (* 100 (cell "占比")))   ; 📍 百分比格式
=elisp:(concat "本季度总计："                     ; 📍 字符串拼接
          (number-to-string (cell "C2")))
=elisp:(+ (reduce #'+ A1:A10) (reduce #'+ B1:B5)) ; 📍 多区域求和
```

📚 **完整公式指南 →** [ELISP_FORMULA_GUIDE.md](docs/ELISP_FORMULA_GUIDE.md) 从语法到高级应用

> ⚠️ **安全提示**:  Elisp代码应用不当有可能破坏文件，损害你的数据！**仅在完全信任的文档中使用**  
> 🔒 **建议**: 通过 `M-x checkdoc` 验证外部文件安全可靠

## 🔗 格式集成教学指南

### Org模式深度集成
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

### 快速反馈路径
- **Bug报告** → [GitHub Issues](https://github.com/your-repo/issues)  
- **功能建议** → [讨论论坛](https://github.com/your-repo/discussions)  
- **插件贡献** → [插件开发手册](docs/PLUGIN_DEVELOPMENT.md)

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
