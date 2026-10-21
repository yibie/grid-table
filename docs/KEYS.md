[English](KEYS.md) | [中文](KEYS_CN.md)

# grid-table keys

Every key of a grid buffer. The list follows `grid-table-etaf--keys` in
`grid-table-etaf.el`. On macOS the Command-key shortcuts (`s-c`, `s-v`, `s-z` …) work too.

## Moving and selecting

| Key | Action |
|---|---|
| Arrows, `C-n` `C-p` `C-f` `C-b` | Move one cell |
| `TAB` / `S-TAB` | Next / previous cell |
| `C-a` / `C-e` | First / last column |
| `M-<` / `M->` | First / last cell |
| `<next>` / `<prior>` | Page down / up |
| `S-<arrow>` | Extend the selection |
| `C-x h` | Select everything |
| `C-c g` | Go to a cell, range, column, row or name (`D120`, `A1:B3`, `B:B`, `Sheet2!A1`) |

The cursor and the selection go past the last row and column; writing
there makes the sheet grow to hold it.

## Editing

| Key | Action |
|---|---|
| Any character | Replace the cell and start editing (set `grid-table-etaf-type-to-edit` to nil to turn off) |
| `F2`, `RET` | Edit the cell's text |
| `RET` (editing) | Store and move down |
| `TAB` (editing) | Store and move right |
| `C-c C-c` (editing) | Store in place |
| `M-RET` (editing) | New line inside the cell |
| `C-c C-k` (editing) | Cancel |
| `DEL`, `<deletechar>` | Clear the selected cells |
| `M-w` / `C-w` / `C-y` | Copy / cut / paste, with formats |
| `C-c y v` | Paste only the values shown |
| `C-c y f` | Paste only the formats |
| `C-c y t` | Paste rows as columns |
| `C-d` / `C-r` | Fill the selection down / right |
| `C-c C-s` | Continue a series (1, 2 → 3, 4; dates; `Week 1` → `Week 2`) |
| `C-s` (`C-u C-s` backward) | Find |
| `M-%` | Replace in the selection or the sheet |
| `C-/`, `C-_`, `C-x u` | Undo |
| `C-?`, `C-M-_` | Redo |
| `C-c t` | Edit the title |

## Rows and columns

| Key | Action |
|---|---|
| `C-c r i` / `C-c r a` / `C-c r d` | Insert row above / add row below / delete row |
| `C-c c a` / `C-c c d` | Add column / delete column |
| `C-c r h` / `C-c r s` | Hide rows / show hidden rows in the selection |
| `C-c c h` / `C-c c s` | Hide columns / show hidden columns in the selection |
| `C-c s` (`C-u` for more columns) | Sort |
| `C-c m` / `C-c u` | Merge the selection / unmerge |

## Filters

| Key | Action |
|---|---|
| `C-c / f` | Filter the cursor's column by a condition |
| `C-c / v` | Keep the rows holding the cursor cell's value |
| `C-c / c` / `C-c / a` | Clear this column's filter / all filters |
| `C-c / d` | Remove duplicate rows |

## Formats

| Key | Action |
|---|---|
| `C-c f b` / `C-c f i` | Bold / italic |
| `C-c f c` / `C-c f g` | Text color / fill color |
| `C-c f l` / `C-c f e` / `C-c f r` | Align left / center / right |
| `C-c f ,` | Number with thousands separators |
| `C-c f p` / `C-c f $` / `C-c f d` | Percent / currency / date |
| `C-c f t` / `C-c f G` | Text / general |
| `C-c f +` / `C-c f -` | More / fewer decimals |
| `C-c f n` / `C-c f N` | Add / clear conditional formats |
| `C-c f v` / `C-c f V` | Allow only listed values / clear the rule |
| `C-c f x` | Clear formatting |

## Names, notes, links and tracing

| Key | Action |
|---|---|
| `C-c n d` | Name the selection (`C-u`: name a formula, such as `=LAMBDA(x,x*2)`) |
| `C-c n l` / `C-c n k` | Go to a name / delete a name |
| `C-c ;` / `C-c :` | Add or edit a note / delete the selection's notes |
| `C-c C-l` / `C-c C-o` | Add or edit a link / follow it |
| `C-c a p` / `C-c a d` | Mark the cells a formula reads / the formulas reading a cell |
| `C-c a n` / `C-c a x` | Visit the marked cells / clear the marks |

## Sheets and panes

| Key | Action |
|---|---|
| `C-<next>` / `C-<prior>` | Next / previous sheet |
| `C-c w n` / `C-c w r` / `C-c w d` | New / rename / delete sheet |
| `C-c w s` | Go to a sheet by name |
| `C-c w <` / `C-c w >` | Move the sheet left / right |
| `C-c w f` / `C-c w u` | Freeze rows above and columns left of the cursor / unfreeze |
| `C-c w h` | Header row on or off |

## Charts

| Key | Action |
|---|---|
| `C-c i` | Insert a floating chart |
| `C-c k t` / `C-c k h` | Chart type / title |
| `C-c k s` | Switch rows and columns |
| `C-c k r` | Draw the selected cells instead |
| `C-c k m` / `C-c k z` | Move to the cursor / resize |
| `C-c k n` / `C-c k x` / `C-c k d` | Select next / deselect / delete |
| `C-c v` | Open a chart formula in its own buffer |

## Files

| Key | Action |
|---|---|
| `C-x C-s` | Save |
| `C-x 4 c` | Show the sheet in a second buffer and window; edits in either are drawn in both |
| `C-c C-w` | Save as |
| `C-c C-f` | Open a grid |
| `C-c C-r` | Recalculate everything and redraw |
| `C-c C-q` | Quit the window |

## Mouse

| Action | Effect |
|---|---|
| Click a cell | Go there |
| Shift-click, drag | Select a range |
| Click a row number, column letter, corner | Select the row, column, whole sheet |
| Drag a column letter's right edge | Change the column's width |
| Click a sheet tab | Show that sheet |
| Click / drag a chart | Select / move it |
| Drag to the window's edge | Scroll the sheet |
| Wheel | Scroll |
| `Esc` during a drag | Stop it; a chart or a column width goes back |
