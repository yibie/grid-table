[English](ELISP_FORMULA_GUIDE.md) | [中文](ELISP_FORMULA_GUIDE_CN.md)

# Lisp formulas

A cell that starts with `=elisp:` holds an Emacs Lisp expression instead
of a spreadsheet formula. The expression is read and evaluated, and its
value is what the cell holds.

```
=elisp:(+ 1 2)
```

The prefix comes right after `=`, in any case: `=ELISP:` works too.

For symbolic math, `CALC` is the safer route: `=CALC("deriv(x^2, x)")`
hands its text to Emacs Calc and needs no leave to run Lisp. See
[the README](../README.md#symbolic-math-with-emacs-calc).

## Turning it on

Lisp formulas are off by default. While they are off, such a cell shows
`#ERROR!`, and so does every cell that reads it.

They are controlled by `exc-allow-elisp`, which is buffer-local
and `nil` by default.

| To allow them | Do this |
|---|---|
| In every grid | `(setq-default exc-allow-elisp t)` in your init file. Lisp formulas are computed as soon as a grid opens. |
| In one grid already open | `M-: (setq exc-allow-elisp t)` in its buffer, then `C-c C-r` to compute everything again. |

Setting the variable from `exc-etaf-mode-hook` does not work: when
a grid is shown, the view puts back the value the buffer had before, so
the hook's value is lost. The variable is marked as risky, so Emacs
asks before applying it from a file's local variables.

The Org, Markdown and reStructuredText previews compute a grid outside
its buffer, so there a Lisp formula shows `#ERROR!` unless the default
value is `t`.

## The risk

A Lisp formula can run any code with your user's rights: delete files,
read your mail, start programs. Opening a `.grid` file with Lisp
formulas computes them at once when they are allowed. Allow them only
for files you wrote or trust, and prefer allowing them in one buffer
over `setq-default`.

## What the expression sees

The expression is evaluated with `eval`, with lexical binding. Every
function and variable of your Emacs is available.

`(cell "B2")` reads a cell of the formula's own sheet:

| The cell holds | `cell` returns |
|---|---|
| A number, typed or computed | The number (`12`, not `"12"`) |
| Text | The string |
| A formula | Its computed value |
| Nothing | `nil` |

`cell` takes one cell written as text, such as `"B2"` or `"$B$2"`. It
does not read ranges or other sheets: `(cell "Data!A1")` is `nil`.

## Values and errors

| The expression returns | The cell shows |
|---|---|
| A number | The number, in the general format (`(sqrt 2)` shows `1.4142135623731`) |
| A string | The string |
| `t` | `TRUE` |
| `nil` | `FALSE` |
| Anything else, such as a list | Not a value a cell can show; return a number or a string instead |

When the expression signals an error, the cell holds the text
`Elisp Error: ` followed by the error message. That value is a string,
not an error value, so `IFERROR` does not catch it, and a formula that
does arithmetic on it shows `#VALUE!`.

Other formulas can read a Lisp formula's cell like any other: if `C2`
holds `=elisp:(* 2 21)`, then `=C2+1` is `43`.

## When it is computed

A Lisp formula is computed again at every edit, wherever the edit is,
since exc cannot tell which cells the code reads. `C-c C-r`
computes it too. Keep the code fast: slow code slows every keystroke
that stores a cell.

Copying, filling or moving a Lisp formula leaves its text as it is; the
row and column numbers inside it are not adjusted.

## Examples

Each of these was evaluated in a grid where `B2` holds `12` and `A2`
holds `pen`.

| Formula | Shows |
|---|---|
| `=elisp:(+ 1 2)` | `3` |
| `=elisp:(upcase "hi")` | `HI` |
| `=elisp:(string-join (list "a" "b") ", ")` | `a, b` |
| `=elisp:(format-time-string "%Y-%m-%d")` | today's date, such as `2026-10-20` |
| `=elisp:(> 3 2)` | `TRUE` |
| `=elisp:(* 2 (cell "B2"))` | `24` |
| `=elisp:(length (cell "A2"))` | `3` |
| `=elisp:(/ 1 0)` | `Elisp Error: Arithmetic error` |

Most of what these do has a spreadsheet function: `=UPPER("hi")`,
`=TEXTJOIN(", ",,"a","b")`, `=TODAY()`, `=B2*2`, `=LEN(A2)`. Those work
without turning anything on, follow references when rows move, and are
computed only when what they read changes. Use a Lisp formula for what
no function does.
