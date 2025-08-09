# Grid Table Elisp Formula Feature Guide

## ⚠️ Warning

**Before using this feature, please note that directly executing any Elisp code in a table is a huge security risk. If you open a .grid file containing malicious Elisp formulas, it may damage your system. Please only use this feature on files you fully trust.**

**I am not responsible for any issues, damages, or bug reports caused by using this feature, and will not handle related issues. Please use this feature at your own risk. And eat your dog food.**

## Introduction

Grid Table now supports executing Elisp code in cells, which provides infinite possibilities for table calculations. By using the `elisp:` prefix, you can run any Elisp expression in a cell.

## Basic Syntax

The syntax for using Elisp formulas in cells is:

```
=elisp:(your-elisp-code-here)
```

Note:
- Must start with `=` (indicates this is a formula)
- Followed by `elisp:` prefix
- Followed by any valid Elisp expression

## Basic Examples

### Mathematical Calculations

```elisp
=elisp:(+ 10 20 30)           ; Result: 60
=elisp:(* 7 8)                ; Result: 56
=elisp:(/ 100 3.0)            ; Result: 33.333...
=elisp:(expt 2 10)            ; Result: 1024
```

### String Processing

```elisp
=elisp:(format "Hello, %s!" "World")              ; Result: "Hello, World!"
=elisp:(upcase "hello world")                     ; Result: "HELLO WORLD"
=elisp:(length "Hello")                           ; Result: 5
=elisp:(substring "Hello World" 0 5)              ; Result: "Hello"
```

### Date and Time

```elisp
=elisp:(format-time-string "%Y-%m-%d")            ; Current date
=elisp:(format-time-string "%H:%M:%S")            ; Current time
=elisp:(current-time-string)                      ; Full current time string
```

### Conditional Logic

```elisp
=elisp:(if (> 10 5) "大于" "小于")                ; Result: "大于"
=elisp:(cond ((> 10 5) "大") ((< 10 5) "小") (t "等于"))
```

### List Operations

```elisp
=elisp:(length '(1 2 3 4 5))                     ; Result: 5
=elisp:(apply '+ '(1 2 3 4 5))                   ; Result: 15
=elisp:(mapconcat 'number-to-string '(1 2 3) ", ") ; Result: "1, 2, 3"
```

## Cell References

Elisp formulas can reference the values of other cells using the `cell` function:

```elisp
=elisp:(* (cell "A1") 2)                         ; Value of A1 cell multiplied by 2
=elisp:(+ (cell "A1") (cell "B1"))               ; Value of A1 and B1 cells added
=elisp:(if (> (cell "A1") 0) "正数" "负数或零")    ; Condition judgment based on the value of A1
```

### Example Table

| A | B | C | 
|---|---|---|
| 10 | 20 | =elisp:(+ (cell "A2") (cell "B2")) |
| 5 | 3 | =elisp:(* (cell "A3") (cell "B3")) |
| 100 | 7 | =elisp:(/ (cell "A4") (cell "B4")) |

In this example:
- C2 will display 30 (10 + 20)
- C3 will display 15 (5 * 3)  
- C4 will display 14.285... (100 / 7)

## Error Handling

If an Elisp code execution error occurs, the cell will display an error message:

```elisp
=elisp:(/ 1 0)                   ; Display: "Elisp Error: Arithmetic error"
=elisp:(undefined-function)      ; Display: "Elisp Error: Symbol's function definition is void: undefined-function"
```

## Advanced Examples

### Random Number Generation

```elisp
=elisp:(random 100)                              ; Random integer between 0-99
=elisp:(+ 1 (random 6))                          ; Simulate dice (1-6)
```

### File Operations

```elisp
=elisp:(file-exists-p "/etc/passwd")             ; Check if file exists
=elisp:(file-name-directory "/path/to/file.txt") ; Get directory name
```

### System Information

```elisp
=elisp:(system-name)                             ; Get system name
=elisp:(user-login-name)                         ; Get current user name
=elisp:(emacs-version)                           ; Get Emacs version
```

### Complex Calculation Example

```elisp
; Calculate factorial
=elisp:(let ((n 5)) (apply '* (number-sequence 1 n)))

; Generate the nth term of the Fibonacci sequence
=elisp:(let ((n 10)) 
         (if (<= n 2) 1
           (let ((a 1) (b 1))
             (dotimes (i (- n 2))
               (let ((temp (+ a b)))
                 (setq a b)
                 (setq b temp)))
             b)))
```

## Best Practices

1. **Security First**: Only use Elisp formulas in trusted files
2. **Simple and Clear**: Keep Elisp code simple and readable
3. **Error Handling**: Consider possible error cases and use appropriate error handling
4. **Performance Considerations**: Avoid performing time-consuming operations in formulas
5. **Documentation**: Add comments to complex formulas

## Comparison with Standard Formulas

| Feature | Standard Formula | Elisp Formula |
|------|----------|-----------|
| Basic Math | `=A1+B1` | `=elisp:(+ (cell "A1") (cell "B1"))` |
| Conditional Judgment | `=IF(A1>0, "正", "负")` | `=elisp:(if (> (cell "A1") 0) "正" "负")` |
| String Operation | Limited | Unlimited (all Elisp string functions) |
| Date and Time | `=TODAY()` | `=elisp:(format-time-string "%Y-%m-%d")` |
| Flexibility | Limited by built-in functions | Full Elisp programming capabilities |

## Notes

1. Elisp formulas are more powerful than standard formulas, but also more complex
2. Executing Elisp code may have security risks
3. Complex Elisp code may affect table performance
4. It is recommended to use standard formulas for simple calculations and Elisp formulas for advanced features

## Troubleshooting

### Common Errors

1. **Syntax Error**: Ensure Elisp code syntax is correct
2. **Reference Error**: Check if cell references are correct (e.g., "A1" instead of A1)
3. **Function Not Defined**: Ensure the function is available in the current Emacs environment
4. **Type Error**: Pay attention to data type matching

### Debugging Tips

1. Test Elisp code in Emacs' `*scratch*` buffer first
2. Start with simple expressions and gradually increase complexity
3. Pay attention to error messages, they usually point out the problem

---

This feature brings infinite possibilities to Grid Table, allowing you to fully utilize the powerful features of Elisp to process table data. Remember to use it safely and enjoy the fun of programming!
``