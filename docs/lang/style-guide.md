# Raven Style Guide

This document captures the default source style for Raven code. It is
intentionally small and focuses on readability and formatter-friendly layout.

## Indentation

- Use 4 spaces per indentation level.
- Do not use tabs for indentation.
- Indent the contents of blocks, type declarations, and accessors one level.

```raven
class Counter {
    var value: int = 0

    func Increment() -> () {
        value += 1
    }
}
```

## Spacing

- Use a single space after `:`, `,`, and around binary operators.
- Do not put spaces immediately inside `()`, `[]`, or generic argument lists.
- Use a single space before `{` in declarations and control-flow statements.
- Keep member access compact: `value.Length`, not `value . Length`.

```raven
func Max(x: int, y: int) -> int {
    return x > y ? x : y
}
```

## Line breaks

- Put each declaration on its own line.
- Prefer one statement per line.
- Keep `else`, `catch`, and `finally` on the same line as the preceding `}`.
- Leave a blank line between top-level declarations and between logical groups
  of members.

## Braces

- Use braces for type declarations, functions, and multi-statement control flow.
- Place the opening brace on the same line as the declaration or condition.
- Place the closing brace on its own line.

## Imports

- Put imports at the top of the file.
- Keep one import per line.
- Leave a blank line between the import block and the first declaration or
  statement.

## Attributes and macros

- Place attributes and attached macros on the line directly above the target
  declaration.
- Do not insert blank lines between an attribute/macro and the declaration it
  applies to.
- When stacking attached macros, keep the order intentional and easy to read.
- Avoid stacking multiple replacement-oriented macros on the same declaration;
  prefer one declaration-owning macro plus additive helpers.
- When both a parent declaration and its members use macros, keep the parent
  macro independent from member rewrites.

```raven
#[Observable]
var Title: string = ""
```

## Generated syntax

- `SyntaxFactory` produces raw structured nodes.
- If you build syntax programmatically, either attach explicit trivia yourself
  or run the result through `NormalizeWhitespace()` / `Formatter.Format(...)`
  before presenting it to users.
