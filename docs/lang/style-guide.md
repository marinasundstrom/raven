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
- Avoid unnecessary trailing separators in ordinary comma-delimited lists.

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

## Bindings

- Use `let` for immutable lexical bindings in standard Raven style.
- Use `var` for lexical bindings that must be reassigned.
- Use `val` and `var` for properties and signature-like declarations.
- `val` remains a supported alternative to `let` for immutable lexical bindings.
  Teams that want to enforce the standard spelling may explicitly enable
  `PreferLetInsteadOfValAnalyzer`.
- A lexical binding introduced with `let` is semantically a read-only `val` and
  is displayed as `val` by semantic tooling such as hover.
- Remember that `let` and `val` prevent reassignment of the binding; they do not
  guarantee that a referenced object is deeply immutable.

## Braces

- Use braces for type declarations, functions, and multi-statement control flow.
- Place the opening brace on the same line as the declaration or condition.
- Place the closing brace on its own line.

## Imports

- Put imports at the top of the file.
- Keep one import per line.
- Leave a blank line between the import block and the first declaration or
  statement.

## Source file organization

- Use `Main.rvn` only when the file contains the `Main` entry point by itself.
- Use `Program.rvn` for top-level statements or a program file that encompasses
  multiple functions or types, including its `Main` entry point.
- Use `<Type>.rvn` for a file centered on one primary type. Closely related
  enums, unions, helper types, and functions may remain in that file when the
  primary type clearly owns the concept.
- Use `<area-name>.rvn` for a related collection of types and functions, similar
  to a module or domain area.
- Use `<function>.rvn` for a function that deserves its own file.
- A small, cohesive program may remain in one `Program.rvn` file. As a project
  grows, extract types, functions, or domain areas into appropriately named
  files, especially when that organization improves the example or API story.

## Attributes and macros

These conventions describe the current macro syntax. The macro model is still a
work in progress and may change as it develops.

- Place attributes and attached macros on the line directly above the target
  declaration.
- Do not insert blank lines between an attribute/macro and the declaration it
  applies to.
- When stacking attached macros, keep the order intentional and easy to read.
- Avoid stacking multiple replacement-oriented macros on the same declaration;
  prefer one declaration-owning macro plus additive helpers.
- When both a parent declaration and its members use macros, keep the parent
  macro independent from member rewrites.
- Prefer the invocation-like `name! { ... }` spelling for token-tree expression
  macros. It keeps the expression flow visible and communicates that the macro
  owns and transforms the following region of code. The `#name { ... }`
  spelling remains supported.

```raven
#[Observable]
var Title: string = ""

let syntax = quote! {
    left + right
}
```

## Generated syntax

- `SyntaxFactory` produces raw structured nodes.
- If you build syntax programmatically, either attach explicit trivia yourself
  or run the result through `NormalizeWhitespace()` / `Formatter.Format(...)`
  before presenting it to users.
