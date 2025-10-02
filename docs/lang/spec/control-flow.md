# Control flow

Raven is primarily **expression-oriented**: most constructs yield values and can
appear wherever an expression is expected, reflecting its expression-first design.
While Raven aims to be expression-oriented and declarative, it can still be used
in an imperative and procedural manner when desired.

## Expression and statement context

Every occurrence of code appears in either an **expression** or a **statement**
context.

* An **expression context** expects a value. Examples include the right-hand
  side of an assignment, arguments in an invocation, the scrutinee or arms of a
  `match`, the condition and branches of an `if` expression, and the final
  position in a block expression where the block's resulting value is produced.
  Any expression form is valid in these positions, including control-flow
  expressions (`if`, `while`, `for`, `match`), lambdas, tuples, collection
  expressions, and nested blocks. The expression contributes to type inference
  and must evaluate to a value assignable to the surrounding context's target
  type.
* A **statement context** expects an effect rather than a value. Statement
  positions occur inside blocks before the final expression, inside method and
  accessor bodies, and anywhere a newline (or semicolon) can terminate a
  statement. The parser rewrites standalone expressions in these positions into
  expression statements that discard the produced value. Declarations such as
  `let`/`var` bindings and dedicated control-flow statements all occupy
  statement contexts.

Only certain constructs are legal exclusively in statement context because they
do not yield a value:

* `return` (including the omission of a value when returning `unit`).
* Iterator statements `yield return` and `yield break`.
* Loop-control transfers `break` and `continue`.
* `throw` statements that propagate exceptions.
* Statement-form `try` blocks that attach `catch` or `finally` clauses.

Using any of these constructs inside an expression context produces the
diagnostics called out in the sections below. Conversely, expression constructs
are always valid in statement context—their values are simply discarded unless
captured.

For clarity and early exits, the language nevertheless permits certain imperative
statement forms, such as the explicit `return` statement. Statements are
terminated by a **newline**, or by an **optional semicolon** `;` that may separate
multiple statements on one line. Newlines inside parentheses, brackets, or braces
do not terminate statements. Multiple consecutive newlines act as a single
separator so you may visually group related statements without affecting
execution. Blocks also terminate with `}`, and certain keywords (such as `else`,
`catch`, and `finally`) implicitly end the preceding construct. These tokens,
along with end-of-file, fulfil the same terminating role as a newline when they
appear.

When a newline is not required to terminate a statement—or any other construct
that relies on newline separation, such as `import` or `alias` directives—it is
preserved as trivia on the following token. This occurs whenever the parser is
still expecting more of the current expression and therefore treats the newline
as part of a continuation rather than as a terminator.

## Line continuations

When the parser expects more tokens to complete the current expression, a
newline is treated as trivia on the following token instead of a statement
terminator. This permits expression-oriented code to flow naturally across
lines without requiring a trailing operator or explicit continuation marker.

Because the newline is preserved as trivia in these scenarios, any indentation
on the continued line also remains as trivia on the subsequent token. The
terminator token itself—whether a newline, semicolon, or closing brace—remains
free of incidental trivia so its presence or absence is unambiguous.

The most common continuations occur after an assignment operator or when a
binary operator still requires its right-hand operand. Indentation on the
continued line becomes leading whitespace trivia for the next token, while the
terminating newline token itself remains trivia-free.

```raven
let sum =
    1
    + offset

let labelled = 42 // comment stays with the literal
let next =
    labelled
```

In the example above, the newline following `=` and the newline immediately
before `+` attach to the numeric literal and the operator, respectively. The
comment preceding a terminating newline remains trailing trivia on the
preceding token, and the indentation before `let next` is preserved as leading
whitespace on the next statement's first token.

```raven
let a = 42
let b = 1; b = 3
```

## Expression statements

Any expression can appear as a statement.

Control flow constructs such as `if`, `while`, and `for` also have dedicated
statement forms for convenience. The parser rewrites an expression in statement
position to the corresponding statement node when the value is discarded.
`ExpressionStatement` covers the remaining expressions that may appear on their
own line and always evaluates to `unit`.

## Return statements

The `return` keyword exits a function, lambda, or property accessor. Because
control-flow constructs are expressions, using `return` inside an expression that
itself produces a value is not allowed. Explicit `return` statements may appear
only in statement positions, such as within a function body or as their own
expression statement. When a `return` occurs in a value context—for example,
within an `if` expression assigned to a variable—the compiler reports diagnostic
`RAV1900` and the block should rely on an implicit return instead. Within an `if`
statement, the `else` keyword implicitly terminates a preceding `return`,
allowing `if flag return else return` to be written without additional
separators.

A `return` statement may omit its expression when the surrounding function or
accessor returns `unit`. See [implementation notes](dotnet-implementation.md#return-statements)
for how such returns are emitted. This is equivalent to returning the `()` value
explicitly.

Within a method-like body, each `return` is validated against the declared
return type of that body. A `return` without an expression is treated as
returning `unit`. If the returned expression's type is not assignable to the
declared return type, the compiler emits a conversion diagnostic.

```raven
func DoOperation(a: int, b: int) -> int {
    if a > b {
        return -1
    }
    return a + b
}

func DoOperation2(a: int, b: int) -> int | false {
    if a > b {
        return false
    }
    return a + b
}
```

Property accessors follow the same rule: each explicit `return` must produce a
value compatible with the property's declared type.

```raven
func choose(flag: bool) -> int | () {
    if flag {
        42            // implicit return
    } else {
        ()             // implicit return
    }
}

if flag {
    return 42          // allowed: expression used as a statement
}

func log(msg: string) {
    Console.WriteLine(msg)
    return            // equivalent to returning ()
}
```

## `yield return` statements

Iterator-like members may suspend execution with `yield return expression`.
Each `yield return` produces the next element in the enumerator sequence while
preserving the generator's state so execution can resume on the next
iteration. The `yield` keyword must be immediately followed by `return`, and an
expression is required.

```raven
func numbers() -> IEnumerable<int> {
    var i = 0
    while i < 3 {
        yield return i
        i += 1
    }
}
```

Like explicit `return` statements, `yield return` is limited to statement
positions and cannot appear in expression context.

## `yield break` statements

`yield break` terminates an iterator early. Any remaining statements in the
member are skipped, and the enumerator completes without producing additional
values.

```raven
func firstOrNone(values: IEnumerable<int>) -> IEnumerable<int> {
    for each value in values {
        yield return value
        yield break
    }

    yield break
}
```

The `yield break` form follows the same placement rules as `yield return` and
`break`—it must appear in statement position and is illegal outside iterator
bodies.

## `break` statements

`break` exits the innermost enclosing loop statement immediately. Execution
resumes at the statement following that loop.

```raven
var i = 0
while true {
    if i == 10 {
        break
    }

    i += 1
}
```

A `break` statement must appear within a `while` or `for` *statement*. Placing
`break` in any expression context, including the bodies of `if`, `while`, or `for`
expressions, produces diagnostic `RAV1902`. Using `break` outside a loop reports
diagnostic `RAV2600`.

## `continue` statements

`continue` skips the remainder of the current loop iteration and jumps to the
loop's re-check point.

```raven
for each value in values {
    if value.isOdd {
        continue
    }

    print(value)
}
```

`continue` follows the same placement rules as `break`: it may only appear inside
`while` or `for` statements. Using it from an expression context results in
diagnostic `RAV1903`, and placing it outside a loop reports diagnostic `RAV2601`.

## Labeled statements

A **labeled statement** prefixes another statement with an identifier followed by
a colon:

```raven
start:
print("running")
```

Labels introduce symbolic targets that `goto` statements can reference. A label
applies to the next statement in the source. When a newline immediately follows
the colon, the compiler synthesizes an empty statement so the label remains a
valid target even without an explicit body. Multiple labels may precede the same
statement.

Label names are scoped to the containing function body. Declaring the same label
more than once in the same body is an error (`RAV2500`). Labels participate in
semantic lookup just like other declarations, so tools can navigate to them.

Like all statements, a label may only appear where the grammar permits
statements. Attempting to nest a labeled statement inside an expression body (for
example, inside the branches of an `if` expression) reports diagnostic `RAV1905`.

## `goto` statements

The `goto` statement jumps to a labeled statement within the same function-like
body:

```raven
start:
print("loop")
goto start
```

Evaluation of a `goto` statement ends the current statement immediately and
transfers control to the target label. Targets may appear before or after the
`goto`; backward jumps form loops while forward jumps skip ahead in the block.
Jumping to a nonexistent label produces diagnostic `RAV2501`.

Gotos cannot escape the body they are declared in. A `goto` inside a function,
lambda, or accessor may only refer to labels declared in that same body. The
compiler ensures any scopes exited by the jump are correctly unwound before
branching.

`goto` statements follow the same placement rules as other control-flow
statements: they are only legal in statement contexts. Embedding a `goto` inside
an expression context (such as the body of an `if` expression) produces
diagnostic `RAV1904`.
