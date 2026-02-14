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

* Statement-form `return` (including the omission of a value when returning `unit`).
* Iterator statements `yield return` and `yield break`.
* Loop-control transfers `break` and `continue`.
* Statement-form `throw` that propagates exceptions.
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

When extra tokens remain on the same line after a statement has already
completed, the parser reports `RAV1019: Expected newline or ';' to terminate the
statement.`【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L256-L262】 Insert
semicolons to separate statements intentionally written on one line or move the
remaining tokens to the following line.

```raven
System.Console.WriteLine("Examples") 42 // RAV1019
System.Console.WriteLine("Examples") ff; // RAV1019
var x = 2 test // RAV1019
```

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

Member-access continuation follows the same model. A leading `.` (or `->`) on
the next line continues the previous expression when separated by a single
newline. If there is a blank line (two or more consecutive newlines), the
continuation is broken and parsing resumes as a new statement/expression.

```raven
val sum =
    1
    + offset

val labelled = 42 // comment stays with the literal
val next =
    labelled
```

```raven
val chain = source
    .Child
    .SubChild

val first = source.Call()

.Ok // starts a new target-typed expression (blank line breaks continuation)
```

In the example above, the newline following `=` and the newline immediately
before `+` attach to the numeric literal and the operator, respectively. The
comment preceding a terminating newline remains trailing trivia on the
preceding token, and the indentation before `val next` is preserved as leading
whitespace on the next statement's first token.

```raven
val a = 42
val b = 1; b = 3
```

## Assignment statements

Assignments in statement position produce an `AssignmentStatement` node rather
than flowing through `ExpressionStatement`. The left-hand side accepts both
assignable expressions and patterns, mirroring tuple and declaration
assignments. Raven recognizes `_ = expression` as a discard assignment where the
left-hand side is a dedicated discard expression. The assignment still runs the
right-hand side but the result is explicitly ignored.

`AssignmentStatementSyntax.IsDiscard` reports whether the assignment targets the
discard identifier (either via a discard pattern or the discard expression).

## Expression statements

Most other expressions can appear as statements.

Control flow constructs such as `if`, `while`, `for`, and statement-form `match`
also have dedicated
statement forms for convenience. The parser rewrites an expression in statement
position to the corresponding statement node when the value is discarded.
`ExpressionStatement` covers the remaining expressions that may appear on their
own line and always evaluates to `unit`.

Values produced by expression statements are discarded and do not become implicit
function return values.

## Match statement

Statement-form `match` (`match expr { ... }`) is a control-flow statement. Its
arms are evaluated for effects, and arm values are discarded. It never
contributes an implicit return value for the enclosing function or block.

## Return statements and expressions

The `return` keyword exits a function, lambda, or property accessor. Because
Raven supports both statement and expression forms:

* Statement form: `return` or `return <expression>` in statement position.
* Expression form: `return <expression>` in expression contexts where early exit
  is valid (for example, `name ?? return -1`).

`return` statements remain statement-only. The compiler reports `RAV1900` when a
statement `return` is used in inline expression contexts (for example, expression
`if`/`match` arms).

`return` expression form is intentionally narrow. It models an abrupt exit from
the enclosing callable while still fitting expression-oriented operators like
`??`, `if` arms, and similar value positions. It does not turn expression blocks
into statement-flow regions. In other words:

* `let b = if (a) e else return -1` is valid (`else return -1` is a return expression).
* `let b = if (a) e else { return -1 }` is invalid because `{ ... }` here is a
  block expression, and `return` inside that block is parsed as a statement.

A `return` statement may omit its expression when the surrounding function or
accessor returns `unit`. See [implementation notes](dotnet-implementation.md#return-statements)
for how such returns are emitted. This is equivalent to returning the `()` value
explicitly.

```raven
return            // equivalent to returning ()
return ()         // explicit unit return
```

Within a method-like body, each `return` is validated against the declared
return type of that body. A `return` without an expression is treated as
returning `unit`. If the returned expression's type is not assignable to the
declared return type, the compiler emits a conversion diagnostic.

Return expressions follow the same return-type validation as return statements:
the payload must be assignable to the enclosing function/lambda/accessor return
type.

Implicit return inference for methods/functions/lambdas uses explicit `return`
statements and the outer body tail expression only. Tail expressions inside nested
statement blocks are not treated as implicit returns for the enclosing member.

```raven
func DoOperation(a: int, b: int) -> int {
    if a > b {
        return -1
    }
    return a + b
}

func DoOperation2(a: int, b: int) -> Result<int, bool> {
    if a > b {
        return .Error(false)
    }
    return .Ok(a + b)
}
```

Property accessors follow the same rule: each explicit `return` must produce a
value compatible with the property's declared type.

```raven
func choose(flag: bool) -> Result<int, ()> {
    return if flag {
        .Ok(42)
    } else {
        .Error(())
    }
}

if flag {
    return 42          // allowed: expression used as a statement
}

func log(msg: string) {
    Console.WriteLine(msg)
    return            // equivalent to returning ()
}

func firstCharOrFail(name: string?) -> Result<int, string> {
    val required = name ?? return .Error("Missing name")
    return .Ok(required.Length)
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
