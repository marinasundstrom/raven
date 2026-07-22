# Control flow

Raven is expression-oriented, but it also supports statement-form control flow
for sequencing, early exits, and interop-friendly imperative code.

## Expression and statement context

Every occurrence of code appears in either an expression context or a statement
context.

### Concept

* An expression context expects a value.
* A statement context expects an effect.
* Expressions are valid in statement context; their values are discarded unless
  captured.
* Some constructs are statement-only because they do not yield a value.

### Example

```raven
val value = if flag { 1 } else { 2 }

if flag {
    Log("side effect")
}
```

### Rules

* Expression contexts include assignment right-hand sides, invocation
  arguments, `match` scrutinees and arms, `if` expression branches, and the
  final position of a block expression.
* Statement contexts include block bodies before the final expression, method
  bodies, accessor bodies, and any location terminated by a newline or
  semicolon.
* Statement-form `return`, `yield return`, `yield break`, `break`, `continue`,
  statement-form `throw`, and statement-form `try` are valid only in statement
  context.
* Statements are terminated by a newline, an optional semicolon, `}`, or a
  construct-closing keyword such as `else`, `catch`, or `finally`.
* Newlines inside parentheses, brackets, or braces do not terminate
  statements.
* End-of-file fulfills the same terminating role as a newline.

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

## Line continuations

Line continuation is layout-sensitive and uses at most one newline.

### Concept

When the parser still expects more of the current expression, one newline is
treated as continuation trivia instead of as a statement terminator.

### Example

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

```raven
x
+ 2     // same expression

x
.Foo()  // same expression

x

+ 2     // new statement

x

.Foo()  // new statement (member binding / target-typed form)
```

```raven
val a = 42
val b = 1; b = 3
```

### Rules

* One newline may continue the current expression.
* Two or more consecutive newlines always terminate the current expression
  statement.
* Continuation commonly occurs after assignment operators, before binary
  operators, and before member access (`.` or `->`).
* Indentation on the continued line remains trivia and does not change the
  semantics of the continued expression.

## Detailed chapters

* [Assignment and expression statements](assignment-and-expression-statements.md)
* [Match statements](match-statements.md)
* [Return and yield](returns-and-yield.md)
* [Jumps and labels](jumps-and-labels.md)
