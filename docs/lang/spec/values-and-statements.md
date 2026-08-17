# Values, expressions, and statements

Raven is expression-oriented: most computations produce values, and constructs
such as `if` and `match` can be used directly where a value is expected.

```raven
let label = if score >= 50 then "pass" else "fail"
```

Raven still distinguishes expressions from statements. An expression computes
a value. A statement introduces a declaration, performs an action, or controls
which code runs without passing a value to a surrounding expression.

```raven
let message = "ready"        // declaration statement
Console.WriteLine(message)   // expression statement

let length = message.Length  // the initializer is an expression
```

Some constructs, including `if`, `match`, `try`, and `throw`, have both
expression and statement forms. Their spelling can be similar, but their role is
determined by context. Other constructs, such as `while`, `for`, and `loop`, are
statements.

See [Expressions and type inference](expressions-and-inference.md) for
value-producing forms, [Statements](assignment-and-expression-statements.md)
for assignment and effectful forms, and [Control flow](control-flow.md) for how
context and newlines distinguish them.

## The unit value

Raven has no `void` type. A computation with no meaningful result produces the
single `unit` value, written `()`:

```raven
func log(message: string) -> unit {
    Console.WriteLine(message)
}

let completed: unit = ()
```

The type itself may be written `unit` or `()`. A function without an explicit
return type returns `unit`.

`unit` is a real value type. It can be used in generics, tuples, and unions, and
maps to `System.Unit` when Raven code is represented on .NET. At CLR method
boundaries it corresponds to a method that returns `void`; see [.NET
implementation notes](dotnet-implementation.md#unit-type).

The type is also a source-level promise that no meaningful value flows out of a
computation. A non-`unit` expression in the final position of a `unit` function
is diagnosed as an unused result. Discard it intentionally with `_ =
expression`:

```raven
func save() {
    _ = tryWriteFile()
}
```

## Null and absence

`unit` does not represent a missing value. Raven distinguishes these concepts:

* `unit` or `()` means a computation has no meaningful result.
* `T?` means a value of type `T` may be `null`.
* `Option<T>` represents explicit domain-level presence or absence.
* `Result<T, E>` represents success or an expected error.

See the [Type system](type-system.md#nullable-types) for nullable types and
[Error propagation and carrier
types](async-and-error-propagation.md#result-and-option-carrier-operators) for
`Option` and `Result`.

## Declarations and file-scope code

Local values are introduced with `let`, `var`, `const`, or `use`. Their
mutability, inference, deconstruction, and cleanup behavior is described under
[Local declarations](local-declarations.md).

Executable statements can also appear directly in a source file, so a small
program does not need an explicit `Main` function:

```raven
import System.Console.*

let name = "Raven"
WriteLine("Hello, $name!")
```

See [Top-level code and entry points](top-level-code-and-entry-points.md) for
file-scope restrictions and executable entry-point selection.
