# Error handling

Raven models exceptions as a last-resort control transfer for unexpected
conditions. The language offers both statement-based structured exception
handling and expression forms that surface errors as values.

## `throw` statements

The `throw` keyword aborts the current control flow by propagating an exception.
The operand must be an expression whose type derives from `System.Exception`;
using any other type produces diagnostic `RAV1020`. Like other control-flow
statements, `throw` may only appear in statement positions. When it occurs inside
an expression context—such as within the branches of an `if` expression—the
compiler reports diagnostic `RAV1907`.

Throwing an exception unwinds the stack just like returning early: `using`
declarations in the current scope are disposed before the exception escapes.
Because exceptions are expensive and intended for unexpected situations, prefer
returning a dedicated result object (for example, a union or struct that carries
either the value or an error) to model normal control flow. Reserve `throw` for
genuinely exceptional circumstances so APIs remain predictable and declarative.

```raven
func parseInt(text: string) -> int | ParseError {
    if text.isEmpty {
        return ParseError.Empty
    }

    try {
        return int.Parse(text)
    } catch (System.FormatException ex) {
        return ParseError.Invalid(ex.Message)
    }
}

func readConfig(path: string) {
    using let stream = File.OpenRead(path)
    if stream is null {
        throw System.IO.FileNotFoundException(path)
    }
    // ...
}
```

## `try` statements

`try` provides structured exception handling and comes in both statement and
expression forms. The statement form wraps a block and must include at least one
`catch` clause or a `finally` clause. Omitting both results in diagnostic
`RAV1015`. Statement `try` constructs follow the usual placement rules for
statements: they can only appear where a statement is permitted.

```
try {
    operation()
} catch (FormatException ex) {
    Console.WriteLine($"Bad input: {ex.Message}")
} finally {
    cleanup()
}
```

Each `catch` may declare an exception type and optional identifier using `catch
(Type name)`. The declared type must be or derive from `System.Exception`;
otherwise the compiler reports `RAV1016`. When the identifier is omitted, the
exception value is still available for pattern-based filtering but is not bound to
a local. A bare `catch` with no parentheses is equivalent to `catch
(System.Exception)` and handles all exceptions of that type. Catch clauses run in
source order until one handles the propagated exception; execution then resumes
after the `try` statement unless a `finally` clause alters control flow.

The optional `finally` clause executes regardless of whether the `try` block or
any `catch` clause complete normally. It also runs when a `catch` clause rethrows
or when control leaves the statement early (for example, because of `return`,
`break`, or `goto`).

## `try` expressions

A `try` expression evaluates an arbitrary subexpression and produces a value
representing either that expression's result or any exception thrown while
evaluating it. The syntax is `try <expression>` with no accompanying `catch` or
`finally` clauses. Because it is an expression, this form is valid anywhere an
expression can appear, including as the scrutinee for `match` expressions or as
part of larger expression trees. The operand may be a block expression, a method
invocation, a conditional, and so on.

The expression's type is the normalized union of the operand's value type and
`System.Exception`. If the operand completes normally, its result is converted to
the union and returned. If an exception escapes the operand, the expression
instead yields the caught exception instance. This makes `try` expressions
particularly useful with pattern matching:

```raven
let value = try int.Parse(input) match {
    int no => $"No is {no}"
    FormatException ex => $"Format invalid: {ex.Message}"
    _ => "unknown"
}
```

Because the exception is materialized as a value, the operand still executes its
normal scope unwinding before control reaches the surrounding expression. A `try`
expression cannot attach `catch` or `finally` clauses; those constructs are
reserved for the statement form. Additionally, `try` expressions cannot nest
directly—`try try value` reports an error—both to avoid ambiguous typing and to
encourage pattern matching over the union result.
