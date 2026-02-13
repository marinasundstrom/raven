# Error handling

Raven models exceptions as a last-resort control transfer for unexpected
conditions. The language offers both statement-based structured exception
handling and expression forms that surface errors as values.

## `throw` statements and expressions

The `throw` keyword aborts the current control flow by propagating an exception.
The operand must be an expression whose type derives from `System.Exception`;
using any other type produces diagnostic `RAV1020`.

Raven supports two forms:

- Statement form: `throw <expression>` as a statement.
- Expression form: `throw <expression>` anywhere an expression is valid (for example in `??`).

The statement form still follows statement placement rules. The compiler reports
`RAV1907` when a statement `throw` is used in inline expression contexts (for
example, expression `if`/`match` arms). Use throw expression form in those
contexts.

Unlike `return`, `throw` has a single control-flow meaning in both forms: it
always aborts the current evaluation path and propagates an exception. That
makes it safe to embed in expression positions (for example the right-hand side
of `??`) without introducing statement-flow semantics into expression blocks.

Throwing an exception unwinds the stack just like returning early: `use`
declarations in the current scope are disposed before the exception escapes.
Because exceptions are expensive and intended for unexpected situations, prefer
returning a dedicated result object (`Result<T, TError>`) to model normal
control flow. Reserve `throw` for
genuinely exceptional circumstances so APIs remain predictable and declarative.
This recommendation applies equally to throw statements and throw expressions.

```raven
func parseInt(text: string) -> Result<int, ParseError> {
    if text.isEmpty {
        return .Error(ParseError.Empty)
    }

    try {
        return .Ok(int.Parse(text))
    } catch (System.FormatException ex) {
        return .Error(ParseError.Invalid(ex.Message))
    }
}

func readConfig(path: string) {
    use stream = File.OpenRead(path)
    if stream is null {
        throw System.IO.FileNotFoundException(path)
    }
    // ...
}

func requireName(name: string?) -> string {
    return name ?? throw ArgumentException("Missing name")
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

The expression's type is `System.Result<T, System.Exception>`, where `T` is the
operand's value type (or `unit` if the operand produces no value). If the operand
completes normally, its result is wrapped in `.Ok(...)` and returned. If an
exception escapes the operand, the expression instead yields `.Error(exception)`.
This makes `try` expressions particularly useful with pattern matching:

```raven
val value = try int.Parse(input) match {
    .Ok(val no) => $"No is {no}"
    .Error(FormatException ex) => $"Format invalid: {ex.Message}"
    .Error(val ex) => $"Unexpected error: {ex.Message}"
}
```

When a case has a single payload, a bare case pattern like `.Ok` is sugar for
`.Ok(())` and matches the unit payload.

Because the exception is materialized as a value, the operand still executes its
normal scope unwinding before control reaches the surrounding expression. A `try`
expression cannot attach `catch` or `finally` clauses; those constructs are
reserved for the statement form. Additionally, `try` expressions cannot nest
directly—`try try value` reports an error—both to avoid ambiguous typing and to
encourage pattern matching over the `Result` value.
