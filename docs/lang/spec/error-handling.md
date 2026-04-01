# Error handling

Raven models exceptions as a last-resort control transfer for unexpected
conditions. The language offers both statement-based structured exception
handling and expression forms that surface errors as values.

## `throw` statements and expressions

`throw` aborts the current evaluation path by propagating an exception value.
The operand must have a type derived from `System.Exception`; otherwise the
compiler reports `RAV1020`.

### Concept

Raven supports both statement-form `throw expr` and expression-form `throw expr`.
Both forms have the same runtime behavior: they unwind the current scope and
propagate the exception outward.

### Example

```raven
func parseInt(text: string) -> Result<int, ParseError> {
    if text.isEmpty {
        return .Error(ParseError.Empty)
    }

    try {
        return .Ok(int.Parse(text))
    } catch System.FormatException ex {
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

### Rules

* Statement-form `throw` is valid only in statement context.
* Expression-form `throw` is valid anywhere an expression is valid.
* Using statement-form `throw` in an inline expression context produces
  `RAV1907`.
* `use` declarations in the current scope are disposed before the exception
  escapes.
* Prefer carrier-based error modeling such as `Result<T, E>` for expected
  failures; reserve `throw` for exceptional conditions.

## `try` statements

Statement-form `try` provides structured exception handling around a block.

### Concept

A `try` statement wraps a block and must include at least one `catch` clause or
a `finally` clause.

### Example

```raven
try {
    operation()
} catch FormatException ex {
    Console.WriteLine($"Bad input: {ex.Message}")
} finally {
    cleanup()
}
```

Parenthesized forms reuse the same pattern surface, and `when` guards may follow the pattern:

```raven
try {
    operation()
} catch (Exception ex) when ex.StatusCode == 2 {
    Console.WriteLine($"Retriable failure: {ex.Message}")
}
```

### Rules

* Omitting both `catch` and `finally` produces `RAV1015`.
* Each `catch` may provide an exception pattern. The supported runtime form today
  is an exception type pattern such as `catch FormatException ex`.
* Parenthesized catch patterns are also accepted for grouping and future
  compatibility, for example `catch (FormatException ex)`.
* `catch` guards follow the catch pattern, for example
  `catch Exception ex when ex.StatusCode == 2` or
  `catch (Exception ex) when ex.StatusCode == 2`.
* The declared catch type must be `System.Exception` or a derived type;
  otherwise the compiler reports `RAV1016`.
* A bare `catch` is equivalent to `catch (System.Exception)`.
* `catch` reuses Raven pattern syntax, but richer non-type primary catch
  patterns are still diagnosed until full catch-pattern semantics are expanded.
* Catch clauses run in source order.
* `finally` executes whether the `try` block, a `catch` clause, or an early
  control transfer completes the statement.

## `try` expressions

`try expr` captures exceptions as a `Result<T, Exception>` value. `try? expr`
captures and immediately propagates failures through the enclosing carrier
context.

### Concept

`try expr` evaluates `expr` exactly once. Success produces `.Ok(value)` or
`.Ok(())`; failure produces `.Error(exception)`.

`try? expr` is shorthand for `(try expr)?`.

### Example

```raven
func describeNumber(text: string) -> string {
    return try int.Parse(text) match {
        Ok(val value) => "Parsed: $value"
        Error(FormatException ex) => "Invalid format: ${ex.Message}"
        Error(_) => "Unexpected failure"
    }
}

func parseRequiredInt(text: string) -> Result<int, Exception> {
    val value = try? int.Parse(text)
    return .Ok(value)
}
```

### Rules

* `try expr` has type `Result<T, Exception>`, where `T` is the operand type or
  `unit`.
* `try expr` does not accept `catch` or `finally`.
* `try? expr` is valid only in an enclosing `Result<_, _>` or `Option<_>`
  return context.
* Directly nested `try` expressions are invalid.
* A trailing `match` after `try?` is invalid.
* A bare `Ok` pattern matches `Ok(())` when the success payload is `unit`;
  `.Ok` remains available as the target-typed shorthand.
