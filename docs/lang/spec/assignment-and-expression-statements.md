# Statements

Statements execute actions, introduce declarations, or control which code runs.
Unlike expressions, they do not pass a value to a surrounding expression.

```raven
let message = "ready"        // declaration statement
Console.WriteLine(message)   // expression statement
count = count + 1            // assignment statement
```

Newlines normally separate statements. Semicolons are optional, but can
separate several statements on one line. See [Control
flow](control-flow.md#statement-termination) for the complete termination and
line-continuation rules.

## Assignment statements

An assignment replaces or updates the value stored in a writable location:

```raven
count = 1
count += 2
person.Name = "Ada"
values[index] = 42
```

In statement position, assignment syntax produces an assignment statement. The
left side may be an assignable expression—such as a mutable local, property,
field, or element access—or a supported assignment pattern.

Use `_ = expression` to evaluate an expression and discard its result
explicitly:

```raven
_ = trySave()
```

Nullable conditional member assignment is also valid:

```raven
receiver?.Member = value
receiver?.Member += delta
```

Raven evaluates the receiver once and skips the write when it is `null`.

Pattern assignments can update several existing locations at once. Their forms
and extraction rules are described under [Matching and
deconstruction](pattern-matching.md#matching-and-deconstruction).

## Expression statements

Most expressions can be used as statements when their value is not needed:

```raven
Console.WriteLine("saved")
service.Refresh()
```

The expression is evaluated for its effects and the statement has type `unit`.
Its value does not become an implicit return value merely because the statement
appears at the end of a function.

A final non-`unit` expression in a `unit`-returning function is still discarded,
but diagnostic `RAV9034` warns because it can resemble an accidental implicit
result. Use `_ = expression` to make an intentional discard clear, change the
return type when the value should be returned, or configure the analyzer when
that convention is not wanted.

## Control-flow statements

`if`, `loop`, `while`, `for`, `match`, and `try` have dedicated statement forms.
They run branches or bodies for their effects instead of contributing a value
to a surrounding expression.

Statement-form `match`, and statement-form `if` with an `else`, can provide an
implicit tail result when they are the final statement of a value-returning
body. Outside that position, Raven warns when statement-form control flow
appears to compute branch values that are discarded:

* `RAV2107` for `match`
* `RAV2108` for `if`
* `RAV2109` for `try`

Add explicit `return` statements when using statement form, or use the
corresponding expression in a value context.

Pattern-binding statements such as `if let`, `while let`, `for let`, and
`let ... else` combine a pattern test with local declarations:

```raven
if let Some(name) = maybeName {
    Console.WriteLine(name)
}

let Some(requiredName) = maybeName else {
    return
}
```

See [Pattern matching](pattern-matching.md) for their binding behavior and
[Control-flow expressions and statements](control-flow-expressions.md) for
branching and loops.

## Lock statements

A `lock` statement prevents multiple threads from executing a protected block
through the same gate at the same time. Use it to synchronize access to shared
mutable state:

```raven
lock gate {
    updateSharedState()
}
```

The gate expression must have a reference type. Raven evaluates it exactly once
and acquires its monitor before entering the body. The monitor is always
released when control leaves the body, including through `return` or an
exception.

Choose a dedicated, privately held reference as the gate so unrelated code
cannot acquire the same monitor accidentally.

On .NET, the statement has the same effect as storing the gate in a hidden
local, calling `System.Threading.Monitor.Enter`, executing the body in a `try`,
and calling `System.Threading.Monitor.Exit` from `finally`.
