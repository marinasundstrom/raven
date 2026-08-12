# Return and yield

Use `return` to finish a function and provide its result. Use `yield` when a
function should produce a sequence one item at a time.

The `return` keyword exits a function, lambda, or property accessor. Because
Raven supports both statement and expression forms:

* Statement form: `return` or `return <expression>` in statement position.
* Expression form: `return <expression>` in expression contexts where early exit
  is valid (for example, `name ?? return -1`).

`return` statements remain statement-only. The compiler reports `RAV1900` when a
statement `return` is used in inline expression contexts (for example, expression
`if`/`match` arms).

`return` expression form models an abrupt exit from the enclosing callable
while still fitting expression-oriented operators like `??`, `if` arms, and
other value positions. It contributes no value to a type join because that
control path never completes normally.

Expression blocks project a leading `return` item as an expression statement
containing `ReturnExpressionSyntax`, rather than as `ReturnStatementSyntax`.
This keeps the same early-exit model available after setup statements without
requiring an extra nested statement block. In other words:

* `let b = if (a) e else return -1` is valid (`else return -1` is a return expression).
* `let b = if (a) e else { return -1 }` is also valid; the expression block's
  `return` item is projected as an abrupt expression.
* A bare `return` in an expression block has an implicit `unit` payload, just
  like statement-form `return` in a unit-returning callable.

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

For a callable returning `unit`, a non-`unit` outer tail value is discarded
rather than used as a return. Because that value can look like an implicit
result, diagnostic `RAV9034` warns by default that it is unused. Write
`_ = expression` to make an intentional discard explicit, change the callable
return type when the value is meant to be its result, or disable `RAV9034` when
that convention is not desired.

```raven
func DoOperation(a: int, b: int) -> int {
    if a > b {
        return -1
    }
    return a + b
}

func DoOperation2(a: int, b: int) -> Result<int, bool> {
    if a > b {
        return Error(false)
    }
    return Ok(a + b)
}
```

Property accessors follow the same rule: each explicit `return` must produce a
value compatible with the property's declared type.

```raven
func choose(flag: bool) -> Result<int, ()> {
    return if flag {
        Ok(42)
    } else {
        Error(())
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
    let required = name ?? return Error("Missing name")
    return Ok(required.Length)
}
```

## `yield` and `yield return` statements

Iterator-like members may suspend execution with either `yield expression` or
`yield return expression`. Each form produces the next element in the
enumerator sequence while preserving the generator's state so execution can
resume on the next iteration. Async iterators returning `IAsyncEnumerable<T>`
or `IAsyncEnumerator<T>` also participate in enumeration cancellation through
`GetAsyncEnumerator(CancellationToken)`. To make that enumerator token visible
inside the iterator body, mark the intended `CancellationToken` parameter with
`[EnumeratorCancellation]`.

```raven
func numbers() -> IEnumerable<int> {
    var i = 0
    while i < 3 {
        yield i
        i += 1
    }
}
```

```raven
import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*
import System.Threading.Tasks.*

async func numbers([EnumeratorCancellation] cancellationToken: CancellationToken) -> IAsyncEnumerable<int> {
    yield 1
    await Task.Delay(1000, cancellationToken)
    yield 2
}
```

`yield expression` is shorthand for `yield return expression`. Raven continues
to support the explicit `yield return` spelling as well. Both spellings are
valid in statement and expression positions. In expression position, yielding
suspends the iterator and evaluates to `unit` when enumeration resumes.

## `yield break` statements

`yield break` terminates an iterator early. Any remaining statements in the
member are skipped, and the enumerator completes without producing additional
values.

```raven
func firstOrNone(values: IEnumerable<int>) -> IEnumerable<int> {
    for value in values {
        yield value
        yield break
    }

    yield break
}
```

The `yield break` form is valid in statement and expression positions, but
remains illegal outside iterator bodies. As an expression it never completes
normally, so it can occupy a value-producing branch without affecting the
branch type.
