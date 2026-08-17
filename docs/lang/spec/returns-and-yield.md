# Return and yield

Use `return` to finish a function and optionally provide its result. Use
`yield` to produce a sequence lazily, one element at a time.

## Returning from a callable

`return` exits the enclosing function, lambda, or property accessor. A callable
that returns `unit` may omit the value; `return` and `return ()` are equivalent.

```raven
func compare(a: int, b: int) -> int {
    if a > b {
        return -1
    }

    return a + b
}

func log(message: string) {
    Console.WriteLine(message)
    return
}
```

Every returned value must be implicitly convertible to the callable's declared
return type. Property accessors follow the same rule using the property's type.

Raven also supports `return` as an abrupt expression. This makes early exit
available inside `if` and `match` branches, null-coalescing expressions, and
other value positions:

```raven
func length(name: string?) -> Result<int, string> {
    let required = name ?? return Error("Missing name")
    return Ok(required.Length)
}

let result = if ready value else return fallback
```

The returning path never produces a value for its surrounding expression, so
it does not affect the type chosen for the remaining paths. Braces do not
change this behavior:

```raven
let result = if ready value else { return fallback }
```

### Return rules

* A return value must be assignable to the enclosing callable's return type.
* A return without a value has the `unit` value.
* Statement-form `return` cannot be used directly in an inline expression
  context; doing so reports `RAV1900`. Use the expression form there.
* Return-type inference considers explicit returns and the outer tail
  expression. A tail expression nested in a statement block is not an implicit
  return from the enclosing callable.
* In a `unit`-returning callable, a non-`unit` outer tail value is discarded and
  reports `RAV9034`. Assign it to `_` when the discard is intentional, or
  change the return type when it is meant to be the result.

## Producing an iterator

A callable containing `yield` produces a lazily evaluated sequence. It must
return one of the following iterator shapes:

* `IEnumerable<T>` or `IEnumerator<T>`
* `IAsyncEnumerable<T>` or `IAsyncEnumerator<T>`
* Their non-generic counterparts

`yield value` and `yield return value` are equivalent. They publish the next
element, suspend execution, and resume immediately after the yield when the
consumer asks for another element.

```raven
func numbers(max: int) -> IEnumerable<int> {
    var current = 0
    while current < max {
        yield current
        current += 1
    }
}
```

The yielded value must be convertible to the iterator's element type. In an
expression position, a yield evaluates to `unit` when execution resumes.

`yield break` completes the iterator without producing another element. It is
valid in statement or expression position; as an expression, it never resumes
and therefore does not affect the surrounding type.

```raven
let item = match next() {
    Some(let item) => item
    None => yield break
}
```

An unannotated function expression containing `yield` infers
`IEnumerable<T>`. An async function expression containing `yield` infers
`IAsyncEnumerable<T>`.

## Cancellation in async iterators

An async iterator may receive the cancellation token passed by its consumer to
`GetAsyncEnumerator`. Mark the intended `CancellationToken` parameter with
`[EnumeratorCancellation]`:

```raven
import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*
import System.Threading.Tasks.*

async func numbers(
    [EnumeratorCancellation] cancellationToken: CancellationToken
) -> IAsyncEnumerable<int> {
    yield 1
    await Task.Delay(1000, cancellationToken)
    yield 2
}
```

Without the attribute, the token supplied during enumeration is ignored and
Raven warns when no `CancellationToken` parameter is marked. When the marked
parameter receives both a direct argument and an enumerator token, Raven
combines them so cancellation from either source is observed.
