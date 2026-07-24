# Pipe expressions

Pipelines make a series of transformations read from left to right. They are
useful when the result of one call becomes the input to the next.

Raven supports a pipeline form that feeds the left-hand value into a call on
the right. The operator has the lowest precedence among binary operators and
associates left-to-right, so a chain such as `source |> First() |> Second()`
evaluates `source`, passes it to `First`, then pipes the result into `Second`.

```raven
val result = 5 |> Square() |> AddOne()

val result = AddOne(Square(5))
```

When the pipeline targets an invocation, the syntax mirrors a regular call:

```raven
val result = 5 |> MathHelpers.Increment(2)

static class MathHelpers {
    static func Increment(x: int, amount: int) -> int {
        return x + amount
    }
}
```

Pipe chains also allow an implicit invocation form for method targets with no
explicit argument list. In that form, `value |> Method` is treated as
`value |> Method()`:

```raven
func Inc(x: int, n: int = 1) -> int {
    return x + n
}

val a = 5 |> Inc
val b = 5 |> Inc()
val c = 5 |> Inc(2)
```

All three bindings above call `Inc`; `a` and `b` use the default value for `n`,
while `c` supplies `n` explicitly.

Inline lambda targets also support implicit invocation. In `value |> x => ...`,
the compiler infers `x` from the piped value type and invokes the lambda with
the left-hand result:

```raven
val length =
    5
        |> x => x.ToString()
        |> text => text.Length
```

Parenthesized inline lambdas are also valid pipeline targets:

```raven
val name = user |> (u => u.Name)
```

When combining lambda targets with additional pipeline stages, parentheses make
the stage boundaries explicit:

```raven
val normalized =
    userOrError
        |> EnsureActive()
        |> (x => match x {
            Ok(let u) => u.Name
            Error(let e) => "ERR: " + e.ToString()
        })
        |> Normalize()
```

The pipe operator accepts an invocation target (explicit or implicit) or a
property access with a setter on the right-hand side. If the syntax does not
form either shape, diagnostic `RAV2800` is issued.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2690-L2766】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L19-L23】

If the pipeline targets a property, Raven assigns the left expression to that
property through its setter before producing the property's type as the result
of the pipe expression. Both instance and static properties are supported:

```raven
val container = Container()
val _ = 42 |> container.Value
val _ = 42 |> Container.Count

class Container {
    var Value: int { get; set; }
    static var Count: int { get; set; }
}
```

When the invocation resolves to an extension method, the left expression becomes
the extension receiver, mirroring `value.Extension()` syntax. Otherwise the
compiler prepends the piped value as the first argument before overload
resolution runs, so ordinary static helpers that expect a leading value parameter
remain callable through pipelines.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2698-L2768】

Pipeline targets participate in normal name lookup, so the operator can call
members brought into scope by `import` directives (including static imports) as
well as top-level `func` declarations. Because overload resolution still sees
the piped value as the first argument, generic methods can infer type arguments
from that value without any additional annotations.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2724-L2768】【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L1396-L1507】
