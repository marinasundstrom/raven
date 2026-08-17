# Pipe expressions

Pipelines make a series of transformations read from left to right. They are
useful when the result of one call becomes the input to the next.

The `|>` operator feeds the value on its left into the call on its right:

```raven
let result = 5 |> Square() |> AddOne()

let result = AddOne(Square(5))
```

The two expressions are equivalent. `|>` has the lowest precedence among
binary operators and associates from left to right, so each stage receives the
result of the previous one.

## Passing the piped value

For an ordinary function or static method, the piped value becomes the first
argument. Other written arguments follow it:

```raven
let result = 5 |> MathHelpers.Increment(2)

static class MathHelpers {
    static func Increment(x: int, amount: int) -> int {
        return x + amount
    }
}
```

If the target is an extension method, the value becomes its receiver, matching
ordinary `value.Extension()` syntax.

## Omitting an empty argument list

An empty argument list can be omitted from a pipeline stage. `value |> Method`
means `value |> Method()`:

```raven
func Inc(x: int, n: int = 1) -> int {
    return x + n
}

let a = 5 |> Inc
let b = 5 |> Inc()
let c = 5 |> Inc(2)
```

All three bindings above call `Inc`; `a` and `b` use the default value for `n`,
while `c` supplies `n` explicitly.

## Function-expression stages

Inline lambda targets also support implicit invocation. In `value |> x => ...`,
the compiler infers `x` from the piped value type and invokes the lambda with
the left-hand result:

```raven
let length =
    5
        |> x => x.ToString()
        |> text => text.Length
```

Parenthesized inline lambdas are also valid pipeline targets:

```raven
let name = user |> (u => u.Name)
```

When combining lambda targets with additional pipeline stages, parentheses make
the stage boundaries explicit:

```raven
let normalized =
    userOrError
        |> EnsureActive()
        |> (x => match x {
            Ok(let u) => u.Name
            Error(let e) => "ERR: " + e.ToString()
        })
        |> Normalize()
```

The pipe operator accepts an explicit or implicit call, an inline function
expression, or a writable property on its right. Any other target produces
diagnostic `RAV2800`.

## Property targets

If the pipeline targets a property, Raven assigns the left expression to that
property through its setter before producing the property's type as the result
of the pipe expression. Both instance and static properties are supported:

```raven
let container = Container()
let _ = 42 |> container.Value
let _ = 42 |> Container.Count

class Container {
    var Value: int { get; set; }
    static var Count: int { get; set; }
}
```

## Lookup, overloads, and generics

Pipeline targets participate in normal name lookup, so the operator can call
members brought into scope by `import` directives (including static imports) as
well as top-level `func` declarations. Because overload resolution still sees
the piped value as the first argument, generic methods can infer type arguments
from that value without any additional annotations.
