# Delegate declarations

Delegates give a name to a function signature. Use one when an API accepts,
stores, or publishes callbacks that should share a distinct named type.

```raven
delegate Transformer(value: int) -> string

class Pipeline {
    delegate Stage<T>(ref value: T) -> bool
}
```

A delegate value can refer to a compatible named function, method, or function
expression:

```raven
delegate Transformer(value: int) -> string

func format(value: int) -> string => "Value: $value"

let named: Transformer = format
let inline: Transformer = value => value.ToString()
```

If the return type is omitted, the delegate returns `unit`:

```raven
delegate Changed(value: int)
```

Delegate declarations may be generic and use the same constraints as other
type declarations. Parameters retain their `ref`, `out`, `in`, and `scoped`
modifiers.

At namespace scope, delegates default to `internal`; use `public` to export one
from the assembly. A delegate nested inside another type defaults to `public`.

## .NET representation

Delegates are emitted as sealed, abstract types derived from
`System.MulticastDelegate`. For every declaration, the compiler provides:

* A constructor `.ctor(object, IntPtr)` that binds a target and method pointer.
* An `Invoke` method whose parameters (including any `ref`/`out`/`in` modifiers) and return type match the declaration.

The generated `Invoke` method preserves parameter lifetime metadata. In
particular, `scoped` parameters emit `ScopedRefAttribute` when required,
including parameters whose generic types permit ref-like arguments.
