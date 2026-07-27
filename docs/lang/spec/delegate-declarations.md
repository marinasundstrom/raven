# Delegate declarations

Delegates give a name to a particular kind of function. Use them when an API
accepts or stores callbacks with a shared signature. The declaration lists the
parameters and optional return type, and the compiler creates the corresponding
delegate members.

```raven
delegate Transformer(value: int) -> string

class Pipeline {
    delegate Stage<T>(ref value: T) -> bool
}
```

Delegates are emitted as sealed, abstract types that inherit from `System.MulticastDelegate`. For every delegate declaration, the compiler synthesizes:

* A constructor `.ctor(object, IntPtr)` that binds a target and method pointer.
* An `Invoke` method whose parameters (including any `ref`/`out`/`in` modifiers) and return type match the declaration.

The synthesized `Invoke` method also preserves parameter lifetime metadata.
In particular, `scoped` parameters emit `ScopedRefAttribute` when required,
including parameters that use generic types permitting ref-like arguments.

If the return type clause is omitted, the delegate returns `unit`. Delegate
declarations support generic type parameters and constraints using the same
`where` clause rules as other type declarations. Accessibility follows the
standard type rules: namespace-level delegates default to `internal` and
require `public` to be exported from the assembly, while nested delegates
default to `public`.
