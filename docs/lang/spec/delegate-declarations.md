# Delegate declarations

Delegate declarations introduce named callable types. The declaration lists a parameter signature and optional return type, and the compiler synthesizes the corresponding delegate members.

```raven
delegate Transformer(value: int) -> string

class Pipeline {
    delegate Stage<T>(ref value: T) -> bool
}
```

Delegates are emitted as sealed, abstract types that inherit from `System.MulticastDelegate`. For every delegate declaration, the compiler synthesizes:

* A constructor `.ctor(object, IntPtr)` that binds a target and method pointer.
* An `Invoke` method whose parameters (including any `ref`/`out`/`in` modifiers) and return type match the declaration.

If the return type clause is omitted, the delegate returns `unit`. Delegate declarations support generic type parameters and constraints using the same `where` clause rules as other type declarations. Accessibility defaults follow the standard type rules: top-level delegates are `public` unless marked `internal`, and an explicit top-level `public` modifier is redundant and diagnosed. Nested delegates default to `private` except when declared inside interfaces.
