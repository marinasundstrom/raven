# Proposal: Function Type Syntax Sugar

## Summary
Introduce a lightweight syntax for naming delegate types directly in function signatures and type annotations. The syntax uses tuple-like parameter lists followed by an arrow to denote the return type, e.g. `(int, int) -> int`, and desugars to the closest matching delegate type (`Func<int, int, int>` in this example). The compiler will map the syntactic form to an existing built-in delegate (such as `Func`/`Action`) or synthesize a delegate when no compatible declaration exists.

## Motivation
Developers working with higher-order functions in Raven routinely reference delegate types such as `Func<T1, T2, TResult>` or `Action<T1, T2>`. These names leak implementation details and add cognitive overhead when reading API signatures. By allowing a natural function-type notation in declarations, we can:

* Align Raven with modern functional languages that expose first-class function types.
* Improve readability for APIs that accept or return delegates.
* Simplify tooling scenarios (quick info, code completion) by surfacing the parameter and return types directly.

## Design

### Surface syntax

* The grammar admits function type expressions in places where a type can appear. The canonical form is `(T1, T2, ..., Tn) -> TRet`. A single-parameter function can omit the surrounding parentheses: `T -> TRet`.
* A `void` return is spelled `-> unit`, matching the existing `unit` type.
* Parameter modifiers (such as `ref` or `out`) are not supported in the initial iteration. These can be added later if scenarios demand them.
* Nested function types are permitted, enabling signatures like `(int, int) -> (int -> bool)`.

### Delegate resolution

1. **Existing delegates**: If the arity fits within the `Action`/`Func` families and all parameter/return types are compatible, the compiler lowers to those built-in types.
2. **User-defined delegates**: If a user-declared delegate matches the shape (parameter and return types) exactly, the compiler reuses it.
3. **Synthesis**: When no suitable delegate exists, the compiler synthesizes an internal delegate declaration with the appropriate signature. Synthesized delegates participate in metadata emission so that interop with .NET remains seamless.

### Metadata representation

Function type syntax is purely sugar. Emitted IL continues to use standard delegate types. Debugger displays and tooling should prefer the function notation when communicating with Raven source; when targeting IL metadata, the delegate type names remain visible.

### Examples

```raven
func aggregate(items: List<int>, reducer: (int, int) -> int, seed: int) -> int
func onClick(handler: () -> unit)
let compose: (int -> int, int -> int) -> (int -> int) = (f, g) => x => f(g(x))
```

## Open Questions

* Should nullability annotations appear inside the function type (e.g. `(int?, string) -> string?`)?
* Do we allow attributes or parameter names within the function type signature?
* How should variance be expressed for synthesized delegates?

These items can be resolved during implementation once the parser and binder work begins.
