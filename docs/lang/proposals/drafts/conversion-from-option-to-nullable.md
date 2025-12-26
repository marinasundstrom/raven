# Proposal: Conversion from `Option<T>` to `T?`

## Summary

Allow `Option<T>` values to be converted to nullable `T?` so they can be passed directly to APIs that accept nullable value types. For example:

```raven
import System.Console.*

let val: Option<int> = .Some(42)
receivesNullable(val)

func receivesNullable(value?: int) -> () { }
```

desugars to a conversion from `Option<int>` to `int?` provided by a user- or library-defined conversion operator:

```raven
union Option<T> {
    Some(value: T)
    None
}

extension OptionExtensions<T> for Option<T> {
    public static explicit operator(self: Option<T>) -> T? {
        if self is .Some(value) {
            return value
        }
        return null
    }
}
```

The recommended baseline is **explicit** conversion to avoid surprising behavior in overload resolution and nullability-sensitive code. The compiler should suggest using that explicit conversion (with a cast) when type checking fails, similar to C#’s “An explicit conversion exists” diagnostics.

This proposal depends on separate features: user-defined conversion operators and static extensions.

## Status

* ✅ Syntax and binding for `explicit operator` / `implicit operator` declarations are implemented.
* ✅ Conversion operators are discovered during cast binding and implicit conversion checks used by overload resolution.
* ⏳ Static extension conversion operators remain blocked on the extension operator work.

## Motivation

`Option<T>` represents “value or no value” in a structured way. Nullable value types (`T?`) and nullable symbol declarations (`x? : T`) represent the same concept at the .NET/nullability level.

In practice:

* .NET APIs commonly use `T?` rather than `Option<T>`.
* Raven code that prefers `Option<T>` for expressiveness still needs to call `T?`-based APIs.
* Manually pattern matching `Option<T>` to produce a `T?` at every call site is verbose and error-prone.

A single, well-defined conversion from `Option<T>` to `T?`:

* Centralizes the semantics of “None → null, Some → value”.
* Makes interop with nullable-based APIs easier.
* Preserves Raven’s functional-style `Option<T>` at the source level while mapping smoothly to .NET’s nullable model.

We want that ergonomic conversion without silently hiding the fact that we’re collapsing a richer type (`Option<T>`) into a nullable scalar.

## Design

### Surface syntax

Library shape:

```raven
union Option<T> {
    Some(value: T)
    None
}

extension OptionExtensions<T> for Option<T> {
    public static explicit operator(self: Option<T>) -> T? {
        if self is .Some(value) {
            return value
        }
        return null
    }
}
```

Key points:

* Conversion operator is declared as a `static` member in an extension for `Option<T>`.
* The method name is the keyword pair `explicit operator` (later we may allow `implicit operator`), avoiding a new `conversion` keyword.
* `self` is the source value (`Option<T>`), whose type is implied by the `for Option<T>` on the extension.

Call sites:

```raven
let opt: Option<int> = .Some(42)

// Explicit cast
let v: int? = (int?)opt

// Passing to a nullable parameter
func receivesNullable(value?: int) -> () { }

receivesNullable((int?)opt)
```

The conversion is found and applied when the user writes an explicit cast to `T?`.

### Conversion semantics

The conversion `Option<T> -> T?` is straightforward:

* If the source is `.Some(value)`, return `value` (lifted to `T?`).
* If the source is `.None`, return `null`.

Pseudo-implementation:

```raven
public static explicit operator(self: Option<T>) -> T? {
    if self is .Some(value) {
        return value
    }
    return null
}
```

This matches the intuitive semantics: `Some` means “has value”; `None` becomes `null`.

### Implicit vs explicit

**Design choice in this proposal: use explicit conversion.**

#### Why not implicit (for now)?

An implicit conversion:

```raven
extension OptionExtensions<T> for Option<T> {
    public static implicit operator(self: Option<T>) -> T? { ... }
}
```

would let you write:

```raven
receivesNullable(opt)  // Ok via implicit Option<int> -> int?
```

but creates several risks:

* **Overload resolution surprises**
  Methods overloaded on `Option<T>` vs `T?` could pick the nullable overload unexpectedly due to the implicit conversion.

* **Loss of clarity**
  Call sites stop making it obvious that you’re collapsing an `Option<T>` into a nullable scalar. This is especially problematic in APIs where you *want* the caller to acknowledge that loss.

* **Nullability reasoning**
  Implicit conversion makes it easier to accidentally treat `Option<T>` as just “another nullable value”, undermining the semantic distinction that `Option` was meant to enforce.

Given those tradeoffs, it’s safer to start with **explicit** conversions only:

* Call sites must opt-in via `(T?)opt` or `opt as T?`.
* The compiler can assist with code-fixes and suggestions, so the friction remains low.

Implicit conversion can be revisited in a separate proposal once we have more experience with how `Option<T>` and `T?` interact in real code.

### Overload resolution and diagnostics

The compiler should treat this like C#:

* Only **implicit** conversions participate automatically in overload resolution.
* **Explicit** conversions are considered only when the user writes a cast or `as` expression.

When a user tries to pass `Option<T>` where `T?` is required and only an explicit conversion exists:

```raven
func receivesNullable(value?: int) -> () { }

let opt: Option<int> = .Some(42)
receivesNullable(opt) // error
```

The compiler should:

* Produce a diagnostic explaining that a conversion exists but must be explicit:

  > Cannot convert from `Option<int>` to `int?`. An explicit conversion exists: cast to `int?`.

* Offer a quick fix to insert the cast:

  ```raven
  receivesNullable((int?)opt)
  ```

This keeps the API discoverable and ergonomic while preserving explicitness.

### Requirements / Dependencies (separate proposals)

This proposal assumes the following language features, each of which deserves its own design doc:

1. **Conversion operators / operator overloading**

   * Syntax for `implicit operator` and `explicit operator` static methods.
   * New syntax node `ConversionOperatorDeclarationSyntax` (parallel to `OperatorDeclarationSyntax`).
   * Representation in the symbol model (`IsConversion`, `IsExplicit`, `IsImplicit` flags).
   * Integration into cast/as/overload resolution rules.

2. **Static extensions**

   * `extension X<T> for TargetType` form that allows defining *static* members associated with the target type.
   * Conversion operator lookup must consider both:

     * Methods declared directly on the type.
     * Static extension members for that type.
   * Static extension members participate in `TargetType.Member` lookup and are also imported via `import TargetType.*`.

Without those, this proposal is not implementable, but it serves as a concrete motivating use case for them.

## Implementation notes

* **Binder & semantic model**

  * Extend cast expression binding to search for user-defined conversions between source and target types, including those declared in static extensions.
  * Only implicit conversions participate automatically in assignment and method call applicability; explicit conversions are used only when the source code asks for them.

* **Nullability integration**

  * The target `T?` uses Raven’s existing nullable machinery (identifier-level nullability, symbol `IsNullable`, etc.).
  * The conversion is just another way to produce a nullable value.

* **Tooling**

  * Quick Info on `Option<T>` could mention “explicit conversion to `T?` available.”
  * Diagnostics should guide the user to the appropriate cast or `as` expression when a nullable is required.
