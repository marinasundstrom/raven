# Proposal: Discriminated unions

> ⚠️ This proposal has **NOT** been implemented

Discriminated unions are value types that represent a fixed set of alternative shapes. Each alternative is modeled as a distinct nested struct type whose constructor is declared inline with the union definition. The compiler emits `TryGet*` helpers for each case and pattern matching is expressed in terms of these helpers, ensuring exhaustiveness.

## Syntax

```csharp
union Token {
    Identifier(text: string)
    Number(text: string)
    Unknown
}
```

* `union` introduces a discriminated union declaration.
* Each clause declares a case. A case name followed by a parameter list defines a constructor. A bare case name (e.g. `Unknown`) produces a parameterless constructor.
* The compiler emits one nested `struct` per case with a constructor and implicit conversion back to the outer union struct.
* The outer union struct exposes `TryGetIdentifier(ref Identifier?)`, `TryGetNumber(ref Number?)`, etc. to interrogate the active case.

### Case construction

Cases are constructed either by qualifying with the union name or by using the shorthand `.` syntax when a type can be inferred.

```csharp
let id1 = Token.Identifier("foo")
let id2 : Token = .Identifier("test")
```

Each case struct exposes the payload values via immutable fields or properties and defines an implicit conversion operator to the union. Assigning a case to the union converts implicitly and simulates a class-like closed hierarchy.

### Generics

Unions support type parameters declared on the `union`:

```csharp
union Result<T> {
    Ok(result: T)
    Error(message: string)
}
```

Generic cases capture the type parameter in their payloads. Each case struct may also declare its own type parameters to overload on type arguments, e.g. `public struct Ok<TResult>(TResult result)` when the payload needs a different generic parameter than the outer union. Construction and pattern matching behave the same as non-generic unions.

## Pattern matching

Union instances participate in the existing pattern matching syntax. Exhaustiveness checking treats unions as closed sets of cases. Example:

```csharp
func describe(token: Token) -> string {
    return token match {
        .Identifier(text) => "identifier ${text}"
        .Number(text) => "number ${text}"
        .Unknown => "unknown"
    }
}
```

Missing cases produce diagnostics similar to other pattern matching scenarios. Pattern matching against unions desugars into nested target-member patterns that call the generated `TryGet*` helpers. For each arm the compiler emits code equivalent to:

```csharp
if (token.TryGetIdentifier(ref Token.Identifier? case1)) { ... }
else if (token.TryGetNumber(ref Token.Number? case2)) { ... }
else if (token.TryGetUnknown(ref Token.Unknown? _)) { ... }
```

The leading `.` in the pattern is the target-member pattern syntax. When used inside a `match` expression or `is` pattern it tells the compiler to resolve the case against the current scrutinee.

## Runtime representation

* Each union is compiled into a sealed `struct` that stores an integer discriminator alongside an `object` payload reference. Case values are boxed before being stored in the payload slot.
* Each case becomes a nested `struct` containing only its payload and an implicit conversion back to the outer union.
* For reference types the language will eventually support closed class hierarchies. Until then unions remain structs.
* Helper methods such as `bool TryGetIdentifier(ref Identifier?)` or `bool TryGetOk(ref Ok<T>?)` are generated to enable low-level inspection and facilitate exhaustiveness analysis in contexts outside of pattern matching.

## Restrictions

* A `union` must declare at least one case. Empty unions produce diagnostic `RAV0401`.
* Case parameter lists may not contain `ref`, `in`, or `out` parameters because the payload is copied into the union's storage. Violations produce diagnostic `RAV0402`.

## Interop and member import

Cases can be imported similarly to enums:

```csharp
import Token.*;

let token = Identifier("bar");
```

Each case struct is a distinct type, so APIs may accept `Token.Identifier` directly when they are only interested in a single payload shape.

## Open questions

* How should unions interact with reference-type inheritance once closed hierarchies are available?
* Should cases support custom attributes or access modifiers?
* How do we best represent large payloads without copying when stored inside the union struct?
