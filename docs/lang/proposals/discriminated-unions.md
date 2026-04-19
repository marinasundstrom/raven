# Proposal: Discriminated unions

> ✅ The syntax described here is implemented in the parser and semantic model
> (including member pattern binding and exhaustiveness). Code generation lowers
> case construction to union constructors (`Union(Union_Case(...))`)
> and emits `TryGet*` helpers for the union cases.

Discriminated unions are value types that represent a fixed set of alternative shapes. Each alternative is modeled as a distinct nested struct type whose constructor is declared inline with the union definition. The compiler emits `TryGet*` helpers for each case and pattern matching is expressed in terms of these helpers, ensuring exhaustiveness.

## Syntax

```csharp
union Token {
    case Identifier(text: string)
    case Number(text: string)
    case Unknown
}
```

* `union` introduces a discriminated union declaration.
* Each clause declares a case and is prefixed with the `case` keyword. A case name followed by a parameter list defines a constructor. A bare case name (e.g. `case Unknown`) produces a parameterless constructor.
* The compiler emits one nested `struct` per case with a constructor.
* The outer union struct exposes one constructor per case (`.ctor(CaseType)`), used for case-to-union conversion.
* The outer union struct exposes `TryGetIdentifier(ref Identifier?)`, `TryGetNumber(ref Number?)`, etc. to interrogate the active case.

### Case construction

Cases are constructed either by qualifying with the union name or by using the shorthand `.` syntax when a type can be inferred.

```csharp
let id1 = Token.Identifier("foo")
let id2 : Token = .Identifier("test")
```

Each case struct exposes the payload values via immutable fields or properties. Assigning a case to the union still converts implicitly at the language level, but the conversion is implemented by calling the matching union constructor (`.ctor(CaseType)`), not by synthesizing `op_Implicit`.

Invariant:

* Case constructors stay independent case-type constructors.
* Union wrapping is explicit via union constructors (`Union(caseValue)`).
* Case compatibility with unions is handled by conversion rules, not by
  rebinding constructors to target union types.

### Generics

Unions support type parameters declared on the `union`:

```csharp
union Result<T> {
    case Ok(result: T)
    case Error(message: string)
}
```

Generic cases capture the type parameter in their payloads. Each case struct may also declare its own type parameters to overload on type arguments, e.g. `public struct Ok<TResult>(TResult result)` when the payload needs a different generic parameter than the outer union. Construction and pattern matching behave the same as non-generic unions.

See `samples/discriminated-unions.rav` for a runnable example that covers both non-generic (`Token`) and generic (`Result<T>`) unions.

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

**Note:** The arms (for example, `.Identifier(text)`) use the `MemberPattern`
syntax from _grammar.ebnf_: a leading `.` resolves the case against the current
scrutinee, and an optional qualifier (such as `Token.Identifier`) forces lookup
against a specific union type.

Member patterns accept the same payload shape declared on the case. A
parameterless case may be matched with either `.Unknown` or `.Unknown()`, while
payload-bearing cases unpack each element positionally:

```csharp
let token = Token.Identifier("foo")

let description = token match {
    .Identifier(let text) => text,
    Token.Unknown() => "missing",
}
```

Adding an explicit qualifier bypasses the scrutinee's static type and is useful
when the union flows in as an interface or object. The parser treats the
qualifier as part of the member path; binding validates that the qualifier and
case belong to the same union and enforces payload arity at the pattern site.

```csharp
union Result<T> {
    case Ok(value: T)
    case Error(message: string)
}

func format(result: Result<int, string>) -> string {
    return result match {
        .Ok(let payload) => "ok ${payload}"
        .Error(let message) => "error ${message}"
    }
}
```

Case payload identifiers may omit `let`/`var`; a bare name such as `.Ok(payload)`
binds an immutable local typed to the corresponding payload parameter.

Guards participate only when they are known to succeed. In a `match` over
`Result<int, string>`, `.Ok(payload) when payload > 1` does not satisfy exhaustiveness
because the guard can reject values; add another `Ok` arm or `_` to cover the
remaining inputs. Case patterns desugar to calls to the generated `TryGet*`
helpers, so nested payload patterns see the same properties exposed by the
case struct.

Missing cases produce diagnostics similar to other pattern matching scenarios. Pattern matching against unions desugars into nested target-member patterns that call the generated `TryGet*` helpers. For each arm the compiler emits code equivalent to:

```csharp
if (token.TryGetIdentifier(ref Token.Identifier? case1)) { ... }
else if (token.TryGetNumber(ref Token.Number? case2)) { ... }
else if (token.TryGetUnknown(ref Token.Unknown? _)) { ... }
```

The leading `.` in the pattern is the target-member pattern syntax. When used inside a `match` expression or `is` pattern it tells the compiler to resolve the case against the current scrutinee.

## Compiler API integration

Surface discriminated unions through the symbol model so semantic consumers can
reason about unions declared in source or supplied via metadata:

* `IsDiscriminatedUnion` returns `true` for the union struct itself and for any
  constructed versions of it. When importing existing metadata, recognise the
  synthesized `[DiscriminatedUnion]` attribute as the signal.
* `IsDiscriminatedUnionCase` returns `true` for nested case structs and their
  constructed forms. Metadata cases can be detected via the
  `[DiscriminatedUnionCase]` attribute.
* `UnderlyingDiscriminatedUnionType` (renamed from
  `UnderlyingDiscriminatedUnion`) should report the outer union type. For
  case structs it returns the containing union, and for the union type itself
  it simply returns `this`. Attribute arguments on metadata-backed cases expose
  the same relationship.

## Runtime representation

* Each union is compiled into a sealed `struct` that stores an integer discriminator alongside an `object` payload reference. Case values are boxed before being stored in the payload slot.
* The outer struct is annotated with a synthesized `[DiscriminatedUnion]` attribute so metadata consumers can distinguish union declarations from ordinary structs. Case structs are not annotated.
* Each case becomes a nested `struct` containing only its payload.
* The outer union struct contains case-wrapper constructors (`.ctor(CaseType)`), and no union-specific implicit conversion operators are synthesized by default.
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
