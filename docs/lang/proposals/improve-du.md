# Proposal: Improve Implementation of Discriminated Unions and Align with Sealed Hierarchies

**Thesis**  
This proposal makes union cases first-class independent types while preserving a single carrier representation, enabling consistent construction, pattern matching, higher-order usage, and cross-language interop without relying on nested CLR case types.

---

## Motivation

This design mirrors established semantics from Rust and F#:

* Like **F#**, case names can be available unqualified when their containing union type is in scope, but ambiguity requires qualification or aliasing.
* Like **Rust**, pattern matching is centered around a finite set of cases; deconstruction patterns match a case name and bind its payload.
* Like both, the language avoids implicit inference in ambiguous contexts.

The proposal also aligns structurally with the direction of discriminated unions in **C# and .NET 11**:

* Union cases are represented as distinct shapes.
* Pattern matching is driven by a finite, known case set.
* Exhaustiveness analysis is integrated.
* Wrapping semantics are explicit and predictable.

The goal is practical interop:

* Raven can consume union-like types from C#.
* C# can consume Raven unions via normal metadata.
* Runtime representation remains conventional .NET.

---

## Alignment with C# Union Proposals

Raven aims to remain structurally compatible with the evolving C# discriminated union design while preserving Raven’s independent case type model.

The intent is minimal surface alignment so Raven unions:

* feel natural when consumed from C#,
* can be recognized by future .NET tooling,
* keep a conventional runtime representation.

### Attribute naming

* Prefer `[Union]` instead of `[DiscriminatedUnion]`.
* Place the attribute in `System.Runtime.CompilerServices`.
* **Question:** Can we drop `[DiscriminatedUnionCase]` attribute?

---

### Presence indicator (optional)

```csharp
bool HasValue;
```

Helps debugging, defensive checks, and default initialization scenarios.

Might become required by .NET 11.

---

### Case access pattern

Raven already supports the generic shape:

```csharp
bool TryGet[CaseType](out CaseType value)
```

Example:

```csharp
bool TryGetOk(out Ok<T> value)
```

For future C# alignment, Raven may switch to:

```csharp
bool TryGetValue(out Ok<T> value);
bool TryGetValue(out Error<E> value);
```

Overloads resolve by case type because cases are real standalone types.

Conceptually:

```csharp
[Union]
public struct Result<T, E> 
{
  // Implementation is omitted

  public bool TryGetValue(out Result.Ok<T> value) {}
  public bool TryGetValue(out Result.Error<E> value) {}

  public record struct Ok<T>(T value) {}
  public record struct Error<E>(E value) {}

  public static implicit operator Result<T, E>(Result.Ok<T> case) {}
  public static implicit operator Result<T, E>(Result.Error<T> case) {}
}
```

*We already have similar structure in Raven today so we can adapt*

---

### Potential common interface

The proposal for .NET 11 adds additional interfaces that may add union semantics to existing objects, such as results in ASP.NET Core.

Example:

```csharp
public interface IUnion
{
    object? Value { get; }
}
```

Optional. Enables reflection, tooling, serialization.

Decision deferred until the C# proposal stabilizes.

---

### Design constraint

Compatibility must not require nested CLR case types.

Raven instead uses:

* discriminator metadata
* independent case types
* carrier conversion

This keeps alignment without coupling to a specific C# implementation.

---

## 1. Design Objectives

### 1.1 Impact of the change

Primary impact is **binding + code generation**:

* Case resolution from the union case set
* Case → union conversion completion
* Carrier-only propagation (`?`) and conditional access (`?.`)

User-facing impact is minimal:

* Member syntax remains
* Tests largely unchanged
* Case types rarely used directly today

---

### Objectives

1. Make union case types independent and importable.
2. Support partial generic case types.
3. Preserve a single efficient carrier type.
4. Remove dependency on nested CLR case types.
5. Align pattern matching with sealed hierarchies.
6. Keep ergonomic member syntax as sugar.
7. Align scoping with F# rules.
8. Preserve propagation semantics.
9. Maintain .NET compatibility.

---

### 1.2 Practical Example: Higher-Order APIs

Independent case types enable natural lambda usage:

```raven
val result: Result<int, DomainError> =
    if cond
        arr.FirstOrError(() => Result.NotFound)
    else
        arr.FirstOrError(() => Result.Unexpected(Exception()))
```

And:

```raven
val result: Result<int, DomainError> =
    if cond
        arr.FirstOrError(() => NotFound)
    else
        arr.FirstOrError(() => Unexpected(Exception()))
```

Because:

* Case types are real values
* Target typing completes generics
* Conversion remains explicit

This improves:

* collection helpers
* async pipelines
* validation DSLs
* functional composition

---

## 2. Current Symbol Contracts

```csharp
public interface IDiscriminatedUnionSymbol : INamedTypeSymbol
{
    ImmutableArray<IDiscriminatedUnionCaseSymbol> Cases { get; }

    IFieldSymbol DiscriminatorField { get; }

    IFieldSymbol PayloadField { get; }
}

public interface IDiscriminatedUnionCaseSymbol : INamedTypeSymbol
{
    IDiscriminatedUnionSymbol Union { get; }

    ImmutableArray<IParameterSymbol> ConstructorParameters { get; }

    int Ordinal { get; }
}
```

Contracts remain valid.

What changes:

* emission
* lookup
* conversion behavior

---

## 3. Revised Symbol Model

Cases become standalone types linked to a carrier.

Enables:

* imports
* aliases
* construction
* pattern matching
* higher-order usage

---

## 4. Case Types

Emitted as independent generic types:

* `Ok<T>`
* `Error<E>`

Removes:

* outer generic capture
* nested coupling
* mobility limitations

---

## 5. Construction and Conversion

Case construction produces a case value.

Conversion to union completes generics using target type.

Keeps:

* explicit semantics
* ergonomic syntax
* predictable inference

---

## 6. Member Syntax (Sugar)

Member access resolves via case lookup.

`.Ok` remains shorthand.

No runtime nesting required.

---

## 7. Scoping and Injection

F#-style case injection.

Unqualified when unique.

Aliases bind directly to case types.

---

## 8. Pattern Matching Alignment

Already working:

* match expressions
* match statements
* member-style case patterns
* exhaustiveness

Currently limited:

* type patterns over case types
* standalone deconstruction patterns

Proposal removes those limitations.

Scrutinee-driven resolution remains unchanged.

---

## 9. Affected Features

Propagation and conditional access remain carrier-only.

Avoids ambiguity.

---

## 10. Debugging and ToString

Carrier owns formatting.

Uses:

```
Ordinal → CaseName
```

Stable with independent case types.

---

## 11. Implementation Steps

1. Refactor emission
2. Emit independent case types
3. Implement conversion completion
4. Update binding
5. Implement F# scoping
6. Verify pattern matching
7. Verify exhaustiveness
8. Update propagation rules
9. Tests
10. Docs
11. Cross-language validation

---

## Other Considerations

### Unit payload elision

Becomes payload-shape based, not syntax based.

`Ok` ≡ `Ok(())`

Applies everywhere:

* standalone
* aliases
* lambdas
* patterns

---

### Access to common members

Union carriers remain normal .NET types.

Object members and extension methods must remain accessible.

The proposal must not interfere with standard member resolution.