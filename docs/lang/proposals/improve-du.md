# Proposal: Improve Implementation of Discriminated Unions and Align with Sealed Hierarchies

**Thesis**
This proposal makes union cases first-class independent types while preserving a single carrier representation, enabling consistent construction, pattern matching, higher-order usage, and cross-language interop without relying on nested CLR case types.

---

## Motivation

This design mirrors established semantics from Rust and F#:

* Like **F#**, case names can be available unqualified when their containing union type is in scope, but ambiguity requires qualification or aliasing.
* Like **Rust**, pattern matching is centered around a finite set of cases; deconstruction patterns match a case name and bind its payload.
* Like both, the language avoids implicit inference in ambiguous contexts.

The proposal also aligns structurally with the direction of discriminated unions in **C# and .NET**:

* Union cases are represented as distinct shapes.
* Pattern matching is driven by a finite, known case set.
* Exhaustiveness analysis is integrated.
* Wrapping semantics are explicit and predictable.

The goal is practical interop:

* Raven can consume union-like types from C# (as they emerge).
* C# can consume Raven unions via normal metadata.
* Runtime representation remains conventional .NET.

The C# union proposal can be read here: [https://github.com/dotnet/csharplang/blob/main/proposals/unions.md](https://github.com/dotnet/csharplang/blob/main/proposals/unions.md)

---

## Alignment with C# Union Proposals

Raven aims to remain structurally compatible with the evolving C# union design while preserving Raven’s independent case type model.

The intent is minimal surface alignment so Raven unions:

* feel natural when consumed from C#,
* can be recognized by future tooling,
* keep a conventional runtime representation.

### Attribute naming

* Prefer `[Union]` instead of `[DiscriminatedUnion]`.
* Place the attribute in `System.Runtime.CompilerServices`.
* Question: can we drop our non-standard `[DiscriminatedUnionCase]` attribute?

```csharp
namespace System.Runtime.CompilerServices
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = false)]
    public sealed class UnionAttribute : Attribute { }
}
```

Notes:

* If Raven continues to emit additional metadata attributes for its own compiler, they should be optional and not required for basic union consumption from other languages.

---

### Presence indicator (optional)

```csharp
bool HasValue;
```

Useful for debugging, defensive checks, and default initialization scenarios.

Whether this is expected by future C# tooling is TBD; Raven can expose it without committing to a specific external contract.

---

### Case access pattern

Raven already supports a non-boxing access pattern:

```csharp
bool TryGet[CaseName](out CaseType value)
```

Example:

```csharp
bool TryGetOk(out Ok<T> value)
bool TryGetError(out Error<E> value);
```

For alignment with potential future C# patterns, Raven should rename them to the form:

```csharp
bool TryGetValue(out Ok<T> value);
bool TryGetValue(out Error<E> value);
```

Overloads resolve by case type because cases are standalone types.

Conceptually:

```csharp
[Union]
public readonly record Result<T, E>
{
    // storage omitted

    public bool HasValue { get; }

    public bool TryGetValue(out Ok<T> value) => /* ... */;
    public bool TryGetValue(out Error<E> value) => /* ... */;

    public static implicit operator Result<T, E>(Ok<T> @case) => /* ... */;
    public static implicit operator Result<T, E>(Error<E> @case) => /* ... */;
}

// Case types no longer nested
public readonly record struct Ok<T>(T Value);
public readonly record struct Error<E>(E Value);
```

Notes:

* The example uses nested types only as a sketch. The design constraint below forbids relying on nested CLR case types for compatibility.
* Raven’s actual emission will use independent case types.
* .NET might use the name `Failure` instead of `Error`. But we will wait for their implementation, and use our for the time being.

---

### Optional common surface (deferred)

The C# proposal discusses a `Value` surface for applying patterns to “union contents”:

```csharp
object? Value { get; }
```

Raven can expose a similar surface for interop, reflection, tooling, and serialization, but the exact shape should be decided only when the external proposal stabilizes.

---

### Design constraint

Compatibility must not require nested CLR case types.

Raven instead uses:

* discriminator + payload storage on a carrier
* independent case types
* carrier conversion

This keeps alignment without coupling Raven to a specific C# implementation strategy.

---

## 1. Design Objectives

### 1.1 Impact of the change

Primary impact is **binding + code generation**:

* Case resolution from the union case set
* Case → union conversion completion
* Carrier-only propagation (`?`) and conditional access (`?.`)
* Simpler member binding: `Result.Ok` becomes a case lookup, not nested-type resolution

User-facing impact is minimal:

* Member syntax remains
* Most tests remain unchanged
* Direct usage of case types becomes more useful but remains optional

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

Independent case types enable natural higher-order usage:

```raven
val result: Result<int, DomainError> =
    if cond
        arr.FirstOrError(() => Result.NotFound)
    else
        arr.FirstOrError(() => Result.Unexpected(Exception()))
```

And with F#-style case injection:

```raven
val result: Result<int, DomainError> =
    if cond
        arr.FirstOrError(() => NotFound)
    else
        arr.FirstOrError(() => Unexpected(Exception()))
```

Because:

* Cases are real values
* Target typing completes the carrier type arguments
* Conversion remains explicit and predictable

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

Conversion to carrier completes generics using target type.

Keeps:

* explicit semantics
* ergonomic syntax
* predictable inference

---

## 6. Member Syntax (Sugar)

Member access resolves via case lookup on the carrier’s declared case set.

`.Ok` remains shorthand in target-typed contexts.

This simplifies member binding because it no longer depends on nested CLR case types or constructed outer generic types.

---

## 7. Scoping and Injection

F#-style case injection.

Unqualified usage is allowed when unique.

Aliases bind directly to case *types*:

```raven
alias ResultOk = Result.Ok
```

---

## 8. Pattern Matching Alignment

Already supported:

* match expressions
* match statements
* member-style case patterns
* exhaustiveness

Currently limited:

* type patterns over case types
* standalone deconstruction patterns

This proposal removes those limitations while preserving scrutinee-driven case resolution (case names are resolved against the scrutinee’s known cases).

---

## 9. Affected Features

Propagation and conditional access remain carrier-only:

* `<expr>?`
* `<expr>?.<access>`

This avoids ambiguity and prevents “case values” from behaving like carriers.

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
4. Update binding (including simplified member binding)
5. Implement F# scoping + ambiguity rules
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

Simplifies:

```raven
func test() -> Result<(), MyError> {
  return Ok // Ok(())
}
```

*Raven supports invocations without parameter list*

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

---

### Tighten member-binding invocations

Ensure member-binding remains correct in:

* invoking methods
* initializing nested classes and other members adjacent to cases
* contexts where target typing can infer the carrier
