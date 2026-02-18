# Proposal: Improve Implementation of Discriminated Unions and Align with Sealed Hierarchies

---

## Motivation: Familiar Semantics (Rust + F#) and .NET Compatibility

This design intentionally mirrors established semantics from Rust and F#:

* Like **F#**, case names can be available unqualified when their containing union type is in scope, but ambiguity requires qualification or aliasing.
* Like **Rust**, pattern matching is centered around a finite set of cases, and deconstruction patterns match a case name and bind its payload.
* Like both, the language avoids implicit inference in ambiguous contexts.

Additionally, this proposal aligns structurally with the direction of discriminated unions in **C# and .NET**:

* Union cases are represented as distinct shapes.
* Pattern matching is driven by a finite known case set.
* Exhaustiveness analysis is integrated.
* Wrapping semantics are explicit and predictable.

The goal is practical interop:

* Raven can consume union-like types from C#.
* C# can consume Raven unions via normal metadata.
* Runtime representation remains conventional .NET.

---

## 1. Goals

1. Make union case types independent and importable.
2. Support partial generic case types (e.g. `Ok<T>`, `Error<E>` for `Result<T,E>`).
3. Preserve a single efficient union wrapper type.
4. Remove dependency on nested CLR case types and member-binding semantics.
5. Align pattern matching behavior between:

   * Discriminated unions
   * Sealed hierarchies
6. Keep ergonomic syntax (`Result.Ok`, `.Ok`) as sugar only.
7. Align scoping with F# rules: unqualified names must be unique; otherwise qualify or alias.
8. Ensure propagation (`?`) and carrier conditional access (`?.`) remain coherent.
9. Maintain compatibility with .NET metadata and C# consumption patterns.

---

## 2. Current Symbol Contracts

As implemented today:

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

These contracts remain valid.

What changes is primarily:

* how case types are emitted (no longer nested CLR types),
* how lookup works (case lookup from the union symbol),
* and how conversions/patterns behave with partial generic case types.

---

## 3. Revised Symbol Model

### 3.1 `IDiscriminatedUnionSymbol`

Represents the union wrapper type.

Still provides:

* `Cases`
* `DiscriminatorField`
* `PayloadField`

No public contract changes are required.

---

### 3.2 `IDiscriminatedUnionCaseSymbol` (Updated Semantics)

A case symbol represents a **standalone case type**:

* cases are independent named types (not nested CLR types),
* cases may have **partial generic parameter lists** relative to the union,
* cases remain linked to the union via `Union`,
* constructor shape is exposed via `ConstructorParameters`,
* `Ordinal` defines the tag order.

This enables:

* importing case types into scope,
* constructing case values directly,
* converting them into union values in typed contexts,
* matching cases consistently in patterns.

---

## 4. Case Types (Independent)

Cases are modeled/emitted as independent generic types (not nested CLR types):

* `Ok<T>`
* `Error<E>`

They can be imported and used directly as normal types.

This removes:

* outer generic capture,
* constructed outer type dependency,
* mobility limitations in symbols and binding.

---

## 5. Construction and Conversion

### 5.1 Case Construction

```raven
val x: Ok<int> = Ok(3)
```

Produces a value of type `Ok<int>` (no union wrapping).

---

### 5.2 Conversion to Union (Completion)

Wrapping occurs via conversion when a union target type is available:

```raven
val r: Result<int, CustomError> = Ok(3)
```

Conceptual rule:

* A case value can be converted to its containing union type when the target union type supplies the full union generic arguments.
* Any union type arguments not present on the case type are taken from the target union type.

If no union target exists:

```raven
val r = Ok(3) // error
```

Diagnostic:

> Cannot determine missing union type arguments. Provide a target union type.

---

## 6. Member-Binding Syntax (Sugar Only)

Preserve:

```raven
Result.Ok(3)
.Ok(3)
```

Semantics:

* `Result.Ok` resolves by case lookup in `IDiscriminatedUnionSymbol.Cases`.
* It yields the associated case type symbol (`Ok<T>`), not a nested type.
* `.Ok` resolves using target-type-driven case lookup.

This keeps ergonomics while decoupling from CLR nesting.

---

## 7. Scoping, Imports, and Aliases (F#-style)

### 7.1 Automatic Case Injection

Case names may be automatically added as eligible unqualified candidates when their containing union type is in scope (F#-like “opened cases”).

This also matches expectations from sealed hierarchies where types live next to each other.

---

### 7.2 Ambiguity Rule (Hard Requirement)

An unqualified case type name (e.g. `Ok`) is valid only if it resolves to exactly one candidate.

If multiple candidates exist:

* report ambiguity,
* require qualification or aliasing.

Example:

```raven
Ok(3) // error if multiple Ok cases are in scope
```

Fix:

```raven
Result.Ok(3)
OtherResult.Ok(3)
```

---

### 7.3 Aliases bind to type symbols

Alias binds to a type symbol, not an expression:

```raven
alias ResultOk = Result.Ok
```

* RHS is resolved as a type path.
* `Result.Ok` uses union case lookup and binds to the case type symbol `Ok<T>`.

Alias can be used:

```raven
val r: Result<int, CustomError> = ResultOk(3)
val x: ResultOk<int> = ResultOk(3)
```

---

## 8. Pattern Matching Alignment (Conceptual)

Pattern matching for discriminated unions and sealed hierarchies should follow the same conceptual rules:

* The scrutinee type provides a finite, compiler-known set of cases (union cases, or sealed derived types).
* A deconstruction pattern head (e.g. `Ok(...)`, `Dog(...)`) resolves against that finite set for the scrutinee type.
* If the head name matches a known case of the scrutinee type:

  * the pattern tests whether the scrutinee is that case,
  * and binds the payload to the inner subpatterns.
* Exhaustiveness analysis is performed over that finite set:

  * missing cases are reported,
  * redundant wildcard arms can be reported as unreachable.

Raven calls these **deconstruction patterns**:

```raven
match r {
  Ok(val x) => ...
  Error(val e) => ...
}
```

Import/type scope does not affect what `Ok(...)` means inside a match on a union/sealed scrutinee; it is resolved using the scrutinee’s known cases.

---

## 9. Affected Features

### 9.1 Propagation Pattern (`<expr>?`)

Applies only to carrier types (e.g. `Result<T,E>` / `IDiscriminatedUnionSymbol`), not case types.

### 9.2 Carrier Conditional Access (`<expr>?.<access>`)

Applies only to carrier types, not case types.

This preserves clarity and avoids semantic ambiguity.

---

## 10. Implementation Steps

1. Refactor DU implementation so `IDiscriminatedUnionCaseSymbol` represents independent case types (not nested CLR types).
2. Emit case types as independent partial generic types (`Ok<T>`, `Error<E>`).
3. Implement conversion completion for case → union based on the target union type.
4. Update member-access binding to use case lookup (`IDiscriminatedUnionSymbol.Cases`) for `Result.Ok` and target-typed `.Ok`.
5. Implement F#-style scoping:

   * optional automatic case injection,
   * strict ambiguity diagnostics,
   * qualification and aliases.
6. Align pattern binding rules for unions and sealed hierarchies as described (scrutinee-driven case resolution, deconstruction payload binding).
7. Align exhaustiveness analysis for unions and sealed hierarchies over the finite known set of cases.
8. Update `?` and `?.` binding rules to require carrier types.
9. Add comprehensive tests.
10. Update documentation.
11. Validate cross-language consumption scenarios with C# unions.

---
