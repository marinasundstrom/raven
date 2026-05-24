# Proposal: Null Case in Unions

> ℹ️ This proposal is under consideration

## Summary

Allow `null` to participate in Raven union declarations and Raven standard union
type syntax, building on the .NET 11-compatible union model described in
[Align Raven Unions with .NET 11](dotnet-11-union-alignment.md).

The source spelling:

```raven
string | Expression<() -> object> | null
```

means a standard union whose active contents may be `null`. For ordinary
nullable display and flow, that form is equivalent to:

```raven
(string | Expression<() -> object>)?
```

The implementation must treat `null` as a union content state, not as a
generated nominal case type.

## Dependency

This proposal depends on the .NET 11 union alignment proposal because null
support relies on the same core distinctions:

* carrier value versus active contents,
* class carrier nullability,
* struct carrier default/inactive state,
* `Value` as the interop content surface,
* pattern matching over unwrapped union contents.

Implementing null support before the compatibility model is settled risks
encoding the wrong distinction between nullable carriers and nullable contents.

## Motivation

Raven already uses `T1 | T2` as ergonomic source sugar for standard unions. APIs
often need a value that is one of several shapes, with `null` as a legitimate
default or absence state:

```raven
Label(text: string | Expression<() -> object> | null = null)
```

Without a first-class rule for `null` in union syntax, users must choose between
less precise overloads, ad hoc `Option<T>` wrapping, or a nullable annotation
whose relationship to the union is unclear. The language should make the
following forms converge:

```raven
text: string | Expression<() -> object> | null
text: (string | Expression<() -> object>)?
```

C# 15 union types in .NET 11 previews already allow nullable case types and
surface possible null contents through `IUnion.Value`. Raven should align with
that behavior while keeping `T1 | T2` as Raven syntax sugar for standard unions.

### Motivating example: JSON values

JSON has a real `null` value, so a Raven model for arbitrary JSON should be able
to include `null` as one of the value alternatives:

```raven
[JsonConverter(typeof(RavenParenthesizedUnionJsonConverterFactory))]
union JsonValue(string | double | bool | JsonObject | JsonValue[] | null)

[JsonConverter(typeof(JsonObjectConverter))]
record JsonObject(Properties: IDictionary<string, JsonValue>)
```

The object converter can then map `JsonValueKind.Null` directly to the union:

```raven
private static func ReadJsonValue(element: JsonElement, options: JsonSerializerOptions) -> JsonValue {
    if element.ValueKind is JsonValueKind.Null {
        return JsonValue(null)
    }

    if element.ValueKind is JsonValueKind.String {
        return JsonValue(element.GetString() ?? "")
    }

    if element.ValueKind is JsonValueKind.Number {
        return JsonValue(element.GetDouble())
    }

    // bool, array, and object handling omitted
}
```

This is not just nullable convenience. It models the JSON data domain: a property
value may be a string, number, Boolean, object, array, or JSON null. That makes
the null state an active union content state rather than a missing property,
failed conversion, or default carrier.

## Goals

* Allow `null` in Raven standard union syntax: `A | B | null`.
* Allow nullable member types in parenthesized union declarations:
  `union Value(string | null)`.
* Allow body-form union case payloads to be nullable:
  `case Text(value: string | null)`.
* Keep `T1 | T2` as Raven source sugar for standard union carriers.
* Let existing type and `null` patterns compose naturally over nullable union
  contents.
* Preserve the distinction between nullable contents, nullable carriers, and
  default/inactive `union struct` carrier state.
* Normalize and display nullable standard unions consistently.

## Non-goals

* Do not synthesize a nominal `Null` case type for `null`.
* Do not make `null` inhabit non-nullable union contents.
* Do not remove `T?` from Raven source syntax.
* Do not change `Option<T>` semantics.
* Do not define the .NET 11 union compatibility surface here; that belongs to
  the alignment proposal.

## Proposed design

### 1. Null in standard union syntax

The type syntax:

```raven
A | B | null
```

constructs the same standard union as:

```raven
(A | B)?
```

when the target representation is the ordinary standard union carrier. The
`null` entry is represented by the union content null state, not by a distinct
case slot.

Examples:

```raven
val text: string | Expression<() -> object> | null = null
val equivalent: (string | Expression<() -> object>)? = null
```

The standard union member set remains `string` and `Expression<() -> object>`.
The nullable bit applies to the union contents as a whole.

### 2. Null in parenthesized union declarations

Parenthesized union declarations may include nullable member types:

```raven
union Value(string | null)
union Input(string | Expression<() -> object> | null)
```

These declarations define a carrier whose active contents may be null. `null`
does not introduce an addressable case type, constructor, or `TryGetValue(out
Null)` member.

The member type list should preserve the non-null member types and record that
the union's contents are nullable:

```text
Value member types: string
Value content nullability: maybe-null
```

For declaration display, Raven may prefer either of these forms depending on
context:

```raven
union Value(string | null)
union Value(string?)
```

When displaying a standard union type rather than a declaration, prefer the
normalized nullable union form:

```raven
(string | Expression<() -> object>)?
```

### 3. Null in body-form union cases

Body-form union cases can already express payload nullability through their
parameter types. This proposal clarifies that `null` in such payloads follows
ordinary nullable type rules:

```raven
union Node {
    case Text(value: string | null)
    case Element(name: string)
}
```

The case remains `Text`; `null` is a possible payload value, not a separate union
case. Matching must distinguish:

```raven
node match {
    Text(null) => "empty"
    Text(val value) => value
    Element(val name) => name
}
```

### 4. Carrier nullability vs content nullability

Raven must keep these states separate in the semantic model:

```raven
U                 // carrier value is present
U?                // carrier value may be absent/null
U with null value // carrier is present; contents may be null
```

For class unions, `U?` means the carrier reference may be null. A null pattern
matches either a null carrier reference or a carrier whose `Value` is null,
matching C# custom class-union behavior.

For struct unions, `U?` is `System.Nullable<U>`. A null pattern on `U?` matches
an absent nullable wrapper. A null pattern on `U` matches a null `Value`, which
includes the default/inactive carrier state if that carrier reports `Value ==
null`.

This gives three meaningful states for `union struct`:

* nullable wrapper absent: `U?` has no value
* default/inactive carrier: `U` exists but `HasValue == false`
* active null contents: `U` exists, `HasValue == true`, and `Value == null`

If the current carrier representation cannot distinguish default/inactive from
active null contents, the semantic model should still reserve the distinction so
future storage improvements do not require a source-breaking change.

### 5. Pattern matching and exhaustiveness

Pattern matching over a union applies patterns to the union contents, consistent
with the .NET 11 alignment proposal:

```raven
value match {
    string s => s
    Expression<() -> object> expr => expr.ToString()
    null => ""
}
```

This is the main ergonomic benefit of the proposal: once `null` is represented
as part of the union's content null state, ordinary Raven patterns should compose
without adding a dedicated union-null pattern form. Type patterns continue to
match the active content type, and the existing `null` pattern covers null
contents. In other words, for most user code, pattern matching should "just
work":

```raven
func Render(value: string | Expression<() -> object> | null) -> string {
    return value match {
        string text => text
        Expression<() -> object> binding => binding.ToString()
        null => ""
    }
}
```

Rules:

* Type patterns match non-null contents of the requested member type.
* `null` matches null contents.
* For class carriers, `null` also matches a null carrier reference.
* Discard and `var` pattern behavior follows the decision made by the .NET 11
  alignment proposal.
* Exhaustiveness must include `null` when the union content null state is
  maybe-null.
* A catch-all arm after all non-null member types and `null` is redundant.

For:

```raven
union Value(string | null)
```

this is exhaustive:

```raven
value match {
    string s => s
    null => ""
}
```

and this is not:

```raven
value match {
    string s => s
}
```

### 6. Conversions

The `null` literal converts implicitly to any union whose contents are nullable:

```raven
val text: string | Expression<() -> object> | null = null
val value: Value = null
```

For class union carriers, assigning `null` to `U?` may mean a null carrier
reference. Assigning `null` to non-nullable `U` should construct or represent a
carrier with null contents only when the union content set permits null.

For struct union carriers, assigning `null` to non-nullable `U` should represent
active null contents only if the carrier can distinguish that state. Otherwise
the compiler should either:

* emit a diagnostic for non-nullable `U = null`, while allowing `U? = null`; or
* explicitly define that `null` maps to the default/inactive state.

The preferred long-term model is to distinguish active null contents from the
default/inactive state.

### 7. Value and HasValue

Raven's carrier surface should use the `Value` foundation from the .NET 11
alignment proposal:

```csharp
object? Value { get; }
```

`Value` is nullable when either:

* the carrier may be default/inactive and report no contents,
* the member set permits null contents,
* the carrier itself may be null and the access is lifted.

`HasValue` should continue to mean "an active member/case is present", not
"Value is non-null". This distinction matters for active null contents:

```text
default union struct: HasValue == false, Value == null
active null contents: HasValue == true, Value == null
```

If a current carrier cannot represent active null contents, `HasValue` behavior
must be documented as a temporary limitation.

### 8. JSON serialization

Parenthesized union JSON conversion should distinguish explicit null contents
from an uninitialized/default carrier when the carrier representation supports
that distinction.

For:

```raven
[JsonConverter(typeof(RavenParenthesizedUnionJsonConverterFactory))]
union JsonValue(string | double | bool | JsonObject | JsonValue[] | null)
```

the JSON token `null` should deserialize to active null contents:

```text
HasValue == true
Value == null
```

That differs from a default `union struct` carrier:

```text
HasValue == false
Value == null
```

Both may serialize as JSON `null`, but they are not the same semantic state.
If the current serializer or carrier cannot distinguish them, document that as a
temporary limitation and prefer preserving the distinction in the semantic model.

## Affected components

Likely implementation touchpoints:

* `src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/NameSyntaxParser.cs`
  for parsing `null` in type-union positions if it is not already accepted.
* `src/Raven.CodeAnalysis/TypeSymbolNormalization.cs` for preserving null
  content state while normalizing `A | B | null`.
* `src/Raven.CodeAnalysis/Symbols/Constructed/TypeUnionSymbol.cs` for standard
  union member sets and nullable content metadata.
* `src/Raven.CodeAnalysis/Symbols/Constructed/NullableTypeSymbol.cs` for
  canonical nullable union representation.
* `src/Raven.CodeAnalysis/SemanticModel.Binding.cs` for parenthesized union
  member binding and removal of diagnostics that reject nullable union members.
* `src/Raven.CodeAnalysis/Compilation.Conversions.cs` for `null` literal to
  nullable union conversion and union-to-nullable conversions.
* `src/Raven.CodeAnalysis/BoundTree/BoundIsPatternExpression.cs` and
  `src/Raven.CodeAnalysis/MatchExhaustivenessEvaluator.cs` for null patterns
  and exhaustive coverage.
* `src/Raven.CodeAnalysis/CodeGen/*` for active-null carrier construction,
  `Value`, `HasValue`, and conversion emission.
* `src/Raven.CodeAnalysis/Symbols/SymbolExtensions.cs` for display formatting,
  especially `(A | B)?`.
* `docs/compiler/diagnostics.md` and `DiagnosticDescriptors.xml` for retiring or
  narrowing `RAV0400` (`Nullable type not allowed in union`).

## Required changes

### Compiler behavior

* Stop rejecting nullable member types in unions as a blanket rule.
* Treat `null` in a standard union as nullable content state.
* Preserve the non-null member set for case matching and construction.
* Require a `null` match arm when content nullability is maybe-null.
* Ensure `Value` nullability follows the union content state.
* Keep `HasValue` independent from `Value is not null`.
* Model default `union struct` separately from active null contents where the
  representation allows it.

### Public API and symbols

* Expose enough semantic information for callers to distinguish:
  * nullable carrier,
  * nullable contents,
  * nullable case payload,
  * default/inactive struct carrier.
* Keep public semantic APIs Roslyn-like; consumers should ask for type info,
  symbol info, conversions, operations, and diagnostics rather than inspecting
  cache-specific union state.

### Documentation

* Update the language spec standard-union and union sections to document `null`
  content.
* Update type-system docs so `T | null` and `T?` remain consistent with the
  unified nullable type symbol proposal.
* Update diagnostics documentation for `RAV0400`.
* Add examples for parenthesized unions, body-form nullable payloads, and
  standard unions with null.

## Testing plan

Add focused tests in these areas:

* Syntax:
  * `string | null`
  * `string | Expression<() -> object> | null`
  * `union Value(string | null)`
* Semantics:
  * `null` literal conversion to nullable standard union.
  * `null` literal conversion to parenthesized union with nullable contents.
  * rejection of `null` where the union contents are not nullable.
  * `Value` property type/nullability.
  * `HasValue` behavior for struct unions.
* Patterns and exhaustiveness:
  * missing `null` arm reports a diagnostic.
  * `null` arm after non-null case types is exhaustive.
  * redundant catch-all after all member types plus `null`.
* Code generation/runtime:
  * default `union struct` behavior.
  * active null contents if supported by the representation.
  * class carrier null reference versus null contents.
  * JSON `null` round-trips through
    `union JsonValue(string | double | bool | JsonObject | JsonValue[] | null)`
    as active null contents.
* Interop:
  * consume a C# Preview 4 union with nullable case type through the aligned
    union surface.
  * expose Raven unions with a C#-recognizable `IUnion.Value` surface from the
    alignment proposal.

## Open questions

* Should Raven allow `val value: U = null` for `union struct U(string | null)`
  before the carrier can distinguish active null from default/inactive?
* Should display prefer `A | B | null` or `(A | B)?` in diagnostics?
* How many standard union arities should Raven keep once .NET's union runtime
  surface stabilizes?
* Should nullable standard union normalization produce `NullableTypeSymbol` over
  `TypeUnionSymbol`, or should `TypeUnionSymbol` carry a nullable-content bit and
  project as nullable only at display/conversion boundaries?

## Migration and compatibility

Existing source that used nullable member types in union declarations should
become valid. Existing diagnostics that relied on `RAV0400` will need updated
expectations.

Source that already wrote `(A | B)?` remains valid. The compiler should
canonicalize it with `A | B | null` for semantic equivalence, while preserving
the original syntax for syntax-tree consumers and formatting.

`Option<T>` remains the explicit algebraic absence model. `T | null` is the
.NET/nullability interop model. Conversions between `Option<T>` and nullable
unions should remain governed by the separate option-to-nullable conversion
proposal.
