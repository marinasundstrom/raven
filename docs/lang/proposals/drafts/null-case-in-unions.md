# Proposal: Null Case in Unions

> ℹ️ This proposal is under consideration

## Summary

Allow `null` to participate in parenthesized Raven union declarations, building
on the .NET 11-compatible union model described in
[Align Raven Unions with .NET 11](dotnet-11-union-alignment.md).

The implemented source spelling:

```raven
union Value(string | Expression<() -> object> | null)
```

means a union carrier whose active contents may be `null`. The implementation
treats `null` as union content nullability metadata, not as a generated nominal
case type or a generally valid type name.

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

Raven already uses parenthesized union declarations as ergonomic source sugar
for standard union carriers. APIs often need a value that is one of several
shapes, with `null` as a legitimate default or absence state:

```raven
union LabelText(string | Expression<() -> object> | null)
```

Without a first-class rule for `null` in union declaration syntax, users must
choose between less precise overloads, ad hoc `Option<T>` wrapping, or a nullable
annotation whose relationship to the union is unclear.

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

* Allow nullable member types in parenthesized union declarations:
  `union Value(string | null)`.
* Keep `T1 | T2` as Raven source sugar for standard union carriers.
* Let existing type and `null` patterns compose naturally over nullable union
  contents.
* Preserve the distinction between nullable contents, nullable carriers, and
  default/inactive `union struct` carrier state.
* Normalize and display nullable standard unions consistently.

## Non-goals

* Do not synthesize a nominal `Null` case type for `null`.
* Do not make `null` inhabit non-nullable union contents.
* Do not introduce a second general nullable type spelling; `T?` remains the
  nullable type annotation spelling.
* Do not remove `T?` from Raven source syntax.
* Do not change `Option<T>` semantics.
* Do not define the .NET 11 union compatibility surface here; that belongs to
  the alignment proposal.

## Proposed design

### 1. Deferred: null in standard union type syntax

The general type syntax:

```raven
A | B | null
```

could construct the same standard union as:

```raven
(A | B)?
```

when the target representation is the ordinary standard union carrier. However,
that spelling is intentionally not part of the current implementation because it
looks like a second way to write nullable types such as `int?`. For now, `T?`
remains the nullable type syntax and `null` is accepted only in parenthesized
union declaration member lists.

Deferred example:

```raven
val text: string | Expression<() -> object> | null = null // not implemented
```

If this form is ever enabled, the standard union member set should remain
`string` and `Expression<() -> object>`, with a nullable-content bit applying to
the union contents as a whole.

### 2. Null in parenthesized union declarations

Parenthesized union declarations may include nullable member types:

```raven
union Value(string | null)
union Input(string | Expression<() -> object> | null)
```

These declarations define a carrier whose active contents may be null. `null`
does not introduce an addressable case type, constructor, or `TryGetValue(out
Null)` member.

Current implementation scope: the `null` spelling is accepted as syntactic
sugar only in parenthesized union declaration member lists. It marks the union's
contents as maybe-null and is not a generally valid Raven type annotation.

The Raven source-domain model preserves the non-null member cases and records
that the union's contents are nullable:

```text
Value member types: string
Value content nullability: maybe-null
```

Implementation note: source and metadata union symbols may expose this state as
derived `ContentMayBeNull`, but it is not independent metadata. For C# union
compatibility, synthesized parenthesized unions lower explicit `| null` to
nullable-capable public case constructor parameter types for the listed members
and do not emit a synthetic null constructor. Raven matching still treats the
domain as the non-null member cases plus a distinct `null` branch.

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

Body-form union cases can express payload nullability through ordinary nullable
type syntax. The `null` type spelling is not enabled in this position by the
current implementation:

```raven
union Node {
    case Text(value: string?)
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

The public C#-compatible surface exposes these meaningful null states for
`union struct`:

* nullable wrapper absent: `U?` has no value
* null contents/default carrier: `U` exists, `Value == null`, and
  `HasValue == false`

A future Raven-specific tagged representation may distinguish active null
contents from a default carrier internally, but that distinction is not exposed
through the C# union interface.

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
union RenderValue(string | Expression<() -> object> | null)

func Render(value: RenderValue) -> string {
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
val value: Value = null
```

For class union carriers, assigning `null` to `U?` may mean a null carrier
reference. Assigning `null` to non-nullable `U` should construct or represent a
carrier with null contents only when the union content set permits null.

For struct union carriers, assigning `null` to non-nullable `U` should produce
a union value whose public C#-compatible surface reports `Value == null`.

The public C#-compatible union surface collapses active null contents and the
default/inactive state to `Value == null`. A stronger Raven-only distinction
would require a separate tagged representation or API.

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

`HasValue` follows the public C# union access pattern and means `Value is not
null`. Raven should not expose active-null contents as `HasValue == true` on the
C#-compatible surface.

### 8. JSON serialization

Parenthesized union JSON conversion should round-trip explicit null contents
through the public C#-compatible `Value == null` surface.

For:

```raven
[JsonConverter(typeof(RavenParenthesizedUnionJsonConverterFactory))]
union JsonValue(string | double | bool | JsonObject | JsonValue[] | null)
```

the JSON token `null` should deserialize to null contents on the public union
surface:

```text
HasValue == false
Value == null
```

This matches the public C# union interface. A stronger Raven-only distinction
would require a separate tagged representation.

## Affected components

Likely implementation touchpoints:

* `src/Raven.CodeAnalysis/Syntax/InternalSyntax/Parser/Parsers/NameSyntaxParser.cs`
  for parsing `null` in parenthesized union declaration member lists if it is not
  already accepted.
* `src/Raven.CodeAnalysis/SemanticModel.Binding.cs` for parenthesized union
  member binding, nullable content metadata, and union-only `null` member syntax.
* `src/Raven.CodeAnalysis/Compilation.Conversions.cs` for `null` literal to
  nullable-content union conversion.
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
* Treat `null` in parenthesized union declaration member lists as nullable
  content state.
* Preserve the non-null member set for case matching and construction.
* Require a `null` match arm when content nullability is maybe-null.
* Ensure `Value` nullability follows the union content state.
* Keep `HasValue` aligned with the C# union access pattern (`Value is not null`).
* Do not expose active-null contents as a distinct public `HasValue` state.

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
* Update type-system docs to clarify that `T?` remains the general nullable type
  syntax and that `null` is union-declaration-only sugar in the current
  implementation.
* Update diagnostics documentation for `RAV0400`.
* Add examples for parenthesized unions and body-form nullable payloads using
  ordinary nullable type syntax.

## Testing plan

Add focused tests in these areas:

* Syntax:
  * `union Value(string | null)`
* Semantics:
  * `null` in ordinary type annotations such as `string | null` is not accepted
    in the current implementation.
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
  * null contents through the public `Value == null` surface.
  * class carrier null reference versus null contents.
  * JSON `null` round-trips through
    `union JsonValue(string | double | bool | JsonObject | JsonValue[] | null)`
    as null contents on the public union surface.
* Interop:
  * consume a C# Preview 4 union with nullable case type through the aligned
    union surface.
  * expose Raven unions with a C#-recognizable `IUnion.Value` surface from the
    alignment proposal.

## Open questions

* Should Raven allow `val value: U = null` for `union struct U(string | null)`
  before the carrier can distinguish active null from default/inactive?
* Should a future general standard-union type syntax allow `A | B | null`, or is
  that too easily confused with `T?`?
* If general `A | B | null` is enabled later, should display prefer that spelling
  or `(A | B)?` in diagnostics?
* How many standard union arities should Raven keep once .NET's union runtime
  surface stabilizes?
* Should nullable standard union normalization produce `NullableTypeSymbol` over
  `TypeUnionSymbol`, or should `TypeUnionSymbol` carry a nullable-content bit and
  project as nullable only at display/conversion boundaries?

## Migration and compatibility

Existing source that used nullable member types in union declarations should
become valid. Existing diagnostics that relied on `RAV0400` will need updated
expectations.

Source that already wrote `(A | B)?` remains valid. The current implementation
does not canonicalize it to `A | B | null`, because that source spelling is
deferred outside parenthesized union declarations.

`Option<T>` remains the explicit algebraic absence model. Nullable-content unions
remain the .NET/nullability interop model. Conversions between `Option<T>` and
nullable unions should remain governed by the separate option-to-nullable
conversion proposal.
