# Proposal: Align Raven Unions with .NET 11

> ℹ️ This proposal is under consideration

## Summary

Align Raven's emitted and consumed union surface with the direction of C# 15
union types in .NET 11, while preserving Raven's existing source syntax and
carrier model. This proposal is the compatibility and interop foundation for
later language work, including [null case support in unions](null-case-in-unions.md).

Raven should continue to support:

```raven
union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}

val input: string | Expression<() -> object>
```

But the compiler should emit and recognize the .NET 11 union markers and basic
union pattern where doing so improves interop:

```csharp
[System.Runtime.CompilerServices.Union]
public readonly struct Result<T, E> : System.Runtime.CompilerServices.IUnion
{
    public object? Value { get; }
}
```

## Motivation

Raven already has unions, standard union syntax, carrier conversion, pattern
matching, and reflection-oriented union metadata. C# 15 is adding unions in
.NET 11 previews. If Raven aligns with that direction now, Raven unions can be
easier to consume from C#, Raven can recognize C# unions more directly, and the
compiler can avoid maintaining an isolated union ABI.

Alignment does not mean copying every C# lowering choice. The goal is practical
interoperability:

* C# should recognize Raven unions as union-like metadata.
* Raven should consume C# unions through normal semantic APIs.
* Shared tooling should see a familiar marker and value surface.
* Raven should keep `T1 | T2` as source sugar for standard unions.
* Raven should keep independent case types and carrier conversion where those
  are part of Raven's language model.

## Current C#/.NET 11 direction

C# 15 union types are in public preview with .NET 11. The current design has
these relevant properties:

* A source `union` declaration generates a value type by default.
* Generated unions implement `System.Runtime.CompilerServices.IUnion`.
* Unions expose `Value`, documented as `object?`; in local .NET 11 Preview 4
  reflection currently reports `System.Object`, while nullable metadata carries
  the nullable state.
* Case types may be classes, structs, interfaces, type parameters, nullable
  types, or other unions.
* Union nullability is case-driven. The default null state of `Value` is
  maybe-null when any case type is maybe-null; otherwise it is not-null for
  constructed values. A constructor or union conversion transfers the incoming
  value's null state to the union's `Value`.
* Pattern matching on a union unwraps the union and applies most patterns to
  `Value`; `var` and discard patterns apply to the union value itself.
* A `null` pattern checks the union contents. For class unions it also checks
  whether the carrier reference is null; for nullable struct-union values it
  also checks whether the nullable wrapper has no value.
* `HasValue` and `TryGetValue(out T)`, when present, participate in the same
  nullability flow as checking `Value`; `HasValue` means `Value` is not null,
  and successful access makes the contents not-null on the true branch.
* C# recognizes custom class or struct unions marked with
  `[System.Runtime.CompilerServices.Union]` when they provide the basic union
  pattern.
* Custom class unions may have a null carrier reference; struct unions may have
  a default value whose `Value` is null.

As of local verification with .NET SDK `11.0.100-preview.4.26230.115`,
`UnionAttribute` and `IUnion` are present in the Preview 4 reference and runtime
assemblies. A small C# probe using:

```csharp
public union Pet(Cat, Dog, string?);

Pet pet = (string?)null;
Console.WriteLine(typeof(Pet).IsValueType);
Console.WriteLine(((IUnion)pet).Value is null);
```

builds and prints:

```text
True
True
```

confirming that generated C# unions are value types by default and expose their
contents through `IUnion.Value`.

References:

* [C# 15: union types](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-15#union-types)
* [C# union type reference](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/union)
* [C# unions feature specification](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/unions)

## Goals

* Emit a C#-recognizable union surface for Raven union carriers.
* Consume C# unions as Raven union-compatible types where the metadata supports
  it.
* Keep Raven's `T1 | T2` standard union syntax as source-level sugar.
* Preserve Raven body-form and parenthesized union declarations.
* Keep public compiler APIs Roslyn-like: callers should use type info, symbol
  info, conversions, operations, and diagnostics.
* Establish a clear representation basis for null support in unions.

## Non-goals

* Do not remove Raven's current union syntax.
* Do not require Raven case types to be nested CLR types.
* Do not require Raven to use C#'s exact storage layout.
* Do not change `Option<T>` or `Result<T, E>` semantics beyond interop surface
  alignment.
* Do not introduce Raven-only metadata that C# must understand for basic union
  construction, conversion, pattern matching, exhaustiveness, or nullability.

## Proposed design

### 1. Marker attribute

Raven should emit or recognize:

```csharp
namespace System.Runtime.CompilerServices
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = false)]
    public sealed class UnionAttribute : Attribute;
}
```

On .NET 11 and later, use the framework-provided type. Raven should align with
the latest installed .NET 11 preview surface and does not need compatibility
branches for older previews.

On .NET 10 and earlier, Raven must provide the compatibility surface itself.
Because Raven currently targets .NET 10 for backwards compatibility and does
not yet have conditional compilation support for source shims, the compiler
should continue generating the required shim metadata during emit. Once Raven
can conditionally compile target-specific support code, prefer source-defined
shims over dynamic emit-time definitions.

Raven-specific metadata can remain for compiler details that C# does not model,
but it should not be required for basic interop on .NET 11 and later.

Raven should not emit `System.Runtime.CompilerServices.UnionCaseAttribute` or
`DiscriminatedUnionCaseAttribute`. Those case marker attributes are not part of
the current .NET 11 Preview 4 direction.

Recognition should follow the C# basic union pattern, not Raven metadata:

* the type is marked with `[System.Runtime.CompilerServices.Union]`;
* the type is a class or struct;
* the type exposes a public `Value` property of type `object?` or `object`;
* public single-parameter constructors define case types;
* optional public `TryGetValue(out T)` members provide non-boxing extraction
  for matching and should be associated with the corresponding case type.

Raven-owned metadata can add source conveniences, such as logical names for
body-declared Raven cases, but it must not be necessary to recognize ordinary
C# generated or custom unions.

### 2. IUnion and Value

Raven carriers should expose a C#-compatible value surface:

```csharp
namespace System.Runtime.CompilerServices
{
    public interface IUnion
    {
        object? Value { get; }
    }
}
```

Raven's existing `Value` property should align with this shape. `Value` means
the active case/member payload as an object, or null when the carrier has no
active contents or the active contents are null.

As with `UnionAttribute`, .NET 11 and later should use the framework-provided
`IUnion`. .NET 10 and earlier should receive Raven's generated compatibility
interface until source shims can be conditionally included.

`Value` is always the interop surface used by C# and Raven tooling. Its metadata
shape is `object?` or `object`, but Raven should not attach an independent
union-level nullable-content contract to that property. Instead, nullable
contents are derived from the case construction surface:

* a case constructor parameter typed as nullable, such as `string?` or `int?`,
  makes the union contents maybe-null for C# and Raven analysis;
* a union conversion from a maybe-null value transfers that maybe-null state to
  the resulting union value;
* `HasValue` and `TryGetValue(out T)` are access-pattern helpers that refine
  the contents to not-null on successful checks, matching C# flow rules.

Raven may keep an internal convenience property such as `ContentMayBeNull`, but
it must be derived from the C#-recognizable case construction surface:
constructor parameter types and imported nullable annotations. `TryGetValue(out
T)` remains an extraction/access pattern and must not introduce extra cases when
constructors already define the case set. `ContentMayBeNull` should not be
emitted as Raven-only metadata and should not make a union nullable when the C#
case surface is not nullable.

This distinction matters for standard union sugar. A value of type
`System.Union<T1, T2>` has a fixed binary shape; Raven cannot attach a separate
nullable-content bit to one use site and remain C#-compatible. If null contents
are needed, they must be represented by nullable case types:

```raven
val value: string? | int
```

rather than a hidden Raven-only nullable marker on `string | int`.

### 3. Case construction

Raven may keep its current construction surface:

```raven
val ok: Result<int, string> = Ok(1)
val err = Result<int, string>.Error("bad")
```

For interop, the emitted carrier should expose creation members that C# can use
or pattern recognize. The preferred shape is:

```csharp
public Result(Ok<T> value);
public Result(Error<E> value);

public static implicit operator Result<T, E>(Ok<T> value);
public static implicit operator Result<T, E>(Error<E> value);
```

Raven can continue to synthesize independent case types rather than nested case
types. Compatibility must not depend on nested CLR case types.

For standard C#-style parenthesized unions, the public constructor set is the
case set. Raven should avoid creating additional public one-parameter
constructors that are not intended to be C# cases. Helper construction APIs, if
needed, should use names or accessibility that do not alter the C# case set.

When Raven consumes an imported union, implicit conversion to the carrier should
be available from each public constructor parameter type. If the imported type
also defines a user conversion, Raven should preserve normal conversion
priority instead of treating the union conversion as a special higher-priority
path.

For parenthesized unions, the constructor parameter types are the case types
from C#'s perspective. Raven's `null` source sugar must therefore lower to this
constructor surface rather than to a separate null case:

```raven
union JsonValue(string | double | bool | JsonObject | JsonValue[] | null)
```

should emit and model a nullable case constructor, for example the equivalent
of `string?`, `double?`, and the other listed member cases when `null` appears
explicitly. It must not emit a synthetic `null` case constructor, and it must
not rely on a Raven-only marker to make `Value` maybe-null.

```raven
union U(string? | Uri)
union V(int? | bool)
```

This keeps binary compatibility centered on the same case constructor pattern
that C# consumes.

Body-declared case types are a Raven extension rather than the C# standard union
shape. For those cases Raven may emit Raven-owned implementation metadata on the
carrier:

```csharp
namespace Raven.Runtime.CompilerServices
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true, Inherited = false)]
    public sealed class RavenUnionCaseAttribute(string caseTypeMetadataName, string name, int ordinal) : Attribute;
}
```

The attribute records the stable case type metadata name, logical source name,
and source ordinal. It is carrier-level metadata so individual case types do not
need a non-standard system marker. PE import should prefer this metadata for
body-declared Raven unions, and use public single-argument constructors and
`TryGetValue(out T)` to infer member types for C#/standard unions.

Raven case types emitted from body-form unions may remain standalone PE types.
The imported union carrier is still the logical owner for source lookup and
semantic APIs:

```raven
import System.Result.*

val result = Ok(42)
```

`import System.Result.*` imports the logical members of the `Result` union,
including `Ok` and `Error`, even though their PE types are emitted as standalone
metadata types. The same applies to `System.Option.*` and to Raven-authored
metadata unions that expose `RavenUnionCaseAttribute` on the carrier. This keeps
source imports aligned with Raven's union model while avoiding non-standard
system case marker attributes.

### 4. Case extraction

Raven should prefer the C#-compatible non-boxing extraction pattern:

```csharp
public bool TryGetValue(out Ok<T> value);
public bool TryGetValue(out Error<E> value);
```

Overload resolution distinguishes the case type. Existing Raven-specific helper
names can remain temporarily, but the C#-compatible form should be the long-term
surface.

For imported C# unions, `TryGetValue(out T)` should be treated as an access
pattern, not as the source of new cases when the constructor case set is already
known. If constructors and `TryGetValue` disagree, constructors define the
authoritative case set and the mismatch should either be ignored for matching
optimization or reported for Raven-authored source, depending on whether the
compiler is validating or merely consuming metadata.

### 5. Pattern matching

Raven pattern matching over a union should be able to lower through the same
conceptual model C# uses: inspect the carrier, obtain the active contents, then
apply ordinary patterns to those contents.

This proposal does not require Raven to use boxed `Value` for efficient Raven
code. The compiler can use non-boxing paths internally and still expose `Value`
for interop.

The semantic rules should align with C#:

* type, constant, relational, property, list, and logical patterns apply to the
  union contents;
* `var` and discard patterns apply to the incoming union value itself;
* for class unions, non-null content patterns require the carrier reference to
  be non-null;
* for nullable struct-union values, non-null content patterns require the
  nullable wrapper to have a value;
* a `null` pattern covers a null `Value`, plus a null carrier reference for
  class unions and an empty nullable wrapper for nullable struct-union values;
* pattern lowering should prefer `TryGetValue(out T)` for type-directed
  extraction, then fall back to `HasValue` for null checks, then fall back to
  `Value`.

For body-declared Raven unions imported from metadata, pattern matching should
resolve the logical case through the carrier and then bind extraction through
`TryGetValue(out CaseType)`. PE import may see the logical case symbol and the
standalone `TryGetValue` parameter type as different symbol instances, so the
compiler must match them by stable metadata identity. With that bridge in
place, ordinary case patterns continue to work:

```raven
import System.Result.*

func Render(result: Result<int, string>) -> string {
    match result {
        Ok(val value) => value.ToString()
        Error(val error) => error
    }
}
```

`var` and discard patterns need an explicit Raven decision. C# applies those
patterns to the union value itself rather than unwrapping. Raven may choose to
preserve current semantics, but the divergence should be documented if it
remains.

### 5.1 Exhaustiveness

Exhaustiveness should be based on the same case set that C# sees:

* public single-parameter constructors for C# generated/custom unions;
* Raven-owned case metadata only for Raven body-declared unions whose logical
  case types cannot be reconstructed from the C# surface alone;
* parenthesized union member types for Raven source unions, emitted as C#
  constructor case types.

A switch/match is exhaustive when every case type is handled. If the incoming
union contents are maybe-null according to C# nullability flow, `null` must also
be handled. Struct union default/inactive state should be modeled as `Value ==
null`; nullable struct-union wrappers add the wrapper-null state on top.

### 5.2 Propagation and carrier conditional access

Raven's `?` propagation and carrier conditional access are part of the same
interop surface as pattern matching. When the receiver is a Raven.Core
`Result<T, E>` or `Option<T>` imported from metadata, the compiler should use
the carrier's logical case set to find the constructed PE case type:

```raven
import System.*

func GetUser() -> Result<User, Err> {
    return Ok(User("Marina", Some(Item("Candy"))))
}

func GetItem() -> Result<string, Err> {
    val maybeItem = GetUser()?.Item?

    match maybeItem {
        Some(val item) => Ok(item.Name)
        None => Error(Err.MissingName)
    }
}
```

The binder should infer the constructed PE case types from constructors and
`TryGetValue(out T)` before falling back to Raven's logical case wrappers. This
matters because metadata names such as `Result_Ok<T>` and `Result_Error<E>`
encode the carrier name and generic arity, while the source-visible logical
names remain `Ok` and `Error`.

### 6. Standard union syntax

Raven keeps:

```raven
T1 | T2
```

as source sugar for a standard union carrier. The carrier implementation may be
Raven's existing bridge or a future .NET-provided standard union type when one
stabilizes.

The semantic model should avoid exposing implementation-specific bridge details
to language-service and analyzer consumers. Public semantic APIs should describe
the union shape, member types, conversions, and nullability.

Nullability for standard union syntax follows the same case-type rule:

* `T | null`, if supported outside declarations, should canonicalize to `T?`
  rather than a one-case standard union.
* `T1 | T2 | null` should either rewrite to an explicit nullable case type when
  the target case is unambiguous or be rejected with a diagnostic requiring
  `T1? | T2` / `T1 | T2?`.
* Raven should not represent standard union nullability as a use-site flag on
  `System.Union<...>` because that flag would not exist in the C# union pattern
  or metadata shape.

### 7. Class and struct carriers

C# generated unions are value types by default, but custom unions may be class or
struct types. Raven already supports `union class` and `union struct`, and
should preserve that distinction.

The compiler must model:

* class carrier nullability,
* struct carrier default/inactive state,
* active member/case contents,
* `HasValue` where Raven exposes it.

Raven should not assume all imported C# unions are value types. Generated C#
unions are structs, but custom unions may be classes, and class-union matching
has an extra carrier-null state. This affects conversions, nullable annotations,
match lowering, exhaustiveness diagnostics, and `is null` behavior.

## Compatibility checklist

Raven is aligned with the C# union pattern when all of the following are true:

* Raven emits `[Union]`, implements `IUnion`, and exposes a public `Value`
  property that C# accepts.
* Raven source parenthesized unions emit public constructors whose parameter
  types exactly match the C# case types Raven wants to expose.
* Raven source body-form unions remain Raven extensions but expose enough
  constructor and `TryGetValue(out T)` surface for C# consumption where
  practical.
* Raven imports C# generated unions and custom `[Union]` class/struct types
  without Raven-specific metadata.
* Raven derives case types from public one-parameter constructors, using
  `TryGetValue(out T)` for optimized extraction rather than as a competing
  source of truth.
* Raven implements union conversions from case types to carriers with normal
  conversion priority.
* Raven pattern matching unwraps to `Value` according to C# rules, including the
  `var`/discard exceptions and class/nullable-struct carrier null states.
* Raven exhaustiveness diagnostics use the C# case set and require `null` only
  when the C# null state of the incoming union contents is maybe-null.
* Raven standard union sugar does not invent metadata beyond the C# union
  pattern; nullable content is represented by nullable case types.

## Affected components

Likely implementation touchpoints:

* `src/Raven.CodeAnalysis/SemanticModel.Binding.cs` for recognizing C# union
  metadata and binding Raven union declarations to the aligned surface.
* `src/Raven.CodeAnalysis/Symbols/PE/PEUnionSymbols.cs` for consuming .NET 11
  union metadata and reconstructing Raven logical case types from carrier
  metadata.
* `src/Raven.CodeAnalysis/Binder/ImportBinder.cs` and member lookup binders for
  exposing logical Raven case types through `import UnionName.*`.
* `src/Raven.CodeAnalysis/Symbols/Source/SourceUnionSymbol.cs` and related case
  symbols for source union shape.
* `src/Raven.CodeAnalysis/UnionFacts.cs` and
  `src/Raven.CodeAnalysis/Symbols/UnionSymbolExtensions.cs` for shared union
  recognition helpers.
* `src/Raven.CodeAnalysis/Compilation.Conversions.cs` for case-to-carrier and
  carrier-to-case conversions.
* `src/Raven.CodeAnalysis/BoundTree/BoundIsPatternExpression.cs` and
  `src/Raven.CodeAnalysis/MatchExhaustivenessEvaluator.cs` for pattern and
  exhaustiveness alignment.
* `src/Raven.CodeAnalysis/BoundTree/Lowering/*Propagate*` and carrier
  conditional-access binding for matching PE case wrappers and constructed
  standalone case types by metadata identity.
* `src/Raven.CodeAnalysis/CodeGen/*` for emitting `[Union]`, `IUnion`, Raven
  case metadata, `Value`, creation members, and `TryGetValue(out T)`.
* `src/Raven.CodeAnalysis/Symbols/SymbolExtensions.cs` for display of standard
  union syntax instead of implementation bridge names.
* `docs/lang/spec/dotnet-implementation.md` and
  `docs/lang/spec/language-specification.md` for interop and source semantics.

## Testing plan

Add focused tests in these areas:

* Emit Raven unions with a .NET 11-compatible marker and `IUnion.Value` surface.
* Verify body-declared Raven cases are recorded through
  `RavenUnionCaseAttribute` on the carrier, not system case marker attributes.
* Consume C# Preview 4 generated unions from metadata.
* Consume custom C# unions marked with `[Union]`.
* Preserve Raven construction syntax for body-form and parenthesized unions.
* Verify wildcard imports such as `import System.Result.*` and
  `import System.Option.*` bring logical case names into unqualified scope from
  metadata.
* Validate `TryGetValue(out T)` extraction for independent case types.
* Verify pattern matching over consumed C# unions and Raven body-form unions
  imported from PE metadata.
* Verify result/option propagation and carrier conditional access over Raven.Core
  metadata uses constructed PE case types.
* Verify standard union `T1 | T2` display and semantic type info stay stable.

## Open questions

* Should Raven emit `[Union]` only for `net11.0+`, or polyfill it for older
  targets when Raven controls both producer and consumer?
* Should Raven follow C# exactly for `var` and discard patterns over unions, or
  preserve existing Raven behavior and document the divergence?
* Should `HasValue` remain part of Raven's public carrier surface if it is not
  required by C#?
* When .NET provides a standard union carrier type, should Raven's `T1 | T2`
  lower to it directly or remain on Raven.Core for compatibility?

## Migration and compatibility

This proposal should be additive. Existing Raven source syntax remains valid.
Existing emitted metadata can continue to be recognized while new metadata gains
the .NET 11-compatible surface.

The likely migration path is:

1. Recognize .NET 11 union markers in PE symbols.
2. Emit `[Union]`, `IUnion`, and compatible `Value` for Raven unions where the
   target framework supports them.
3. Replace non-standard system case marker attributes with Raven-owned
   carrier-level case metadata for body-declared Raven cases.
4. Add `TryGetValue(out T)` alongside existing Raven extraction helpers.
5. Update docs and display once the compatibility surface is stable.

Null content support should be implemented only after this foundation is clear,
because it depends on the distinction between carrier state, content state, and
default struct state.
