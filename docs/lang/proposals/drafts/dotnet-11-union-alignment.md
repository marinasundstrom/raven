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
* Pattern matching on a union unwraps the union and applies most patterns to
  `Value`; `var` and discard patterns apply to the union value itself.
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
* Do not decide nullable union contents in this proposal; that is handled by the
  dependent null proposal.
* Do not change `Option<T>` or `Result<T, E>` semantics beyond interop surface
  alignment.

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

The exact nullability of `Value` is refined by the dependent null proposal. This
proposal only establishes that `Value` is the interop surface used by C# and
Raven tooling.

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

### 5. Pattern matching

Raven pattern matching over a union should be able to lower through the same
conceptual model C# uses: inspect the carrier, obtain the active contents, then
apply ordinary patterns to those contents.

This proposal does not require Raven to use boxed `Value` for efficient Raven
code. The compiler can use non-boxing paths internally and still expose `Value`
for interop.

For body-declared Raven unions imported from metadata, pattern matching should
resolve the logical case through the carrier and then bind extraction through
`TryGetValue(out CaseType)`. PE import may see the logical case symbol and the
standalone `TryGetValue` parameter type as different symbol instances, so the
compiler must match them by stable metadata identity. With that bridge in
place, ordinary case patterns continue to work:

```raven
import System.Result.*

func Render(result: Result<int, string>) -> string {
    return result match {
        Ok(val value) => value.ToString()
        Error(val error) => error
    }
}
```

`var` and discard patterns need an explicit Raven decision. C# applies those
patterns to the union value itself rather than unwrapping. Raven may choose to
preserve current semantics, but the divergence should be documented if it
remains.

### 5.1 Propagation and carrier conditional access

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

    return maybeItem match {
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

### 7. Class and struct carriers

C# generated unions are value types by default, but custom unions may be class or
struct types. Raven already supports `union class` and `union struct`, and
should preserve that distinction.

The compiler must model:

* class carrier nullability,
* struct carrier default/inactive state,
* active member/case contents,
* `HasValue` where Raven exposes it.

The dependent null proposal builds on these states to define null contents.

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
