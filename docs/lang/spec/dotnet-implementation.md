# .NET Implementation Notes

This document outlines how Raven constructs map to the .NET runtime and metadata. For language semantics, see [language-specification.md](language-specification.md).

## Unit type
When interacting with .NET, methods that return `void` are projected as returning `unit`, and Raven's `unit` emits as `void` unless the value is observed. After any call that returns metadata `void`, the compiler loads `Unit.Value` so the invocation still produces a `unit` result. In an expression statement that value is discarded, enabling nested `unit`-returning calls such as `Console.WriteLine(Console.WriteLine("foo"))`. The `unit` type is a value type (`struct`) and participates in generics, tuples, and unions like any other type.

## Return statements
A `return` without an expression in a method that returns `unit` emits IL with no value. If the underlying method returns `void`, `Unit.Value` is loaded to produce a `unit` result before the `ret` instruction.

## Attributes
Source attributes are bound using the same import and namespace lookup rules as other type references, so `import System.*` enables `[Obsolete]` without a fully qualified name. When Raven creates metadata `AttributeData`, it accepts the subset of argument expressions supported by the runtime: literals (including `null`), `typeof` expressions, array/collection literals (including empty collections when a target type is known), and conversions among those forms. The compiler lowers these argument forms to typed constants before emitting the attribute payload.

Raven also validates explicit target prefixes (`assembly:`, `return:`, etc.)
against declaration position:

* `assembly:` binds only at the compilation-unit level.
* `return:` binds to callable return metadata, not to declaration-level method attributes.
* Target prefixes used in an invalid declaration context are rejected with an
  attribute-target diagnostic.

## Extension members
Raven both declares and consumes extension members using the CLR's
`ExtensionAttribute`. Source extensions arise from two forms:

* An `extension` declaration emits a `static` class named after the container.
  Each member inside the declaration becomes a `static` method whose first
  parameter represents the `self` receiver. The compiler synthesizes that
  parameter, applies the `ExtensionAttribute`, and copies any explicit
  parameters written in source onto the emitted method signature.
* Existing static methods annotated with `[Extension]` continue to be recognised
  as extensions.

Computed properties declared inside an `extension` body lower to accessor
methods that follow the same pattern. The compiler synthesizes `get_` and
`set_` methods, inserts the receiver as the leading parameter, and marks each
accessor with `ExtensionAttribute`. Property metadata is emitted alongside the
accessors so reflection reports a property with the expected accessor pair even
though the backing logic is implemented by static methods.

To interoperate with C# extension blocks (C# 14), Raven also emits an extension
marker nested type for each `extension` declaration. The marker type is named
`<>__RavenExtensionMarker` and contains a single `<Extension>$` method whose
parameter encodes the receiver type. Each emitted extension member (methods and
properties, including static extension members) is annotated with
`System.Runtime.CompilerServices.ExtensionMarkerNameAttribute` pointing to the
marker type name, enabling C# to recover the extension receiver signature when
consuming Raven-compiled assemblies.

In both cases the attribute ensures the metadata matches C#'s expectations.【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L197-L233】 When binding a
member-style invocation, Raven merges instance methods with any imported
extensions that can accept the receiver, then rewrites the call to pass the
receiver as the leading static argument during lowering and IL emission.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1946-L2001】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Invocation.cs†L8-L29】 The same rewrite applies to extension-property access: getters become static calls that receive the target as their first argument, and setters pass both the target and assigned value to the synthesized method.

The CLI ships with regression coverage that compiles and runs extension-heavy
programs, including LINQ-style pipelines that rely on lambda arguments, to
ensure the metadata load context path continues to resolve delegate
constructors and emit correct IL.【F:test/Raven.CodeAnalysis.Samples.Tests/SampleProgramsTests.cs†L66-L140】【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L563-L703】

## Properties and fields

Raven is property-first for type members:

* `val`/`var` declarations in classes/structs emit CLR properties with accessor
  methods (`get_`/`set_`/`init` as applicable).
* Stored and auto-style properties synthesize backing storage when needed.
* `field` used inside a property accessor refers to that synthesized backing
  field for the current property.

Explicit `field` declarations are emitted as CLR fields and are intended for
storage/layout-sensitive scenarios (for example interop with
`StructLayout(LayoutKind.Sequential|Explicit)` and `FieldOffset`).
`readonly field` emits an `initonly` field. Dedicated `const` declarations emit
metadata literal fields (`static`/`literal`).

## Lifecycle declarations

Raven lifecycle declarations map to CLR methods:

* `init { ... }` lowers as an instance constructor body (`.ctor`) with no
  explicit parameter list in source.
* `static init { ... }` lowers as a static constructor (`.cctor`).
* `init(...)` remains constructor-shape syntax and maps to `.ctor` overloads.
* `finally { ... }` lowers as a `Finalize` override.

## Union interop (C#)
Raven unions compile into a carrier type plus independent case types that C#
can consume directly. Each case becomes a public type with a constructor that
accepts the payload values, a set of get-only properties for those payloads,
and a `Deconstruct(out ...)` method that mirrors the payload order. The union
carrier exposes overloaded `TryGetValue(out CaseType value)` helpers to safely
extract a case instance. For parenthesized unions such as
`union Either<T1, T2>(T1, T2)`, the carrier is declared over existing member
types instead of synthesized case types, so the carrier constructor and
`TryGetValue` overloads operate directly on those member types.

Producing a union from C# is done by constructing the case and then converting
it to the carrier:

```csharp
// Raven
// union Token { Identifier(text: string) Number(value: int) }

var identifier = new TokenIdentifier("hello");
Token token = identifier;
Token other = new TokenNumber(42);
```

Consuming a union from C# involves calling an overloaded `TryGetValue` helper
and then using the case properties or deconstruction to extract payload values:

```csharp
if (token.TryGetValue(out TokenIdentifier identifier))
{
    var (text) = identifier; // Deconstruct(out string text)
    Console.WriteLine(text);
}
```

For a parenthesized union, extraction is performed directly on the member type:

```csharp
// Raven
// union Either<T1, T2>(T1, T2)

Either<int, string> value = 42;
if (value.TryGetValue(out int left))
{
    Console.WriteLine(left);
}
```

These members allow C# callers to work with Raven unions without
needing reflection, while Raven still relies on the synthesized metadata
attributes to preserve the union semantics for other tools.

## Generic variance

The Raven compiler surfaces the CLR's variance metadata directly. When importing
types from reference assemblies, the `GenericParameterAttributes` flag on a type
parameter controls the reported `VarianceKind`: `Covariant` maps to `out` and
`Contravariant` maps to `in`. These annotations influence interface
implementation checks and reference conversions so that, for example,
`IEnumerable<string>` is recognised as an implementation of
`IEnumerable<object>`, and `IComparer<object>` satisfies a requirement for
`IComparer<string>`.

Source interface declarations may annotate their type parameters with `out` or
`in`. Raven maps those modifiers onto the same metadata flags when emitting
symbols, so variant source interfaces interoperate with metadata-defined
counterparts without requiring any special handling.
