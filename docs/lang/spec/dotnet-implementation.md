# .NET Implementation Notes

This document outlines how Raven constructs map to the .NET runtime and metadata. For language semantics, see [language-specification.md](language-specification.md).

## Unit type
When interacting with .NET, methods that return `void` are projected as returning `unit`, and Raven's `unit` emits as `void` unless the value is observed. After any call that returns metadata `void`, the compiler loads `Unit.Value` so the invocation still produces a `unit` result. In an expression statement that value is discarded, enabling nested `unit`-returning calls such as `Console.WriteLine(Console.WriteLine("foo"))`. The `unit` type is a value type (`struct`) and participates in generics, tuples, and unions like any other type.

## Return statements
A `return` without an expression in a method that returns `unit` emits IL with no value. If the underlying method returns `void`, `Unit.Value` is loaded to produce a `unit` result before the `ret` instruction.

## Attributes
Source attributes are bound using the same import and namespace lookup rules as other type references, so `import System.*` enables `[Obsolete]` without a fully qualified name. When Raven creates metadata `AttributeData`, it accepts the subset of argument expressions supported by the runtime: literals (including `null`), `typeof` expressions, array/collection literals (including empty collections when a target type is known), and conversions among those forms. The compiler lowers these argument forms to typed constants before emitting the attribute payload.

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

## Union types
When emitted to .NET metadata, a union is projected as the narrowest common denominator of its members. If every member shares a base class, that base type becomes the metadata type; otherwise, `object` is used. Including `null` in the union marks the emitted type as nullable.

For example:

```raven
val pet = if flag { Dog() } else { Cat() } // Dog | Cat
```

Emits `Animal` because both `Dog` and `Cat` derive from it. In contrast:

```raven
val value = if flag { 0 } else { "hi" } // int | string | null
```

Emits `object?` since `int` and `string` share no base class other than `object`, and `null` is included.

This narrowing makes unions friendlier to inheritance-based languages such as C#, and it gives the runtime a smaller set of types to resolve. The `TypeUnionsAnalyzer` provides additional hints about possible targets so that consumers can work with the projected type more effectively.

To preserve the original union members, the compiler also attaches a `TypeUnionAttribute` to the parameter or return type in metadata. Its constructor accepts `object[]` so that each argument can be either a `System.Type` for a type branch or the literal value itself for a literal branch. The method signature still uses the narrowed base type (or `object`) as described above.

For example:

```raven
func f(x: string | unit | null) -> unit { }
```

Emits a parameter of type `object?` with:

```csharp
[TypeUnionAttribute(typeof(string), typeof(Unit), typeof(Null))]
```

attached, indicating the full set of possible values.

Literal unions are represented in the same way. A parameter constrained to the string literals `"yes"` or `"no"` is emitted as a `string` with:

```csharp
[TypeUnionAttribute("yes", "no")]
```

where each literal value is encoded directly in the attribute.

Literal types can also be combined with other types. A parameter typed as `"yes" | "no" | null` is emitted as `string?` with:

```csharp
[TypeUnionAttribute("yes", "no", typeof(Null))]
```

mixing literal values and `System.Type` references in the attribute.

Raven emits shim types so that every union member has a concrete `Type`:

* `Unit` represents the Raven `unit` value and is emitted into every assembly.
* `Null` represents the `null` literal and is emitted only when a union includes `null`.

## Discriminated union interop (C#)
Discriminated unions compile into a nested-struct hierarchy that C# can consume
directly. Each case becomes a public nested `struct` with a constructor that
accepts the payload values, a set of get-only properties for those payloads,
and a `Deconstruct(out ...)` method that mirrors the payload order. The outer
union struct declares an implicit conversion from each case and a `TryGet{Case}`
helper to safely extract a case instance.

Producing a union from C# is typically done by constructing the case and letting
the implicit conversion take over:

```csharp
// Raven
// union Token { Identifier(text: string) Number(value: int) }

Token token = new Token.Identifier("hello"); // implicit case -> union conversion
Token other = new Token.Number(42);
```

Consuming a union from C# involves calling the `TryGet{Case}` helper and then
using the case properties or deconstruction to extract payload values:

```csharp
if (token.TryGetIdentifier(out var identifier))
{
    var (text) = identifier; // Deconstruct(out string text)
    Console.WriteLine(text);
}
```

These members allow C# callers to work with Raven discriminated unions without
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
