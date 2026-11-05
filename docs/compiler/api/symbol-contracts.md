# Symbol contract names

Raven's semantic symbol interfaces use names without an `I` prefix: `Symbol`,
`TypeSymbol`, `NamedTypeSymbol`, `MethodSymbol`, and the related contracts. They
remain interfaces. This is an intentional naming exception to the otherwise
Roslyn-like compiler API; .NET interfaces such as `IEquatable<T>` and external
Roslyn interfaces keep their names.

This migration is source- and binary-breaking. Recompile compiler consumers and
replace the old Raven interface names with their unprefixed names. It applies to
both target implementations, analyzers, language services, macro providers and
samples. .NET remains the default target. Equality, symbol identity, binding,
metadata representation and target capabilities are unchanged.

Concrete types in `Raven.CodeAnalysis.Symbols` have distinct implementation names:

| Previous concrete name | New concrete name |
| --- | --- |
| `Symbol` | `SymbolBase` |
| `AliasSymbol`, `LabelSymbol` | `AliasSymbolImplementation`, `LabelSymbolImplementation` |
| `ArrayTypeSymbol`, `PointerTypeSymbol`, `AddressTypeSymbol` | Corresponding name plus `Implementation` |
| `TupleTypeSymbol`, `IntersectionTypeSymbol`, `StructuralUnionTypeSymbol` | Corresponding name plus `Implementation` |
| `FunctionReturnTypeSymbol`, `ErrorSymbol`, `ErrorTypeSymbol` | Corresponding name plus `Implementation` |

Existing source/provider-specific names such as `SourceMethodSymbol` retain their
roles. The internal `ImportedAssemblySymbol`, `InstanceFieldLayoutSymbol` and
`UnionCompanionSymbol` contracts follow the same unprefixed convention.

Generated visitor method names are preserved, including `VisitIntersectionType`.
The bound-node generator recognizes `SymbolBase` and implementation suffixes;
interface names are no longer shortened by stripping their first letter.

This naming pass is separate from the Raven symbol implementation port. Existing
Raven sources reference the renamed bootstrap contracts until the corresponding
implementations are ported and validated. It does not establish native neoCLR
execution parity or complete the bootstrap compiler.


## Attribute constant values

`TypedConstant` is a readonly record struct. Its value consists of `Type`,
`Value`, and `Kind`; typed equality, object equality, equality operators and
hashing use those stored values. Array constants retain `ImmutableArray`
storage identity equality, rather than comparing their elements recursively.
A default array and an empty array remain distinct.

The constructor and compiler factories remain internal. The three stored
properties are init-only and support record copies, and deconstruction returns
`(Type, Value, Kind)`. `IsNull` and `Values` are computed views, not additional
record state. The existing `ToString()` result,
`Raven.CodeAnalysis.TypedConstant`, is preserved. `WithType` preserves the boxed
payload rather than allocating a new box for an array constant.

These compiler API semantics are shared across target policies. Validation of
the C# implementation and the isolated Raven translation runs on .NET; it does
not establish native neoCLR execution or a completed symbol implementation port.


## Generic interface implementation signatures

Method type parameters have distinct symbol identities in an interface and its
implementation. CLI interface mapping substitutes the contract's parameters by
ordinal before comparing return and parameter types, including constructed types
such as `Box<T>`. Parameter names need not match. Generic arity and static versus
instance shape must match; overloads with different arity are distinct. This
mapping does not change symbol identity or make unrelated type parameters equal.


## Macro binding values and attribute occurrences

`MacroParameterBinding` is a sealed record class. Its parameter, source role,
declaration ordinal and optional invocation argument ordinal determine equality
and hashing. Init-only properties support immutable copies, and deconstruction
returns those four values. Its internal constructor and original `ToString()`
output remain unchanged. This is a value mapping stored as a reference type.

`AttributeData` remains a sealed class with reference identity for bound attribute
occurrences. Its immutable argument arrays and syntax reference are retained
without copying. The Raven translation preserves this distinction instead of
making all immutable reference containers use the same equality model.


## Type uses and namespace ownership

By-reference and nullable wrappers describe uses of an existing type. They retain
its containing namespace for context, but they are not declarations in that
namespace. Creating either wrapper must not add entries to namespace `GetMembers`
or affect subsequent name lookup. This applies to source and imported namespaces
and is shared by the .NET and neoCLR semantic implementations. The regression
coverage exercises source and .NET metadata symbols; it does not establish native
neoCLR execution. These wrapper implementations remain C# bootstrap dependencies
until their Raven translations are integrated.


## Internal type identity facts

`SymbolEqualityComparer` consumes internal provider contracts for nullable,
structural tuple/function and target-selected Unit identity. Implementations expose
the same underlying type, tuple fields, callable signature and runtime representation
used previously; the comparer owns recursive equality and hashing. Constructed
named types reuse `IConstructedTypeSubstitutionInfo` for explicit type arguments.
These contracts do not expose binding services or caches, add public symbol
categories, or change equality rules. Their `I` prefix follows the existing internal
provider convention rather than the public semantic-symbol naming convention.

This separates equality from concrete symbol storage so both compiler source
implementations can share the same identity semantics. Runtime representation
selection stays target-owned, including Unit normalization. Focused coverage compares
independent providers with the existing implementations and retains nominal/generic,
nullable, native structural-function and tuple cases. These checks run on .NET;
neoCLR semantic tests are not proof of execution on a native runtime.
