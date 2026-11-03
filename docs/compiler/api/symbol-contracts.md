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
