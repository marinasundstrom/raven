# Symbols API Investigation Findings

## Summary
- Identified an equality contract violation in `SymbolEqualityComparer` that can cause incorrect lookups in hashed collections.
- Found that constructed generic types are compared solely by their definitions, ignoring the closed type arguments.
- Noted several surface APIs on constructed symbols that still throw `NotImplementedException`, posing runtime risks.

## Detailed Findings

### 1. `SymbolEqualityComparer` mixes name and metadata inconsistently
The comparer only looks at `ISymbol.Name` when checking equality, but `GetHashCode` also incorporates `MetadataName`. When two symbols differ only by metadata (for example, an explicit interface implementation whose metadata name includes the interface qualification), `Equals` returns `true` while the hash code differs. This breaks the equality contract and can corrupt dictionaries or hash sets that rely on the comparer. Relevant code:

- Equality path ignores `MetadataName` and stops after matching `Name`, namespace, and container. 【F:src/Raven.CodeAnalysis/SymbolEqualityComparer.cs†L18-L79】
- Hash code path includes `MetadataName`, meaning two symbols considered equal can still produce different hashes. 【F:src/Raven.CodeAnalysis/SymbolEqualityComparer.cs†L96-L136】

**Impact:** Collections using `SymbolEqualityComparer` can behave erratically (e.g., missed lookups or duplicate keys) when explicit interface implementations or other metadata-differing members are present.

### 2. Generic type comparisons ignore closed arguments
`ConstructedNamedTypeSymbol` intentionally reuses the original definition's `Name` and `MetadataName`. Because `SymbolEqualityComparer.Equals` does not examine `INamedTypeSymbol.TypeArguments`, *all* constructed instantiations of a generic definition compare equal as soon as the containing symbol matches. 【F:src/Raven.CodeAnalysis/SymbolEqualityComparer.cs†L53-L79】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L65-L100】

**Example scenario:** `List<int>` and `List<string>` have the same name and container. With the current comparer they compare equal, so lookups for a concrete instantiation may hit the wrong cached entry or report no difference between incompatible types.

**Suggested direction:** Extend the comparer to recognize constructed named types (and arrays) by recursively comparing their type arguments / element types so closed generics remain distinct.

### 3. Constructed symbol APIs still throw `NotImplementedException`
Several members on `ConstructedNamedTypeSymbol` and related helpers still throw, which means legitimate compiler queries can crash when they touch those code paths:

- Tuple projections are unimplemented (`UnderlyingTupleType`, `TupleElements`).
- Name-based lookups and member checks on constructed types immediately throw. 【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L108-L129】

**Impact:** Features that rely on tuple metadata or need to check whether a constructed generic exposes a member (e.g., overload resolution, completion) cannot safely use these APIs yet.

**Suggested direction:** Audit the constructed symbol surface and either implement the missing members or defensively guard callers to avoid runtime exceptions.

