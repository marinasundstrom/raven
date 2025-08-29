# Symbols

## Symbol categories

* **Source symbols** - Represents symbols in source code
* **PE symbols** - Wraps metadata representing types in referenced assemblies
* **Constructed symbols** - Represents closed generic types, array types, type unions, tuple types, literal and nullable types, fields etc.
* **Synthesized symbols** - Represents concepts that are created as part of semantic analysis. Such as the implicit `Program` class and `Main` method synthesized for file-scope code, or symbols used when rewriting the Bound Tree.

## Symbol equality

Symbols are not equal by their instance, but requires the use of `SymbolEqualityComparer` to determine if they represent the same thing.

```csharp
SymbolEqualityComparer.Default.Equals(source, destination)
```

## Constructed symbols

The representation of an array type is `ArrayTypeSymbol`.

Closed generic types are `ConstructedNamedTypeSymbols` that have "SubstituteMember" with the type parameters substituted by the type arguments.

Some type symbols are specialized, exist in the language or only during semantic analysis, and thus have no equivalent in .NET. These include `UnionTypeSymbol` and `TupleTypeSymbol`, both implementing `INamedTypeSymbol`.

Other constructed type symbols serve particular purposes:

* `NullableTypeSymbol` – wraps an underlying type in `T?`.
* `ByRefTypeSymbol` – represents a `ref` or `out` type.
* `LiteralTypeSymbol` – captures a specific constant value as a type for flow analysis and generics.
* `NullTypeSymbol` – the type of the `null` literal.
* `UnitTypeSymbol` – models the `Unit` (void) type.

## PE symbols

### Lazy initialization

Every dependent type is resolved lazily via the injected `TypeResolver`, which creates symbols for arrays and closed generic types on demand. The resolver caches the mapping between `Type` and `ITypeSymbol` internally to avoid recomputation.
