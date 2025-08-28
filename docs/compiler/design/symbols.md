# Symbols

## Symbol categories

* **Source symbols** - Represents symbols in source code
* **PE symbols** - Wraps metadata representing types in referenced assemblies
* **Constructed symbols** - Represents close generic types, array types, type unions, tuple types and fields etc.
* **Synthesized symbols** - Represents concepts that are created as part of semantic analysis. Such as the implicit `Program` class and `Main` method synthesized for file-scope code, or symbols used when rewriting the Bound Tree.

## Symbol equality

Symbols are not equal by their instance, but requires the use of `SymbolEqualityComparer` to determine if they represent the same thing.

```csharp
SymbolEqualityComparer.Default.Equals(source, destination)
```

## Constructed symbols

The representation of an array type is `ArrayTypeSymbol`.

Closed generic types are `ConstructedNamedTypeSymbols` that have "SubstituteMember" with the type parameters substituted by the type arguments.

Some type symbols are specialized, exists in the language or at semantic analysis, and thus have no equivalent in .NET speak. That include `UnionTypeSymbol` and `TupleTypeSymbol`, both implementing from `INamedTypeSymbol`.

## PESymbols

### Lazy intialization

Every dependant type is resolved lazily via the injected `TypeResolver` which handles the creation of symbols representing arrays and closed generic types. These resolved types (mapping `Type` and `ITypeSymbol`) are cached internally.