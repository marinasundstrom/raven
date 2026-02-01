# Symbols

## Symbol categories

* **Source symbols** - Represents symbols in source code
* **PE symbols** - Wraps metadata representing types in referenced assemblies
* **Constructed symbols** - Represents closed generic types, array types, type unions, tuple types, literal and nullable types, fields etc.
* **Synthesized symbols** - Represents concepts that are created as part of semantic analysis. Such as the implicit `Program` class and `Main` method synthesized for file-scope code, or symbols used when rewriting the Bound Tree.
* **Alias symbols** - Wrap an underlying symbol to expose it under a new name through an `alias` directive.

## Symbol equality

Symbols are not equal by their instance, but requires the use of `SymbolEqualityComparer` to determine if they represent the same thing.

```csharp
SymbolEqualityComparer.Default.Equals(source, destination)
```

## Alias symbols

Alias directives produce `IAliasSymbol` instances. Each alias symbol forwards its members to the underlying symbol while providing an alternate name. Equality comparisons operate on the underlying symbol, so aliases behave identically to their targets aside from the new name.

## Constructed symbols

The representation of an array type is `ArrayTypeSymbol`.

Closed generic types are `ConstructedNamedTypeSymbols` that have "SubstituteMember" with the type parameters substituted by the type arguments.

Some type symbols are specialized, exist in the language or only during semantic analysis, and thus have no equivalent in .NET. These include `TypeUnionSymbol` and `TupleTypeSymbol`. `TypeUnionSymbol` implements `ITypeUnionSymbol`, and represents an anonymous union of constituent member types, while `TupleTypeSymbol` implements `INamedTypeSymbol`.

Other constructed type symbols serve particular purposes:

* `TypeUnionSymbol` – models a value whose type can be any of its constituent types.
* `TupleTypeSymbol` – packs named elements into a fixed-size tuple backed by `System.ValueTuple`.
* `NullableTypeSymbol` – wraps an underlying type in `T?`.
* `ByRefTypeSymbol` – represents a `ref` or `out` type.
* `LiteralTypeSymbol` – captures a specific constant value as a type for flow analysis and generics.
* `NullTypeSymbol` – the type of the `null` literal.
* `UnitTypeSymbol` – models the `Unit` (void) type.

## PE symbols

### Lazy initialization

Every dependent type is resolved lazily via the injected `ReflectionTypeLoader`, which creates symbols for arrays and closed generic types on demand. The resolver caches the mapping between `Type` and `ITypeSymbol` internally to avoid recomputation.
