using System;
using System.Collections.Generic;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class SemanticFacts
{
    public static bool IsDerivedFrom(
        ITypeSymbol? type,
        ITypeSymbol? potentialBase,
        SymbolEqualityComparer? comparer = null)
    {
        if (type is null || potentialBase is null)
            return false;

        comparer ??= SymbolEqualityComparer.Default;

        if (type is ITypeParameterSymbol typeParameter)
            return IsDerivedFromTypeParameter(typeParameter, potentialBase, comparer, CreateVisitedSet(comparer));

        for (var current = type.BaseType; current is not null; current = current.BaseType)
        {
            if (comparer.Equals(current, potentialBase))
                return true;
        }

        return false;
    }

    public static bool ImplementsInterface(
        ITypeSymbol? type,
        INamedTypeSymbol? interfaceType,
        SymbolEqualityComparer? comparer = null)
    {
        if (type is null || interfaceType is null)
            return false;

        if (interfaceType.TypeKind != TypeKind.Interface)
            throw new ArgumentException("interfaceType must be an interface symbol.", nameof(interfaceType));

        comparer ??= SymbolEqualityComparer.Default;

        if (comparer.Equals(type, interfaceType))
            return true;

        if (type is IArrayTypeSymbol array && ImplementsArrayInterface(array, interfaceType, comparer))
            return true;

        if (type is ITypeParameterSymbol typeParameter)
            return ImplementsInterfaceTypeParameter(typeParameter, interfaceType, comparer, CreateVisitedSet(comparer));

        foreach (var implementedInterface in GetAllInterfaces(type))
        {
            if (comparer.Equals(implementedInterface, interfaceType))
                return true;
        }

        return false;
    }

    private static bool IsDerivedFromTypeParameter(
        ITypeParameterSymbol typeParameter,
        ITypeSymbol potentialBase,
        SymbolEqualityComparer comparer,
        HashSet<ISymbol> visited)
    {
        if (!visited.Add(typeParameter))
            return false;

        foreach (var constraint in typeParameter.ConstraintTypes)
        {
            if (comparer.Equals(constraint, potentialBase))
                return true;

            if (constraint is ITypeParameterSymbol nestedTypeParameter &&
                IsDerivedFromTypeParameter(nestedTypeParameter, potentialBase, comparer, visited))
            {
                return true;
            }

            if (IsDerivedFrom(constraint, potentialBase, comparer))
                return true;
        }

        return false;
    }

    private static bool ImplementsInterfaceTypeParameter(
        ITypeParameterSymbol typeParameter,
        INamedTypeSymbol interfaceType,
        SymbolEqualityComparer comparer,
        HashSet<ISymbol> visited)
    {
        if (!visited.Add(typeParameter))
            return false;

        foreach (var constraint in typeParameter.ConstraintTypes)
        {
            if (constraint is INamedTypeSymbol namedConstraint)
            {
                if (comparer.Equals(namedConstraint, interfaceType))
                    return true;

                foreach (var implementedInterface in namedConstraint.AllInterfaces)
                {
                    if (comparer.Equals(implementedInterface, interfaceType))
                        return true;
                }
            }
            else if (constraint is ITypeParameterSymbol nestedTypeParameter &&
                     ImplementsInterfaceTypeParameter(nestedTypeParameter, interfaceType, comparer, visited))
            {
                return true;
            }
            else if (constraint is not null && ImplementsInterface(constraint, interfaceType, comparer))
            {
                return true;
            }
        }

        return false;
    }

    private static IEnumerable<INamedTypeSymbol> GetAllInterfaces(ITypeSymbol type)
    {
        if (type is INamedTypeSymbol named)
            return named.AllInterfaces;

        if (type is ArrayTypeSymbol array && array.BaseType is INamedTypeSymbol arrayBase)
            return arrayBase.AllInterfaces;

        return Array.Empty<INamedTypeSymbol>();
    }

    private static bool ImplementsArrayInterface(IArrayTypeSymbol array, INamedTypeSymbol interfaceType, SymbolEqualityComparer comparer)
    {
        var interfaceDefinition = interfaceType.ConstructedFrom as INamedTypeSymbol ?? interfaceType;
        var metadataName = interfaceDefinition.ToFullyQualifiedMetadataName();

        return metadataName switch
        {
            "System.Collections.IEnumerable" => true,
            "System.Collections.ICollection" => true,
            "System.Collections.IList" => true,
            "System.Collections.Generic.IEnumerable`1" =>
                array.Rank == 1 && ImplementsArrayGenericInterface(array, interfaceType, comparer),
            "System.Collections.Generic.ICollection`1" =>
                array.Rank == 1 && ImplementsArrayGenericInterface(array, interfaceType, comparer),
            "System.Collections.Generic.IList`1" =>
                array.Rank == 1 && ImplementsArrayGenericInterface(array, interfaceType, comparer),
            "System.Collections.Generic.IReadOnlyCollection`1" =>
                array.Rank == 1 && ImplementsArrayGenericInterface(array, interfaceType, comparer),
            "System.Collections.Generic.IReadOnlyList`1" =>
                array.Rank == 1 && ImplementsArrayGenericInterface(array, interfaceType, comparer),
            _ => false,
        };
    }

    private static bool ImplementsArrayGenericInterface(
        IArrayTypeSymbol array,
        INamedTypeSymbol interfaceType,
        SymbolEqualityComparer comparer)
    {
        if (interfaceType.TypeArguments.Length != 1)
            return false;

        var elementType = array.ElementType;
        var targetArgument = interfaceType.TypeArguments[0];

        return comparer.Equals(elementType, targetArgument);
    }

    private static HashSet<ISymbol> CreateVisitedSet(SymbolEqualityComparer comparer)
        => new(SymbolEqualityComparerAdapter.Get(comparer));

    private sealed class SymbolEqualityComparerAdapter : IEqualityComparer<ISymbol>
    {
        private readonly SymbolEqualityComparer _comparer;

        private SymbolEqualityComparerAdapter(SymbolEqualityComparer comparer)
            => _comparer = comparer;

        public static IEqualityComparer<ISymbol> Get(SymbolEqualityComparer comparer)
            => comparer as IEqualityComparer<ISymbol> ?? new SymbolEqualityComparerAdapter(comparer);

        public bool Equals(ISymbol? x, ISymbol? y)
            => _comparer.Equals(x, y);

        public int GetHashCode(ISymbol obj)
            => _comparer.GetHashCode(obj);
    }
}
