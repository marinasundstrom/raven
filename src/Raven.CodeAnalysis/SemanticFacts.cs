using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class SemanticFacts
{
    public static bool IsDerivedFrom(
        this ITypeSymbol? type,
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
            if (current.MetadataIdentityEquals(potentialBase))
                return true;
        }

        return false;
    }

    public static bool ImplementsInterface(
        this ITypeSymbol? type,
        INamedTypeSymbol? interfaceType,
        SymbolEqualityComparer? comparer = null)
    {
        if (type is null || interfaceType is null)
            return false;

        if (interfaceType.TypeKind != TypeKind.Interface)
            throw new ArgumentException("interfaceType must be an interface symbol.", nameof(interfaceType));

        comparer ??= SymbolEqualityComparer.Default;

        if (type.MetadataIdentityEquals(interfaceType))
            return true;

        if (type is INamedTypeSymbol typeInterface &&
            typeInterface.TypeKind == TypeKind.Interface &&
            IsVariantCompatible(typeInterface, interfaceType, comparer))
        {
            return true;
        }

        if (type is ITypeParameterSymbol typeParameter)
            return ImplementsInterfaceTypeParameter(typeParameter, interfaceType, comparer, CreateVisitedSet(comparer));

        foreach (var implementedInterface in GetAllInterfaces(type))
        {
            if (implementedInterface.MetadataIdentityEquals(interfaceType))
                return true;

            if (IsVariantCompatible(implementedInterface, interfaceType, comparer))
                return true;
        }

        return false;
    }

    public static bool SatisfiesConstraints(
        this ITypeSymbol? typeArgument,
        ITypeParameterSymbol? typeParameter)
    {
        if (typeArgument is null || typeParameter is null)
            return false;

        if (typeArgument is IErrorTypeSymbol)
            return true;

        var constraintKind = typeParameter.ConstraintKind;

        if ((constraintKind & TypeParameterConstraintKind.ReferenceType) != 0 &&
            !SatisfiesReferenceTypeConstraint(typeArgument))
        {
            return false;
        }

        if ((constraintKind & TypeParameterConstraintKind.ValueType) != 0 &&
            !SatisfiesValueTypeConstraint(typeArgument))
        {
            return false;
        }

        if ((constraintKind & TypeParameterConstraintKind.TypeConstraint) != 0)
        {
            foreach (var constraintType in typeParameter.ConstraintTypes)
            {
                if (constraintType is IErrorTypeSymbol)
                    continue;

                if (!SatisfiesTypeConstraint(typeArgument, constraintType))
                    return false;
            }
        }

        return true;
    }

    public static bool SatisfiesReferenceTypeConstraint(ITypeSymbol type)
    {
        if (type.IsReferenceType)
            return true;

        if (type is ITypeParameterSymbol typeParameter)
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0;

        return false;
    }

    public static bool SatisfiesValueTypeConstraint(ITypeSymbol type)
    {
        if (type.TypeKind == TypeKind.Nullable)
            return false;

        if (type.IsValueType)
            return true;

        if (type is ITypeParameterSymbol typeParameter)
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0;

        return false;
    }

    public static bool SatisfiesTypeConstraint(ITypeSymbol typeArgument, ITypeSymbol constraintType)
    {
        if (typeArgument is IErrorTypeSymbol || constraintType is IErrorTypeSymbol)
            return true;

        if (typeArgument.MetadataIdentityEquals(constraintType))
            return true;

        if (constraintType is INamedTypeSymbol namedConstraint)
            return SatisfiesNamedTypeConstraint(typeArgument, namedConstraint);

        return true;
    }

    public static bool SatisfiesNamedTypeConstraint(ITypeSymbol typeArgument, INamedTypeSymbol constraintType)
    {
        if (typeArgument.MetadataIdentityEquals(constraintType))
            return true;

        if (constraintType.TypeKind == TypeKind.Interface)
        {
            if (typeArgument is INamedTypeSymbol namedArgument &&
                namedArgument.TypeKind == TypeKind.Interface &&
                namedArgument.MetadataIdentityEquals(constraintType))
            {
                return true;
            }

            foreach (var implemented in typeArgument.AllInterfaces)
            {
                if (implemented.MetadataIdentityEquals(constraintType))
                    return true;
            }

            return false;
        }

        if (typeArgument is INamedTypeSymbol namedType)
        {
            for (var current = namedType; current is not null; current = current.BaseType)
            {
                if (current.MetadataIdentityEquals(constraintType))
                    return true;
            }

            return false;
        }

        if (typeArgument is ITypeParameterSymbol typeParameter)
        {
            foreach (var nestedConstraint in typeParameter.ConstraintTypes)
            {
                if (nestedConstraint is INamedTypeSymbol nestedNamed &&
                    SatisfiesNamedTypeConstraint(nestedNamed, constraintType))
                {
                    return true;
                }
            }

            return false;
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
            if (constraint.MetadataIdentityEquals(potentialBase))
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
                if (namedConstraint.MetadataIdentityEquals(interfaceType))
                    return true;

                if (namedConstraint.TypeKind == TypeKind.Interface &&
                    IsVariantCompatible(namedConstraint, interfaceType, comparer))
                {
                    return true;
                }

                foreach (var implementedInterface in namedConstraint.AllInterfaces)
                {
                    if (implementedInterface.MetadataIdentityEquals(interfaceType))
                        return true;

                    if (IsVariantCompatible(implementedInterface, interfaceType, comparer))
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

    private static IEnumerable<INamedTypeSymbol> GetAllInterfaces(ITypeSymbol type) => type.AllInterfaces;

    private static HashSet<ISymbol> CreateVisitedSet(SymbolEqualityComparer comparer)
        => new(SymbolEqualityComparerAdapter.Get(comparer));

    private static bool IsVariantCompatible(
        INamedTypeSymbol implementedInterface,
        INamedTypeSymbol targetInterface,
        SymbolEqualityComparer comparer)
    {
        if (!implementedInterface.IsGenericType || !targetInterface.IsGenericType)
            return false;

        if (implementedInterface.ConstructedFrom is not INamedTypeSymbol implementedDefinition ||
            targetInterface.ConstructedFrom is not INamedTypeSymbol targetDefinition)
        {
            return false;
        }

        if (!implementedDefinition.MetadataIdentityEquals(targetDefinition))
            return false;

        var typeParameters = implementedDefinition.TypeParameters;
        var implementedArguments = implementedInterface.TypeArguments;
        var targetArguments = targetInterface.TypeArguments;

        if (typeParameters.Length != implementedArguments.Length ||
            typeParameters.Length != targetArguments.Length)
        {
            return false;
        }

        for (var i = 0; i < typeParameters.Length; i++)
        {
            var parameter = typeParameters[i];
            var implementedArgument = implementedArguments[i];
            var targetArgument = targetArguments[i];

            switch (parameter.Variance)
            {
                case VarianceKind.Out:
                    if (!AreAssignable(implementedArgument, targetArgument, comparer))
                        return false;
                    break;
                case VarianceKind.In:
                    if (!AreAssignable(targetArgument, implementedArgument, comparer))
                        return false;
                    break;
                default:
                    if (!implementedArgument.MetadataIdentityEquals(targetArgument))
                        return false;
                    break;
            }
        }

        return true;
    }

    private static bool AreAssignable(
        ITypeSymbol source,
        ITypeSymbol destination,
        SymbolEqualityComparer comparer)
    {
        if (source.MetadataIdentityEquals(destination))
            return true;

        if (destination.SpecialType == SpecialType.System_Object)
        {
            // Reference and boxing conversions are both valid for variance purposes.
            return true;
        }

        if (IsDerivedFrom(source, destination, comparer))
            return true;

        if (destination is INamedTypeSymbol destinationInterface &&
            destinationInterface.TypeKind == TypeKind.Interface &&
            ImplementsInterface(source, destinationInterface, comparer))
        {
            return true;
        }

        return false;
    }

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
