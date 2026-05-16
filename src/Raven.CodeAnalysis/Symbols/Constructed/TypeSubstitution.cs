using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal interface IConstructedTypeSubstitutionInfo
{
    INamedTypeSymbol DefinitionForSubstitution { get; }
    ImmutableArray<ITypeSymbol> ExplicitTypeArgumentsForSubstitution { get; }
}

internal static class TypeSubstitution
{
    internal static INamedTypeSymbol GetDefinitionForSubstitution(INamedTypeSymbol type)
    {
        if (type is IConstructedTypeSubstitutionInfo constructed)
            return constructed.DefinitionForSubstitution;

        return (INamedTypeSymbol?)(type.ConstructedFrom ?? type) ?? type;
    }

    internal static ImmutableArray<ITypeSymbol> GetShallowTypeArguments(INamedTypeSymbol type)
    {
        if (type is IConstructedTypeSubstitutionInfo constructed)
            return constructed.ExplicitTypeArgumentsForSubstitution;

        if ((type.TypeArguments.IsDefaultOrEmpty || type.TypeArguments.Length == 0) &&
            !type.TypeParameters.IsDefaultOrEmpty &&
            type.TypeParameters.Length > 0)
        {
            return type.TypeParameters.Cast<ITypeSymbol>().ToImmutableArray();
        }

        return type.TypeArguments;
    }

    internal static bool TryGetEquivalentTypeParameterSubstitution(
        ITypeParameterSymbol parameter,
        IReadOnlyDictionary<ITypeParameterSymbol, ITypeSymbol> substitutions,
        out ITypeSymbol replacement)
    {
        foreach (var entry in substitutions)
        {
            if (!AreEquivalentTypeParameters(parameter, entry.Key))
                continue;

            replacement = entry.Value;
            return true;
        }

        replacement = null!;
        return false;
    }

    internal static bool AreEquivalentTypeParameters(
        ITypeParameterSymbol left,
        ITypeParameterSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        if (left.OwnerKind != right.OwnerKind ||
            left.Ordinal != right.Ordinal)
        {
            return false;
        }

        return HaveEquivalentTypeParameterOwners(left.ContainingSymbol, right.ContainingSymbol);
    }

    private static bool HaveEquivalentTypeParameterOwners(
        ISymbol? leftOwner,
        ISymbol? rightOwner)
    {
        if (leftOwner is null || rightOwner is null)
            return false;

        if (SymbolEqualityComparer.Default.Equals(leftOwner, rightOwner))
            return true;

        if (leftOwner is INamedTypeSymbol leftType &&
            rightOwner is INamedTypeSymbol rightType)
        {
            return SymbolEqualityComparer.Default.Equals(
                GetDefinitionForSubstitution(leftType),
                GetDefinitionForSubstitution(rightType));
        }

        if (leftOwner is IMethodSymbol leftMethod &&
            rightOwner is IMethodSymbol rightMethod)
        {
            return SymbolEqualityComparer.Default.Equals(
                leftMethod.OriginalDefinition ?? leftMethod,
                rightMethod.OriginalDefinition ?? rightMethod);
        }

        return false;
    }

    internal static INamedTypeSymbol ReanchorNested(
        INamedTypeSymbol nestedDefinition,
        INamedTypeSymbol containingOverride,
        Dictionary<ITypeParameterSymbol, ITypeSymbol>? inheritedSubstitution,
        ImmutableArray<ITypeSymbol> typeArguments)
    {
        return ConstructedNamedTypeSymbol.ReanchorNested(
            nestedDefinition,
            containingOverride,
            inheritedSubstitution,
            typeArguments);
    }

    internal static void AddContainingTypeSubstitutions(
        INamedTypeSymbol? containingType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutionMap)
    {
        if (containingType is null)
            return;

        if (containingType.ContainingType is INamedTypeSymbol outer)
            AddContainingTypeSubstitutions(outer, substitutionMap);

        var definition = GetDefinitionForSubstitution(containingType);
        var typeParameters = definition.TypeParameters;
        if (typeParameters.IsDefaultOrEmpty || typeParameters.Length == 0)
            return;

        var typeArguments = GetShallowTypeArguments(containingType);
        if (typeArguments.IsDefaultOrEmpty || typeArguments.Length == 0)
            return;

        var arity = Math.Min(typeParameters.Length, typeArguments.Length);
        for (var i = 0; i < arity; i++)
        {
            var key = (ITypeParameterSymbol)(typeParameters[i].OriginalDefinition ?? typeParameters[i]);
            var value = typeArguments[i];

            if (!substitutionMap.TryGetValue(key, out var existing) || existing is ITypeParameterSymbol)
                substitutionMap[key] = value;
        }
    }
}
