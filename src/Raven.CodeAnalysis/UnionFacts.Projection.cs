using System;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static partial class UnionFacts
{
    public static bool TryProjectCaseTypeParameterFromUnionArguments(
        UnionCaseTypeSymbol caseSymbol,
        TypeParameterSymbol caseTypeParameter,
        ImmutableArray<TypeParameterSymbol> unionTypeParameters,
        ImmutableArray<TypeSymbol> unionTypeArguments,
        out TypeSymbol mappedType)
    {
        mappedType = caseTypeParameter;

        if (unionTypeParameters.IsDefaultOrEmpty || unionTypeArguments.IsDefaultOrEmpty)
            return false;

        if (!TryMapCaseTypeParameterToUnionTypeParameter(caseSymbol, caseTypeParameter, out var unionTypeParameter))
            return false;

        var mappedIndex = -1;
        for (var i = 0; i < unionTypeParameters.Length; i++)
        {
            if (SymbolEqualityComparer.Default.Equals(unionTypeParameters[i], unionTypeParameter))
            {
                mappedIndex = i;
                break;
            }
        }

        if (mappedIndex < 0)
        {
            for (var i = 0; i < unionTypeParameters.Length; i++)
            {
                if (string.Equals(unionTypeParameters[i].Name, unionTypeParameter.Name, StringComparison.Ordinal))
                {
                    mappedIndex = i;
                    break;
                }
            }
        }

        if (mappedIndex < 0 || mappedIndex >= unionTypeArguments.Length)
            return false;

        mappedType = unionTypeArguments[mappedIndex];
        return true;
    }

    private static bool TryMapCaseTypeParameterToUnionTypeParameter(
        UnionCaseTypeSymbol caseSymbol,
        TypeParameterSymbol caseTypeParameter,
        [NotNullWhen(true)] out TypeParameterSymbol? unionTypeParameter)
    {
        if (TryGetMappingProvider(caseSymbol) is { } mappingProvider &&
            TryGetMappedUnionTypeParameter(mappingProvider, caseTypeParameter, out unionTypeParameter))
        {
            return true;
        }

        // PE/imported compatibility path: map by name when the case has independently emitted type parameters.
        var unionTypeParameters = caseSymbol.Union.TypeParameters;
        foreach (var unionParameter in unionTypeParameters)
        {
            if (string.Equals(unionParameter.Name, caseTypeParameter.Name, StringComparison.Ordinal))
            {
                unionTypeParameter = unionParameter;
                return true;
            }
        }

        unionTypeParameter = null;
        return false;
    }

    private static bool TryGetMappedUnionTypeParameter(
        UnionCaseTypeParameterMapping mappingProvider,
        TypeParameterSymbol caseTypeParameter,
        [NotNullWhen(true)] out TypeParameterSymbol? unionTypeParameter)
    {
        if (mappingProvider.TryGetProjectedUnionTypeParameter(caseTypeParameter, out unionTypeParameter))
            return true;

        var caseTypeParameters = mappingProvider.TypeParameters;
        if (!caseTypeParameters.IsDefaultOrEmpty &&
            caseTypeParameter.Ordinal >= 0 &&
            caseTypeParameter.Ordinal < caseTypeParameters.Length)
        {
            return mappingProvider.TryGetProjectedUnionTypeParameter(caseTypeParameters[caseTypeParameter.Ordinal], out unionTypeParameter);
        }

        unionTypeParameter = null;
        return false;
    }

    private static UnionCaseTypeParameterMapping? TryGetMappingProvider(UnionCaseTypeSymbol caseSymbol)
    {
        if (caseSymbol is UnionCaseTypeParameterMapping provider)
            return provider;

        if (caseSymbol is NamedTypeSymbol namedCase &&
            namedCase.OriginalDefinition is UnionCaseTypeParameterMapping definitionProvider)
        {
            return definitionProvider;
        }

        return null;
    }
}
