using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class DiscriminatedUnionFacts
{
    private const char CaseMetadataSeparator = '_';

    public static bool IsDiscriminatedUnionType(ITypeSymbol? type)
    {
        return type?.IsDiscriminatedUnion == true;
    }

    public static bool IsDiscriminatedUnionCaseType(ITypeSymbol? type)
    {
        return type?.IsDiscriminatedUnionCase == true;
    }

    public static bool TryGetCaseUnionType(ITypeSymbol? caseType, out ITypeSymbol? unionType)
    {
        unionType = caseType?.UnderlyingDiscriminatedUnion;
        return unionType is not null;
    }

    public static string GetCasePropertyName(string parameterName)
    {
        if (string.IsNullOrEmpty(parameterName))
            return parameterName;

        if (char.IsUpper(parameterName[0]))
            return parameterName;

        Span<char> buffer = stackalloc char[parameterName.Length];
        parameterName.AsSpan().CopyTo(buffer);
        buffer[0] = char.ToUpperInvariant(buffer[0]);
        return new string(buffer);
    }

    public static string GetCaseMetadataBaseName(string unionName, string caseName)
        => $"{unionName}{CaseMetadataSeparator}{caseName}";

    public static bool TryGetLogicalCaseNameFromMetadata(string unionName, string metadataCaseName, out string logicalCaseName)
    {
        var separatedPrefix = unionName + CaseMetadataSeparator;
        if (metadataCaseName.StartsWith(separatedPrefix, StringComparison.Ordinal) &&
            metadataCaseName.Length > separatedPrefix.Length)
        {
            logicalCaseName = metadataCaseName.Substring(separatedPrefix.Length);
            return true;
        }

        // Compatibility: support historical metadata names without a separator.
        if (metadataCaseName.StartsWith(unionName, StringComparison.Ordinal) &&
            metadataCaseName.Length > unionName.Length)
        {
            logicalCaseName = metadataCaseName.Substring(unionName.Length);
            return true;
        }

        logicalCaseName = metadataCaseName;
        return false;
    }

    public static bool TryProjectCaseTypeParameterFromUnionArguments(
        IDiscriminatedUnionCaseSymbol caseSymbol,
        ITypeParameterSymbol caseTypeParameter,
        ImmutableArray<ITypeParameterSymbol> unionTypeParameters,
        ImmutableArray<ITypeSymbol> unionTypeArguments,
        out ITypeSymbol mappedType)
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
        IDiscriminatedUnionCaseSymbol caseSymbol,
        ITypeParameterSymbol caseTypeParameter,
        out ITypeParameterSymbol unionTypeParameter)
    {
        if (TryGetSourceCaseDefinition(caseSymbol) is { } sourceCaseDefinition &&
            TryGetSourceMappedUnionTypeParameter(sourceCaseDefinition, caseTypeParameter, out unionTypeParameter))
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

        unionTypeParameter = null!;
        return false;
    }

    private static bool TryGetSourceMappedUnionTypeParameter(
        SourceDiscriminatedUnionCaseTypeSymbol sourceCaseDefinition,
        ITypeParameterSymbol caseTypeParameter,
        out ITypeParameterSymbol unionTypeParameter)
    {
        if (sourceCaseDefinition.TryGetProjectedUnionTypeParameter(caseTypeParameter, out unionTypeParameter))
            return true;

        var sourceCaseTypeParameters = sourceCaseDefinition.TypeParameters;
        if (!sourceCaseTypeParameters.IsDefaultOrEmpty &&
            caseTypeParameter.Ordinal >= 0 &&
            caseTypeParameter.Ordinal < sourceCaseTypeParameters.Length)
        {
            return sourceCaseDefinition.TryGetProjectedUnionTypeParameter(sourceCaseTypeParameters[caseTypeParameter.Ordinal], out unionTypeParameter);
        }

        unionTypeParameter = null!;
        return false;
    }

    private static SourceDiscriminatedUnionCaseTypeSymbol? TryGetSourceCaseDefinition(IDiscriminatedUnionCaseSymbol caseSymbol)
    {
        if (caseSymbol is SourceDiscriminatedUnionCaseTypeSymbol sourceCase)
            return sourceCase;

        if (caseSymbol is INamedTypeSymbol namedCase &&
            namedCase.OriginalDefinition is SourceDiscriminatedUnionCaseTypeSymbol sourceDefinition)
        {
            return sourceDefinition;
        }

        return null;
    }
}
