using System;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class UnionFacts
{
    public static NamedTypeSymbol? GetMemberProvider(NamedTypeSymbol type)
        => type.GetMembers("IUnionMembers").OfType<NamedTypeSymbol>()
            .FirstOrDefault(member => member.TypeKind == TypeKind.Interface &&
                member.DeclaredAccessibility == Accessibility.Public && member.Arity == 0);

    public static ImmutableArray<MethodSymbol> GetProviderFactories(NamedTypeSymbol type)
        => GetMemberProvider(type)?.GetMembers("Create").OfType<MethodSymbol>()
            .Where(method => method.IsStatic && method.DeclaredAccessibility == Accessibility.Public &&
                method.TypeParameters.IsDefaultOrEmpty && method.Parameters.Length == 1 &&
                method.Parameters[0].RefKind is RefKind.None or RefKind.In &&
                SymbolEqualityComparer.Default.Equals(method.ReturnType, type))
            .ToImmutableArray() ?? ImmutableArray<MethodSymbol>.Empty;

    // Only the standard ad-hoc carrier has structural alternative-set conversion.
    // Named user unions retain their declared identity.
    public static bool IsStandardCarrierWidening(TypeSymbol source, TypeSymbol destination)
        => source is NamedTypeSymbol from && destination is NamedTypeSymbol to &&
           IsStandardCarrier(from) && IsStandardCarrier(to) &&
           from.TypeArguments.All(part => to.TypeArguments.Any(candidate =>
               SymbolEqualityComparer.Default.Equals(part, candidate)));

    private static bool IsStandardCarrier(NamedTypeSymbol type)
        => type.IsValueType && type.Arity is >= 2 and <= 5 &&
           type.Name == "Union" && type.ContainingNamespace?.ToDisplayString() == "System" &&
           type.TryGetUnion() is not null;

    private const char CaseMetadataSeparator = '_';

    public static bool IsUnionType(TypeSymbol? type)
    {
        return type?.IsUnion == true;
    }

    public static bool IsUnionCaseType(TypeSymbol? type)
    {
        return type?.IsUnionCase == true;
    }

    public static bool TryGetCaseUnionType(TypeSymbol? caseType, out TypeSymbol? unionType)
    {
        unionType = caseType?.UnderlyingUnionType;
        return unionType is not null;
    }

    public static bool UsesCarrierRepresentation(TypeSymbol? type)
    {
        var named = type?.GetNonNullableType() as NamedTypeSymbol;
        var union = named?.TryGetUnion();
        return named is not null && union is not null;
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

    public static string GetCaseParameterName(string declaredName, int ordinal, int parameterCount)
    {
        if (!string.IsNullOrEmpty(declaredName))
            return declaredName;

        if (parameterCount == 1)
            return "value";

        return "item" + (ordinal + 1).ToString(CultureInfo.InvariantCulture);
    }

    public static string GetCaseMetadataBaseName(string unionName, string caseName)
        => $"{unionName}{CaseMetadataSeparator}{caseName}";

    public static bool TryGetLogicalCaseNameFromMetadata(string unionName, string metadataCaseName, out string logicalCaseName)
    {
        var separatedPrefix = unionName + CaseMetadataSeparator;
        if (metadataCaseName.StartsWith(separatedPrefix, StringComparison.Ordinal) &&
            metadataCaseName.Length > separatedPrefix.Length)
        {
            logicalCaseName = StripGenericArity(metadataCaseName.Substring(separatedPrefix.Length));
            return true;
        }

        // Compatibility: support historical metadata names without a separator.
        if (metadataCaseName.StartsWith(unionName, StringComparison.Ordinal) &&
            metadataCaseName.Length > unionName.Length)
        {
            logicalCaseName = StripGenericArity(metadataCaseName.Substring(unionName.Length));
            return true;
        }

        logicalCaseName = StripGenericArity(metadataCaseName);
        return false;
    }

    private static string StripGenericArity(string metadataName)
    {
        var arityIndex = metadataName.IndexOf('`', StringComparison.Ordinal);
        return arityIndex >= 0 ? metadataName[..arityIndex] : metadataName;
    }

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
        out TypeParameterSymbol unionTypeParameter)
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
        SourceUnionCaseTypeSymbol sourceCaseDefinition,
        TypeParameterSymbol caseTypeParameter,
        out TypeParameterSymbol unionTypeParameter)
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

    private static SourceUnionCaseTypeSymbol? TryGetSourceCaseDefinition(UnionCaseTypeSymbol caseSymbol)
    {
        if (caseSymbol is SourceUnionCaseTypeSymbol sourceCase)
            return sourceCase;

        if (caseSymbol is NamedTypeSymbol namedCase &&
            namedCase.OriginalDefinition is SourceUnionCaseTypeSymbol sourceDefinition)
        {
            return sourceDefinition;
        }

        return null;
    }
}
