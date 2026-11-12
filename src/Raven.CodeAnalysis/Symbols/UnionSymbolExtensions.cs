using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal static partial class UnionSymbolExtensions
{
    internal static bool TryFindUnionCaseType(
        this NamedTypeSymbol targetType,
        string caseName,
        out NamedTypeSymbol caseType)
    {
        caseType = null!;

        var normalizedType = targetType.UnwrapLiteralType() ?? targetType;
        normalizedType = normalizedType.GetNonNullableType();
        if (normalizedType is not NamedTypeSymbol namedType)
            return false;

        var unionSymbol = namedType.TryGetUnion()
            ?? namedType.TryGetUnionCase()?.Union;
        if (unionSymbol is null)
            return false;

        var unionCarrier = namedType.TryGetUnion() is not null
            ? namedType
            : unionSymbol as NamedTypeSymbol;
        if (unionCarrier is null)
            return false;

        if (!TryFindUnionCaseDefinition(unionSymbol, caseName, out var caseDefinition) ||
            caseDefinition is not NamedTypeSymbol namedCaseDefinition)
        {
            return false;
        }

        caseType = ProjectCaseTypeToUnionArguments(namedCaseDefinition, unionCarrier);
        return true;
    }

    private static bool TryFindUnionCaseDefinition(
        UnionSymbol unionSymbol,
        string caseName,
        out UnionCaseTypeSymbol caseType)
    {
        if (unionSymbol is PEUnionSymbol peUnion &&
            peUnion.TryGetDeclaredCaseType(caseName, out caseType))
        {
            return true;
        }

        if (unionSymbol is NamedTypeSymbol unionNamed &&
            TypeSubstitution.GetDefinitionForSubstitution(unionNamed) is PEUnionSymbol peUnionDefinition &&
            peUnionDefinition.TryGetDeclaredCaseType(caseName, out caseType))
        {
            return true;
        }

        caseType = unionSymbol.Variants
            .OfType<UnionCaseTypeSymbol>()
            .FirstOrDefault(@case => string.Equals(@case.Name, caseName, System.StringComparison.Ordinal))!;
        return caseType is not null;
    }

    private static NamedTypeSymbol ProjectCaseTypeToUnionArguments(
        NamedTypeSymbol caseType,
        NamedTypeSymbol unionType)
    {
        if (!caseType.IsGenericType || caseType.TypeParameters.IsDefaultOrEmpty)
            return caseType;

        var unionDefinition = unionType.TryGetUnion() ?? unionType;
        var unionTypeParameters = unionDefinition.TypeParameters;
        var unionTypeArguments = unionType.TypeArguments;

        if (unionTypeParameters.IsDefaultOrEmpty || unionTypeArguments.IsDefaultOrEmpty)
            return caseType;

        var caseSymbol = caseType.TryGetUnionCase();
        if (caseSymbol is null)
            return caseType;

        var projectedArguments = new TypeSymbol[caseType.TypeParameters.Length];
        var changed = false;
        for (var i = 0; i < caseType.TypeParameters.Length; i++)
        {
            var parameter = caseType.TypeParameters[i];
            if (Raven.CodeAnalysis.UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(
                    caseSymbol,
                    parameter,
                    unionTypeParameters,
                    unionTypeArguments,
                    out var mapped))
            {
                projectedArguments[i] = mapped;
                if (!AreEquivalentGenericInstantiationArgument(mapped, parameter))
                    changed = true;
            }
            else
            {
                projectedArguments[i] = parameter;
            }
        }

        return changed && caseType.Construct(projectedArguments) is NamedTypeSymbol projectedCase
            ? projectedCase
            : caseType;
    }

    private static bool AreEquivalentGenericInstantiationArgument(TypeSymbol candidate, TypeSymbol parameter)
    {
        if (SymbolEqualityComparer.Default.Equals(candidate, parameter))
            return true;

        if (candidate is TypeParameterSymbol candidateParameter &&
            parameter is TypeParameterSymbol expectedParameter)
        {
            return string.Equals(candidateParameter.Name, expectedParameter.Name, System.StringComparison.Ordinal);
        }

        return false;
    }

    public static bool TryGetUnionCarrierConstructor(
        this NamedTypeSymbol unionType,
        TypeSymbol caseType,
        out MethodSymbol constructor)
    {
        var caseDefinition = caseType.OriginalDefinition ?? caseType;

        foreach (var candidate in unionType.Constructors)
        {
            if (candidate.IsStatic || candidate.Parameters.Length != 1)
                continue;

            var parameterDefinition = candidate.Parameters[0].Type.OriginalDefinition ?? candidate.Parameters[0].Type;
            if (parameterDefinition.MetadataIdentityEquals(caseDefinition))
            {
                if (unionType is ConstructedNamedTypeSymbol constructedUnion)
                {
                    var definition = candidate switch
                    {
                        SubstitutedMethodSymbol substituted => substituted.OriginalDefinition ?? candidate,
                        ConstructedMethodSymbol constructed => constructed.Definition,
                        _ => candidate,
                    };

                    constructor = new SubstitutedMethodSymbol(definition, constructedUnion);
                }
                else
                {
                    constructor = candidate;
                }

                return true;
            }
        }

        constructor = null!;
        return false;
    }
}
