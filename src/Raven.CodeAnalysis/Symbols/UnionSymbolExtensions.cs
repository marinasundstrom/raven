using System.Collections.Generic;
using System.Linq;
using System.Text;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Symbols;

internal static class UnionSymbolExtensions
{
    /// <summary>
    /// Formats a discriminated union case type for use in diagnostic messages as
    /// <c>[Namespace.]UnionName&lt;TypeArgs&gt;.CaseName</c>.
    /// </summary>
    /// <param name="caseType">The case type to format.</param>
    /// <param name="includeNamespace">
    /// When <see langword="true"/> the containing namespace of the union carrier is prepended,
    /// e.g. <c>System.Result&lt;T, E&gt;.Error</c> vs <c>Result&lt;T, E&gt;.Error</c>.
    /// Pass <see langword="true"/> when two ambiguous candidates have the same union name but
    /// live in different namespaces.
    /// </param>
    public static string FormatUnionCaseForDiagnostic(
        this INamedTypeSymbol caseType,
        bool includeNamespace = false)
    {
        var unionCase = caseType.TryGetUnionCase();
        if (unionCase is null)
            return caseType.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat);

        if (unionCase.Union is not INamedTypeSymbol union)
            return caseType.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat);

        var sb = new StringBuilder();

        // Optionally prepend the namespace of the union carrier.
        if (includeNamespace && union.ContainingNamespace is { IsGlobalNamespace: false } ns)
        {
            sb.Append(FormatNamespace(ns));
            sb.Append('.');
        }

        sb.Append(union.Name);

        // Append type arguments — concrete if available, otherwise parameter names.
        var typeArgs = GetUnionTypeArgsForDisplay(caseType, union);
        if (typeArgs.Count > 0)
        {
            sb.Append('<');
            sb.Append(string.Join(", ", typeArgs));
            sb.Append('>');
        }

        sb.Append('.');
        sb.Append(caseType.Name);

        return sb.ToString();
    }

    /// <summary>
    /// Chooses display names for two ambiguous case candidates, automatically including
    /// namespaces when both candidates' union carriers share the same short name.
    /// </summary>
    public static (string First, string Second) FormatAmbiguousCasePair(
        INamedTypeSymbol first, INamedTypeSymbol second)
    {
        var firstUnion = first.TryGetUnionCase()?.Union as INamedTypeSymbol;
        var secondUnion = second.TryGetUnionCase()?.Union as INamedTypeSymbol;

        // If both carriers share the same short name we need the namespace to distinguish them.
        bool includeNs = string.Equals(firstUnion?.Name, secondUnion?.Name, System.StringComparison.Ordinal);

        return (
            first.FormatUnionCaseForDiagnostic(includeNs),
            second.FormatUnionCaseForDiagnostic(includeNs)
        );
    }

    // ── helpers ──────────────────────────────────────────────────────────────

    private static List<string> GetUnionTypeArgsForDisplay(INamedTypeSymbol caseType, INamedTypeSymbol union)
    {
        if (union.TypeParameters.IsDefaultOrEmpty || union.TypeParameters.Length == 0)
            return [];

        // If the case type carries concrete (non-type-parameter) type arguments, use them.
        if (!caseType.TypeArguments.IsDefaultOrEmpty &&
            caseType.TypeArguments.Length > 0 &&
            caseType.TypeArguments.Any(static t => t.TypeKind != TypeKind.TypeParameter))
        {
            return caseType.TypeArguments
                .Select(static a => a.ToDisplayString(SymbolDisplayFormat.RavenErrorMessageFormat))
                .ToList();
        }

        // Fall back to the union's type parameter names.
        return union.TypeParameters
            .Select(static p => p.Name)
            .ToList();
    }

    private static string FormatNamespace(INamespaceSymbol ns)
    {
        var parts = new List<string>();
        var current = ns;
        while (current is { IsGlobalNamespace: false })
        {
            parts.Add(current.Name);
            current = current.ContainingNamespace;
        }
        parts.Reverse();
        return string.Join(".", parts);
    }

    public static IUnionCaseTypeSymbol? TryGetUnionCase(this ITypeSymbol? type)
    {
        if (type is null)
            return null;

        if (type is IUnionCaseTypeSymbol caseSymbol && type.IsUnionCase)
            return caseSymbol;

        if (type is INamedTypeSymbol named && named.IsUnionCase)
            return named switch
            {
                IUnionCaseTypeSymbol constructedCase => constructedCase,
                _ when named.ConstructedFrom is IUnionCaseTypeSymbol constructedDefinition => constructedDefinition,
                _ => null,
            };

        return null;
    }

    public static IUnionSymbol? TryGetUnion(this ITypeSymbol? type)
    {
        if (type is null)
            return null;

        if (type is IUnionSymbol unionSymbol && type.IsUnion)
            return unionSymbol;

        if (type is INamedTypeSymbol named && named.IsUnion)
            return named switch
            {
                IUnionSymbol constructedUnion => constructedUnion,
                _ when named.ConstructedFrom is IUnionSymbol constructedDefinition => constructedDefinition,
                _ => null,
            };

        return null;
    }

    internal static bool TryFindUnionCaseType(
        this INamedTypeSymbol targetType,
        string caseName,
        out INamedTypeSymbol caseType)
    {
        caseType = null!;

        var normalizedType = targetType.UnwrapLiteralType() ?? targetType;
        normalizedType = normalizedType.GetPlainType();
        if (normalizedType is not INamedTypeSymbol namedType)
            return false;

        var unionSymbol = namedType.TryGetUnion()
            ?? namedType.TryGetUnionCase()?.Union;
        if (unionSymbol is null)
            return false;

        var unionCarrier = namedType.TryGetUnion() is not null
            ? namedType
            : unionSymbol as INamedTypeSymbol;
        if (unionCarrier is null)
            return false;

        if (!TryFindUnionCaseDefinition(unionSymbol, caseName, out var caseDefinition) ||
            caseDefinition is not INamedTypeSymbol namedCaseDefinition)
        {
            return false;
        }

        caseType = ProjectCaseTypeToUnionArguments(namedCaseDefinition, unionCarrier);
        return true;
    }

    private static bool TryFindUnionCaseDefinition(
        IUnionSymbol unionSymbol,
        string caseName,
        out IUnionCaseTypeSymbol caseType)
    {
        if (unionSymbol is PEUnionSymbol peUnion &&
            peUnion.TryGetDeclaredCaseType(caseName, out caseType))
        {
            return true;
        }

        if (unionSymbol is INamedTypeSymbol unionNamed &&
            TypeSubstitution.GetDefinitionForSubstitution(unionNamed) is PEUnionSymbol peUnionDefinition &&
            peUnionDefinition.TryGetDeclaredCaseType(caseName, out caseType))
        {
            return true;
        }

        caseType = unionSymbol.CaseTypes
            .OfType<IUnionCaseTypeSymbol>()
            .FirstOrDefault(@case => string.Equals(@case.Name, caseName, System.StringComparison.Ordinal))!;
        return caseType is not null;
    }

    private static INamedTypeSymbol ProjectCaseTypeToUnionArguments(
        INamedTypeSymbol caseType,
        INamedTypeSymbol unionType)
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

        var projectedArguments = new ITypeSymbol[caseType.TypeParameters.Length];
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

        return changed && caseType.Construct(projectedArguments) is INamedTypeSymbol projectedCase
            ? projectedCase
            : caseType;
    }

    private static bool AreEquivalentGenericInstantiationArgument(ITypeSymbol candidate, ITypeSymbol parameter)
    {
        if (SymbolEqualityComparer.Default.Equals(candidate, parameter))
            return true;

        if (candidate is ITypeParameterSymbol candidateParameter &&
            parameter is ITypeParameterSymbol expectedParameter)
        {
            return string.Equals(candidateParameter.Name, expectedParameter.Name, System.StringComparison.Ordinal);
        }

        return false;
    }

    public static bool TryGetUnionCarrierConstructor(
        this INamedTypeSymbol unionType,
        ITypeSymbol caseType,
        out IMethodSymbol constructor)
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
