using System.Collections.Generic;
using System.Linq;
using System.Text;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Symbols;

internal static class DiscriminatedUnionSymbolExtensions
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
        var unionCase = caseType.TryGetDiscriminatedUnionCase();
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
        var firstUnion  = first.TryGetDiscriminatedUnionCase()?.Union  as INamedTypeSymbol;
        var secondUnion = second.TryGetDiscriminatedUnionCase()?.Union as INamedTypeSymbol;

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

    public static IDiscriminatedUnionCaseSymbol? TryGetDiscriminatedUnionCase(this ITypeSymbol? type)
    {
        if (type is null)
            return null;

        if (type is IDiscriminatedUnionCaseSymbol caseSymbol && type.IsDiscriminatedUnionCase)
            return caseSymbol;

        if (type is INamedTypeSymbol named && named.IsDiscriminatedUnionCase)
            return named switch
            {
                IDiscriminatedUnionCaseSymbol constructedCase => constructedCase,
                _ when named.ConstructedFrom is IDiscriminatedUnionCaseSymbol constructedDefinition => constructedDefinition,
                _ => null,
            };

        return null;
    }

    public static IDiscriminatedUnionSymbol? TryGetDiscriminatedUnion(this ITypeSymbol? type)
    {
        if (type is null)
            return null;

        if (type is IDiscriminatedUnionSymbol unionSymbol && type.IsDiscriminatedUnion)
            return unionSymbol;

        if (type is INamedTypeSymbol named && named.IsDiscriminatedUnion)
            return named switch
            {
                IDiscriminatedUnionSymbol constructedUnion => constructedUnion,
                _ when named.ConstructedFrom is IDiscriminatedUnionSymbol constructedDefinition => constructedDefinition,
                _ => null,
            };

        return null;
    }
}
