using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Symbols;

internal static class DiscriminatedUnionSymbolExtensions
{
    public static IDiscriminatedUnionCaseSymbol? TryGetDiscriminatedUnionCase(this ITypeSymbol? type)
    {
        if (type is null)
            return null;

        if (type is IDiscriminatedUnionCaseSymbol caseSymbol)
            return caseSymbol;

        if (type is INamedTypeSymbol named && named.ConstructedFrom is IDiscriminatedUnionCaseSymbol constructedCase)
            return constructedCase;

        return null;
    }

    public static IDiscriminatedUnionSymbol? TryGetDiscriminatedUnion(this ITypeSymbol? type)
    {
        if (type is null)
            return null;

        if (type is IDiscriminatedUnionSymbol unionSymbol)
            return unionSymbol;

        if (type is INamedTypeSymbol named && named.ConstructedFrom is IDiscriminatedUnionSymbol constructedUnion)
            return constructedUnion;

        return null;
    }
}
