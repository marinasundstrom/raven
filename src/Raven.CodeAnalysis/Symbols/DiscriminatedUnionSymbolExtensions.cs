using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Symbols;

internal static class DiscriminatedUnionSymbolExtensions
{
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
