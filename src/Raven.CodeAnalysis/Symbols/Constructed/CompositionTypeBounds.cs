namespace Raven.CodeAnalysis.Symbols;

internal static class CompositionTypeBounds
{
    internal static bool IsClosedNominalReference(ITypeSymbol type) =>
        type is INamedTypeSymbol { IsReferenceType: true } && IsClosed(type);

    // Generic arguments may be values; the outer constructed type determines
    // reference storage and extendability. Open parameters are not concrete bounds.
    private static bool IsClosed(ITypeSymbol type) => type switch
    {
        INamedTypeSymbol named => !named.IsUnboundGenericType && named.TypeArguments.All(IsClosed) &&
            (named.ContainingType is null || IsClosed(named.ContainingType)),
        IArrayTypeSymbol array => IsClosed(array.ElementType),
        _ => false
    };
}
