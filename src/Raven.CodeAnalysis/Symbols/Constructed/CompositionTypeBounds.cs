namespace Raven.CodeAnalysis.Symbols;

internal static class CompositionTypeBounds
{
    // A union may describe generic and value alternatives before a backend chooses
    // storage. Unbound generic placeholders and Void are not value contracts.
    internal static bool IsUnionAlternative(ITypeSymbol type) => type is ITypeParameterSymbol ||
        type is INamedTypeSymbol { IsUnboundGenericType: false } && type.SpecialType != SpecialType.System_Void;
}
