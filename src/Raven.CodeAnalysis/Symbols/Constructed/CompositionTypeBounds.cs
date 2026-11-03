namespace Raven.CodeAnalysis.Symbols;

internal static class CompositionTypeBounds
{
    // A union may describe generic and value alternatives before a backend chooses
    // storage. Unbound generic placeholders and Void are not value contracts.
    internal static bool IsUnionAlternative(TypeSymbol type) => type is TypeParameterSymbol ||
        type is NamedTypeSymbol { IsUnboundGenericType: false } && type.SpecialType != SpecialType.System_Void;
}
