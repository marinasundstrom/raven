namespace Raven.CodeAnalysis.Symbols;

internal static class PESymbolRegistrationExtensions
{
    public static T AddAsMember<T>(this T symbol)
        where T : PENamedTypeSymbol
    {
        symbol.AddAsMember(symbol.ContainingType, symbol.ContainingNamespace);
        return symbol;
    }

    public static T AddAsMember2<T>(this T symbol)
        where T : PETypeParameterSymbol
    {
        symbol.AddAsMember(symbol.ContainingType, symbol.ContainingNamespace);
        return symbol;
    }
}
