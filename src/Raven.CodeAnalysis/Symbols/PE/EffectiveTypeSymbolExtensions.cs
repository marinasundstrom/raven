namespace Raven.CodeAnalysis.Symbols;

internal static class EffectiveTypeSymbolExtensions
{
    public static ITypeSymbol GetEffectiveType(this IParameterSymbol parameter)
        => parameter.RefKind is RefKind.None
            ? parameter.Type
            : new ByRefTypeSymbol(parameter.Type); // however you construct it

    public static ITypeSymbol GetEffectiveType(this ILocalSymbol local)
        => local.RefKind is RefKind.None
            ? local.Type
            : new ByRefTypeSymbol(local.Type);
}
