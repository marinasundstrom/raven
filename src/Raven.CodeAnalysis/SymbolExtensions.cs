namespace Raven.CodeAnalysis;

public static partial class SymbolExtensions
{
    /// <summary>
    /// Unwraps the actual type of a property symbol or local symbol. 
    /// If the symbol is a type symbol, then return it, otherwise null.
    /// </summary>
    /// <param name="symbol"></param>
    /// <returns></returns>
    public static ITypeSymbol? UnwrapType(this ISymbol symbol)
    {
        if (symbol is IPropertySymbol propertySymbol) return propertySymbol.Type;

        if (symbol is IFieldSymbol fieldSymbol) return fieldSymbol.Type;

        if (symbol is IParameterSymbol parameterSymbol) return parameterSymbol.Type;

        if (symbol is ILocalSymbol localSymbol) return localSymbol.Type;

        return symbol as ITypeSymbol;
    }
}