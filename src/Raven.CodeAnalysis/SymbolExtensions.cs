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

    public static INamedTypeSymbol? GetAbsoluteBaseType(this ITypeSymbol symbol)
    {
        INamedTypeSymbol? baseType = symbol.BaseType;

        if (baseType is null) return (INamedTypeSymbol?)symbol;

        while (baseType?.BaseType is not null)
        {
            baseType = baseType.BaseType;
        }

        return baseType;
    }

    public static IEnumerable<ISymbol> ResolveMembers(this ITypeSymbol symbol, string name)
    {
        for (ITypeSymbol? current = symbol; current is not null; current = current.BaseType)
        {
            var members = current.GetMembers();
            if (members.Length > 0)
            {
                foreach (var member in members)
                {
                    if (member.Name == name)
                    {
                        yield return member;
                    }
                }
            }
        }
    }
}