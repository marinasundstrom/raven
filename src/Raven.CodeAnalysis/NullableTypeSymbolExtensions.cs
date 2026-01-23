using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class NullableTypeSymbolExtensions
{
    public static ITypeSymbol GetPlainType(this ITypeSymbol typeSymbol)
    {
        if (typeSymbol is NullableTypeSymbol { UnderlyingType: var underlying })
        {
            return underlying;
        }

        return typeSymbol;
    }

    public static ITypeSymbol MakeNullable(this ITypeSymbol typeSymbol)
    {
        if (typeSymbol.IsNullable)
        {
            throw new InvalidOperationException($"Type '{0}' is already a nullable type");
        }

        return new NullableTypeSymbol(typeSymbol, null, null, null, []);
    }

    public static ITypeSymbol? StripNullable(this ITypeSymbol typeSymbol)
    {
        if (!typeSymbol.IsNullable)
        {
            return null;
        }

        return typeSymbol is NullableTypeSymbol nullable
            ? nullable.UnderlyingType
            : (ITypeSymbol)typeSymbol.UnderlyingSymbol;
    }

    public static ITypeSymbol UnwrapNullableOrThrow(this ITypeSymbol typeSymbol)
    {
        return typeSymbol.StripNullable() ?? throw new InvalidOperationException($"Type '{0}' is not a nullable type");
    }
}
