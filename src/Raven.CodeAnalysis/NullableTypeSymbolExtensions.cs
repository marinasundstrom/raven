using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class NullableTypeSymbolExtensions
{
    public static ITypeSymbol MakeNullable(this ITypeSymbol typeSymbol)
    {
        if (typeSymbol.IsNullable)
        {
            return typeSymbol;
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

    public static ITypeSymbol GetPlainType(this ITypeSymbol typeSymbol)
    {
        var current = typeSymbol;

        while (current is NullableTypeSymbol nullable)
        {
            current = nullable.UnderlyingType;
        }

        return current;
    }

    public static ITypeSymbol EffectiveNullableType(this ITypeSymbol typeSymbol, Compilation compilation)
    {
        if (typeSymbol is not NullableTypeSymbol nullable)
            return typeSymbol;

        var plainUnderlying = nullable.UnderlyingType.GetPlainType();

        if (!plainUnderlying.IsValueType)
            return plainUnderlying;

        var nullableDefinition = compilation.GetSpecialType(SpecialType.System_Nullable_T);
        if (nullableDefinition.TypeKind == TypeKind.Error)
            return typeSymbol;

        return nullableDefinition.Construct(plainUnderlying);
    }
}
