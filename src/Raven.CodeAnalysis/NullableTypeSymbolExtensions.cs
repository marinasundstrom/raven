using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class NullableTypeSymbolExtensions
{
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

    public static ITypeSymbol GetPlainType(this ITypeSymbol typeSymbol)
    {
        return typeSymbol is NullableTypeSymbol nullable
            ? nullable.UnderlyingType
            : typeSymbol;
    }

    public static ITypeSymbol EffectiveNullableType(this ITypeSymbol typeSymbol, Compilation compilation)
    {
        if (typeSymbol is not NullableTypeSymbol nullable)
            return typeSymbol;

        if (!nullable.UnderlyingType.IsValueType)
            return typeSymbol;

        var nullableDefinition = compilation.GetSpecialType(SpecialType.System_Nullable_T);
        if (nullableDefinition.TypeKind == TypeKind.Error)
            return typeSymbol;

        return nullableDefinition.Construct(nullable.UnderlyingType);
    }
}
