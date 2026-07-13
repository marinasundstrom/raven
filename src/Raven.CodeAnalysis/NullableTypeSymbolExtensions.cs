using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class NullableTypeSymbolExtensions
{
    public static ITypeSymbol GetPlainType(this ITypeSymbol typeSymbol)
    {
        return typeSymbol.StripNullable() ?? typeSymbol;
    }

    public static ITypeSymbol MakeNullable(this ITypeSymbol typeSymbol)
    {
        if (typeSymbol.IsNullable)
        {
            throw new InvalidOperationException($"Type '{0}' is already a nullable type");
        }

        return new NullableTypeSymbol(typeSymbol, null, null, null, []);
    }

    internal static ITypeSymbol GetDefaultValueType(this ITypeSymbol typeSymbol)
    {
        if (typeSymbol.IsNullable ||
            typeSymbol.TypeKind is TypeKind.Error or TypeKind.Null)
        {
            return typeSymbol;
        }

        if (typeSymbol is ITypeParameterSymbol typeParameter)
        {
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0
                ? typeSymbol.MakeNullable()
                : typeSymbol;
        }

        return typeSymbol.IsValueType
            ? typeSymbol
            : typeSymbol.MakeNullable();
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
