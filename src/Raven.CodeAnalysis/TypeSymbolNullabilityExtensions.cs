using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class TypeSymbolNullabilityExtensions
{
    /// <summary>
    /// Returns the type without Raven's nullable decoration. A type that is not
    /// nullable is returned unchanged.
    /// </summary>
    public static ITypeSymbol GetNonNullableType(this ITypeSymbol typeSymbol)
    {
        ArgumentNullException.ThrowIfNull(typeSymbol);
        return typeSymbol.TryGetNullableUnderlyingType(out var underlyingType)
            ? underlyingType
            : typeSymbol;
    }

    /// <summary>
    /// Attempts to obtain the type wrapped by Raven's nullable decoration.
    /// </summary>
    public static bool TryGetNullableUnderlyingType(
        this ITypeSymbol typeSymbol,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out ITypeSymbol? underlyingType)
    {
        ArgumentNullException.ThrowIfNull(typeSymbol);

        if (!typeSymbol.IsNullable)
        {
            underlyingType = null;
            return false;
        }

        underlyingType = typeSymbol is NullableTypeSymbol nullable
            ? nullable.UnderlyingType
            : typeSymbol.UnderlyingSymbol as ITypeSymbol;
        return underlyingType is not null;
    }

    /// <summary>
    /// Returns the nullable form of the type. A type that is already nullable
    /// is returned unchanged.
    /// </summary>
    public static ITypeSymbol GetNullableType(this ITypeSymbol typeSymbol)
    {
        ArgumentNullException.ThrowIfNull(typeSymbol);
        return typeSymbol.IsNullable
            ? typeSymbol
            : new NullableTypeSymbol(typeSymbol, null, null, null, []);
    }

    internal static ITypeSymbol ApplySubstitutedNullability(
        this ITypeSymbol substitutedType,
        NullableTypeSymbol originalNullableType)
    {
        if (substitutedType.IsNullable)
            return substitutedType;

        return new NullableTypeSymbol(
            substitutedType,
            originalNullableType.ContainingSymbol,
            originalNullableType.ContainingType,
            originalNullableType.ContainingNamespace,
            originalNullableType.Locations.ToArray(),
            originalNullableType.RuntimeProjection);
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
                ? typeSymbol.GetNullableType()
                : typeSymbol;
        }

        return typeSymbol.IsValueType
            ? typeSymbol
            : typeSymbol.GetNullableType();
    }

}
