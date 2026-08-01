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
    /// Returns a type with the requested declared nullable annotation.
    /// Flow state is reported separately by <see cref="TypeInfo.Nullability"/>.
    /// </summary>
    /// <exception cref="ArgumentOutOfRangeException">
    /// <paramref name="annotation"/> is <see cref="NullableAnnotation.None"/>,
    /// which does not describe a concrete type in Raven's unified nullability model.
    /// </exception>
    public static ITypeSymbol WithNullableAnnotation(
        this ITypeSymbol typeSymbol,
        NullableAnnotation annotation)
    {
        ArgumentNullException.ThrowIfNull(typeSymbol);

        return annotation switch
        {
            NullableAnnotation.Annotated => typeSymbol.IsNullable
                ? typeSymbol
                : new NullableTypeSymbol(typeSymbol, null, null, null, []),
            NullableAnnotation.NotAnnotated => typeSymbol.GetNonNullableType(),
            _ => throw new ArgumentOutOfRangeException(nameof(annotation), annotation, "A concrete type requires an explicit nullable annotation."),
        };
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
                ? typeSymbol.WithNullableAnnotation(NullableAnnotation.Annotated)
                : typeSymbol;
        }

        return typeSymbol.IsValueType
            ? typeSymbol
            : typeSymbol.WithNullableAnnotation(NullableAnnotation.Annotated);
    }

}
