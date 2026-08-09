using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

/// <summary>
/// Describes how a Raven nullable type is represented in a CLR signature.
/// </summary>
public enum NullableAbiProjection
{
    /// <summary>
    /// The type is not nullable in Raven's semantic model.
    /// </summary>
    None,

    /// <summary>
    /// The nullable type uses its underlying CLR type. This is the projection
    /// used by nullable reference types and nullable unconstrained type
    /// parameters.
    /// </summary>
    AnnotatedUnderlyingType,

    /// <summary>
    /// The nullable type uses <c>System.Nullable&lt;T&gt;</c> in CLR signatures.
    /// </summary>
    NullableValueType,
}

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

    /// <summary>
    /// Returns the CLR ABI projection selected for a Raven nullable type. The
    /// result is <see cref="NullableAbiProjection.None"/> when the type is not
    /// nullable.
    /// </summary>
    public static NullableAbiProjection GetNullableAbiProjection(this ITypeSymbol typeSymbol)
    {
        ArgumentNullException.ThrowIfNull(typeSymbol);

        if (!typeSymbol.TryGetNullableUnderlyingType(out var underlyingType))
            return NullableAbiProjection.None;

        if (typeSymbol is NullableTypeSymbol nullableType)
            return nullableType.AbiProjection;

        return InferNullableAbiProjection(underlyingType);
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
            originalNullableType.AbiProjection);
    }

    internal static NullableAbiProjection InferNullableAbiProjection(ITypeSymbol underlyingType)
    {
        if (underlyingType is ITypeParameterSymbol typeParameter)
        {
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0
                ? NullableAbiProjection.NullableValueType
                : NullableAbiProjection.AnnotatedUnderlyingType;
        }

        return IsKnownValueType(underlyingType)
            ? NullableAbiProjection.NullableValueType
            : NullableAbiProjection.AnnotatedUnderlyingType;
    }

    private static bool IsKnownValueType(ITypeSymbol typeSymbol)
        => typeSymbol.IsValueType ||
           typeSymbol.SpecialType is SpecialType.System_Boolean
               or SpecialType.System_Char
               or SpecialType.System_SByte
               or SpecialType.System_Byte
               or SpecialType.System_Int16
               or SpecialType.System_UInt16
               or SpecialType.System_Int32
               or SpecialType.System_UInt32
               or SpecialType.System_Int64
               or SpecialType.System_UInt64
               or SpecialType.System_Decimal
               or SpecialType.System_Single
               or SpecialType.System_Double
               or SpecialType.System_IntPtr
               or SpecialType.System_UIntPtr
               or SpecialType.System_DateTime;

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
