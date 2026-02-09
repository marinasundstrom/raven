namespace Raven.CodeAnalysis;

public class TypeInfo
{
    internal TypeInfo(ITypeSymbol? type, ITypeSymbol? convertedType, Conversion conversion = default)
    {
        Type = type;
        ConvertedType = convertedType;
        Conversion = conversion;
        Nullability = CreateNullabilityInfo(type);
        ConvertedNullability = CreateNullabilityInfo(convertedType);
    }

    public Conversion Conversion { get; }

    public NullabilityInfo ConvertedNullability { get; }

    public ITypeSymbol? ConvertedType { get; }

    public NullabilityInfo Nullability { get; }

    public ITypeSymbol? Type { get; }

    private static NullabilityInfo CreateNullabilityInfo(ITypeSymbol? typeSymbol)
    {
        if (typeSymbol is null)
            return new NullabilityInfo(NullableAnnotation.None, NullableFlowState.None);

        if (typeSymbol.IsNullable)
            return new NullabilityInfo(NullableAnnotation.Annotated, NullableFlowState.MaybeNull);

        return new NullabilityInfo(NullableAnnotation.NotAnnotated, NullableFlowState.NotNull);
    }
}
