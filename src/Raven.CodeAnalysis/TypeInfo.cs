namespace Raven.CodeAnalysis;

public class TypeInfo
{
    internal TypeInfo(ITypeSymbol? type, ITypeSymbol? convertedType, Conversion conversion = default)
        : this(type, convertedType, conversion, type, convertedType)
    {
    }

    internal TypeInfo(
        ITypeSymbol? type,
        ITypeSymbol? convertedType,
        Conversion conversion,
        ITypeSymbol? flowType,
        ITypeSymbol? convertedFlowType)
    {
        Type = type;
        ConvertedType = convertedType;
        Conversion = conversion;
        Nullability = CreateNullabilityInfo(type, flowType);
        ConvertedNullability = CreateNullabilityInfo(convertedType, convertedFlowType);
    }

    public Conversion Conversion { get; }

    public NullabilityInfo ConvertedNullability { get; }

    public ITypeSymbol? ConvertedType { get; }

    public NullabilityInfo Nullability { get; }

    public ITypeSymbol? Type { get; }

    private static NullabilityInfo CreateNullabilityInfo(ITypeSymbol? typeSymbol, ITypeSymbol? flowType)
    {
        if (typeSymbol is null)
            return new NullabilityInfo(NullableAnnotation.None, NullableFlowState.None);

        var annotation = typeSymbol.IsNullable
            ? NullableAnnotation.Annotated
            : NullableAnnotation.NotAnnotated;
        var flowState = flowType is null
            ? typeSymbol.IsNullable ? NullableFlowState.MaybeNull : NullableFlowState.NotNull
            : flowType.IsNullable ? NullableFlowState.MaybeNull : NullableFlowState.NotNull;

        return new NullabilityInfo(annotation, flowState);
    }
}
