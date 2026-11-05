namespace Raven.CodeAnalysis;

public record class TypeInfo
{
    internal TypeInfo(TypeSymbol? type, TypeSymbol? convertedType, Conversion conversion = default)
    {
        Type = type;
        ConvertedType = convertedType;
        Conversion = conversion;
    }

    public Conversion Conversion { get; }

    public TypeSymbol? ConvertedType { get; }

    public TypeSymbol? Type { get; }
}
