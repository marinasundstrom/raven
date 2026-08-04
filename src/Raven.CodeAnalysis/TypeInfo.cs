namespace Raven.CodeAnalysis;

public class TypeInfo
{
    internal TypeInfo(ITypeSymbol? type, ITypeSymbol? convertedType, Conversion conversion = default)
    {
        Type = type;
        ConvertedType = convertedType;
        Conversion = conversion;
    }

    public Conversion Conversion { get; }

    public ITypeSymbol? ConvertedType { get; }

    public ITypeSymbol? Type { get; }
}
