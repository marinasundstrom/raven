namespace Raven.CodeAnalysis;

public class TypeInfo
{
    internal TypeInfo(ITypeSymbol type, ITypeSymbol? convertedType)
    {
        Type = type;
        ConvertedType = convertedType;
    }

    public NullabilityInfo ConvertedNullability { get; }

    public ITypeSymbol? ConvertedType { get; }

    public NullabilityInfo Nullability { get; }

    public ITypeSymbol? Type { get; }
}