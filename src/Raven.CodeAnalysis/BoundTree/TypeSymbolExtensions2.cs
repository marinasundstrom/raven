using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class TypeSymbolExtensions2
{
    public static TypeSymbol? GetElementType(this TypeSymbol? type)
        => type switch
        {
            ArrayTypeSymbol array => array.ElementType,
            PointerTypeSymbol pointer => pointer.PointedAtType,
            AddressTypeSymbol address => address.ReferencedType,
            IByRefTypeInfo reference => reference.ElementType,
            _ => null,
        };
}
