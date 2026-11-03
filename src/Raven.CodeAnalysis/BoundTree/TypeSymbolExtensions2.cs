using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class TypeSymbolExtensions2
{
    //public static bool IsByRef(this TypeSymbol type) => type.TypeKind == TypeKind.ByRef;

    public static TypeSymbol? GetElementType(this TypeSymbol type)
    {
        if (type is ArrayTypeSymbolImplementation arrayType)
            return arrayType.ElementType;

        if (type is PointerTypeSymbol pointer)
            return pointer.PointedAtType;

        if (type is AddressTypeSymbol address)
            return address.ReferencedType;

        return type is RefTypeSymbol refType ? refType.ElementType : null;
    }
}
