using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class TypeSymbolExtensions2
{
    //public static bool IsByRef(this ITypeSymbol type) => type.TypeKind == TypeKind.ByRef;

    public static ITypeSymbol? GetElementType(this ITypeSymbol type)
    {
        if (type is ArrayTypeSymbol arrayType)
            return arrayType.ElementType;

        if (type is IPointerTypeSymbol pointer)
            return pointer.PointedAtType;

        if (type is IAddressTypeSymbol address)
            return address.ReferencedType;

        return type is ByRefTypeSymbol byRef ? byRef.ElementType : null;
    }
}
