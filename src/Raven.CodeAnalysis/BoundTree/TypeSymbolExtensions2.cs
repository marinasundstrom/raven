using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class TypeSymbolExtensions2
{
    //public static bool IsByRef(this ITypeSymbol type) => type.TypeKind == TypeKind.ByRef;

    public static ITypeSymbol? GetElementType(this ITypeSymbol type)
        => type is ByRefTypeSymbol byRef ? byRef.ElementType : null;
}