using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class BoundAddressOfExpression : BoundExpression
{
    public BoundAddressOfExpression(ISymbol symbol, ITypeSymbol valueType)
        : base(new ByRefTypeSymbol(valueType), symbol)
    {
    }

    public ITypeSymbol ReferencedType => ((ByRefTypeSymbol)Type).ElementType;
}

public static class TypeSymbolExtensions2
{
    //public static bool IsByRef(this ITypeSymbol type) => type.TypeKind == TypeKind.ByRef;

    public static ITypeSymbol? GetElementType(this ITypeSymbol type)
        => type is ByRefTypeSymbol byRef ? byRef.ElementType : null;
}