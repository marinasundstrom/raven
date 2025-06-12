using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundAddressOfExpression : BoundExpression
{
    public BoundAddressOfExpression(ISymbol symbol, ITypeSymbol valueType)
        : base(new ByRefTypeSymbol(valueType), symbol)
    {
    }

    public ITypeSymbol ReferencedType => ((ByRefTypeSymbol)Type).ElementType;

    public ITypeSymbol ValueType => ReferencedType;
}
