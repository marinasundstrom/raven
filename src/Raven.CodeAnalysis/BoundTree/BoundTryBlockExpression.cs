using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundTryBlockExpression : BoundExpression
{
    public BoundTryBlockExpression(BoundBlockExpression block, ITypeSymbol exceptionType, ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
        Block = block;
        ExceptionType = exceptionType;
    }

    public BoundBlockExpression Block { get; }
    public ITypeSymbol ExceptionType { get; }
}
