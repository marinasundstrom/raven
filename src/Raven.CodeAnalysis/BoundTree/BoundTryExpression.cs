using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundTryExpression : BoundExpression
{
    public BoundTryExpression(BoundExpression expression, ITypeSymbol exceptionType, ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
        Expression = expression;
        ExceptionType = exceptionType;
    }

    public BoundExpression Expression { get; }
    public ITypeSymbol ExceptionType { get; }
}
