using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundTryExpression : BoundExpression
{
    public BoundTryExpression(
        BoundExpression expression,
        ITypeSymbol exceptionType,
        ITypeSymbol type,
        IMethodSymbol okConstructor,
        IMethodSymbol errorConstructor)
        : base(type, null, BoundExpressionReason.None)
    {
        Expression = expression;
        ExceptionType = exceptionType;
        OkConstructor = okConstructor;
        ErrorConstructor = errorConstructor;
    }

    public BoundExpression Expression { get; }
    public ITypeSymbol ExceptionType { get; }
    public IMethodSymbol OkConstructor { get; }
    public IMethodSymbol ErrorConstructor { get; }
}
