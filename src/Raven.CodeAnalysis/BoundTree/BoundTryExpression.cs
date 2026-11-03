using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundTryExpression : BoundExpression
{
    public BoundTryExpression(
        BoundExpression expression,
        TypeSymbol exceptionType,
        TypeSymbol type,
        MethodSymbol okConstructor,
        MethodSymbol errorConstructor)
        : base(type, null, BoundExpressionReason.None)
    {
        Expression = expression;
        ExceptionType = exceptionType;
        OkConstructor = okConstructor;
        ErrorConstructor = errorConstructor;
    }

    public BoundExpression Expression { get; }
    public TypeSymbol ExceptionType { get; }
    public MethodSymbol OkConstructor { get; }
    public MethodSymbol ErrorConstructor { get; }
}
