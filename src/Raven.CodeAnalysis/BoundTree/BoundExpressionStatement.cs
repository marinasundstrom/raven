namespace Raven.CodeAnalysis;

sealed partial class BoundExpressionStatement : BoundStatement
{
    public BoundExpression Expression { get; }

    public BoundExpressionStatement(BoundExpression expression) => Expression = expression;

    public override ISymbol Symbol => Expression.Symbol;
    public override ITypeSymbol Type => Expression.Type;
}
