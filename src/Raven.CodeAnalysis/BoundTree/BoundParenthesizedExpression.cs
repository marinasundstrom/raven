namespace Raven.CodeAnalysis;

internal partial class BoundParenthesizedExpression : BoundExpression
{
    public BoundExpression Expression { get; }

    public BoundParenthesizedExpression(BoundExpression expression)
        : base(expression.Type!, expression.Symbol, expression.Reason)
    {
        Expression = expression;
    }

    //public override BoundNodeKind Kind => BoundNodeKind.ParenthesizedExpression;
}