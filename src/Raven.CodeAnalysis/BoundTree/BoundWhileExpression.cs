namespace Raven.CodeAnalysis;

internal class BoundWhileExpression : BoundExpression
{
    public BoundExpression Condition { get; }
    public BoundExpression Body { get; }

    public BoundWhileExpression(BoundExpression condition, BoundExpression body)
        : base(condition.Type.Compilation.GetSpecialType(SpecialType.System_Void), null, BoundExpressionReason.None)
    {
        Condition = condition;
        Body = body;
    }
}
