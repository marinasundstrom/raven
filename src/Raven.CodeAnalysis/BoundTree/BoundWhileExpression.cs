namespace Raven.CodeAnalysis;

internal partial class BoundWhileExpression : BoundExpression
{
    public BoundExpression Condition { get; }
    public BoundExpression Body { get; }

    public BoundWhileExpression(BoundExpression condition, BoundExpression body)
        : base(condition.Type.ContainingAssembly.GetTypeByMetadataName("System.Void"), null, BoundExpressionReason.None)
    {
        Condition = condition;
        Body = body;
    }
}
