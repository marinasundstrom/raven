namespace Raven.CodeAnalysis;

internal class BoundIfExpression : BoundExpression
{
    public BoundExpression Condition { get; }
    public BoundExpression ThenBranch { get; }
    public BoundExpression? ElseBranch { get; }

    public BoundIfExpression(BoundExpression condition, BoundExpression thenBranch, BoundExpression? elseBranch = null)
        : base(condition.Type.Compilation.GetSpecialType(SpecialType.System_Void), null, BoundExpressionReason.None)
    {
        Condition = condition;
        ThenBranch = thenBranch;
        ElseBranch = elseBranch;
    }
}