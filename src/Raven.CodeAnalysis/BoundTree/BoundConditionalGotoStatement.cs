namespace Raven.CodeAnalysis;

internal partial class BoundConditionalGotoStatement : BoundStatement
{
    public BoundConditionalGotoStatement(ILabelSymbol target, BoundExpression condition, bool jumpIfTrue)
    {
        Target = target;
        Condition = condition;
        JumpIfTrue = jumpIfTrue;
    }

    public ILabelSymbol Target { get; }

    public BoundExpression Condition { get; }

    public bool JumpIfTrue { get; }

    public override ISymbol Symbol => Target;
}
