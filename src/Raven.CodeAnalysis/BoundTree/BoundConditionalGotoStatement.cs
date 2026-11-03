namespace Raven.CodeAnalysis;

internal partial class BoundConditionalGotoStatement : BoundStatement
{
    public BoundConditionalGotoStatement(LabelSymbol target, BoundExpression condition, bool jumpIfTrue)
    {
        Target = target;
        Condition = condition;
        JumpIfTrue = jumpIfTrue;
    }

    public LabelSymbol Target { get; }

    public BoundExpression Condition { get; }

    public bool JumpIfTrue { get; }

    public override Symbol Symbol => Target;
}
