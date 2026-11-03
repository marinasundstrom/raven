namespace Raven.CodeAnalysis;

internal partial class BoundGotoStatement : BoundStatement
{
    public BoundGotoStatement(LabelSymbol target, bool isBackward = false)
    {
        Target = target;
        IsBackward = isBackward;
    }

    public LabelSymbol Target { get; }

    public bool IsBackward { get; }

    public override Symbol Symbol => Target;
}
