namespace Raven.CodeAnalysis;

internal partial class BoundGotoStatement : BoundStatement
{
    public BoundGotoStatement(ILabelSymbol target, bool isBackward = false)
    {
        Target = target;
        IsBackward = isBackward;
    }

    public ILabelSymbol Target { get; }

    public bool IsBackward { get; }

    public override ISymbol Symbol => Target;
}
