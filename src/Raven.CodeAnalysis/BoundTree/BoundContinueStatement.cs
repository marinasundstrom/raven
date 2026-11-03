namespace Raven.CodeAnalysis;

internal partial class BoundContinueStatement : BoundStatement
{
    public BoundContinueStatement(LabelSymbol? targetLabel = null)
    {
        TargetLabel = targetLabel;
    }

    public LabelSymbol? TargetLabel { get; }
}
