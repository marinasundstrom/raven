namespace Raven.CodeAnalysis;

internal partial class BoundContinueStatement : BoundStatement
{
    public BoundContinueStatement(ILabelSymbol? targetLabel = null)
    {
        TargetLabel = targetLabel;
    }

    public ILabelSymbol? TargetLabel { get; }
}
