namespace Raven.CodeAnalysis;

internal partial class BoundLabeledStatement : BoundStatement
{
    public BoundLabeledStatement(ILabelSymbol label, BoundStatement statement)
    {
        Label = label;
        Statement = statement;
    }

    public ILabelSymbol Label { get; }

    public BoundStatement Statement { get; }

    public override ISymbol Symbol => Label;
}
