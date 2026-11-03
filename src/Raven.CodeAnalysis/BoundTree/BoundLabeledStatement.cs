namespace Raven.CodeAnalysis;

internal partial class BoundLabeledStatement : BoundStatement
{
    public BoundLabeledStatement(LabelSymbol label, BoundStatement statement)
    {
        Label = label;
        Statement = statement;
    }

    public LabelSymbol Label { get; }

    public BoundStatement Statement { get; }

    public override Symbol Symbol => Label;
}
