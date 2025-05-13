namespace Raven.CodeAnalysis;

internal class BoundLocalExpression : BoundExpression
{
    private ILocalSymbol _local;

    public BoundLocalExpression(ILocalSymbol local)
    {
        _local = local;
    }

    public override ITypeSymbol Type => _local.Type;
}