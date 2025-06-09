namespace Raven.CodeAnalysis;

internal partial class BoundNamespaceExpression : BoundExpression
{
    public INamespaceSymbol Namespace { get; }

    public BoundNamespaceExpression(INamespaceSymbol ns)
        : base(null!, ns) // type is not relevant
    {
        Namespace = ns;
    }
}
