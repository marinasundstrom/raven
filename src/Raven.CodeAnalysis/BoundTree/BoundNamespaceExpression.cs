namespace Raven.CodeAnalysis;

internal partial class BoundNamespaceExpression : BoundExpression
{
    public NamespaceSymbol Namespace { get; }

    public BoundNamespaceExpression(NamespaceSymbol @namespace)
        : base(null!, @namespace) // type is not relevant
    {
        Namespace = @namespace;
    }
}
