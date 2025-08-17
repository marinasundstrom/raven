namespace Raven.CodeAnalysis;

internal partial class BoundNamespaceExpression : BoundExpression
{
    public INamespaceSymbol Namespace { get; }

    public BoundNamespaceExpression(INamespaceSymbol @namespace)
        : base(null!, @namespace) // type is not relevant
    {
        Namespace = @namespace;
    }
}