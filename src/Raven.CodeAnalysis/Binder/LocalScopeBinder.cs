using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class LocalScopeBinder : Binder
{
    public LocalScopeBinder(Binder parent) : base(parent) { }

    internal override SymbolInfo BindIdentifierReference(IdentifierNameSyntax node)
    {
        return ParentBinder?.BindIdentifierReference(node) ?? default;
    }

    internal override SymbolInfo BindMemberAccessReference(MemberAccessExpressionSyntax node)
    {
        return ParentBinder?.BindMemberAccessReference(node) ?? default;
    }

    internal override SymbolInfo BindMemberBindingReference(MemberBindingExpressionSyntax node)
    {
        return ParentBinder?.BindMemberBindingReference(node) ?? default;
    }

    internal override SymbolInfo BindInvocationReference(InvocationExpressionSyntax node)
    {
        return ParentBinder?.BindInvocationReference(node) ?? default;
    }

    public override BoundNode GetOrBindForSemanticQuery(SyntaxNode node)
        => ParentBinder?.GetOrBindForSemanticQuery(node) ?? base.GetOrBindForSemanticQuery(node);

}
