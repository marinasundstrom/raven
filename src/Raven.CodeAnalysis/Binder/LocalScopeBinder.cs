using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class LocalScopeBinder : Binder
{
    private readonly Dictionary<string, SourceLocalSymbol> _locals = new();

    public LocalScopeBinder(Binder parent) : base(parent) { }

    /// <summary>
    /// Defines a new local variable in the current scope.
    /// </summary>
    public void DeclareLocal(string name, ITypeSymbol type)
    {
        if (_locals.ContainsKey(name))
        {
            throw new Exception($"Variable '{name}' is already defined in this scope.");
        }

        _locals[name] = null; //new SourceLocalSymbol(name, type);
    }

    /// <summary>
    /// Attempts to resolve a local variable in this scope or parent binders.
    /// </summary>
    public override SymbolInfo BindSymbol(SyntaxNode node)
    {
        if (node is IdentifierNameSyntax identifier)
        {
            // First, check local variables
            if (_locals.TryGetValue(identifier.Identifier.Text, out var symbol))
                return new SymbolInfo(symbol);

            // If not found, check method parameters
            return ParentBinder?.BindSymbol(node) ?? default;
        }

        return base.BindSymbol(node);
    }

    internal override SymbolInfo BindIdentifierReference(IdentifierNameSyntax node)
    {
        return ParentBinder?.BindIdentifierReference(node) ?? default;
    }

    internal override SymbolInfo BindMemberAccessReference(MemberAccessExpressionSyntax node)
    {
        return ParentBinder?.BindMemberAccessReference(node) ?? default;
    }

    internal override SymbolInfo BindInvocationReference(InvocationExpressionSyntax node)
    {
        return ParentBinder?.BindInvocationReference(node) ?? default;
    }

}