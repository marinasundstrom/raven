using System.Collections.Generic;
using System.Linq;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class MethodBinder : TypeMemberBinder
{
    private readonly IMethodSymbol _methodSymbol;

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent)
        : base(parent, (INamedTypeSymbol)methodSymbol.ContainingType!)
    {
        _methodSymbol = methodSymbol;
    }

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent, IEnumerable<IParameterSymbol> parameters)
        : this(methodSymbol, parent)
    {
        // Parameters are retrieved from _methodSymbol; no additional storage needed
    }

    public override ISymbol? LookupSymbol(string name)
    {
        var paramSymbol = _methodSymbol.Parameters.FirstOrDefault(p => p.Name == name);
        if (paramSymbol is not null)
            return paramSymbol;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        if (node is MethodDeclarationSyntax
            or ConstructorDeclarationSyntax
            or NamedConstructorDeclarationSyntax
            or AccessorDeclarationSyntax)
            return _methodSymbol;

        return base.BindDeclaredSymbol(node);
    }

    public IMethodSymbol GetMethodSymbol() => _methodSymbol;
}
