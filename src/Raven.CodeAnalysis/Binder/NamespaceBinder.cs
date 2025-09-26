using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class NamespaceBinder : Binder
{
    private readonly INamespaceSymbol _namespaceSymbol;
    private readonly List<SourceNamedTypeSymbol> _declaredTypes = [];

    public NamespaceBinder(Binder parent, INamespaceSymbol ns)
        : base(parent)
    {
        _namespaceSymbol = ns;
    }

    public override INamespaceSymbol? CurrentNamespace => _namespaceSymbol;

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        if (node is BaseNamespaceDeclarationSyntax)
            return _namespaceSymbol;

        return base.BindDeclaredSymbol(node);
    }

    public void DeclareType(SourceNamedTypeSymbol type)
    {
        _declaredTypes.Add(type);
    }

    public IEnumerable<SourceNamedTypeSymbol> DeclaredTypes => _declaredTypes;

    public INamespaceSymbol NamespaceSymbol => _namespaceSymbol;

    public override IEnumerable<IMethodSymbol> LookupExtensionMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);

        foreach (var method in GetExtensionMethodsFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
            if (seen.Add(method))
                yield return method;

        foreach (var method in base.LookupExtensionMethods(name, receiverType, includePartialMatches))
            if (seen.Add(method))
                yield return method;
    }
}
