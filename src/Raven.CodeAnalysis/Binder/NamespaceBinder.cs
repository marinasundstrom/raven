using System.Collections.Generic;
using Raven.CodeAnalysis.Symbols;

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

    public void DeclareType(SourceNamedTypeSymbol type)
    {
        _declaredTypes.Add(type);
    }

    public IEnumerable<SourceNamedTypeSymbol> DeclaredTypes => _declaredTypes;

    public INamespaceSymbol NamespaceSymbol => _namespaceSymbol;
}