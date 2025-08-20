using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

class NamespaceBinder : Binder
{
    private readonly INamespaceSymbol _namespaceSymbol;
    private readonly List<INamespaceSymbol> _imports = new(); // Stores `using` directives
    private readonly List<SourceNamedTypeSymbol> _declaredTypes = [];

    public NamespaceBinder(Binder parent, INamespaceSymbol ns)
        : base(parent)
    {
        _namespaceSymbol = ns;
    }

    /// <summary>
    /// Adds a namespace to the list of imports (`using System;`).
    /// </summary>
    public void AddUsingDirective(INamespaceSymbol importedNamespace)
    {
        if (!_imports.Contains(importedNamespace))
            _imports.Add(importedNamespace);
    }

    /// <summary>
    /// Looks up a type, checking imported namespaces before the current namespace.
    /// </summary>
    public override ITypeSymbol? LookupType(string name)
    {
        // 1. Check the current namespace
        var type = NamespaceSymbol.LookupType(name);
        if (type != null)
            return type;

        // 2. Check imported namespaces (from `using` statements)
        foreach (var ns in _imports)
        {
            type = ns.LookupType(name);
            if (type != null)
                return type;
        }

        // 3. Finally, check global namespace for metadata types
        return Compilation.GlobalNamespace.LookupType(name) ?? base.LookupType(name);
    }

    public void DeclareType(SourceNamedTypeSymbol type)
    {
        _declaredTypes.Add(type);
    }

    public IEnumerable<SourceNamedTypeSymbol> DeclaredTypes => _declaredTypes;

    public INamespaceSymbol NamespaceSymbol => _namespaceSymbol;
}