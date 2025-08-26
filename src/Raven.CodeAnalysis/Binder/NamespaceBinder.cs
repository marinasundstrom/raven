using System.Collections.Generic;
using System.Linq;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

class NamespaceBinder : Binder
{
    private readonly INamespaceSymbol _namespaceSymbol;
    private readonly List<INamespaceSymbol> _imports = new(); // Stores `using` directives
    private readonly Dictionary<string, IReadOnlyList<IAliasSymbol>> _aliases = new();
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

    public void AddAlias(string alias, IEnumerable<IAliasSymbol> symbols)
    {
        if (!_aliases.ContainsKey(alias))
            _aliases[alias] = symbols.ToArray();
    }

    /// <summary>
    /// Looks up a type, checking imported namespaces before the current namespace.
    /// </summary>
    public override ITypeSymbol? LookupType(string name)
    {
        if (_aliases.TryGetValue(name, out var importedSymbols))
            return importedSymbols.OfType<ITypeSymbol>().FirstOrDefault();

        var type = NamespaceSymbol.LookupType(name);
        if (type != null)
            return type;

        foreach (var ns in _imports)
        {
            type = ns.LookupType(name);
            if (type != null)
                return type;
        }

        return Compilation.GlobalNamespace.LookupType(name) ?? base.LookupType(name);
    }

    public override INamespaceSymbol? LookupNamespace(string name)
    {
        if (_aliases.TryGetValue(name, out var importedSymbols))
            return importedSymbols.OfType<INamespaceSymbol>().FirstOrDefault();

        var ns = NamespaceSymbol.LookupNamespace(name);
        if (ns != null)
            return ns;

        foreach (var import in _imports)
        {
            ns = import.LookupNamespace(name);
            if (ns != null)
                return ns;
        }

        return base.LookupNamespace(name);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        if (_aliases.TryGetValue(name, out var symbols))
            return symbols.FirstOrDefault();

        return base.LookupSymbol(name);
    }

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        if (_aliases.TryGetValue(name, out var symbols))
            return symbols;

        return base.LookupSymbols(name);
    }

    public void DeclareType(SourceNamedTypeSymbol type)
    {
        _declaredTypes.Add(type);
    }

    public IEnumerable<SourceNamedTypeSymbol> DeclaredTypes => _declaredTypes;

    public INamespaceSymbol NamespaceSymbol => _namespaceSymbol;
}