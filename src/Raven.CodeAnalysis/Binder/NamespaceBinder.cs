using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

class NamespaceBinder : Binder
{
    private readonly INamespaceSymbol _namespaceSymbol;
    private readonly List<INamespaceSymbol> _imports = new(); // Stores `using` directives
    private readonly Dictionary<string, ITypeSymbol> _typeImports = new();
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

    public void AddTypeImport(string alias, ITypeSymbol type)
    {
        if (!_typeImports.ContainsKey(alias))
            _typeImports[alias] = type;
    }

    /// <summary>
    /// Looks up a type, checking imported namespaces before the current namespace.
    /// </summary>
    public override ITypeSymbol? LookupType(string name)
    {
        if (_typeImports.TryGetValue(name, out var importedType))
            return importedType;

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

    public override ISymbol? LookupSymbol(string name)
    {
        if (_typeImports.TryGetValue(name, out var type))
            return type;

        return base.LookupSymbol(name);
    }

    public void DeclareType(SourceNamedTypeSymbol type)
    {
        _declaredTypes.Add(type);
    }

    public IEnumerable<SourceNamedTypeSymbol> DeclaredTypes => _declaredTypes;

    public INamespaceSymbol NamespaceSymbol => _namespaceSymbol;
}