using System.Collections;

namespace Raven.CodeAnalysis;

class ImportBinder : Binder
{
    private readonly IReadOnlyList<INamespaceOrTypeSymbol> _namespaceOrTypeScopeImports;
    private readonly IReadOnlyList<ITypeSymbol> _typeImports;
    private readonly IReadOnlyDictionary<string, ITypeSymbol> _typeAliases;

    public ImportBinder(Binder parent, IReadOnlyList<INamespaceOrTypeSymbol> namespaceOrTypeScopeImports, IReadOnlyList<ITypeSymbol> typeImports, IReadOnlyDictionary<string, ITypeSymbol> typeAliases)
        : base(parent)
    {
        _namespaceOrTypeScopeImports = namespaceOrTypeScopeImports;
        _typeImports = typeImports;
        _typeAliases = typeAliases;
    }

    public override ITypeSymbol? LookupType(string name)
    {
        var type = _typeImports.FirstOrDefault(x => x.Name == name);
        if (type is not null)
            return type;

        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            var t = ns.LookupType(name);
            if (t != null)
                return t;
        }

        return ParentBinder?.LookupType(name);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        if (_typeAliases.TryGetValue(name, out var type))
            return type;

        var matchingType = _typeImports.FirstOrDefault(x => x.Name == name);
        if (matchingType is not null)
            return matchingType;

        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            var t = ns.LookupType(name);
            if (t != null)
                return t;
        }

        return ParentBinder?.LookupSymbol(name);
    }

    /// <summary>
    /// Gets namespaces and types whose members are in scope.
    /// </summary>
    public IEnumerable<INamespaceOrTypeSymbol> GetImportedNamespacesOrTypeScopes() => _namespaceOrTypeScopeImports;

    /// <summary>
    /// Get types that have been explicitly imported.
    /// </summary>
    public IEnumerable<ITypeSymbol> GetImportedTypes() => _typeImports;

    /// <summary>
    /// Gets a dictionary with the mapping from alias to resolved type.
    /// </summary>
    public IReadOnlyDictionary<string, ITypeSymbol> GetAliases() => _typeAliases;
}
