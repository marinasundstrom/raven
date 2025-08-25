using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis;

class ImportBinder : Binder
{
    private readonly IReadOnlyList<INamespaceOrTypeSymbol> _namespaceOrTypeScopeImports;
    private readonly IReadOnlyList<ITypeSymbol> _typeImports;
    private readonly IReadOnlyDictionary<string, IReadOnlyList<ISymbol>> _aliases;

    public ImportBinder(
        Binder parent,
        IReadOnlyList<INamespaceOrTypeSymbol> namespaceOrTypeScopeImports,
        IReadOnlyList<ITypeSymbol> typeImports,
        IReadOnlyDictionary<string, IReadOnlyList<ISymbol>> aliases)
        : base(parent)
    {
        _namespaceOrTypeScopeImports = namespaceOrTypeScopeImports;
        _typeImports = typeImports;
        _aliases = aliases;
    }

    public override ITypeSymbol? LookupType(string name)
    {
        if (_aliases.TryGetValue(name, out var aliasSymbols))
            return aliasSymbols.OfType<ITypeSymbol>().FirstOrDefault();

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
        if (_aliases.TryGetValue(name, out var symbols))
            return symbols.FirstOrDefault();

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
    /// Gets a dictionary with the mapping from alias to resolved symbols.
    /// </summary>
    public IReadOnlyDictionary<string, IReadOnlyList<ISymbol>> GetAliases() => _aliases;

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        if (_aliases.TryGetValue(name, out var symbols))
            return symbols;

        var type = _typeImports.FirstOrDefault(x => x.Name == name);
        if (type != null)
            return [type];

        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            var t = ns.LookupType(name);
            if (t != null)
                return [t];
        }

        return ParentBinder?.LookupSymbols(name) ?? Enumerable.Empty<ISymbol>();
    }
}
