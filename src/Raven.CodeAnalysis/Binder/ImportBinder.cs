using System.Collections;

namespace Raven.CodeAnalysis;

class ImportBinder : Binder
{
    private readonly IReadOnlyList<INamespaceSymbol> _namespaceImports;
    private readonly IReadOnlyList<ITypeSymbol> _typeImports;
    private readonly IReadOnlyDictionary<string, ITypeSymbol> _typeAliases;

    public ImportBinder(Binder parent, IReadOnlyList<INamespaceSymbol> namespaceImports, IReadOnlyList<ITypeSymbol> typeImports, IReadOnlyDictionary<string, ITypeSymbol> typeAliases)
        : base(parent)
    {
        _namespaceImports = namespaceImports;
        _typeImports = typeImports;
        _typeAliases = typeAliases;
    }

    public override ITypeSymbol? LookupType(string name)
    {
        var type = _typeImports.FirstOrDefault(x => x.Name == name);
        if (type is not null)
            return type;

        foreach (var ns in _namespaceImports)
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

        foreach (var ns in _namespaceImports)
        {
            var t = ns.LookupType(name);
            if (t != null)
                return t;
        }

        return ParentBinder?.LookupSymbol(name);
    }

    public IEnumerable<INamespaceSymbol> GetImportedNamespaces() => _namespaceImports;

    public IEnumerable<ITypeSymbol> GetImportedTypes() => _typeImports;

    public IReadOnlyDictionary<string, ITypeSymbol> GetAliases() => _typeAliases;
}
