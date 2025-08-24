using System.Collections;

namespace Raven.CodeAnalysis;

class ImportBinder : Binder
{
    private readonly IReadOnlyList<INamespaceSymbol> _namespaceImports;
    private readonly IReadOnlyDictionary<string, ITypeSymbol> _typeImports;

    public ImportBinder(Binder parent, IReadOnlyList<INamespaceSymbol> namespaceImports, IReadOnlyDictionary<string, ITypeSymbol> typeImports)
        : base(parent)
    {
        _namespaceImports = namespaceImports;
        _typeImports = typeImports;
    }

    public override ITypeSymbol? LookupType(string name)
    {
        if (_typeImports.TryGetValue(name, out var type))
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
        if (_typeImports.TryGetValue(name, out var type))
            return type;

        foreach (var ns in _namespaceImports)
        {
            var t = ns.LookupType(name);
            if (t != null)
                return t;
        }

        return ParentBinder?.LookupSymbol(name);
    }

    public IEnumerable<INamespaceSymbol> GetImportedNamespaces() => _namespaceImports;
}