namespace Raven.CodeAnalysis;

class ImportBinder : Binder
{
    private readonly IReadOnlyList<INamespaceSymbol> _imports;

    public ImportBinder(Binder parent, IReadOnlyList<INamespaceSymbol> imports)
        : base(parent)
    {
        _imports = imports;
    }

    public override ITypeSymbol LookupType(string name)
    {
        foreach (var ns in _imports)
        {
            var type = ns.LookupType(name);
            if (type != null)
                return type;
        }

        return base.LookupType(name);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        foreach (var ns in _imports)
        {
            var type = ns.LookupType(name);
            if (type != null)
                return type;
        }

        return base.LookupSymbol(name);
    }
}