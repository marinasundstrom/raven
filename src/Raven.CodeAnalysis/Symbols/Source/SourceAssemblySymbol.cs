namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceAssemblySymbol : SourceSymbol, IAssemblySymbol
{
    private readonly List<SourceModuleSymbol> _modules = new List<SourceModuleSymbol>();
    private INamespaceSymbol _globalNamespace;

    public SourceAssemblySymbol(string name, Location[] locations)
        : base(SymbolKind.Assembly, name, null!, null, null, locations, [])
    {

    }

    public string FullName => Name;

    public INamespaceSymbol GlobalNamespace => _globalNamespace ??= (
        _modules.Count == 1
            ? _modules[0].GlobalNamespace
            : new MergedNamespaceSymbol(_modules.Select(x => x.GlobalNamespace), null));

    public IEnumerable<IModuleSymbol> Modules => _modules;

    public INamedTypeSymbol? GetTypeByMetadataName(string fullyQualifiedMetadataName)
    {
        return _modules
            .OfType<SourceModuleSymbol>()
            .Select(m => m.ResolveMetadataMember(GlobalNamespace, fullyQualifiedMetadataName))
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault();
    }

    internal void AddModule(SourceModuleSymbol sourceModuleSymbol)
    {
        _modules.Add(sourceModuleSymbol);
    }
}