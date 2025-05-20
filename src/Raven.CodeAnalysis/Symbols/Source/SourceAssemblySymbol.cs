namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceAssemblySymbol : SourceSymbol, IAssemblySymbol
{
    public SourceAssemblySymbol(string name, Location[] locations)
        : base(SymbolKind.Assembly, name, null!, null, null, locations, [])
    {

    }

    public string FullName => Name;

    public INamespaceSymbol GlobalNamespace => throw new NotImplementedException();

    public IEnumerable<IModuleSymbol> Modules => throw new NotImplementedException();

    public INamedTypeSymbol? GetTypeByMetadataName(string fullyQualifiedMetadataName)
    {
        throw new NotImplementedException();
    }
}
