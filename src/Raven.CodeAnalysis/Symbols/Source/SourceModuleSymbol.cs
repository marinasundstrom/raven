using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceModuleSymbol : SourceSymbol, IModuleSymbol
{
    private SourceAssemblySymbol _containingAssembly;
    private ImmutableArray<IAssemblySymbol> _referencedAssemblySymbols;
    private SourceNamespaceSymbol _globalNamespace;

    public SourceModuleSymbol(string name, SourceAssemblySymbol containingAssembly, IEnumerable<IAssemblySymbol> referencedAssemblySymbols, Location[] locations)
        : base(SymbolKind.Module, name, null!, null, null, locations, [])
    {
        _containingAssembly = containingAssembly;
        _referencedAssemblySymbols = referencedAssemblySymbols.ToImmutableArray();
        _containingAssembly.AddModule(this);
    }

    public override IAssemblySymbol ContainingAssembly => _containingAssembly;

    public override SymbolKind Kind => SymbolKind.Module;

    public INamespaceSymbol GlobalNamespace =>
        _globalNamespace ??= new SourceNamespaceSymbol(this,
            "", this, null, null,
            [], []);


    public ImmutableArray<IAssemblySymbol> ReferencedAssemblySymbols => _referencedAssemblySymbols;

    public INamespaceSymbol? GetModuleNamespace(INamespaceSymbol namespaceSymbol)
    {
        throw new NotImplementedException();
    }

    public ISymbol? ResolveMetadataMember(INamespaceSymbol namespaceSymbol, string name)
    {
        var nsName = namespaceSymbol.ToMetadataName();
        var fullName = string.IsNullOrEmpty(nsName) ? name : nsName + "." + name;

        // TODO: Resolve

        var types = ReferencedAssemblySymbols.OfType<PEAssemblySymbol>()
            .Select(x => x.GetTypeByMetadataName(name))
            .Where(x => x is not null);

        if (types.Any())
        {
            return types.FirstOrDefault();
        }

        return null;
    }
}
