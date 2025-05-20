using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceModuleSymbol : SourceSymbol, IModuleSymbol
{
    private SourceAssemblySymbol _containingAssembly;
    private SourceNamespaceSymbol _globalNamespace;

    public SourceModuleSymbol(string name, SourceAssemblySymbol containingAssembly, IEnumerable<IAssemblySymbol> referencedAssemblySymbols, Location[] locations)
        : base(SymbolKind.Module, name, null!, null, null, locations, [])
    {
        _containingAssembly = containingAssembly;

        _containingAssembly.AddModule(this);
    }

    public override IAssemblySymbol ContainingAssembly => _containingAssembly;

    public override SymbolKind Kind => SymbolKind.Module;

    public INamespaceSymbol GlobalNamespace =>
        _globalNamespace ??= new SourceNamespaceSymbol(this,
            "", this, null, null,
            [], []);


    public ImmutableArray<IAssemblySymbol> ReferencedAssemblySymbols => [];

    public INamespaceSymbol? GetModuleNamespace(INamespaceSymbol namespaceSymbol)
    {
        throw new NotImplementedException();
    }
}
