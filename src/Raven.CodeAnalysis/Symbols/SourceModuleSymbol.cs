using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceModuleSymbol : SourceSymbol, IModuleSymbol
{

    public SourceModuleSymbol(string name, Location[] locations)
        : base(SymbolKind.Module, name, null!, null, null, locations, [])
    {

    }

    public override SymbolKind Kind => SymbolKind.Module;

    public INamespaceSymbol GlobalNamespace { get; set; }

    public ImmutableArray<IAssemblySymbol> ReferencedAssemblySymbols => throw new NotImplementedException();

    public INamespaceSymbol? GetModuleNamespace(INamespaceSymbol namespaceSymbol)
    {
        throw new NotImplementedException();
    }
}
