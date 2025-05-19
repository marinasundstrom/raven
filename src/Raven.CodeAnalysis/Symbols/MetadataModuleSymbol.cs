using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class MetadataModuleSymbol : MetadataSymbol, IModuleSymbol
{
    private readonly Module _module;

    public MetadataModuleSymbol(Module module, Location[] locations)
        : base(null!, null, null, locations)
    {
        _module = module;
    }

    public override SymbolKind Kind => SymbolKind.Module;

    public override string Name => _module.Name;

    public INamespaceSymbol GlobalNamespace { get; set; }

    public ImmutableArray<IAssemblySymbol> ReferencedAssemblySymbols => throw new NotImplementedException();

    public INamespaceSymbol? GetModuleNamespace(INamespaceSymbol namespaceSymbol)
    {
        throw new NotImplementedException();
    }
}
