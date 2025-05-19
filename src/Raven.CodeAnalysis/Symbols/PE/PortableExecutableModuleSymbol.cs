using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PortableExecutableModuleSymbol : PortableExecutableSymbol, IModuleSymbol
{
    private readonly Module _module;

    public PortableExecutableModuleSymbol(Module module, Location[] locations)
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
