using System.Collections.Generic;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceAssemblySymbol : SourceSymbol, AssemblySymbol
{
    private readonly List<SourceModuleSymbol> _modules = new List<SourceModuleSymbol>();
    private NamespaceSymbol? _globalNamespace;

    public SourceAssemblySymbol(
        Compilation compilation,
        string name,
        SyntaxReference[] declaringSyntaxReferences,
        Location[]? locations = null)
        : base(SymbolKind.Assembly, name, null!, null, null, locations ?? [], declaringSyntaxReferences)
    {
        Compilation = compilation;
    }

    internal Compilation Compilation { get; }

    public string FullName => Name;

    public NamespaceSymbol GlobalNamespace => LazyInitializer.EnsureInitialized(
        ref _globalNamespace,
        () => _modules.Count == 1
            ? _modules[0].GlobalNamespace
            : new MergedNamespaceSymbol(_modules.Select(x => x.GlobalNamespace), null));

    public IEnumerable<ModuleSymbol> Modules => _modules;

    public NamedTypeSymbol? GetTypeByMetadataName(string fullyQualifiedMetadataName)
    {
        return _modules
            .OfType<SourceModuleSymbol>()
            .Select(m => m.FindDeclaredType(fullyQualifiedMetadataName))
            .OfType<NamedTypeSymbol>()
            .FirstOrDefault();
    }

    internal void AddModule(SourceModuleSymbol sourceModuleSymbol)
    {
        _modules.Add(sourceModuleSymbol);
    }
}
