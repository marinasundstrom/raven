using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceModuleSymbol : SourceSymbol, IModuleSymbol
{
    private readonly SourceAssemblySymbol _containingAssembly;
    private readonly ImmutableArray<IAssemblySymbol> _referencedAssemblySymbols;
    private SourceNamespaceSymbol _globalNamespace;

    public SourceModuleSymbol(string name, SourceAssemblySymbol containingAssembly, IEnumerable<IAssemblySymbol> referencedAssemblySymbols, Location[] locations)
        : base(SymbolKind.Module, name, containingAssembly, null, null, locations, [])
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

        var p = FindType(GlobalNamespace, fullName);
        if (p is not null)
        {
            return p;
        }

        var types = ReferencedAssemblySymbols.OfType<PEAssemblySymbol>()
            .Select(x => x.GetTypeByMetadataName(name))
            .Where(x => x is not null);

        if (types.Any())
        {
            return types.FirstOrDefault();
        }

        return null;
    }

    private ITypeSymbol? FindType(INamespaceSymbol namespaceSymbol, string fullyQualifiedName)
    {
        var parts = fullyQualifiedName!.Split('.'); // "System.Collections.Generic.List`1" → [System, Collections, Generic, List`1]
        int index = 0;
        INamespaceSymbol currentNamespace = namespaceSymbol;

        // Walk namespaces
        while (index < parts.Length - 1)
        {
            var nsPart = parts[index++];
            currentNamespace = currentNamespace.GetMembers(nsPart)
                                               .OfType<INamespaceSymbol>()
                                               .FirstOrDefault();

            if (currentNamespace is null)
                return null;
        }

        // Final part is the type name
        var typeName = parts[^1];

        return currentNamespace.GetMembers(typeName)
                               .OfType<ITypeSymbol>()
                               .FirstOrDefault();
    }
}