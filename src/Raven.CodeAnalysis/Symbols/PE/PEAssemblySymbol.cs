using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEAssemblySymbol : PESymbol, IAssemblySymbol
{
    private readonly Assembly _assembly;
    private PEModuleSymbol[] _modules = [];
    private INamespaceSymbol? _globalNamespace;

    public PEAssemblySymbol(Assembly assembly, Location[] locations)
        : base(null!, null, null, locations)
    {
        _assembly = assembly;
    }

    public void AddModules(params PEModuleSymbol[] modules)
    {
        // Filter out invalid or null modules
        _modules = modules
            .Where(m => m != null && m.GlobalNamespace != null)
            .ToArray();
    }

    public override SymbolKind Kind => SymbolKind.Assembly;

    public override string Name => _assembly.GetName().Name!;

    public string FullName => _assembly.GetName().FullName;

    public INamespaceSymbol GlobalNamespace => _globalNamespace ??= (
        _modules.Length == 1
            ? _modules[0].GlobalNamespace
            : new MergedNamespaceSymbol(_modules.Select(x => x.GlobalNamespace)));

    public IEnumerable<IModuleSymbol> Modules => _modules;

    internal PEModuleSymbol PrimaryModule =>
        _modules.Length > 0 ? _modules[0] : throw new InvalidOperationException("No modules assigned.");

    public INamedTypeSymbol? GetTypeByMetadataName(string fullyQualifiedPEName)
    {
        return _modules
            .OfType<PEModuleSymbol>()
            .Select(m => m.ResolveMetadataMember(GlobalNamespace, fullyQualifiedPEName))
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault();
    }

    public ITypeSymbol? GetType(Type type)
    {
        return _modules
            .OfType<PEModuleSymbol>()
            .Select(m => m.GetType(type))
            .SingleOrDefault();
    }

    private INamespaceSymbol? TryGetNamespaceSymbol(string? ns)
    {
        if (ns is null)
            return GlobalNamespace;

        var namespaceParts = ns.Split('.');
        var currentNamespace = GlobalNamespace;

        foreach (var part in namespaceParts)
        {
            var next = currentNamespace.GetMembers()
                .FirstOrDefault(n => n.Name == part) as INamespaceSymbol;

            if (next is null)
                return null;

            currentNamespace = next;
        }

        return currentNamespace;
    }

    public Assembly GetAssemblyInfo() => _assembly;
}