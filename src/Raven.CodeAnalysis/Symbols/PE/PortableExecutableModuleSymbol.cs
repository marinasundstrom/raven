using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PortableExecutableModuleSymbol : PortableExecutableSymbol, IModuleSymbol
{
    readonly Dictionary<Type, ITypeSymbol> _typeSymbolTypeInfoMapping = new Dictionary<Type, ITypeSymbol>();

    private readonly PortableExecutableAssemblySymbol _assembly;
    private readonly Module _module;
    private INamespaceSymbol? _globalNamespace;

    public PortableExecutableModuleSymbol(
        PortableExecutableAssemblySymbol assembly,
        Module module,
        Location[] locations,
        IEnumerable<IAssemblySymbol> referencedAssemblySymbols)
        : base(null!, null, null, locations)
    {
        _assembly = assembly;
        _module = module;
        ReferencedAssemblySymbols = referencedAssemblySymbols.ToImmutableArray();
    }

    public override SymbolKind Kind => SymbolKind.Module;

    public override string Name => _module.Name;

    public override IAssemblySymbol ContainingAssembly => _assembly;

    public INamespaceSymbol GlobalNamespace =>
        _globalNamespace ??= new PortableExecutableNamespaceSymbol(this, string.Empty, this, null);

    public ImmutableArray<IAssemblySymbol> ReferencedAssemblySymbols { get; }

    public INamespaceSymbol? GetModuleNamespace(INamespaceSymbol namespaceSymbol)
    {
        throw new NotImplementedException();
    }

    public ITypeSymbol? GetType(Type type)
    {
        if (_typeSymbolTypeInfoMapping.TryGetValue(type, out var typeSymbol))
            return typeSymbol;

        return ReferencedAssemblySymbols
                .OfType<PortableExecutableAssemblySymbol>()
                .Select(x => x.GetType(type))
                .SingleOrDefault();
    }

    public ISymbol? ResolveMetadataMember(INamespaceSymbol namespaceSymbol, string name)
    {
        var nsName = namespaceSymbol.ToMetadataName();
        var fullName = string.IsNullOrEmpty(nsName) ? name : nsName + "." + name;

        if (namespaceSymbol.IsMemberDefined(name, out var existingSymbol0))
            return existingSymbol0;

        var assembly = PEContainingAssembly.GetAssemblyInfo();

        var type = assembly.GetType(fullName, throwOnError: false, ignoreCase: false);
        if (type is not null)
        {
            var symbol = CreateMetadataTypeSymbol(type);
            return symbol;
        }

        // If no type found, try to verify as a namespace
        // Does any type start with "System.Text."?
        bool namespaceLikelyExists = assembly.GetTypes()
            .Any(t => t.FullName.StartsWith(fullName + ".", StringComparison.Ordinal));

        if (namespaceLikelyExists && namespaceSymbol is INamespaceSymbol parentNs)
        {
            // Check if the namespace already exists in the parent
            if (parentNs.IsMemberDefined(name, out var existingSymbol))
                return existingSymbol;

            return new PortableExecutableNamespaceSymbol(name, parentNs, parentNs);
        }

        return null;
    }

    private PortableExecutableNamedTypeSymbol CreateMetadataTypeSymbol(Type type)
    {
        var ns = GetOrCreateNamespaceSymbol(type.Namespace);

        var typeInfo = type.GetTypeInfo();

        var typeSymbol = new PortableExecutableNamedTypeSymbol(
            typeInfo, ns, null, ns,
            [new MetadataLocation()]);

        _typeSymbolTypeInfoMapping[typeInfo] = typeSymbol;

        return typeSymbol;
    }

    private INamespaceSymbol GetOrCreateNamespaceSymbol(string? ns)
    {
        if (string.IsNullOrEmpty(ns))
            return GlobalNamespace;

        var parts = ns.Split('.');
        var current = GlobalNamespace;

        foreach (var part in parts)
        {
            var next = current.GetMembers(part)
                .OfType<INamespaceSymbol>()
                .FirstOrDefault();

            if (next is null)
            {
                next = new PortableExecutableNamespaceSymbol(part, _assembly, current);
            }

            current = next;
        }

        return current;
    }
}