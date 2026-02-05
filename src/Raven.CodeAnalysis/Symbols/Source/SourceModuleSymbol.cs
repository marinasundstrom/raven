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

    private ITypeSymbol? FindType(INamespaceSymbol rootNamespace, string fullyQualifiedName)
    {
        if (string.IsNullOrWhiteSpace(fullyQualifiedName))
            return null;

        // Metadata uses '+' for nested types. Normalize so we can treat '.' and '+' uniformly.
        var normalized = fullyQualifiedName.Replace('+', '.');
        var parts = normalized.Split('.', StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0)
            return null;

        // Helper: Parse a segment like Foo`1 into baseName=Foo, arity=1
        static bool TryParseMetadataArity(string segment, out string baseName, out int arity)
        {
            baseName = segment;
            arity = 0;

            var tickIndex = segment.LastIndexOf('`');
            if (tickIndex < 0 || tickIndex == segment.Length - 1)
                return false;

            baseName = segment.Substring(0, tickIndex);
            return int.TryParse(segment.Substring(tickIndex + 1), out arity) && arity >= 0;
        }

        // Helper: Try to resolve a type in a namespace, supporting metadata arity notation.
        static ITypeSymbol? ResolveTypeInNamespace(INamespaceSymbol ns, string segment)
        {
            // First try exact match (works for PE metadata names like List`1)
            var exact = ns.GetMembers(segment).OfType<ITypeSymbol>().FirstOrDefault();
            if (exact is not null)
                return exact;

            // If the segment uses metadata arity (Foo`1), try matching source symbols by base name + arity.
            if (TryParseMetadataArity(segment, out var baseName, out var arity))
            {
                var named = ns.GetMembers(baseName).OfType<INamedTypeSymbol>().FirstOrDefault(t => t.Arity == arity);
                if (named is not null)
                    return named;
            }

            return null;
        }

        // Helper: Try to resolve a nested type in a containing type, supporting metadata arity notation.
        static ITypeSymbol? ResolveNestedTypeInType(ITypeSymbol container, string segment)
        {
            // First try exact match (works for PE metadata names)
            var exact = container.GetMembers(segment).OfType<ITypeSymbol>().FirstOrDefault();
            if (exact is not null)
                return exact;

            // Source nested generics will likely be named without `arity in Name.
            if (TryParseMetadataArity(segment, out var baseName, out var arity))
            {
                var named = container.GetMembers(baseName).OfType<INamedTypeSymbol>().FirstOrDefault(t => t.Arity == arity);
                if (named is not null)
                    return named;
            }

            return null;
        }

        // We first try the common path: consume as many namespace segments as possible, then resolve a type and any nested types.
        // To be robust against namespace/type name collisions (rare but possible), we backtrack: if it fails,
        // we try treating one fewer segment as namespace, and so on.
        var maxNamespaceSegments = parts.Length - 1; // at least one segment must remain for a type

        for (var nsCount = maxNamespaceSegments; nsCount >= 0; nsCount--)
        {
            var currentNs = rootNamespace;
            var failed = false;

            // Walk exactly nsCount namespace segments
            for (var i = 0; i < nsCount; i++)
            {
                var nextNs = currentNs.GetMembers(parts[i]).OfType<INamespaceSymbol>().FirstOrDefault();
                if (nextNs is null)
                {
                    failed = true;
                    break;
                }

                currentNs = nextNs;
            }

            if (failed)
                continue;

            // The first type segment is immediately after the namespaces
            var typeIndex = nsCount;
            if (typeIndex >= parts.Length)
                continue;

            var currentType = ResolveTypeInNamespace(currentNs, parts[typeIndex]);
            if (currentType is null)
                continue;

            // Remaining segments are nested types
            for (var i = typeIndex + 1; i < parts.Length; i++)
            {
                var nestedName = parts[i];
                currentType = ResolveNestedTypeInType(currentType, nestedName);
                if (currentType is null)
                {
                    failed = true;
                    break;
                }
            }

            if (!failed)
                return currentType;
        }

        return null;
    }
}
