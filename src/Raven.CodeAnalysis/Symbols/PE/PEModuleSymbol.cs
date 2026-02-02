using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEModuleSymbol : PESymbol, IModuleSymbol
{
    readonly Dictionary<Type, ITypeSymbol> _typeSymbolTypeInfoMapping = new Dictionary<Type, ITypeSymbol>();
    readonly Dictionary<string, INamedTypeSymbol> _resolvedMetadataTypes = new();
    private readonly ReflectionTypeLoader _reflectionTypeLoader;
    private readonly PEAssemblySymbol _assembly;
    private readonly Module _module;
    private INamespaceSymbol? _globalNamespace;

    public PEModuleSymbol(
        ReflectionTypeLoader reflectionTypeLoader,
        PEAssemblySymbol assembly,
        Module module,
        Location[] locations,
        IEnumerable<IAssemblySymbol> referencedAssemblySymbols)
        : base(null!, null, null, locations)
    {
        _reflectionTypeLoader = reflectionTypeLoader;
        _assembly = assembly;
        _module = module;
        ReferencedAssemblySymbols = referencedAssemblySymbols.ToImmutableArray();
    }

    public override SymbolKind Kind => SymbolKind.Module;

    public override string Name => _module.Name;

    public override IAssemblySymbol ContainingAssembly => _assembly;

    public INamespaceSymbol GlobalNamespace =>
        _globalNamespace ??= new PENamespaceSymbol(_reflectionTypeLoader, this, string.Empty, this, null);

    public ImmutableArray<IAssemblySymbol> ReferencedAssemblySymbols { get; }

    public INamespaceSymbol? GetModuleNamespace(INamespaceSymbol namespaceSymbol)
    {
        throw new NotImplementedException();
    }

    public ITypeSymbol? GetType(Type type)
    {
        if (_typeSymbolTypeInfoMapping.TryGetValue(type, out var typeSymbol))
            return typeSymbol;

        // If the runtime type belongs to this module's assembly, we can intern it directly.
        // Avoid walking namespaces/members first, as that can create alternate symbol instances.
        var thisAssembly = PEContainingAssembly.GetAssemblyInfo();
        if (ReferenceEquals(type.Assembly, thisAssembly))
            return GetOrCreateTypeSymbol(type);

        // Otherwise, ask referenced assemblies.
        return ReferencedAssemblySymbols
            .OfType<PEAssemblySymbol>()
            .Select(x => x.GetType(type))
            .FirstOrDefault();
    }

    private ITypeSymbol? FindType(INamespaceSymbol namespaceSymbol, Type type)
    {
        if (type.IsNested)
        {
            // Nested types are not namespace members; they are members of their declaring type.
            return GetOrCreateTypeSymbol(type);
        }

        var parts = type.FullName!.Split('.'); // "System.Collections.Generic.List`1" → [System, Collections, Generic, List`1]
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

        var t = currentNamespace.GetMembers(typeName)
                               .OfType<PENamedTypeSymbol>()
                               .Where(x => x.GetTypeInfo() == type)
                               .FirstOrDefault();

        if (t is not null) return t;

        return ResolveMetadataMember(GlobalNamespace, type.FullName) as ITypeSymbol;
    }

    // Metadata type names never contain generic argument lists ("<...>") or commas.
    // Commas are interpreted by the runtime as assembly-qualified name separators.
    // Examples of problematic display names: "System.Result<(), CustomError>.Ok".
    private static bool LooksLikeNonMetadataTypeName(string name)
    {
        return name.IndexOf('<') >= 0 || name.IndexOf('>') >= 0 || name.IndexOf(',') >= 0;
    }

    public ISymbol? ResolveMetadataMember(INamespaceSymbol namespaceSymbol, string name)
    {
        if (string.IsNullOrEmpty(name))
            return null;

        // This API only supports metadata names (no type arguments). If a display-form name
        // slips through (e.g. contains "<" or ","), do not forward it to reflection.
        if (LooksLikeNonMetadataTypeName(name))
            return null;

        var nsName = namespaceSymbol.ToMetadataName();
        var fullName = string.IsNullOrEmpty(nsName) ? name : nsName + "." + name;

        if (_resolvedMetadataTypes.TryGetValue(fullName, out var cached))
            return cached;

        var assembly = PEContainingAssembly.GetAssemblyInfo();

        var value = ResolveTypeFromAssembly(fullName, assembly);
        if (value is not null)
        {
            if (value is INamedTypeSymbol named)
                _resolvedMetadataTypes[fullName] = named;

            return value;
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

            if (parentNs is PENamespaceSymbol peParent)
            {
                var module = peParent.ContainingModule as PEModuleSymbol ?? this;
                var nestedNamespace = new PENamespaceSymbol(_reflectionTypeLoader, module, name, peParent, peParent);
                return nestedNamespace;
            }

            return new PENamespaceSymbol(_reflectionTypeLoader, name, parentNs, parentNs);
        }

        return null;
    }

    private ISymbol? ResolveTypeFromAssembly(string fullName, Assembly assembly)
    {
        // If a display-form name slips through, `Assembly.GetType` (under MetadataLoadContext)
        // may interpret the comma in generic argument lists as an assembly separator and throw.
        if (LooksLikeNonMetadataTypeName(fullName))
            return null;

        var type = assembly.GetType(fullName, throwOnError: false, ignoreCase: false);
        if (type is not null)
        {
            return GetType(type);
        }

        return null;
    }

    /*
        private ITypeSymbol GetOrCreateTypeSymbol(Type type)
        {
            if (_typeSymbolTypeInfoMapping.TryGetValue(type, out var existing))
                return existing;

            // Nested types must be created under their declaring type, not under the namespace.
            if (type.IsNested && type.DeclaringType is not null)
            {
                var declaring = GetType(type.DeclaringType) as INamedTypeSymbol;
                if (declaring is null)
                    return compilation.GetTypeByMetadataName(type.FullName ?? type.Name) ?? compilation.ErrorTypeSymbol;

                // Ensure the declaring type has had a chance to load its members so the nested container is stable.
                _ = declaring.GetMembers();

                var containingNamespace = declaring.ContainingNamespace;
                return CreateMetadataTypeSymbol(type.GetTypeInfo(), containingNamespace, declaring, declaring);
            }

            // Top-level type: create under its namespace.
            var ns = GetOrCreateNamespaceSymbol(type.Namespace);
            return CreateMetadataTypeSymbol(type.GetTypeInfo(), ns, containingType: null, containingSymbol: ns);
        }
        */

    private ITypeSymbol GetOrCreateTypeSymbol(Type type)
    {
        if (_typeSymbolTypeInfoMapping.TryGetValue(type, out var existing))
            return existing;

        // Nested types must be created under their declaring type, not under the namespace.
        if (type.IsNested && type.DeclaringType is not null)
        {
            var declaring = GetType(type.DeclaringType) as INamedTypeSymbol;
            if (declaring is null)
            {
                // Fallback: attempt direct creation; will still be cached by Type.
                return CreateMetadataTypeSymbol(type);
            }

            // Ensure the declaring type has had a chance to load its members so the nested container is stable.
            _ = declaring.GetMembers();

            var containingNamespace = declaring.ContainingNamespace;
            return CreateMetadataTypeSymbol(type.GetTypeInfo(), containingNamespace, declaring, declaring);
        }

        // Top-level type: create under its namespace.
        return CreateMetadataTypeSymbol(type);
    }

    private PENamedTypeSymbol CreateMetadataTypeSymbol(Type type)
    {
        var ns = GetOrCreateNamespaceSymbol(type.Namespace);
        return CreateMetadataTypeSymbol(type.GetTypeInfo(), ns, containingType: null, containingSymbol: ns);
    }

    internal PENamedTypeSymbol CreateMetadataTypeSymbol(
        System.Reflection.TypeInfo typeInfo,
        INamespaceSymbol containingNamespace,
        INamedTypeSymbol? containingType,
        ISymbol containingSymbol)
    {
        if (_typeSymbolTypeInfoMapping.TryGetValue(typeInfo.AsType(), out var existingSymbol))
        {
            return (PENamedTypeSymbol)existingSymbol;
        }

        PENamedTypeSymbol typeSymbol;

        if (typeInfo.IsArray)
        {
            typeSymbol = new PEArrayTypeSymbol(
                _reflectionTypeLoader,
                typeInfo, containingSymbol, containingType, containingNamespace,
                [new MetadataLocation(containingNamespace.ContainingModule!)]);
        }
        else
        {
            typeSymbol = PENamedTypeSymbol.Create(
                _reflectionTypeLoader,
                typeInfo,
                containingSymbol,
                containingType,
                containingNamespace,
                [new MetadataLocation(containingNamespace.ContainingModule!)]);
        }

        _typeSymbolTypeInfoMapping[typeInfo.AsType()] = typeSymbol;

        return typeSymbol;
    }

    private INamespaceSymbol GetOrCreateNamespaceSymbol(string? ns)
    {
        if (string.IsNullOrEmpty(ns))
            return GlobalNamespace;

        var parts = ns.Split('.');
        var current = (INamespaceSymbol)GlobalNamespace;

        foreach (var part in parts)
        {
            var next = current.GetMembers(part)
                .OfType<INamespaceSymbol>()
                .FirstOrDefault();

            if (next is null)
            {
                next = new PENamespaceSymbol(_reflectionTypeLoader, part, _assembly, current);
            }

            current = next;
        }

        return current;
    }
}
