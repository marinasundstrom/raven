using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class PENamespaceSymbol : PESymbol, NamespaceSymbol, INamespaceExtensionLookup
{
    private readonly ReflectionTypeLoader _reflectionTypeLoader;
    private readonly PEModuleSymbol _module = default!;
    private readonly List<Symbol> _members = new(); // new(SymbolEqualityComparer.Default);
    private readonly object _membersGate = new();
    private readonly HashSet<string> _memberNamesLoaded = new(StringComparer.Ordinal);
    private readonly HashSet<string> _memberNamesLoading = new(StringComparer.Ordinal);
    private readonly string _name;
    private bool _membersLoading;
    private bool _membersLoaded;

    public PENamespaceSymbol(ReflectionTypeLoader reflectionTypeLoader, string name, Symbol containingSymbol, NamespaceSymbol? containingNamespace)
        : base(containingSymbol, null, containingNamespace, [])
    {
        _reflectionTypeLoader = reflectionTypeLoader;
        _name = name;
    }

    public PENamespaceSymbol(ReflectionTypeLoader reflectionTypeLoader, PEModuleSymbol containingModule, string name, Symbol containingSymbol, NamespaceSymbol? containingNamespace)
        : base(containingSymbol, null, containingNamespace, [])
    {
        _reflectionTypeLoader = reflectionTypeLoader;
        _module = containingModule;
        _name = name;
    }

    public override AssemblySymbol ContainingAssembly => ContainingModule!.ContainingAssembly!;
    public override ModuleSymbol ContainingModule => _module ?? ContainingNamespace!.ContainingModule!;
    public override string Name => _name;
    public override SymbolKind Kind => SymbolKind.Namespace;

    public override string MetadataName => IsGlobalNamespace ? "" : ToMetadataName();

    internal bool IsLoadingMembers
    {
        get
        {
            lock (_membersGate)
                return _membersLoading;
        }
    }

    public bool IsModule => _reflectionTypeLoader.Compilation.Options.TargetPlatform == TargetPlatform.NeoCLR;
    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    internal void AddMember(Symbol member)
    {
        lock (_membersGate)
            _members.Add(member);

        /*
        if (!_members.Add(member))
        {
            throw new InvalidOperationException($"Member '{member.ToDisplayString()}' has already been added to namespace '{this.ToDisplayString()}'");
        }
        */
    }

    private bool IsSelectedDeclaration(Symbol symbol) => symbol is not TypeSymbol type ||
        _reflectionTypeLoader.Compilation.Options.MetadataImportOptions?.PrimitiveAssemblies.TryGetValue(type.SpecialType, out var provider) != true ||
        type.ContainingAssembly?.Name == provider;

    public ImmutableArray<Symbol> GetMembers()
    {
        EnsureMembersLoaded();
        lock (_membersGate)
            return _members.Where(IsSelectedDeclaration).ToImmutableArray();
    }

    public ImmutableArray<Symbol> GetMembers(string name)
    {
        EnsureMemberWithNameLoaded(name);
        lock (_membersGate)
            return _members.Where(m => m.Name == name && IsSelectedDeclaration(m)).ToImmutableArray();
    }

    public NamespaceSymbol? LookupNamespace(string name)
    {
        EnsureMemberWithNameLoaded(name);
        lock (_membersGate)
            return _members.OfType<NamespaceSymbol>().FirstOrDefault(ns => ns.Name == name);
    }

    public TypeSymbol? LookupType(string name)
    {
        EnsureMemberWithNameLoaded(name);
        NamedTypeSymbol? first = null;
        List<TypeSymbol>? multiple = null;
        lock (_membersGate)
        {
            foreach (var member in _members)
            {
                if (member is not NamedTypeSymbol candidate || candidate.Name != name || !IsSelectedDeclaration(candidate))
                    continue;

                if (first is null)
                    first = candidate;
                else
                    (multiple ??= [first]).Add(candidate);
            }
        }

        var type = multiple is null ? first : TypeLookupUtilities.SelectBestTypeByName(multiple);
        if (type is not null)
            return type;

        var namespaceName = MetadataName;
        var fullName = string.IsNullOrEmpty(namespaceName) ? name : namespaceName + "." + name;
        var fallback = (ContainingAssembly as PEAssemblySymbol)?.GetTypeByMetadataName(fullName);
        return fallback is not null && IsSelectedDeclaration(fallback) ? fallback : null;
    }

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        EnsureMemberWithNameLoaded(name);
        lock (_membersGate)
            symbol = _members.FirstOrDefault(m => m.Name == name && IsSelectedDeclaration(m));
        return symbol is not null;
    }

    public ImmutableArray<NamedTypeSymbol> GetExtensionMethodContainers(string methodName)
    {
        if (string.IsNullOrWhiteSpace(methodName))
            return ImmutableArray<NamedTypeSymbol>.Empty;

        var module = (PEModuleSymbol)ContainingModule;
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();

        foreach (var type in module.GetExtensionMethodContainersInNamespace(MetadataName, methodName))
        {
            if (module.GetType(type) is NamedTypeSymbol typeSymbol)
                builder.Add(typeSymbol);
        }

        return builder.ToImmutable();
    }

    public override string ToString() => IsGlobalNamespace ? "<global>" : ToMetadataName();

    public string ToMetadataName()
    {
        var parts = new Stack<string>();
        for (NamespaceSymbol current = this; !current.IsGlobalNamespace; current = current.ContainingNamespace!)
            parts.Push(current.Name);

        return string.Join(".", parts);
    }

    private void EnsureMembersLoaded()
    {
        lock (_membersGate)
        {
            while (_membersLoading || _memberNamesLoading.Count > 0)
                Monitor.Wait(_membersGate);

            if (_membersLoaded)
                return;

            _membersLoading = true;
        }

        try
        {
            var module = (PEModuleSymbol)ContainingModule;

            foreach (var type in module.GetTopLevelTypesInNamespace(MetadataName))
            {
                // IMPORTANT: Always intern types via the module's Type-based cache.
                // Creating symbols directly here bypasses the module cache and can create duplicate type symbols
                // (e.g., two instances of Result`2 with different loaded states).
                _ = module.GetType(type);
            }

            foreach (var childName in module.GetDirectNestedNamespaceNames(MetadataName))
            {
                var fullName = string.IsNullOrEmpty(MetadataName)
                    ? childName
                    : MetadataName + "." + childName;
                _ = module.GetOrCreateNamespaceSymbol(fullName);
            }

            ImmutableArray<PEAssemblySymbol> assemblyMembers;
            lock (_membersGate)
                assemblyMembers = _members.OfType<PEAssemblySymbol>().ToImmutableArray();

            foreach (var member in assemblyMembers)
                member.Complete();
        }
        finally
        {
            lock (_membersGate)
            {
                _membersLoaded = true;
                _membersLoading = false;
                Monitor.PulseAll(_membersGate);
            }
        }
    }

    private void EnsureMemberWithNameLoaded(string name)
    {
        if (string.IsNullOrEmpty(name))
        {
            EnsureMembersLoaded();
            return;
        }

        lock (_membersGate)
        {
            while (_membersLoading || _memberNamesLoading.Contains(name))
                Monitor.Wait(_membersGate);

            if (_membersLoaded || _memberNamesLoaded.Contains(name))
                return;

            _memberNamesLoading.Add(name);
        }

        try
        {
            var module = (PEModuleSymbol)ContainingModule;

            foreach (var type in module.GetTopLevelTypesInNamespace(MetadataName, name))
                _ = module.GetType(type);

            var childNamespaceName = string.IsNullOrEmpty(MetadataName)
                ? name
                : MetadataName + "." + name;

            if (module.MetadataNamespaceExists(childNamespaceName))
                _ = module.GetOrCreateNamespaceSymbol(childNamespaceName);
        }
        finally
        {
            lock (_membersGate)
            {
                _memberNamesLoaded.Add(name);
                _memberNamesLoading.Remove(name);
                Monitor.PulseAll(_membersGate);
            }
        }
    }
}
