using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class PENamespaceSymbol : PESymbol, INamespaceSymbol
{
    private readonly ReflectionTypeLoader _reflectionTypeLoader;
    private readonly PEModuleSymbol _module = default!;
    private readonly List<ISymbol> _members = new(); // new(SymbolEqualityComparer.Default);
    private readonly object _membersGate = new();
    private readonly HashSet<string> _memberNamesLoaded = new(StringComparer.Ordinal);
    private readonly HashSet<string> _memberNamesLoading = new(StringComparer.Ordinal);
    private readonly string _name;
    private bool _membersLoading;
    private bool _membersLoaded;

    public PENamespaceSymbol(ReflectionTypeLoader reflectionTypeLoader, string name, ISymbol containingSymbol, INamespaceSymbol? containingNamespace)
        : base(containingSymbol, null, containingNamespace, [])
    {
        _reflectionTypeLoader = reflectionTypeLoader;
        _name = name;
    }

    public PENamespaceSymbol(ReflectionTypeLoader reflectionTypeLoader, PEModuleSymbol containingModule, string name, ISymbol containingSymbol, INamespaceSymbol? containingNamespace)
        : base(containingSymbol, null, containingNamespace, [])
    {
        _reflectionTypeLoader = reflectionTypeLoader;
        _module = containingModule;
        _name = name;
    }

    public override IAssemblySymbol ContainingAssembly => ContainingModule!.ContainingAssembly!;
    public override IModuleSymbol ContainingModule => _module ?? ContainingNamespace!.ContainingModule!;
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

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    internal void AddMember(ISymbol member)
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

    public ImmutableArray<ISymbol> GetMembers()
    {
        EnsureMembersLoaded();
        lock (_membersGate)
            return _members.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        EnsureMemberWithNameLoaded(name);
        lock (_membersGate)
            return _members.Where(m => m.Name == name).ToImmutableArray();
    }

    public INamespaceSymbol? LookupNamespace(string name)
    {
        EnsureMemberWithNameLoaded(name);
        lock (_membersGate)
            return _members.OfType<INamespaceSymbol>().FirstOrDefault(ns => ns.Name == name);
    }

    public ITypeSymbol? LookupType(string name)
    {
        EnsureMemberWithNameLoaded(name);
        ImmutableArray<ITypeSymbol> candidates;
        lock (_membersGate)
            candidates = _members.OfType<ITypeSymbol>().Where(t => t.Name == name).ToImmutableArray();

        var type = TypeLookupUtilities.SelectBestTypeByName(candidates);
        if (type != null)
            return type;

        var fullName = string.IsNullOrEmpty(MetadataName) ? name : MetadataName + "." + name;
        return (ContainingAssembly as PEAssemblySymbol)?.GetTypeByMetadataName(fullName);
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        EnsureMemberWithNameLoaded(name);
        lock (_membersGate)
            symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }

    internal ImmutableArray<INamedTypeSymbol> GetExtensionMethodContainers(string methodName)
    {
        if (string.IsNullOrWhiteSpace(methodName))
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var module = (PEModuleSymbol)ContainingModule;
        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        foreach (var type in module.GetExtensionMethodContainersInNamespace(MetadataName, methodName))
        {
            if (module.GetType(type) is INamedTypeSymbol typeSymbol)
                builder.Add(typeSymbol);
        }

        return builder.ToImmutable();
    }

    public override string ToString() => IsGlobalNamespace ? "<global>" : ToMetadataName();

    public string ToMetadataName()
    {
        var parts = new Stack<string>();
        for (INamespaceSymbol current = this; !current.IsGlobalNamespace; current = current.ContainingNamespace!)
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
