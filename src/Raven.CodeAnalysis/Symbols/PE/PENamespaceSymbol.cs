using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class PENamespaceSymbol : PESymbol, INamespaceSymbol
{
    private readonly ReflectionTypeLoader _reflectionTypeLoader;
    private readonly PEModuleSymbol _module = default!;
    private readonly List<ISymbol> _members = new(); // new(SymbolEqualityComparer.Default);
    private readonly string _name;
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

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    internal void AddMember(ISymbol member)
    {
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
        return _members.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        EnsureMembersLoaded();
        return _members.Where(m => m.Name == name).ToImmutableArray();
    }

    public INamespaceSymbol? LookupNamespace(string name)
    {
        EnsureMembersLoaded();
        return _members.OfType<INamespaceSymbol>().FirstOrDefault(ns => ns.Name == name);
    }

    public ITypeSymbol? LookupType(string name)
    {
        EnsureMembersLoaded();
        var type = _members.OfType<ITypeSymbol>().FirstOrDefault(t => t.Name == name);
        if (type != null)
            return type;

        var fullName = string.IsNullOrEmpty(MetadataName) ? name : MetadataName + "." + name;
        return (ContainingAssembly as PEAssemblySymbol)?.GetTypeByMetadataName(fullName);
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        EnsureMembersLoaded();
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
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
        if (_membersLoaded)
            return;

        _membersLoaded = true;

        var assemblyInfo = PEContainingAssembly.GetAssemblyInfo();

        foreach (var type in assemblyInfo.GetTypes().Where(x => !x.IsNested))
        {
            if (type.Namespace != MetadataName)
                continue;

            // IMPORTANT: Always intern types via the module's Type-based cache.
            // Creating symbols directly here bypasses the module cache and can create duplicate type symbols
            // (e.g., two instances of Result`2 with different loaded states).
            var module = (PEModuleSymbol)ContainingModule;
            _ = module.GetType(type);
        }

        foreach (var nsName in FindNestedNamespaces(assemblyInfo))
        {
            var childName = nsName.Split('.').Last(); // e.g., for "System.IO", take "IO"
            var nestedNamespace = new PENamespaceSymbol(_reflectionTypeLoader, _module, childName, this, this);
            //AddMember(nestedNamespace);
        }

        foreach (var member in _members.OfType<PEAssemblySymbol>())
        {
            member.Complete();
        }
    }

    private IEnumerable<string> FindNestedNamespaces(Assembly assembly)
    {
        var thisName = MetadataName;
        var prefix = string.IsNullOrEmpty(thisName) ? "" : thisName + ".";

        return assembly.GetTypes()
            .Select(t => t.Namespace ?? string.Empty)
            .Where(ns => ns.StartsWith(prefix) && ns.Length > prefix.Length)
            .Select(ns => ns.Substring(0, ns.IndexOf('.', prefix.Length) > -1
                ? ns.IndexOf('.', prefix.Length)
                : ns.Length))
            .Distinct();
    }
}
