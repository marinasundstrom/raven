using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class PENamespaceSymbol : PESymbol, INamespaceSymbol
{
    private readonly TypeResolver _typeResolver;
    private readonly PEModuleSymbol _module = default!;
    private readonly List<ISymbol> _members = new();
    private readonly string _name;
    private bool _membersLoaded;

    public PENamespaceSymbol(TypeResolver typeResolver, string name, ISymbol containingSymbol, INamespaceSymbol? containingNamespace)
        : base(containingSymbol, null, containingNamespace, [])
    {
        _typeResolver = typeResolver;
        _name = name;
    }

    public PENamespaceSymbol(TypeResolver typeResolver, PEModuleSymbol containingModule, string name, ISymbol containingSymbol, INamespaceSymbol? containingNamespace)
        : base(containingSymbol, null, containingNamespace, [])
    {
        _typeResolver = typeResolver;
        _module = containingModule;
        _name = name;
    }

    public override IAssemblySymbol ContainingAssembly => ContainingModule!.ContainingAssembly!;
    public override IModuleSymbol ContainingModule => _module ?? ContainingSymbol!.ContainingModule!;
    public override string Name => _name;
    public override SymbolKind Kind => SymbolKind.Namespace;

    public override string MetadataName => IsGlobalNamespace ? "" : ToMetadataName();

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    internal void AddMember(ISymbol symbol) => _members.Add(symbol);

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
        return _members.OfType<ITypeSymbol>().FirstOrDefault(t => t.Name == name);
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        EnsureMembersLoaded();
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }

    public override string ToString() => IsGlobalNamespace ? "<global>" : this.ToDisplayString();

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

        foreach (var type in assemblyInfo.GetTypes())
        {
            if (type.Namespace != MetadataName)
                continue;

            var typeSymbol = new PENamedTypeSymbol(
                _typeResolver,
                type.GetTypeInfo(),
                this,
                null,
                this,
                [new MetadataLocation(ContainingModule!)]);

            //AddMember(typeSymbol);
        }

        foreach (var nsName in FindNestedNamespaces(assemblyInfo))
        {
            var childName = nsName.Split('.').Last(); // e.g., for "System.IO", take "IO"
            var nestedNamespace = new PENamespaceSymbol(_typeResolver, _module, childName, this, this);
            //AddMember(nestedNamespace);
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