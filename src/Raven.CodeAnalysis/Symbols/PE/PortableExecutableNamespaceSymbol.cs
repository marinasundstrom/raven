using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class PortableExecutableNamespaceSymbol : PortableExecutableSymbol, INamespaceSymbol
{
    private PortableExecutableModuleSymbol _module = default!;
    private readonly List<ISymbol> _members = new();
    private readonly string _name;

    public PortableExecutableNamespaceSymbol(string name, ISymbol containingSymbol,
    INamespaceSymbol? containingNamespace)
    : base(containingSymbol, null, containingNamespace, [])
    {
        _name = name;
    }

    public PortableExecutableNamespaceSymbol(PortableExecutableModuleSymbol containingModule, string name, ISymbol containingSymbol,
        INamespaceSymbol? containingNamespace)
        : base(containingSymbol, null, containingNamespace, [])
    {
        _module = containingModule;
        _name = name;
    }

    //public override Compilation Compilation => _compilation ?? ContainingNamespace!.Compilation;

    public override IAssemblySymbol ContainingAssembly => ContainingModule!.ContainingAssembly!;

    public override IModuleSymbol ContainingModule => _module ?? ContainingSymbol!.ContainingModule!;

    public override string Name => _name;

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    internal void AddMember(ISymbol symbol) => _members.Add(symbol);

    public ImmutableArray<ISymbol> GetMembers()
    {
        return _members.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        var matches = _members.Where(m => m.Name == name).ToList();

        if (matches.Count > 0)
            return matches.ToImmutableArray();

        // Lazy resolve from metadata
        var metadataSymbol = Compilation.ResolveMetadataMember(this, name);

        if (metadataSymbol is not null)
        {
            return [metadataSymbol];
        }

        return [];
    }

    public INamespaceSymbol? LookupNamespace(string name)
    {
        // Check existing members
        foreach (var member in _members)
        {
            if (member is INamespaceSymbol ns && member.Name == name)
                return ns;
        }

        // Lazy resolve from metadata (assumes ResolveMetadataMember handles namespaces)
        var resolved = Compilation.ResolveMetadataMember(this, name);

        if (resolved is INamespaceSymbol nsResolved)
        {
            return nsResolved;
        }

        return null;
    }

    public ITypeSymbol? LookupType(string name)
    {
        // Check if a matching type already exists in this namespace
        foreach (var member in _members)
        {
            if (member is ITypeSymbol type && member.Name == name)
                return type;
        }

        // Attempt to resolve from metadata (e.g., Console in System)
        var metadataSymbol = Compilation.ResolveMetadataMember(this, name);

        if (metadataSymbol is ITypeSymbol typeSymbol)
        {
            return typeSymbol;
        }

        // Could be a namespace, but this method only cares about types
        return null;
    }

    public override string ToString() => IsGlobalNamespace ? "<global>" : this.ToDisplayString();

    public string ToMetadataName()
    {
        var parts = new Stack<string>();
        INamespaceSymbol current = this;

        while (!current.IsGlobalNamespace)
        {
            parts.Push(current.Name);
            current = current.ContainingNamespace!;
        }

        return string.Join(".", parts);
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }
}
