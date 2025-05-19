using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceNamespaceSymbol : SourceSymbol, INamespaceSymbol
{
    private readonly List<ISymbol> _members = new();
    private readonly SourceModuleSymbol _containingModule;

    public SourceNamespaceSymbol(SourceModuleSymbol containingModule, string name, ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        _containingModule = containingModule;
    }

    public SourceNamespaceSymbol(string name, ISymbol containingSymbol,
        INamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
    : base(SymbolKind.Namespace, name, containingSymbol, null, containingNamespace, locations, declaringSyntaxReferences)
    {

    }

    public override IModuleSymbol ContainingModule => _containingModule ?? ContainingSymbol!.ContainingModule!;

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    internal void AddMember(ISymbol member) => _members.Add(member);

    public ImmutableArray<ISymbol> GetMembers() => _members.ToImmutableArray();

    public ImmutableArray<ISymbol> GetMembers(string name) =>
        _members.Where(m => m.Name == name).ToImmutableArray();

    public INamespaceSymbol? LookupNamespace(string name) =>
        _members.OfType<INamespaceSymbol>().FirstOrDefault(ns => ns.Name == name);

    public ITypeSymbol? LookupType(string name) =>
        _members.OfType<ITypeSymbol>().FirstOrDefault(t => t.Name == name);

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

internal sealed partial class MetadataNamespaceSymbol : MetadataSymbol, INamespaceSymbol
{
    private MetadataModuleSymbol _module = default!;
    private readonly List<ISymbol> _members = new();
    private readonly string _name;

    public MetadataNamespaceSymbol(string name, ISymbol containingSymbol,
    INamespaceSymbol? containingNamespace)
    : base(containingSymbol, null, containingNamespace, [])
    {
        _name = name;
    }

    public MetadataNamespaceSymbol(MetadataModuleSymbol containingModule, string name, ISymbol containingSymbol,
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

internal sealed partial class MergedNamespaceSymbol : Symbol, INamespaceSymbol
{
    private readonly SourceNamespaceSymbol _source;
    private readonly MetadataNamespaceSymbol _metadata;

    public MergedNamespaceSymbol(SourceNamespaceSymbol source, MetadataNamespaceSymbol metadata)
        : base(SymbolKind.Namespace,
               source.Name,
               source.ContainingSymbol,
               null,
               source.ContainingNamespace ?? metadata.ContainingNamespace,
               [],
               [])
    {
        _source = source;
        _metadata = metadata;
    }

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    public ImmutableArray<ISymbol> GetMembers()
    {
        var combined = new List<ISymbol>(_source.GetMembers());
        combined.AddRange(_metadata.GetMembers());
        return combined.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        var combined = new List<ISymbol>(_source.GetMembers(name));
        combined.AddRange(_metadata.GetMembers(name));
        return combined.ToImmutableArray();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        if (_source.IsMemberDefined(name, out symbol)) return true;
        if (_metadata.IsMemberDefined(name, out symbol)) return true;

        symbol = null;
        return false;
    }

    public INamespaceSymbol? LookupNamespace(string name)
    {
        var sourceChild = _source.LookupNamespace(name) as SourceNamespaceSymbol;
        var metadataChild = _metadata.LookupNamespace(name) as MetadataNamespaceSymbol;

        if (sourceChild is not null && metadataChild is not null)
            return new MergedNamespaceSymbol(sourceChild, metadataChild);

        return sourceChild ?? (INamespaceSymbol?)metadataChild;
    }

    public ITypeSymbol? LookupType(string name)
    {
        // You can prioritize source over metadata, or merge lists if needed
        return _source.LookupType(name) ?? _metadata.LookupType(name);
    }

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

    public override string ToString() => IsGlobalNamespace ? "<global>" : this.ToDisplayString();
}