using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class NamespaceSymbol : SourceSymbol, INamespaceSymbol
{
    private readonly List<ISymbol> _members = new List<ISymbol>();
    private Compilation _compilation;

    public NamespaceSymbol(string name, ISymbol containingSymbol, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
    : base(SymbolKind.Namespace, name, containingSymbol, null, containingNamespace, locations, declaringSyntaxReferences)
    {

    }

    public NamespaceSymbol(Compilation compilation, string name, ISymbol containingSymbol, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, null, null, locations, declaringSyntaxReferences)
    {
        _compilation = compilation;
    }

    public override Compilation Compilation => _compilation ?? ContainingNamespace!.Compilation;

    public bool IsNamespace { get; } = true;
    public bool IsType { get; } = false;

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

    internal void AddMember(ISymbol member)
    {
        _members.Add(member);
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

    public bool IsGlobalNamespace => ContainingNamespace is null;

    public string ToMetadataName()
    {
        var parts = new Stack<string>();
        var current = this;

        while (!current.IsGlobalNamespace)
        {
            parts.Push(current.Name);
            current = (NamespaceSymbol)current.ContainingNamespace!;
        }

        return string.Join(".", parts);
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }

    public override string ToString()
    {
        if (IsGlobalNamespace) return "<global>";

        return this.ToDisplayString();
    }
}