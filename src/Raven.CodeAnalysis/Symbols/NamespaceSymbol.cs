using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal class NamespaceSymbol : SourceSymbol, INamespaceSymbol
{
    private readonly List<ISymbol> _members = new List<ISymbol>();
    private Compilation _compilation;

    public NamespaceSymbol(Compilation compilation, string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        _compilation = compilation;
    }

    public override Compilation Compilation => _compilation;

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
            AddMember(metadataSymbol); // Ensure next call finds it directly
            return [metadataSymbol];
        }

        return [];
    }

    /*

    public override IEnumerable<ISymbol> GetMembers(string name)
    {
        var matches = _members.Where(m => m.Name == name).ToList();

        if (matches.Count > 0)
            return matches;

        // Lazy resolve from metadata
        var metadataSymbol = _compilation.ResolveMetadataMember(this, name);

        if (metadataSymbol is not null)
        {
            AddMember(metadataSymbol); // Ensure next call finds it directly
            return [metadataSymbol];
        }

        return [];
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return _members.Where(x => x.Name == name).ToImmutableArray();
    }
    
    */


    internal void AddMember(ISymbol member)
    {
        _members.Add(member);
    }

    public INamespaceSymbol? LookupNamespace(string name)
    {
        throw new NotImplementedException();
    }

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
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
}