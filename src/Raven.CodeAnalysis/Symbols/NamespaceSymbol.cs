using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal class NamespaceSymbol : SourceSymbol, INamespaceSymbol
{
    private readonly List<ISymbol> _members = new List<ISymbol>();

    public NamespaceSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {

    }

    public bool IsNamespace { get; } = true;
    public bool IsType { get; } = false;

    public ImmutableArray<ISymbol> GetMembers()
    {
        return _members.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return _members.Where(x => x.Name == name).ToImmutableArray();
    }

    internal void AddMember(ISymbol member)
    {
        _members.Add(member);
    }

    public bool IsGlobalNamespace => ContainingNamespace is null;
}