using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal class SourceTypeSymbol : SourceSymbol, ITypeSymbol, INamedTypeSymbol
{
    private List<ISymbol> _members = new List<ISymbol>();

    public SourceTypeSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        
    }
    
    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;
    
    public ImmutableArray<IMethodSymbol> Constructors { get; }
    public IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }
    
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
}