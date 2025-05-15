using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal class SourceTypeSymbol : SourceSymbol, ITypeSymbol, INamedTypeSymbol
{
    private readonly List<ISymbol> _members = new List<ISymbol>();

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

    public SpecialType SpecialType => SpecialType.None;

    public bool IsValueType => throw new NotImplementedException();

    public INamedTypeSymbol? BaseType => throw new NotImplementedException();

    public bool IsArray => false;

    public ImmutableArray<ISymbol> GetMembers()
    {
        return _members.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return _members.Where(x => x.Name == name).ToImmutableArray();
    }

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    internal void AddMember(ISymbol member)
    {
        _members.Add(member);
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }
}