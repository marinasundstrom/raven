using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataTypeSymbol : MetadataSymbol, ITypeSymbol, INamedTypeSymbol
{
    private readonly TypeInfo _typeInfo;
    private List<ISymbol> _members = new List<ISymbol>();

    public MetadataTypeSymbol(TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _typeInfo = typeInfo;
    }

    public override SymbolKind Kind => SymbolKind.Type;
    public override string Name => _typeInfo.Name;
    
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