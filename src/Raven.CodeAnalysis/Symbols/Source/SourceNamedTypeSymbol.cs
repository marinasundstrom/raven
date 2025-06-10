using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceNamedTypeSymbol : SourceSymbol, INamedTypeSymbol
{
    private readonly List<ISymbol> _members = new List<ISymbol>();

    public SourceNamedTypeSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        BaseType = containingSymbol.ContainingAssembly!.GetTypeByMetadataName("System.Object");

        TypeKind = TypeKind.Class;
    }

    public SourceNamedTypeSymbol(string name, INamedTypeSymbol baseType, TypeKind typeKind, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
    : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        BaseType = baseType;

        TypeKind = typeKind;
    }

    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;

    public ImmutableArray<IMethodSymbol> Constructors { get; }
    public IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }
    public ITypeSymbol ConstructedFrom { get; }
    public bool IsGenericType { get; }
    public bool IsUnboundGenericType { get; }

    public SpecialType SpecialType => SpecialType.None;

    public virtual INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public int Arity { get; } = 0;

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

    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        throw new NotImplementedException();
    }
}