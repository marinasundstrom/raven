using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceNamedTypeSymbol : SourceSymbol, INamedTypeSymbol
{
    private readonly List<ISymbol> _members = new List<ISymbol>();
    private ImmutableArray<INamedTypeSymbol> _interfaces = ImmutableArray<INamedTypeSymbol>.Empty;
    private ImmutableArray<INamedTypeSymbol>? _allInterfaces;

    public SourceNamedTypeSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        BaseType = containingSymbol.ContainingAssembly!.GetTypeByMetadataName("System.Object");

        TypeKind = TypeKind.Class;
        IsSealed = true;
    }

    public SourceNamedTypeSymbol(string name, INamedTypeSymbol baseType, TypeKind typeKind, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, bool isSealed = false)
    : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        BaseType = baseType;

        TypeKind = typeKind;
        IsSealed = isSealed;
    }

    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;

    public ImmutableArray<IMethodSymbol> Constructors => _members
        .OfType<SourceMethodSymbol>()
        .Where(x => x.MethodKind == MethodKind.Constructor)
        .Cast<IMethodSymbol>()
        .ToImmutableArray();

    public IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }
    public ITypeSymbol? ConstructedFrom { get; }
    public bool IsAbstract { get; } = false;
    public bool IsSealed { get; }
    public bool IsGenericType { get; }
    public bool IsUnboundGenericType { get; }

    public ImmutableArray<INamedTypeSymbol> Interfaces => _interfaces;
    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        _allInterfaces ??=
            _interfaces
                .Concat(_interfaces.SelectMany(i => i.AllInterfaces))
                .Concat(BaseType?.AllInterfaces ?? ImmutableArray<INamedTypeSymbol>.Empty)
                .Distinct<INamedTypeSymbol>(SymbolEqualityComparer.Default)
                .ToImmutableArray();

    public bool IsValueType => TypeKind == TypeKind.Struct || TypeKind == TypeKind.Enum;

    public override string MetadataName => !ContainingNamespace!.IsGlobalNamespace ? ContainingNamespace.MetadataName + "." + Name : Name;

    public bool IsReferenceType =>
        TypeKind == TypeKind.Class ||
        TypeKind == TypeKind.Interface ||
        TypeKind == TypeKind.Delegate ||
        TypeKind == TypeKind.Array;

    public SpecialType SpecialType => SpecialType.None;

    public virtual INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public int Arity { get; } = 0;

    public INamedTypeSymbol UnderlyingTupleType => throw new NotImplementedException();

    public ImmutableArray<IFieldSymbol> TupleElements => throw new NotImplementedException();

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

    internal void SetInterfaces(IEnumerable<INamedTypeSymbol> interfaces)
    {
        _interfaces = interfaces.ToImmutableArray();
        _allInterfaces = null;
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
