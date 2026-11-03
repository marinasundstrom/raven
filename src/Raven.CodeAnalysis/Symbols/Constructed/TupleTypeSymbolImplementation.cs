using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class TupleTypeSymbolImplementation : PESymbol, TupleTypeSymbol
{
    private ImmutableArray<FieldSymbol> _tupleElements;

    public TupleTypeSymbolImplementation(
        NamedTypeSymbol underlyingTupleType,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        UnderlyingTupleType = underlyingTupleType;
        BaseType = underlyingTupleType.BaseType;

        TypeKind = TypeKind.Tuple;
    }

    public void SetTupleElements(IEnumerable<FieldSymbol> fieldSymbols)
    {
        _tupleElements = fieldSymbols.ToImmutableArray();
    }

    public override string Name => "ValueTuple";

    public override string MetadataName => UnderlyingTupleType.MetadataName;

    public override SymbolKind Kind => SymbolKind.Type;

    public SpecialType SpecialType => UnderlyingTupleType.SpecialType;

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsValueType => true;

    public int Rank { get; }

    public NamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public TypeSymbol? OriginalDefinition { get; }

    public int Arity { get; } = 0;

    public ImmutableArray<MethodSymbol> Constructors { get; } = [];

    public ImmutableArray<MethodSymbol> InstanceConstructors { get; } = [];

    public MethodSymbol? StaticConstructor { get; } = null;

    public NamedTypeSymbol UnderlyingTupleType { get; }

    public ImmutableArray<FieldSymbol> TupleElements => _tupleElements;

    public ImmutableArray<TypeSymbol> TypeArguments =>
        TupleElements.Select(e => e.Type).ToImmutableArray();

    public ImmutableArray<TypeParameterSymbol> TypeParameters { get; } = [];

    public TypeSymbol? ConstructedFrom { get; } = null;

    public bool IsAbstract => false;

    public bool IsClosed => true;

    public bool IsGenericType => false;

    public bool IsUnboundGenericType => false;

    public ImmutableArray<NamedTypeSymbol> Interfaces => UnderlyingTupleType.Interfaces;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => UnderlyingTupleType.AllInterfaces;

    public ImmutableArray<Symbol> GetMembers()
    {
        IEnumerable<Symbol> symbols = UnderlyingTupleType.GetMembers();
        return symbols.Concat(TupleElements.CastArray<Symbol>()).ToImmutableArray();
    }

    public ImmutableArray<Symbol> GetMembers(string name)
    {
        IEnumerable<Symbol> symbols = UnderlyingTupleType.GetMembers(name);
        return symbols.Concat(TupleElements.Where(f => f.Name == name).Cast<Symbol>()).ToImmutableArray();
    }

    public TypeSymbol? LookupType(string name) => UnderlyingTupleType.LookupType(name);

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = TupleElements.FirstOrDefault(field => field.Name == name);
        if (symbol is not null)
            return true;

        return UnderlyingTupleType.IsMemberDefined(name, out symbol);
    }

    public TypeSymbol Construct(params TypeSymbol[] typeArguments) => this;

    public override string ToString()
    {
        var parts = TupleElements
            .Select(e => string.IsNullOrEmpty(e.Name) ? e.Type.ToString() : $"{e.Name}: {e.Type}");
        return $"({string.Join(", ", parts)})";
    }
}
