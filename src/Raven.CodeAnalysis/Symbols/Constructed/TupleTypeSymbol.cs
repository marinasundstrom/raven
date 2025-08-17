using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class TupleTypeSymbol : PESymbol, ITupleTypeSymbol
{
    private ImmutableArray<IFieldSymbol> _tupleElements;

    public TupleTypeSymbol(
        INamedTypeSymbol underlyingTupleType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        UnderlyingTupleType = underlyingTupleType;
        BaseType = underlyingTupleType.BaseType;

        TypeKind = TypeKind.Tuple;
    }

    public void SetTupleElements(IEnumerable<IFieldSymbol> fieldSymbols)
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

    public INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public int Arity { get; } = 0;

    public ImmutableArray<IMethodSymbol> Constructors { get; } = [];

    public IMethodSymbol? StaticConstructor { get; } = null;

    public INamedTypeSymbol UnderlyingTupleType { get; }

    public ImmutableArray<IFieldSymbol> TupleElements => _tupleElements;

    public ImmutableArray<ITypeSymbol> TypeArguments =>
        TupleElements.Select(e => e.Type).ToImmutableArray();

    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; } = [];

    public ITypeSymbol? ConstructedFrom { get; } = null;

    public bool IsAbstract => false;

    public bool IsGenericType => false;

    public bool IsUnboundGenericType => false;

    public ImmutableArray<ISymbol> GetMembers()
    {
        IEnumerable<ISymbol> symbols = UnderlyingTupleType.GetMembers();
        return symbols.Concat(TupleElements.CastArray<ISymbol>()).ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        IEnumerable<ISymbol> symbols = UnderlyingTupleType.GetMembers(name);
        return symbols.Concat(TupleElements.Where(f => f.Name == name).Cast<ISymbol>()).ToImmutableArray();
    }

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        throw new NotSupportedException();
    }

    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        throw new NotImplementedException();
    }

    public override string ToString()
    {
        var parts = TupleElements
            .Select(e => string.IsNullOrEmpty(e.Name) ? e.Type.ToString() : $"{e.Name}: {e.Type}");
        return $"({string.Join(", ", parts)})";
    }
}
