using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

// A native signature shape, with no nominal tuple definition or generic carrier.
internal sealed class StructuralTupleTypeSymbol : SymbolBase, TupleTypeSymbol
{
    private readonly ImmutableArray<Symbol> _members;

    internal StructuralTupleTypeSymbol(IEnumerable<(string? name, TypeSymbol type)> elements)
        : base(SymbolKind.Type, string.Empty, null, null, null, [], [], Accessibility.NotApplicable, addAsMember: false)
    {
        TupleElements = elements.Select((element, index) => (FieldSymbol)new ElementField(
            element.name ?? $"Item{index + 1}", $"Item{index + 1}", element.type, this)).ToImmutableArray();
        _members = TupleElements.SelectMany(field => field.Name == field.MetadataName
            ? new Symbol[] { field }
            : [field, new ElementField(field.MetadataName, field.MetadataName, field.Type, this)]).ToImmutableArray();
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
    public override string Name => "Tuple";
    public override string MetadataName => string.Empty;
    public override bool IsImplicitlyDeclared => true;
    public SpecialType SpecialType => SpecialType.None;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsValueType => true;
    public TypeKind TypeKind => TypeKind.Tuple;
    public NamedTypeSymbol? BaseType => null;
    public TypeSymbol? OriginalDefinition => null;
    public TypeSymbol? ConstructedFrom => null;
    public NamedTypeSymbol? UnderlyingTupleType => null;
    public int Arity => 0;
    public bool IsAbstract => false;
    public bool IsClosed => true;
    public bool IsGenericType => false;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<FieldSymbol> TupleElements { get; }
    public ImmutableArray<TypeSymbol> TypeArguments => [];
    public ImmutableArray<TypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<MethodSymbol> Constructors => [];
    public ImmutableArray<MethodSymbol> InstanceConstructors => [];
    public MethodSymbol? StaticConstructor => null;
    public ImmutableArray<NamedTypeSymbol> Interfaces => [];
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<Symbol> GetMembers() => _members;
    public ImmutableArray<Symbol> GetMembers(string name) => _members.Where(member => member.Name == name).ToImmutableArray();
    public TypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = _members.FirstOrDefault(member => member.Name == name);
        return symbol is not null;
    }
    public TypeSymbol Construct(params TypeSymbol[] typeArguments) => this;
    public override string ToString() => $"({string.Join(", ", TupleElements.Select(field => $"{field.Name}: {field.Type}"))})";

    private sealed class ElementField(string name, string metadataName, TypeSymbol type, StructuralTupleTypeSymbol owner)
        : SymbolBase(SymbolKind.Field, name, owner, owner, null, [], [], Accessibility.Public, addAsMember: false), FieldSymbol
    {
        public override string MetadataName => metadataName;
        public override bool IsImplicitlyDeclared => true;
        public TypeSymbol Type => type;
        public RefKind RefKind => RefKind.None;
        public bool IsConst => false;
        public bool IsRequired => false;
        public bool IsReadOnly => false;
        public override bool IsStatic => false;
        public object? GetConstantValue() => null;
        public override void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
        public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
    }
}
