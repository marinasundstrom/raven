using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

// A native signature shape, with no nominal tuple definition or generic carrier.
internal sealed class StructuralTupleTypeSymbol : Symbol, ITupleTypeSymbol
{
    private readonly ImmutableArray<ISymbol> _members;

    internal StructuralTupleTypeSymbol(IEnumerable<(string? name, ITypeSymbol type)> elements)
        : base(SymbolKind.Type, string.Empty, null, null, null, [], [], Accessibility.NotApplicable, addAsMember: false)
    {
        TupleElements = elements.Select((element, index) => (IFieldSymbol)new ElementField(
            element.name ?? $"Item{index + 1}", $"Item{index + 1}", element.type, this)).ToImmutableArray();
        _members = TupleElements.SelectMany(field => field.Name == field.MetadataName
            ? new ISymbol[] { field }
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
    public INamedTypeSymbol? BaseType => null;
    public ITypeSymbol? OriginalDefinition => null;
    public ITypeSymbol? ConstructedFrom => null;
    public INamedTypeSymbol? UnderlyingTupleType => null;
    public int Arity => 0;
    public bool IsAbstract => false;
    public bool IsClosed => true;
    public bool IsGenericType => false;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<IFieldSymbol> TupleElements { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments => [];
    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<IMethodSymbol> Constructors => [];
    public ImmutableArray<IMethodSymbol> InstanceConstructors => [];
    public IMethodSymbol? StaticConstructor => null;
    public ImmutableArray<INamedTypeSymbol> Interfaces => [];
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<ISymbol> GetMembers() => _members;
    public ImmutableArray<ISymbol> GetMembers(string name) => _members.Where(member => member.Name == name).ToImmutableArray();
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = _members.FirstOrDefault(member => member.Name == name);
        return symbol is not null;
    }
    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => this;
    public override string ToString() => $"({string.Join(", ", TupleElements.Select(field => $"{field.Name}: {field.Type}"))})";

    private sealed class ElementField(string name, string metadataName, ITypeSymbol type, StructuralTupleTypeSymbol owner)
        : Symbol(SymbolKind.Field, name, owner, owner, null, [], [], Accessibility.Public, addAsMember: false), IFieldSymbol
    {
        public override string MetadataName => metadataName;
        public override bool IsImplicitlyDeclared => true;
        public ITypeSymbol Type => type;
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
