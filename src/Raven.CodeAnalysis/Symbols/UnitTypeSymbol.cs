using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class UnitTypeSymbol : SourceSymbol, INamedTypeSymbol
{
    private readonly Compilation _compilation;

    public UnitTypeSymbol(Compilation compilation)
        : base(SymbolKind.Type, "Unit", compilation.Assembly, null, compilation.Assembly.GlobalNamespace, [], [])
    {
        _compilation = compilation;
        TypeKind = TypeKind.Unit;
    }

    public override string Name => "Unit";

    public INamedTypeSymbol? BaseType => _compilation.GetSpecialType(SpecialType.System_ValueType);

    public SpecialType SpecialType => SpecialType.System_Unit;

    public bool IsNamespace => false;

    public bool IsType => true;

    public TypeKind TypeKind { get; }

    public bool IsValueType => true;

    public ITypeSymbol? OriginalDefinition => this;

    public ImmutableArray<ISymbol> GetMembers() => [];

    public ImmutableArray<ISymbol> GetMembers(string name) => [];

    public ITypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);

    public int Arity => 0;

    public ImmutableArray<IMethodSymbol> Constructors => [];

    public IMethodSymbol? StaticConstructor => null;

    public INamedTypeSymbol UnderlyingTupleType => this;

    public ImmutableArray<IFieldSymbol> TupleElements => [];

    public ImmutableArray<ITypeSymbol> TypeArguments => [];

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];

    public ITypeSymbol? ConstructedFrom => this;

    public bool IsAbstract => false;

    public bool IsSealed => true;

    public bool IsGenericType => false;

    public bool IsUnboundGenericType => false;

    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => this;
}
