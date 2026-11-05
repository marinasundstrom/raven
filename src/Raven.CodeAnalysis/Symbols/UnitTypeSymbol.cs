using System.Linq;

using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class UnitTypeSymbol : SourceSymbol, NamedTypeSymbol, IUnitTypeIdentity
{
    private readonly Compilation _compilation;

    public UnitTypeSymbol(Compilation compilation, NamespaceSymbol containingNamespace, NamedTypeSymbol? runtimeRepresentation = null)
        : base(SymbolKind.Type, compilation.Options.RuntimeUnitContract?.TypeName.Split('.').Last() ?? "Unit", compilation.Assembly, null, containingNamespace, [], [], addAsMember: compilation.Options.RuntimeUnitContract is null)
    {
        _compilation = compilation;
        _runtimeRepresentation = runtimeRepresentation;
        TypeKind = TypeKind.Unit;
    }

    private readonly NamedTypeSymbol? _runtimeRepresentation;
    internal NamedTypeSymbol? RuntimeRepresentation => _runtimeRepresentation ?? _compilation.ResolveRuntimeUnitType();

    NamedTypeSymbol? IUnitTypeIdentity.RuntimeRepresentation => RuntimeRepresentation;

    public override string Name => _compilation.Options.RuntimeUnitContract?.TypeName.Split('.').Last() ?? "Unit";

    public NamedTypeSymbol? BaseType => _compilation.GetSpecialType(SpecialType.System_ValueType);

    public SpecialType SpecialType => SpecialType.System_Unit;

    public bool IsNamespace => false;

    public bool IsType => true;

    public TypeKind TypeKind { get; }

    public bool IsValueType => true;

    public TypeSymbol? OriginalDefinition => this;

    public ImmutableArray<Symbol> GetMembers() => [];

    public ImmutableArray<Symbol> GetMembers(string name) => [];

    public TypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = null;
        return false;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);

    public int Arity => 0;

    public ImmutableArray<MethodSymbol> Constructors => [];

    public ImmutableArray<MethodSymbol> InstanceConstructors => [];

    public MethodSymbol? StaticConstructor => null;

    public NamedTypeSymbol? UnderlyingTupleType => null;

    public ImmutableArray<FieldSymbol> TupleElements => [];

    public ImmutableArray<TypeSymbol> TypeArguments => [];

    public ImmutableArray<TypeParameterSymbol> TypeParameters => [];

    public TypeSymbol? ConstructedFrom => this;

    public bool IsAbstract => false;

    public bool IsClosed => true;

    public bool IsGenericType => false;

    public bool IsUnboundGenericType => false;

    public TypeSymbol Construct(params TypeSymbol[] typeArguments) => this;

    public ImmutableArray<NamedTypeSymbol> Interfaces => [];
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => [];
}
