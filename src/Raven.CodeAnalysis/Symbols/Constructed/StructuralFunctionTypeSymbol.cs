using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

// Native callable signature. Delegate TypeKind is retained for shared callable binding,
// but this symbol has no nominal declaration, Delegate ancestry or constructor.
internal sealed class StructuralFunctionTypeSymbol : SymbolBase, NamedTypeSymbol, IStructuralFunctionIdentity
{
    private readonly ImmutableArray<Symbol> _members;
    private readonly Compilation? _compilation;

    internal StructuralFunctionTypeSymbol(ImmutableArray<TypeSymbol> parameters,
        ImmutableArray<RefKind> refKinds, TypeSymbol result, NamedTypeSymbol objectType, Compilation? compilation = null)
        : base(SymbolKind.Type, string.Empty, null, null, null, [], [], Accessibility.Public, addAsMember: false)
    {
        _compilation = compilation;
        BaseType = objectType;
        ParameterTypes = parameters;
        ParameterRefKinds = refKinds;
        ReturnType = result;
        InvokeMethod = new SourceMethodSymbol("Invoke", result, [], this, this, null, [], [],
            isStatic: false, declaredAccessibility: Accessibility.Public);
        InvokeMethod.SetParameters(parameters.Select((type, index) => new SourceParameterSymbol(
            $"arg{index}", type, InvokeMethod, this, null, [], [], refKinds[index])));
        _members = [InvokeMethod];
    }

    internal ImmutableArray<TypeSymbol> ParameterTypes { get; }
    internal ImmutableArray<RefKind> ParameterRefKinds { get; }
    internal TypeSymbol ReturnType { get; }
    ImmutableArray<TypeSymbol> IStructuralFunctionIdentity.ParameterTypes => ParameterTypes;
    ImmutableArray<RefKind> IStructuralFunctionIdentity.ParameterRefKinds => ParameterRefKinds;
    TypeSymbol IStructuralFunctionIdentity.ReturnType => ReturnType;

    internal SourceMethodSymbol InvokeMethod { get; }
    internal StructuralFunctionTypeSymbol Substitute(Func<TypeSymbol, TypeSymbol> substitute)
    {
        var parameters = ParameterTypes.Select(substitute).ToImmutableArray();
        var result = substitute(ReturnType);
        return ReferenceEquals(result, ReturnType) && parameters.Where((p, i) => !ReferenceEquals(p, ParameterTypes[i])).Any() == false
            ? this : new(parameters, ParameterRefKinds, result, BaseType, _compilation);
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
    public override string Name => "Function";
    public override string MetadataName => string.Empty;
    public override bool IsImplicitlyDeclared => true;
    public SpecialType SpecialType => SpecialType.None;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsValueType => false;
    public TypeKind TypeKind => TypeKind.Delegate;
    public NamedTypeSymbol BaseType { get; }
    public TypeSymbol? OriginalDefinition => null;
    public TypeSymbol? ConstructedFrom => null;
    public NamedTypeSymbol? UnderlyingTupleType => null;
    public ImmutableArray<FieldSymbol> TupleElements => [];
    public int Arity => 0;
    public bool IsAbstract => false;
    public bool IsClosed => true;
    public bool IsGenericType => false;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<TypeSymbol> TypeArguments => [];
    public ImmutableArray<TypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<MethodSymbol> Constructors => [];
    public ImmutableArray<MethodSymbol> InstanceConstructors => [];
    public MethodSymbol? StaticConstructor => null;
    public ImmutableArray<NamedTypeSymbol> Interfaces => [];
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<Symbol> GetMembers() => _members.AddRange(_compilation?.GetFunctionContractMembers(this) ?? []);
    public ImmutableArray<Symbol> GetMembers(string name) => GetMembers().Where(member => member.Name == name).ToImmutableArray();
    public TypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = GetMembers().FirstOrDefault(member => member.Name == name);
        return symbol is not null;
    }
    public TypeSymbol Construct(params TypeSymbol[] typeArguments) => this;
}
