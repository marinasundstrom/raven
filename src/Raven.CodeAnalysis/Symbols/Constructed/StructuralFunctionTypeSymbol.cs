using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

// Native callable signature. Delegate TypeKind is retained for shared callable binding,
// but this symbol has no nominal declaration, Delegate ancestry or constructor.
internal sealed class StructuralFunctionTypeSymbol : Symbol, INamedTypeSymbol
{
    private readonly ImmutableArray<ISymbol> _members;

    internal StructuralFunctionTypeSymbol(ImmutableArray<ITypeSymbol> parameters,
        ImmutableArray<RefKind> refKinds, ITypeSymbol result, INamedTypeSymbol objectType)
        : base(SymbolKind.Type, string.Empty, null, null, null, [], [], Accessibility.Public, addAsMember: false)
    {
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

    internal ImmutableArray<ITypeSymbol> ParameterTypes { get; }
    internal ImmutableArray<RefKind> ParameterRefKinds { get; }
    internal ITypeSymbol ReturnType { get; }
    internal SourceMethodSymbol InvokeMethod { get; }
    internal StructuralFunctionTypeSymbol Substitute(Func<ITypeSymbol, ITypeSymbol> substitute)
    {
        var parameters = ParameterTypes.Select(substitute).ToImmutableArray();
        var result = substitute(ReturnType);
        return ReferenceEquals(result, ReturnType) && parameters.Where((p, i) => !ReferenceEquals(p, ParameterTypes[i])).Any() == false
            ? this : new(parameters, ParameterRefKinds, result, BaseType);
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
    public INamedTypeSymbol BaseType { get; }
    public ITypeSymbol? OriginalDefinition => null;
    public ITypeSymbol? ConstructedFrom => null;
    public INamedTypeSymbol? UnderlyingTupleType => null;
    public ImmutableArray<IFieldSymbol> TupleElements => [];
    public int Arity => 0;
    public bool IsAbstract => false;
    public bool IsClosed => true;
    public bool IsGenericType => false;
    public bool IsUnboundGenericType => false;
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
}
