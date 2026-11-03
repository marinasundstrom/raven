using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;


internal partial class ErrorTypeSymbolImplementation : SourceSymbol, ErrorTypeSymbol
{
    private readonly Compilation _compilation;

    public ErrorTypeSymbolImplementation(Compilation compilation, string name, Symbol containingSymbol, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.ErrorType, name, containingSymbol, null, null, locations, declaringSyntaxReferences)
    {
        _compilation = compilation;

        TypeKind = TypeKind.Error;
    }

    public ImmutableArray<MethodSymbol> Constructors => [];

    public ImmutableArray<MethodSymbol> InstanceConstructors => [];

    public MethodSymbol? StaticConstructor => null;

    public ImmutableArray<TypeSymbol> TypeArguments => [];

    public ImmutableArray<TypeParameterSymbol> TypeParameters => [];
    public TypeSymbol? ConstructedFrom => this;
    public bool IsAbstract => false;
    public bool IsClosed => true;
    public bool IsGenericType => false;
    public bool IsUnboundGenericType => false;

    public ImmutableArray<NamedTypeSymbol> Interfaces => [];
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => [];

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public NamedTypeSymbol? BaseType => null;

    public TypeKind TypeKind { get; }

    public TypeSymbol? OriginalDefinition => this;

    public int Arity => 0;

    public NamedTypeSymbol? UnderlyingTupleType => null;
    public ImmutableArray<FieldSymbol> TupleElements => [];

    public ImmutableArray<Symbol> GetMembers() => [];

    public ImmutableArray<Symbol> GetMembers(string name) => [];

    public TypeSymbol? LookupType(string name)
    {
        return null;
    }

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = null;
        return false;
    }

    public TypeSymbol Construct(params TypeSymbol[] typeArguments)
    {
        return this;
    }
}
