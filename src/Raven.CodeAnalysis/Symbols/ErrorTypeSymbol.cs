using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;


internal partial class ErrorTypeSymbol : SourceSymbol, IErrorTypeSymbol
{
    private readonly Compilation _compilation;

    public ErrorTypeSymbol(Compilation compilation, string name, ISymbol containingSymbol, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.ErrorType, name, containingSymbol, null, null, locations, declaringSyntaxReferences)
    {
        _compilation = compilation;

        TypeKind = TypeKind.Error;
    }

    public ImmutableArray<IMethodSymbol> Constructors => [];

    public IMethodSymbol? StaticConstructor => null;

    public ImmutableArray<ITypeSymbol> TypeArguments => [];

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];
    public ITypeSymbol? ConstructedFrom { get; }
    public bool IsAbstract { get; }
    public bool IsSealed { get; }
    public bool IsGenericType { get; }
    public bool IsUnboundGenericType { get; }

    public ImmutableArray<INamedTypeSymbol> Interfaces => [];
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => [];

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public INamedTypeSymbol? BaseType => _compilation.GetSpecialType(SpecialType.System_Object);

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public int Arity => 0;

    public INamedTypeSymbol UnderlyingTupleType => this;

    public ImmutableArray<IFieldSymbol> TupleElements => [];

    public ImmutableArray<ISymbol> GetMembers() => [];

    public ImmutableArray<ISymbol> GetMembers(string name) => [];

    public ITypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }

    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => this;
}
