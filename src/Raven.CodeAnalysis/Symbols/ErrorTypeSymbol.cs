using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;


internal partial class ErrorTypeSymbol : SourceSymbol, IErrorTypeSymbol
{
    private readonly Compilation _compilation;

    public ErrorTypeSymbol(Compilation compilation, string name, ISymbol containingSymbol, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.ErrorType, name, containingSymbol, null, null, locations, declaringSyntaxReferences)
    {
        _compilation = compilation;
    }

    public override Compilation Compilation => _compilation;

    public ImmutableArray<IMethodSymbol> Constructors => [];

    public IMethodSymbol? StaticConstructor => null;

    public ImmutableArray<ITypeSymbol> TypeArguments => [];

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];

    public SpecialType SpecialType => SpecialType.None;

    public bool IsValueType => false;

    public bool IsNamespace => false;

    public bool IsType => false;

    public INamedTypeSymbol? BaseType => throw new NotImplementedException();

    public bool IsArray => false;

    public ImmutableArray<ISymbol> GetMembers() => [];

    public ImmutableArray<ISymbol> GetMembers(string name) => [];

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        throw new NotSupportedException();
    }
}
