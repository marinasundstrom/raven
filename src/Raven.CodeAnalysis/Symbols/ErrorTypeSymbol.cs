using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;


internal partial class ErrorTypeSymbol : SourceSymbol, IErrorTypeSymbol
{
    public static ITypeSymbol Default { get; } = new ErrorTypeSymbol("Error", null, [], []);

    public ErrorTypeSymbol(string name, ISymbol containingSymbol, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.ErrorType, name, containingSymbol, null, null, locations, declaringSyntaxReferences)
    {
    }


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
