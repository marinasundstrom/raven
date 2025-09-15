using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class UnionTypeSymbol : SourceSymbol, IUnionTypeSymbol
{
    public UnionTypeSymbol(IEnumerable<ITypeSymbol> types, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(SymbolKind.Type, string.Empty, containingSymbol, containingType, containingNamespace, locations, [])
    {
        Types = types;

        TypeKind = TypeKind.Union;
    }

    public override string Name => string.Join(" | ", Types.Select(x => x.ToDisplayStringKeywordAware(SymbolDisplayFormat.FullyQualifiedFormat)));

    public IEnumerable<ITypeSymbol> Types { get; }

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public INamedTypeSymbol? BaseType => null;

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public ImmutableArray<ISymbol> GetMembers()
    {
        return ImmutableArray<ISymbol>.Empty;
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return ImmutableArray<ISymbol>.Empty;
    }

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public override string ToString()
    {
        return Name;
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }
}
