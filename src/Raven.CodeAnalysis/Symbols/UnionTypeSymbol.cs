using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class UnionTypeSymbol : SourceSymbol, IUnionTypeSymbol
{
    public UnionTypeSymbol(IEnumerable<ITypeSymbol> types, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(SymbolKind.Type, string.Empty, containingSymbol, containingType, containingNamespace, locations, [])
    {
        Types = types;
    }

    public override string Name => string.Join(" | ", Types.Select(x => x.ToDisplayStringKeywordAware(SymbolDisplayFormat.FullyQualifiedFormat)));

    public IEnumerable<ITypeSymbol> Types { get; }

    public SpecialType SpecialType => SpecialType.None;

    public bool IsValueType => false;

    public bool IsNamespace => false;

    public bool IsType => true;

    public INamedTypeSymbol? BaseType { get; }

    public bool IsArray => false;

    public bool IsUnion => true;

    public ImmutableArray<ISymbol> GetMembers()
    {
        return BaseType!.GetMembers();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return BaseType!.GetMembers(name);
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
        throw new NotSupportedException();
    }
}