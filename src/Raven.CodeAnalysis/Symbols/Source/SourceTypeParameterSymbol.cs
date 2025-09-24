using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceTypeParameterSymbol : Symbol, ITypeParameterSymbol
{
    public SourceTypeParameterSymbol(
        string name,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        int ordinal)
        : base(SymbolKind.TypeParameter, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Ordinal = ordinal;
    }

    public int Ordinal { get; }

    public override string MetadataName => Name;

    public SpecialType SpecialType => SpecialType.None;

    public TypeKind TypeKind => TypeKind.TypeParameter;

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsReferenceType => false;

    public bool IsValueType => false;

    public INamedTypeSymbol? BaseType => null;

    public ITypeSymbol? OriginalDefinition => this;

    public ImmutableArray<ISymbol> GetMembers() => ImmutableArray<ISymbol>.Empty;

    public ImmutableArray<ISymbol> GetMembers(string name) => ImmutableArray<ISymbol>.Empty;

    public ITypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }

    public override void Accept(SymbolVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}
