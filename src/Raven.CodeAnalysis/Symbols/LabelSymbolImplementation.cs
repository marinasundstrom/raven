namespace Raven.CodeAnalysis.Symbols;

internal partial class LabelSymbolImplementation : SymbolBase, LabelSymbol
{
    public LabelSymbolImplementation(
        string name,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Label, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
    }

    public override string MetadataName => Name;

    public override bool CanBeReferencedByName => true;
}
