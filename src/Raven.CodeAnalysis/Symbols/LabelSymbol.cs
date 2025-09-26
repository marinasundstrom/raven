namespace Raven.CodeAnalysis.Symbols;

internal partial class LabelSymbol : Symbol, ILabelSymbol
{
    public LabelSymbol(
        string name,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Label, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
    }

    public override string MetadataName => Name;

    public override bool CanBeReferencedByName => true;
}
