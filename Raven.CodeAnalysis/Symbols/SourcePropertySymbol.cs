namespace Raven.CodeAnalysis.Symbols;

internal class SourcePropertySymbol : SourceSymbol, IPropertySymbol
{
    public SourcePropertySymbol(string name, ITypeSymbol propertyType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Property, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        PropertyType = propertyType;
    }

    public ITypeSymbol PropertyType { get; }
}