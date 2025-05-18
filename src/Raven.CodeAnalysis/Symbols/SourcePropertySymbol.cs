namespace Raven.CodeAnalysis.Symbols;

internal partial class SourcePropertySymbol : SourceSymbol, IPropertySymbol
{
    public SourcePropertySymbol(string name, ITypeSymbol propertyType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Property, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = propertyType;
    }

    public ITypeSymbol Type { get; }
    public IMethodSymbol? GetMethod { get; }
    public IMethodSymbol? SetMethod { get; }
    public bool IsIndexer => false;
}