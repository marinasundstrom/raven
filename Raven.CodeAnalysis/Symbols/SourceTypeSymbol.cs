namespace Raven.CodeAnalysis.Symbols;

internal class SourceTypeSymbol : SourceSymbol, ITypeSymbol, INamedTypeSymbol
{
    public SourceTypeSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Type, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        
    }

}