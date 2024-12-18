namespace Raven.CodeAnalysis.Symbols;

internal class SourceNamespaceSymbol : SourceSymbol, INamespaceSymbol
{
    public SourceNamespaceSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
    }
}
