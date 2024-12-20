namespace Raven.CodeAnalysis.Symbols;

internal class NamespaceSymbol : SourceSymbol, INamespaceSymbol
{
    public NamespaceSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
    }
}