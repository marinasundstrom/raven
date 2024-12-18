namespace Raven.CodeAnalysis.Symbols;

internal class SourceLocalSymbol : SourceSymbol, ILocalSymbol
{
    public SourceLocalSymbol(string name, ITypeSymbol type, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Local, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = type;
    }

    public ITypeSymbol Type { get; }
}