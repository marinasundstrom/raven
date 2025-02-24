namespace Raven.CodeAnalysis.Symbols;

internal class SourceLocalSymbol : SourceSymbol, ILocalSymbol
{
    public SourceLocalSymbol(string name, ITypeSymbol type, bool isReadOnly, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Local, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = type;
        IsReadOnly = isReadOnly;
    }

    public ITypeSymbol Type { get; }

    public bool IsReadOnly { get; }
}