namespace Raven.CodeAnalysis.Symbols;

internal class SourceFieldSymbol : SourceSymbol, IFieldSymbol
{
    public SourceFieldSymbol(string name, ITypeSymbol fieldType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Field, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = fieldType;
    }

    public ITypeSymbol Type { get; }
}