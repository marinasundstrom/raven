namespace Raven.CodeAnalysis.Symbols;

internal class SourceParameterSymbol : SourceSymbol, IParameterSymbol
{
    public SourceParameterSymbol(string name, ITypeSymbol parameterType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Parameter, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = parameterType;
    }

    public ITypeSymbol Type { get; }
}