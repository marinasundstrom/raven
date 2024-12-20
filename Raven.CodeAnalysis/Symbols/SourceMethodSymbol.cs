namespace Raven.CodeAnalysis.Symbols;

internal class SourceMethodSymbol : SourceSymbol, IMethodSymbol
{
    public SourceMethodSymbol(string name, ITypeSymbol returnType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        ReturnType = returnType;
    }

    public ITypeSymbol ReturnType { get; }
}