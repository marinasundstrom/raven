namespace Raven.CodeAnalysis.Symbols;

internal partial class ErrorSymbolImplementation : SourceSymbol, ErrorSymbol
{
    private readonly Compilation _compilation;

    public ErrorSymbolImplementation(Compilation compilation, string name, Symbol containingSymbol, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Error, name, containingSymbol, null, null, locations, declaringSyntaxReferences)
    {
        _compilation = compilation;
    }
}
