namespace Raven.CodeAnalysis.Symbols;

internal partial class ErrorSymbol : SourceSymbol, IErrorSymbol
{
    private readonly Compilation _compilation;

    public ErrorSymbol(Compilation compilation, string name, ISymbol containingSymbol, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Error, name, containingSymbol, null, null, locations, declaringSyntaxReferences)
    {
        _compilation = compilation;
    }
}
