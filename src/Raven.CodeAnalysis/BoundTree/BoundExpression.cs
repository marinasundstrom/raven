namespace Raven.CodeAnalysis;

abstract class BoundExpression : BoundNode
{
    public ITypeSymbol Type { get; }
    public ISymbol? Symbol { get; }
    public CandidateReason CandidateReason { get; } // new field

    protected BoundExpression(ITypeSymbol type, ISymbol? symbol = null, CandidateReason reason = CandidateReason.None)
    {
        Type = type;
        Symbol = symbol;
        CandidateReason = reason;
    }

    public SymbolInfo GetSymbolInfo()
    {
        if (CandidateReason is CandidateReason.None)
            return new SymbolInfo(Symbol!);

        return new SymbolInfo(CandidateReason, Symbol is not null ? [Symbol] : []);
    }
}