
namespace Raven.CodeAnalysis;

abstract class BoundExpression : BoundNode
{
    public ITypeSymbol Type { get; }
    public ISymbol? Symbol { get; }
    public BoundExpressionReason Reason { get; }

    protected BoundExpression(ITypeSymbol type, ISymbol? symbol = null, BoundExpressionReason reason = BoundExpressionReason.None)
    {
        Type = type;
        Symbol = symbol;
        Reason = reason;
    }

    public SymbolInfo GetSymbolInfo()
    {
        if (Reason is BoundExpressionReason.None)
            return new SymbolInfo(Symbol!);

        return new SymbolInfo(Convert(Reason), Symbol is not null ? [Symbol] : []);
    }

    private MapToCandidateReason Convert(BoundExpressionReason candidateReason)
    {
        return candidateReason switch
        {
            BoundExpressionReason.None => CodeAnalysis.MapToCandidateReason.None,
            BoundExpressionReason.NotFound => CodeAnalysis.MapToCandidateReason.NotFound,
            BoundExpressionReason.Ambiguous => CodeAnalysis.MapToCandidateReason.Ambiguous,
            BoundExpressionReason.Inaccessible => CodeAnalysis.MapToCandidateReason.Inaccessible,
            BoundExpressionReason.WrongArity => CodeAnalysis.MapToCandidateReason.WrongArity,
            BoundExpressionReason.OverloadResolutionFailed => CodeAnalysis.MapToCandidateReason.OverloadResolutionFailure,
            _ => throw new ArgumentOutOfRangeException(nameof(candidateReason), candidateReason, null)
        };
    }
}
