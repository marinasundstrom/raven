
namespace Raven.CodeAnalysis;

static class BoundExpressionExtensions
{
    public static SymbolInfo GetSymbolInfo(this BoundExpression expression)
    {
        if (expression.Reason is BoundExpressionReason.None)
            return new SymbolInfo(expression.Symbol!);

        return new SymbolInfo(Convert(expression.Reason), expression.Symbol is not null ? [expression.Symbol] : []);
    }
    
    public static SymbolInfo GetSymbolInfo(this BoundStatement statement)
    {
        return new SymbolInfo(statement.Symbol);
    }

    private static CandidateReason Convert(BoundExpressionReason candidateReason)
    {
        return candidateReason switch
        {
            BoundExpressionReason.None => CandidateReason.None,
            BoundExpressionReason.NotFound => CandidateReason.NotFound,
            BoundExpressionReason.Ambiguous => CandidateReason.Ambiguous,
            BoundExpressionReason.Inaccessible => CandidateReason.Inaccessible,
            BoundExpressionReason.WrongArity => CandidateReason.WrongArity,
            BoundExpressionReason.OverloadResolutionFailed => CandidateReason.OverloadResolutionFailure,
            _ => throw new ArgumentOutOfRangeException(nameof(candidateReason), candidateReason, null)
        };
    }
}
