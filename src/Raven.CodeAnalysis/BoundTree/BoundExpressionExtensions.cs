
namespace Raven.CodeAnalysis;

static class BoundExpressionExtensions
{
    public static SymbolInfo GetSymbolInfo(this BoundExpression expression)
    {
        if (expression.Reason is BoundExpressionReason.None)
            return new SymbolInfo(expression.Symbol!);

        return new SymbolInfo(Convert(expression.Reason), expression.Symbol is not null ? [expression.Symbol] : []);
    }

    private static CandidateReason Convert(BoundExpressionReason candidateReason)
    {
        return candidateReason switch
        {
            BoundExpressionReason.None => CodeAnalysis.CandidateReason.None,
            BoundExpressionReason.NotFound => CodeAnalysis.CandidateReason.NotFound,
            BoundExpressionReason.Ambiguous => CodeAnalysis.CandidateReason.Ambiguous,
            BoundExpressionReason.Inaccessible => CodeAnalysis.CandidateReason.Inaccessible,
            BoundExpressionReason.WrongArity => CodeAnalysis.CandidateReason.WrongArity,
            BoundExpressionReason.OverloadResolutionFailed => CodeAnalysis.CandidateReason.OverloadResolutionFailure,
            _ => throw new ArgumentOutOfRangeException(nameof(candidateReason), candidateReason, null)
        };
    }
}
