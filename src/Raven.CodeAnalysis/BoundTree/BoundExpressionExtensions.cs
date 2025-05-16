
namespace Raven.CodeAnalysis;

static class BoundExpressionExtensions
{
    public static SymbolInfo GetSymbolInfo(this BoundExpression expression)
    {
        if (expression.Reason is BoundExpressionReason.None)
            return new SymbolInfo(expression.Symbol!);

        return new SymbolInfo(Convert(expression.Reason), expression.Symbol is not null ? [expression.Symbol] : []);
    }

    private static MapToCandidateReason Convert(BoundExpressionReason candidateReason)
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
