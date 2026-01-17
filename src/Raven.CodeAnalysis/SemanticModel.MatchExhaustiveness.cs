using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public MatchExhaustivenessInfo GetMatchExhaustiveness(
        MatchExpressionSyntax matchExpression,
        MatchExhaustivenessOptions options = default)
    {
        EnsureDiagnosticsCollected();

        var boundMatch = GetBoundNode(matchExpression) as BoundMatchExpression;
        if (boundMatch is null)
            return new MatchExhaustivenessInfo(isExhaustive: true, ImmutableArray<string>.Empty, hasCatchAll: false);

        var evaluator = new MatchExhaustivenessEvaluator(Compilation);
        return evaluator.Evaluate(boundMatch, options);
    }
}
