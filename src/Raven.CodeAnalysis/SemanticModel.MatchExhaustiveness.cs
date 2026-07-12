using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public MatchExhaustivenessInfo GetMatchExhaustiveness(
        MatchExpressionSyntax matchExpression,
        MatchExhaustivenessOptions options = default)
        => GetMatchExhaustivenessCore(matchExpression, options);

    public MatchExhaustivenessInfo GetMatchExhaustiveness(
        PostfixMatchExpressionSyntax matchExpression,
        MatchExhaustivenessOptions options = default)
        => GetMatchExhaustivenessCore(matchExpression, options);

    private MatchExhaustivenessInfo GetMatchExhaustivenessCore(
        SyntaxNode matchExpression,
        MatchExhaustivenessOptions options)
    {
        using var semanticAccess = EnterSemanticAccess(CancellationToken.None);

        var boundMatch = GetBoundNode(matchExpression) as BoundMatchExpression;
        if (boundMatch is null)
            return new MatchExhaustivenessInfo(isExhaustive: true, ImmutableArray<string>.Empty, hasCatchAll: false);

        var evaluator = new MatchExhaustivenessEvaluator(Compilation, node => TryGetCachedBoundNode(node) ?? GetBoundNode(node));
        return evaluator.Evaluate(matchExpression, boundMatch, options);
    }
}
