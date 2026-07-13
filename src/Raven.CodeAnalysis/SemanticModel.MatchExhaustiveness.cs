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

    public MatchExhaustivenessInfo GetMatchExhaustiveness(
        MatchStatementSyntax matchStatement,
        MatchExhaustivenessOptions options = default)
        => GetMatchExhaustivenessCore(matchStatement, options);

    private MatchExhaustivenessInfo GetMatchExhaustivenessCore(
        SyntaxNode matchSyntax,
        MatchExhaustivenessOptions options)
    {
        using var semanticAccess = EnterSemanticAccess(CancellationToken.None);

        var evaluator = new MatchExhaustivenessEvaluator(Compilation, node => TryGetCachedBoundNode(node) ?? GetBoundNode(node));
        return GetBoundNode(matchSyntax) switch
        {
            BoundMatchExpression boundMatch => evaluator.Evaluate(matchSyntax, boundMatch, options),
            BoundMatchStatement boundMatch => evaluator.Evaluate(matchSyntax, boundMatch, options),
            _ => new MatchExhaustivenessInfo(isExhaustive: true, ImmutableArray<string>.Empty, hasCatchAll: false),
        };
    }
}
