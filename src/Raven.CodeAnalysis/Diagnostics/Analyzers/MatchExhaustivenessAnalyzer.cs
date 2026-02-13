using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports match expressions that are one case away from being exhaustive.
/// </summary>
public sealed class MatchExhaustivenessAnalyzer : DiagnosticAnalyzer
{
    public const string MissingCaseDiagnosticId = "RAV9002";
    public const string DiscardCaseDiagnosticId = "RAV9005";

    private static readonly DiagnosticDescriptor MissingCaseDescriptor = DiagnosticDescriptor.Create(
        id: MissingCaseDiagnosticId,
        title: "Match expression can be exhaustive",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Match is missing a case for '{0}'.",
        category: "Match",
        defaultSeverity: DiagnosticSeverity.Info);

    private static readonly DiagnosticDescriptor DiscardCaseDescriptor = DiagnosticDescriptor.Create(
        id: DiscardCaseDiagnosticId,
        title: "Discard pattern can be more specific",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Replace '_' with '{0}' to cover the remaining case.",
        category: "Match",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxTreeAction(AnalyzeTree);
    }

    private static void AnalyzeTree(SyntaxTreeAnalysisContext context)
    {
        var semanticModel = context.Compilation.GetSemanticModel(context.SyntaxTree);
        var root = context.SyntaxTree.GetRoot();

        foreach (var matchExpression in root.DescendantNodes().OfType<MatchExpressionSyntax>())
        {
            var info = semanticModel.GetMatchExhaustiveness(matchExpression);

            ReportMissingCaseSuggestion(context, matchExpression, info);

            if (!info.HasCatchAll || !info.IsExhaustive)
                continue;

            var ignoreInfo = semanticModel.GetMatchExhaustiveness(
                matchExpression,
                new MatchExhaustivenessOptions(ignoreCatchAllPatterns: true));

            if (!TryGetSingleMissingCase(ignoreInfo.MissingCases, out var missingCase))
                continue;

            if (missingCase == "_")
                continue;

            var discardLocation = FindDiscardCatchAllLocation(matchExpression, semanticModel);
            if (discardLocation is null)
                continue;

            var discardDiagnostic = Diagnostic.Create(DiscardCaseDescriptor, discardLocation, missingCase);
            context.ReportDiagnostic(discardDiagnostic);
        }
    }

    private static void ReportMissingCaseSuggestion(
        SyntaxTreeAnalysisContext context,
        MatchExpressionSyntax matchExpression,
        MatchExhaustivenessInfo info)
    {
        if (info.IsExhaustive)
            return;

        if (!TryGetSingleMissingCase(info.MissingCases, out var missingCase))
            return;

        if (missingCase == "_")
            return;

        var diagnostic = Diagnostic.Create(MissingCaseDescriptor, matchExpression.MatchKeyword.GetLocation(), missingCase);
        context.ReportDiagnostic(diagnostic);
    }

    private static bool TryGetSingleMissingCase(ImmutableArray<string> missingCases, out string missingCase)
    {
        missingCase = string.Empty;

        if (missingCases.Length != 1)
            return false;

        missingCase = missingCases[0];
        return true;
    }

    private static Location? FindDiscardCatchAllLocation(MatchExpressionSyntax matchExpression, SemanticModel semanticModel)
    {
        if (semanticModel.GetBoundNode(matchExpression) is not BoundMatchExpression boundMatch)
            return null;

        if (boundMatch.Expression.Type is not ITypeSymbol scrutineeType)
            return null;

        scrutineeType = UnwrapAlias(scrutineeType);

        for (var i = 0; i < boundMatch.Arms.Length && i < matchExpression.Arms.Count; i++)
        {
            var arm = boundMatch.Arms[i];

            if (arm.Guard is not null)
                continue;

            if (!MatchExhaustivenessEvaluator.IsDiscardPattern(arm.Pattern))
                continue;

            if (!MatchExhaustivenessEvaluator.IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;

            return matchExpression.Arms[i].Pattern.GetLocation();
        }

        return null;
    }

    private static ITypeSymbol UnwrapAlias(ITypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
            type = alias;

        return type;
    }
}
