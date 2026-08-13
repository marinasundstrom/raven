using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports unconditional <c>while true</c> statements that can use Raven's dedicated
/// <c>loop</c> statement.
/// </summary>
public sealed class PreferLoopOverWhileTrueAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9036";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Prefer 'loop' over 'while true'",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Prefer 'loop' for an unconditional loop.",
        category: "Style",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(AnalyzeWhileStatement, SyntaxKind.WhileStatement);

    private static void AnalyzeWhileStatement(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not WhileStatementSyntax whileStatement ||
            !IsTrueLiteral(whileStatement.Condition))
        {
            return;
        }

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, whileStatement.WhileKeyword.GetLocation()));
    }

    internal static bool IsTrueLiteral(ExpressionSyntax expression)
    {
        while (expression is ParenthesizedExpressionSyntax parenthesized)
            expression = parenthesized.Expression;

        return expression.Kind == SyntaxKind.TrueLiteralExpression;
    }
}
