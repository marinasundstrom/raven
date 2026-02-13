using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class ThrowStatementUseResultAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9013";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Prefer Result over throw",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Use Result for expected failures instead of 'throw'.",
        category: "ErrorHandling",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(
            AnalyzeThrow,
            SyntaxKind.ThrowStatement,
            SyntaxKind.ThrowExpression);

    private static void AnalyzeThrow(SyntaxNodeAnalysisContext context)
    {
        var location = Location.Create(context.Node.SyntaxTree, context.Node.EffectiveSpan);

        var diagnostic = Diagnostic.Create(Descriptor, location);
        context.ReportDiagnostic(diagnostic);
    }
}
