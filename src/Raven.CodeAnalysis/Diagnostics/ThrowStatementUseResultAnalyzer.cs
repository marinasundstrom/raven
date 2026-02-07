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
        messageFormat: "Throw statements are discouraged; prefer returning a Result instead",
        category: "ErrorHandling",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(AnalyzeThrowStatement, SyntaxKind.ThrowStatement);

    private static void AnalyzeThrowStatement(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ThrowStatementSyntax throwStatement)
            return;

        var location = Location.Create(
            throwStatement.SyntaxTree,
            throwStatement.EffectiveSpan);

        var diagnostic = Diagnostic.Create(Descriptor, location);
        context.ReportDiagnostic(diagnostic);
    }
}
