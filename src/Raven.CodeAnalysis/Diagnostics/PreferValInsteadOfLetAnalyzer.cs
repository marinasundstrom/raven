using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports when the 'let' keyword is used instead of 'val'
/// </summary>
public sealed class PreferValInsteadOfLetAnalyzer : DiagnosticAnalyzer
{
    public const string PreferValInsteadOfLetDiagnosticId = "RAV9009";

    private static readonly DiagnosticDescriptor PreferValInsteadOfLetDescriptor = DiagnosticDescriptor.Create(
        id: PreferValInsteadOfLetDiagnosticId,
        title: "Prefer binding keyword 'val' instead of 'let'",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Prefer 'val' instead of 'let'",
        category: "Match",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeVariableDeclaration, SyntaxKind.VariableDeclaration);
    }

    private static void AnalyzeVariableDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not VariableDeclarationSyntax variableDeclarationSyntax)
            return;

        var bindingKeyword = variableDeclarationSyntax.BindingKeyword;
        if (!bindingKeyword.IsKind(SyntaxKind.LetKeyword))
            return;

        var location = bindingKeyword.GetLocation();
        var diagnostic = Diagnostic.Create(PreferValInsteadOfLetDescriptor, location);
        context.ReportDiagnostic(diagnostic);
    }
}
