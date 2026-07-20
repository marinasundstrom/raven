using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports when the 'val' keyword is used for an immutable lexical binding instead of 'let'.
/// </summary>
public sealed class PreferLetInsteadOfValAnalyzer : DiagnosticAnalyzer
{
    public const string PreferLetInsteadOfValDiagnosticId = "RAV9035";

    private static readonly DiagnosticDescriptor PreferLetInsteadOfValDescriptor = DiagnosticDescriptor.Create(
        id: PreferLetInsteadOfValDiagnosticId,
        title: "Prefer binding keyword 'let' instead of 'val'",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Prefer 'let' for an immutable lexical binding.",
        category: "Style",
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
        if (!bindingKeyword.IsKind(SyntaxKind.ValKeyword))
            return;

        var diagnostic = Diagnostic.Create(PreferLetInsteadOfValDescriptor, bindingKeyword.GetLocation());
        context.ReportDiagnostic(diagnostic);
    }
}
