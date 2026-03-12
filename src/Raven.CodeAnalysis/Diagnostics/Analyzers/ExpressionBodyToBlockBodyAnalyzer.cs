using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class ExpressionBodyToBlockBodyAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9025";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Expression body can be block body",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Expression body can be converted to block body.",
        category: "Style",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeFunctionStatement, SyntaxKind.FunctionStatement);
        context.RegisterSyntaxNodeAction(
            AnalyzeAccessorDeclaration,
            SyntaxKind.AccessorDeclaration,
            SyntaxKind.GetAccessorDeclaration,
            SyntaxKind.SetAccessorDeclaration,
            SyntaxKind.InitAccessorDeclaration,
            SyntaxKind.AddAccessorDeclaration,
            SyntaxKind.RemoveAccessorDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeConstructorDeclaration, SyntaxKind.ConstructorDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeOperatorDeclaration, SyntaxKind.OperatorDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeConversionOperatorDeclaration, SyntaxKind.ConversionOperatorDeclaration);
    }

    private static void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not MethodDeclarationSyntax method)
            return;

        if (method.ExpressionBody is null || method.Body is not null)
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, method.Identifier.GetLocation()));
    }

    private static void AnalyzeFunctionStatement(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not FunctionStatementSyntax function)
            return;

        if (function.ExpressionBody is null || function.Body is not null)
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, function.Identifier.GetLocation()));
    }

    private static void AnalyzeAccessorDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not AccessorDeclarationSyntax accessor)
            return;

        if (accessor.ExpressionBody is null || accessor.Body is not null)
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, accessor.Keyword.GetLocation()));
    }

    private static void AnalyzeConstructorDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ConstructorDeclarationSyntax constructor)
            return;

        if (constructor.ExpressionBody is null || constructor.Body is not null)
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, constructor.ExpressionBody.GetLocation()));
    }

    private static void AnalyzeOperatorDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not OperatorDeclarationSyntax operatorDeclaration)
            return;

        if (operatorDeclaration.ExpressionBody is null || operatorDeclaration.Body is not null)
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, operatorDeclaration.ExpressionBody.GetLocation()));
    }

    private static void AnalyzeConversionOperatorDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ConversionOperatorDeclarationSyntax conversionOperator)
            return;

        if (conversionOperator.ExpressionBody is null || conversionOperator.Body is not null)
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, conversionOperator.ExpressionBody.GetLocation()));
    }
}
