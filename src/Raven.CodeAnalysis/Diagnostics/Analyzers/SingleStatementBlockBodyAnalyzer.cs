using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class SingleStatementBlockBodyAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9024";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Block body can be expression body",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Single-statement block body can be converted to expression body.",
        category: "Style",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeFunctionStatement, SyntaxKind.FunctionStatement);
        context.RegisterSyntaxNodeAction(AnalyzeAccessorDeclaration, SyntaxKind.AccessorDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeConstructorDeclaration, SyntaxKind.ConstructorDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeOperatorDeclaration, SyntaxKind.OperatorDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeConversionOperatorDeclaration, SyntaxKind.ConversionOperatorDeclaration);
    }

    private static void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not MethodDeclarationSyntax method)
            return;

        if (method.ExpressionBody is not null || method.Body is null)
            return;

        if (!TryGetConvertibleExpression(method.Body, out _))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, method.Body.GetLocation()));
    }

    private static void AnalyzeFunctionStatement(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not FunctionStatementSyntax function)
            return;

        if (function.ExpressionBody is not null || function.Body is null)
            return;

        if (!TryGetConvertibleExpression(function.Body, out _))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, function.Body.GetLocation()));
    }

    private static void AnalyzeAccessorDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not AccessorDeclarationSyntax accessor)
            return;

        if (accessor.ExpressionBody is not null || accessor.Body is null)
            return;

        if (!TryGetConvertibleExpression(accessor.Body, out _))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, accessor.Body.GetLocation()));
    }

    private static void AnalyzeConstructorDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ConstructorDeclarationSyntax constructor)
            return;

        if (constructor.ExpressionBody is not null || constructor.Body is null)
            return;

        if (!TryGetConvertibleExpression(constructor.Body, out _))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, constructor.Body.GetLocation()));
    }

    private static void AnalyzeOperatorDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not OperatorDeclarationSyntax operatorDeclaration)
            return;

        if (operatorDeclaration.ExpressionBody is not null || operatorDeclaration.Body is null)
            return;

        if (!TryGetConvertibleExpression(operatorDeclaration.Body, out _))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, operatorDeclaration.Body.GetLocation()));
    }

    private static void AnalyzeConversionOperatorDeclaration(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not ConversionOperatorDeclarationSyntax conversionOperator)
            return;

        if (conversionOperator.ExpressionBody is not null || conversionOperator.Body is null)
            return;

        if (!TryGetConvertibleExpression(conversionOperator.Body, out _))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, conversionOperator.Body.GetLocation()));
    }

    internal static bool TryGetConvertibleExpression(BlockStatementSyntax body, out ExpressionSyntax expression)
    {
        expression = null!;

        if (body.Statements.Count != 1)
            return false;

        var statement = body.Statements[0];
        switch (statement)
        {
            case ReturnStatementSyntax { Expression: { } returnExpression }:
                expression = returnExpression;
                return true;
            case ExpressionStatementSyntax { Expression: { } expressionStatement }:
                expression = expressionStatement;
                return true;
            default:
                return false;
        }
    }
}
