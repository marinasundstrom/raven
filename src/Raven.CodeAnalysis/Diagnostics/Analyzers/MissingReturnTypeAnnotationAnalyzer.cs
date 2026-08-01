using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports methods without explicit return type annotations and suggests the inferred type.
/// </summary>
/// <remarks>
/// Making return types explicit helps teams balance the expressiveness of functional-style
/// inference with the clarity and intent often emphasized in object-oriented code.
/// </remarks>
public sealed class MissingReturnTypeAnnotationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9001";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Return type annotation missing",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Add a return type to '{0}': '{1}'.",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(
            AnalyzeNode,
            SyntaxKind.MethodDeclaration,
            SyntaxKind.FunctionStatement);
    }

    private static void AnalyzeNode(SyntaxNodeAnalysisContext context)
    {
        switch (context.Node)
        {
            case MethodDeclarationSyntax method:
                AnalyzeDeclaration(
                    context,
                    method,
                    method.Identifier,
                    method.ReturnType,
                    method.Body ?? (SyntaxNode?)method.ExpressionBody);
                break;

            case FunctionStatementSyntax function:
                AnalyzeDeclaration(
                    context,
                    function,
                    function.Identifier,
                    function.ReturnType,
                    function.Body ?? (SyntaxNode?)function.ExpressionBody);
                break;
        }
    }

    private static void AnalyzeDeclaration(
        SyntaxNodeAnalysisContext context,
        SyntaxNode node,
        SyntaxToken identifier,
        SyntaxNode? returnType,
        SyntaxNode? body)
    {
        if (returnType is not null || body is null)
            return;

        var symbol = context.SemanticModel.GetDeclaredSymbol(node) as IMethodSymbol;
        if (symbol is null)
            return;

        if (symbol.ReturnType.SpecialType is SpecialType.System_Unit or SpecialType.System_Void &&
            (symbol.IsOverride || !symbol.ExplicitInterfaceImplementations.IsDefaultOrEmpty))
            return;

        if (symbol.ReturnType is ErrorTypeSymbol)
            return;

        var boundBody = context.SemanticModel.GetBoundNode(body);
        var inferred = ReturnTypeCollector.Infer(boundBody);

        if (inferred is null ||
            inferred.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
            return;

        if (inferred is ErrorTypeSymbol)
            return;

        var typeDisplay = inferred.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
        var location = identifier.GetLocation();
        var diagnostic = Diagnostic.Create(Descriptor, location, symbol.Name, typeDisplay);
        context.ReportDiagnostic(diagnostic);
    }

}
