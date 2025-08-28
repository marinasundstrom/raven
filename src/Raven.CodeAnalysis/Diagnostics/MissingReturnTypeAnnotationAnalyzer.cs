using System.Linq;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports methods without explicit return type annotations and suggests the inferred type.
/// </summary>
public sealed class MissingReturnTypeAnnotationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9001";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Return type annotation missing",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Method '{0}' has no return type annotation; consider '{1}'",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxTreeAction(AnalyzeTree);
    }

    private static void AnalyzeTree(SyntaxTreeAnalysisContext context)
    {
        var semanticModel = context.Compilation.GetSemanticModel(context.SyntaxTree);
        var root = context.SyntaxTree.GetRoot();

        void AnalyzeNode(SyntaxNode node, SyntaxToken identifier, SyntaxNode? returnType)
        {
            if (returnType is not null)
                return;

            var symbol = semanticModel.GetDeclaredSymbol(node) as IMethodSymbol;
            if (symbol is null)
                return;

            var typeDisplay = symbol.ReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            var location = identifier.GetLocation();
            var diagnostic = Diagnostic.Create(Descriptor, location, symbol.Name, typeDisplay);
            context.ReportDiagnostic(diagnostic);
        }

        foreach (var method in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
            AnalyzeNode(method, method.Identifier, method.ReturnType);

        foreach (var function in root.DescendantNodes().OfType<FunctionStatementSyntax>())
            AnalyzeNode(function, function.Identifier, function.ReturnType);
    }
}
