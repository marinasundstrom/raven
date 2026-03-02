namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports string-literal concatenations that can be merged into a single literal.
/// </summary>
public sealed class MergeStringLiteralConcatenationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9022";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "String literals can be merged",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "String literals can be merged into a single literal.",
        category: "Style",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        context.RegisterSyntaxNodeAction(AnalyzeAddExpression, SyntaxKind.AddExpression);
    }

    private static void AnalyzeAddExpression(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not BinaryExpressionSyntax add)
            return;

        var semanticModel = context.SemanticModel;
        if (!IsStringType(semanticModel.GetTypeInfo(add).Type))
            return;

        if (IsParentStringConcat(semanticModel, add))
            return;

        var parts = new List<ExpressionSyntax>();
        FlattenStringConcat(semanticModel, add, parts);
        if (parts.Count < 2)
            return;

        if (!parts.All(IsStringLiteral))
            return;

        var location = add.OperatorToken.GetLocation();
        context.ReportDiagnostic(Diagnostic.Create(Descriptor, location));
    }

    private static bool IsParentStringConcat(SemanticModel semanticModel, BinaryExpressionSyntax add)
    {
        if (add.Parent is BinaryExpressionSyntax parentAdd &&
            parentAdd.Kind == SyntaxKind.AddExpression &&
            IsStringType(semanticModel.GetTypeInfo(parentAdd).Type))
        {
            return true;
        }

        return false;
    }

    private static void FlattenStringConcat(
        SemanticModel semanticModel,
        ExpressionSyntax expression,
        List<ExpressionSyntax> parts)
    {
        if (expression is BinaryExpressionSyntax add &&
            add.Kind == SyntaxKind.AddExpression &&
            IsStringType(semanticModel.GetTypeInfo(add).Type))
        {
            FlattenStringConcat(semanticModel, add.Left, parts);
            FlattenStringConcat(semanticModel, add.Right, parts);
            return;
        }

        parts.Add(expression);
    }

    private static bool IsStringType(ITypeSymbol? type)
        => type?.SpecialType == SpecialType.System_String;

    private static bool IsStringLiteral(ExpressionSyntax expression)
        => expression is LiteralExpressionSyntax literal &&
           literal.Kind == SyntaxKind.StringLiteralExpression;
}
