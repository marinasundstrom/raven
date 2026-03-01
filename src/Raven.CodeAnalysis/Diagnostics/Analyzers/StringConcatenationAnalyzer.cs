namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports string concatenations that can be rewritten as interpolated strings.
/// </summary>
public sealed class StringConcatenationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9010";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "String concatenation can be an interpolated string",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "String concatenation can be simplified to an interpolated string.",
        category: "Style",
        defaultSeverity: DiagnosticSeverity.Info);

    public override void Initialize(AnalysisContext context)
    {
        // Adjust the SyntaxKind names if Raven differs, but the idea is:
        // - AddExpression:  a + b
        // - AddAssignmentExpression: a += b
        context.RegisterSyntaxNodeAction(AnalyzeAddExpression, SyntaxKind.AddExpression);
        context.RegisterSyntaxNodeAction(AnalyzeAddAssignment, SyntaxKind.AddAssignmentExpression);
    }

    private static void AnalyzeAddExpression(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not BinaryExpressionSyntax add)
            return;

        var semanticModel = context.SemanticModel;

        // Must be string concat (not numeric +)
        if (!IsStringType(semanticModel.GetTypeInfo(add).Type))
            return;

        // Only report on the topmost node in a chain: (a + b) + c
        if (IsParentStringConcat(semanticModel, add))
            return;

        // Flatten the chain into parts
        var parts = new List<ExpressionSyntax>();
        FlattenStringConcat(semanticModel, add, parts);

        // If everything is a string literal, constant folding is better than interpolation.
        if (parts.Count == 0 || parts.All(IsStringLiteral))
            return;

        // (Optional) if it’s only 1 part, nothing to rewrite
        if (parts.Count < 2)
            return;

        // Place diagnostic on the operator token or the whole expression (choose what your API supports best)
        var location = add.OperatorToken.GetLocation();
        context.ReportDiagnostic(Diagnostic.Create(Descriptor, location));
    }

    private static void AnalyzeAddAssignment(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not AssignmentExpressionSyntax assignment)
            return;

        // If your Raven syntax exposes operator kind, ensure it is "+="
        // (You already do similar checks in your other analyzer via SyntaxKind.)
        var semanticModel = context.SemanticModel;

        // For "+=" the whole assignment may not be "string", so check RHS or LHS type.
        // Prefer checking LHS type: if LHS is string, "+=" is string concat.
        var leftType = semanticModel.GetTypeInfo((ExpressionSyntax)assignment.Left).Type;
        if (!IsStringType(leftType))
            return;

        // Optional: also ensure RHS participates (guards against weird operator overloads)
        // var rightType = semanticModel.GetTypeInfo(assignment.Right).Type;

        // If RHS is itself a concat chain, the codefix can fold everything into one interpolated string.
        // Even if RHS isn’t a chain, $"...{rhs}" may still be useful.
        var location = assignment.OperatorToken.GetLocation();
        context.ReportDiagnostic(Diagnostic.Create(Descriptor, location));
    }

    private static bool IsParentStringConcat(SemanticModel semanticModel, BinaryExpressionSyntax add)
    {
        // If parent is also AddExpression and results in string, we’re not topmost.
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
        ExpressionSyntax expr,
        List<ExpressionSyntax> parts)
    {
        if (expr is BinaryExpressionSyntax add &&
            add.Kind == SyntaxKind.AddExpression &&
            IsStringType(semanticModel.GetTypeInfo(add).Type))
        {
            FlattenStringConcat(semanticModel, add.Left, parts);
            FlattenStringConcat(semanticModel, add.Right, parts);
            return;
        }

        parts.Add(expr);
    }

    private static bool IsStringType(ITypeSymbol? type)
    {
        if (type is null)
            return false;

        // Adjust to match Raven’s symbol model.
        // If you have SpecialType.System_String, use that.
        if (type.SpecialType == SpecialType.System_String)
            return true;

        // Fallback: metadata name / display name check (only if needed)
        // return type.Name == "String" && type.ContainingNamespace?.ToDisplayString() == "System";

        return false;
    }

    private static bool IsStringLiteral(ExpressionSyntax expr)
    {
        // Adjust if Raven names differ.
        // In Roslyn you’d check LiteralExpressionSyntax + kind == StringLiteralExpression.
        if (expr is LiteralExpressionSyntax lit &&
            lit.Kind == SyntaxKind.StringLiteralExpression)
        {
            return true;
        }

        return false;
    }
}
