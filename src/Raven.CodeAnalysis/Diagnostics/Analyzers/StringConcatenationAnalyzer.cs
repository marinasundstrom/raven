namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>
/// Reports string concatenations that can be rewritten as interpolated strings.
/// </summary>
public sealed class StringConcatenationAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9021";
    public const string MergeDiagnosticId = "RAV9022";

    private static readonly DiagnosticDescriptor InterpolationDescriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "String concatenation can be an interpolated string",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "String concatenation can be simplified to an interpolated string.",
        category: "Style",
        defaultSeverity: DiagnosticSeverity.Info);

    private static readonly DiagnosticDescriptor MergeDescriptor = DiagnosticDescriptor.Create(
        id: MergeDiagnosticId,
        title: "String concatenation can be merged",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "String concatenation can be simplified to a single string literal.",
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
        if (context.Node is not InfixOperatorExpressionSyntax add)
            return;

        var semanticModel = context.SemanticModel;

        // Must be string concat (not numeric +)
        if (!IsStringType(semanticModel.GetTypeInfo(add).Type))
            return;

        if (CanMergeIntoStringText(add.Left, semanticModel) &&
            CanMergeIntoStringText(add.Right, semanticModel))
        {
            context.ReportDiagnostic(Diagnostic.Create(MergeDescriptor, add.OperatorToken.GetLocation()));
        }

        // Interpolation fix applies to the full chain only.
        if (IsParentStringConcat(semanticModel, add))
            return;

        var parts = new List<ExpressionSyntax>();
        FlattenStringConcat(semanticModel, add, parts);
        if (parts.Count < 2)
            return;

        if (!parts.Any(part => !CanMergeIntoStringText(part, semanticModel)))
            return;

        context.ReportDiagnostic(Diagnostic.Create(InterpolationDescriptor, add.GetLocation()));
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

        var parts = new List<ExpressionSyntax>();
        FlattenStringConcat(semanticModel, assignment.Right, parts);
        if (parts.Count < 2 || !parts.Any(part => !CanMergeIntoStringText(part, semanticModel)))
            return;

        var location = assignment.OperatorToken.GetLocation();
        context.ReportDiagnostic(Diagnostic.Create(InterpolationDescriptor, location));
    }

    private static bool IsParentStringConcat(SemanticModel semanticModel, InfixOperatorExpressionSyntax add)
    {
        // If parent is also AddExpression and results in string, we’re not topmost.
        if (add.Parent is InfixOperatorExpressionSyntax parentAdd &&
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
        if (expr is InfixOperatorExpressionSyntax add &&
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

    private static bool CanMergeIntoStringText(ExpressionSyntax expression, SemanticModel semanticModel)
    {
        if (IsStringLiteral(expression))
            return true;

        if (TryGetConstantText(expression, semanticModel, out _))
            return true;

        return false;
    }

    private static bool TryGetConstantText(ExpressionSyntax expression, SemanticModel semanticModel, out string text)
    {
        if (expression is ParenthesizedExpressionSyntax parenthesized)
            return TryGetConstantText(parenthesized.Expression, semanticModel, out text);

        if (expression is LiteralExpressionSyntax literal)
            return TryFormatConstant(literal.Token.Value, out text);

        var symbol = semanticModel.GetSymbolInfo(expression).Symbol;
        symbol = symbol?.UnderlyingSymbol ?? symbol;

        switch (symbol)
        {
            case ILocalSymbol { IsConst: true } local:
                return TryFormatConstant(local.ConstantValue, out text);
            case IFieldSymbol { IsConst: true } field:
                return TryFormatConstant(field.GetConstantValue(), out text);
            default:
                text = string.Empty;
                return false;
        }
    }

    private static bool TryFormatConstant(object? value, out string text)
    {
        switch (value)
        {
            case null:
                text = string.Empty;
                return true;
            case string s:
                text = s;
                return true;
            case char c:
                text = c.ToString();
                return true;
            case bool b:
                text = b.ToString();
                return true;
            case sbyte i8:
                text = i8.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case byte u8:
                text = u8.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case short i16:
                text = i16.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case ushort u16:
                text = u16.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case int i32:
                text = i32.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case uint u32:
                text = u32.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case long i64:
                text = i64.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case ulong u64:
                text = u64.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case nint ni:
                text = ni.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            case nuint nui:
                text = nui.ToString(System.Globalization.CultureInfo.InvariantCulture);
                return true;
            default:
                text = string.Empty;
                return false;
        }
    }
}
