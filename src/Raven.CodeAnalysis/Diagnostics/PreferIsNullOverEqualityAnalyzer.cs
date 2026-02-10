using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class PreferIsNullOverEqualityAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9015";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Use 'is null' for strict null checks",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "This comparison may call a custom equality operator, so nullability isn’t narrowed. Use 'is null' or 'is not null' for a strict check.",
        category: "Typing",
        defaultSeverity: DiagnosticSeverity.Warning);

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(
            AnalyzeBinaryExpression,
            SyntaxKind.EqualsExpression,
            SyntaxKind.NotEqualsExpression);

    private static void AnalyzeBinaryExpression(SyntaxNodeAnalysisContext context)
    {
        if (context.Node is not BinaryExpressionSyntax binary)
            return;

        if (binary.OperatorToken.Kind is not (SyntaxKind.EqualsEqualsToken or SyntaxKind.NotEqualsToken))
            return;

        if (!IsNullLiteral(binary.Left) && !IsNullLiteral(binary.Right))
            return;

        var comparedExpression = IsNullLiteral(binary.Left) ? binary.Right : binary.Left;
        var comparedType = context.SemanticModel.GetTypeInfo(comparedExpression).Type;
        if (comparedType is null || comparedType.TypeKind == TypeKind.Error)
            return;

        if (IsPointerLike(comparedType))
            return;

        context.ReportDiagnostic(Diagnostic.Create(Descriptor, binary.GetLocation()));
    }

    private static bool IsNullLiteral(ExpressionSyntax expression)
    {
        while (expression is ParenthesizedExpressionSyntax parenthesized)
            expression = parenthesized.Expression;

        return expression.Kind == SyntaxKind.NullLiteralExpression;
    }

    private static bool IsPointerLike(ITypeSymbol type)
    {
        return type is IPointerTypeSymbol or IAddressTypeSymbol or ByRefTypeSymbol;
    }
}
