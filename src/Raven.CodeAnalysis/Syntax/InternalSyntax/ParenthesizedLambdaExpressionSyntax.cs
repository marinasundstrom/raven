namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ParenthesizedLambdaExpressionSyntax : LambdaExpressionSyntax
{
    internal ParenthesizedLambdaExpressionSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }

    public ParenthesizedLambdaExpressionSyntax(
        SyntaxToken funcKeyword,
        ParameterListSyntax parameterList,
        ArrowTypeClauseSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
            : base(
          SyntaxKind.ParenthesizedLambdaExpression,
          [
                funcKeyword ?? throw new ArgumentNullException(nameof(funcKeyword)),
                parameterList ?? throw new ArgumentNullException(nameof(parameterList)),
                returnType ?? throw new ArgumentNullException(nameof(returnType)),
                arrowToken ?? throw new ArgumentNullException(nameof(arrowToken)),
                expressionBody!

          ],
          diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(
        SyntaxToken funcKeyword,
        ParameterListSyntax parameterList,
        ArrowTypeClauseSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(funcKeyword, parameterList, returnType, arrowToken, expressionBody, diagnostics);
}