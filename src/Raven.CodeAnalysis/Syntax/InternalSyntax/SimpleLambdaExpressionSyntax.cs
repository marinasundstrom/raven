namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class SimpleLambdaExpressionSyntax : LambdaExpressionSyntax
{
    protected SimpleLambdaExpressionSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }

    public SimpleLambdaExpressionSyntax(
        SyntaxToken funcKeyword,
        ParameterSyntax parameter,
        ArrowTypeClauseSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
            : base(
          SyntaxKind.SimpleLambdaExpression,
          [
                funcKeyword ?? throw new ArgumentNullException(nameof(funcKeyword)),
                parameter ?? throw new ArgumentNullException(nameof(parameter)),
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
    public static SimpleLambdaExpressionSyntax SimpleLambdaExpression(
        SyntaxToken funcKeyword,
        ParameterSyntax parameter,
        ArrowTypeClauseSyntax returnType,
        SyntaxToken arrowToken,
        ExpressionSyntax? expressionBody,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(funcKeyword, parameter, returnType, arrowToken, expressionBody, diagnostics);
}