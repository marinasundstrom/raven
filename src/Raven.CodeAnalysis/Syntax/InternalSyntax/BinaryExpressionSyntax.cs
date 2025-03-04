namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class BinaryExpressionSyntax : ExpressionSyntax
{
    public BinaryExpressionSyntax(
        SyntaxKind kind,
        ExpressionSyntax leftHandSide,
        SyntaxToken operatorToken,
        ExpressionSyntax rightHandSide,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              kind,
              [
                      leftHandSide ?? throw new ArgumentNullException(nameof(leftHandSide)),
                      operatorToken ?? throw new ArgumentNullException(nameof(operatorToken)),
                      rightHandSide ?? throw new ArgumentNullException(nameof(rightHandSide))
              ],
              diagnostics)
    {

    }
}

internal static partial class SyntaxFactory
{
    public static BinaryExpressionSyntax BinaryExpression(
        SyntaxKind kind,
        ExpressionSyntax leftHandSide,
        SyntaxToken operatorToken,
        ExpressionSyntax rightHandSide,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, leftHandSide, operatorToken, rightHandSide, diagnostics);
}