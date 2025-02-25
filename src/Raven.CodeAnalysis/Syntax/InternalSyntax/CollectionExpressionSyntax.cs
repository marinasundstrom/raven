namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class CollectionExpressionSyntax : ExpressionSyntax
{
    public CollectionExpressionSyntax(
        SyntaxToken openBracketToken,
        SyntaxList elements,
        SyntaxToken closeBracketToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.CollectionExpression,
              [
                      openBracketToken ?? throw new ArgumentNullException(nameof(openBracketToken)),
                      elements ?? throw new ArgumentNullException(nameof(elements)),
                      closeBracketToken ?? throw new ArgumentNullException(nameof(closeBracketToken))
              ],
              diagnostics)
    {
        if (openBracketToken.Kind != SyntaxKind.OpenBracketToken)
            throw new ArgumentException("Invalid token for open brace.", nameof(openBracketToken));

        if (closeBracketToken.Kind != SyntaxKind.CloseBracketToken)
            throw new ArgumentException("Invalid token for close brace.", nameof(closeBracketToken));
    }
}

internal static partial class SyntaxFactory
{
    public static CollectionExpressionSyntax CollectionExpression(
        SyntaxToken openBracketToken,
        SyntaxList elements,
        SyntaxToken closeBracketToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(openBracketToken, elements, closeBracketToken, diagnostics);
}