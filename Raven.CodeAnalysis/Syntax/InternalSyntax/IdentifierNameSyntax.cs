namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class IdentifierNameSyntax : ExpressionSyntax
{
    public IdentifierNameSyntax(
        SyntaxToken identifierToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.IdentifierName,
              [
                      identifierToken,
              ],
              identifierToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.IdentifierNameSyntax(this, parent);
    }
}
