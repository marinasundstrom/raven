namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class EqualsValueClauseSyntax : StatementSyntax
{
    public EqualsValueClauseSyntax(
        SyntaxToken equalsToken,
        ExpressionSyntax value,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.EqualsValueClause,
              [
                      equalsToken,
                      value
              ],
              equalsToken.FullWidth + value.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.EqualsValueClauseSyntax(this, parent);
    }
}
