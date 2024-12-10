namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class EqualsValueClauseSyntax : StatementSyntax
{
    public EqualsValueClauseSyntax(
        SyntaxToken equalsToken,
        ExpressionSyntax value,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.EqualsValueClause,
              [
                      equalsToken,
                      value
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.EqualsValueClauseSyntax(this, parent, position);
    }
}