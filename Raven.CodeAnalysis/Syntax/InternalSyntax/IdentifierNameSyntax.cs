namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class IdentifierNameSyntax : ExpressionSyntax
{
    public IdentifierNameSyntax(
        SyntaxToken identifierToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.IdentifierName,
              [
                      identifierToken,
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.IdentifierNameSyntax(this, parent, position);
    }
}