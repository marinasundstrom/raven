namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ElseClauseSyntax : SyntaxNode
{
    public ElseClauseSyntax(
        SyntaxToken elseKeyword,
        StatementSyntax statement,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.ElseClause,
              [
                      elseKeyword,
                      statement
              ],
              elseKeyword.FullWidth + statement.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.ElseClauseSyntax(this, parent);
    }
}