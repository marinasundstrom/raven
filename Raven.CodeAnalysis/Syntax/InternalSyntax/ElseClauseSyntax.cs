namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ElseClauseSyntax : SyntaxNode
{
    public ElseClauseSyntax(
        SyntaxToken elseKeyword,
        StatementSyntax statement)
        : base(
              SyntaxKind.ElseClause,
              [
                      elseKeyword,
                      statement
              ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.ElseClauseSyntax(this, parent, position);
    }
}