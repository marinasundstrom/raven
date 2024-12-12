namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class IfStatementSyntax : StatementSyntax
{
    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        SyntaxToken semicolonToken)
        : base(
              SyntaxKind.IfStatement,
              [
                ifKeyword,
                openParenToken,
                condition,
                closeParenToken,
                statement,
                elseClause,
                semicolonToken
              ])
    {
    }

    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause)
    : base(
          SyntaxKind.IfStatement,
          [
                ifKeyword,
                openParenToken,
                condition,
                closeParenToken,
                statement,
                elseClause
          ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.IfStatementSyntax(this, parent, position);
    }
}