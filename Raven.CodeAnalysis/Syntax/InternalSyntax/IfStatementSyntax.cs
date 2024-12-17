namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class IfStatementSyntax : StatementSyntax
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
}