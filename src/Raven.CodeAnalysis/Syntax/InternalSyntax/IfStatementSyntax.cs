
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
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
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
              ],
              diagnostics)
    {
    }

    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        IEnumerable<Diagnostic>? diagnostics = null)
    : base(
          SyntaxKind.IfStatement,
          [
                ifKeyword,
                openParenToken,
                condition,
                closeParenToken,
                statement,
                elseClause
          ],
          diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static IfStatementSyntax IfStatement(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(ifKeyword, openParenToken, condition, closeParenToken, statement, elseClause, diagnostics);
}