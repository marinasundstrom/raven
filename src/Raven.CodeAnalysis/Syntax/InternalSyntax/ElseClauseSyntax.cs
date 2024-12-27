namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ElseClauseSyntax : SyntaxNode
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
}