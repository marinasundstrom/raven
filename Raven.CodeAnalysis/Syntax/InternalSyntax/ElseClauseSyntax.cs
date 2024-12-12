namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class ElseClauseSyntax : SyntaxNode
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