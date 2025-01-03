namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ElseClauseSyntax : SyntaxNode
{
    public ElseClauseSyntax(
        SyntaxToken elseKeyword,
        StatementSyntax statement,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.ElseClause,
              [
                      elseKeyword,
                      statement
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ElseClauseSyntax ElseClause(
        SyntaxToken elseKeyword,
        StatementSyntax statement,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(elseKeyword, statement, diagnostics);
}