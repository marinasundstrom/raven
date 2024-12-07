namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ElseClauseSyntax : SyntaxNode
{
    public ElseClauseSyntax(
        SyntaxToken elseKeyword,
        SyntaxList block,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.ElseClause,
              [
                      elseKeyword,
                      block
              ],
              elseKeyword.FullWidth + block.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.ElseClauseSyntax(this, parent);
    }
}
