namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class GlobalStatementSyntax : SyntaxNode
{
    public GlobalStatementSyntax(StatementSyntax statement)
        : base(SyntaxKind.GlobalStatement,
              [
                    statement
              ],
              statement.FullWidth)
    {
    }
    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.GlobalStatementSyntax(this, parent);
    }
}