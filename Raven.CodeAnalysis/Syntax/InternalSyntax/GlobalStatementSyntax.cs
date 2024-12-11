namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public GlobalStatementSyntax(StatementSyntax statement)
        : base(SyntaxKind.GlobalStatement,
              [
                    statement
              ])
    {
    }
    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.GlobalStatementSyntax(this, parent, position);
    }
}