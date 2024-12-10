namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ExpressionStatementSyntax : StatementSyntax
{
    public ExpressionStatementSyntax(ExpressionSyntax expression)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression
              ])
    {
    }
    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.ExpressionStatementSyntax(this, parent, position);
    }
}