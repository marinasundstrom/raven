namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ExpressionStatementSyntax : StatementSyntax
{
    public ExpressionStatementSyntax(ExpressionSyntax expression)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression
              ],
              expression.FullWidth)
    {
    }
    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.ExpressionStatementSyntax(this, parent);
    }
}
