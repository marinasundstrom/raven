namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ReturnStatementSyntax : StatementSyntax
{
    public ReturnStatementSyntax(
        SyntaxToken returnKeyword,
        ExpressionSyntax expression,
        SyntaxToken semicolonToken)
        : base(
              SyntaxKind.ReturnStatement,
              [
                      returnKeyword,
                      expression,
                      semicolonToken
              ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.ReturnStatementSyntax(this, parent, position);
    }
}