namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class IdentifierNameSyntax : ExpressionSyntax
{
    public IdentifierNameSyntax(
        SyntaxToken identifierToken)
        : base(
              SyntaxKind.IdentifierName,
              [
                      identifierToken,
              ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.IdentifierNameSyntax(this, parent, position);
    }
}