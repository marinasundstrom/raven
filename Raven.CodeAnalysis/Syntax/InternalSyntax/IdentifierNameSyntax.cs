namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class IdentifierNameSyntax : ExpressionSyntax
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
}