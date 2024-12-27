namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class MemberAccessExpressionSyntax : ExpressionSyntax
{
    public MemberAccessExpressionSyntax(
        SyntaxKind kind,
        ExpressionSyntax expression,
        SyntaxToken operatorToken,
        SimpleNameSyntax name)
        : base(
              kind,
              [
                      expression,
                      operatorToken,
                      name
              ])
    {
    }
}