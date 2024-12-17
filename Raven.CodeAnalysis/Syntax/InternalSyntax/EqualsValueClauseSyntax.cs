namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EqualsValueClauseSyntax : StatementSyntax
{
    public EqualsValueClauseSyntax(
        SyntaxToken equalsToken,
        ExpressionSyntax value)
        : base(
              SyntaxKind.EqualsValueClause,
              [
                      equalsToken,
                      value
              ])
    {
    }
}