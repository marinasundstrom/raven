namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EqualsValueClauseSyntax : StatementSyntax
{
    public EqualsValueClauseSyntax(
        SyntaxToken equalsToken,
        ExpressionSyntax value,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.EqualsValueClause,
              [
                      equalsToken,
                      value
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static EqualsValueClauseSyntax EqualsValueClause(
        SyntaxToken equalsToken,
        ExpressionSyntax value,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(equalsToken, value, diagnostics);
}