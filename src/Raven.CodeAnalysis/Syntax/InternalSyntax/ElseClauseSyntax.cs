namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ElseClauseSyntax : SyntaxNode
{
    public ElseClauseSyntax(
        SyntaxToken elseKeyword,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ElseClause,
              [
                      elseKeyword ?? throw new ArgumentNullException(nameof(elseKeyword)),
                      expression ?? throw new ArgumentNullException(nameof(expression))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ElseClauseSyntax ElseClause(
        SyntaxToken elseKeyword,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(elseKeyword, expression, diagnostics);
}