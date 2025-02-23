namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class ExpressionStatementSyntax : StatementSyntax
{
    protected ExpressionStatementSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}

internal partial class ExpressionStatement1Syntax : ExpressionStatementSyntax
{
    public ExpressionStatement1Syntax(
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression ?? throw new ArgumentNullException(nameof(expression)),
              ],
              diagnostics)
    {
    }
}

internal partial class ExpressionStatement2Syntax : ExpressionStatementSyntax
{
    public ExpressionStatement2Syntax(
        ExpressionSyntax expression,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression ?? throw new ArgumentNullException(nameof(expression)),
                    semicolonToken
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ExpressionStatementSyntax ExpressionStatement(
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new ExpressionStatement1Syntax(expression, diagnostics);

    public static ExpressionStatementSyntax ExpressionStatementWithSemicolon(
        ExpressionSyntax expression, SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new ExpressionStatement2Syntax(expression, semicolonToken, diagnostics);
}