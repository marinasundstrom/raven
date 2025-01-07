namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public GlobalStatementSyntax(
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.GlobalStatement,
              [
                    statement  ?? throw new ArgumentNullException(nameof(statement))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static GlobalStatementSyntax GlobalStatement(
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(statement, diagnostics);
}