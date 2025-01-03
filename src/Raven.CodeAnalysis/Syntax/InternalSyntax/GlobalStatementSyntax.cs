namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public GlobalStatementSyntax(
        StatementSyntax statement,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.GlobalStatement,
              [
                    statement
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static GlobalStatementSyntax GlobalStatement(
        StatementSyntax statement,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(statement, diagnostics);
}