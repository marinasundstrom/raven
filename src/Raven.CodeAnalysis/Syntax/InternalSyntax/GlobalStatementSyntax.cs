namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public GlobalStatementSyntax(StatementSyntax statement)
        : base(SyntaxKind.GlobalStatement,
              [
                    statement
              ])
    {
    }
}