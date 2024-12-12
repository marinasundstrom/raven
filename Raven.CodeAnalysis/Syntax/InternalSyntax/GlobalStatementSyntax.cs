namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public GlobalStatementSyntax(StatementSyntax statement)
        : base(SyntaxKind.GlobalStatement,
              [
                    statement
              ])
    {
    }
}