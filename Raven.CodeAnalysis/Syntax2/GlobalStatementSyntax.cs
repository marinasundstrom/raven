namespace Raven.CodeAnalysis.Syntax;

public class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public StatementSyntax Statement { get; set; }
}