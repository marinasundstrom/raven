namespace Raven.CodeAnalysis.Syntax;

public partial class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public partial StatementSyntax Statement { get; }

    public GlobalStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public GlobalStatementSyntax(StatementSyntax statement)
        : this(new InternalSyntax.GlobalStatementSyntax((InternalSyntax.StatementSyntax)statement.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static GlobalStatementSyntax GlobalStatement(StatementSyntax statement)
        => new GlobalStatementSyntax(statement);
}

