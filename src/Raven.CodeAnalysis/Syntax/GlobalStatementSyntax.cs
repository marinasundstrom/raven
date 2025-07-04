namespace Raven.CodeAnalysis.Syntax;

public partial class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public override SyntaxTokenList Modifiers { get; }

    public partial StatementSyntax Statement { get; }

    internal GlobalStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
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