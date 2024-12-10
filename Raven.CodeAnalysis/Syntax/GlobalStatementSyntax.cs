namespace Raven.CodeAnalysis.Syntax;

public partial class GlobalStatementSyntax : MemberDeclarationSyntax
{
    public partial StatementSyntax Statement { get; }

    public GlobalStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public GlobalStatementSyntax(StatementSyntax statement)
        : this(new InternalSyntax.GlobalStatementSyntax((InternalSyntax.StatementSyntax)statement.Green))
    {
    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitGlobalStatement(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitGlobalStatement(this);
    }
}

public static partial class SyntaxFactory
{
    public static GlobalStatementSyntax GlobalStatement(StatementSyntax statement)
        => new GlobalStatementSyntax(statement);
}