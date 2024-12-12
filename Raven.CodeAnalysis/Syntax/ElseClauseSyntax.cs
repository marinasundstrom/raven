namespace Raven.CodeAnalysis.Syntax;

public partial class ElseClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ElseKeyword { get; }

    public partial StatementSyntax Statement { get; }

    public ElseClauseSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public ElseClauseSyntax(SyntaxToken elseKeyword, StatementSyntax statement)
      : this(
            new InternalSyntax.ElseClauseSyntax(elseKeyword.Green, (InternalSyntax.StatementSyntax)statement.Green))
    {

    }

    public ElseClauseSyntax(StatementSyntax statement)
      : this(SyntaxFactory.ElseKeyword, statement)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ElseClauseSyntax ElseClause(StatementSyntax statement)
        => new ElseClauseSyntax(statement);
}