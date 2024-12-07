namespace Raven.CodeAnalysis.Syntax;

public partial class ElseClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ElseKeyword { get; }

    public partial StatementSyntax StatementOrBlock { get; }

    public ElseClauseSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null)
    : base(greenNode, parent)
    {
    }

    public ElseClauseSyntax(SyntaxToken elseKeyword, StatementSyntax statementOrBlock)
      : this(
            new InternalSyntax.ElseClauseSyntax(elseKeyword.Green, (InternalSyntax.StatementSyntax)statementOrBlock.Green))
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