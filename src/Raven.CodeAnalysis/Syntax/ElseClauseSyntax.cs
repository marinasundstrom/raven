namespace Raven.CodeAnalysis.Syntax;

public partial class ElseClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ElseKeyword { get; }

    public partial ExpressionSyntax Expression { get; }

    internal ElseClauseSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public ElseClauseSyntax(SyntaxToken elseKeyword, ExpressionSyntax expression)
      : this(
            new InternalSyntax.ElseClauseSyntax(elseKeyword.Green, (InternalSyntax.ExpressionSyntax)expression.Green))
    {

    }

    public ElseClauseSyntax(ExpressionSyntax expression)
      : this(SyntaxFactory.ElseKeyword, expression)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ElseClauseSyntax ElseClause(ExpressionSyntax expression)
        => new ElseClauseSyntax(expression);
}