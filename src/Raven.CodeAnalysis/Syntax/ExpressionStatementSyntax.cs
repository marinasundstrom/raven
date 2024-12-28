namespace Raven.CodeAnalysis.Syntax;

public partial class ExpressionStatementSyntax : StatementSyntax
{
    public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken SemicolonToken { get; }

    internal ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression)
    : this(expression, SyntaxFactory.SemicolonToken)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression, SyntaxToken semicolonToken)
        : this(new InternalSyntax.ExpressionStatementSyntax((InternalSyntax.ExpressionSyntax)expression.Green, semicolonToken.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ExpressionStatementSyntax ExpressionStatement(ExpressionSyntax expression)
        => new ExpressionStatementSyntax(expression);

    public static ExpressionStatementSyntax ExpressionStatement(ExpressionSyntax expression, SyntaxToken semicolonToken)
        => new ExpressionStatementSyntax(expression, semicolonToken);
}