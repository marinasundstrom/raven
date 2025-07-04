namespace Raven.CodeAnalysis.Syntax;

public partial class ExpressionStatementSyntax : StatementSyntax
{
    public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression)
    : this(expression, SyntaxFactory.SemicolonToken)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression, SyntaxToken terminatorToken)
        : this(new InternalSyntax.ExpressionStatementSyntax((InternalSyntax.ExpressionSyntax)expression.Green, terminatorToken.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ExpressionStatementSyntax ExpressionStatement(ExpressionSyntax expression, SyntaxToken terminatorToken)
        => new ExpressionStatementSyntax(expression, terminatorToken);
}