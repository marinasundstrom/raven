namespace Raven.CodeAnalysis.Syntax;

public partial class ExpressionStatementSyntax : StatementSyntax
{
    public partial ExpressionSyntax Expression { get; }

    public ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression)
        : this(new InternalSyntax.ExpressionStatementSyntax((InternalSyntax.ExpressionSyntax)expression.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ExpressionStatementSyntax ExpressionStatement(ExpressionSyntax expression)
        => new ExpressionStatementSyntax(expression);
}