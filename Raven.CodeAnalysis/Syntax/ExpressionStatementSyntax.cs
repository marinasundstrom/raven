namespace Raven.CodeAnalysis.Syntax;

public partial class ExpressionStatementSyntax : StatementSyntax
{
    public partial ExpressionSyntax Expression { get; }

    internal ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
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