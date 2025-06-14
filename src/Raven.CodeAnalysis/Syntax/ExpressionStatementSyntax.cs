namespace Raven.CodeAnalysis.Syntax;

public abstract class ExpressionStatementSyntax : StatementSyntax
{
    public virtual ExpressionSyntax Expression { get; }

    internal ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }
}

public partial class ExpressionStatement1Syntax : ExpressionStatementSyntax
{
    public override partial ExpressionSyntax Expression { get; }

    internal ExpressionStatement1Syntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ExpressionStatement1Syntax(ExpressionSyntax expression)
    : this(expression, SyntaxFactory.SemicolonToken)
    {
    }

    public ExpressionStatement1Syntax(ExpressionSyntax expression, SyntaxToken terminatorToken)
        : this(new InternalSyntax.ExpressionStatement1Syntax((InternalSyntax.ExpressionSyntax)expression.Green))
    {
    }
}


public partial class ExpressionStatement2Syntax : ExpressionStatementSyntax
{
    public override partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal ExpressionStatement2Syntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ExpressionStatement2Syntax(ExpressionSyntax expression)
    : this(expression, SyntaxFactory.SemicolonToken)
    {
    }

    public ExpressionStatement2Syntax(ExpressionSyntax expression, SyntaxToken terminatorToken)
        : this(new InternalSyntax.ExpressionStatement2Syntax((InternalSyntax.ExpressionSyntax)expression.Green, terminatorToken.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ExpressionStatementSyntax ExpressionStatement(ExpressionSyntax expression)
        => new ExpressionStatement1Syntax(expression);

    public static ExpressionStatementSyntax ExpressionStatementWithSemicolon(ExpressionSyntax expression, SyntaxToken terminatorToken)
        => new ExpressionStatement2Syntax(expression, terminatorToken);
}