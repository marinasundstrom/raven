namespace Raven.CodeAnalysis.Syntax;

public partial class IsPatternExpressionSyntax : ExpressionSyntax
{
    public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken IsKeyword { get; }

    public partial PatternSyntax Pattern { get; }

    internal IsPatternExpressionSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public IsPatternExpressionSyntax(ExpressionSyntax expression, SyntaxToken isKeyword, PatternSyntax pattern)
        : this(new InternalSyntax.IsPatternExpressionSyntax((InternalSyntax.ExpressionSyntax)expression.Green, isKeyword.Green, (InternalSyntax.PatternSyntax)pattern.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static IsPatternExpressionSyntax IsPatternExpression(ExpressionSyntax expression, SyntaxToken isKeyword, PatternSyntax pattern)
        => new IsPatternExpressionSyntax(expression, isKeyword, pattern);
}