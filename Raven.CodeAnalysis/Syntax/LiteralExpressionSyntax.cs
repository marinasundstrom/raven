namespace Raven.CodeAnalysis.Syntax;

public partial class LiteralExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }
    public partial SyntaxToken Token { get; }

    public LiteralExpressionSyntax(GreenNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public LiteralExpressionSyntax(SyntaxKind kind, SyntaxToken token)
         : this(new InternalSyntax.LiteralExpressionSyntax(kind, token.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    // TODO: Fix
    public static LiteralExpressionSyntax LiteralExpression(SyntaxKind kind, SyntaxToken token) => new LiteralExpressionSyntax(kind, token);
}