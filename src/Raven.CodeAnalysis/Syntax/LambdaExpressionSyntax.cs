namespace Raven.CodeAnalysis.Syntax;

public abstract partial class LambdaExpressionSyntax : ExpressionSyntax
{
    internal LambdaExpressionSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public abstract SyntaxToken FuncKeyword { get; }

    public abstract SyntaxToken ArrowToken { get; }

    public abstract ExpressionSyntax ExpressionBody { get; }
}