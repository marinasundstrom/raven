namespace Raven.CodeAnalysis.Syntax;

public partial class ArgumentSyntax : SyntaxNode
{
    public partial ExpressionSyntax Expression { get; }

    public ArgumentSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ArgumentSyntax(
        ExpressionSyntax expression)
        : this(new InternalSyntax.ArgumentSyntax((InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ArgumentSyntax Argument(ExpressionSyntax expression)
        => new ArgumentSyntax(expression);
}