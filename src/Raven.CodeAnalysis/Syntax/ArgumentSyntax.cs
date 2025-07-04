namespace Raven.CodeAnalysis.Syntax;

public partial class ArgumentSyntax : SyntaxNode
{
    public partial NameColonSyntax? NameColon { get; }

    public partial ExpressionSyntax Expression { get; }

    internal ArgumentSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ArgumentSyntax(
        NameColonSyntax? nameColon,
        ExpressionSyntax expression)
        : this(new InternalSyntax.ArgumentSyntax((InternalSyntax.NameColonSyntax?)nameColon?.Green, (InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }

    public ArgumentSyntax(
        ExpressionSyntax expression)
        : this(new InternalSyntax.ArgumentSyntax(null, (InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ArgumentSyntax Argument(NameColonSyntax? name, ExpressionSyntax expression)
    => new ArgumentSyntax(name, expression);

    public static ArgumentSyntax Argument(ExpressionSyntax expression)
        => new ArgumentSyntax(expression);
}