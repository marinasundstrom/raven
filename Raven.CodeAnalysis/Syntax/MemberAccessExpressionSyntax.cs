namespace Raven.CodeAnalysis.Syntax;

public partial class MemberAccessExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }
    public partial ExpressionSyntax Expression { get; }
    public partial SyntaxToken OperatorToken { get; }
    public partial SimpleNameSyntax Name { get; }

    internal MemberAccessExpressionSyntax(
        InternalSyntax.MemberAccessExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public MemberAccessExpressionSyntax(SyntaxKind kind, ExpressionSyntax expression, SyntaxToken operatorToken, SimpleNameSyntax name)
          : this(
                new InternalSyntax.MemberAccessExpressionSyntax(kind, (InternalSyntax.ExpressionSyntax)expression.Green, operatorToken.Green, (InternalSyntax.SimpleNameSyntax)name.Green), null)
    {

    }

    public MemberAccessExpressionSyntax(SyntaxKind kind, ExpressionSyntax expression, SimpleNameSyntax name)
        : this(kind, expression, SyntaxFactory.DotToken, name)
    {

    }
}

public static partial class SyntaxFactory
{
    public static MemberAccessExpressionSyntax MemberAccessExpression(SyntaxKind kind, ExpressionSyntax expression, SimpleNameSyntax name)
        => new MemberAccessExpressionSyntax(kind, expression, name);

    public static MemberAccessExpressionSyntax MemberAccessExpression(SyntaxKind kind, ExpressionSyntax expression, SyntaxToken operatorToken, SimpleNameSyntax name)
        => new MemberAccessExpressionSyntax(kind, expression, operatorToken, name);
}