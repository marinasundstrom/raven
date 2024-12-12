namespace Raven.CodeAnalysis.Syntax;

public partial class EqualsValueClauseSyntax : SyntaxNode
{
    public partial SyntaxToken EqualsToken { get; }

    public partial ExpressionSyntax Value { get; }

    public EqualsValueClauseSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public EqualsValueClauseSyntax(SyntaxToken equalsToken, ExpressionSyntax value)
      : this(
            new InternalSyntax.EqualsValueClauseSyntax(equalsToken.Green, (InternalSyntax.ExpressionSyntax)value.Green))
    {

    }

    public EqualsValueClauseSyntax(ExpressionSyntax value)
      : this(SyntaxFactory.EqualsToken, value)
    {

    }
}

public static partial class SyntaxFactory
{
    public static EqualsValueClauseSyntax EqualsValueClause(SyntaxToken equalsToken, ExpressionSyntax value)
        => new EqualsValueClauseSyntax(equalsToken, value);

    public static EqualsValueClauseSyntax EqualsValueClause(ExpressionSyntax value)
        => new EqualsValueClauseSyntax(value);
}