namespace Raven.CodeAnalysis.Syntax;

public partial class IdentifierNameSyntax : ExpressionSyntax
{
    public partial SyntaxToken IdentifierToken { get; }

    public IdentifierNameSyntax(
        InternalSyntax.IdentifierNameSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public IdentifierNameSyntax(SyntaxToken identifierToken)
          : this(
                new InternalSyntax.IdentifierNameSyntax(identifierToken.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static IdentifierNameSyntax IdentifierName(string text) => new IdentifierNameSyntax(IdentifierToken(text));
}