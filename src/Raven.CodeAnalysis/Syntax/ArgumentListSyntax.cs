namespace Raven.CodeAnalysis.Syntax;

public partial class ArgumentListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenParenToken { get; }
    public partial SeparatedSyntaxList<ArgumentSyntax> Arguments { get; }
    public partial SyntaxToken CloseParenToken { get; }

    internal ArgumentListSyntax(
        InternalSyntax.ArgumentListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ArgumentListSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<ArgumentSyntax> parameters, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.ArgumentListSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public ArgumentListSyntax(SeparatedSyntaxList<ArgumentSyntax> parameters)
        : this(SyntaxFactory.OpenParenToken, parameters, SyntaxFactory.CloseParenToken)
    {

    }
}

public static partial class SyntaxFactory
{

    public static ArgumentListSyntax ArgumentList(SyntaxToken openBracketToken, SeparatedSyntaxList<ArgumentSyntax> arguments, SyntaxToken closeBracketToken)
        => new ArgumentListSyntax(openBracketToken, arguments, closeBracketToken);

    public static ArgumentListSyntax ArgumentList(SeparatedSyntaxList<ArgumentSyntax> arguments)
        => ArgumentList(SyntaxFactory.OpenParenToken, arguments, SyntaxFactory.CloseParenToken);
}