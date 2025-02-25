namespace Raven.CodeAnalysis.Syntax;

public partial class BracketedArgumentListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenBracketToken { get; }
    public partial SeparatedSyntaxList<ArgumentSyntax> Arguments { get; }
    public partial SyntaxToken CloseBracketToken { get; }

    internal BracketedArgumentListSyntax(
        InternalSyntax.BracketedArgumentListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public BracketedArgumentListSyntax(SyntaxToken openBracketToken, SeparatedSyntaxList<ArgumentSyntax> parameters, SyntaxToken closeBracketToken)
          : this(
                new InternalSyntax.BracketedArgumentListSyntax(openBracketToken.Green, parameters.Green, closeBracketToken.Green), null)
    {

    }

    public BracketedArgumentListSyntax(SeparatedSyntaxList<ArgumentSyntax> parameters)
        : this(SyntaxFactory.OpenBracketToken, parameters, SyntaxFactory.CloseBracketToken)
    {

    }
}

public static partial class SyntaxFactory
{

    public static BracketedArgumentListSyntax BracketedArgumentList(SyntaxToken openBracketToken, SeparatedSyntaxList<ArgumentSyntax> arguments, SyntaxToken closeBracketToken)
        => new BracketedArgumentListSyntax(openBracketToken, arguments, closeBracketToken);

    public static BracketedArgumentListSyntax BracketedArgumentList(SeparatedSyntaxList<ArgumentSyntax> arguments)
        => BracketedArgumentList(SyntaxFactory.OpenBracketToken, arguments, SyntaxFactory.CloseBracketToken);
}