namespace Raven.CodeAnalysis.Syntax;

public partial class BracketedParameterListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenBracketToken { get; }
    public partial SeparatedSyntaxList<ParameterSyntax> Parameters { get; }
    public partial SyntaxToken CloseBracketToken { get; }

    internal BracketedParameterListSyntax(
        InternalSyntax.BracketedParameterListSyntax greenNode,
        SyntaxNode bracket = null,
        int position = 0)
        : base(greenNode, bracket, position)
    {
    }

    public BracketedParameterListSyntax(SyntaxToken openBracketToken, SeparatedSyntaxList<ParameterSyntax> parameters, SyntaxToken closeBracketToken)
          : this(
                new InternalSyntax.BracketedParameterListSyntax(openBracketToken.Green, parameters.Green, closeBracketToken.Green), null)
    {

    }

    public BracketedParameterListSyntax(SeparatedSyntaxList<ParameterSyntax> parameters)
        : this(SyntaxFactory.OpenBracketToken, parameters, SyntaxFactory.CloseBracketToken)
    {

    }
}

public static partial class SyntaxFactory
{
    public static BracketedParameterListSyntax BracketedParameterList(SeparatedSyntaxList<ParameterSyntax> parameters)
        => new BracketedParameterListSyntax(parameters);
}