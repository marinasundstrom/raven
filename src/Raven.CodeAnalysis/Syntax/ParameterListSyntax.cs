namespace Raven.CodeAnalysis.Syntax;

public partial class ParameterListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenParenToken { get; }
    public partial SeparatedSyntaxList<ParameterSyntax> Parameters { get; }
    public partial SyntaxToken CloseParenToken { get; }

    internal ParameterListSyntax(
        InternalSyntax.ParameterListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ParameterListSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<ParameterSyntax> parameters, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.ParameterListSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public ParameterListSyntax(SeparatedSyntaxList<ParameterSyntax> parameters)
        : this(SyntaxFactory.OpenParenToken, parameters, SyntaxFactory.CloseParenToken)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ParameterListSyntax ParameterList(SeparatedSyntaxList<ParameterSyntax> parameters)
        => new ParameterListSyntax(parameters);
}