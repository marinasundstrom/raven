namespace Raven.CodeAnalysis.Syntax;

public partial class TypeParameterListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenParenToken { get; }
    public partial SeparatedSyntaxList<ParameterSyntax> Items { get; }

    public IEnumerable<ParameterSyntax> Parameters => Items.OfType<ParameterSyntax>();

    public partial SyntaxToken CloseParenToken { get; }

    public TypeParameterListSyntax(
        InternalSyntax.TypeParameterListSyntax greenNode,
        SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public TypeParameterListSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<ParameterSyntax> parameters, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.TypeParameterListSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public TypeParameterListSyntax(SeparatedSyntaxList<ParameterSyntax> parameters)
        : this(SyntaxFactory.OpenParenToken, parameters, SyntaxFactory.CloseParenToken)
    {

    }

    // Additional properties or methods specific to BlockSyntax can be added here.
}

public static partial class SyntaxFactory
{
    public static TypeParameterListSyntax TypeParameterList(SeparatedSyntaxList<ParameterSyntax> parameters)
        => new TypeParameterListSyntax(parameters);
}