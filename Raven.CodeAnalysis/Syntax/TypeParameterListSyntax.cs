namespace Raven.CodeAnalysis.Syntax;

public partial class TypeParameterListSyntax : SyntaxNode
{
    public partial SyntaxToken OpenParenToken { get; }
    public partial SeparatedSyntaxList<ParameterSyntax> Parameters { get; }
    public partial SyntaxToken CloseParenToken { get; }

    public TypeParameterListSyntax(
        InternalSyntax.TypeParameterListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
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

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitParameterList(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitParameterList(this);
    }
}

public static partial class SyntaxFactory
{
    public static TypeParameterListSyntax TypeParameterList(SeparatedSyntaxList<ParameterSyntax> parameters)
        => new TypeParameterListSyntax(parameters);
}