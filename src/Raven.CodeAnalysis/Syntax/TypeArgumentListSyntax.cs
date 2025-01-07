namespace Raven.CodeAnalysis.Syntax;

public partial class TypeArgumentListSyntax : SyntaxNode
{
    public partial SyntaxToken GreaterThanToken { get; }
    public partial SeparatedSyntaxList<TypeArgumentSyntax> Arguments { get; }
    public partial SyntaxToken LessThanToken { get; }

    public int Count => Arguments.Count;

    internal TypeArgumentListSyntax(
        InternalSyntax.TypeArgumentListSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public TypeArgumentListSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<TypeArgumentSyntax> parameters, SyntaxToken closeParenToken)
        : this(
            new InternalSyntax.TypeArgumentListSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public TypeArgumentListSyntax(SeparatedSyntaxList<TypeArgumentSyntax> parameters)
        : this(SyntaxFactory.LessThanToken, parameters, SyntaxFactory.GreaterThanToken)
    {

    }
}

public static partial class SyntaxFactory
{

    public static TypeArgumentListSyntax TypeArgumentList(SyntaxToken lessThanToken, SeparatedSyntaxList<TypeArgumentSyntax> arguments, SyntaxToken greaterThanToken)
        => new TypeArgumentListSyntax(lessThanToken, arguments, greaterThanToken);

    public static TypeArgumentListSyntax TypeArgumentList(SeparatedSyntaxList<TypeArgumentSyntax> arguments)
        => TypeArgumentList(SyntaxFactory.LessThanToken, arguments, SyntaxFactory.GreaterThanToken);
}