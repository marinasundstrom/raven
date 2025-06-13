namespace Raven.CodeAnalysis.Syntax;

public partial class TupleExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenParenToken { get; }
    public partial SeparatedSyntaxList<ArgumentSyntax> Arguments { get; }
    public partial SyntaxToken CloseParenToken { get; }

    internal TupleExpressionSyntax(
        InternalSyntax.TupleExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public TupleExpressionSyntax(SyntaxToken openParenToken, SeparatedSyntaxList<ArgumentSyntax> parameters, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.TupleExpressionSyntax(openParenToken.Green, parameters.Green, closeParenToken.Green), null)
    {

    }

    public TupleExpressionSyntax(SeparatedSyntaxList<ArgumentSyntax> parameters)
        : this(SyntaxFactory.OpenParenToken, parameters, SyntaxFactory.CloseParenToken)
    {

    }
}

public static partial class SyntaxFactory
{

    public static TupleExpressionSyntax TupleExpression(SyntaxToken openBracketToken, SeparatedSyntaxList<ArgumentSyntax> arguments, SyntaxToken closeBracketToken)
        => new TupleExpressionSyntax(openBracketToken, arguments, closeBracketToken);

    public static TupleExpressionSyntax TupleExpression(SeparatedSyntaxList<ArgumentSyntax> arguments)
        => TupleExpression(SyntaxFactory.OpenParenToken, arguments, SyntaxFactory.CloseParenToken);
}