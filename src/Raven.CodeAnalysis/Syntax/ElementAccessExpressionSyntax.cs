using System.Reflection.Metadata;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class ElementAccessExpressionSyntax : ExpressionSyntax
{
    public partial ExpressionSyntax Expression { get; }
    public partial BracketedArgumentListSyntax ArgumentList { get; }

    internal ElementAccessExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ElementAccessExpressionSyntax(ExpressionSyntax expression, BracketedArgumentListSyntax argumentList)
        : this(new InternalSyntax.ElementAccessExpressionSyntax((InternalSyntax.ExpressionSyntax)expression.Green, (InternalSyntax.BracketedArgumentListSyntax)argumentList.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ElementAccessExpressionSyntax ElementAccessExpression(ExpressionSyntax expression, BracketedArgumentListSyntax argumentList)
        => new ElementAccessExpressionSyntax(expression, argumentList);
}