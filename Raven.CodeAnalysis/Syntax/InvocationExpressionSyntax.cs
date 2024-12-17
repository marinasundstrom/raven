using System.Reflection.Metadata;

using Raven.CodeAnalysis.Syntax.InternalSyntax;

namespace Raven.CodeAnalysis.Syntax;

public partial class InvocationExpressionSyntax : ExpressionSyntax
{
    public partial ExpressionSyntax Expression { get; }
    public partial ArgumentListSyntax ArgumentList { get; }

    internal InvocationExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public InvocationExpressionSyntax(ExpressionSyntax expression, ArgumentListSyntax argumentList)
        : this(new InternalSyntax.InvocationExpressionSyntax((InternalSyntax.ExpressionSyntax)expression.Green, (InternalSyntax.ArgumentListSyntax)argumentList.Green))
    {
    }
}

public static partial class SyntaxFactory
{
    public static InvocationExpressionSyntax InvocationExpression(ExpressionSyntax expression, ArgumentListSyntax argumentList)
        => new InvocationExpressionSyntax(expression, argumentList);
}