
namespace Raven.CodeAnalysis.Syntax;

public sealed partial class IfExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken IfKeyword { get; }
    public partial ExpressionSyntax Condition { get; }
    public partial ExpressionSyntax Expression { get; }
    public partial ElseClauseSyntax? ElseClause { get; }

    internal IfExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public IfExpressionSyntax(SyntaxToken ifKeyword, ExpressionSyntax condition, ExpressionSyntax expression)
          : this(
                new InternalSyntax.IfExpressionSyntax(ifKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.ExpressionSyntax)expression.Green, null))
    {

    }

    public IfExpressionSyntax(SyntaxToken ifKeyword, ExpressionSyntax condition, ExpressionSyntax expression, ElseClauseSyntax? elseClause)
      : this(
            new InternalSyntax.IfExpressionSyntax(ifKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.ExpressionSyntax)expression.Green, (InternalSyntax.ElseClauseSyntax?)elseClause?.Green))
    {

    }

    public IfExpressionSyntax(ExpressionSyntax condition, ExpressionSyntax expression)
        : this(SyntaxFactory.IfKeyword, condition, expression)
    {

    }

    public IfExpressionSyntax(ExpressionSyntax condition, ExpressionSyntax expression, ElseClauseSyntax elseClause)
        : this(SyntaxFactory.IfKeyword, condition, expression, elseClause)
    {

    }
}

public static partial class SyntaxFactory
{
    public static IfExpressionSyntax IfStatement(ExpressionSyntax condition, ExpressionSyntax expression)
        => new IfExpressionSyntax(condition, expression);

    public static IfExpressionSyntax IfStatement(SyntaxToken ifKeyword, ExpressionSyntax condition, ExpressionSyntax expression)
        => new IfExpressionSyntax(ifKeyword, condition, expression);
}