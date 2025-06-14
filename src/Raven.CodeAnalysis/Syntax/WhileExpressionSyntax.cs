
namespace Raven.CodeAnalysis.Syntax;

public sealed partial class WhileExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken WhileKeyword { get; }
    public partial ExpressionSyntax Condition { get; }
    public partial StatementSyntax Statement { get; }
    public partial SyntaxToken? TerminatorToken { get; }

    internal WhileExpressionSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public WhileExpressionSyntax(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement, SyntaxToken? terminatorToken)
      : this(
            new InternalSyntax.WhileExpressionSyntax(whileKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.StatementSyntax)statement.Green))
    {

    }

    public WhileExpressionSyntax(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement)
          : this(
                new InternalSyntax.WhileExpressionSyntax(whileKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.StatementSyntax)statement.Green, null))
    {

    }

    public WhileExpressionSyntax(ExpressionSyntax condition, StatementSyntax statement)
        : this(SyntaxFactory.WhileKeyword, condition, statement)
    {

    }
}

public static partial class SyntaxFactory
{
    public static WhileExpressionSyntax WhileStatement(ExpressionSyntax condition, StatementSyntax statement)
        => new WhileExpressionSyntax(condition, statement);

    public static WhileExpressionSyntax WhileStatement(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement)
        => new WhileExpressionSyntax(whileKeyword, condition, statement);
}