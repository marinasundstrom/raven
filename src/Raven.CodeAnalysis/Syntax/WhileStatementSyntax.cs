
namespace Raven.CodeAnalysis.Syntax;

public sealed partial class WhileStatementSyntax : StatementSyntax
{
    public partial SyntaxToken WhileKeyword { get; }
    public partial ExpressionSyntax Condition { get; }
    public partial StatementSyntax Statement { get; }
    public partial SyntaxToken? SemicolonToken { get; }

    internal WhileStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public WhileStatementSyntax(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement, SyntaxToken? semicolonToken)
      : this(
            new InternalSyntax.WhileStatementSyntax(whileKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.StatementSyntax)statement.Green, semicolonToken?.Green))
    {

    }

    public WhileStatementSyntax(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement)
          : this(
                new InternalSyntax.WhileStatementSyntax(whileKeyword.Green, (InternalSyntax.ExpressionSyntax)condition.Green, (InternalSyntax.StatementSyntax)statement.Green, null))
    {

    }

    public WhileStatementSyntax(ExpressionSyntax condition, StatementSyntax statement)
        : this(SyntaxFactory.WhileKeyword, condition, statement)
    {

    }
}

public static partial class SyntaxFactory
{
    public static WhileStatementSyntax WhileStatement(ExpressionSyntax condition, StatementSyntax statement)
        => new WhileStatementSyntax(condition, statement);

    public static WhileStatementSyntax WhileStatement(SyntaxToken whileKeyword, ExpressionSyntax condition, StatementSyntax statement)
        => new WhileStatementSyntax(whileKeyword, condition, statement);
}