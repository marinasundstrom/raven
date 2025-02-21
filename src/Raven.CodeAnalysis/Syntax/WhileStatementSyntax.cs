
namespace Raven.CodeAnalysis.Syntax;

public sealed partial class WhileStatementSyntax : StatementSyntax
{
    public partial SyntaxToken WhileKeyword { get; }
    public partial SyntaxToken OpenParenToken { get; }
    public partial ExpressionSyntax Condition { get; }
    public partial SyntaxToken CloseParenToken { get; }
    public partial StatementSyntax Statement { get; }
    public partial SyntaxToken? SemicolonToken { get; }

    internal WhileStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public WhileStatementSyntax(SyntaxToken whileKeyword, SyntaxToken openParenToken, ExpressionSyntax condition, SyntaxToken closeParenToken, StatementSyntax statement, SyntaxToken? semicolonToken)
      : this(
            new InternalSyntax.WhileStatementSyntax(whileKeyword.Green, openParenToken.Green, (InternalSyntax.ExpressionSyntax)condition.Green, closeParenToken.Green, (InternalSyntax.StatementSyntax)statement.Green, semicolonToken?.Green))
    {

    }

    public WhileStatementSyntax(SyntaxToken whileKeyword, SyntaxToken openParenToken, ExpressionSyntax condition, SyntaxToken closeParenToken, StatementSyntax statement)
          : this(
                new InternalSyntax.WhileStatementSyntax(whileKeyword.Green, openParenToken.Green, (InternalSyntax.ExpressionSyntax)condition.Green, closeParenToken.Green, (InternalSyntax.StatementSyntax)statement.Green, null))
    {

    }

    public WhileStatementSyntax(ExpressionSyntax condition, StatementSyntax statement)
        : this(SyntaxFactory.WhileKeyword, SyntaxFactory.OpenParenToken, condition, SyntaxFactory.CloseParenToken, statement)
    {

    }
}

public static partial class SyntaxFactory
{
    public static WhileStatementSyntax WhileStatement(ExpressionSyntax condition, StatementSyntax statement)
        => new WhileStatementSyntax(condition, statement);

    public static WhileStatementSyntax WhileStatement(SyntaxToken whileKeyword, SyntaxToken openParenToken, ExpressionSyntax condition, SyntaxToken closeParenToken, StatementSyntax statement)
        => new WhileStatementSyntax(whileKeyword, openParenToken, condition, closeParenToken, statement);
}