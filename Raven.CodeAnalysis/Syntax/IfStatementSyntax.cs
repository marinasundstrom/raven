namespace Raven.CodeAnalysis.Syntax;

public sealed partial class IfStatementSyntax : StatementSyntax
{
    public partial SyntaxToken IfKeyword { get; }
    public partial SyntaxToken OpenParenToken { get; }
    public partial ExpressionSyntax Condition { get; }
    public partial SyntaxToken CloseParenToken { get; }
    public partial StatementSyntax Statement { get; }
    public partial ElseClauseSyntax? ElseClause { get; }

    public IfStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public IfStatementSyntax(SyntaxToken ifKeyword, SyntaxToken openParenToken, ExpressionSyntax condition, SyntaxToken closeParenToken, StatementSyntax statement)
          : this(
                new InternalSyntax.IfStatementSyntax(ifKeyword.Green, openParenToken.Green, (InternalSyntax.ExpressionSyntax)condition?.Green, closeParenToken.Green, (InternalSyntax.StatementSyntax)statement.Green))
    {

    }

    public IfStatementSyntax(ExpressionSyntax condition, StatementSyntax statement)
        : this(SyntaxFactory.IfKeyword, SyntaxFactory.OpenParenToken, condition, SyntaxFactory.CloseParenToken, statement)
    {

    }

    // Additional properties or methods specific to IfStatement
}

public static partial class SyntaxFactory
{
    public static IfStatementSyntax IfStatement(ExpressionSyntax condition, StatementSyntax statement)
        => new IfStatementSyntax(condition, statement);
}