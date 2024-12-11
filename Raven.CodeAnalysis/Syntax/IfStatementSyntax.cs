
namespace Raven.CodeAnalysis.Syntax;

public sealed partial class IfStatementSyntax : StatementSyntax
{
    public partial SyntaxToken IfKeyword { get; }
    public partial SyntaxToken OpenParenToken { get; }
    public partial ExpressionSyntax Condition { get; }
    public partial SyntaxToken CloseParenToken { get; }
    public partial StatementSyntax Statement { get; }
    public partial ElseClauseSyntax? ElseClause { get; }
    public partial SyntaxToken? SemicolonToken { get; }

    public IfStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public IfStatementSyntax(SyntaxToken ifKeyword, SyntaxToken openParenToken, ExpressionSyntax condition, SyntaxToken closeParenToken, StatementSyntax statement)
          : this(
                new InternalSyntax.IfStatementSyntax(ifKeyword.Green, openParenToken.Green, (InternalSyntax.ExpressionSyntax)condition.Green, closeParenToken.Green, (InternalSyntax.StatementSyntax)statement.Green))
    {

    }

    public IfStatementSyntax(SyntaxToken ifKeyword, SyntaxToken openParenToken, ExpressionSyntax condition, SyntaxToken closeParenToken, StatementSyntax statement, ElseClauseSyntax? elseClause)
      : this(
            new InternalSyntax.IfStatementSyntax(ifKeyword.Green, openParenToken.Green, (InternalSyntax.ExpressionSyntax)condition.Green, closeParenToken.Green, (InternalSyntax.StatementSyntax)statement.Green, (InternalSyntax.ElseClauseSyntax?)elseClause?.Green))
    {

    }

    public IfStatementSyntax(ExpressionSyntax condition, StatementSyntax statement)
        : this(SyntaxFactory.IfKeyword, SyntaxFactory.OpenParenToken, condition, SyntaxFactory.CloseParenToken, statement)
    {

    }

    public IfStatementSyntax(ExpressionSyntax condition, StatementSyntax statement, ElseClauseSyntax elseClause)
        : this(SyntaxFactory.IfKeyword, SyntaxFactory.OpenParenToken, condition, SyntaxFactory.CloseParenToken, statement, elseClause)
    {

    }

    public IfStatementSyntax WithElseClause(ElseClauseSyntax elseClause)
    {
        return new IfStatementSyntax(Condition, Statement, elseClause);
    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitIfStatement(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitIfStatement(this);
    }

    public IfStatementSyntax Update(SyntaxToken ifKeyword, SyntaxToken openParenToken, ExpressionSyntax condition, SyntaxToken closeParenToken, StatementSyntax statement, ElseClauseSyntax? elseClause)
    {
        return new IfStatementSyntax(ifKeyword, openParenToken, condition, closeParenToken, statement, elseClause);
    }
}

public static partial class SyntaxFactory
{
    public static IfStatementSyntax IfStatement(ExpressionSyntax condition, StatementSyntax statement)
        => new IfStatementSyntax(condition, statement);
}