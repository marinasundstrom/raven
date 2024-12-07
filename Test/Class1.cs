using Raven.CodeAnalysis.Syntax;

namespace Test;

public class TestNodeSyntax : SyntaxNode
{
    private SyntaxNode condition;
    private StatementSyntax statement;

    public SyntaxToken IfKeyword => new SyntaxToken(Green.GetSlot(0) as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken, this);
    public SyntaxToken OpenParenToken => new SyntaxToken(Green.GetSlot(1) as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken, this);
    public SyntaxNode Condition => GetNodeSlot(2);
    public SyntaxToken CloseParenToken => new SyntaxToken(Green.GetSlot(3) as Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken, this);
    public StatementSyntax Statement => (StatementSyntax)GetNodeSlot(4);
    public ElseClauseSyntax? ElseClause => Green.SlotCount > 5 ? (ElseClauseSyntax?)GetNodeSlot(5) : null;

    internal override SyntaxNode? GetNodeSlot(int index)
    {
        return index switch
        {
            2 => Green.GetRed(ref condition, 2),
            4 => Green.GetRed(ref statement, 4),
            5 => Green.GetRed(ref statement, 5),
            _ => throw new Exception()
        };
    }

    public TestNodeSyntax(Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }
}

public partial class TestNode2Syntax : SyntaxNode
{
    public partial SyntaxToken IfKeyword { get; }
    public partial SyntaxToken OpenParenToken { get; }
    public partial SyntaxNode Condition { get; }
    public partial SyntaxToken CloseParenToken { get; }
    public partial StatementSyntax Statement { get; }
    public partial ElseClauseSyntax? ElseClause { get; }

    public TestNode2Syntax(Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public TestNode2Syntax(SyntaxToken ifKeyword, SyntaxToken openParenToken, SyntaxNode condition, SyntaxToken closeParenToken, StatementSyntax statement)
          : this(
                new InternalSyntax.IfStatementSyntax(ifKeyword.Green, openParenToken.Green, (InternalSyntax.SyntaxNode)condition?.Green, closeParenToken.Green, (InternalSyntax.StatementSyntax)statement.Green), null)
    {

    }

    public TestNode2Syntax(SyntaxNode condition, StatementSyntax statement)
        : this(SyntaxFactory.IfKeyword, SyntaxFactory.OpenParenToken, condition, SyntaxFactory.CloseParenToken, statement)
    {

    }

}

public partial class TestNode3Syntax : StatementSyntax
{
    public partial SyntaxToken OpenBraceToken { get; }
    public partial SyntaxList<StatementSyntax> Statements { get; }
    public partial SyntaxToken CloseBraceToken { get; }

    public TestNode3Syntax(Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
    : base(greenNode, parent)
    {
    }
}