namespace Raven.CodeAnalysis.Syntax;

public sealed partial class IfStatementSyntax : StatementSyntax
{
    public partial SyntaxToken IfKeyword { get; }
    public partial SyntaxToken OpenParenToken { get; }
    public partial SyntaxNode Condition { get; }
    public partial SyntaxToken CloseParenToken { get; }
    public partial StatementSyntax Statement { get; }
    public partial ElseClauseSyntax? ElseClause { get; }

    public IfStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public IfStatementSyntax(SyntaxToken ifKeyword, SyntaxToken openParenToken, SyntaxNode condition, SyntaxToken closeParenToken, StatementSyntax statement)
          : this(
                new InternalSyntax.IfStatementSyntax(ifKeyword.Green, openParenToken.Green, (InternalSyntax.SyntaxNode)condition?.Green, closeParenToken.Green, (InternalSyntax.StatementSyntax)statement.Green))
    {

    }

    public IfStatementSyntax(SyntaxNode condition, StatementSyntax statement)
        : this(SyntaxFactory.IfKeyword, SyntaxFactory.OpenParenToken, condition, SyntaxFactory.CloseParenToken, statement)
    {

    }

    // Additional properties or methods specific to IfStatement
}

public abstract class StatementSyntax : SyntaxNode
{
    protected StatementSyntax(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent)
    {
    }

    protected StatementSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }
}
public partial class BlockSyntax : StatementSyntax
{
    public partial SyntaxToken OpenBraceToken { get; }
    public partial SyntaxList Statements { get; }
    public partial SyntaxToken CloseBraceToken { get; }

    public BlockSyntax(
        InternalSyntax.BlockSyntax greenNode,
        SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public BlockSyntax(SyntaxToken openBraceToken, SyntaxList<StatementSyntax> statements, SyntaxToken closeBrace)
          : this(
                new InternalSyntax.BlockSyntax(openBraceToken.Green, statements?._greenList, closeBrace.Green), null)
    {

    }

    public BlockSyntax(SyntaxList<StatementSyntax> statements)
        : this(SyntaxFactory.OpenBraceToken, statements, SyntaxFactory.CloseBraceToken)
    {

    }

    // Additional properties or methods specific to BlockSyntax can be added here.
}

public partial class ElseClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ElseKeyword { get; }

    public partial StatementSyntax Statement { get; }

    public ElseClauseSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null)
    : base(greenNode, parent)
    {
    }

}

public partial class MethodDeclarationSyntax : SyntaxNode
{
    public partial SyntaxToken ReturnType { get; }
    public partial SyntaxToken Identifier { get; }
    public partial SeparatedSyntaxList Parameters { get; }
    public partial SyntaxList Body { get; }

    public MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    // Additional properties or methods specific to MethodDeclaration
}


/*
public sealed class IfStatementSyntax : StatementSyntax
{
    private SyntaxNode condition;
    private StatementSyntax statement;

    public SyntaxToken IfKeyword => new SyntaxToken(Green.GetSlot(0) as InternalSyntax.SyntaxToken, this);
    public SyntaxToken OpenParenToken => new SyntaxToken(Green.GetSlot(1) as InternalSyntax.SyntaxToken, this);
    public SyntaxNode Condition => (SyntaxNode)GetNodeSlot(2);
    public SyntaxToken CloseParenToken => new SyntaxToken(Green.GetSlot(3) as InternalSyntax.SyntaxToken, this);
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

    public IfStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public IfStatementSyntax(SyntaxToken ifKeyword, SyntaxToken openParenToken, SyntaxNode condition, SyntaxToken closeParenToken, StatementSyntax statement)
          : this(
                new InternalSyntax.IfStatementSyntax(ifKeyword.Green, openParenToken.Green, (InternalSyntax.SyntaxNode)condition?.Green, closeParenToken.Green, (InternalSyntax.StatementSyntax)statement.Green), null)
    {

    }

    public IfStatementSyntax(SyntaxNode condition, StatementSyntax statement)
        : this(SyntaxFactory.IfKeyword, SyntaxFactory.OpenParenToken, condition, SyntaxFactory.CloseParenToken, statement)
    {

    }

    // Additional properties or methods specific to IfStatement
}

public abstract class StatementSyntax : SyntaxNode
{
    protected StatementSyntax(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent)
    {
    }

    protected StatementSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }
}
public class BlockSyntax : StatementSyntax
{
    public SyntaxToken OpenBraceToken => new SyntaxToken(Green.GetSlot(0) as InternalSyntax.SyntaxToken, this);
    public SyntaxList Statements => SyntaxFactory.CreateListWrapper(Green.GetSlot(1) as InternalSyntax.SyntaxList, this);
    public SyntaxToken CloseBraceToken => new SyntaxToken(Green.GetSlot(2) as InternalSyntax.SyntaxToken, this);

    public BlockSyntax(
        InternalSyntax.BlockSyntax greenNode,
        SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public BlockSyntax(SyntaxToken openBraceToken, SyntaxList<StatementSyntax> statements, SyntaxToken closeBrace)
          : this(
                new InternalSyntax.BlockSyntax(openBraceToken.Green, statements?._greenList, closeBrace.Green), null)
    {

    }

    public BlockSyntax(SyntaxList<StatementSyntax> statements)
        : this(SyntaxFactory.OpenBraceToken, statements, SyntaxFactory.CloseBraceToken)
    {

    }

    // Additional properties or methods specific to BlockSyntax can be added here.
}

public class ElseClauseSyntax : SyntaxNode
{
    public SyntaxToken ElseKeyword => new SyntaxToken(Green.GetSlot(0) as InternalSyntax.SyntaxToken, this);

    public StatementSyntax Statement => (StatementSyntax)SyntaxFactory.CreateWrapper(Green.GetSlot(1) as InternalSyntax.SyntaxList, this);

    public ElseClauseSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null)
    : base(greenNode, parent)
    {
    }

}

public class MethodDeclarationSyntax : SyntaxNode
{
    public SyntaxToken ReturnType => new SyntaxToken(Green.GetSlot(0) as InternalSyntax.SyntaxToken, this);
    public SyntaxToken Identifier => new SyntaxToken(Green.GetSlot(1) as InternalSyntax.SyntaxToken, this);
    public SeparatedSyntaxList Parameters => SyntaxFactory.CreateSeparatedListWrapper(Green.GetSlot(3) as InternalSyntax.SyntaxList, this);
    public SyntaxList Body => SyntaxFactory.CreateListWrapper(Green.GetSlot(5) as InternalSyntax.SyntaxList, this);

    public MethodDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    // Additional properties or methods specific to MethodDeclaration
}
*/