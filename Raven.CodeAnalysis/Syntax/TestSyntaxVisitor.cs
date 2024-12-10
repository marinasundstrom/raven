
namespace Raven.CodeAnalysis.Syntax;

public sealed class TestSyntaxVisitor : SyntaxVisitor
{
    public override void DefaultVisit(SyntaxNode node)
    {
        Console.WriteLine(node);
    }

    public override void VisitIfStatement(IfStatementSyntax node)
    {
        node.Condition.Accept(this);
        node.Statement.Accept(this);
        node.ElseClause?.Accept(this);
    }

    public override void VisitReturnStatement(ReturnStatementSyntax node)
    {
        node.Expression?.Accept(this);
    }
}