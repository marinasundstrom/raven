namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class StatementSyntax : SyntaxNode
{
    public StatementSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}