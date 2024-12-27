namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class StatementSyntax : SyntaxNode
{
    public StatementSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}