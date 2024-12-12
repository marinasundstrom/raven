namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class ExpressionSyntax : SyntaxNode
{
    public ExpressionSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}