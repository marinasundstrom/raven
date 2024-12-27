namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class ExpressionSyntax : SyntaxNode
{
    public ExpressionSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }
}