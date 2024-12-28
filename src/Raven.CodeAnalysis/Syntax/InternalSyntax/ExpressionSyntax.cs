namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class ExpressionSyntax : SyntaxNode
{
    public ExpressionSyntax(SyntaxKind kind, GreenNode[] slots) : base(kind, slots)
    {
    }

    internal class Missing : ExpressionSyntax
    {
        public Missing(GreenNode[] slots) : base(SyntaxKind.None, slots)
        {
            LeadingTrivia = SyntaxTriviaList.Empty;
            TrailingTrivia = SyntaxTriviaList.Empty;
        }

        public override bool IsMissing => true;

        public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
        {
            return new Syntax.ExpressionSyntax.Missing(this, parent, position);
        }
    }
}