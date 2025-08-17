namespace Raven.CodeAnalysis.Syntax;

public abstract partial class ExpressionSyntax : ExpressionOrPatternSyntax
{
    public class Missing : ExpressionSyntax
    {
        internal Missing(SyntaxKind kind = SyntaxKind.None)
            : base(new InternalSyntax.ExpressionSyntax.Missing(kind), null, 0)
        {
        }

        internal Missing(
            InternalSyntax.ExpressionSyntax.Missing greenNode,
            SyntaxNode parent = null,
            int position = 0)
            : base(greenNode, parent, position)
        {
        }

        public override void Accept(SyntaxVisitor visitor)
        {
            visitor.DefaultVisit(this);
        }

        public override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
        {
            return visitor.DefaultVisit(this);
        }
    }
}