namespace Raven.CodeAnalysis.Syntax;

public abstract class ExpressionSyntax : SyntaxNode
{
    internal ExpressionSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public class Missing : ExpressionSyntax
    {
        internal Missing()
            : base(new InternalSyntax.ExpressionSyntax.Missing([]), null, 0)
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