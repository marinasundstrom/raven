namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class ExpressionSyntax : ExpressionOrPatternSyntax
{
    public ExpressionSyntax(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }

    internal class Missing : ExpressionSyntax
    {
        public Missing(
            IEnumerable<DiagnosticInfo>? diagnostics = null)
            : base(SyntaxKind.None, [], diagnostics)
        {
            LeadingTrivia = SyntaxTriviaList.Empty;
            TrailingTrivia = SyntaxTriviaList.Empty;
        }

        public override bool IsMissing => true;

        public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
        {
            return new Syntax.ExpressionSyntax.Missing(this, parent, position);
        }

        internal override void Accept(SyntaxVisitor visitor)
        {
            visitor.Visit(this);
        }

        internal override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
        {
            return visitor.Visit(this);
        }
    }
}

internal static partial class SyntaxFactory
{
    public static ExpressionSyntax.Missing MissingExpression(
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(diagnostics);
}