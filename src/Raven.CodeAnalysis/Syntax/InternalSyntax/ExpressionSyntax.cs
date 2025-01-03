namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class ExpressionSyntax : SyntaxNode
{
    public ExpressionSyntax(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }

    internal class Missing : ExpressionSyntax
    {
        public Missing(
            IEnumerable<Diagnostic>? diagnostics = null)
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
    }
}

internal static partial class SyntaxFactory
{
    public static ExpressionSyntax.Missing MissingExpression(
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(diagnostics);
}