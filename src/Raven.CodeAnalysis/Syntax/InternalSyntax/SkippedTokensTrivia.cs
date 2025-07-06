namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class SkippedTokensTrivia : SyntaxNode
{
    public SkippedTokensTrivia(SyntaxList tokens,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(
        SyntaxKind.SkippedTokensTrivia, [tokens ?? throw new ArgumentNullException(nameof(tokens))], diagnostics)
    {

    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.SkippedTokensTrivia(this, parent, position);
    }

    internal override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitSkippedTokensTrivia(this);
    }

    internal override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
    {
        return visitor.VisitSkippedTokensTrivia(this);
    }
}

internal static partial class SyntaxFactory
{
    public static SkippedTokensTrivia SkippedTokensTrivia(
        SyntaxList tokens,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(tokens, diagnostics);
}