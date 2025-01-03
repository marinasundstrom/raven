namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class SkippedTokensTrivia : SyntaxNode
{
    public SkippedTokensTrivia(SyntaxList tokens,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(
        SyntaxKind.SkippedTokensTrivia, [tokens], diagnostics)
    {

    }
}

internal static partial class SyntaxFactory
{
    public static SkippedTokensTrivia SkippedTokensTrivia(
        SyntaxList tokens,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(tokens, diagnostics);
}
