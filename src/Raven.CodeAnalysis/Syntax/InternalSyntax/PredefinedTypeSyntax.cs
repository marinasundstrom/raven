namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class PredefinedTypeSyntax : ExpressionSyntax
{
    public PredefinedTypeSyntax(
        SyntaxToken keyword,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(
          SyntaxKind.PredefinedType,
          [
            keyword ?? throw new ArgumentNullException(nameof(keyword))
          ],
          diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static PredefinedTypeSyntax PredefinedType(
      SyntaxToken keyword,
      IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(keyword, diagnostics);
}