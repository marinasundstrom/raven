namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class PredefinedTypeSyntax : ExpressionSyntax
{
    public PredefinedTypeSyntax(
        SyntaxToken keyword,
        IEnumerable<Diagnostic>? diagnostics = null)
    : base(
          SyntaxKind.PredefinedType,
          [
            keyword
          ],
          diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static PredefinedTypeSyntax PredefinedType(
      SyntaxToken keyword,
      IEnumerable<Diagnostic>? diagnostics = null)
      => new(keyword, diagnostics);
}
