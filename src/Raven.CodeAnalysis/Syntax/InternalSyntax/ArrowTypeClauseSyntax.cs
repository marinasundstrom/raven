namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ArrowTypeClauseSyntax : StatementSyntax
{
    public ArrowTypeClauseSyntax(
        SyntaxToken arrowToken,
        TypeSyntax type,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ReturnTypeAnnotation,
              [
                      arrowToken ?? throw new ArgumentNullException(nameof(arrowToken)),
                      type ?? throw new ArgumentNullException(nameof(type))
              ], diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ArrowTypeClauseSyntax ReturnTypeAnnotation(
        SyntaxToken arrowToken,
        TypeSyntax type,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(arrowToken, type, diagnostics);
}