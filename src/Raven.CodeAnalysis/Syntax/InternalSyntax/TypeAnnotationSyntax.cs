namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class TypeAnnotationSyntax : StatementSyntax
{
    public TypeAnnotationSyntax(
        SyntaxToken colonToken,
        TypeSyntax type,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.TypeAnnotation,
              [
                      colonToken,
                      type
              ], diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static TypeAnnotationSyntax TypeAnnotation(
        SyntaxToken colonToken,
        TypeSyntax type,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(colonToken, type, diagnostics);
}
