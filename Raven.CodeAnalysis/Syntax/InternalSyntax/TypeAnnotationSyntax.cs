namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class TypeAnnotationSyntax : StatementSyntax
{
    public TypeAnnotationSyntax(
        SyntaxToken colonToken,
        TypeSyntax type,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.TypeAnnotation,
              [
                      colonToken,
                      type
              ],
              colonToken.FullWidth + type.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.TypeAnnotationSyntax(this, parent);
    }
}