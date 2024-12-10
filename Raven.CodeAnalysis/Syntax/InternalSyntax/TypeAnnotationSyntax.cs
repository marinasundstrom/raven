namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class TypeAnnotationSyntax : StatementSyntax
{
    public TypeAnnotationSyntax(
        SyntaxToken colonToken,
        TypeSyntax type,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.TypeAnnotation,
              [
                      colonToken,
                      type
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.TypeAnnotationSyntax(this, parent, position);
    }
}