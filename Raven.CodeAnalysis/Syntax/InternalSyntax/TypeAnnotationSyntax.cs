namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class TypeAnnotationSyntax : StatementSyntax
{
    public TypeAnnotationSyntax(
        SyntaxToken colonToken,
        TypeSyntax type)
        : base(
              SyntaxKind.TypeAnnotation,
              [
                      colonToken,
                      type
              ])
    {
    }
}