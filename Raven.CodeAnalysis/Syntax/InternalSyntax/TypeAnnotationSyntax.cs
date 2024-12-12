namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class TypeAnnotationSyntax : StatementSyntax
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

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.TypeAnnotationSyntax(this, parent, position);
    }
}