namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ParameterSyntax : SyntaxNode
{
    public ParameterSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax? typeAnnotation
    )
        : base(SyntaxKind.Parameter, [
            name,
            typeAnnotation
        ])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.ParameterSyntax(this, parent, position);
    }
}