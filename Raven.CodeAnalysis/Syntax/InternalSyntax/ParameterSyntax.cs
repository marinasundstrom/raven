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
        ], 0, [], 0)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.ParameterSyntax(this, parent);
    }
}