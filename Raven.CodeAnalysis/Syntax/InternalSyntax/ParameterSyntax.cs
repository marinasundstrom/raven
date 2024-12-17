namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ParameterSyntax : SyntaxNode
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
}