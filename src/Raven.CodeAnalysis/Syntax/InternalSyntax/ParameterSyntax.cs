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

internal partial class SkippedTokensTrivia : SyntaxNode
{
    public SkippedTokensTrivia(SyntaxList tokens)
    : base(SyntaxKind.SkippedTokensTrivia, [
        tokens,
    ])
    {

    }
}