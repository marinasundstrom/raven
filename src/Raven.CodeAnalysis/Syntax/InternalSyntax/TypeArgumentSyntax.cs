namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class TypeArgumentSyntax : SyntaxNode
{
    public TypeArgumentSyntax(
        TypeSyntax type)
        : base(SyntaxKind.TypeArgument, [
            type,
        ])
    {
    }
}