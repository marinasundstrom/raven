namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class TypeArgumentListSyntax : SyntaxNode
{
    public TypeArgumentListSyntax(
        SyntaxToken lessThanToken,
        SyntaxList arguments,
        SyntaxToken greaterThanToken)
        : base(
            SyntaxKind.TypeArgumentList,
            [
                lessThanToken,
                arguments,
                greaterThanToken
            ])
    {
    }
}