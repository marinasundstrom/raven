namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class TypeArgumentListSyntax : SyntaxNode
{
    public TypeArgumentListSyntax(
        SyntaxToken lessThanToken,
        SeparatedSyntaxList arguments,
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