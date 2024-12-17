namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ArgumentListSyntax : SyntaxNode
{
    public ArgumentListSyntax(
        SyntaxToken openParenToken,
        SeparatedSyntaxList arguments,
        SyntaxToken closeParentToken)
        : base(
              SyntaxKind.ArgumentList,
              [
                      openParenToken,
                      arguments,
                      closeParentToken
              ])
    {
    }
}