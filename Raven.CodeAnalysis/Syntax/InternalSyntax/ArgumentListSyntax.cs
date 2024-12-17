namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class ArgumentListSyntax : SyntaxNode
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