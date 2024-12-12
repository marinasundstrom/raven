namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class ParameterListSyntax : SyntaxNode
{
    public ParameterListSyntax(
        SyntaxToken openParenToken,
        SeparatedSyntaxList parameters,
        SyntaxToken closeParentToken)
        : base(
              SyntaxKind.Block,
              [
                      openParenToken,
                      parameters,
                      closeParentToken
              ])
    {
    }
}