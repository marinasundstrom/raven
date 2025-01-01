namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class PredefinedTypeSyntax : ExpressionSyntax
{
    public PredefinedTypeSyntax(
    SyntaxToken keyword)
    : base(
          SyntaxKind.PredefinedType,
          [
            keyword
          ])
    {
    }
}