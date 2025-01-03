namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ArgumentListSyntax : SyntaxNode
{
    public ArgumentListSyntax(
        SyntaxToken openParenToken,
        SyntaxList arguments,
        SyntaxToken closeParentToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.ArgumentList,
              [
                      openParenToken,
                      arguments,
                      closeParentToken
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ArgumentListSyntax ArgumentList(
            SyntaxToken openParenToken,
            SyntaxList arguments,
            SyntaxToken closeParentToken,
            IEnumerable<Diagnostic>? diagnostics = null)
        => new(openParenToken, arguments, closeParentToken, diagnostics);
}