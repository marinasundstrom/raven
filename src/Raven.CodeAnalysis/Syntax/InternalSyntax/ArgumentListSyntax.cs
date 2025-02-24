namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ArgumentListSyntax : SyntaxNode
{
    public ArgumentListSyntax(
        SyntaxToken openParenToken,
        SyntaxList arguments,
        SyntaxToken closeParenToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ArgumentList,
              [
                      openParenToken ?? throw new ArgumentNullException(nameof(openParenToken)),
                      arguments ?? throw new ArgumentNullException(nameof(arguments)),
                      closeParenToken ?? throw new ArgumentNullException(nameof(closeParenToken))
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
            IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(openParenToken, arguments, closeParentToken, diagnostics);
}