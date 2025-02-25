namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class BracketedArgumentListSyntax : SyntaxNode
{
    public BracketedArgumentListSyntax(
        SyntaxToken openBracketToken,
        SyntaxList arguments,
        SyntaxToken closeBracketToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.BracketedArgumentList,
              [
                      openBracketToken ?? throw new ArgumentNullException(nameof(openBracketToken)),
                      arguments ?? throw new ArgumentNullException(nameof(arguments)),
                      closeBracketToken ?? throw new ArgumentNullException(nameof(closeBracketToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static BracketedArgumentListSyntax BracketedArgumentList(
            SyntaxToken openBracketToken,
            SyntaxList arguments,
            SyntaxToken closeBrackettToken,
            IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(openBracketToken, arguments, closeBrackettToken, diagnostics);
}