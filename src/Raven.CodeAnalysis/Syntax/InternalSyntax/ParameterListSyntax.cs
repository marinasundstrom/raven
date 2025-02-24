namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ParameterListSyntax : SyntaxNode
{
    public ParameterListSyntax(
        SyntaxToken openParenToken,
        SyntaxList parameters,
        SyntaxToken closeParenToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ParameterList,
              [
                      openParenToken ?? throw new ArgumentNullException(nameof(openParenToken)),
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      closeParenToken ?? throw new ArgumentNullException(nameof(closeParenToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ParameterListSyntax ParameterList(
        SyntaxToken openParenToken,
        SyntaxList parameters,
        SyntaxToken closeParenToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(openParenToken, parameters, closeParenToken, diagnostics);
}