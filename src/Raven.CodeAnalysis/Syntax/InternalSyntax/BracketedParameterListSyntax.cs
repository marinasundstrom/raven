namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class BracketedParameterListSyntax : SyntaxNode
{
    public BracketedParameterListSyntax(
        SyntaxToken openBracketToken,
        SyntaxList parameters,
        SyntaxToken closeBracketToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ParameterList,
              [
                      openBracketToken ?? throw new ArgumentNullException(nameof(openBracketToken)),
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      closeBracketToken ?? throw new ArgumentNullException(nameof(closeBracketToken))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static BracketedParameterListSyntax BracketedParameterList(
        SyntaxToken openBracketToken,
        SyntaxList parameters,
        SyntaxToken closeBracketToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(openBracketToken, parameters, closeBracketToken, diagnostics);
}