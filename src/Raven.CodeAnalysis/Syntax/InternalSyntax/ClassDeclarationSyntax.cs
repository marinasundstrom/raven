
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ClassDeclarationSyntax : TypeDeclarationSyntax
{
    public ClassDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken keyword,
        SyntaxToken identifier,
        SyntaxList parameterList,
        SyntaxToken openBraceToken,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.ClassDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      keyword ?? throw new ArgumentNullException(nameof(keyword)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      parameterList ?? throw new ArgumentNullException(nameof(parameterList)),
                      openBraceToken ?? throw new ArgumentNullException(nameof(openBraceToken)),
                      members ?? throw new ArgumentNullException(nameof(members)),
                      closeBraceToken ?? throw new ArgumentNullException(nameof(closeBraceToken)),
                      terminatorToken!,
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ClassDeclarationSyntax ClassDeclaration(
        SyntaxList modifiers,
        SyntaxToken keyword,
        SyntaxToken identifier,
        SyntaxList parameterList,
        SyntaxToken openBraceToken,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, keyword, identifier, parameterList, openBraceToken, members, closeBraceToken, terminatorToken, diagnostics);
}