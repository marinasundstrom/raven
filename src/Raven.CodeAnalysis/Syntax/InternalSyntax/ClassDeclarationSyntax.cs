
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ClassDeclarationSyntax : TypeDeclarationSyntax
{
    public ClassDeclarationSyntax(
        SyntaxToken enumKeyword,
        SyntaxToken identifier,
        SyntaxToken openBraceToken,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.ClassDeclaration,
              [
                      enumKeyword ?? throw new ArgumentNullException(nameof(enumKeyword)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
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
        SyntaxToken enumKeyword,
        SyntaxToken identifier,
        SyntaxToken openBraceToken,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken? terminatorToken,
        IClasserable<DiagnosticInfo>? diagnostics = null)
        => new(enumKeyword, identifier, openBraceToken, members, closeBraceToken, terminatorToken, diagnostics);
}