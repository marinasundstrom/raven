
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EnumDeclarationSyntax : BaseTypeDeclarationSyntax
{
    public EnumDeclarationSyntax(
        SyntaxToken enumKeyword,
        SyntaxToken identifier,
        SyntaxToken openBraceToken,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken? semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.EnumDeclaration,
              [
                      enumKeyword ?? throw new ArgumentNullException(nameof(enumKeyword)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      openBraceToken ?? throw new ArgumentNullException(nameof(openBraceToken)),
                      members ?? throw new ArgumentNullException(nameof(members)),
                      closeBraceToken ?? throw new ArgumentNullException(nameof(closeBraceToken)),
                      semicolonToken!,
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static EnumDeclarationSyntax EnumDeclaration(
        SyntaxToken enumKeyword,
        SyntaxToken identifier,
        SyntaxToken openBraceToken,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken? semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(enumKeyword, identifier, openBraceToken, members, closeBraceToken, semicolonToken, diagnostics);
}
