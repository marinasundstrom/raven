
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EnumDeclarationSyntax : BaseTypeDeclarationSyntax
{
    public EnumDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken enumKeyword,
        SyntaxToken identifier,
        SyntaxToken openBraceToken,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.EnumDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
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
    public static EnumDeclarationSyntax EnumDeclaration(
        SyntaxList modifiers,
        SyntaxToken enumKeyword,
        SyntaxToken identifier,
        SyntaxToken openBraceToken,
        SyntaxList members,
        SyntaxToken closeBraceToken,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, enumKeyword, identifier, openBraceToken, members, closeBraceToken, terminatorToken, diagnostics);
}