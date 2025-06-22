
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class FieldDeclarationSyntax : MemberDeclarationSyntax
{
    public FieldDeclarationSyntax(
        SyntaxList modifiers,
        VariableDeclarationSyntax variableDeclarationSyntax,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.MethodDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      variableDeclarationSyntax ?? throw new ArgumentNullException(nameof(variableDeclarationSyntax)),
                      terminatorToken
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static FieldDeclarationSyntax FieldDeclaration(
        SyntaxList modifiers,
        VariableDeclarationSyntax declaration,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, declaration, terminatorToken, diagnostics);
}