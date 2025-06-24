

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class AccessorDeclarationSyntax : SyntaxNode
{
    protected AccessorDeclarationSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }

    public AccessorDeclarationSyntax(
        SyntaxKind kind,
        SyntaxList modifiers,
        SyntaxToken keyword,
        BlockSyntax? body,
        //BlockSyntax expressionBody,
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(
          kind,
          [
                modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                keyword,
                body,
                terminatorToken
          ],
          diagnostics)
    {

    }
}


internal static partial class SyntaxFactory
{
    public static AccessorDeclarationSyntax AccessorDeclaration(
        SyntaxKind kind,
        SyntaxList modifiers,
        SyntaxToken keyword,
        BlockSyntax body,
        //BlockSyntax expressionBody,
        SyntaxToken terminatorToken,
            IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, modifiers, keyword, body, terminatorToken, diagnostics);
}