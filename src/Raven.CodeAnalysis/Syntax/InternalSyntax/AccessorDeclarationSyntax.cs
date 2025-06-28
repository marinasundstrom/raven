

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
    ArrowExpressionClauseSyntax? expressionBody,
    SyntaxToken terminatorToken,
    IEnumerable<DiagnosticInfo>? diagnostics = null)
: base(
      kind,
      [
            modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                keyword,
                body,
                expressionBody,
                terminatorToken
      ],
      diagnostics)
    {

    }

    public AccessorDeclarationSyntax(
        SyntaxKind kind,
        SyntaxList modifiers,
        SyntaxToken keyword,
        BlockSyntax body,
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(
          kind,
          [
                modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                keyword,
                body ?? throw new ArgumentNullException(nameof(body)),
                null,
                terminatorToken
          ],
          diagnostics)
    {

    }

    public AccessorDeclarationSyntax(
        SyntaxKind kind,
        SyntaxList modifiers,
        SyntaxToken keyword,
        ArrowExpressionClauseSyntax expressionBody,
        SyntaxToken terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
: base(
      kind,
      [
            modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                keyword,
                null,
                expressionBody ?? throw new ArgumentNullException(nameof(expressionBody)),
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
        SyntaxToken terminatorToken,
            IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, modifiers, keyword, body, terminatorToken, diagnostics);

    public static AccessorDeclarationSyntax AccessorDeclaration(
        SyntaxKind kind,
        SyntaxList modifiers,
        SyntaxToken keyword,
        ArrowExpressionClauseSyntax expressionBody,
        SyntaxToken terminatorToken,
            IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, modifiers, keyword, expressionBody, terminatorToken, diagnostics);
}