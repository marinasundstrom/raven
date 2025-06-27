
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ConstructorDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public ConstructorDeclarationSyntax(
    SyntaxList modifiers,
    SyntaxToken initKeyword,
    SyntaxToken? identifier,
    ParameterListSyntax parameters,
    BlockSyntax? body,
    ArrowExpressionClauseSyntax? expressionBody,
    SyntaxToken? terminatorToken,
    IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(SyntaxKind.ConstructorDeclaration,
          [
                  modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                    initKeyword  ?? throw new ArgumentNullException(nameof(initKeyword)),
                      identifier,
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      body,
                      expressionBody,
                      terminatorToken
          ],
          diagnostics)
    {
    }

    public ConstructorDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken initKeyword,
        SyntaxToken? identifier,
        ParameterListSyntax parameters,
        BlockSyntax body,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.ConstructorDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                    initKeyword  ?? throw new ArgumentNullException(nameof(initKeyword)),
                      identifier,
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      body ?? throw new ArgumentNullException(nameof(body)),
                      null,
                      terminatorToken
              ],
              diagnostics)
    {
    }

    public ConstructorDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken initKeyword,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ArrowExpressionClauseSyntax expressionBody,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(SyntaxKind.ConstructorDeclaration,
          [
                  modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                    initKeyword ?? throw new ArgumentNullException(nameof(initKeyword)),
                      identifier,
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      null,
                      expressionBody ?? throw new ArgumentNullException(nameof(expressionBody)),
                      terminatorToken,
          ],
          diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ConstructorDeclarationSyntax ConstructorDeclaration(
        SyntaxList modifiers,
        SyntaxToken initKeyword,
        SyntaxToken? identifier,
        ParameterListSyntax parameters,
        BlockSyntax body,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, initKeyword, identifier, parameters, body, terminatorToken, diagnostics);

    public static ConstructorDeclarationSyntax ConstructorDeclaration(
        SyntaxList modifiers,
        SyntaxToken initKeyword,
        SyntaxToken? identifier,
        ParameterListSyntax parameters,
        ArrowExpressionClauseSyntax expressionBody,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, initKeyword, identifier, parameters, expressionBody, terminatorToken, diagnostics);
}