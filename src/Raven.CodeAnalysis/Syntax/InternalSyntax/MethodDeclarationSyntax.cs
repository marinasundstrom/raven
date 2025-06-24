
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class MethodDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public MethodDeclarationSyntax(
    SyntaxList modifiers,
    SyntaxToken identifier,
    ParameterListSyntax parameters,
    ReturnTypeAnnotationSyntax returnTypeAnnotation,
    BlockSyntax? body,
    ArrowExpressionClauseSyntax? expressionBody,
    SyntaxToken? terminatorToken,
    IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(SyntaxKind.MethodDeclaration,
          [
                  modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      returnTypeAnnotation,
                      body,
                      expressionBody,
                      terminatorToken
          ],
          diagnostics)
    {
    }

    public MethodDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.MethodDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      returnTypeAnnotation,
                      body ?? throw new ArgumentNullException(nameof(body)),
                      null,
                      terminatorToken
              ],
              diagnostics)
    {
    }

    public MethodDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        ArrowExpressionClauseSyntax expressionBody,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(SyntaxKind.MethodDeclaration,
          [
                  modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      returnTypeAnnotation,
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
    public static MethodDeclarationSyntax MethodDeclaration(
        SyntaxList modifiers,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, identifier, parameters, returnTypeAnnotation, body, terminatorToken, diagnostics);

    public static MethodDeclarationSyntax MethodDeclaration(
        SyntaxList modifiers,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        ArrowExpressionClauseSyntax expressionBody,
        SyntaxToken? terminatorToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, identifier, parameters, returnTypeAnnotation, expressionBody, terminatorToken, diagnostics);
}