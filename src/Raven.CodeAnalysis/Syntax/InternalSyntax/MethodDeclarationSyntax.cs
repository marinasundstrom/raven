
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class MethodDeclarationSyntax : BaseMethodDeclarationSyntax
{
    public MethodDeclarationSyntax(
        SyntaxList modifiers,
        SyntaxToken funcKeyword,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.MethodDeclaration,
              [
                      modifiers ?? throw new ArgumentNullException(nameof(modifiers)),
                      funcKeyword ?? throw new ArgumentNullException(nameof(funcKeyword)),
                      identifier ?? throw new ArgumentNullException(nameof(identifier)),
                      parameters ?? throw new ArgumentNullException(nameof(parameters)),
                      returnTypeAnnotation,
                      body ?? throw new ArgumentNullException(nameof(body)),
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static MethodDeclarationSyntax MethodDeclaration(
        SyntaxList modifiers,
        SyntaxToken funcKeyword,
        SyntaxToken identifier,
        ParameterListSyntax parameters,
        ReturnTypeAnnotationSyntax returnTypeAnnotation,
        BlockSyntax body,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(modifiers, funcKeyword, identifier, parameters, returnTypeAnnotation, body, diagnostics);
}